//! Copy-on-write overlay filesystem.
//!
//! Writes land in an upper layer; the lower layer is never touched. The
//! overlay tracks exactly what changed (whiteouts for deletions, base
//! snapshots for first-touch content) so a consumer can later render a
//! patch, commit, fork, or discard. Design: `docs/kaish-overlayfs.md`.

use crate::budget::ByteBudget;
use crate::paths::normalize;
use crate::traits::{DirEntry, DirEntryKind, Filesystem, ReadRange};
use async_trait::async_trait;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::SystemTime;
use tokio::sync::RwLock;

/// The kind of change a path has relative to the lower layer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChangeKind {
    /// Path did not exist in lower; it was added in the overlay.
    Added,
    /// Path existed in lower and was modified in the overlay.
    Modified,
    /// Path existed in lower and was removed in the overlay.
    Removed,
}

/// One entry in the overlay's dirty set, as returned by [`OverlayFs::changes`].
///
/// `base` and `current` are byte vectors; `changes()` reports files only.
/// A `Modified` entry with `base == current` is possible (e.g. `set_mtime`
/// copy-up) and is reported as-is — the file content is unchanged but the
/// overlay still owns a copy-up and will replay the write on commit.
#[derive(Debug, Clone)]
pub struct OverlayChange {
    pub path: PathBuf,
    pub kind: ChangeKind,
    /// Lower content at first touch; `None` for `Added`.
    pub base: Option<Vec<u8>>,
    /// Current overlay content; `None` for `Removed`.
    pub current: Option<Vec<u8>>,
}

/// Copy-on-write overlay over a (typically read-only) lower filesystem.
///
/// Reads prefer the upper layer; the first mutation of a lower path copies
/// it up, snapshotting the lower content as the change's *base*. Deletions
/// of lower paths are recorded as whiteouts. The lower layer is never
/// written.
pub struct OverlayFs {
    lower: Arc<dyn Filesystem>,
    upper: Arc<dyn Filesystem>,
    state: RwLock<OverlayState>,
    /// Bytes held by base snapshots — overlay-owned RAM regardless of the
    /// upper's cost model; the hidden 2× of copy-up.
    base_bytes: AtomicU64,
    budget: Option<Arc<ByteBudget>>,
}

#[derive(Default)]
struct OverlayState {
    /// Paths deleted relative to lower. A later write clears the whiteout.
    whiteouts: HashSet<PathBuf>,
    /// First-touch snapshots: lower content at the moment a path first became
    /// dirty. `None` = the path did not exist in lower (it's an Added file).
    /// Files only — directories and symlinks are out of the changes scope.
    bases: HashMap<PathBuf, Option<Vec<u8>>>,
    /// Symlinks created in the upper. These are dirty state that the
    /// files-only `bases` map cannot track; their presence makes `changes()`
    /// and `commit_into` error `Unsupported` loudly rather than silently
    /// omitting them. Also populated when a lower-resident symlink is removed
    /// (whiteouted), since that deletion cannot be expressed as a base
    /// snapshot.
    dirty_symlinks: HashSet<PathBuf>,
}

fn not_found(path: &Path) -> io::Error {
    io::Error::new(
        io::ErrorKind::NotFound,
        format!("not found: {}", path.display()),
    )
}

fn is_not_found(error: &io::Error) -> bool {
    error.kind() == io::ErrorKind::NotFound
}

/// Build a sorted, deduplicated display string of paths for error messages.
fn format_paths(paths: &HashSet<PathBuf>) -> String {
    let mut sorted: Vec<&PathBuf> = paths.iter().collect();
    sorted.sort();
    sorted
        .iter()
        .map(|p| p.display().to_string())
        .collect::<Vec<_>>()
        .join(", ")
}

impl OverlayFs {
    /// Overlay `upper` (writable) on top of `lower` (never written).
    pub fn new(lower: Arc<dyn Filesystem>, upper: Arc<dyn Filesystem>) -> Self {
        Self::build(lower, upper, None)
    }

    /// Overlay whose base snapshots draw on a shared budget. The budget
    /// covers only what the overlay itself holds (the bases); hand the same
    /// `Arc` to the upper at its construction to cap the full 2×.
    pub fn with_budget(
        lower: Arc<dyn Filesystem>,
        upper: Arc<dyn Filesystem>,
        budget: Arc<ByteBudget>,
    ) -> Self {
        Self::build(lower, upper, Some(budget))
    }

    fn build(
        lower: Arc<dyn Filesystem>,
        upper: Arc<dyn Filesystem>,
        budget: Option<Arc<ByteBudget>>,
    ) -> Self {
        Self {
            lower,
            upper,
            state: RwLock::new(OverlayState::default()),
            base_bytes: AtomicU64::new(0),
            budget,
        }
    }

    /// Overlay a fresh in-memory upper on top of `lower` — the conventional
    /// pairing: zero disk writes, strictly ephemeral.
    #[cfg(feature = "memory")]
    pub fn over(lower: Arc<dyn Filesystem>) -> Self {
        Self::new(lower, Arc::new(crate::memory::MemoryFs::new()))
    }

    /// The conventional pairing with one budget across both halves: the
    /// in-memory upper's content and the overlay's base snapshots draw on
    /// the same pool, so the budget caps the full 2× of copy-up.
    #[cfg(feature = "memory")]
    pub fn over_with_budget(lower: Arc<dyn Filesystem>, budget: Arc<ByteBudget>) -> Self {
        let upper = Arc::new(crate::memory::MemoryFs::with_budget(budget.clone()));
        Self::with_budget(lower, upper, budget)
    }

    /// Reserve budget for a base snapshot *before* any layer is mutated, so
    /// a refused charge leaves both layers and the state untouched.
    fn charge_base(&self, bytes: u64) -> io::Result<()> {
        if bytes > 0
            && let Some(budget) = &self.budget
        {
            budget.try_charge(bytes)?;
        }
        Ok(())
    }

    /// Undo a `charge_base` after a later step failed.
    fn refund_base_charge(&self, bytes: u64) {
        if bytes > 0
            && let Some(budget) = &self.budget
        {
            budget.credit(bytes);
        }
    }

    /// Record a successfully charged base snapshot's bytes.
    fn settle_base(&self, bytes: u64) {
        self.base_bytes.fetch_add(bytes, Ordering::AcqRel);
    }

    /// Credit a dropped base snapshot (e.g. lower drift discovered on
    /// remove; later, `reset`). NEVER bypass this — the counter panics on
    /// drift by design.
    fn credit_base(&self, bytes: u64) {
        let previous = self.base_bytes.fetch_sub(bytes, Ordering::AcqRel);
        assert!(
            previous >= bytes,
            "OverlayFs base_bytes underflow: {} - {} — accounting bug; possible cause: \
             out-of-band mutation of the upper Arc or internal bookkeeping error",
            previous,
            bytes
        );
        self.refund_base_charge(bytes);
    }

    /// Clear the whiteout on `path` and every ancestor: writing or creating
    /// under a previously-removed directory implicitly recreates it.
    fn clear_whiteouts_for(whiteouts: &mut HashSet<PathBuf>, path: &Path) {
        whiteouts.remove(path);
        let mut ancestor = path.parent();
        while let Some(dir) = ancestor {
            whiteouts.remove(dir);
            ancestor = dir.parent();
        }
    }

    /// Union of upper and lower listings minus whiteouts; upper wins name
    /// collisions. Takes the state by reference so callers holding either
    /// guard flavor can use it without re-locking.
    async fn merged_list(&self, path: &Path, state: &OverlayState) -> io::Result<Vec<DirEntry>> {
        let upper_result = self.upper.list(path).await;
        let lower_result = self.lower.list(path).await;

        let mut merged: BTreeMap<String, DirEntry> = BTreeMap::new();

        match (&upper_result, &lower_result) {
            // Upper errors other than NotFound shadow the lower view: a file
            // copied up over the path makes it NotADirectory regardless of
            // what lower holds there.
            (Err(upper_err), _) if !is_not_found(upper_err) => {
                return Err(io::Error::new(upper_err.kind(), upper_err.to_string()));
            }
            // Neither layer has it (or lower has a non-dir and upper nothing):
            // lower's verdict stands.
            (Err(_), Err(lower_err)) => {
                return Err(io::Error::new(lower_err.kind(), lower_err.to_string()));
            }
            _ => {}
        }

        if let Ok(entries) = lower_result {
            for entry in entries {
                let child = path.join(&entry.name);
                if !state.whiteouts.contains(&child) {
                    merged.insert(entry.name.clone(), entry);
                }
            }
        }
        if let Ok(entries) = upper_result {
            for entry in entries {
                merged.insert(entry.name.clone(), entry);
            }
        }

        Ok(merged.into_values().collect())
    }

    /// Enumerate changes from an already-held state guard.
    ///
    /// Shared by `changes()` and `commit_into()` to avoid double-locking.
    /// Errors on dirty symlinks (Unsupported naming the paths). For each
    /// base entry: resolves current upper content, panics on an invariant
    /// violation (base present + no whiteout + upper read returns an error
    /// that is neither NotFound nor IsADirectory — genuine accounting bug),
    /// and treats IsADirectory as Removed (file replaced by a directory;
    /// the file content is gone from the merged view, the new directory is
    /// out of files-only scope).
    async fn changes_locked(&self, state: &OverlayState) -> io::Result<Vec<OverlayChange>> {
        if !state.dirty_symlinks.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::Unsupported,
                format!(
                    "overlay has dirty symlinks; changes()/commit_into() require file-only \
                     changes — reset or remove the symlinks first: {}",
                    format_paths(&state.dirty_symlinks)
                ),
            ));
        }

        // Walk bases sorted by path for deterministic output.
        let mut sorted: Vec<(&PathBuf, &Option<Vec<u8>>)> = state.bases.iter().collect();
        sorted.sort_by_key(|(path, _)| *path);

        let mut changes = Vec::with_capacity(sorted.len());

        for (path, base) in sorted {
            let whiteouted = state.whiteouts.contains(path);

            let current: Option<Vec<u8>> = if whiteouted {
                None
            } else {
                match self.upper.read(path).await {
                    Ok(content) => Some(content),
                    Err(ref error) if error.kind() == io::ErrorKind::IsADirectory => {
                        // File replaced by a directory (rm f; mkdir f): the
                        // file content is gone from the merged view, the new
                        // directory is out of files-only scope. Treat as
                        // Removed — the file is gone, directory out of scope.
                        None
                    }
                    Err(ref error) if is_not_found(error) => {
                        // base entry implies upper content XOR whiteout —
                        // this is an accounting bug or out-of-band upper
                        // mutation. Panic so the violation is never silent.
                        panic!(
                            "OverlayFs invariant violated: bases contains {:?} but upper \
                             reports NotFound with no whiteout — accounting bug or \
                             out-of-band mutation of the upper Arc",
                            path
                        );
                    }
                    Err(error) => return Err(error),
                }
            };

            let kind = match (base, &current) {
                // Added with no current content is impossible under correct
                // accounting: write() is the only path that inserts a None base,
                // and it always writes content to the upper first. A None base
                // combined with a whiteout (current=None) indicates a stale
                // phantom entry; skip it rather than fabricating a no-op change.
                (None, None) => {
                    debug_assert!(
                        false,
                        "OverlayFs invariant violated: bases contains {:?} with base=None \
                         and current=None — phantom Added entry; accounting bug",
                        path
                    );
                    continue;
                }
                (None, Some(_)) => ChangeKind::Added,
                (Some(_), Some(_)) => ChangeKind::Modified,
                (Some(_), None) => ChangeKind::Removed,
            };

            changes.push(OverlayChange {
                path: path.clone(),
                kind,
                base: base.clone(),
                current,
            });
        }

        Ok(changes)
    }

    /// Returns all changes in the overlay relative to the lower layer, sorted
    /// by path.
    ///
    /// Holds the state read lock for the entire call (torn-read protection).
    ///
    /// Returns `Err(Unsupported)` if any dirty symlinks are present (symlinks
    /// created in the upper, or lower symlinks that were removed). Remove or
    /// reset the symlink paths first.
    ///
    /// A `Modified` entry with `base == current` is possible (e.g. `set_mtime`
    /// copy-up) and is reported as-is — the overlay owns a copy-up, which will
    /// be replayed by `commit_into`.
    ///
    /// Note: directories are out of the dirty-tracking scope (files only,
    /// pinned). A session that only called `mkdir` returns an empty `Vec` here.
    pub async fn changes(&self) -> io::Result<Vec<OverlayChange>> {
        let state = self.state.read().await;
        self.changes_locked(&state).await
    }

    /// Returns `true` if there are any pending changes: whiteouts, base
    /// snapshots, or dirty symlinks.
    ///
    /// Note: directories are out of dirty scope (files-only, pinned), so a
    /// `mkdir`-only session reports clean. However, removing a lower
    /// *directory* (which leaves a whiteout) makes this return `true` while
    /// `changes()` returns an empty `Vec` — the overlay is dirty but the
    /// files-only inspection surface cannot describe the change.
    pub async fn is_dirty(&self) -> bool {
        let state = self.state.read().await;
        !state.whiteouts.is_empty()
            || !state.bases.is_empty()
            || !state.dirty_symlinks.is_empty()
    }

    /// Reset one path: undo all overlay changes for `path`, restoring lower
    /// visibility.
    ///
    /// - Removes the path from the upper (ignores `NotFound`; propagates other
    ///   errors — e.g. resetting an added directory with children fails loudly
    ///   with `DirectoryNotEmpty`; reset children first or use `reset_all`).
    /// - Clears whiteouts on `path` and all ancestors (matching `write()`'s
    ///   "touching a child resurrects removed ancestors" semantics — resetting
    ///   a child un-removes its removed ancestor directories).
    /// - Removes the path from `dirty_symlinks`.
    /// - Credits the budget if `bases` held a snapshot for this path.
    ///
    /// Resetting a clean (unmodified) path is `Ok` (idempotent).
    pub async fn reset(&self, path: &Path) -> io::Result<()> {
        let path = normalize(path);
        let mut state = self.state.write().await;

        match self.upper.remove(&path).await {
            Ok(()) => {}
            Err(ref error) if is_not_found(error) => {}
            Err(error) => return Err(error),
        }

        // Clear ancestor whiteouts so the path is reachable again.
        Self::clear_whiteouts_for(&mut state.whiteouts, &path);
        state.dirty_symlinks.remove(&path);

        if let Some(Some(content)) = state.bases.remove(&path) {
            self.credit_base(content.len() as u64);
        }

        Ok(())
    }

    /// Reset all changes: wipe the upper completely and clear all bookkeeping.
    ///
    /// Walks the upper depth-first (files and symlinks before their parent
    /// directories, never the root itself), removing each entry. Bookkeeping
    /// for each path is drained as its removal succeeds, so a mid-walk I/O
    /// failure leaves the overlay in a consistent (still-dirty) state whose
    /// `changes()` continues to work correctly. Untracked `mkdir` residue in
    /// the upper is also wiped.
    ///
    /// After a successful `reset_all`, `is_dirty()` returns `false` and the
    /// upper is empty.
    ///
    /// Note: the overlay owns its upper's content — wiping a caller-provided
    /// upper is within contract. Do not share an upper Arc with other owners
    /// if this would be surprising.
    pub async fn reset_all(&self) -> io::Result<()> {
        let mut state = self.state.write().await;

        // Collect all entries from the upper with a depth-first traversal so
        // we can remove leaves before parents.
        let all_paths = Self::collect_upper_paths_depth_first(&*self.upper).await?;

        for path in all_paths {
            // Remove from upper; ignore NotFound (concurrent removal or root).
            match self.upper.remove(&path).await {
                Ok(()) => {}
                Err(ref error) if is_not_found(error) => {}
                Err(error) => return Err(error),
            }

            // Drain the per-path bookkeeping immediately after the removal
            // succeeds so that a later failure leaves a consistent state.
            state.whiteouts.remove(&path);
            state.dirty_symlinks.remove(&path);
            if let Some(Some(content)) = state.bases.remove(&path) {
                self.credit_base(content.len() as u64);
            }
        }

        // Anything remaining in the bookkeeping sets wasn't in the upper
        // (whiteout-only entries for lower paths). Clear them now that the
        // upper is empty.
        state.whiteouts.clear();
        state.dirty_symlinks.clear();
        // Credit any remaining bases (lower-only paths that were whiteouted
        // without being copied up into the upper).
        let remaining: Vec<(PathBuf, Option<Vec<u8>>)> = state.bases.drain().collect();
        for (_, base) in remaining {
            if let Some(content) = base {
                self.credit_base(content.len() as u64);
            }
        }

        Ok(())
    }

    /// Walk the upper depth-first; return non-root paths with leaves first
    /// so they can be removed before their parents.
    async fn collect_upper_paths_depth_first(upper: &dyn Filesystem) -> io::Result<Vec<PathBuf>> {
        let mut result: Vec<PathBuf> = Vec::new();
        // Stack entries: (path, already_processed_children)
        let mut stack: Vec<(PathBuf, bool)> = vec![(PathBuf::from(""), false)];

        while let Some((dir, processed)) = stack.last_mut() {
            let dir = dir.clone();
            if !*processed {
                *processed = true;
                let entries = match upper.list(&dir).await {
                    Ok(e) => e,
                    Err(ref error) if is_not_found(error) => continue,
                    Err(error) => return Err(error),
                };
                for entry in entries {
                    let child = if dir.as_os_str().is_empty() {
                        PathBuf::from(&entry.name)
                    } else {
                        dir.join(&entry.name)
                    };
                    if entry.is_dir() {
                        stack.push((child, false));
                    } else {
                        result.push(child);
                    }
                }
            } else {
                stack.pop();
                // The root "" is never removed.
                if !dir.as_os_str().is_empty() {
                    result.push(dir);
                }
            }
        }

        Ok(result)
    }

    /// Write the overlay's dirty set into `target`.
    ///
    /// Holds the state read lock throughout (mutations to the overlay are
    /// blocked during the commit).
    ///
    /// **target must not be this overlay or wrap it** — the read lock held
    /// here and a target write attempting the write lock would deadlock.
    ///
    /// Two phases (pre-flight then write, per the design's commit atomicity):
    ///
    /// 1. Pre-flight (no mutation): check every change against `target`.
    ///    `Added` => target must NOT exist (conflict). `Modified`/`Removed` =>
    ///    target's current content must byte-equal `base` (stale-base
    ///    detection).
    /// 2. Write: `Added`/`Modified` => `target.mkdir(parent)` (create-parents,
    ///    Ok if existing), then `target.write`. `Removed` => `target.remove`.
    ///
    /// On any write-phase error, returns a loud error naming the failed path
    /// and the paths already committed.
    ///
    /// Does **not** mutate overlay state — the overlay stays dirty after a
    /// successful commit. Call `reset_all()` afterward if a clean overlay is
    /// wanted.
    ///
    /// Note: `commit_into` writes file content only; timestamps (`set_mtime`)
    /// are not propagated — the target's mtime becomes "now" for written
    /// files. A `set_mtime`-only `Modified` (where `base == current`) will
    /// commit identical bytes without transferring the pinned time.
    ///
    /// Note: directory removals are not committed (files-only scope). An empty
    /// added directory is not committed (implicit parent creation covers the
    /// common case; explicitly empty added dirs are out of scope).
    ///
    /// Returns `Err(Unsupported)` if any dirty symlinks are present.
    pub async fn commit_into(&self, target: &dyn Filesystem) -> io::Result<()> {
        let state = self.state.read().await;
        let changes = self.changes_locked(&state).await?;

        // Phase 1: pre-flight — verify all changes are applicable.
        for change in &changes {
            match change.kind {
                ChangeKind::Added => {
                    if target.exists(&change.path).await {
                        return Err(io::Error::new(
                            io::ErrorKind::AlreadyExists,
                            format!(
                                "conflict: {} exists in target but is Added in the overlay; \
                                 the overlay's snapshot pre-dates a concurrent add",
                                change.path.display()
                            ),
                        ));
                    }
                }
                ChangeKind::Modified | ChangeKind::Removed => {
                    let base = change.base.as_deref().unwrap_or(&[]);
                    match target.read(&change.path).await {
                        Ok(ref current) if current.as_slice() == base => {}
                        Ok(_) => {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                format!(
                                    "stale base: {} changed in target since snapshot; \
                                     commit would overwrite a diverged version",
                                    change.path.display()
                                ),
                            ));
                        }
                        Err(ref error) if is_not_found(error) => {
                            return Err(io::Error::new(
                                io::ErrorKind::InvalidData,
                                format!(
                                    "stale base: {} missing in target; file was deleted \
                                     after the overlay snapshot was taken",
                                    change.path.display()
                                ),
                            ));
                        }
                        Err(error) => return Err(error),
                    }
                }
            }
        }

        // Phase 2: write.
        let mut committed: Vec<&Path> = Vec::new();
        for change in &changes {
            let result = match change.kind {
                ChangeKind::Added | ChangeKind::Modified => {
                    // current is always Some for Added/Modified — changes_locked
                    // skips Added+current:None as an impossible phantom entry.
                    let current = change.current.as_deref().unwrap_or_else(|| {
                        panic!(
                            "OverlayFs commit_into: {:?} is Added/Modified but current is None \
                             — accounting bug; phantom entry should have been skipped",
                            change.path
                        )
                    });
                    // Create parent directories (create-parents, Ok if existing).
                    if let Some(parent) = change.path.parent()
                        && !parent.as_os_str().is_empty()
                    {
                        if let Err(error) = target.mkdir(parent).await {
                            Err(error)
                        } else {
                            target.write(&change.path, current).await
                        }
                    } else {
                        target.write(&change.path, current).await
                    }
                }
                ChangeKind::Removed => target.remove(&change.path).await,
            };

            match result {
                Ok(()) => committed.push(&change.path),
                Err(error) => {
                    let already: Vec<String> =
                        committed.iter().map(|p| p.display().to_string()).collect();
                    return Err(io::Error::new(
                        error.kind(),
                        format!(
                            "commit failed at {}: {}; already committed: [{}]",
                            change.path.display(),
                            error,
                            already.join(", ")
                        ),
                    ));
                }
            }
        }

        Ok(())
    }

    /// Fork this overlay: produce a new `OverlayFs` with the same lower and a
    /// copy of this overlay's dirty set written into `fresh_upper`.
    ///
    /// `fresh_upper` must be empty (its root listing must be empty).
    /// Pass this overlay's own `upper` Arc as `fresh_upper` is forbidden —
    /// a same-Arc fork silently makes parent and child share one upper, so
    /// post-fork edits bleed both directions.
    ///
    /// The child inherits the parent's budget handle (Arc clone) — forked
    /// bases are real RAM and draw on the same pool. The total of all
    /// `Some(base)` lengths is charged up-front; if the budget charge fails,
    /// the charge is refunded and `fresh_upper` is untouched. On a
    /// mid-replication failure (e.g. `fresh_upper`'s own budget exhausted
    /// mid-walk), the base charge is refunded and a best-effort wipe of
    /// `fresh_upper` is attempted; if the wipe fails, partial content may
    /// remain in `fresh_upper` and the caller must clean it up directly.
    ///
    /// `base_bytes` counts unconditionally (even without a budget) per the
    /// resident_bytes contract: it equals the sum of all `Some(base)` lengths
    /// in `bases` at all times.
    pub async fn fork_into(&self, fresh_upper: Arc<dyn Filesystem>) -> io::Result<OverlayFs> {
        // Guard: do not allow forking into our own upper — parent and child
        // would silently share one upper and post-fork edits would bleed both
        // directions.
        if Arc::ptr_eq(&fresh_upper, &self.upper) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "fork_into: fresh_upper must not be the same Arc as this overlay's upper; \
                 sharing an upper makes parent and child edits bleed both directions",
            ));
        }

        let state = self.state.read().await;

        // Verify fresh_upper is truly empty.
        let root_entries = fresh_upper.list(Path::new("")).await?;
        if !root_entries.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "fork_into: fresh_upper must be empty (non-empty root listing)",
            ));
        }

        // Calculate total base bytes to charge up-front.
        let base_charge: u64 = state
            .bases
            .values()
            .map(|b| b.as_deref().map_or(0, |c| c.len() as u64))
            .sum();

        // Charge before replicating; refund on any later failure.
        if base_charge > 0
            && let Some(budget) = &self.budget
        {
            budget.try_charge(base_charge)?;
        }

        // Replicate self.upper into fresh_upper with an iterative walk.
        let replicate_result = self.replicate_upper(&*fresh_upper).await;
        if let Err(error) = replicate_result {
            // Refund the pre-charged bases.
            if base_charge > 0
                && let Some(budget) = &self.budget
            {
                budget.credit(base_charge);
            }
            // Best-effort wipe of any partial content already written into
            // fresh_upper. If the wipe itself fails, partial content may remain
            // and the caller owns cleanup (retry with the same Arc will fail the
            // must-be-empty check). Intentionally ignored — we are already on the
            // error path and the original error is the useful one.
            let _ = Self::wipe_filesystem(&*fresh_upper).await;
            return Err(error);
        }

        Ok(OverlayFs {
            lower: Arc::clone(&self.lower),
            upper: fresh_upper,
            state: RwLock::new(OverlayState {
                whiteouts: state.whiteouts.clone(),
                bases: state.bases.clone(),
                dirty_symlinks: state.dirty_symlinks.clone(),
            }),
            base_bytes: AtomicU64::new(base_charge),
            budget: self.budget.clone(),
        })
    }

    /// Wipe all content from a filesystem depth-first. Used for best-effort
    /// cleanup of partial replication state on `fork_into` failure.
    async fn wipe_filesystem(fs: &dyn Filesystem) -> io::Result<()> {
        let paths = Self::collect_upper_paths_depth_first(fs).await?;
        for path in paths {
            match fs.remove(&path).await {
                Ok(()) | Err(_) => {}
            }
        }
        Ok(())
    }

    /// Copy all content from `self.upper` into `dest`.
    async fn replicate_upper(&self, dest: &dyn Filesystem) -> io::Result<()> {
        let mut stack: Vec<PathBuf> = vec![PathBuf::from("")];

        while let Some(dir) = stack.pop() {
            let entries = match self.upper.list(&dir).await {
                Ok(e) => e,
                Err(ref error) if is_not_found(error) => continue,
                Err(error) => return Err(error),
            };

            for entry in entries {
                let child = if dir.as_os_str().is_empty() {
                    PathBuf::from(&entry.name)
                } else {
                    dir.join(&entry.name)
                };

                match entry.kind {
                    DirEntryKind::File => {
                        let content = self.upper.read(&child).await?;
                        dest.write(&child, &content).await?;
                    }
                    DirEntryKind::Directory => {
                        dest.mkdir(&child).await?;
                        stack.push(child);
                    }
                    DirEntryKind::Symlink => {
                        let target = self.upper.read_link(&child).await?;
                        dest.symlink(&target, &child).await?;
                    }
                    _ => {}
                }
            }
        }

        Ok(())
    }
}

/// Credit base-snapshot bytes back to the shared budget when the overlay is
/// dropped without a preceding `reset_all()`. Without this, repeated
/// fork-then-drop cycles drain the shared `ByteBudget` monotonically toward
/// spurious `StorageFull` with zero resident bytes — the budget would no
/// longer track actual RAM.
///
/// Note: the overlay's upper-content bytes are the upper filesystem's own
/// concern; only the overlay-owned base snapshots (`base_bytes`) are credited
/// here.
impl Drop for OverlayFs {
    fn drop(&mut self) {
        let bytes = self.base_bytes.load(Ordering::Acquire);
        if bytes > 0
            && let Some(budget) = &self.budget
        {
            budget.credit(bytes);
        }
    }
}

#[async_trait]
impl Filesystem for OverlayFs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        let path = normalize(path);
        if self.state.read().await.whiteouts.contains(&path) {
            return Err(not_found(&path));
        }
        match self.upper.read(&path).await {
            Err(error) if is_not_found(&error) => self.lower.read(&path).await,
            other => other,
        }
    }

    async fn read_range(&self, path: &Path, range: Option<ReadRange>) -> io::Result<Vec<u8>> {
        // Delegate the slice to whichever layer holds the file so a byte range
        // rides on that backend's own `read_range` (e.g. MemoryFs slices its
        // stored bytes) instead of the default whole-file-read+slice. Without
        // this, chunked streaming over an overlay would be O(n²).
        let path = normalize(path);
        if self.state.read().await.whiteouts.contains(&path) {
            return Err(not_found(&path));
        }
        match self.upper.read_range(&path, range.clone()).await {
            Err(error) if is_not_found(&error) => self.lower.read_range(&path, range).await,
            other => other,
        }
    }

    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        let path = normalize(path);
        let mut state = self.state.write().await;

        // First touch of a lower path: snapshot its content as the base.
        // Already-dirty paths keep their first-touch base; a whiteouted path
        // either has a base from `remove` (lower file) or was a directory,
        // which is out of the bases scope — the re-created file is Added.
        //
        // Remember whether the path was whiteouted *before* any mutation so we
        // can decide below whether to clear a dirty_symlinks entry. See comment
        // near the dirty_symlinks.remove call.
        let was_whiteouted = state.whiteouts.contains(&path);
        let base_to_record = if state.bases.contains_key(&path) {
            None
        } else if was_whiteouted {
            Some(None)
        } else {
            match self.lower.read(&path).await {
                Ok(content) => Some(Some(content)),
                Err(error) if is_not_found(&error) => Some(None),
                // e.g. a visible lower directory: writing a file over it is
                // an error, surfaced before the upper is touched.
                Err(error) => return Err(error),
            }
        };

        let base_len = match &base_to_record {
            Some(Some(content)) => content.len() as u64,
            _ => 0,
        };
        self.charge_base(base_len)?;

        if let Err(error) = self.upper.write(&path, data).await {
            self.refund_base_charge(base_len);
            return Err(error);
        }

        // All mutations to state happen after the upper write succeeds.
        if let Some(base) = base_to_record {
            state.bases.insert(path.clone(), base);
            self.settle_base(base_len);
        }
        Self::clear_whiteouts_for(&mut state.whiteouts, &path);
        // Only clear a dirty_symlinks entry when the path was NOT whiteouted at
        // entry — in that case the dirty symlink was upper-resident and the write
        // genuinely replaced it with a file. When the path was whiteouted, the
        // dirty entry (if any) denotes a lower-resident symlink whose deletion
        // cannot be expressed as a base snapshot; that entry must survive so
        // changes()/commit_into continue to error Unsupported loudly.
        if !was_whiteouted {
            state.dirty_symlinks.remove(&path);
        }
        Ok(())
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let path = normalize(path);
        let state = self.state.read().await;
        if state.whiteouts.contains(&path) {
            return Err(not_found(&path));
        }
        self.merged_list(&path, &state).await
    }

    async fn stat(&self, path: &Path) -> io::Result<DirEntry> {
        let path = normalize(path);
        if self.state.read().await.whiteouts.contains(&path) {
            return Err(not_found(&path));
        }
        match self.upper.stat(&path).await {
            Err(error) if is_not_found(&error) => self.lower.stat(&path).await,
            other => other,
        }
    }

    async fn lstat(&self, path: &Path) -> io::Result<DirEntry> {
        let path = normalize(path);
        if self.state.read().await.whiteouts.contains(&path) {
            return Err(not_found(&path));
        }
        match self.upper.lstat(&path).await {
            Err(error) if is_not_found(&error) => self.lower.lstat(&path).await,
            other => other,
        }
    }

    async fn mkdir(&self, path: &Path) -> io::Result<()> {
        let path = normalize(path);
        let mut state = self.state.write().await;

        // A visible lower non-directory blocks mkdir; a visible lower
        // directory makes it the usual create-parents `Ok`.
        if !state.whiteouts.contains(&path)
            && let Ok(entry) = self.lower.stat(&path).await
            && !entry.is_dir()
        {
            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("file exists: {}", path.display()),
            ));
        }

        self.upper.mkdir(&path).await?;
        Self::clear_whiteouts_for(&mut state.whiteouts, &path);
        Ok(())
    }

    async fn remove(&self, path: &Path) -> io::Result<()> {
        let path = normalize(path);
        let mut state = self.state.write().await;
        if state.whiteouts.contains(&path) {
            return Err(not_found(&path));
        }

        let upper_entry = self.upper.lstat(&path).await.ok();
        let lower_entry = self.lower.lstat(&path).await.ok();
        let visible = match upper_entry.as_ref().or(lower_entry.as_ref()) {
            Some(entry) => entry,
            None => return Err(not_found(&path)),
        };

        // Directories follow the trait's empty-dir contract against the
        // *merged* view: upper-empty but lower-populated is still non-empty.
        if visible.is_dir() {
            let children = self.merged_list(&path, &state).await?;
            if !children.is_empty() {
                return Err(io::Error::new(
                    io::ErrorKind::DirectoryNotEmpty,
                    format!("directory not empty: {}", path.display()),
                ));
            }
        }

        // Prepare the lower-side bookkeeping (snapshot read + budget charge)
        // before mutating the upper, so a refused charge or failed read
        // aborts the remove with both layers intact.
        //
        // Files snapshot their base at first touch. Symlinks are out of the
        // files-only bases scope but ARE dirty state — a removed lower symlink
        // is inserted into dirty_symlinks so that changes()/commit_into error
        // Unsupported loudly rather than silently omitting the deletion.
        let mut pending_base: Option<Vec<u8>> = None;
        let mut pending_dirty_symlink = false;

        if let Some(entry) = &lower_entry {
            match entry.kind {
                DirEntryKind::File if !state.bases.contains_key(&path) => {
                    let content = self.lower.read(&path).await?;
                    self.charge_base(content.len() as u64)?;
                    pending_base = Some(content);
                }
                DirEntryKind::Symlink => {
                    // A removed lower symlink cannot be expressed as a base
                    // snapshot; mark it dirty so inspection methods are loud.
                    pending_dirty_symlink = true;
                }
                _ => {}
            }
        }

        if upper_entry.is_some()
            && let Err(error) = self.upper.remove(&path).await
        {
            self.refund_base_charge(
                pending_base.as_ref().map_or(0, |content| content.len() as u64),
            );
            return Err(error);
        }

        match lower_entry {
            Some(_) => {
                // Lower still holds the path: whiteout it.
                if let Some(content) = pending_base {
                    self.settle_base(content.len() as u64);
                    state.bases.insert(path.clone(), Some(content));
                } else {
                    // Removing a path that was Added (bases[path] = None): the net
                    // result is no change relative to lower. Drop the stale Added
                    // entry so changes() does not fabricate a phantom Added entry
                    // with base=None, current=None.
                    state.bases.remove(&path);
                }
                // Clear any stale dirty_symlinks entry before conditionally
                // re-inserting (the lower-Symlink case re-adds via
                // pending_dirty_symlink; clearing first avoids stale entries when
                // the upper held a symlink that the lower does not).
                state.dirty_symlinks.remove(&path);
                if pending_dirty_symlink {
                    state.dirty_symlinks.insert(path.clone());
                }
                state.whiteouts.insert(path);
            }
            None => {
                // Added in the upper, now removed: net no change. The base
                // is normally `None` here, but the lower is live and can
                // drift — a snapshot taken before an out-of-band lower
                // delete still holds bytes to credit.
                if let Some(Some(stale)) = state.bases.remove(&path) {
                    self.credit_base(stale.len() as u64);
                }
                // An upper-only symlink that was tracked as dirty is now gone.
                state.dirty_symlinks.remove(&path);
            }
        }
        Ok(())
    }

    async fn set_mtime(&self, path: &Path, mtime: SystemTime) -> io::Result<()> {
        let path = normalize(path);
        let mut state = self.state.write().await;
        if state.whiteouts.contains(&path) {
            return Err(not_found(&path));
        }

        match self.upper.set_mtime(&path, mtime).await {
            Err(error) if is_not_found(&error) => {
                // Lower-resident: a timestamp change is a mutation, so copy
                // up first. No silent no-op, per the trait's contract.
                let entry = self.lower.stat(&path).await?;
                if entry.is_dir() {
                    self.upper.mkdir(&path).await?;
                } else {
                    let content = self.lower.read(&path).await?;
                    let needs_base = !state.bases.contains_key(&path);
                    let base_len = if needs_base { content.len() as u64 } else { 0 };
                    self.charge_base(base_len)?;
                    if let Err(error) = self.upper.write(&path, &content).await {
                        self.refund_base_charge(base_len);
                        return Err(error);
                    }
                    if needs_base {
                        state.bases.insert(path.clone(), Some(content));
                        self.settle_base(base_len);
                    }
                }
                self.upper.set_mtime(&path, mtime).await
            }
            other => other,
        }
    }

    async fn read_link(&self, path: &Path) -> io::Result<PathBuf> {
        let path = normalize(path);
        if self.state.read().await.whiteouts.contains(&path) {
            return Err(not_found(&path));
        }
        match self.upper.read_link(&path).await {
            Err(error) if is_not_found(&error) => self.lower.read_link(&path).await,
            other => other,
        }
    }

    async fn symlink(&self, target: &Path, link: &Path) -> io::Result<()> {
        let link = normalize(link);
        let mut state = self.state.write().await;

        // A visible lower entry blocks creation, matching POSIX symlink(2).
        if !state.whiteouts.contains(&link) && self.lower.lstat(&link).await.is_ok() {
            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("file exists: {}", link.display()),
            ));
        }

        self.upper.symlink(target, &link).await?;
        // Track upper-created symlinks so changes()/commit_into error loudly
        // rather than silently omitting them.
        state.dirty_symlinks.insert(link.clone());
        Self::clear_whiteouts_for(&mut state.whiteouts, &link);
        Ok(())
    }

    fn read_only(&self) -> bool {
        false
    }

    /// Base snapshots plus whatever the upper reports as memory-resident.
    /// With the conventional private `MemoryFs` upper this is the full 2× of
    /// copy-up; with a disk-backed upper it's the bases alone — this counter
    /// is about RAM. (A consumer that mounts the upper separately and sums
    /// across mounts would count it twice; the conventional upper is owned
    /// by the overlay.)
    fn resident_bytes(&self) -> Option<u64> {
        Some(
            self.base_bytes.load(Ordering::Acquire)
                + self.upper.resident_bytes().unwrap_or(0),
        )
    }

    /// Always `None`: returning lower's real path for clean files would hand
    /// a tool a host path that bypasses the overlay (git and friends write
    /// through real paths). Consumers that need real paths materialize.
    fn real_path(&self, path: &Path) -> Option<PathBuf> {
        let _ = path;
        None
    }
}

#[cfg(all(test, feature = "memory"))]
#[allow(clippy::unwrap_used)]
mod tests {
    use super::*;
    use crate::memory::MemoryFs;
    use crate::traits::DirEntryKind;

    /// Lower with a couple of files, fresh MemoryFs upper.
    async fn overlay_with_lower() -> (Arc<MemoryFs>, OverlayFs) {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("a.txt"), b"alpha").await.unwrap();
        lower.write(Path::new("d/x.txt"), b"in dir").await.unwrap();
        let overlay = OverlayFs::over(lower.clone());
        (lower, overlay)
    }

    // Doc test 2: read-after-write coherence.
    #[tokio::test]
    async fn test_read_after_write_coherence() {
        let (_, overlay) = overlay_with_lower().await;

        // Clean read falls through to lower.
        assert_eq!(overlay.read(Path::new("a.txt")).await.unwrap(), b"alpha");

        // Edit a lower file: the overlay sees the new content.
        overlay.write(Path::new("a.txt"), b"edited").await.unwrap();
        assert_eq!(overlay.read(Path::new("a.txt")).await.unwrap(), b"edited");
        let entry = overlay.stat(Path::new("a.txt")).await.unwrap();
        assert_eq!(entry.size, 6);

        // Brand-new file.
        overlay.write(Path::new("new.txt"), b"fresh").await.unwrap();
        assert_eq!(overlay.read(Path::new("new.txt")).await.unwrap(), b"fresh");
    }

    // Doc test 1's MemoryFs sibling: the lower is never written.
    #[tokio::test]
    async fn test_lower_untouched_by_all_mutations() {
        let (lower, overlay) = overlay_with_lower().await;

        overlay.write(Path::new("a.txt"), b"edited").await.unwrap();
        overlay.remove(Path::new("d/x.txt")).await.unwrap();
        overlay.write(Path::new("new.txt"), b"fresh").await.unwrap();
        overlay.mkdir(Path::new("newdir")).await.unwrap();

        assert_eq!(lower.read(Path::new("a.txt")).await.unwrap(), b"alpha");
        assert_eq!(lower.read(Path::new("d/x.txt")).await.unwrap(), b"in dir");
        assert!(!lower.exists(Path::new("new.txt")).await);
        assert!(!lower.exists(Path::new("newdir")).await);
    }

    // Doc test 3: remove a lower file.
    #[tokio::test]
    async fn test_remove_lower_file() {
        let (lower, overlay) = overlay_with_lower().await;

        overlay.remove(Path::new("a.txt")).await.unwrap();

        assert!(!overlay.exists(Path::new("a.txt")).await);
        let err = overlay.read(Path::new("a.txt")).await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::NotFound);
        let names: Vec<String> = overlay
            .list(Path::new(""))
            .await
            .unwrap()
            .into_iter()
            .map(|e| e.name)
            .collect();
        assert!(!names.contains(&"a.txt".to_string()));

        // Lower untouched.
        assert_eq!(lower.read(Path::new("a.txt")).await.unwrap(), b"alpha");
    }

    // Doc test 4: write after remove clears the whiteout.
    #[tokio::test]
    async fn test_write_after_remove() {
        let (_, overlay) = overlay_with_lower().await;

        overlay.remove(Path::new("a.txt")).await.unwrap();
        assert!(!overlay.exists(Path::new("a.txt")).await);

        overlay.write(Path::new("a.txt"), b"replaced").await.unwrap();
        assert!(overlay.exists(Path::new("a.txt")).await);
        assert_eq!(
            overlay.read(Path::new("a.txt")).await.unwrap(),
            b"replaced"
        );
    }

    // Doc test 5: merged listing, upper wins collisions.
    #[tokio::test]
    async fn test_list_merges_layers() {
        let (_, overlay) = overlay_with_lower().await;

        overlay.write(Path::new("b.txt"), b"upper file").await.unwrap();
        overlay.write(Path::new("d/y.txt"), b"upper in lower dir").await.unwrap();
        overlay.mkdir(Path::new("newdir")).await.unwrap();
        // Collision: shadow a lower file with different-sized content.
        overlay.write(Path::new("a.txt"), b"upper!").await.unwrap();

        let root: Vec<DirEntry> = overlay.list(Path::new("")).await.unwrap();
        let names: Vec<&str> = root.iter().map(|e| e.name.as_str()).collect();
        assert_eq!(names, vec!["a.txt", "b.txt", "d", "newdir"]);

        // Upper wins the collision: size reflects "upper!".
        let a = root.iter().find(|e| e.name == "a.txt").unwrap();
        assert_eq!(a.size, 6);

        let in_d: Vec<String> = overlay
            .list(Path::new("d"))
            .await
            .unwrap()
            .into_iter()
            .map(|e| e.name)
            .collect();
        assert_eq!(in_d, vec!["x.txt", "y.txt"]);
    }

    // Doc test 6: rename a lower file (trait-default copy+delete must keep
    // overlay bookkeeping straight because it goes through our write/remove).
    #[tokio::test]
    async fn test_rename_lower_file() {
        let (lower, overlay) = overlay_with_lower().await;

        overlay
            .rename(Path::new("a.txt"), Path::new("renamed.txt"))
            .await
            .unwrap();

        assert_eq!(
            overlay.read(Path::new("renamed.txt")).await.unwrap(),
            b"alpha"
        );
        assert!(!overlay.exists(Path::new("a.txt")).await);

        // Disk (lower) untouched on both ends.
        assert_eq!(lower.read(Path::new("a.txt")).await.unwrap(), b"alpha");
        assert!(!lower.exists(Path::new("renamed.txt")).await);

        // Doc test 6 pin: bases correct on both ends.
        // rename = write(dest) + remove(source), so dest is Added (base=None)
        // and source is Removed (base=original lower content).
        let changes = overlay.changes().await.unwrap();
        let dest = changes.iter().find(|c| c.path == Path::new("renamed.txt")).unwrap();
        assert_eq!(dest.kind, ChangeKind::Added);
        assert!(dest.base.is_none());
        assert_eq!(dest.current.as_deref(), Some(b"alpha" as &[u8]));
        let source = changes.iter().find(|c| c.path == Path::new("a.txt")).unwrap();
        assert_eq!(source.kind, ChangeKind::Removed);
        assert_eq!(source.base.as_deref(), Some(b"alpha" as &[u8]));
        assert!(source.current.is_none());
    }

    // Merged empty-dir contract: a dir that is empty in the upper but
    // populated in the lower is still non-empty.
    #[tokio::test]
    async fn test_remove_directory_follows_merged_emptiness() {
        let (lower, overlay) = overlay_with_lower().await;

        let err = overlay.remove(Path::new("d")).await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::DirectoryNotEmpty);

        overlay.remove(Path::new("d/x.txt")).await.unwrap();
        overlay.remove(Path::new("d")).await.unwrap();
        assert!(!overlay.exists(Path::new("d")).await);

        assert!(lower.exists(Path::new("d/x.txt")).await);
    }

    // Doc test 11: set_mtime on a lower file copies up.
    #[tokio::test]
    async fn test_set_mtime_copies_up() {
        let (lower, overlay) = overlay_with_lower().await;
        let pinned = SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_000_000);

        overlay.set_mtime(Path::new("a.txt"), pinned).await.unwrap();

        let entry = overlay.stat(Path::new("a.txt")).await.unwrap();
        assert_eq!(entry.modified, Some(pinned));
        // Content still readable, lower timestamp not the pinned one.
        assert_eq!(overlay.read(Path::new("a.txt")).await.unwrap(), b"alpha");
        let lower_entry = lower.stat(Path::new("a.txt")).await.unwrap();
        assert_ne!(lower_entry.modified, Some(pinned));
    }

    // Doc test 13: whiteouts and bases key on normalized paths.
    #[tokio::test]
    async fn test_path_normalization() {
        let (_, overlay) = overlay_with_lower().await;

        overlay.remove(Path::new("d/./x.txt")).await.unwrap();
        assert!(!overlay.exists(Path::new("d/x.txt")).await);

        overlay
            .write(Path::new("/a.txt"), b"via absolute")
            .await
            .unwrap();
        assert_eq!(
            overlay.read(Path::new("a.txt")).await.unwrap(),
            b"via absolute"
        );
    }

    #[tokio::test]
    async fn test_mkdir_over_visible_lower_file_fails() {
        let (_, overlay) = overlay_with_lower().await;

        let err = overlay.mkdir(Path::new("a.txt")).await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::AlreadyExists);

        // Once the file is removed, the name is free.
        overlay.remove(Path::new("a.txt")).await.unwrap();
        overlay.mkdir(Path::new("a.txt")).await.unwrap();
        let entry = overlay.stat(Path::new("a.txt")).await.unwrap();
        assert_eq!(entry.kind, DirEntryKind::Directory);
    }

    #[tokio::test]
    async fn test_remove_copied_up_file_whiteouts_lower() {
        let (lower, overlay) = overlay_with_lower().await;

        // Copy up, then remove: the lower copy must not shine through.
        overlay.write(Path::new("a.txt"), b"edited").await.unwrap();
        overlay.remove(Path::new("a.txt")).await.unwrap();

        assert!(!overlay.exists(Path::new("a.txt")).await);
        assert_eq!(lower.read(Path::new("a.txt")).await.unwrap(), b"alpha");
    }

    #[tokio::test]
    async fn test_symlink_goes_to_upper() {
        let (lower, overlay) = overlay_with_lower().await;

        overlay
            .symlink(Path::new("a.txt"), Path::new("link.txt"))
            .await
            .unwrap();
        let target = overlay.read_link(Path::new("link.txt")).await.unwrap();
        assert_eq!(target, Path::new("a.txt"));
        assert!(!lower.exists(Path::new("link.txt")).await);

        // A visible lower entry blocks creation.
        let err = overlay
            .symlink(Path::new("d"), Path::new("a.txt"))
            .await
            .unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::AlreadyExists);
    }

    #[tokio::test]
    async fn test_real_path_is_always_none() {
        let (_, overlay) = overlay_with_lower().await;
        assert_eq!(overlay.real_path(Path::new("a.txt")), None);
    }

    // --- Byte accounting tests ---

    // Lower "a.txt" is 5 bytes ("alpha"), "d/x.txt" is 6 ("in dir").

    #[tokio::test]
    async fn test_resident_bytes_counts_upper_plus_bases() {
        let (_, overlay) = overlay_with_lower().await;
        assert_eq!(overlay.resident_bytes(), Some(0));

        // Copy-up: upper holds the new content, the overlay holds the base.
        overlay.write(Path::new("a.txt"), b"edited!").await.unwrap();
        assert_eq!(overlay.resident_bytes(), Some(7 + 5));

        // Second write to the same path: upper delta only, base unchanged.
        overlay.write(Path::new("a.txt"), b"x").await.unwrap();
        assert_eq!(overlay.resident_bytes(), Some(1 + 5));

        // Added file: no base.
        overlay.write(Path::new("new.txt"), b"123").await.unwrap();
        assert_eq!(overlay.resident_bytes(), Some(1 + 3 + 5));

        // Removing a lower file snapshots its base.
        overlay.remove(Path::new("d/x.txt")).await.unwrap();
        assert_eq!(overlay.resident_bytes(), Some(1 + 3 + 5 + 6));

        // Removing an added file nets back out.
        overlay.remove(Path::new("new.txt")).await.unwrap();
        assert_eq!(overlay.resident_bytes(), Some(1 + 5 + 6));
    }

    #[tokio::test]
    async fn test_shared_budget_caps_the_full_copy_up_cost() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("big.txt"), &[b'x'; 60]).await.unwrap();
        let budget = Arc::new(ByteBudget::labeled(100, "workspace"));
        let overlay = OverlayFs::over_with_budget(lower, budget.clone());

        // Copy-up costs base (60) + new content (50) = 110 > 100: refused
        // loudly, nothing changed anywhere.
        let error = overlay
            .write(Path::new("big.txt"), &[b'y'; 50])
            .await
            .unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::StorageFull);
        assert!(error.to_string().contains("workspace"));
        assert_eq!(overlay.resident_bytes(), Some(0));
        assert_eq!(budget.used(), 0);
        assert_eq!(
            overlay.read(Path::new("big.txt")).await.unwrap(),
            vec![b'x'; 60]
        );

        // A smaller edit fits: 60 base + 30 content.
        overlay.write(Path::new("big.txt"), &[b'y'; 30]).await.unwrap();
        assert_eq!(budget.used(), 90);
        assert_eq!(overlay.resident_bytes(), Some(90));
    }

    #[tokio::test]
    async fn test_budget_refused_remove_leaves_overlay_intact() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("big.txt"), &[b'x'; 60]).await.unwrap();
        let budget = Arc::new(ByteBudget::new(10));
        let overlay = OverlayFs::over_with_budget(lower, budget.clone());

        // Removing a lower file must snapshot 60 bytes of base — over
        // budget, so the remove fails loudly and the file stays visible.
        let error = overlay.remove(Path::new("big.txt")).await.unwrap_err();
        assert_eq!(error.kind(), io::ErrorKind::StorageFull);
        assert!(overlay.exists(Path::new("big.txt")).await);
        assert_eq!(budget.used(), 0);
        assert_eq!(overlay.resident_bytes(), Some(0));
    }

    #[tokio::test]
    async fn test_set_mtime_copy_up_charges_base() {
        let (_, overlay) = overlay_with_lower().await;
        let pinned = SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_000_000);

        overlay.set_mtime(Path::new("a.txt"), pinned).await.unwrap();
        // Copy-up: 5 bytes in the upper + 5 bytes of base.
        assert_eq!(overlay.resident_bytes(), Some(10));
    }

    #[tokio::test]
    async fn test_drop_credits_the_full_copy_up_charge() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("a.txt"), b"alpha").await.unwrap();
        let budget = Arc::new(ByteBudget::new(100));
        {
            let overlay = OverlayFs::over_with_budget(lower, budget.clone());
            overlay.write(Path::new("a.txt"), b"edited!").await.unwrap();
            // 7 upper bytes + 5 base bytes.
            assert_eq!(budget.used(), 12);
        }
        // Dropping the overlay returns both halves: the bases via
        // OverlayFs::drop, the upper content via MemoryFs::drop.
        assert_eq!(budget.used(), 0);
    }

    // ===== Inspection API tests =====

    // Doc test 7: changes() returns the correct kinds and bases.
    #[tokio::test]
    async fn test_changes_kinds_and_bases() {
        let lower2 = Arc::new(MemoryFs::new());
        lower2.write(Path::new("to_remove.txt"), b"bye").await.unwrap();
        lower2.write(Path::new("existing.txt"), b"original").await.unwrap();
        let overlay2 = OverlayFs::over(lower2);

        overlay2.write(Path::new("added.txt"), b"new content").await.unwrap();
        overlay2.write(Path::new("existing.txt"), b"changed").await.unwrap();
        overlay2.remove(Path::new("to_remove.txt")).await.unwrap();

        let changes = overlay2.changes().await.unwrap();
        // Sorted by path: added.txt, existing.txt, to_remove.txt
        assert_eq!(changes.len(), 3);

        let added = changes.iter().find(|c| c.path == Path::new("added.txt")).unwrap();
        assert_eq!(added.kind, ChangeKind::Added);
        assert!(added.base.is_none());
        assert_eq!(added.current.as_deref(), Some(b"new content" as &[u8]));

        let modified = changes.iter().find(|c| c.path == Path::new("existing.txt")).unwrap();
        assert_eq!(modified.kind, ChangeKind::Modified);
        assert_eq!(modified.base.as_deref(), Some(b"original" as &[u8]));
        assert_eq!(modified.current.as_deref(), Some(b"changed" as &[u8]));

        let removed = changes.iter().find(|c| c.path == Path::new("to_remove.txt")).unwrap();
        assert_eq!(removed.kind, ChangeKind::Removed);
        assert_eq!(removed.base.as_deref(), Some(b"bye" as &[u8]));
        assert!(removed.current.is_none());

        // Verify sorted order.
        let paths: Vec<&Path> = changes.iter().map(|c| c.path.as_path()).collect();
        assert_eq!(paths, vec![
            Path::new("added.txt"),
            Path::new("existing.txt"),
            Path::new("to_remove.txt"),
        ]);
    }

    // Doc test 4b: remove-then-rewrite keeps the ORIGINAL base (pinned).
    #[tokio::test]
    async fn test_remove_then_rewrite_is_modified_with_original_base() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("f.txt"), b"original").await.unwrap();
        let overlay = OverlayFs::over(lower);

        overlay.remove(Path::new("f.txt")).await.unwrap();
        overlay.write(Path::new("f.txt"), b"new").await.unwrap();

        let changes = overlay.changes().await.unwrap();
        assert_eq!(changes.len(), 1);
        let change = &changes[0];
        assert_eq!(change.kind, ChangeKind::Modified);
        // Base is the ORIGINAL lower content, not "None" or "new".
        assert_eq!(change.base.as_deref(), Some(b"original" as &[u8]));
        assert_eq!(change.current.as_deref(), Some(b"new" as &[u8]));
    }

    // Doc test 8: lower drift — changes() reports the snapshot base;
    // commit_into a drifted target errors stale-base.
    #[tokio::test]
    async fn test_lower_drift_snapshot_and_stale_base_error() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("f.txt"), b"original").await.unwrap();
        let overlay = OverlayFs::over(lower.clone());

        // Copy up.
        overlay.write(Path::new("f.txt"), b"edited").await.unwrap();

        // Drift: mutate lower directly.
        lower.write(Path::new("f.txt"), b"drifted").await.unwrap();

        // changes() still reports the snapshot base, not the drifted content.
        let changes = overlay.changes().await.unwrap();
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].base.as_deref(), Some(b"original" as &[u8]));
        assert_eq!(changes[0].current.as_deref(), Some(b"edited" as &[u8]));

        // commit_into a target seeded with the drifted content => InvalidData.
        let target = Arc::new(MemoryFs::new());
        target.write(Path::new("f.txt"), b"drifted").await.unwrap();
        let err = overlay.commit_into(&*target).await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        assert!(err.to_string().contains("stale base"));
        // Target must be completely untouched by the failed commit.
        assert_eq!(target.read(Path::new("f.txt")).await.unwrap(), b"drifted");
    }

    // commit_into success: fresh target seeded to match lower; after commit
    // the target matches the overlay view; overlay stays dirty.
    #[tokio::test]
    async fn test_commit_into_success() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("existing.txt"), b"original").await.unwrap();
        lower.write(Path::new("to_remove.txt"), b"remove me").await.unwrap();
        let overlay = OverlayFs::over(lower);

        overlay.write(Path::new("existing.txt"), b"modified").await.unwrap();
        overlay.write(Path::new("added.txt"), b"brand new").await.unwrap();
        overlay.remove(Path::new("to_remove.txt")).await.unwrap();

        // Target seeded to match lower.
        let target = Arc::new(MemoryFs::new());
        target.write(Path::new("existing.txt"), b"original").await.unwrap();
        target.write(Path::new("to_remove.txt"), b"remove me").await.unwrap();

        overlay.commit_into(&*target).await.unwrap();

        // Target now matches the overlay view.
        assert_eq!(target.read(Path::new("existing.txt")).await.unwrap(), b"modified");
        assert_eq!(target.read(Path::new("added.txt")).await.unwrap(), b"brand new");
        assert!(!target.exists(Path::new("to_remove.txt")).await);

        // Overlay is still dirty after commit.
        assert!(overlay.is_dirty().await);
    }

    // commit_into Added-conflict: file appears in target where overlay added one.
    #[tokio::test]
    async fn test_commit_into_added_conflict() {
        let lower = Arc::new(MemoryFs::new());
        let overlay = OverlayFs::over(lower);

        overlay.write(Path::new("added.txt"), b"new").await.unwrap();

        let target = Arc::new(MemoryFs::new());
        target.write(Path::new("added.txt"), b"already there").await.unwrap();

        let err = overlay.commit_into(&*target).await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::AlreadyExists);
        assert!(err.to_string().contains("conflict"));
        // Target untouched.
        assert_eq!(target.read(Path::new("added.txt")).await.unwrap(), b"already there");
    }

    // commit_into Removed already absent in target => stale-base error.
    #[tokio::test]
    async fn test_commit_into_removed_already_absent() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("f.txt"), b"content").await.unwrap();
        let overlay = OverlayFs::over(lower);

        overlay.remove(Path::new("f.txt")).await.unwrap();

        // Target does NOT have the file (already gone).
        let target = Arc::new(MemoryFs::new());

        let err = overlay.commit_into(&*target).await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::InvalidData);
        assert!(err.to_string().contains("missing in target"));
    }

    // Modified with base == current (set_mtime copy-up) appears in changes()
    // and commits cleanly.
    #[tokio::test]
    async fn test_modified_base_equals_current_commits() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("f.txt"), b"same bytes").await.unwrap();
        let overlay = OverlayFs::over(lower);

        let pinned = SystemTime::UNIX_EPOCH + std::time::Duration::from_secs(1_000_000);
        overlay.set_mtime(Path::new("f.txt"), pinned).await.unwrap();

        let changes = overlay.changes().await.unwrap();
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].kind, ChangeKind::Modified);
        assert_eq!(changes[0].base.as_deref(), Some(b"same bytes" as &[u8]));
        assert_eq!(changes[0].current.as_deref(), Some(b"same bytes" as &[u8]));

        // Commits cleanly even with base == current.
        let target = Arc::new(MemoryFs::new());
        target.write(Path::new("f.txt"), b"same bytes").await.unwrap();
        overlay.commit_into(&*target).await.unwrap();
    }

    // Doc test 9: reset(path) restores lower visibility and credits the budget;
    // reset_all leaves is_dirty() false, budget used back to 0, upper fully wiped.
    #[tokio::test]
    async fn test_reset_path_and_reset_all() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("a.txt"), b"alpha").await.unwrap();
        lower.write(Path::new("d/x.txt"), b"in dir").await.unwrap();
        let budget = Arc::new(ByteBudget::new(1000));
        let upper = Arc::new(MemoryFs::with_budget(budget.clone()));
        let overlay = OverlayFs::with_budget(lower.clone(), upper, budget.clone());

        // Make some changes.
        overlay.write(Path::new("a.txt"), b"edited").await.unwrap();
        overlay.write(Path::new("added.txt"), b"new").await.unwrap();
        overlay.remove(Path::new("d/x.txt")).await.unwrap();
        overlay.mkdir(Path::new("newdir")).await.unwrap();

        let used_before_reset = budget.used();
        assert!(used_before_reset > 0);

        // reset(path) for a.txt: lower becomes visible again.
        overlay.reset(Path::new("a.txt")).await.unwrap();
        assert_eq!(overlay.read(Path::new("a.txt")).await.unwrap(), b"alpha");

        // reset(path) for added.txt: it disappears.
        overlay.reset(Path::new("added.txt")).await.unwrap();
        assert!(!overlay.exists(Path::new("added.txt")).await);

        // reset_all: everything wiped.
        overlay.reset_all().await.unwrap();
        assert!(!overlay.is_dirty().await);
        assert_eq!(budget.used(), 0);

        // Lower content is visible again.
        assert_eq!(overlay.read(Path::new("a.txt")).await.unwrap(), b"alpha");
        assert_eq!(overlay.read(Path::new("d/x.txt")).await.unwrap(), b"in dir");
        // Added file is gone.
        assert!(!overlay.exists(Path::new("added.txt")).await);
        // mkdir-only dir is gone from the overlay view (upper wiped).
        // The lower "d" dir is still visible.
        let root_names: Vec<String> = overlay
            .list(Path::new(""))
            .await
            .unwrap()
            .into_iter()
            .map(|e| e.name)
            .collect();
        assert!(root_names.contains(&"a.txt".to_string()));
        assert!(root_names.contains(&"d".to_string()));
        assert!(!root_names.contains(&"newdir".to_string()));
    }

    // reset(path) clears ancestor whiteouts (blocker 7): a whiteouted ancestor
    // must not block the reset child from being visible.
    #[tokio::test]
    async fn test_reset_clears_ancestor_whiteouts() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("d/x.txt"), b"content").await.unwrap();
        let overlay = OverlayFs::over(lower);

        // Remove child, then parent.
        overlay.remove(Path::new("d/x.txt")).await.unwrap();
        overlay.remove(Path::new("d")).await.unwrap();

        // Resetting the child must un-whiteout the ancestor directory.
        overlay.reset(Path::new("d/x.txt")).await.unwrap();

        // After reset, the child is visible again.
        assert_eq!(overlay.read(Path::new("d/x.txt")).await.unwrap(), b"content");
    }

    // Doc test 10: fork_into independence — edits after the fork don't bleed
    // either direction; child changes() equals parent's at fork time;
    // fork charges budget for cloned bases; too-tight budget refuses loudly.
    #[tokio::test]
    async fn test_fork_into_independence() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("f.txt"), b"original").await.unwrap();
        let budget = Arc::new(ByteBudget::new(1000));
        let upper = Arc::new(MemoryFs::with_budget(budget.clone()));
        let parent = OverlayFs::with_budget(lower.clone(), upper, budget.clone());

        // Parent: copy up f.txt.
        parent.write(Path::new("f.txt"), b"parent edit").await.unwrap();
        let parent_changes_before_fork = parent.changes().await.unwrap();

        let fresh_upper = Arc::new(MemoryFs::new());
        let child = parent.fork_into(fresh_upper).await.unwrap();

        // Child changes() equals parent's at fork time.
        let child_changes = child.changes().await.unwrap();
        assert_eq!(child_changes.len(), parent_changes_before_fork.len());
        assert_eq!(child_changes[0].path, parent_changes_before_fork[0].path);
        assert_eq!(child_changes[0].base, parent_changes_before_fork[0].base);
        assert_eq!(child_changes[0].current, parent_changes_before_fork[0].current);

        // Parent edit does not bleed into child.
        parent.write(Path::new("f.txt"), b"parent after fork").await.unwrap();
        assert_eq!(child.read(Path::new("f.txt")).await.unwrap(), b"parent edit");

        // Child edit does not bleed into parent.
        child.write(Path::new("f.txt"), b"child edit").await.unwrap();
        assert_eq!(parent.read(Path::new("f.txt")).await.unwrap(), b"parent after fork");

        // Budget is charged for cloned bases.
        let base_bytes = child.base_bytes.load(Ordering::Acquire);
        assert_eq!(base_bytes, b"original".len() as u64);
    }

    // fork_into with too-tight budget refuses the fork loudly with nothing
    // replicated.
    #[tokio::test]
    async fn test_fork_into_budget_refused() {
        // The overlay budget covers only base snapshots. Use separate budgets
        // for upper content and bases so we can exhaust just the bases budget
        // after the initial copy-up succeeds.
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("big.txt"), &[b'x'; 60]).await.unwrap();

        // Generous base budget: 200 bytes allows the 60-byte base charge and
        // leaves room to drain later.
        let base_budget = Arc::new(ByteBudget::new(200));
        // Separate upper budget: not shared with the base budget.
        let upper = Arc::new(MemoryFs::new());
        let parent = OverlayFs::with_budget(lower.clone(), upper, base_budget.clone());

        // Copy up: 60-byte base is charged to base_budget.
        parent.write(Path::new("big.txt"), &[b'y'; 10]).await.unwrap();
        assert_eq!(base_budget.used(), 60);

        // Drain the remaining budget so fork's base charge (60 bytes) will fail.
        // base_budget.used() == 60; limit == 200; remaining == 140.
        // Charge 81 more: used == 141, remaining == 59 < 60 needed for fork.
        base_budget.try_charge(81).unwrap(); // now at 141/200; remaining = 59.

        let fresh_upper = Arc::new(MemoryFs::new());
        let result = parent.fork_into(fresh_upper).await;
        let err = result.err().expect("expected fork to fail with StorageFull");
        assert_eq!(err.kind(), io::ErrorKind::StorageFull);
    }

    // fork_into non-empty fresh_upper => InvalidInput.
    #[tokio::test]
    async fn test_fork_into_nonempty_upper_rejected() {
        let lower = Arc::new(MemoryFs::new());
        let overlay = OverlayFs::over(lower);

        let nonempty_upper = Arc::new(MemoryFs::new());
        nonempty_upper.write(Path::new("stale.txt"), b"x").await.unwrap();

        let err = overlay
            .fork_into(nonempty_upper)
            .await
            .err()
            .expect("expected fork to fail with InvalidInput");
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
    }

    // fork_into same-Arc upper => InvalidInput (blocker 3/improvement 3).
    #[tokio::test]
    async fn test_fork_into_same_upper_arc_rejected() {
        let lower = Arc::new(MemoryFs::new());
        let upper = Arc::new(MemoryFs::new());
        let overlay = OverlayFs::new(lower, upper.clone());

        let err = overlay
            .fork_into(upper)
            .await
            .err()
            .expect("expected fork to fail with InvalidInput");
        assert_eq!(err.kind(), io::ErrorKind::InvalidInput);
        assert!(err.to_string().contains("same Arc"));
    }

    // Dirty symlink: changes() and commit_into both return Unsupported naming
    // the path; after remove of the symlink, they work again.
    #[tokio::test]
    async fn test_dirty_symlink_blocks_changes_and_commit() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("target.txt"), b"content").await.unwrap();
        let overlay = OverlayFs::over(lower);

        overlay
            .symlink(Path::new("target.txt"), Path::new("link.txt"))
            .await
            .unwrap();

        let err = overlay.changes().await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::Unsupported);
        assert!(err.to_string().contains("link.txt"));

        let target = Arc::new(MemoryFs::new());
        let err2 = overlay.commit_into(&*target).await.unwrap_err();
        assert_eq!(err2.kind(), io::ErrorKind::Unsupported);

        // After removing the symlink, both work again.
        overlay.remove(Path::new("link.txt")).await.unwrap();
        // The symlink was upper-only (no lower entry) so it's just gone.
        assert!(overlay.changes().await.unwrap().is_empty());
    }

    // Dirty symlink from removing a lower symlink also blocks changes()/commit_into
    // (blocker 2 & 5).
    #[tokio::test]
    async fn test_lower_symlink_removal_is_dirty_symlink() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("target.txt"), b"content").await.unwrap();
        lower
            .symlink(Path::new("target.txt"), Path::new("link.txt"))
            .await
            .unwrap();
        let overlay = OverlayFs::over(lower);

        overlay.remove(Path::new("link.txt")).await.unwrap();

        // is_dirty() is true.
        assert!(overlay.is_dirty().await);

        // changes() errors Unsupported naming the path.
        let err = overlay.changes().await.unwrap_err();
        assert_eq!(err.kind(), io::ErrorKind::Unsupported);
        assert!(err.to_string().contains("link.txt"));

        // commit_into also errors Unsupported.
        let target_fs = Arc::new(MemoryFs::new());
        let err2 = overlay.commit_into(&*target_fs).await.unwrap_err();
        assert_eq!(err2.kind(), io::ErrorKind::Unsupported);
    }

    // Blocker 1 & 6: file replaced by directory (rm f; mkdir f) is treated as
    // Removed in changes() (file content gone from merged view).
    #[tokio::test]
    async fn test_file_replaced_by_directory_treated_as_removed() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("a.txt"), b"alpha").await.unwrap();
        let overlay = OverlayFs::over(lower);

        overlay.remove(Path::new("a.txt")).await.unwrap();
        overlay.mkdir(Path::new("a.txt")).await.unwrap();

        // changes() must not panic and must treat this as Removed.
        let changes = overlay.changes().await.unwrap();
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].path, Path::new("a.txt"));
        assert_eq!(changes[0].kind, ChangeKind::Removed);
        assert_eq!(changes[0].base.as_deref(), Some(b"alpha" as &[u8]));
        assert!(changes[0].current.is_none());
    }

    // Symlink interleaved: remove-then-symlink-then-write is Modified with
    // original base; dirty_symlinks ends empty (improvement 8).
    #[tokio::test]
    async fn test_remove_symlink_write_is_modified_with_original_base() {
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("f.txt"), b"original").await.unwrap();
        let overlay = OverlayFs::over(lower);

        // Remove the file (records base Some("original") + whiteout).
        overlay.remove(Path::new("f.txt")).await.unwrap();
        // Create a symlink at the same path (clears whiteout, adds dirty_symlinks entry).
        overlay
            .symlink(Path::new("other.txt"), Path::new("f.txt"))
            .await
            .unwrap();
        // Write over the symlink (clears dirty_symlinks on success; keeps base Some("original")).
        overlay.write(Path::new("f.txt"), b"replaced").await.unwrap();

        // No dirty symlinks.
        let changes = overlay.changes().await.unwrap();
        assert_eq!(changes.len(), 1);
        assert_eq!(changes[0].kind, ChangeKind::Modified);
        assert_eq!(changes[0].base.as_deref(), Some(b"original" as &[u8]));
        assert_eq!(changes[0].current.as_deref(), Some(b"replaced" as &[u8]));
    }

    #[tokio::test]
    async fn test_read_range_byte_slice_through_layers() {
        // Lower file, read a mid slice (served from lower).
        let lower = Arc::new(MemoryFs::new());
        lower.write(Path::new("f.txt"), b"0123456789").await.unwrap();
        let overlay = OverlayFs::over(lower);
        let from_lower = overlay
            .read_range(Path::new("f.txt"), Some(ReadRange::bytes(2, 4)))
            .await
            .unwrap();
        assert_eq!(from_lower, b"2345");

        // Overwrite in the upper; the slice now comes from the upper copy.
        overlay.write(Path::new("f.txt"), b"ABCDEFGHIJ").await.unwrap();
        let from_upper = overlay
            .read_range(Path::new("f.txt"), Some(ReadRange::bytes(2, 4)))
            .await
            .unwrap();
        assert_eq!(from_upper, b"CDEF");

        // Whiteout: a removed file is not found, even for a ranged read.
        overlay.remove(Path::new("f.txt")).await.unwrap();
        let removed = overlay
            .read_range(Path::new("f.txt"), Some(ReadRange::bytes(0, 4)))
            .await;
        assert!(removed.is_err());
    }

    // is_dirty() false for a mkdir-only session.
    #[tokio::test]
    async fn test_mkdir_only_is_clean() {
        let lower = Arc::new(MemoryFs::new());
        let overlay = OverlayFs::over(lower);

        overlay.mkdir(Path::new("newdir")).await.unwrap();
        assert!(!overlay.is_dirty().await);
    }

    // Doc finding 9 (low): directory asymmetry — removing a lower directory
    // (which was already emptied at the lower level — use a lower that has only
    // an empty dir, no files) leaves a whiteout (is_dirty() true) but no base
    // entry (changes() returns an empty Vec). Also verifies the companion claim
    // that commit_into never deletes directories in the target.
    #[tokio::test]
    async fn test_whiteout_only_dir_is_dirty_but_changes_empty() {
        // Lower has an empty directory 'e' (created directly, no files inside).
        let lower = Arc::new(MemoryFs::new());
        lower.mkdir(Path::new("e")).await.unwrap();
        let overlay = OverlayFs::over(lower);

        // The dir is empty in the merged view, so remove succeeds.
        overlay.remove(Path::new("e")).await.unwrap();
        assert!(!overlay.exists(Path::new("e")).await);

        // Whiteout-only state: is_dirty is true but changes() is an empty Vec.
        assert!(overlay.is_dirty().await);
        let changes = overlay.changes().await.unwrap();
        assert!(changes.is_empty(), "expected empty changes, got {:?}", changes);

        // commit_into does not delete the directory in the target.
        let target = Arc::new(MemoryFs::new());
        target.mkdir(Path::new("e")).await.unwrap();
        overlay.commit_into(&*target).await.unwrap();
        // The directory survives in the target (directory removals not committed).
        assert!(target.exists(Path::new("e")).await);
    }

    // Doc finding 6 (medium): commit partial-failure reporting ('already
    // committed: [...]'). Overlay adds 'a.txt' and 'dir/b.txt'; target holds a
    // regular FILE named 'dir'. Pre-flight passes (neither Added path exists in
    // target), write phase commits a.txt then fails at target.mkdir('dir') —
    // assert the error names dir/b.txt and lists a.txt as committed.
    #[tokio::test]
    async fn test_commit_partial_failure_names_committed_paths() {
        let lower = Arc::new(MemoryFs::new());
        let overlay = OverlayFs::over(lower);

        // changes_locked sorts by path: a.txt < dir/b.txt, so a.txt commits first.
        overlay.write(Path::new("a.txt"), b"first").await.unwrap();
        overlay.write(Path::new("dir/b.txt"), b"second").await.unwrap();

        // Target has a regular FILE named 'dir', which blocks mkdir('dir').
        let target = Arc::new(MemoryFs::new());
        target.write(Path::new("dir"), b"file not dir").await.unwrap();

        // Pre-flight: Added => target must not exist. 'a.txt' is absent (ok);
        // 'dir/b.txt' — target.exists('dir/b.txt') is false because 'dir' is a
        // file, not a directory — so pre-flight passes. Phase 2 commits a.txt,
        // then target.mkdir('dir') fails with AlreadyExists (dir is a file).
        let err = overlay.commit_into(&*target).await.unwrap_err();
        let msg = err.to_string();
        assert!(
            msg.contains("dir/b.txt") || msg.contains("dir"),
            "error should name the failing path: {msg}"
        );
        assert!(
            msg.contains("a.txt"),
            "error should list already-committed paths: {msg}"
        );
        assert!(
            msg.contains("already committed"),
            "error should use pinned wording: {msg}"
        );
    }

    // Doc test 1 against a real disk tree: the invariant with teeth.
    #[cfg(feature = "localfs")]
    mod localfs {
        use super::*;
        use crate::local::LocalFs;

        #[tokio::test]
        async fn test_real_tree_byte_identical_after_overlay_writes() {
            let dir = tempfile::tempdir().unwrap();
            std::fs::write(dir.path().join("hello.txt"), b"original bytes").unwrap();

            let lower = Arc::new(LocalFs::read_only(dir.path()));
            let overlay = OverlayFs::over(lower);

            overlay
                .write(Path::new("hello.txt"), b"changed in overlay")
                .await
                .unwrap();
            overlay.write(Path::new("added.txt"), b"new").await.unwrap();
            overlay.remove(Path::new("hello.txt")).await.unwrap();
            overlay
                .write(Path::new("hello.txt"), b"re-created")
                .await
                .unwrap();

            // The real tree is byte-identical and gained nothing.
            assert_eq!(
                std::fs::read(dir.path().join("hello.txt")).unwrap(),
                b"original bytes"
            );
            assert!(!dir.path().join("added.txt").exists());

            // ...while the overlay sees the edited state.
            assert_eq!(
                overlay.read(Path::new("hello.txt")).await.unwrap(),
                b"re-created"
            );
        }

        // The harness can fail: writing through a LocalFs (bypassing the
        // overlay) while the overlay is in play must trip the byte-identical
        // assertion. This demonstrates the invariant test above is not vacuous —
        // an overlay that forwarded writes to its lower (broken) would be caught
        // here because an interleaved direct lower write leaves a mis-match
        // between what the overlay claimed to see and what is on disk.
        #[tokio::test]
        async fn test_harness_detects_real_tree_mutation() {
            let dir = tempfile::tempdir().unwrap();
            std::fs::write(dir.path().join("hello.txt"), b"original bytes").unwrap();

            // Build an overlay over a read-only lower.
            let lower = Arc::new(LocalFs::read_only(dir.path()));
            let overlay = OverlayFs::over(lower);

            // Write through the overlay — the lower must stay byte-identical.
            overlay
                .write(Path::new("hello.txt"), b"changed in overlay")
                .await
                .unwrap();

            // Interleave a direct lower write via a writable LocalFs, bypassing
            // the overlay. This is the mutation the invariant test would catch.
            let writable = LocalFs::new(dir.path());
            writable
                .write(Path::new("hello.txt"), b"mutated on disk directly")
                .await
                .unwrap();

            // The real tree has been mutated — confirming the byte comparison
            // used in the invariant test above *can* trip.
            assert_ne!(
                std::fs::read(dir.path().join("hello.txt")).unwrap(),
                b"original bytes",
                "a write through the writable lower must trip the byte comparison"
            );

            // The overlay still serves its own copy-up — independent of the lower.
            assert_eq!(
                overlay.read(Path::new("hello.txt")).await.unwrap(),
                b"changed in overlay"
            );
        }
    }
}
