//! Copy-on-write overlay filesystem.
//!
//! Writes land in an upper layer; the lower layer is never touched. The
//! overlay tracks exactly what changed (whiteouts for deletions, base
//! snapshots for first-touch content) so a consumer can later render a
//! patch, commit, fork, or discard. Design: `docs/kaish-overlayfs.md`.

use crate::paths::normalize;
use crate::traits::{DirEntry, Filesystem};
use async_trait::async_trait;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;
use tokio::sync::RwLock;

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
}

#[derive(Default)]
struct OverlayState {
    /// Paths deleted relative to lower. A later write clears the whiteout.
    whiteouts: HashSet<PathBuf>,
    /// First-touch snapshots: lower content at the moment a path first became
    /// dirty. `None` = the path did not exist in lower (it's an Added file).
    /// Files only — directories and symlinks are out of the changes scope.
    bases: HashMap<PathBuf, Option<Vec<u8>>>,
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

impl OverlayFs {
    /// Overlay `upper` (writable) on top of `lower` (never written).
    pub fn new(lower: Arc<dyn Filesystem>, upper: Arc<dyn Filesystem>) -> Self {
        Self {
            lower,
            upper,
            state: RwLock::new(OverlayState::default()),
        }
    }

    /// Overlay a fresh in-memory upper on top of `lower` — the conventional
    /// pairing: zero disk writes, strictly ephemeral.
    #[cfg(feature = "memory")]
    pub fn over(lower: Arc<dyn Filesystem>) -> Self {
        Self::new(lower, Arc::new(crate::memory::MemoryFs::new()))
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

    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        let path = normalize(path);
        let mut state = self.state.write().await;

        // First touch of a lower path: snapshot its content as the base.
        // Already-dirty paths keep their first-touch base; a whiteouted path
        // either has a base from `remove` (lower file) or was a directory,
        // which is out of the bases scope — the re-created file is Added.
        let base_to_record = if state.bases.contains_key(&path) {
            None
        } else if state.whiteouts.contains(&path) {
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

        self.upper.write(&path, data).await?;

        if let Some(base) = base_to_record {
            state.bases.insert(path.clone(), base);
        }
        Self::clear_whiteouts_for(&mut state.whiteouts, &path);
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

        if upper_entry.is_some() {
            self.upper.remove(&path).await?;
        }

        match lower_entry {
            Some(entry) => {
                // Lower still holds the path: whiteout it. Files snapshot
                // their base at first touch; directories and symlinks are
                // out of the bases scope (files-only, pinned in the design).
                if entry.kind == crate::traits::DirEntryKind::File
                    && !state.bases.contains_key(&path)
                {
                    let content = self.lower.read(&path).await?;
                    state.bases.insert(path.clone(), Some(content));
                }
                state.whiteouts.insert(path);
            }
            None => {
                // Added in the upper, now removed: net no change.
                state.bases.remove(&path);
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
                    if !state.bases.contains_key(&path) {
                        state.bases.insert(path.clone(), Some(content.clone()));
                    }
                    self.upper.write(&path, &content).await?;
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
        Self::clear_whiteouts_for(&mut state.whiteouts, &link);
        Ok(())
    }

    fn read_only(&self) -> bool {
        false
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

        // The harness can fail: the byte-identical assertion above is not
        // vacuous. Write to the real tree (no overlay protecting it) and
        // confirm the same comparison trips.
        #[tokio::test]
        async fn test_harness_detects_real_tree_mutation() {
            let dir = tempfile::tempdir().unwrap();
            std::fs::write(dir.path().join("hello.txt"), b"original bytes").unwrap();

            let writable = LocalFs::new(dir.path());
            writable
                .write(Path::new("hello.txt"), b"mutated on disk")
                .await
                .unwrap();

            assert_ne!(
                std::fs::read(dir.path().join("hello.txt")).unwrap(),
                b"original bytes",
                "a write to an unprotected lower must trip the byte comparison"
            );
        }
    }
}
