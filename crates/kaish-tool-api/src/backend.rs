//! The `KernelBackend` trait — kaish's abstract I/O and tool-dispatch layer.
//!
//! The trait lives here (not in `kaish-types`) because it is async and refers
//! to [`ToolCtx`](crate::ToolCtx). Its data types (errors, results, ops) and
//! the concrete implementations (`LocalBackend`, overlay, …) live elsewhere —
//! the data in `kaish-types::backend`, the impls in `kaish-kernel`.

use std::path::{Path, PathBuf};

use async_trait::async_trait;

use kaish_types::backend::{
    BackendError, BackendResult, MountInfo, PatchOp, ReadRange, ToolInfo, ToolResult, WriteMode,
};
use kaish_types::{DirEntry, PathAccess, ToolArgs};

use crate::ctx::ToolCtx;

/// Abstract backend interface for file operations and tool dispatch.
///
/// Implementations select where a path resolves and how tools are dispatched:
/// - `LocalBackend` — VfsRouter-backed local filesystem (the default).
/// - `KaijutsuBackend` — CRDT-backed blocks when embedded in kaijutsu.
#[async_trait]
pub trait KernelBackend: Send + Sync {
    // ═══════════════════════════════════════════════════════════════════════
    // File Operations
    // ═══════════════════════════════════════════════════════════════════════

    /// Read file contents, optionally with a range specification.
    async fn read(&self, path: &Path, range: Option<ReadRange>) -> BackendResult<Vec<u8>>;

    /// Write content to a file with the specified mode.
    async fn write(&self, path: &Path, content: &[u8], mode: WriteMode) -> BackendResult<()>;

    /// Append content to a file.
    async fn append(&self, path: &Path, content: &[u8]) -> BackendResult<()>;

    /// Apply a sequence of patch operations to a file.
    ///
    /// Operations apply in order to one snapshot of the file, and the result
    /// is written once. If an operation fails — a CAS `expected` mismatch, an
    /// offset or line number past the end — the batch stops before the write
    /// and the file keeps every byte it had. A caller holding an error never
    /// has to work out how much of the batch survived.
    ///
    /// The snapshot accumulates, so each operation sees the edits before it:
    /// offsets and line numbers are relative to the content as patched so far,
    /// not to the file as it was on entry. An insert at line 1 shifts the line
    /// numbers every later operation in the same batch uses.
    ///
    /// The promise covers the operations, not the write that follows them.
    /// Persisting the result is `write`'s business, with whatever crash,
    /// concurrency, and I/O-error behavior the implementation gives it — a
    /// write that fails partway through can still leave a partial file.
    async fn patch(&self, path: &Path, ops: &[PatchOp]) -> BackendResult<()>;

    /// List a directory's entries.
    async fn list(&self, path: &Path) -> BackendResult<Vec<DirEntry>>;

    /// Stat a path (following symlinks).
    async fn stat(&self, path: &Path) -> BackendResult<DirEntry>;

    /// Create a directory.
    async fn mkdir(&self, path: &Path) -> BackendResult<()>;

    /// Set the modification time of an existing path.
    ///
    /// Read-only or purely-virtual mounts reject rather than silently
    /// succeeding — `touch` on an existing file must route through here, never
    /// escape to the host via `resolve_real_path`.
    async fn set_mtime(&self, path: &Path, mtime: std::time::SystemTime) -> BackendResult<()>;

    /// Remove a file, directory, or symlink; `recursive` descends into a
    /// directory. The final component is never followed: a symlink is
    /// unlinked and its target kept, and a link to a directory is not
    /// descended into.
    async fn remove(&self, path: &Path, recursive: bool) -> BackendResult<()>;

    /// Rename/move a path. Neither side follows a final symlink: a symlink
    /// source moves as a link, and a symlink at the destination is replaced,
    /// never written through.
    async fn rename(&self, from: &Path, to: &Path) -> BackendResult<()>;

    /// Whether a path exists, following symlinks: a dangling link does not
    /// exist, and an error reads as `false`. Use `lstat` to ask whether a
    /// link is present.
    async fn exists(&self, path: &Path) -> bool;

    /// Stat a path without following symlinks.
    async fn lstat(&self, path: &Path) -> BackendResult<DirEntry>;

    /// Read a symlink's target as stored, without resolving it.
    async fn read_link(&self, path: &Path) -> BackendResult<PathBuf>;

    /// Create a symlink at `link` pointing to `target`. The target is stored
    /// verbatim; a relative target resolves from the link's directory. An
    /// absolute target is rewritten relative to the link when both are on one
    /// mount, and refused when they are not.
    async fn symlink(&self, target: &Path, link: &Path) -> BackendResult<()>;

    /// Resolve `path` to its canonical form: follow every symlink hop, fold
    /// `.` and `..` lexically. The final component may be missing when
    /// `allow_missing_final` is true (GNU `readlink -f` semantics); a
    /// missing INTERMEDIATE component is always an error. Symlink hops are
    /// capped at 40, matching Linux `MAXSYMLINKS`; exceeding the cap is an
    /// error, never a silent stop.
    ///
    /// The default walks component by component through
    /// [`KernelBackend::lstat`] and [`KernelBackend::read_link`], so it
    /// inherits whatever containment those already give. `LocalBackend`
    /// overrides this to delegate straight to the VFS layer's single-shot
    /// resolver instead of one round trip per hop.
    async fn canonicalize(&self, path: &Path, allow_missing_final: bool) -> BackendResult<PathBuf> {
        let components: Vec<_> = path.components().collect();
        let total = components.len();
        let mut current = PathBuf::new();

        for (idx, component) in components.iter().enumerate() {
            let is_last = idx + 1 == total;
            match component {
                std::path::Component::RootDir => {}
                std::path::Component::CurDir => {}
                std::path::Component::ParentDir => {
                    current.pop();
                }
                std::path::Component::Normal(_) => {
                    current.push(component);
                    current =
                        resolve_symlink_hop(self, current, is_last && allow_missing_final).await?;
                }
                std::path::Component::Prefix(_) => {
                    current.push(component);
                }
            }
        }
        Ok(current)
    }

    // ═══════════════════════════════════════════════════════════════════════
    // Tool Dispatch
    // ═══════════════════════════════════════════════════════════════════════

    /// Call a tool by name with the given arguments and execution context.
    ///
    /// For local backends, this executes the tool directly via ToolRegistry.
    /// For remote backends (e.g. kaijutsu), this may serialize the call and
    /// forward it to the parent process.
    async fn call_tool(
        &self,
        name: &str,
        args: ToolArgs,
        ctx: &mut dyn ToolCtx,
    ) -> BackendResult<ToolResult>;

    /// List available external tools.
    async fn list_tools(&self) -> BackendResult<Vec<ToolInfo>>;

    /// Get information about a specific tool.
    async fn get_tool(&self, name: &str) -> BackendResult<Option<ToolInfo>>;

    // ═══════════════════════════════════════════════════════════════════════
    // Backend Information
    // ═══════════════════════════════════════════════════════════════════════

    /// Returns true if this backend is read-only.
    fn read_only(&self) -> bool;

    /// What the kernel can do with one path: the query behind `test -r`,
    /// `test -w`, and `test -x`.
    ///
    /// Neither [`KernelBackend::read_only`] nor `DirEntry.permissions`
    /// answers "can this path be written" alone. A read-only wrapper over an
    /// OS-writable directory reports permissive mode bits and refuses every
    /// write; a `DevFs` mount reports `read_only() == false` (so `>
    /// /dev/null` works) while its `/dev` directory accepts nothing.
    /// [`PathAccess::resolve`] combines the two, and is the only way to build
    /// a `PathAccess` — a caller cannot consult one fact by accident.
    ///
    /// The default answers from `stat` plus this backend's whole-backend
    /// `read_only()`, which is right for a backend that is uniformly
    /// read-only or uniformly writable. A backend whose mounts differ —
    /// `LocalBackend`, which routes through a `VfsRouter` — overrides this to
    /// ask the mount that owns the path.
    ///
    /// Errors exactly as `stat` does: a path that does not exist is an error,
    /// not a `PathAccess` of all-false.
    async fn path_access(&self, path: &Path) -> BackendResult<PathAccess> {
        let entry = self.stat(path).await?;
        Ok(PathAccess::resolve(entry.permissions, self.read_only()))
    }

    /// Returns the backend type identifier (e.g. "local", "kaijutsu").
    fn backend_type(&self) -> &str;

    /// List all mount points.
    fn mounts(&self) -> Vec<MountInfo>;

    /// Resolve a VFS path to a real filesystem path.
    ///
    /// Returns `Some(path)` if the VFS path maps to a real filesystem (like
    /// LocalFs), or `None` if the path is virtual (like MemoryFs). Tools like
    /// `git` that hand paths to external C libraries need the real path.
    fn resolve_real_path(&self, path: &Path) -> Option<PathBuf>;
}

/// Symlink hops [`KernelBackend::canonicalize`]'s default walk follows
/// before refusing, matching Linux's `MAXSYMLINKS`.
const MAX_SYMLINK_HOPS: usize = 40;

/// Follow the symlink chain starting at `path`, if any, to the entry it
/// names. `allow_missing` permits `path` itself to be absent; every hop
/// short of it must exist.
async fn resolve_symlink_hop<B: KernelBackend + ?Sized>(
    backend: &B,
    path: PathBuf,
    allow_missing: bool,
) -> BackendResult<PathBuf> {
    let mut current = path;
    for _ in 0..MAX_SYMLINK_HOPS {
        match backend.lstat(&current).await {
            Ok(entry) if entry.is_symlink() => {
                let target = backend.read_link(&current).await?;
                current = if target.is_absolute() {
                    target
                } else {
                    let parent = current.parent().unwrap_or(Path::new(""));
                    parent.join(target)
                };
                current = fold_dots(current);
            }
            Ok(_) => return Ok(current),
            Err(BackendError::NotFound(_)) if allow_missing => return Ok(current),
            Err(e) => return Err(e),
        }
    }
    Err(BackendError::InvalidOperation(format!(
        "too many levels of symbolic links: {}",
        current.display()
    )))
}

/// Collapse `.` and `..` lexically in a path: `..` past the start is
/// dropped, not accumulated, matching the VFS layer's own clamp-at-root
/// rule for a root-relative path.
fn fold_dots(path: PathBuf) -> PathBuf {
    let mut out = PathBuf::new();
    for component in path.components() {
        match component {
            std::path::Component::ParentDir => {
                out.pop();
            }
            std::path::Component::CurDir => {}
            other => out.push(other),
        }
    }
    out
}
