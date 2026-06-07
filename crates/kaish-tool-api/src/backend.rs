//! The `KernelBackend` trait — kaish's abstract I/O and tool-dispatch layer.
//!
//! The trait lives here (not in `kaish-types`) because it is async and refers
//! to [`ToolCtx`](crate::ToolCtx). Its data types (errors, results, ops) and
//! the concrete implementations (`LocalBackend`, overlay, …) live elsewhere —
//! the data in `kaish-types::backend`, the impls in `kaish-kernel`.

use std::path::{Path, PathBuf};

use async_trait::async_trait;

use kaish_types::backend::{
    BackendResult, MountInfo, PatchOp, ReadRange, ToolInfo, ToolResult, WriteMode,
};
use kaish_types::{DirEntry, ToolArgs};

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

    /// Remove a file or directory.
    async fn remove(&self, path: &Path, recursive: bool) -> BackendResult<()>;

    /// Rename/move a path.
    async fn rename(&self, from: &Path, to: &Path) -> BackendResult<()>;

    /// Whether a path exists.
    async fn exists(&self, path: &Path) -> bool;

    /// Stat a path without following symlinks.
    async fn lstat(&self, path: &Path) -> BackendResult<DirEntry>;

    /// Read a symlink's target.
    async fn read_link(&self, path: &Path) -> BackendResult<PathBuf>;

    /// Create a symlink.
    async fn symlink(&self, target: &Path, link: &Path) -> BackendResult<()>;

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
