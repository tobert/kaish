//! KernelBackend trait for abstracting kaish's I/O layer.
//!
//! This module defines the `KernelBackend` trait which provides a unified interface
//! for file operations and tool dispatch. Two implementations are provided:
//!
//! - `LocalBackend`: Default implementation wrapping VfsRouter for local filesystem access
//! - (Future) `KaijutsuBackend`: CRDT-backed blocks when running under kaijutsu
//!
//! # Architecture
//!
//! ```text
//! Builtins (cat, ls, echo, etc.)
//!     ↓
//! ctx.backend: Arc<dyn KernelBackend>
//!     ↓
//! ┌─────────────────────────────────────────────┐
//! │  LocalBackend (default)  │  KaijutsuBackend │
//! │  - wraps VfsRouter       │  - CRDT blocks   │
//! │  - local ToolRegistry    │  - parent tools  │
//! └─────────────────────────────────────────────┘
//! ```

mod local;
mod overlay;

pub use local::LocalBackend;
pub use overlay::VirtualOverlayBackend;

#[cfg(test)]
pub mod testing;

#[cfg(test)]
pub use testing::MockBackend;

use async_trait::async_trait;
use std::path::{Path, PathBuf};

// Data types re-exported from kaish-types.
pub use kaish_types::backend::{
    BackendError, BackendResult, ConflictError, PatchOp, ReadRange, ToolInfo, ToolResult, WriteMode,
};

use crate::tools::{ExecContext, ToolArgs};
use crate::vfs::{DirEntry, MountInfo};

/// Abstract backend interface for file operations and tool dispatch.
///
/// This trait abstracts kaish's I/O layer, enabling different backends:
/// - `LocalBackend`: Default implementation using VfsRouter
/// - `KaijutsuBackend`: CRDT-backed implementation for collaborative editing
#[async_trait]
pub trait KernelBackend: Send + Sync {
    // ═══════════════════════════════════════════════════════════════════════════
    // File Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// Read file contents, optionally with a range specification.
    async fn read(&self, path: &Path, range: Option<ReadRange>) -> BackendResult<Vec<u8>>;

    /// Write content to a file with the specified mode.
    async fn write(&self, path: &Path, content: &[u8], mode: WriteMode) -> BackendResult<()>;

    /// Append content to a file.
    async fn append(&self, path: &Path, content: &[u8]) -> BackendResult<()>;

    /// Apply patch operations to a file.
    ///
    /// Patch operations support compare-and-set (CAS) for conflict detection.
    /// If an operation's `expected` field doesn't match the actual content,
    /// returns `BackendError::Conflict`.
    async fn patch(&self, path: &Path, ops: &[PatchOp]) -> BackendResult<()>;

    // ═══════════════════════════════════════════════════════════════════════════
    // Directory Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// List directory contents.
    async fn list(&self, path: &Path) -> BackendResult<Vec<DirEntry>>;

    /// Get file or directory metadata.
    async fn stat(&self, path: &Path) -> BackendResult<DirEntry>;

    /// Create a directory (and parent directories if needed).
    async fn mkdir(&self, path: &Path) -> BackendResult<()>;

    /// Remove a file or directory.
    ///
    /// If `recursive` is true, removes directories and their contents.
    async fn remove(&self, path: &Path, recursive: bool) -> BackendResult<()>;

    /// Rename (move) a file or directory.
    ///
    /// This is an atomic operation when source and destination are on the same
    /// filesystem. Cross-mount renames are not supported.
    async fn rename(&self, from: &Path, to: &Path) -> BackendResult<()>;

    /// Check if a path exists.
    async fn exists(&self, path: &Path) -> bool;

    // ═══════════════════════════════════════════════════════════════════════════
    // Symlink Operations
    // ═══════════════════════════════════════════════════════════════════════════

    /// Get metadata for a path without following symlinks.
    ///
    /// Unlike `stat`, this returns metadata about the symlink itself,
    /// not the target it points to.
    async fn lstat(&self, path: &Path) -> BackendResult<DirEntry>;

    /// Read the target of a symbolic link.
    ///
    /// Returns the path the symlink points to without following it.
    async fn read_link(&self, path: &Path) -> BackendResult<PathBuf>;

    /// Create a symbolic link.
    ///
    /// Creates a symlink at `link` pointing to `target`.
    async fn symlink(&self, target: &Path, link: &Path) -> BackendResult<()>;

    // ═══════════════════════════════════════════════════════════════════════════
    // Tool Dispatch
    // ═══════════════════════════════════════════════════════════════════════════

    /// Call a tool by name with the given arguments and execution context.
    ///
    /// For local backends, this executes the tool directly via ToolRegistry.
    /// For remote backends (e.g., kaijutsu), this may serialize the call
    /// and forward it to the parent process.
    async fn call_tool(
        &self,
        name: &str,
        args: ToolArgs,
        ctx: &mut ExecContext,
    ) -> BackendResult<ToolResult>;

    /// List available external tools.
    async fn list_tools(&self) -> BackendResult<Vec<ToolInfo>>;

    /// Get information about a specific tool.
    async fn get_tool(&self, name: &str) -> BackendResult<Option<ToolInfo>>;

    // ═══════════════════════════════════════════════════════════════════════════
    // Backend Information
    // ═══════════════════════════════════════════════════════════════════════════

    /// Returns true if this backend is read-only.
    fn read_only(&self) -> bool;

    /// Returns the backend type identifier (e.g., "local", "kaijutsu").
    fn backend_type(&self) -> &str;

    /// List all mount points.
    fn mounts(&self) -> Vec<MountInfo>;

    /// Resolve a VFS path to a real filesystem path.
    ///
    /// Returns `Some(path)` if the VFS path maps to a real filesystem (like LocalFs),
    /// or `None` if the path is in a virtual filesystem (like MemoryFs).
    ///
    /// This is needed for tools like `git` that must use real paths with external libraries.
    fn resolve_real_path(&self, path: &Path) -> Option<std::path::PathBuf>;
}

#[cfg(test)]
mod tests {
    use super::*;
    use super::testing::MockBackend;
    use std::sync::atomic::Ordering;
    use std::sync::Arc;

    #[test]
    fn test_backend_error_from_io_error() {
        let not_found = std::io::Error::new(std::io::ErrorKind::NotFound, "file not found");
        let backend_err: BackendError = not_found.into();
        assert!(matches!(backend_err, BackendError::NotFound(_)));

        let permission = std::io::Error::new(std::io::ErrorKind::PermissionDenied, "no access");
        let backend_err: BackendError = permission.into();
        assert!(matches!(backend_err, BackendError::PermissionDenied(_)));
    }

    #[test]
    fn test_dir_entry_constructors() {
        let dir = DirEntry::directory("mydir");
        assert!(dir.is_dir());
        assert_eq!(dir.name, "mydir");

        let file = DirEntry::file("myfile.txt", 1024);
        assert!(file.is_file());
        assert_eq!(file.size, 1024);
    }

    #[test]
    fn test_tool_result() {
        let success = ToolResult::success("hello");
        assert!(success.ok());
        assert_eq!(success.stdout, "hello");

        let failure = ToolResult::failure(1, "error");
        assert!(!failure.ok());
        assert_eq!(failure.code, 1);
    }

    #[test]
    fn test_read_range() {
        let lines = ReadRange::lines(10, 20);
        assert_eq!(lines.start_line, Some(10));
        assert_eq!(lines.end_line, Some(20));

        let bytes = ReadRange::bytes(100, 50);
        assert_eq!(bytes.offset, Some(100));
        assert_eq!(bytes.limit, Some(50));
    }

    #[tokio::test]
    async fn test_mock_backend_call_tool_routing() {
        let (backend, call_count) = MockBackend::new();
        let backend: Arc<dyn KernelBackend> = Arc::new(backend);
        let mut ctx = ExecContext::with_backend(backend.clone());

        // Verify initial count is 0
        assert_eq!(call_count.load(Ordering::SeqCst), 0);

        // Call tool through backend
        let args = ToolArgs::new();
        let result = backend.call_tool("test-tool", args, &mut ctx).await.unwrap();

        // Verify call was routed through backend
        assert_eq!(call_count.load(Ordering::SeqCst), 1);
        assert!(result.ok());
        assert!(result.stdout.contains("mock executed: test-tool"));

        // Call again to verify count increments
        let args = ToolArgs::new();
        backend.call_tool("another-tool", args, &mut ctx).await.unwrap();
        assert_eq!(call_count.load(Ordering::SeqCst), 2);
    }
}
