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

// Data types re-exported from kaish-types.
pub use kaish_types::backend::{
    BackendError, BackendResult, ConflictError, MountInfo, PatchOp, ReadRange, ToolInfo,
    ToolResult, WriteMode,
};

// The `KernelBackend` trait moved to the leaf `kaish-tool-api` crate (its
// `call_tool` takes the portable `&mut dyn ToolCtx`, and tools reach it through
// `ctx.backend()`). Re-exported here so existing `crate::backend::KernelBackend`
// paths — and the `LocalBackend` / overlay / testing impls below — keep working.
pub use kaish_tool_api::KernelBackend;

#[cfg(test)]
mod tests {
    use super::*;
    use super::testing::MockBackend;
    use crate::tools::{ExecContext, ToolArgs};
    use crate::vfs::DirEntry;
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
