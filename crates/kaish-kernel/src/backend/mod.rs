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

pub use local::LocalBackend;

use async_trait::async_trait;
use serde_json::Value as JsonValue;
use std::path::Path;
use thiserror::Error;

use crate::tools::ToolSchema;
use crate::vfs::MountInfo;

/// Result type for backend operations.
pub type BackendResult<T> = Result<T, BackendError>;

/// Backend operation errors.
#[derive(Debug, Clone, Error)]
pub enum BackendError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("permission denied: {0}")]
    PermissionDenied(String),
    #[error("is a directory: {0}")]
    IsDirectory(String),
    #[error("not a directory: {0}")]
    NotDirectory(String),
    #[error("read-only filesystem")]
    ReadOnly,
    #[error("conflict: {0}")]
    Conflict(ConflictError),
    #[error("tool not found: {0}")]
    ToolNotFound(String),
    #[error("io error: {0}")]
    Io(String),
    #[error("invalid operation: {0}")]
    InvalidOperation(String),
}

impl From<std::io::Error> for BackendError {
    fn from(err: std::io::Error) -> Self {
        use std::io::ErrorKind;
        match err.kind() {
            ErrorKind::NotFound => BackendError::NotFound(err.to_string()),
            ErrorKind::AlreadyExists => BackendError::AlreadyExists(err.to_string()),
            ErrorKind::PermissionDenied => BackendError::PermissionDenied(err.to_string()),
            ErrorKind::IsADirectory => BackendError::IsDirectory(err.to_string()),
            ErrorKind::NotADirectory => BackendError::NotDirectory(err.to_string()),
            ErrorKind::ReadOnlyFilesystem => BackendError::ReadOnly,
            _ => BackendError::Io(err.to_string()),
        }
    }
}

/// Error when CAS (compare-and-set) check fails during patching.
#[derive(Debug, Clone, Error)]
#[error("conflict at {location}: expected {expected:?}, found {actual:?}")]
pub struct ConflictError {
    /// Location of the conflict (e.g., "offset 42" or "line 7")
    pub location: String,
    /// Expected content at that location
    pub expected: String,
    /// Actual content found at that location
    pub actual: String,
}

/// Generic patch operation for file modifications.
///
/// Maps to POSIX operations, CRDTs, or REST APIs. All positional ops
/// support compare-and-set (CAS) via optional `expected` field.
/// If `expected` is Some, the operation fails with ConflictError if the
/// current content at that position doesn't match.
#[derive(Debug, Clone)]
pub enum PatchOp {
    /// Insert content at byte offset.
    Insert { offset: usize, content: String },

    /// Delete bytes from offset to offset+len.
    /// `expected`: if Some, must match content being deleted (CAS)
    Delete {
        offset: usize,
        len: usize,
        expected: Option<String>,
    },

    /// Replace content at offset.
    /// `expected`: if Some, must match content being replaced (CAS)
    Replace {
        offset: usize,
        len: usize,
        content: String,
        expected: Option<String>,
    },

    /// Insert a line at line number (1-indexed).
    InsertLine { line: usize, content: String },

    /// Delete a line at line number (1-indexed).
    /// `expected`: if Some, must match line being deleted (CAS)
    DeleteLine { line: usize, expected: Option<String> },

    /// Replace a line at line number (1-indexed).
    /// `expected`: if Some, must match line being replaced (CAS)
    ReplaceLine {
        line: usize,
        content: String,
        expected: Option<String>,
    },

    /// Append content to end of file (no CAS needed - always safe).
    Append { content: String },
}

/// Range specification for partial file reads.
#[derive(Debug, Clone, Default)]
pub struct ReadRange {
    /// Start line (1-indexed). If set, read from this line.
    pub start_line: Option<usize>,
    /// End line (1-indexed, inclusive). If set, read until this line.
    pub end_line: Option<usize>,
    /// Byte offset to start reading from.
    pub offset: Option<u64>,
    /// Maximum number of bytes to read.
    pub limit: Option<u64>,
}

impl ReadRange {
    /// Create a range for reading specific lines.
    pub fn lines(start: usize, end: usize) -> Self {
        Self {
            start_line: Some(start),
            end_line: Some(end),
            ..Default::default()
        }
    }

    /// Create a range for reading bytes at an offset.
    pub fn bytes(offset: u64, limit: u64) -> Self {
        Self {
            offset: Some(offset),
            limit: Some(limit),
            ..Default::default()
        }
    }
}

/// Write mode for file operations.
#[derive(Debug, Clone, Copy, Default)]
pub enum WriteMode {
    /// Fail if file already exists.
    CreateNew,
    /// Overwrite existing file (default, like `>`).
    #[default]
    Overwrite,
    /// Fail if file does not exist.
    UpdateOnly,
    /// Explicitly truncate file before writing.
    Truncate,
}

/// Information about a file or directory entry.
#[derive(Debug, Clone)]
pub struct EntryInfo {
    /// Entry name (file or directory name).
    pub name: String,
    /// True if this is a directory.
    pub is_dir: bool,
    /// True if this is a file.
    pub is_file: bool,
    /// Size in bytes.
    pub size: u64,
    /// Last modification time (Unix timestamp in seconds).
    pub modified: Option<u64>,
    /// Unix permissions (e.g., 0o644).
    pub permissions: Option<u32>,
}

impl EntryInfo {
    /// Create a new directory entry.
    pub fn directory(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            is_dir: true,
            is_file: false,
            size: 0,
            modified: None,
            permissions: None,
        }
    }

    /// Create a new file entry.
    pub fn file(name: impl Into<String>, size: u64) -> Self {
        Self {
            name: name.into(),
            is_dir: false,
            is_file: true,
            size,
            modified: None,
            permissions: None,
        }
    }
}

/// Result from tool execution via backend.
#[derive(Debug, Clone)]
pub struct ToolResult {
    /// Exit code (0 = success).
    pub code: i32,
    /// Standard output.
    pub stdout: String,
    /// Standard error.
    pub stderr: String,
    /// Structured data (if any).
    pub data: Option<JsonValue>,
}

impl ToolResult {
    /// Create a successful result.
    pub fn success(stdout: impl Into<String>) -> Self {
        Self {
            code: 0,
            stdout: stdout.into(),
            stderr: String::new(),
            data: None,
        }
    }

    /// Create a failed result.
    pub fn failure(code: i32, stderr: impl Into<String>) -> Self {
        Self {
            code,
            stdout: String::new(),
            stderr: stderr.into(),
            data: None,
        }
    }

    /// Create a result with structured data.
    pub fn with_data(stdout: impl Into<String>, data: JsonValue) -> Self {
        Self {
            code: 0,
            stdout: stdout.into(),
            stderr: String::new(),
            data: Some(data),
        }
    }

    /// Check if the tool execution succeeded.
    pub fn ok(&self) -> bool {
        self.code == 0
    }
}

/// Information about an available tool.
#[derive(Debug, Clone)]
pub struct ToolInfo {
    /// Tool name.
    pub name: String,
    /// Tool description.
    pub description: String,
    /// Full tool schema.
    pub schema: ToolSchema,
}

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
    async fn list(&self, path: &Path) -> BackendResult<Vec<EntryInfo>>;

    /// Get file or directory metadata.
    async fn stat(&self, path: &Path) -> BackendResult<EntryInfo>;

    /// Create a directory (and parent directories if needed).
    async fn mkdir(&self, path: &Path) -> BackendResult<()>;

    /// Remove a file or directory.
    ///
    /// If `recursive` is true, removes directories and their contents.
    async fn remove(&self, path: &Path, recursive: bool) -> BackendResult<()>;

    /// Check if a path exists.
    async fn exists(&self, path: &Path) -> bool;

    // ═══════════════════════════════════════════════════════════════════════════
    // Tool Dispatch
    // ═══════════════════════════════════════════════════════════════════════════

    /// Call an external tool (MCP or system command).
    async fn call_tool(&self, name: &str, args: JsonValue) -> BackendResult<ToolResult>;

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
}

#[cfg(test)]
mod tests {
    use super::*;

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
    fn test_entry_info_constructors() {
        let dir = EntryInfo::directory("mydir");
        assert!(dir.is_dir);
        assert!(!dir.is_file);
        assert_eq!(dir.name, "mydir");

        let file = EntryInfo::file("myfile.txt", 1024);
        assert!(!file.is_dir);
        assert!(file.is_file);
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
}
