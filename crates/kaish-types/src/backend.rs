//! Backend data types — errors, results, and operations.
//!
//! These types define the data contract for `KernelBackend` implementations.
//! The trait itself lives in kaish-kernel (it depends on async_trait and ExecContext).

use serde_json::Value as JsonValue;
use thiserror::Error;

use crate::output::OutputData;
use crate::result::{value_to_json, ExecResult};
use crate::tool::ToolSchema;

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
///
/// # Line Ending Normalization
///
/// Line-based operations (`InsertLine`, `DeleteLine`, `ReplaceLine`) normalize
/// line endings to Unix-style (`\n`). Files with `\r\n` (Windows) line endings
/// will be converted to `\n` after a line-based patch. This is intentional for
/// kaish's Unix-first design. Use byte-based operations (`Insert`, `Delete`,
/// `Replace`) to preserve original line endings.
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
    /// Structured output data for rendering (preserved from ExecResult).
    pub output: Option<OutputData>,
}

impl ToolResult {
    /// Create a successful result.
    pub fn success(stdout: impl Into<String>) -> Self {
        Self {
            code: 0,
            stdout: stdout.into(),
            stderr: String::new(),
            data: None,
            output: None,
        }
    }

    /// Create a failed result.
    pub fn failure(code: i32, stderr: impl Into<String>) -> Self {
        Self {
            code,
            stdout: String::new(),
            stderr: stderr.into(),
            data: None,
            output: None,
        }
    }

    /// Create a result with structured data.
    pub fn with_data(stdout: impl Into<String>, data: JsonValue) -> Self {
        Self {
            code: 0,
            stdout: stdout.into(),
            stderr: String::new(),
            data: Some(data),
            output: None,
        }
    }

    /// Check if the tool execution succeeded.
    pub fn ok(&self) -> bool {
        self.code == 0
    }
}

impl From<ExecResult> for ToolResult {
    fn from(exec: ExecResult) -> Self {
        // Saturating cast: codes outside i32 range clamp to i32::MIN/MAX
        let code = exec.code.clamp(i32::MIN as i64, i32::MAX as i64) as i32;

        // Convert ast::Value to serde_json::Value if present
        let data = exec.data.map(|v| value_to_json(&v));

        Self {
            code,
            stdout: exec.out,
            stderr: exec.err,
            data,
            output: exec.output,
        }
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
