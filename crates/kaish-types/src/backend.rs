//! Backend data types — errors, results, and operations.
//!
//! These types define the data contract for `KernelBackend` implementations.
//! The trait itself lives in kaish-kernel (it depends on async_trait and ExecContext).

use std::collections::BTreeMap;
use std::path::PathBuf;

use serde_json::Value as JsonValue;
use thiserror::Error;

use crate::output::OutputData;
use crate::result::{json_to_value_no_envelope, value_to_json, ExecResult, LatchRequest};
use crate::tool::ToolSchema;

/// Result type for backend operations.
pub type BackendResult<T> = Result<T, BackendError>;

/// Information about a mount point.
///
/// Returned by `KernelBackend::mounts`. Pure data so it can live in the leaf
/// types crate alongside the rest of the backend contract.
#[derive(Debug, Clone)]
pub struct MountInfo {
    /// The mount path (e.g., "/mnt/project").
    pub path: PathBuf,
    /// Whether this mount is read-only.
    pub read_only: bool,
    /// Memory-resident content bytes held by this mount, if it tracks them.
    ///
    /// `Some(n)` for memory-backed mounts (`MemoryFs`, `OverlayFs`).
    /// `None` for disk-backed mounts (`LocalFs`) — disk residency is the
    /// host's concern (`df`), not this counter. Embedder-supplied `MountInfo`
    /// values that do not track residency should use `None` so `kaish-mounts`
    /// renders `-` rather than a misleading number.
    pub resident_bytes: Option<u64>,
}

/// Backend operation errors.
#[derive(Debug, Clone, Error)]
#[non_exhaustive]
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

    /// Apply this range to already-read file content.
    ///
    /// Byte ranges win over line ranges when both are set. A line range on
    /// non-UTF-8 content returns the content untouched (there are no lines to
    /// slice). This is the single source of truth for range slicing, shared by
    /// the `Filesystem::read_range` default and the kernel backends.
    pub fn apply(&self, content: &[u8]) -> Vec<u8> {
        // Byte-based range
        if self.offset.is_some() || self.limit.is_some() {
            let offset = self.offset.unwrap_or(0) as usize;
            let limit = self.limit.map(|l| l as usize).unwrap_or(content.len());
            let end = offset.saturating_add(limit).min(content.len());
            return content.get(offset..end).unwrap_or(&[]).to_vec();
        }

        // Line-based range
        if self.start_line.is_some() || self.end_line.is_some() {
            let content_str = match std::str::from_utf8(content) {
                Ok(s) => s,
                Err(_) => return content.to_vec(),
            };
            let lines: Vec<&str> = content_str.lines().collect();
            let start = self.start_line.unwrap_or(1).saturating_sub(1);
            let end = self.end_line.unwrap_or(lines.len()).min(lines.len());
            let selected: Vec<&str> = lines.get(start..end).unwrap_or(&[]).to_vec();
            let mut result = selected.join("\n");
            // Preserve a trailing newline only when reading to the implicit end
            // and the original content had one.
            if self.end_line.is_none() && content_str.ends_with('\n') && !result.is_empty() {
                result.push('\n');
            }
            return result.into_bytes();
        }

        content.to_vec()
    }
}

/// Write mode for file operations.
#[non_exhaustive]
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
#[non_exhaustive]
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
    /// True if the output limiter capped this result (propagated from
    /// `ExecResult`). See `ExecResult.did_spill` for the full semantics (disk
    /// spill vs. in-memory truncation, exit code remap to 3).
    pub did_spill: bool,
    /// The command's original exit code before spill logic overwrote it
    /// (propagated from `ExecResult`). Present only when `did_spill` is true
    /// and `code` was changed. See `ExecResult.original_code`.
    pub original_code: Option<i64>,
    /// MIME content type hint (propagated from ExecResult).
    pub content_type: Option<String>,
    /// Opaque key-value context (propagated from ExecResult).
    pub baggage: BTreeMap<String, String>,
    /// A pending confirmation-latch request (propagated from ExecResult), so a
    /// backend-tool latch survives the ExecResult↔ToolResult roundtrip. Its own
    /// typed field — never folded into `data`. Boxed to match `ExecResult.latch`
    /// (keeps the roundtrip a direct move; see that field for why).
    pub latch: Option<Box<LatchRequest>>,
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
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            latch: None,
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
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            latch: None,
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
            did_spill: false,
            original_code: None,
            content_type: None,
            baggage: BTreeMap::new(),
            latch: None,
        }
    }

    /// Check if the tool execution succeeded.
    pub fn ok(&self) -> bool {
        self.code == 0
    }

    /// Set the structured output-data payload, returning self for chaining.
    pub fn with_output(mut self, output: Option<OutputData>) -> Self {
        self.output = output;
        self
    }

    /// Set the content-type hint, returning self for chaining.
    pub fn with_content_type(mut self, ct: impl Into<String>) -> Self {
        self.content_type = Some(ct.into());
        self
    }

    /// Replace the baggage map, returning self for chaining.
    pub fn with_baggage(mut self, baggage: BTreeMap<String, String>) -> Self {
        self.baggage = baggage;
        self
    }

    /// Set the pending confirmation-latch request, returning self for chaining.
    pub fn with_latch(mut self, latch: Option<LatchRequest>) -> Self {
        self.latch = latch.map(Box::new);
        self
    }

    /// Set the spill flag, returning self for chaining.
    pub fn with_did_spill(mut self, did_spill: bool) -> Self {
        self.did_spill = did_spill;
        self
    }

    /// Set the pre-spill original exit code, returning self for chaining.
    pub fn with_original_code(mut self, original_code: Option<i64>) -> Self {
        self.original_code = original_code;
        self
    }
}

impl From<ExecResult> for ToolResult {
    fn from(mut exec: ExecResult) -> Self {
        // Saturating cast: codes outside i32 range clamp to i32::MIN/MAX
        let code = exec.code.clamp(i32::MIN as i64, i32::MAX as i64) as i32;

        // HAZARD: `text_out()` is the infallible/lossy decoder — a binary
        // `OutputPayload::Bytes` (already produced by `cat`/`head`/`tail`/
        // `base64 -d`/`xxd -r`/`dd`/`tee`/external commands, so this is real
        // today, not merely a future risk) gets its invalid-UTF-8 bytes
        // replaced with U+FFFD here. This `From` impl is infallible by
        // signature, so it cannot fail loud the way `try_text_out()` does —
        // this is an accepted, deliberate exception, not an oversight. The
        // binary survives losslessly in the preserved `output: Option<OutputData>`
        // field below; a structured/binary-aware consumer MUST read `output`,
        // never `stdout`, to avoid the lossy decode. If a future embedder seam
        // needs a fallible conversion here, add a `TryFrom` (or a bytes-carrying
        // field) rather than changing this `From`'s behavior. See
        // `docs/binary-data.md`.
        let stdout = exec.text_out().into_owned();
        let output = exec.take_output();

        // Convert ast::Value to serde_json::Value if present
        let data = exec.data.map(|v| value_to_json(&v));

        Self {
            code,
            stdout,
            stderr: exec.err,
            data,
            output,
            did_spill: exec.did_spill,
            original_code: exec.original_code,
            content_type: exec.content_type,
            baggage: exec.baggage,
            latch: exec.latch,
        }
    }
}

impl From<ToolResult> for ExecResult {
    /// The symmetric peer of `From<ExecResult> for ToolResult` above — every
    /// field that direction preserves, this direction must preserve too, or a
    /// backend-registered tool's structured `data`/`content_type`/`baggage`
    /// silently vanishes crossing back into the kernel (the embedder seam:
    /// `x=$(embedder_tool)` and `for r in $(embedder_tool)` need `.data` to
    /// see typed results, not just stdout text).
    ///
    /// `data` uses [`json_to_value_no_envelope`] rather than the internal
    /// round-trip conversion: a backend tool's JSON is external input, so an
    /// object shaped like the byte envelope must stay a plain record, never
    /// silently auto-decode to `Value::Bytes`.
    fn from(result: ToolResult) -> Self {
        let mut exec = ExecResult::from_output(result.code as i64, result.stdout, result.stderr);
        exec.set_output(result.output);
        exec.data = result.data.map(json_to_value_no_envelope);
        exec.did_spill = result.did_spill;
        exec.original_code = result.original_code;
        exec.content_type = result.content_type;
        exec.baggage = result.baggage;
        exec.latch = result.latch;
        exec
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tool_result_from_exec_result_preserves_content_type_and_baggage() {
        let mut exec = ExecResult::success("hello");
        exec.content_type = Some("text/markdown".to_string());
        exec.baggage.insert("traceparent".to_string(), "00-abc-def-01".to_string());

        let tool_result = ToolResult::from(exec);
        assert_eq!(tool_result.content_type.as_deref(), Some("text/markdown"));
        assert_eq!(
            tool_result.baggage.get("traceparent").map(|s| s.as_str()),
            Some("00-abc-def-01")
        );
    }

    #[test]
    fn tool_result_constructors_default_to_empty_baggage() {
        let success = ToolResult::success("ok");
        assert!(success.baggage.is_empty());
        assert!(success.content_type.is_none());

        let failure = ToolResult::failure(1, "err");
        assert!(failure.baggage.is_empty());
        assert!(failure.content_type.is_none());
    }

    #[test]
    fn tool_result_constructors_default_did_spill_and_original_code() {
        // GH #93 item 3 baseline: the new fields must not silently drift the
        // existing constructors' defaults.
        assert!(!ToolResult::success("ok").did_spill);
        assert!(ToolResult::success("ok").original_code.is_none());
        assert!(!ToolResult::failure(1, "err").did_spill);
        assert!(!ToolResult::with_data("ok", serde_json::json!(1)).did_spill);
    }

    #[test]
    fn tool_result_from_exec_result_preserves_did_spill_and_original_code() {
        // GH #93 item 3: ExecResult -> ToolResult must not drop the spill
        // metadata — an embedder reading a ToolResult off this seam needs to
        // know the output was capped and what the code was before the remap.
        let mut exec = ExecResult::success("hello");
        exec.did_spill = true;
        exec.original_code = Some(0);

        let tool_result = ToolResult::from(exec);
        assert!(tool_result.did_spill);
        assert_eq!(tool_result.original_code, Some(0));
    }

    #[test]
    fn exec_result_from_tool_result_preserves_did_spill_and_original_code() {
        // The reverse direction: a backend tool that reports a capped result
        // (e.g. an embedder fronting its own output limiter) must have that
        // survive back into the kernel's ExecResult.
        let tool_result = ToolResult::success("hello")
            .with_did_spill(true)
            .with_original_code(Some(5));

        let exec = ExecResult::from(tool_result);
        assert!(exec.did_spill);
        assert_eq!(exec.original_code, Some(5));
    }

    #[test]
    fn tool_result_builder_setters_chain() {
        // Exercises the ergonomic construction surface added for
        // `#[non_exhaustive]`: every field not covered by success/failure/
        // with_data gets a with_* setter, matching the ExecResult style.
        let mut baggage = BTreeMap::new();
        baggage.insert("k".to_string(), "v".to_string());

        let latch = LatchRequest {
            nonce: "n".to_string(),
            command: "rm".to_string(),
            paths: vec!["f".to_string()],
            hint: "rm --confirm=n f".to_string(),
            tool: "rm".to_string(),
            argv: vec!["f".to_string()],
            ttl: 60,
        };

        let result = ToolResult::success("hi")
            .with_output(Some(OutputData::text("hi")))
            .with_content_type("text/plain")
            .with_baggage(baggage.clone())
            .with_latch(Some(latch.clone()))
            .with_did_spill(true)
            .with_original_code(Some(2));

        assert!(result.output.is_some());
        assert_eq!(result.content_type.as_deref(), Some("text/plain"));
        assert_eq!(result.baggage, baggage);
        assert_eq!(result.latch.as_deref(), Some(&latch));
        assert!(result.did_spill);
        assert_eq!(result.original_code, Some(2));
    }
}
