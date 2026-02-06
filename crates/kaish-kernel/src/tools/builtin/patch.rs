//! patch — Apply unified diffs to files.
//!
//! # Examples
//!
//! ```kaish
//! patch < changes.patch           # Apply patch from stdin
//! patch -p1 < changes.patch       # Strip 1 path component
//! patch -R < changes.patch        # Reverse the patch
//! patch --dry-run < changes.patch # Show what would change
//! patch file.txt < changes.patch  # Explicit target file
//! ```

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::backend::PatchOp;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Patch tool: applies unified diffs to files.
pub struct Patch;

#[async_trait]
impl Tool for Patch {
    fn name(&self) -> &str {
        "patch"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("patch", "Apply unified diff to files")
            .param(ParamSchema::optional(
                "file",
                "string",
                Value::Null,
                "Target file (overrides patch header)",
            ))
            .param(ParamSchema::optional(
                "p",
                "int",
                Value::Int(0),
                "Strip N leading path components",
            ))
            .param(ParamSchema::optional(
                "R",
                "bool",
                Value::Bool(false),
                "Reverse the patch (swap + and -)",
            ))
            .param(ParamSchema::optional(
                "dry-run",
                "bool",
                Value::Bool(false),
                "Show what would change without applying",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Read patch content from stdin
        let patch_content = ctx.stdin.take().unwrap_or_default();
        if patch_content.is_empty() {
            return ExecResult::failure(1, "patch: no input provided (use stdin)");
        }

        // Parse options
        let strip_level = args
            .get_named("p")
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
            .unwrap_or(0);

        let reverse = args.has_flag("R");
        let dry_run = args.has_flag("dry-run");
        let explicit_file = args.get_string("file", 0);

        // Parse the unified diff
        let hunks = match parse_unified_diff(&patch_content) {
            Ok(h) => h,
            Err(e) => return ExecResult::failure(1, format!("patch: {}", e)),
        };

        if hunks.is_empty() {
            return ExecResult::failure(1, "patch: no valid hunks found in input");
        }

        let mut output = String::new();
        let mut total_applied = 0;

        // Group hunks by target file
        for file_hunks in group_by_file(&hunks) {
            let target_path = if let Some(ref explicit) = explicit_file {
                explicit.clone()
            } else {
                strip_path(&file_hunks.target_file, strip_level)
            };

            let resolved_path = ctx.resolve_path(&target_path);
            let path = Path::new(&resolved_path);

            // Read current file content
            let current_content = match ctx.backend.read(path, None).await {
                Ok(data) => String::from_utf8_lossy(&data).into_owned(),
                Err(e) => {
                    return ExecResult::failure(
                        1,
                        format!("patch: cannot read '{}': {}", target_path, e),
                    );
                }
            };

            // Convert hunks to PatchOps
            let ops = match hunks_to_patch_ops(&file_hunks.hunks, &current_content, reverse) {
                Ok(ops) => ops,
                Err(e) => {
                    return ExecResult::failure(
                        1,
                        format!("patch: failed to apply to '{}': {}", target_path, e),
                    );
                }
            };

            if dry_run {
                output.push_str(&format!(
                    "patching file {} ({} changes)\n",
                    target_path,
                    ops.len()
                ));
                for op in &ops {
                    output.push_str(&format!("  {}\n", describe_patch_op(op)));
                }
            } else {
                // Apply the patch
                if let Err(e) = ctx.backend.patch(path, &ops).await {
                    return ExecResult::failure(
                        1,
                        format!("patch: failed to apply to '{}': {}", target_path, e),
                    );
                }
                output.push_str(&format!("patching file {}\n", target_path));
                total_applied += ops.len();
            }
        }

        if !dry_run && total_applied > 0 {
            output.push_str(&format!("{} changes applied\n", total_applied));
        }

        ExecResult::with_output(OutputData::text(output.trim_end()))
    }
}

/// A single hunk from a unified diff.
#[derive(Debug, Clone)]
struct DiffHunk {
    /// Original file start line (1-indexed)
    old_start: usize,
    /// Original file line count
    #[allow(dead_code)]
    old_count: usize,
    /// New file start line (1-indexed)
    new_start: usize,
    /// New file line count
    #[allow(dead_code)]
    new_count: usize,
    /// Lines in this hunk
    lines: Vec<DiffLine>,
}

/// A single line in a diff hunk.
#[derive(Debug, Clone)]
enum DiffLine {
    Context(String),
    Delete(String),
    Insert(String),
}

/// Hunks grouped by file.
struct FileHunks {
    target_file: String,
    hunks: Vec<DiffHunk>,
}

/// Parse unified diff format into hunks.
fn parse_unified_diff(content: &str) -> Result<Vec<FileHunks>, String> {
    let mut result: Vec<FileHunks> = Vec::new();
    let mut current_file: Option<String> = None;
    let mut current_hunks: Vec<DiffHunk> = Vec::new();
    let mut current_hunk: Option<DiffHunk> = None;

    for line in content.lines() {
        // Detect file header (--- and +++)
        if line.starts_with("--- ") {
            // Save previous file if any
            if let Some(file) = current_file.take() {
                if let Some(hunk) = current_hunk.take() {
                    current_hunks.push(hunk);
                }
                if !current_hunks.is_empty() {
                    result.push(FileHunks {
                        target_file: file,
                        hunks: std::mem::take(&mut current_hunks),
                    });
                }
            }
            // Parse will continue with +++ line
        } else if line.starts_with("+++ ") {
            // Extract target filename (after +++ )
            let path = line
                .strip_prefix("+++ ")
                .unwrap_or("")
                .split('\t')
                .next()
                .unwrap_or("")
                .to_string();
            current_file = Some(path);
        } else if line.starts_with("@@ ") {
            // Save previous hunk if any
            if let Some(hunk) = current_hunk.take() {
                current_hunks.push(hunk);
            }
            // Parse hunk header: @@ -old_start,old_count +new_start,new_count @@
            current_hunk = Some(parse_hunk_header(line)?);
        } else if let Some(ref mut hunk) = current_hunk {
            // Parse hunk content
            if let Some(rest) = line.strip_prefix('-') {
                hunk.lines.push(DiffLine::Delete(rest.to_string()));
            } else if let Some(rest) = line.strip_prefix('+') {
                hunk.lines.push(DiffLine::Insert(rest.to_string()));
            } else if let Some(rest) = line.strip_prefix(' ') {
                hunk.lines.push(DiffLine::Context(rest.to_string()));
            } else if line.is_empty() || line == "\\ No newline at end of file" {
                // Handle empty context line or no-newline marker
                if line.is_empty() {
                    hunk.lines.push(DiffLine::Context(String::new()));
                }
            }
        }
    }

    // Save final hunk and file
    if let Some(hunk) = current_hunk {
        current_hunks.push(hunk);
    }
    if let Some(file) = current_file
        && !current_hunks.is_empty() {
            result.push(FileHunks {
                target_file: file,
                hunks: current_hunks,
            });
        }

    Ok(result)
}

/// Parse a hunk header like "@@ -1,3 +1,4 @@" or "@@ -1 +1,2 @@".
fn parse_hunk_header(line: &str) -> Result<DiffHunk, String> {
    // Remove @@ prefix and suffix
    let content = line
        .strip_prefix("@@ ")
        .and_then(|s| s.split(" @@").next())
        .ok_or_else(|| format!("invalid hunk header: {}", line))?;

    // Split into old and new parts
    let parts: Vec<&str> = content.split_whitespace().collect();
    if parts.len() < 2 {
        return Err(format!("invalid hunk header: {}", line));
    }

    let (old_start, old_count) = parse_range(parts[0].strip_prefix('-').unwrap_or(parts[0]))?;
    let (new_start, new_count) = parse_range(parts[1].strip_prefix('+').unwrap_or(parts[1]))?;

    Ok(DiffHunk {
        old_start,
        old_count,
        new_start,
        new_count,
        lines: Vec::new(),
    })
}

/// Parse a range like "1,3" or just "1" (which means count of 1).
fn parse_range(s: &str) -> Result<(usize, usize), String> {
    if let Some((start, count)) = s.split_once(',') {
        let start: usize = start.parse().map_err(|_| format!("invalid number: {}", s))?;
        let count: usize = count.parse().map_err(|_| format!("invalid number: {}", s))?;
        Ok((start, count))
    } else {
        let start: usize = s.parse().map_err(|_| format!("invalid number: {}", s))?;
        Ok((start, 1))
    }
}

/// Strip leading path components from a path.
fn strip_path(path: &str, level: usize) -> String {
    if level == 0 {
        return path.to_string();
    }

    let components: Vec<&str> = path.split('/').collect();
    if level >= components.len() {
        components.last().unwrap_or(&path).to_string()
    } else {
        components[level..].join("/")
    }
}

/// Group parsed hunks by file.
fn group_by_file(file_hunks: &[FileHunks]) -> Vec<&FileHunks> {
    // Already grouped by parse_unified_diff, just return references
    file_hunks.iter().collect()
}

/// Convert diff hunks to PatchOp operations.
fn hunks_to_patch_ops(
    hunks: &[DiffHunk],
    content: &str,
    reverse: bool,
) -> Result<Vec<PatchOp>, String> {
    let lines: Vec<&str> = content.lines().collect();
    let mut ops: Vec<PatchOp> = Vec::new();

    // Track line offset as we apply operations
    let mut line_offset: isize = 0;

    for hunk in hunks {
        let start_line = if reverse {
            hunk.new_start
        } else {
            hunk.old_start
        };

        let mut current_line = start_line;

        for diff_line in &hunk.lines {
            // Adjust line number based on previous operations
            let adjusted_line = (current_line as isize + line_offset) as usize;

            match diff_line {
                DiffLine::Context(expected) => {
                    // Verify context matches
                    if adjusted_line > 0 && adjusted_line <= lines.len() {
                        let actual = lines.get(adjusted_line - 1).unwrap_or(&"");
                        if *actual != expected {
                            return Err(format!(
                                "context mismatch at line {}: expected '{}', found '{}'",
                                adjusted_line, expected, actual
                            ));
                        }
                    }
                    current_line += 1;
                }
                DiffLine::Delete(expected) => {
                    if reverse {
                        // In reverse mode, delete becomes insert
                        ops.push(PatchOp::InsertLine {
                            line: adjusted_line,
                            content: expected.clone(),
                        });
                        line_offset += 1;
                    } else {
                        // Normal mode: delete the line
                        ops.push(PatchOp::DeleteLine {
                            line: adjusted_line,
                            expected: Some(expected.clone()),
                        });
                        line_offset -= 1;
                    }
                    if !reverse {
                        current_line += 1;
                    }
                }
                DiffLine::Insert(content_line) => {
                    if reverse {
                        // In reverse mode, insert becomes delete
                        ops.push(PatchOp::DeleteLine {
                            line: adjusted_line,
                            expected: Some(content_line.clone()),
                        });
                        line_offset -= 1;
                        current_line += 1;
                    } else {
                        // Normal mode: insert the line
                        ops.push(PatchOp::InsertLine {
                            line: adjusted_line,
                            content: content_line.clone(),
                        });
                        line_offset += 1;
                    }
                }
            }
        }
    }

    Ok(ops)
}

/// Describe a PatchOp for dry-run output.
fn describe_patch_op(op: &PatchOp) -> String {
    match op {
        PatchOp::InsertLine { line, content } => {
            format!("+{}: {}", line, truncate(content, 40))
        }
        PatchOp::DeleteLine { line, expected } => {
            let content = expected.as_deref().unwrap_or("?");
            format!("-{}: {}", line, truncate(content, 40))
        }
        PatchOp::ReplaceLine { line, content, .. } => {
            format!("~{}: {}", line, truncate(content, 40))
        }
        PatchOp::Insert { offset, content } => {
            format!("+@{}: {}", offset, truncate(content, 40))
        }
        PatchOp::Delete { offset, len, .. } => {
            format!("-@{}..{}", offset, offset + len)
        }
        PatchOp::Replace { offset, len, content, .. } => {
            format!("~@{}..{}: {}", offset, offset + len, truncate(content, 40))
        }
        PatchOp::Append { content } => {
            format!("+$: {}", truncate(content, 40))
        }
    }
}

/// Truncate a string for display.
fn truncate(s: &str, max: usize) -> String {
    if s.len() <= max {
        s.to_string()
    } else {
        format!("{}...", &s[..max.saturating_sub(3)])
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_test_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        // Create test file
        mem.write(Path::new("test.txt"), b"line1\nline2\nline3\n")
            .await
            .unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    fn simple_patch() -> String {
        // Note: context lines must have a leading space!
        concat!(
            "--- a/test.txt\n",
            "+++ b/test.txt\n",
            "@@ -1,3 +1,3 @@\n",
            " line1\n",
            "-line2\n",
            "+modified\n",
            " line3\n",
        )
        .to_string()
    }

    #[tokio::test]
    async fn test_patch_apply() {
        let mut ctx = make_test_ctx().await;
        ctx.stdin = Some(simple_patch());

        let mut args = ToolArgs::new();
        // Strip 'b/' prefix from target path
        args.named.insert("p".to_string(), Value::Int(1));
        let result = Patch.execute(args, &mut ctx).await;

        assert!(result.ok(), "patch failed: {}", result.err);
        assert!(result.out.contains("patching file"));

        // Verify the file was modified
        let content = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        let text = String::from_utf8_lossy(&content);
        assert!(text.contains("modified"), "file not modified: {}", text);
        assert!(!text.contains("line2"), "old line still present");
    }

    #[tokio::test]
    async fn test_patch_dry_run() {
        let mut ctx = make_test_ctx().await;
        ctx.stdin = Some(simple_patch());

        let mut args = ToolArgs::new();
        args.named.insert("p".to_string(), Value::Int(1));
        args.flags.insert("dry-run".to_string());

        let result = Patch.execute(args, &mut ctx).await;

        assert!(result.ok(), "dry-run failed: {}", result.err);
        assert!(result.out.contains("changes"), "output: {}", result.out);

        // Verify the file was NOT modified
        let content = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        let text = String::from_utf8_lossy(&content);
        assert!(text.contains("line2"), "file was modified in dry-run mode");
    }

    #[tokio::test]
    async fn test_patch_reverse() {
        let mut ctx = make_test_ctx().await;

        // First apply the patch
        ctx.stdin = Some(simple_patch());
        let mut args = ToolArgs::new();
        args.named.insert("p".to_string(), Value::Int(1));
        Patch.execute(args, &mut ctx).await;

        // Then reverse it
        ctx.stdin = Some(simple_patch());
        let mut args = ToolArgs::new();
        args.named.insert("p".to_string(), Value::Int(1));
        args.flags.insert("R".to_string());

        let result = Patch.execute(args, &mut ctx).await;

        assert!(result.ok(), "reverse patch failed: {}", result.err);

        // Verify original content restored
        let content = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        let text = String::from_utf8_lossy(&content);
        assert!(text.contains("line2"), "original not restored: {}", text);
    }

    #[tokio::test]
    async fn test_patch_strip_path() {
        assert_eq!(strip_path("a/b/c/file.txt", 0), "a/b/c/file.txt");
        assert_eq!(strip_path("a/b/c/file.txt", 1), "b/c/file.txt");
        assert_eq!(strip_path("a/b/c/file.txt", 2), "c/file.txt");
        assert_eq!(strip_path("a/b/c/file.txt", 3), "file.txt");
        assert_eq!(strip_path("a/b/c/file.txt", 10), "file.txt");
    }

    #[tokio::test]
    async fn test_patch_no_input() {
        let mut ctx = make_test_ctx().await;
        // No stdin

        let args = ToolArgs::new();
        let result = Patch.execute(args, &mut ctx).await;

        assert!(!result.ok());
        assert!(result.err.contains("no input"));
    }

    #[test]
    fn test_parse_hunk_header() {
        let hunk = parse_hunk_header("@@ -1,3 +1,4 @@").unwrap();
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.old_count, 3);
        assert_eq!(hunk.new_start, 1);
        assert_eq!(hunk.new_count, 4);

        let hunk = parse_hunk_header("@@ -1 +1,2 @@").unwrap();
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.old_count, 1);
        assert_eq!(hunk.new_start, 1);
        assert_eq!(hunk.new_count, 2);
    }

    #[test]
    fn test_parse_unified_diff() {
        let patch = simple_patch();
        let files = parse_unified_diff(&patch).unwrap();

        assert_eq!(files.len(), 1);
        assert_eq!(files[0].target_file, "b/test.txt");
        assert_eq!(files[0].hunks.len(), 1);

        let hunk = &files[0].hunks[0];
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.lines.len(), 4); // context + delete + insert + context
    }
}
