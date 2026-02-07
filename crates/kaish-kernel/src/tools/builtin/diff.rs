//! diff — Compare files line by line.
//!
//! # Examples
//!
//! ```kaish
//! diff file1.txt file2.txt          # Unified diff (default)
//! diff -u file1.txt file2.txt       # Explicit unified format
//! diff -q file1.txt file2.txt       # Quiet mode (just report if different)
//! diff --color file1.txt file2.txt  # Colorized output
//! ```

use async_trait::async_trait;
use similar::{ChangeTag, TextDiff};
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Diff tool: compares two files line by line.
pub struct Diff;

#[async_trait]
impl Tool for Diff {
    fn name(&self) -> &str {
        "diff"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("diff", "Compare files line by line")
            .param(ParamSchema::required(
                "file1",
                "string",
                "First file to compare",
            ))
            .param(ParamSchema::required(
                "file2",
                "string",
                "Second file to compare",
            ))
            .param(ParamSchema::optional(
                "unified",
                "bool",
                Value::Bool(false),
                "Output unified diff format (default)",
            ).with_aliases(["-u"]))
            .param(ParamSchema::optional(
                "quiet",
                "bool",
                Value::Bool(false),
                "Quiet mode: only report if files differ",
            ).with_aliases(["-q"]))
            .param(ParamSchema::optional(
                "color",
                "bool",
                Value::Bool(false),
                "Colorize output",
            ).with_aliases(["--color"]))
            .param(ParamSchema::optional(
                "context",
                "int",
                Value::Int(3),
                "Lines of context (default: 3)",
            ).with_aliases(["-C"]))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get file paths
        let file1 = match args.get_string("file1", 0) {
            Some(f) => f,
            None => return ExecResult::failure(2, "diff: missing first file"),
        };

        let file2 = match args.get_string("file2", 1) {
            Some(f) => f,
            None => return ExecResult::failure(2, "diff: missing second file"),
        };

        let path1 = ctx.resolve_path(&file1);
        let path2 = ctx.resolve_path(&file2);

        // Read file contents
        let content1 = match ctx.backend.read(Path::new(&path1), None).await {
            Ok(data) => String::from_utf8_lossy(&data).into_owned(),
            Err(e) => return ExecResult::failure(2, format!("diff: {}: {}", file1, e)),
        };

        let content2 = match ctx.backend.read(Path::new(&path2), None).await {
            Ok(data) => String::from_utf8_lossy(&data).into_owned(),
            Err(e) => return ExecResult::failure(2, format!("diff: {}: {}", file2, e)),
        };

        // Check options
        let quiet = args.has_flag("q");
        let colorize = args.has_flag("color");
        let context_lines = args
            .get_named("context")
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                _ => None,
            })
            .unwrap_or(3);

        // Quick check if files are identical
        if content1 == content2 {
            return ExecResult::with_output(OutputData::text(""));
        }

        // Quiet mode: just report difference
        if quiet {
            let text = format!("Files {} and {} differ\n", file1, file2);
            let mut result = ExecResult::from_output(1, text.clone(), String::new());
            result.output = Some(OutputData::text(text));
            return result;
        }

        // Generate diff using similar's built-in unified format
        let diff = TextDiff::from_lines(&content1, &content2);

        let output = if colorize {
            // Manual colorized output
            format_colorized_diff(&diff, &file1, &file2, context_lines)
        } else {
            // Use similar's built-in unified diff
            diff.unified_diff()
                .context_radius(context_lines)
                .header(&file1, &file2)
                .to_string()
        };

        // Exit code 1 if files differ (POSIX convention)
        let mut result = ExecResult::from_output(1, output.clone(), String::new());
        result.output = Some(OutputData::text(output));
        result
    }
}

/// Format a colorized unified diff.
fn format_colorized_diff<'a>(
    diff: &TextDiff<'a, 'a, 'a, str>,
    file1: &str,
    file2: &str,
    _context: usize,
) -> String {
    let mut output = String::new();

    // Header
    output.push_str(&format!("\x1b[1m--- {}\x1b[0m\n", file1));
    output.push_str(&format!("\x1b[1m+++ {}\x1b[0m\n", file2));

    // Simple approach: iterate all changes with coloring
    // This is a simplified version that outputs all changes
    output.push_str("@@ -1 +1 @@\n");

    for change in diff.iter_all_changes() {
        match change.tag() {
            ChangeTag::Equal => {
                output.push(' ');
                output.push_str(change.value());
            }
            ChangeTag::Delete => {
                output.push_str("\x1b[31m-");
                output.push_str(change.value());
                output.push_str("\x1b[0m");
            }
            ChangeTag::Insert => {
                output.push_str("\x1b[32m+");
                output.push_str(change.value());
                output.push_str("\x1b[0m");
            }
        }
        if !change.value().ends_with('\n') {
            output.push('\n');
        }
    }

    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_test_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        // Create test files
        mem.write(Path::new("file1.txt"), b"line1\nline2\nline3\n")
            .await
            .unwrap();
        mem.write(Path::new("file2.txt"), b"line1\nmodified\nline3\n")
            .await
            .unwrap();
        mem.write(Path::new("same1.txt"), b"identical\n")
            .await
            .unwrap();
        mem.write(Path::new("same2.txt"), b"identical\n")
            .await
            .unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_diff_files_differ() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("file1.txt".into()));
        args.positional.push(Value::String("file2.txt".into()));

        let result = Diff.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1); // Files differ
        assert!(result.out.contains("---"));
        assert!(result.out.contains("+++"));
        assert!(result.out.contains("-line2"));
        assert!(result.out.contains("+modified"));
    }

    #[tokio::test]
    async fn test_diff_files_identical() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("same1.txt".into()));
        args.positional.push(Value::String("same2.txt".into()));

        let result = Diff.execute(args, &mut ctx).await;
        assert!(result.ok()); // Files identical = exit 0
        assert!(result.out.is_empty());
    }

    #[tokio::test]
    async fn test_diff_quiet_mode() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("file1.txt".into()));
        args.positional.push(Value::String("file2.txt".into()));
        args.flags.insert("q".to_string());

        let result = Diff.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        assert!(result.out.contains("differ"));
        assert!(!result.out.contains("---")); // No diff output
    }

    #[tokio::test]
    async fn test_diff_missing_file() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("file1.txt".into()));
        args.positional.push(Value::String("nonexistent.txt".into()));

        let result = Diff.execute(args, &mut ctx).await;
        assert_eq!(result.code, 2);
        assert!(result.err.contains("nonexistent.txt"));
    }

    #[tokio::test]
    async fn test_diff_colorized() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("file1.txt".into()));
        args.positional.push(Value::String("file2.txt".into()));
        args.flags.insert("color".to_string());

        let result = Diff.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        // Check for ANSI escape codes
        assert!(result.out.contains("\x1b["));
    }
}
