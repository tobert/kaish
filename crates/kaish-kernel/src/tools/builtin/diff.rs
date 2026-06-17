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
use clap::{CommandFactory, Parser};
use similar::TextDiff;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, validate_against_schema, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use crate::validator::{IssueCode, ValidationIssue};

/// Diff tool: compares two files line by line.
pub struct Diff;

/// clap-derived argv layer for diff. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "diff", about = "Compare files line by line")]
struct DiffArgs {
    /// Output unified diff format (default)
    #[arg(id = "unified", short = 'u', long = "unified")]
    _unified: bool,

    /// Quiet mode: only report if files differ
    #[arg(short = 'q', long = "quiet")]
    quiet: bool,

    /// Colorize output
    #[arg(long = "color")]
    color: bool,

    /// Lines of context (default: 3)
    #[arg(short = 'C', long = "context")]
    context: Option<i64>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Two files to compare.
    files: Vec<String>,
}

#[async_trait]
impl Tool for Diff {
    fn name(&self) -> &str {
        "diff"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &DiffArgs::command(),
            "diff",
            "Compare files line by line (unified diff by default)",
            [
                ("Compare two files (unified)", "diff file1.txt file2.txt"),
                ("Quiet mode", "diff -q old.txt new.txt"),
            ],
        )
    }

    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        let mut issues = validate_against_schema(args, &self.schema());

        // diff compares exactly two files (no stdin path), so the operand count
        // is knowable up front for all-literal argv — catch both the too-few
        // (`diff a`) and the silently-dropped too-many (`diff a b c`) cases
        // before any pipeline runs.
        //
        // Two static-analysis caveats, both handled conservatively:
        //  - A `<dynamic>` operand (variable, `$(cmd)`, or a bare glob) has an
        //    unknown runtime count, so skip the check entirely — `diff *.txt`
        //    may legitimately expand to two files. Mirrors grep's guard.
        //  - The only value-taking flag, `-C`/`--context`, binds its value at
        //    execute time; in space form (`-C 3`) the value still sits in
        //    `positional` here, so discount one slot when a bare context flag
        //    is present.
        let has_dynamic = args
            .positional
            .iter()
            .any(|v| matches!(v, Value::String(s) if s == "<dynamic>"));
        if has_dynamic {
            return issues;
        }

        let context_steals_positional =
            args.flags.contains("C") || args.flags.contains("context");
        let operand_count = args
            .positional
            .len()
            .saturating_sub(if context_steals_positional { 1 } else { 0 });

        if operand_count != 2 {
            issues.push(
                ValidationIssue::error(
                    IssueCode::DiffNeedsTwoFiles,
                    format!("diff: needs exactly two file operands (got {operand_count})"),
                )
                .with_suggestion("usage: diff OLD NEW"),
            );
        }

        issues
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match DiffArgs::try_parse_from(
            std::iter::once("diff".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("diff: {e}")),
        };
        parsed.global.apply(ctx);

        // Runtime backstop for the too-many case the validator can't see: a glob
        // or variables expanding to 3+ files (`diff *.txt`, `diff $a $b $c`) reach
        // here as `<dynamic>` and skip the static arity check. `parsed.files` is
        // clap's clean operand list (the `-C 3` value already pulled out), so a
        // surplus operand is a loud error instead of a silent drop.
        if parsed.files.len() > 2 {
            return ExecResult::failure(
                2,
                format!(
                    "diff: needs exactly two file operands (got {})",
                    parsed.files.len()
                ),
            );
        }

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

        // Read file contents — diff is a text tool; binary is a loud error
        // (use `cmp` for byte comparison), not a lossy decode.
        let content1 = match ctx.backend.read(Path::new(&path1), None).await {
            Ok(data) => match String::from_utf8(data) {
                Ok(s) => s,
                Err(_) => return ExecResult::failure(2, format!("diff: {}: binary data — use cmp", file1)),
            },
            Err(e) => return ExecResult::failure(2, format!("diff: {}: {}", file1, e)),
        };

        let content2 = match ctx.backend.read(Path::new(&path2), None).await {
            Ok(data) => match String::from_utf8(data) {
                Ok(s) => s,
                Err(_) => return ExecResult::failure(2, format!("diff: {}: binary data — use cmp", file2)),
            },
            Err(e) => return ExecResult::failure(2, format!("diff: {}: {}", file2, e)),
        };

        // Check options
        let quiet = parsed.quiet;
        let colorize = parsed.color;
        let context_lines = parsed
            .context
            .map(|n| n as usize)
            .or_else(|| {
                args.get_named("context").and_then(|v| match v {
                    Value::Int(i) => Some(*i as usize),
                    _ => None,
                })
            })
            .unwrap_or(3);

        // Quick check if files are identical
        if content1 == content2 {
            return ExecResult::success("");
        }

        // Quiet mode: just report difference
        if quiet {
            let text = format!("Files {} and {} differ\n", file1, file2);
            let mut result = ExecResult::from_output(1, text.clone(), String::new());
            result.set_output(Some(OutputData::text(text)));
            return result;
        }

        // Generate diff using similar's built-in unified format
        let diff = TextDiff::from_lines(&content1, &content2);

        let plain = diff
            .unified_diff()
            .context_radius(context_lines)
            .header(&file1, &file2)
            .to_string();

        let output = if colorize {
            colorize_unified_output(&plain)
        } else {
            plain
        };

        // Exit code 1 if files differ (POSIX convention)
        let mut result = ExecResult::from_output(1, output.clone(), String::new());
        result.set_output(Some(OutputData::text(output)));
        result
    }
}

/// Apply ANSI color codes to pre-formatted unified diff output.
fn colorize_unified_output(plain: &str) -> String {
    let mut output = String::with_capacity(plain.len() + 256);

    for line in plain.lines() {
        if line.starts_with("---") || line.starts_with("+++") {
            output.push_str("\x1b[1m");
            output.push_str(line);
            output.push_str("\x1b[0m");
        } else if line.starts_with("@@") {
            output.push_str("\x1b[36m");
            output.push_str(line);
            output.push_str("\x1b[0m");
        } else if line.starts_with('-') {
            output.push_str("\x1b[31m");
            output.push_str(line);
            output.push_str("\x1b[0m");
        } else if line.starts_with('+') {
            output.push_str("\x1b[32m");
            output.push_str(line);
            output.push_str("\x1b[0m");
        } else {
            output.push_str(line);
        }
        output.push('\n');
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
        assert!(result.text_out().contains("---"));
        assert!(result.text_out().contains("+++"));
        assert!(result.text_out().contains("-line2"));
        assert!(result.text_out().contains("+modified"));
    }

    #[tokio::test]
    async fn test_diff_files_identical() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("same1.txt".into()));
        args.positional.push(Value::String("same2.txt".into()));

        let result = Diff.execute(args, &mut ctx).await;
        assert!(result.ok()); // Files identical = exit 0
        assert!(result.text_out().is_empty());
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
        assert!(result.text_out().contains("differ"));
        assert!(!result.text_out().contains("---")); // No diff output
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
    async fn test_diff_correct_hunk_header() {
        // Bug G: hunk header should show actual line positions, not always @@ -1 +1 @@
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(
            Path::new("a.txt"),
            b"line1\nline2\nline3\nline4\nline5\nline6\n",
        )
        .await
        .unwrap();
        mem.write(
            Path::new("b.txt"),
            b"line1\nline2\nline3\nline4\nchanged5\nline6\n",
        )
        .await
        .unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a.txt".into()));
        args.positional.push(Value::String("b.txt".into()));

        let result = Diff.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        // The hunk header should reference line 5, not line 1
        assert!(
            result.text_out().contains("@@ -") && !result.text_out().contains("@@ -1 +1 @@"),
            "hunk header should show correct line positions: {}",
            result.text_out()
        );
    }

    #[tokio::test]
    async fn test_diff_colorized_correct_hunk_header() {
        // Colorized diff should also have correct hunk headers
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(
            Path::new("a.txt"),
            b"line1\nline2\nline3\nline4\nline5\nline6\n",
        )
        .await
        .unwrap();
        mem.write(
            Path::new("b.txt"),
            b"line1\nline2\nline3\nline4\nchanged5\nline6\n",
        )
        .await
        .unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a.txt".into()));
        args.positional.push(Value::String("b.txt".into()));
        args.flags.insert("color".to_string());

        let result = Diff.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        // Should contain ANSI codes and correct hunk header (not -1 +1)
        assert!(result.text_out().contains("\x1b["), "should have ANSI codes");
        assert!(
            !result.text_out().contains("@@ -1 +1 @@"),
            "should not have hardcoded hunk header: {}",
            result.text_out()
        );
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
        assert!(result.text_out().contains("\x1b["));
    }
}
