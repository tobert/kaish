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
use similar::{ChangeTag, TextDiff};
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, validate_against_schema, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use crate::validator::{IssueCode, ValidationIssue};

/// Diff tool: compares two files line by line.
pub struct Diff;

/// clap-derived argv layer for diff.
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

        // Quick check if files are identical. The human/pipe path stays empty
        // (exit 0, no output), but a `--json` consumer still gets a consistent
        // object — `differ:false` plus empty `hunks` — instead of an empty
        // string that won't parse. Mirrors the quiet-mode `differ` signal.
        if content1 == content2 {
            let rich = serde_json::json!({
                "old_file": file1,
                "new_file": file2,
                "differ": false,
                "hunks": [],
            });
            let data = OutputData::text(String::new()).with_rich_json(rich);
            return ExecResult::with_output_and_text(data, String::new());
        }

        // Generate diff using similar's built-in unified format
        let diff = TextDiff::from_lines(&content1, &content2);

        // Quiet mode: just report difference. `--json` consumers still get a
        // structured signal (`{old_file, new_file, differ}`) via rich_json.
        if quiet {
            let text = format!("Files {} and {} differ\n", file1, file2);
            let rich = serde_json::json!({
                "old_file": file1,
                "new_file": file2,
                "differ": true,
            });
            let data = OutputData::text(text.clone()).with_rich_json(rich);
            return ExecResult::with_output_and_text(data, text).with_code(1);
        }

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

        // Structured hunks for `--json` (always built — cheap; the kernel only
        // serializes it when `--json` was requested). Mirrors grep's rich_json.
        let rich = diff_to_json(&diff, &file1, &file2, context_lines);
        let data = OutputData::text(output.clone()).with_rich_json(rich);

        // Exit code 1 if files differ (POSIX convention).
        ExecResult::with_output_and_text(data, output).with_code(1)
    }
}

/// Build the structured `--json` representation: one object per file pair with
/// `hunks`, each hunk carrying its `@@` line ranges (1-indexed) and a `changes`
/// list of `{tag, content}` (tag ∈ equal|delete|insert; content has its trailing
/// newline stripped so `jq` sees clean line text).
fn diff_to_json<'a>(
    diff: &TextDiff<'a, 'a, 'a, str>,
    file1: &str,
    file2: &str,
    context: usize,
) -> serde_json::Value {
    let mut unified = diff.unified_diff();
    unified.context_radius(context);

    let mut hunks = Vec::new();
    for hunk in unified.iter_hunks() {
        let ops = hunk.ops();
        let (Some(first), Some(last)) = (ops.first(), ops.last()) else {
            continue;
        };
        let old_start = first.old_range().start;
        let old_end = last.old_range().end;
        let new_start = first.new_range().start;
        let new_end = last.new_range().end;
        // Match the unified `@@` convention (and `similar`'s own header): a
        // non-empty range is 1-indexed (start + 1); a zero-length range (a pure
        // insertion or deletion) reports the 0-indexed line it sits *after*, so
        // we don't point one past the edit site.
        let line_no = |start: usize, len: usize| if len == 0 { start } else { start + 1 };

        let mut changes = Vec::new();
        for change in hunk.iter_changes() {
            let tag = match change.tag() {
                ChangeTag::Equal => "equal",
                ChangeTag::Delete => "delete",
                ChangeTag::Insert => "insert",
            };
            let mut content = change.value().to_string();
            if content.ends_with('\n') {
                content.pop();
                if content.ends_with('\r') {
                    content.pop();
                }
            }
            // `missing_newline` preserves the terminal-newline signal the text
            // path shows as `\ No newline at end of file` — without it a change
            // that only toggles the file's final newline is byte-identical to a
            // no-op (both sides strip to the same `content`).
            changes.push(serde_json::json!({
                "tag": tag,
                "content": content,
                "missing_newline": change.missing_newline(),
            }));
        }

        hunks.push(serde_json::json!({
            "old_start": line_no(old_start, old_end - old_start),
            "old_lines": old_end - old_start,
            "new_start": line_no(new_start, new_end - new_start),
            "new_lines": new_end - new_start,
            "changes": changes,
        }));
    }

    serde_json::json!({
        "old_file": file1,
        "new_file": file2,
        "differ": true,
        "hunks": hunks,
    })
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

    #[tokio::test]
    async fn test_diff_json_structured_hunks() {
        use crate::interpreter::{apply_output_format, OutputFormat};
        // file1.txt = line1/line2/line3, file2.txt = line1/modified/line3.
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("file1.txt".into()));
        args.positional.push(Value::String("file2.txt".into()));

        let result = Diff.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        // The text payload is still the unified diff (pipe/human path).
        assert!(result.text_out().contains("-line2"));

        // --json path: rich_json structured hunks, not the text wrapped.
        let formatted = apply_output_format(result, OutputFormat::Json);
        let json: serde_json::Value =
            serde_json::from_str(&formatted.text_out()).expect("valid JSON");
        assert_eq!(json["old_file"], "file1.txt");
        assert_eq!(json["new_file"], "file2.txt");
        let hunks = json["hunks"].as_array().expect("hunks array");
        assert_eq!(hunks.len(), 1, "one hunk: {json}");
        let changes = hunks[0]["changes"].as_array().expect("changes array");
        // The hunk must carry the delete (line2) and insert (modified) with
        // clean, newline-stripped content.
        assert!(changes.iter().any(|c| c["tag"] == "delete" && c["content"] == "line2"));
        assert!(changes.iter().any(|c| c["tag"] == "insert" && c["content"] == "modified"));
        assert!(changes.iter().any(|c| c["tag"] == "equal" && c["content"] == "line1"));
    }

    #[tokio::test]
    async fn test_diff_json_correct_line_ranges() {
        use crate::interpreter::{apply_output_format, OutputFormat};
        // Change is on line 5 of 6 — the hunk header must report line 5, not 1.
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("a.txt"), b"l1\nl2\nl3\nl4\nl5\nl6\n").await.unwrap();
        mem.write(Path::new("b.txt"), b"l1\nl2\nl3\nl4\nCHANGED\nl6\n").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a.txt".into()));
        args.positional.push(Value::String("b.txt".into()));
        let result = Diff.execute(args, &mut ctx).await;

        let formatted = apply_output_format(result, OutputFormat::Json);
        let json: serde_json::Value =
            serde_json::from_str(&formatted.text_out()).expect("valid JSON");
        let hunk = &json["hunks"][0];
        // Default context 3: hunk spans lines 2..6, change at 5.
        assert_eq!(hunk["old_start"], 2, "hunk: {hunk}");
        assert!(hunk["changes"].as_array().unwrap().iter()
            .any(|c| c["tag"] == "delete" && c["content"] == "l5"));
    }

    #[tokio::test]
    async fn test_diff_json_pure_insertion_line_numbers() {
        use crate::interpreter::{apply_output_format, OutputFormat};
        // Old side empty (0 lines): a zero-length range reports the line it
        // follows (0), not one past it (1) — matches the `@@` convention.
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("empty.txt"), b"").await.unwrap();
        mem.write(Path::new("full.txt"), b"new line\n").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("empty.txt".into()));
        args.positional.push(Value::String("full.txt".into()));
        let result = Diff.execute(args, &mut ctx).await;

        let formatted = apply_output_format(result, OutputFormat::Json);
        let json: serde_json::Value =
            serde_json::from_str(&formatted.text_out()).expect("valid JSON");
        let hunk = &json["hunks"][0];
        assert_eq!(hunk["old_lines"], 0, "hunk: {hunk}");
        assert_eq!(hunk["old_start"], 0, "count-0 range reports the preceding line: {hunk}");
    }

    #[tokio::test]
    async fn test_diff_json_marks_missing_newline() {
        use crate::interpreter::{apply_output_format, OutputFormat};
        // A change that only toggles the file's terminal newline must stay
        // distinguishable in --json: old "A" (no newline) and new "A" (with
        // newline) both strip to "A", so without `missing_newline` the two
        // changes are byte-identical and the intent is lost (the text path
        // shows it as `\ No newline at end of file`).
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("no_nl.txt"), b"A").await.unwrap();
        mem.write(Path::new("with_nl.txt"), b"A\n").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("no_nl.txt".into()));
        args.positional.push(Value::String("with_nl.txt".into()));
        let result = Diff.execute(args, &mut ctx).await;

        let formatted = apply_output_format(result, OutputFormat::Json);
        let json: serde_json::Value =
            serde_json::from_str(&formatted.text_out()).expect("valid JSON");
        let changes = json["hunks"][0]["changes"].as_array().expect("changes array");
        assert!(
            changes.iter().any(|c| c["tag"] == "delete"
                && c["content"] == "A"
                && c["missing_newline"] == true),
            "deleted line lacked a trailing newline; must be flagged: {json}"
        );
        assert!(
            changes.iter().any(|c| c["tag"] == "insert"
                && c["content"] == "A"
                && c["missing_newline"] == false),
            "inserted line has a trailing newline: {json}"
        );
    }

    #[tokio::test]
    async fn test_diff_json_identical_emits_object() {
        use crate::interpreter::{apply_output_format, OutputFormat};
        // Identical files must still yield a well-formed `--json` object so an
        // agent iterating file pairs gets a consistent shape (`differ:false`,
        // empty `hunks`), not an empty string that won't parse.
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("same1.txt".into()));
        args.positional.push(Value::String("same2.txt".into()));

        let result = Diff.execute(args, &mut ctx).await;
        assert!(result.ok(), "identical files exit 0: {}", result.err);
        // Plain/pipe path stays empty (unchanged human-facing output).
        assert!(result.text_out().is_empty());

        let formatted = apply_output_format(result, OutputFormat::Json);
        let json: serde_json::Value = serde_json::from_str(&formatted.text_out())
            .expect("identical files must still emit valid JSON");
        assert_eq!(json["differ"], false);
        assert_eq!(json["old_file"], "same1.txt");
        assert_eq!(json["new_file"], "same2.txt");
        assert_eq!(
            json["hunks"].as_array().expect("hunks array").len(),
            0,
            "no hunks for identical files: {json}"
        );
    }

    #[tokio::test]
    async fn test_diff_json_quiet_emits_differ_object() {
        use crate::interpreter::{apply_output_format, OutputFormat};
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("file1.txt".into()));
        args.positional.push(Value::String("file2.txt".into()));
        args.flags.insert("q".to_string());

        let result = Diff.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        let formatted = apply_output_format(result, OutputFormat::Json);
        let json: serde_json::Value =
            serde_json::from_str(&formatted.text_out()).expect("valid JSON");
        assert_eq!(json["differ"], true);
        assert_eq!(json["old_file"], "file1.txt");
    }
}
