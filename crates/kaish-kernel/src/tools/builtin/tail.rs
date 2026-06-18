//! tail — Output the last part of files.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Tail tool: output the last part of files or stdin.
pub struct Tail;

/// clap-derived argv layer for tail. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "tail", about = "Output the last part of files")]
struct TailArgs {
    /// Number of lines to output (-n)
    #[arg(short = 'n', long = "lines")]
    lines: Option<i64>,

    /// Number of bytes to output (-c), overrides lines
    #[arg(short = 'c', long = "bytes")]
    bytes: Option<i64>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to read; reads stdin when none are given.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Tail {
    fn name(&self) -> &str {
        "tail"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TailArgs::command(),
            "tail",
            "Output the last part of files",
            [
                ("Last 10 lines (default)", "tail file.txt"),
                ("Last 20 lines", "tail -n 20 log.txt"),
                ("From line 2 to the end", "tail -n +2 log.txt"),
                ("Last 1000 bytes", "tail -c 1000 file.txt"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // Handle POSIX shorthand: tail -3 file → tail -n 3 file
        // Lexer tokenizes "-3" as Int(-3), which lands in positional[0].
        if let Some(Value::Int(n)) = args.positional.first() {
            if *n < 0 {
                let count = n.unsigned_abs() as i64;
                args.named.insert("lines".to_string(), Value::Int(count));
                args.positional.remove(0);
            }
        }

        let parsed = match TailArgs::try_parse_from(
            std::iter::once("tail".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("tail: {e}")),
        };
        parsed.global.apply(ctx);

        // Collect all file paths, expanding globs
        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("tail: {}", e)),
        };

        // Multiple files: show each with header
        if paths.len() > 1 {
            return self.tail_files(ctx, &args, &paths).await;
        }

        // Byte mode (-c) resolved up front: it reads raw bytes so binary tails
        // to a Bytes result instead of being rejected or lossy-decoded.
        // `-c -N` (explicit "from the end" sign) means the same as `-c N`: last
        // N bytes. A bare `as usize` on the negative `Int` would wrap and emit
        // the whole input.
        let bytes = args.get("bytes", usize::MAX).and_then(|v| match v {
            Value::Int(i) => Some(i.unsigned_abs() as usize),
            Value::String(s) => s.trim_start_matches('-').parse().ok(),
            _ => None,
        });

        if let Some(byte_count) = bytes {
            let data: Vec<u8> = match paths.first() {
                Some(path) => {
                    let resolved = ctx.resolve_path(path);
                    match ctx.backend.read(Path::new(&resolved), None).await {
                        Ok(d) => d,
                        Err(e) => return ExecResult::failure(1, format!("tail: {}: {}", path, e)),
                    }
                }
                None => ctx.read_stdin_to_bytes().await.unwrap_or_default(),
            };
            // Last N bytes; valid UTF-8 stays text, otherwise a Bytes result.
            let start = data.len().saturating_sub(byte_count);
            return ExecResult::success_text_or_bytes(data[start..].to_vec());
        }

        // Line mode: text input (a binary file/stream is a loud error).
        let input = match paths.first() {
            Some(path) => {
                let resolved = ctx.resolve_path(path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            return ExecResult::failure(
                                1,
                                format!("tail: {}: invalid UTF-8", path),
                            )
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("tail: {}: {}", path, e)),
                }
            }
            None => match ctx.read_stdin_to_text().await {
                Ok(s) => s.unwrap_or_default(),
                Err(e) => return ExecResult::failure(2, format!("tail: {e}")),
            },
        };

        // Line mode: `-n N` = last N lines; `-n +N` = from line N (1-based).
        let (count, from_start) = parse_line_spec(&args);

        let all_lines: Vec<&str> = input.lines().collect();
        let total = all_lines.len();
        let skip_count = if from_start {
            // From line N: skip the first N-1 lines (`+1` = whole input).
            count.saturating_sub(1).min(total)
        } else {
            total.saturating_sub(count)
        };
        let output_lines: Vec<&str> = all_lines.into_iter().skip(skip_count).collect();

        if output_lines.is_empty() {
            ExecResult::with_output(OutputData::new())
        } else {
            // Build nodes with line numbers as cells (actual line number in file)
            let nodes: Vec<OutputNode> = output_lines
                .iter()
                .enumerate()
                .map(|(i, line)| {
                    let line_num = skip_count + i + 1;
                    OutputNode::new(*line).with_cells(vec![line_num.to_string()])
                })
                .collect();

            let output_data = OutputData::table(
                vec!["LINE".to_string(), "NUM".to_string()],
                nodes,
            );
            ExecResult::with_output_and_text(output_data, format!("{}\n", output_lines.join("\n")))
        }
    }
}

/// Resolve the `-n` line spec into `(count, from_start)`. `-n +N` (lexed as a
/// String with a leading `+`) means "from line N" (1-based); plain `-n N` and
/// the explicit `-n -N` both mean "last N". A bare `parse()` would silently drop
/// the `+` and conflate the first two; a bare `as usize` on a negative `Int`
/// wraps to ~`usize::MAX` and silently emits the whole input.
fn parse_line_spec(args: &ToolArgs) -> (usize, bool) {
    match args.get("lines", usize::MAX) {
        Some(Value::String(s)) if s.starts_with('+') => {
            (s[1..].parse().unwrap_or(1), true)
        }
        Some(Value::Int(i)) => (i.unsigned_abs() as usize, false),
        // A leading `-` is the explicit "from the end" sign (same as no sign).
        Some(Value::String(s)) => (s.trim_start_matches('-').parse().unwrap_or(10), false),
        _ => (10, false),
    }
}

impl Tail {
    /// Tail for multiple files: show each with `==> filename <==` header.
    async fn tail_files(&self, ctx: &mut ExecContext, args: &ToolArgs, paths: &[String]) -> ExecResult {
        let (count, from_start) = parse_line_spec(args);

        let mut output = String::new();
        let multi = paths.len() > 1;

        for (i, path) in paths.iter().enumerate() {
            let resolved = ctx.resolve_path(path);

            match ctx.backend.read(std::path::Path::new(&resolved), None).await {
                Ok(data) => match String::from_utf8(data) {
                    Ok(content) => {
                        if multi {
                            if i > 0 { output.push('\n'); }
                            output.push_str(&format!("==> {} <==\n", path));
                        }
                        let all_lines: Vec<&str> = content.lines().collect();
                        let skip = if from_start {
                            count.saturating_sub(1).min(all_lines.len())
                        } else {
                            all_lines.len().saturating_sub(count)
                        };
                        let tail: Vec<&str> = all_lines.into_iter().skip(skip).collect();
                        output.push_str(&tail.join("\n"));
                        output.push('\n');
                    }
                    Err(_) => return ExecResult::failure(1, format!("tail: {}: invalid UTF-8", path)),
                },
                Err(e) => return ExecResult::failure(1, format!("tail: {}: {}", path, e)),
            }
        }

        let trimmed = output.trim_end().to_string();
        ExecResult::with_output(OutputData::text(trimmed))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(
            Path::new("lines.txt"),
            b"line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9\nline 10\nline 11\nline 12",
        )
        .await
        .unwrap();
        mem.write(Path::new("short.txt"), b"one\ntwo\nthree")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_tail_default_10_lines() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 10);
        assert_eq!(lines[0], "line 3");
        assert_eq!(lines[9], "line 12");
    }

    #[tokio::test]
    async fn test_tail_custom_lines() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(3));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[0], "line 10");
        assert_eq!(lines[2], "line 12");
    }

    #[tokio::test]
    async fn test_tail_bytes() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("bytes".to_string(), Value::Int(5));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "three");
    }

    #[tokio::test]
    async fn test_tail_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("alpha\nbeta\ngamma\ndelta\n".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!result.text_out().contains("alpha"));
        assert!(!result.text_out().contains("beta"));
        assert!(result.text_out().contains("gamma"));
        assert!(result.text_out().contains("delta"));
    }

    #[tokio::test]
    async fn test_tail_fewer_lines_than_requested() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(100));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3);
    }

    #[tokio::test]
    async fn test_tail_file_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    // --- Additional tests for common patterns ---

    #[tokio::test]
    async fn test_tail_zero_lines() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(0));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_tail_one_line() {
        // tail -n 1 (very common pattern)
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(1));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "line 12");
    }

    #[tokio::test]
    async fn test_tail_unicode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本語\n中国語\n英語\n".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["中国語", "英語"]);
    }

    #[tokio::test]
    async fn test_tail_bytes_unicode() {
        // Byte mode with multibyte chars — POSIX tail -c counts bytes
        let mut ctx = make_ctx().await;
        ctx.set_stdin("abc日本語".to_string()); // 3 ASCII + 9 UTF-8 bytes = 12 total

        let mut args = ToolArgs::new();
        args.named.insert("bytes".to_string(), Value::Int(3));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Last 3 bytes = last UTF-8 char "語" (e8 aa 9e)
        assert_eq!(result.text_out().as_ref(), "語");
    }

    #[tokio::test]
    async fn test_tail_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_tail_single_line_no_newline() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("single line".to_string());

        let args = ToolArgs::new();
        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "single line");
    }

    #[tokio::test]
    async fn test_tail_more_lines_than_available() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(1000));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3);
    }

    #[tokio::test]
    async fn test_tail_exact_match() {
        // Request exactly the number of lines available
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(3));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["one", "two", "three"]);
    }

    #[tokio::test]
    async fn test_tail_posix_dash_number() {
        // Bug 5: tail -3 should be shorthand for tail -n 3
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(-3)); // lexer produces Int(-3) for "-3"
        args.positional.push(Value::String("/lines.txt".into()));
        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().lines().count(), 3);
        assert!(result.text_out().contains("line 12")); // should be last 3 lines
    }

    #[tokio::test]
    async fn test_tail_posix_dash_number_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("a\nb\nc\nd\ne\nf\ng\n".to_string());
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(-3));
        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().lines().count(), 3);
        assert!(result.text_out().contains("e"));
        assert!(result.text_out().contains("f"));
        assert!(result.text_out().contains("g"));
    }

    #[tokio::test]
    async fn test_tail_glob() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Multiple files: should have headers
        assert!(result.text_out().contains("==>"));
        // Should contain last 2 lines from lines.txt
        assert!(result.text_out().contains("line 11"));
        assert!(result.text_out().contains("line 12"));
        // Should contain last 2 lines from short.txt
        assert!(result.text_out().contains("two"));
        assert!(result.text_out().contains("three"));
    }

    #[tokio::test]
    async fn test_tail_multiple_files() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should show both files with headers
        assert!(result.text_out().contains("==> /lines.txt <=="));
        assert!(result.text_out().contains("==> /short.txt <=="));
        assert!(result.text_out().contains("line 12"));
        assert!(result.text_out().contains("three"));
    }
}
