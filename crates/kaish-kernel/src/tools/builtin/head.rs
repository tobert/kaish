//! head — Output the first part of files.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::ast::Value;
use crate::backend::ReadRange;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Head tool: output the first part of files or stdin.
pub struct Head;

/// clap-derived argv layer for head.
#[derive(Parser, Debug)]
#[command(name = "head", about = "Output the first part of files")]
struct HeadArgs {
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
impl Tool for Head {
    fn name(&self) -> &str {
        "head"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &HeadArgs::command(),
            "head",
            "Output the first part of files",
            [
                ("First 10 lines (default)", "head file.txt"),
                ("First 5 lines", "head -n 5 file.txt"),
                ("All but the last line", "head -n -1 file.txt"),
                ("First 100 bytes", "head -c 100 file.txt"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // Pop the POSIX shorthand `-N` Int before we hand off to clap below.
        // The pop transforms positional[0] = Int(-N) into named lines=N.
        // Handle POSIX shorthand: head -3 file → head -n 3 file
        // Lexer tokenizes "-3" as Int(-3), which lands in positional[0].
        if let Some(Value::Int(n)) = args.positional.first() {
            if *n < 0 {
                let count = n.unsigned_abs() as i64;
                args.named.insert("lines".to_string(), Value::Int(count));
                args.positional.remove(0);
            }
        }

        // Drop ambiguous flag-form duplicates: if a named value exists for the
        // same key as a flag (e.g. flags={"n"} AND named={"n": 3}), the flag
        // form is meaningless — clap would see `-n -n=3` and try to consume
        // the second `-n=3` as the value for the first `-n`. The named form
        // wins.
        for key in ["n", "lines", "c", "bytes"] {
            if args.named.contains_key(key) {
                args.flags.remove(key);
            }
        }

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("head: {e}")),
        };
        let parsed = match HeadArgs::try_parse_from(
            std::iter::once("head".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("head: {e}")),
        };
        parsed.global.apply(ctx);

        // Collect all file paths, expanding globs
        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("head: {}", e)),
        };

        // Byte count (-c) off the clap struct: clap already rejected
        // non-numeric values loudly; negative (GNU's "all but the last N
        // bytes") is unsupported and refused rather than usize-wrapped into
        // a huge count.
        let bytes: Option<usize> = match parsed.bytes {
            Some(b) if b < 0 => {
                return ExecResult::failure(
                    2,
                    format!("head: invalid byte count {b}: negative -c is not supported"),
                )
            }
            other => other.map(|b| b as usize),
        };

        // Multiple files: show each with header
        if paths.len() > 1 {
            return self.head_files(ctx, &args, &paths).await;
        }

        // Streaming path: read from pipe_stdin line by line, stop after N lines
        // This enables early termination — `seq 1 1000000 | head -5` stops after 5 lines
        if paths.is_empty() && let Some(pipe_in) = ctx.pipe_stdin.take() {
            let (count, all_but_last) = Self::line_spec(&args);
            if bytes.is_some() || all_but_last {
                // Bytes mode and `-n -N` (all but last N) both need the whole
                // input — they can't early-terminate. Put the pipe back and fall
                // through to the buffered path.
                ctx.pipe_stdin = Some(pipe_in);
            } else {
                return self.stream_head_lines(ctx, pipe_in, count).await;
            }
        }

        // Byte count (-c) is resolved up front (see `bytes` above) so a
        // single-file read can ask the backend for exactly that many bytes.
        // That keeps `head -c N` from pulling whole files into memory and,
        // crucially, lets it read endless devices like /dev/zero — a
        // whole-file read of those is a hard error.
        // Single-file byte mode: request exactly N bytes via the read range.
        if let (Some(byte_count), Some(path)) = (bytes, paths.first()) {
            let resolved = ctx.resolve_path(path);
            let range = Some(ReadRange::bytes(0, byte_count as u64));
            return match ctx.backend.read(Path::new(&resolved), range).await {
                // -c counts bytes and may slice a multibyte boundary. Valid
                // UTF-8 stays text; otherwise it's a Bytes result (hex dump /
                // base64 envelope / raw through a pipe) rather than a lossy mangle.
                Ok(data) => ExecResult::success_text_or_bytes(data),
                Err(e) => ExecResult::failure(1, format!("head: {}: {}", path, e)),
            };
        }

        // Stdin byte mode: read raw bytes so piped binary survives intact (the
        // single-file case returned above; here paths is empty).
        if let Some(byte_count) = bytes {
            let mut data = ctx.read_stdin_to_bytes().await.unwrap_or_default();
            data.truncate(byte_count);
            return ExecResult::success_text_or_bytes(data);
        }

        // Get input: from single file or stdin
        let input = match paths.first() {
            Some(path) => {
                let resolved = ctx.resolve_path(path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            return ExecResult::failure(
                                1,
                                format!("head: {}: invalid UTF-8", path),
                            )
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("head: {}: {}", path, e)),
                }
            }
            None => match ctx.read_stdin_to_text().await {
                Ok(s) => s.unwrap_or_default(),
                Err(e) => return ExecResult::failure(2, format!("head: {e}")),
            },
        };

        // Line mode: `-n N` = first N; `-n -N` = all but the last N.
        let (count, all_but_last) = Self::line_spec(&args);
        let all_lines: Vec<&str> = input.lines().collect();
        let take_n = if all_but_last {
            all_lines.len().saturating_sub(count)
        } else {
            count
        };
        let output_lines: Vec<&str> = all_lines.into_iter().take(take_n).collect();
        if output_lines.is_empty() {
            ExecResult::with_output(OutputData::new())
        } else {
            // Build nodes with line numbers as cells
            let nodes: Vec<OutputNode> = output_lines
                .iter()
                .enumerate()
                .map(|(i, line)| {
                    OutputNode::new(*line).with_cells(vec![(i + 1).to_string()])
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

impl Head {
    /// Head for multiple files: show each with `==> filename <==` header.
    async fn head_files(&self, ctx: &mut ExecContext, args: &ToolArgs, paths: &[String]) -> ExecResult {
        let (count, all_but_last) = Self::line_spec(args);
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
                        let file_lines: Vec<&str> = content.lines().collect();
                        let take_n = if all_but_last {
                            file_lines.len().saturating_sub(count)
                        } else {
                            count
                        };
                        let head: Vec<&str> = file_lines.into_iter().take(take_n).collect();
                        output.push_str(&head.join("\n"));
                        output.push('\n');
                    }
                    Err(_) => return ExecResult::failure(1, format!("head: {}: invalid UTF-8", path)),
                },
                Err(e) => return ExecResult::failure(1, format!("head: {}: {}", path, e)),
            }
        }

        let trimmed = output.trim_end().to_string();
        ExecResult::with_output(OutputData::text(trimmed))
    }

    /// Parse the `-n` line spec into `(count, all_but_last)`. A negative value
    /// (`-n -N`, lexed as `Int(-N)` or a `-`-prefixed string) means "all lines
    /// but the last N"; a bare `as usize` cast used to wrap it into a giant
    /// count and emit everything. The POSIX shorthand `head -N` is normalized to
    /// a positive `lines` before this runs, so a negative here is unambiguously
    /// the `-n -N` form. Shared by every head path (stream / single / multi).
    fn line_spec(args: &ToolArgs) -> (usize, bool) {
        fn pick(v: &Value) -> Option<(usize, bool)> {
            match v {
                Value::Int(i) if *i < 0 => Some((i.unsigned_abs() as usize, true)),
                Value::Int(i) => Some((*i as usize, false)),
                Value::String(s) if s.starts_with('-') => Some((s[1..].parse().ok()?, true)),
                Value::String(s) => Some((s.parse().ok()?, false)),
                _ => None,
            }
        }
        let base = args.get("lines", usize::MAX).and_then(pick).unwrap_or((10, false));
        if args.has_flag("n") {
            args.get("n", usize::MAX).and_then(pick).unwrap_or(base)
        } else {
            base
        }
    }

    /// Stream head: read lines from pipe_stdin, write to pipe_stdout or buffer,
    /// stop after `max_lines`. Drops pipe_stdin early to signal upstream to stop.
    async fn stream_head_lines(&self, ctx: &mut ExecContext, pipe_in: crate::scheduler::PipeReader, max_lines: usize) -> ExecResult {
        use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
        let mut reader = BufReader::new(pipe_in);
        let mut pipe_out = ctx.pipe_stdout.take();
        let mut buffered = String::new();
        let mut line_count = 0;

        let mut line_buf = String::new();
        while line_count < max_lines {
            line_buf.clear();
            match reader.read_line(&mut line_buf).await {
                Ok(0) => break, // EOF
                Ok(_) => {
                    line_count += 1;
                    if let Some(ref mut out) = pipe_out {
                        if out.write_all(line_buf.as_bytes()).await.is_err() {
                            break; // broken pipe
                        }
                    } else {
                        buffered.push_str(&line_buf);
                    }
                }
                // `read_line` rejects non-UTF-8 with `InvalidData`. Line-mode
                // head is text-only — error loudly rather than silently
                // truncating at the bad byte (binary stdin is reachable via the
                // lazy pipe). Other I/O errors end the stream as before.
                Err(e) if e.kind() == std::io::ErrorKind::InvalidData => {
                    return ExecResult::failure(
                        2,
                        "head: input is not valid UTF-8 (binary data?) — \
                         use `head -c N` for bytes",
                    );
                }
                Err(_) => break,
            }
        }

        // Drop reader (and pipe_stdin inside it) — signals broken pipe to upstream
        drop(reader);

        if let Some(mut out) = pipe_out {
            let _ = out.shutdown().await;
            ExecResult::success("")
        } else {
            // Empty input → empty output. Without this guard the `format!`
            // below emits a bare "\n" for `true | head` (the buffered path
            // guards this the same way).
            if buffered.is_empty() {
                return ExecResult::with_output(OutputData::new());
            }
            // Remove trailing newline for consistency with buffered path
            if buffered.ends_with('\n') {
                buffered.pop();
            }
            let output_lines: Vec<&str> = buffered.lines().collect();
            let nodes: Vec<OutputNode> = output_lines
                .iter()
                .enumerate()
                .map(|(i, line)| OutputNode::new(*line).with_cells(vec![(i + 1).to_string()]))
                .collect();
            let output_data = OutputData::table(
                vec!["LINE".to_string(), "NUM".to_string()],
                nodes,
            );
            ExecResult::with_output_and_text(output_data, format!("{}\n", output_lines.join("\n")))
        }
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
    async fn test_head_default_10_lines() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 10);
        assert_eq!(lines[0], "line 1");
        assert_eq!(lines[9], "line 10");
    }

    #[tokio::test]
    async fn test_head_custom_lines() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(3));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3);
        assert_eq!(lines[2], "line 3");
    }

    #[tokio::test]
    async fn test_head_bytes() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("bytes".to_string(), Value::Int(5));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "one\nt");
    }

    #[tokio::test]
    async fn test_head_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("alpha\nbeta\ngamma\ndelta\n".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("alpha"));
        assert!(result.text_out().contains("beta"));
        assert!(!result.text_out().contains("gamma"));
    }

    #[tokio::test]
    async fn test_head_fewer_lines_than_requested() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(100));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3);
    }

    #[tokio::test]
    async fn test_head_file_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Head.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    // --- Additional tests for common patterns ---

    #[tokio::test]
    async fn test_head_zero_lines() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(0));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_head_one_line() {
        // head -n 1 (very common pattern)
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(1));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "line 1");
    }

    #[tokio::test]
    async fn test_head_unicode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本語\n中国語\n英語\n".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines, vec!["日本語", "中国語"]);
    }

    #[tokio::test]
    async fn test_head_bytes_unicode() {
        // Byte mode with multibyte chars — POSIX head -c counts bytes
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本語".to_string()); // 9 bytes in UTF-8, 3 bytes per char

        let mut args = ToolArgs::new();
        args.named.insert("bytes".to_string(), Value::Int(3));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        // 3 bytes = first UTF-8 char "日" (e6 97 a5)
        assert_eq!(result.text_out().as_ref(), "日");
    }

    #[tokio::test]
    async fn test_head_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_head_single_line_no_newline() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("single line no newline".to_string());

        let args = ToolArgs::new();
        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "single line no newline");
    }

    #[tokio::test]
    async fn test_head_large_request() {
        // Requesting more than available
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(1000));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        let text = result.text_out();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines.len(), 3);
    }

    #[tokio::test]
    async fn test_head_posix_dash_number() {
        // Bug 5: head -3 should be shorthand for head -n 3
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(-3)); // lexer produces Int(-3) for "-3"
        args.positional.push(Value::String("/lines.txt".into()));
        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().lines().count(), 3);
    }

    #[tokio::test]
    async fn test_head_posix_dash_number_stdin() {
        // head -5 with stdin
        let mut ctx = make_ctx().await;
        ctx.set_stdin("a\nb\nc\nd\ne\nf\ng\n".to_string());
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(-5));
        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().lines().count(), 5);
    }

    #[tokio::test]
    async fn test_head_glob() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Multiple files: should have headers
        assert!(result.text_out().contains("==>"));
        // Should contain first 2 lines from lines.txt
        assert!(result.text_out().contains("line 1"));
        assert!(result.text_out().contains("line 2"));
        // Should contain first 2 lines from short.txt
        assert!(result.text_out().contains("one"));
        assert!(result.text_out().contains("two"));
    }

    #[tokio::test]
    async fn test_head_multiple_files() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should show both files with headers
        assert!(result.text_out().contains("==> /lines.txt <=="));
        assert!(result.text_out().contains("==> /short.txt <=="));
        assert!(result.text_out().contains("line 1"));
        assert!(result.text_out().contains("one"));
    }
}
