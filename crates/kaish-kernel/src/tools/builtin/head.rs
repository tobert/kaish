//! head — Output the first part of files.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::glob::contains_glob;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Head tool: output the first part of files or stdin.
pub struct Head;

#[async_trait]
impl Tool for Head {
    fn name(&self) -> &str {
        "head"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("head", "Output the first part of files")
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::Null,
                "File to read (reads stdin if not provided)",
            ))
            .param(ParamSchema::optional(
                "lines",
                "int",
                Value::Int(10),
                "Number of lines to output (-n)",
            ).with_aliases(["-n"]))
            .param(ParamSchema::optional(
                "bytes",
                "int",
                Value::Null,
                "Number of bytes to output (-c), overrides lines",
            ).with_aliases(["-c"]))
            .example("First 10 lines (default)", "head file.txt")
            .example("First 5 lines", "head -n 5 file.txt")
            .example("First 100 bytes", "head -c 100 file.txt")
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Handle POSIX shorthand: head -3 file → head -n 3 file
        // Lexer tokenizes "-3" as Int(-3), which lands in positional[0].
        if let Some(Value::Int(n)) = args.positional.first() {
            if *n < 0 {
                let count = n.unsigned_abs() as i64;
                args.named.insert("lines".to_string(), Value::Int(count));
                args.positional.remove(0);
            }
        }

        // Collect all file paths, expanding globs
        let mut paths: Vec<String> = Vec::new();
        for arg in &args.positional {
            if let Value::String(s) = arg {
                if contains_glob(s) {
                    match ctx.expand_glob(s).await {
                        Ok(expanded) => {
                            let root = ctx.resolve_path(".");
                            for p in expanded {
                                let rel = p.strip_prefix(&root).unwrap_or(&p);
                                paths.push(rel.to_string_lossy().to_string());
                            }
                        }
                        Err(e) => return ExecResult::failure(1, format!("head: {}", e)),
                    }
                } else {
                    paths.push(s.clone());
                }
            }
        }

        // Multiple files: show each with header
        if paths.len() > 1 {
            return self.head_files(ctx, &args, &paths).await;
        }

        // Streaming path: read from pipe_stdin line by line, stop after N lines
        // This enables early termination — `seq 1 1000000 | head -5` stops after 5 lines
        if paths.is_empty() && let Some(pipe_in) = ctx.pipe_stdin.take() {
            let bytes = args.get("bytes", usize::MAX).and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            });
            if bytes.is_some() {
                // Put pipe back — bytes mode doesn't use streaming
                ctx.pipe_stdin = Some(pipe_in);
            } else {
                let lines = args
                    .get("lines", usize::MAX)
                    .and_then(|v| match v {
                        Value::Int(i) => Some(*i as usize),
                        Value::String(s) => s.parse().ok(),
                        _ => None,
                    })
                    .unwrap_or(10);

                let lines = if args.has_flag("n") {
                    args.get("n", usize::MAX)
                        .and_then(|v| match v {
                            Value::Int(i) => Some(*i as usize),
                            Value::String(s) => s.parse().ok(),
                            _ => None,
                        })
                        .unwrap_or(lines)
                } else {
                    lines
                };

                return self.stream_head_lines(ctx, pipe_in, lines).await;
            }
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
            None => ctx.read_stdin_to_string().await.unwrap_or_default(),
        };

        // Check for byte mode (-c)
        let bytes = args.get("bytes", usize::MAX).and_then(|v| match v {
            Value::Int(i) => Some(*i as usize),
            Value::String(s) => s.parse().ok(),
            _ => None,
        });

        if let Some(byte_count) = bytes {
            // Byte mode: output first N bytes (keep as text, no structure)
            let output: String = input.chars().take(byte_count).collect();
            return ExecResult::with_output(OutputData::text(output));
        }

        // Line mode: output first N lines
        let lines = args
            .get("lines", usize::MAX)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
            .unwrap_or(10);

        // Handle -n flag as alias
        let lines = if args.has_flag("n") {
            args.get("n", usize::MAX)
                .and_then(|v| match v {
                    Value::Int(i) => Some(*i as usize),
                    Value::String(s) => s.parse().ok(),
                    _ => None,
                })
                .unwrap_or(lines)
        } else {
            lines
        };

        let output_lines: Vec<&str> = input.lines().take(lines).collect();
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
                vec!["Line".to_string(), "Num".to_string()],
                nodes,
            );
            let mut result = ExecResult::with_output(output_data);
            // Override canonical output with traditional format
            result.out = format!("{}\n", output_lines.join("\n"));
            result
        }
    }
}

impl Head {
    /// Head for multiple files: show each with `==> filename <==` header.
    async fn head_files(&self, ctx: &mut ExecContext, args: &ToolArgs, paths: &[String]) -> ExecResult {
        let lines = Self::parse_line_count(args);
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
                        let head: Vec<&str> = content.lines().take(lines).collect();
                        output.push_str(&head.join("\n"));
                        output.push('\n');
                    }
                    Err(_) => return ExecResult::failure(1, format!("head: {}: invalid UTF-8", path)),
                },
                Err(e) => return ExecResult::failure(1, format!("head: {}: {}", path, e)),
            }
        }

        let mut result = ExecResult::with_output(OutputData::text(output.trim_end().to_string()));
        result.out = output.trim_end().to_string();
        result
    }

    /// Parse line count from args (shared by execute and head_glob).
    fn parse_line_count(args: &ToolArgs) -> usize {
        let lines = args
            .get("lines", usize::MAX)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
            .unwrap_or(10);

        if args.has_flag("n") {
            args.get("n", usize::MAX)
                .and_then(|v| match v {
                    Value::Int(i) => Some(*i as usize),
                    Value::String(s) => s.parse().ok(),
                    _ => None,
                })
                .unwrap_or(lines)
        } else {
            lines
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
                Err(_) => break,
            }
        }

        // Drop reader (and pipe_stdin inside it) — signals broken pipe to upstream
        drop(reader);

        if let Some(mut out) = pipe_out {
            let _ = out.shutdown().await;
            ExecResult::success("")
        } else {
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
                vec!["Line".to_string(), "Num".to_string()],
                nodes,
            );
            let mut result = ExecResult::with_output(output_data);
            result.out = format!("{}\n", output_lines.join("\n"));
            result
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        assert_eq!(result.out, "one\nt");
    }

    #[tokio::test]
    async fn test_head_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("alpha\nbeta\ngamma\ndelta\n".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("alpha"));
        assert!(result.out.contains("beta"));
        assert!(!result.out.contains("gamma"));
    }

    #[tokio::test]
    async fn test_head_fewer_lines_than_requested() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(100));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
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
        assert!(result.out.is_empty());
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
        assert_eq!(result.out.trim(), "line 1");
    }

    #[tokio::test]
    async fn test_head_unicode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本語\n中国語\n英語\n".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines, vec!["日本語", "中国語"]);
    }

    #[tokio::test]
    async fn test_head_bytes_unicode() {
        // Byte mode with multibyte chars
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本語".to_string()); // 9 bytes in UTF-8

        let mut args = ToolArgs::new();
        args.named.insert("bytes".to_string(), Value::Int(3));

        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Note: we're taking 3 chars (head -c counts chars not bytes in our impl)
        assert_eq!(result.out.chars().count(), 3);
    }

    #[tokio::test]
    async fn test_head_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.is_empty());
    }

    #[tokio::test]
    async fn test_head_single_line_no_newline() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("single line no newline".to_string());

        let args = ToolArgs::new();
        let result = Head.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "single line no newline");
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        assert_eq!(result.out.lines().count(), 3);
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
        assert_eq!(result.out.lines().count(), 5);
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
        assert!(result.out.contains("==>"));
        // Should contain first 2 lines from lines.txt
        assert!(result.out.contains("line 1"));
        assert!(result.out.contains("line 2"));
        // Should contain first 2 lines from short.txt
        assert!(result.out.contains("one"));
        assert!(result.out.contains("two"));
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
        assert!(result.out.contains("==> /lines.txt <=="));
        assert!(result.out.contains("==> /short.txt <=="));
        assert!(result.out.contains("line 1"));
        assert!(result.out.contains("one"));
    }
}
