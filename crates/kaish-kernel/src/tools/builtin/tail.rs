//! tail — Output the last part of files.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::glob::contains_glob;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Tail tool: output the last part of files or stdin.
pub struct Tail;

#[async_trait]
impl Tool for Tail {
    fn name(&self) -> &str {
        "tail"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("tail", "Output the last part of files")
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
            .example("Last 10 lines (default)", "tail file.txt")
            .example("Last 20 lines", "tail -n 20 log.txt")
            .example("Last 1000 bytes", "tail -c 1000 file.txt")
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Handle POSIX shorthand: tail -3 file → tail -n 3 file
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
                        Err(e) => return ExecResult::failure(1, format!("tail: {}", e)),
                    }
                } else {
                    paths.push(s.clone());
                }
            }
        }

        // Multiple files: show each with header
        if paths.len() > 1 {
            return self.tail_files(ctx, &args, &paths).await;
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
                                format!("tail: {}: invalid UTF-8", path),
                            )
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("tail: {}: {}", path, e)),
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
            // Byte mode: output last N bytes (keep as text, no structure)
            let char_count = input.chars().count();
            let skip = char_count.saturating_sub(byte_count);
            let output: String = input.chars().skip(skip).collect();
            return ExecResult::with_output(OutputData::text(output));
        }

        // Line mode: output last N lines
        let lines = args
            .get("lines", usize::MAX)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
            .unwrap_or(10);

        let all_lines: Vec<&str> = input.lines().collect();
        let total = all_lines.len();
        let skip_count = total.saturating_sub(lines);
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

impl Tail {
    /// Tail for multiple files: show each with `==> filename <==` header.
    async fn tail_files(&self, ctx: &mut ExecContext, args: &ToolArgs, paths: &[String]) -> ExecResult {
        let lines = args
            .get("lines", usize::MAX)
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            })
            .unwrap_or(10);

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
                        let skip = all_lines.len().saturating_sub(lines);
                        let tail: Vec<&str> = all_lines.into_iter().skip(skip).collect();
                        output.push_str(&tail.join("\n"));
                        output.push('\n');
                    }
                    Err(_) => return ExecResult::failure(1, format!("tail: {}: invalid UTF-8", path)),
                },
                Err(e) => return ExecResult::failure(1, format!("tail: {}: {}", path, e)),
            }
        }

        let mut result = ExecResult::with_output(OutputData::text(output.trim_end().to_string()));
        result.out = output.trim_end().to_string();
        result
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        assert_eq!(result.out, "three");
    }

    #[tokio::test]
    async fn test_tail_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("alpha\nbeta\ngamma\ndelta\n".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!result.out.contains("alpha"));
        assert!(!result.out.contains("beta"));
        assert!(result.out.contains("gamma"));
        assert!(result.out.contains("delta"));
    }

    #[tokio::test]
    async fn test_tail_fewer_lines_than_requested() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(100));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
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
        assert!(result.out.is_empty());
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
        assert_eq!(result.out.trim(), "line 12");
    }

    #[tokio::test]
    async fn test_tail_unicode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("日本語\n中国語\n英語\n".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("lines".to_string(), Value::Int(2));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
        assert_eq!(lines, vec!["中国語", "英語"]);
    }

    #[tokio::test]
    async fn test_tail_bytes_unicode() {
        // Byte mode with multibyte chars
        let mut ctx = make_ctx().await;
        ctx.set_stdin("abc日本語".to_string()); // 3 ASCII + 9 UTF-8 bytes

        let mut args = ToolArgs::new();
        args.named.insert("bytes".to_string(), Value::Int(3));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Takes last 3 chars (our impl uses chars not bytes)
        assert_eq!(result.out, "日本語");
    }

    #[tokio::test]
    async fn test_tail_empty_input() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.is_empty());
    }

    #[tokio::test]
    async fn test_tail_single_line_no_newline() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("single line".to_string());

        let args = ToolArgs::new();
        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "single line");
    }

    #[tokio::test]
    async fn test_tail_more_lines_than_available() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/short.txt".into()));
        args.named.insert("lines".to_string(), Value::Int(1000));

        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        let lines: Vec<&str> = result.out.lines().collect();
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
        let lines: Vec<&str> = result.out.lines().collect();
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
        assert_eq!(result.out.lines().count(), 3);
        assert!(result.out.contains("line 12")); // should be last 3 lines
    }

    #[tokio::test]
    async fn test_tail_posix_dash_number_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("a\nb\nc\nd\ne\nf\ng\n".to_string());
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(-3));
        let result = Tail.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.lines().count(), 3);
        assert!(result.out.contains("e"));
        assert!(result.out.contains("f"));
        assert!(result.out.contains("g"));
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
        assert!(result.out.contains("==>"));
        // Should contain last 2 lines from lines.txt
        assert!(result.out.contains("line 11"));
        assert!(result.out.contains("line 12"));
        // Should contain last 2 lines from short.txt
        assert!(result.out.contains("two"));
        assert!(result.out.contains("three"));
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
        assert!(result.out.contains("==> /lines.txt <=="));
        assert!(result.out.contains("==> /short.txt <=="));
        assert!(result.out.contains("line 12"));
        assert!(result.out.contains("three"));
    }
}
