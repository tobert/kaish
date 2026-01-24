//! head — Output the first part of files.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
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
            ))
            .param(ParamSchema::optional(
                "bytes",
                "int",
                Value::Null,
                "Number of bytes to output (-c), overrides lines",
            ))
            .example("First 10 lines (default)", "head file.txt")
            .example("First 5 lines", "head -n 5 file.txt")
            .example("First 100 bytes", "head -c 100 file.txt")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get input: from file or stdin
        let input = match args.get_string("path", 0) {
            Some(path) => {
                let resolved = ctx.resolve_path(&path);
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
            None => ctx.take_stdin().unwrap_or_default(),
        };

        // Check for byte mode (-c)
        let bytes = args.get("bytes", usize::MAX).and_then(|v| match v {
            Value::Int(i) => Some(*i as usize),
            Value::String(s) => s.parse().ok(),
            _ => None,
        });

        if let Some(byte_count) = bytes {
            // Byte mode: output first N bytes
            let output: String = input.chars().take(byte_count).collect();
            return ExecResult::success(output);
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

        let output: Vec<&str> = input.lines().take(lines).collect();
        if output.is_empty() {
            ExecResult::success("")
        } else {
            ExecResult::success(format!("{}\n", output.join("\n")))
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
}
