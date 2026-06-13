//! cat — Read and output file contents.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

#[cfg(test)]
use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Cat tool: read and output file contents.
pub struct Cat;

/// clap-derived argv layer for cat. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "cat", about = "Read and output file contents")]
struct CatArgs {
    /// Number output lines.
    #[arg(short = 'n', long = "number")]
    number: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to read. Reads stdin when no files are given.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Cat {
    fn name(&self) -> &str {
        "cat"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &CatArgs::command(),
            "cat",
            "Read and output file contents",
            [
                ("Read a file", "cat README.md"),
                ("Show line numbers", "cat -n src/main.rs"),
                ("Concatenate files", "cat header.txt body.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match CatArgs::try_parse_from(
            std::iter::once("cat".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("cat: {e}")),
        };
        parsed.global.apply(ctx);
        let number_lines = parsed.number;

        // If no files specified, read from stdin (like POSIX cat)
        if args.positional.is_empty() {
            // Streaming path: pipe_stdin → pipe_stdout without buffering
            if !number_lines && ctx.pipe_stdin.is_some() && ctx.pipe_stdout.is_some() {
                if let (Some(mut pipe_in), Some(mut pipe_out)) =
                    (ctx.pipe_stdin.take(), ctx.pipe_stdout.take())
                {
                    use tokio::io::{AsyncReadExt, AsyncWriteExt};
                    let mut buf = [0u8; 8192];
                    loop {
                        match pipe_in.read(&mut buf).await {
                            Ok(0) => break,
                            Ok(n) => {
                                if pipe_out.write_all(&buf[..n]).await.is_err() {
                                    break; // broken pipe
                                }
                            }
                            Err(_) => break,
                        }
                    }
                    let _ = pipe_out.shutdown().await;
                    return ExecResult::success("");
                }
            }

            // Buffered path (e.g. `cat` as a pipeline's last stage). With `-n`
            // we need text for line numbering, so binary is a loud error;
            // without it, stay byte-clean so piped binary survives intact
            // (`dd if=/dev/urandom … | cat` → a Bytes result, not a lossy mangle).
            if number_lines {
                let stdin = match ctx.read_stdin_to_text().await {
                    Ok(s) => s.unwrap_or_default(),
                    Err(e) => return ExecResult::failure(2, format!("cat: {e}")),
                };
                if stdin.is_empty() {
                    return ExecResult::with_output(OutputData::text(stdin));
                }
                let numbered = stdin
                    .lines()
                    .enumerate()
                    .map(|(i, line)| format!("{:6}\t{}", i + 1, line))
                    .collect::<Vec<_>>()
                    .join("\n");
                return ExecResult::with_output(OutputData::text(numbered));
            }
            let stdin = ctx.read_stdin_to_bytes().await.unwrap_or_default();
            return ExecResult::success_text_or_bytes(stdin);
        }
        // Collect paths, expanding any glob patterns
        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("cat: {}", e)),
        };

        if paths.is_empty() {
            return ExecResult::failure(1, "cat: missing path argument");
        }

        // Binary-capable fast path: a single file with no line numbering. Valid
        // UTF-8 is text as before; anything else becomes a Bytes result (a hex
        // dump in the REPL, a base64 envelope under --json) instead of the old
        // "invalid UTF-8" error. Multi-file / -n stay text-only below — you
        // can't line-number or text-concat binary. See docs/binary-data.md.
        if paths.len() == 1 && !number_lines {
            let resolved = ctx.resolve_path(&paths[0]);
            return match ctx.backend.read(Path::new(&resolved), None).await {
                Ok(data) => ExecResult::success_text_or_bytes(data),
                Err(e) => ExecResult::failure(1, format!("cat: {}: {}", paths[0], e)),
            };
        }

        let mut all_content = String::new();
        let mut line_num = 1;

        for (i, path) in paths.iter().enumerate() {
            let resolved = ctx.resolve_path(path);

            match ctx.backend.read(Path::new(&resolved), None).await {
                Ok(data) => match String::from_utf8(data) {
                    Ok(content) => {
                        if number_lines {
                            for line in content.lines() {
                                if !all_content.is_empty() {
                                    all_content.push('\n');
                                }
                                all_content.push_str(&format!("{:6}\t{}", line_num, line));
                                line_num += 1;
                            }
                        } else {
                            if i > 0 && !all_content.is_empty() {
                                all_content.push('\n');
                            }
                            all_content.push_str(&content);
                        }
                    }
                    Err(_) => return ExecResult::failure(1, format!("cat: {}: invalid UTF-8", path)),
                },
                Err(e) => return ExecResult::failure(1, format!("cat: {}: {}", path, e)),
            }
        }

        ExecResult::with_output(OutputData::text(all_content))
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
        mem.write(Path::new("test.txt"), b"hello world").await.unwrap();
        mem.write(Path::new("dir/nested.txt"), b"nested content").await.unwrap();
        mem.write(Path::new("lines.txt"), b"line1\nline2\nline3").await.unwrap();
        mem.write(Path::new("other.txt"), b"other content").await.unwrap();
        // Non-UTF-8 bytes (0xFF 0xFE … is invalid UTF-8).
        mem.write(Path::new("blob.bin"), &[0u8, 0xff, 0xfe, 0x41, 0x80]).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_cat_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hello world");
    }

    #[tokio::test]
    async fn test_cat_nested() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dir/nested.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "nested content");
    }

    #[tokio::test]
    async fn test_cat_binary_file_yields_bytes() {
        // A single non-UTF-8 file becomes a Bytes result, not an error.
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/blob.bin".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok(), "stderr: {}", result.err);
        assert!(result.is_bytes(), "binary file should produce a Bytes result");
        assert_eq!(result.out_bytes(), Some(&[0u8, 0xff, 0xfe, 0x41, 0x80][..]));
    }

    #[tokio::test]
    async fn test_cat_binary_file_json_envelope() {
        // Under --json, a binary result serializes as the base64 envelope.
        use crate::interpreter::{apply_output_format, OutputFormat};
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/blob.bin".into()));

        let result = Cat.execute(args, &mut ctx).await;
        let formatted = apply_output_format(result, OutputFormat::Json);
        let json: serde_json::Value = serde_json::from_str(&formatted.text_out()).unwrap();
        assert_eq!(json["_type"], "bytes");
        assert_eq!(json["len"], 5);
    }

    #[tokio::test]
    async fn test_cat_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not found") || result.err.contains("nonexistent"));
    }

    #[tokio::test]
    async fn test_cat_no_arg_no_stdin() {
        // POSIX: cat with no args and no stdin exits 0 with empty output
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_cat_from_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello from stdin".to_string());
        let args = ToolArgs::new();

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hello from stdin");
    }

    #[tokio::test]
    async fn test_cat_from_stdin_with_line_numbers() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("line1\nline2\nline3".to_string());
        let mut args = ToolArgs::new();
        args.flags.insert("n".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("1\tline1"));
        assert!(result.text_out().contains("2\tline2"));
        assert!(result.text_out().contains("3\tline3"));
    }

    #[tokio::test]
    async fn test_cat_multiple_files() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/test.txt".into()));
        args.positional.push(Value::String("/other.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("hello world"));
        assert!(result.text_out().contains("other content"));
    }

    #[tokio::test]
    async fn test_cat_n_line_numbers() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("n".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("1\tline1"));
        assert!(result.text_out().contains("2\tline2"));
        assert!(result.text_out().contains("3\tline3"));
    }

    #[tokio::test]
    async fn test_cat_number_line_numbers() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.flags.insert("number".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("1\tline1"));
    }

    #[tokio::test]
    async fn test_cat_n_multiple_files_continuous_numbering() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));
        args.positional.push(Value::String("/test.txt".into()));
        args.flags.insert("n".to_string());

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        // lines.txt has 3 lines, so test.txt should start at line 4
        assert!(result.text_out().contains("4\thello world"));
    }

    #[tokio::test]
    async fn test_cat_glob() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.txt".into()));

        let result = Cat.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should include content from test.txt, lines.txt, other.txt
        assert!(result.text_out().contains("hello world"));
        assert!(result.text_out().contains("other content"));
    }

    #[tokio::test]
    async fn test_cat_glob_no_matches() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("*.nonexistent".into()));

        let result = Cat.execute(args, &mut ctx).await;
        // No matches → missing path
        assert!(!result.ok());
    }
}
