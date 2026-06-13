//! tee — Read from stdin and write to both stdout and files.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::backend::WriteMode;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Tee tool: duplicate stdin to stdout and files.
pub struct Tee;

/// clap-derived argv layer for tee. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "tee", about = "Read from stdin and write to stdout and files")]
struct TeeArgs {
    /// Append to file instead of overwriting.
    #[arg(id = "append", short = 'a', long = "append")]
    _append: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to write to in addition to stdout.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Tee {
    fn name(&self) -> &str {
        "tee"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TeeArgs::command(),
            "tee",
            "Read from stdin and write to stdout and files",
            [
                ("Save and display", "echo hello | tee output.txt"),
                ("Append to log", "echo entry | tee -a log.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match TeeArgs::try_parse_from(
            std::iter::once("tee".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("tee: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(1, "tee: missing file argument");
        }

        let append = args.has_flag("append") || args.has_flag("a");
        // Read raw bytes so binary passes through tee intact (to files and to
        // the next stage).
        let input = ctx.read_stdin_to_bytes().await.unwrap_or_default();

        // POSIX: tee writes stdin to every file AND to stdout. Continue past
        // per-file errors (matches POSIX `tee` semantics) and report the last
        // failure as a non-zero exit so callers can detect partial failure.
        let mut last_err: Option<String> = None;
        for value in &args.positional {
            let path_str = crate::interpreter::value_to_string(value);
            let resolved = ctx.resolve_path(&path_str);
            let path = Path::new(&resolved);

            let final_content = if append {
                // kaish VFS lacks an append mode — read-modify-write.
                match ctx.backend.read(path, None).await {
                    Ok(existing) => {
                        let mut combined = existing;
                        combined.extend_from_slice(&input);
                        combined
                    }
                    Err(_) => input.clone(),
                }
            } else {
                input.clone()
            };

            if let Err(e) = ctx
                .backend
                .write(path, &final_content, WriteMode::Overwrite)
                .await
            {
                last_err = Some(format!("tee: {}: {}", path_str, e));
            }
        }

        // Pass the input through unchanged: text for text input, binary for
        // binary input.
        let mut result = ExecResult::success_text_or_bytes(input);
        if let Some(msg) = last_err {
            result.err = msg;
            result = result.with_code(1);
        }
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("existing.txt"), b"original content\n")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_tee_new_file() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello world\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/output.txt".into()));

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hello world\n");

        let written = ctx
            .backend
            .read(Path::new("/output.txt"), None)
            .await
            .unwrap();
        assert_eq!(written, b"hello world\n");
    }

    #[tokio::test]
    async fn test_tee_overwrite() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("new content\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());

        let written = ctx
            .backend
            .read(Path::new("/existing.txt"), None)
            .await
            .unwrap();
        assert_eq!(written, b"new content\n");
    }

    #[tokio::test]
    async fn test_tee_append() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("appended\n".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));
        args.flags.insert("a".to_string());

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());

        let written = ctx
            .backend
            .read(Path::new("/existing.txt"), None)
            .await
            .unwrap();
        assert_eq!(written, b"original content\nappended\n");
    }

    #[tokio::test]
    async fn test_tee_empty_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/empty.txt".into()));

        let result = Tee.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "");

        let written = ctx
            .backend
            .read(Path::new("/empty.txt"), None)
            .await
            .unwrap();
        assert!(written.is_empty());
    }

    #[tokio::test]
    async fn test_tee_missing_file() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("data\n".to_string());

        let result = Tee.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
