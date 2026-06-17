//! tac — Reverse lines of files or stdin.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Tac tool: output lines in reverse order.
pub struct Tac;

/// clap-derived argv layer for tac. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "tac", about = "Reverse lines of files or stdin")]
struct TacArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Files to read in reverse; reads stdin when none are given.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Tac {
    fn name(&self) -> &str {
        "tac"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TacArgs::command(),
            "tac",
            "Reverse lines of files or stdin",
            [
                ("Reverse a file", "tac log.txt"),
                ("Reverse stdin", "seq 1 5 | tac"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match TacArgs::try_parse_from(
            std::iter::once("tac".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("tac: {e}")),
        };
        parsed.global.apply(ctx);

        // Collect file paths, expanding globs
        let paths = match ctx.expand_paths(&args.positional).await {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(1, format!("tac: {}", e)),
        };

        // Multiple files: reverse each in order (like GNU tac)
        if paths.len() > 1 {
            let mut output = String::new();
            for path in &paths {
                let resolved = ctx.resolve_path(path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => {
                            let mut lines: Vec<&str> = s.lines().collect();
                            lines.reverse();
                            if !output.is_empty() {
                                output.push('\n');
                            }
                            output.push_str(&lines.join("\n"));
                        }
                        Err(_) => {
                            return ExecResult::failure(
                                1,
                                format!("tac: {}: invalid UTF-8", path),
                            )
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("tac: {}: {}", path, e)),
                }
            }
            // Trailing-newline policy (builtin-sweep P4.1).
            if !output.is_empty() {
                output.push('\n');
            }
            return ExecResult::with_output(OutputData::text(output));
        }

        // Single file or stdin
        let input = match paths.first() {
            Some(path) => {
                let resolved = ctx.resolve_path(path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            return ExecResult::failure(
                                1,
                                format!("tac: {}: invalid UTF-8", path),
                            )
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("tac: {}: {}", path, e)),
                }
            }
            None => match ctx.read_stdin_to_text().await {
                Ok(s) => s.unwrap_or_default(),
                Err(e) => return ExecResult::failure(2, format!("tac: {e}")),
            },
        };

        let mut lines: Vec<&str> = input.lines().collect();
        lines.reverse();

        // Trailing-newline policy (builtin-sweep P4.1); empty input stays empty.
        let text = if lines.is_empty() {
            String::new()
        } else {
            format!("{}\n", lines.join("\n"))
        };
        ExecResult::with_output(OutputData::text(text))
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
        mem.write(Path::new("lines.txt"), b"one\ntwo\nthree\nfour\nfive")
            .await
            .expect("write failed");
        mem.write(Path::new("single.txt"), b"only")
            .await
            .expect("write failed");
        mem.write(Path::new("a.txt"), b"a1\na2")
            .await
            .expect("write failed");
        mem.write(Path::new("b.txt"), b"b1\nb2")
            .await
            .expect("write failed");
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_tac_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/lines.txt".into()));

        let result = Tac.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "five\nfour\nthree\ntwo\none\n");
    }

    #[tokio::test]
    async fn test_tac_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("alpha\nbeta\ngamma".to_string());

        let args = ToolArgs::new();
        let result = Tac.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "gamma\nbeta\nalpha\n");
    }

    #[tokio::test]
    async fn test_tac_single_line() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/single.txt".into()));

        let result = Tac.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "only\n");
    }

    #[tokio::test]
    async fn test_tac_empty_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("".to_string());

        let args = ToolArgs::new();
        let result = Tac.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "");
    }

    #[tokio::test]
    async fn test_tac_multiple_files() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/a.txt".into()));
        args.positional.push(Value::String("/b.txt".into()));

        let result = Tac.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().as_ref(), "a2\na1\nb2\nb1\n");
    }

    #[tokio::test]
    async fn test_tac_file_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nope.txt".into()));

        let result = Tac.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }
}
