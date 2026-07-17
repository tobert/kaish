//! cd — Change working directory.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::builtin::get_path_string;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Cd tool: change current working directory.
pub struct Cd;

/// clap-derived argv layer for cd.
#[derive(Parser, Debug)]
#[command(name = "cd", about = "Change current working directory")]
struct CdArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Directory to change to; defaults to `$HOME`, or `-` for the previous
    /// directory.
    path: Vec<String>,
}

#[async_trait]
impl Tool for Cd {
    fn name(&self) -> &str {
        "cd"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &CdArgs::command(),
            "cd",
            "Change current working directory",
            [
                ("Go home", "cd"),
                ("Change directory", "cd /tmp"),
                ("Previous directory", "cd -"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("cd: {e}")),
        };
        let parsed = match CdArgs::try_parse_from(
            std::iter::once("cd".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("cd: {e}")),
        };
        parsed.global.apply(ctx);

        // A binary `path` operand goes loud rather than silently falling
        // through to the "no operand" branch below (which would silently `cd`
        // to $HOME/`/` instead of erroring on the path the user actually gave).
        let path_arg = match get_path_string(&args, "path", 0) {
            Ok(Some(p)) => p,
            Ok(None) => {
                // Consult the session HOME from the kernel scope only — never the
                // host env (the kernel is hermetic). With no HOME in scope, bare
                // `cd` falls back to `/` rather than leaking the host home dir.
                ctx.scope
                    .get("HOME")
                    .and_then(|v| match v {
                        Value::String(s) => Some(s.clone()),
                        _ => None,
                    })
                    .unwrap_or_else(|| "/".to_string())
            }
            Err(e) => return ExecResult::failure(1, format!("cd: {e}")),
        };

        // Handle `cd -` for previous directory
        let resolved: PathBuf = if path_arg == "-" {
            match ctx.get_prev_cwd() {
                Some(prev) => prev.clone(),
                None => return ExecResult::failure(1, "cd: OLDPWD not set"),
            }
        } else {
            ctx.resolve_path(&path_arg)
        };

        // Verify the path exists and is a directory
        match ctx.backend.stat(Path::new(&resolved)).await {
            Ok(info) => {
                if info.is_dir() {
                    let new_cwd = resolved.clone();
                    ctx.set_cwd(new_cwd);
                    // For `cd -`, output the new directory (like bash)
                    if path_arg == "-" {
                        ExecResult::with_output(OutputData::text(resolved.to_string_lossy().to_string()))
                    } else {
                        ExecResult::success("")
                    }
                } else {
                    ExecResult::failure(1, format!("cd: {}: Not a directory", path_arg))
                }
            }
            Err(e) => ExecResult::failure(1, format!("cd: {}: {}", path_arg, e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::path::PathBuf;
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.mkdir(Path::new("subdir")).await.unwrap();
        mem.write(Path::new("file.txt"), b"data").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_cd_subdir() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/subdir".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.cwd, PathBuf::from("/subdir"));
    }

    #[tokio::test]
    async fn test_cd_root() {
        let mut ctx = make_ctx().await;
        ctx.set_cwd(PathBuf::from("/subdir"));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.cwd, PathBuf::from("/"));
    }

    #[tokio::test]
    async fn test_cd_file_fails() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("Not a directory"));
    }

    #[tokio::test]
    async fn test_cd_nonexistent() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_cd_dash_previous_dir() {
        let mut ctx = make_ctx().await;

        // First cd to subdir
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/subdir".into()));
        let result = Cd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.cwd, PathBuf::from("/subdir"));

        // Now cd - should go back to /
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-".into()));
        let result = Cd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.cwd, PathBuf::from("/"));
        // cd - prints the new directory
        assert_eq!(&*result.text_out(), "/");
    }

    #[tokio::test]
    async fn test_cd_dash_toggles() {
        let mut ctx = make_ctx().await;

        // cd to subdir
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/subdir".into()));
        Cd.execute(args, &mut ctx).await;
        assert_eq!(ctx.cwd, PathBuf::from("/subdir"));

        // cd - back to /
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-".into()));
        Cd.execute(args, &mut ctx).await;
        assert_eq!(ctx.cwd, PathBuf::from("/"));

        // cd - back to /subdir
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-".into()));
        Cd.execute(args, &mut ctx).await;
        assert_eq!(ctx.cwd, PathBuf::from("/subdir"));
    }

    #[tokio::test]
    async fn test_cd_dash_no_previous() {
        let mut ctx = make_ctx().await;
        // Without any previous cd, OLDPWD is not set
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("OLDPWD not set"));
    }

    #[tokio::test]
    async fn test_bare_cd_uses_scope_home() {
        let mut ctx = make_ctx().await;
        // Create /home dir in the VFS
        ctx.backend.mkdir(Path::new("/home")).await.unwrap();

        // Set HOME in scope to /home
        ctx.scope.set("HOME", Value::String("/home".into()));

        // Bare cd (no args) should go to scope HOME
        let args = ToolArgs::new();
        let result = Cd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.cwd, PathBuf::from("/home"));
    }
}
