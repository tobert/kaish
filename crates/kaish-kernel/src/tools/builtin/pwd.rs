//! pwd — Print working directory.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Pwd tool: print current working directory.
pub struct Pwd;

/// clap-derived argv layer for pwd.
#[derive(Parser, Debug)]
#[command(name = "pwd", about = "Print current working directory")]
struct PwdArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — to_argv() always emits `--` before positionals.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for Pwd {
    fn name(&self) -> &str {
        "pwd"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &PwdArgs::command(),
            "pwd",
            "Print current working directory",
            [("Show current directory", "pwd")],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("pwd: {e}")),
        };
        let parsed = match PwdArgs::try_parse_from(
            std::iter::once("pwd".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("pwd: {e}")),
        };
        parsed.global.apply(ctx);

        ExecResult::with_output(OutputData::text(ctx.cwd.to_string_lossy().to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::path::PathBuf;
    use std::sync::Arc;

    #[tokio::test]
    async fn test_pwd_default() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = Pwd.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "/");
    }

    #[tokio::test]
    async fn test_pwd_changed() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_cwd(PathBuf::from("/mnt/project"));

        let result = Pwd.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "/mnt/project");
    }
}
