//! true/false — Boolean exit code builtins.
//!
//! These builtins provide shell-compatible boolean semantics:
//! - `true` exits with code 0 (success/truthy)
//! - `false` exits with code 1 (failure/falsy)
//!
//! Used in conditions like `if true; then` or `while false; do`.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// True builtin: always succeeds (exit code 0).
pub struct True;

/// clap-derived argv layer for true.
#[derive(Parser, Debug)]
#[command(name = "true", about = "Exit with success (code 0)")]
struct TrueArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — to_argv() always emits `--` before positionals. POSIX true
    /// ignores any arguments; clap parses them but we never read them.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for True {
    fn name(&self) -> &str {
        "true"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TrueArgs::command(),
            "true",
            "Exit with success (code 0)",
            [("Always succeeds", "true")],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("true: {e}")),
        };
        let parsed = match TrueArgs::try_parse_from(
            std::iter::once("true".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("true: {e}")),
        };
        parsed.global.apply(ctx);

        ExecResult::success("")
    }
}

/// False builtin: always fails (exit code 1).
pub struct False;

/// clap-derived argv layer for false.
#[derive(Parser, Debug)]
#[command(name = "false", about = "Exit with failure (code 1)")]
struct FalseArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — to_argv() always emits `--` before positionals. POSIX false
    /// ignores any arguments; clap parses them but we never read them.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for False {
    fn name(&self) -> &str {
        "false"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &FalseArgs::command(),
            "false",
            "Exit with failure (code 1)",
            [("Always fails", "false")],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("false: {e}")),
        };
        let parsed = match FalseArgs::try_parse_from(
            std::iter::once("false".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("false: {e}")),
        };
        parsed.global.apply(ctx);

        ExecResult::failure(1, "")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    #[tokio::test]
    async fn true_returns_success() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = True.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.code, 0);
    }

    #[tokio::test]
    async fn true_with_args_still_succeeds() {
        // Like real shells, true ignores any arguments
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ignored".into()));

        let result = True.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.code, 0);
    }

    #[tokio::test]
    async fn false_returns_failure() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = False.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 1);
    }

    #[tokio::test]
    async fn false_with_args_still_fails() {
        // Like real shells, false ignores any arguments
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ignored".into()));

        let result = False.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 1);
    }
}
