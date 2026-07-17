//! kaish-clear — Reset kernel session state.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData, Scope};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// kaish-clear: reset session state (variables, cwd).
pub struct KaishClear;

/// clap-derived argv layer for kaish-clear.
#[derive(Parser, Debug)]
#[command(name = "kaish-clear", about = "Clear session state (variables, cwd)")]
struct KaishClearArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — to_argv() always emits `--` before positionals.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for KaishClear {
    fn name(&self) -> &str {
        "kaish-clear"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &KaishClearArgs::command(),
            "kaish-clear",
            "Clear session state (variables, cwd)",
            [("Reset session", "kaish-clear")],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kaish-clear: {e}")),
        };
        let parsed = match KaishClearArgs::try_parse_from(
            std::iter::once("kaish-clear".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kaish-clear: {e}")),
        };
        parsed.global.apply(ctx);

        // Preserve $$ across the reset — the kernel hasn't restarted, just
        // the variables/cwd were cleared. A user comparing $$ before and
        // after kaish-clear would expect the same identifier.
        let pid = ctx.scope.pid();
        ctx.scope = Scope::new();
        ctx.scope.set_pid(pid);
        ctx.cwd = std::path::PathBuf::from("/");
        ExecResult::with_output(OutputData::text("Session reset (variables cleared)\n"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_clear_resets_scope() {
        let mut ctx = make_ctx();
        ctx.scope.set("FOO", Value::String("bar".into()));
        assert!(ctx.scope.get("FOO").is_some());

        let result = KaishClear.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.get("FOO").is_none());
    }
}
