//! kaish-status — Show kernel session status.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// kaish-status: display kernel name, variable count, and job count.
pub struct KaishStatus;

/// clap-derived argv layer for kaish-status.
#[derive(Parser, Debug)]
#[command(name = "kaish-status", about = "Show kernel session status")]
struct KaishStatusArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — to_argv() always emits `--` before positionals.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for KaishStatus {
    fn name(&self) -> &str {
        "kaish-status"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &KaishStatusArgs::command(),
            "kaish-status",
            "Show kernel session status",
            [("Check session status", "kaish-status")],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kaish-status: {e}")),
        };
        let parsed = match KaishStatusArgs::try_parse_from(
            std::iter::once("kaish-status".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kaish-status: {e}")),
        };
        parsed.global.apply(ctx);

        let var_count = ctx.scope.all_names().len();
        let job_count = match ctx.job_manager.as_ref() {
            Some(jm) => jm.list().await.len(),
            None => 0,
        };

        let headers = vec![
            "KEY".to_string(),
            "VALUE".to_string(),
        ];
        let nodes = vec![
            OutputNode::new("variables").with_cells(vec![var_count.to_string()]),
            OutputNode::new("jobs").with_cells(vec![job_count.to_string()]),
        ];

        ExecResult::with_output(OutputData::table(headers, nodes))
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
    async fn test_status_output() {
        let mut ctx = make_ctx();
        ctx.scope.set("X", Value::Int(1));

        let result = KaishStatus.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("variables"));
    }
}
