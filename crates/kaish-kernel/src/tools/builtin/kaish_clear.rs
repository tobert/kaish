//! kaish-clear — Reset kernel session state.

use async_trait::async_trait;

use crate::interpreter::{ExecResult, OutputData, Scope};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema};

/// kaish-clear: reset session state (variables, cwd).
pub struct KaishClear;

#[async_trait]
impl Tool for KaishClear {
    fn name(&self) -> &str {
        "kaish-clear"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("kaish-clear", "Clear session state (variables, cwd)")
            .example("Reset session", "kaish-clear")
    }

    async fn execute(&self, _args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        ctx.scope = Scope::new();
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
