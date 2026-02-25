//! kaish-status — Show kernel session status.

use async_trait::async_trait;

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema};

/// kaish-status: display kernel name, variable count, and job count.
pub struct KaishStatus;

#[async_trait]
impl Tool for KaishStatus {
    fn name(&self) -> &str {
        "kaish-status"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("kaish-status", "Show kernel session status")
            .example("Check session status", "kaish-status")
    }

    async fn execute(&self, _args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
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
        assert!(result.out.contains("variables"));
    }
}
