//! jobs — List background jobs.

use async_trait::async_trait;

use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema};

/// Jobs tool: list background jobs.
pub struct Jobs;

#[async_trait]
impl Tool for Jobs {
    fn name(&self) -> &str {
        "jobs"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("jobs", "List background jobs")
    }

    async fn execute(&self, _args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let manager = match &ctx.job_manager {
            Some(m) => m,
            None => return ExecResult::success("(no job manager)"),
        };

        let jobs = manager.list().await;

        if jobs.is_empty() {
            return ExecResult::success("(no jobs)\n");
        }

        let mut output = String::new();
        for job in jobs {
            output.push_str(&format!(
                "[{}] {} {}\n",
                job.id, job.status, job.command
            ));
        }

        ExecResult::success(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scheduler::JobManager;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;
    use std::time::Duration;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_jobs_no_manager() {
        let mut ctx = make_ctx();
        let result = Jobs.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("no job manager"));
    }

    #[tokio::test]
    async fn test_jobs_empty() {
        let mut ctx = make_ctx();
        ctx.set_job_manager(Arc::new(JobManager::new()));

        let result = Jobs.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("no jobs"));
    }

    #[tokio::test]
    async fn test_jobs_with_running() {
        let mut ctx = make_ctx();
        let manager = Arc::new(JobManager::new());
        ctx.set_job_manager(manager.clone());

        // Spawn a job
        manager.spawn("test command".to_string(), async {
            tokio::time::sleep(Duration::from_millis(100)).await;
            ExecResult::success("")
        });

        // Wait for job to register
        tokio::time::sleep(Duration::from_millis(10)).await;

        let result = Jobs.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("[1]"));
        assert!(result.out.contains("test command"));
        assert!(result.out.contains("Running"));
    }
}
