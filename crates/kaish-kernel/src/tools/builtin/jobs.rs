//! jobs — List and manage background jobs.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Jobs tool: list and manage background jobs.
pub struct Jobs;

#[async_trait]
impl Tool for Jobs {
    fn name(&self) -> &str {
        "jobs"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("jobs", "List and manage background jobs")
            .param(ParamSchema::optional(
                "cleanup",
                "bool",
                Value::Bool(false),
                "Remove completed jobs from tracking (--cleanup)",
            ))
            .example("List background jobs", "jobs")
            .example("Clean up completed jobs", "jobs --cleanup")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let manager = match &ctx.job_manager {
            Some(m) => m,
            None => return ExecResult::with_output(OutputData::text("(no job manager)")),
        };

        // Handle --cleanup flag
        if args.has_flag("cleanup") {
            let before = manager.list().await.len();
            manager.cleanup().await;
            let after = manager.list().await.len();
            let removed = before - after;
            return ExecResult::with_output(OutputData::text(format!("Cleaned up {} completed job(s)\n", removed)));
        }

        let jobs = manager.list().await;

        if jobs.is_empty() {
            return ExecResult::with_output(OutputData::text("(no jobs)\n"));
        }

        let nodes: Vec<OutputNode> = jobs.iter().map(|job| {
            OutputNode::new(job.id.to_string())
                .with_cells(vec![
                    job.status.to_string(),
                    job.command.clone(),
                    format!("/v/jobs/{}/", job.id),
                ])
        }).collect();

        let headers = vec![
            "ID".to_string(),
            "Status".to_string(),
            "Command".to_string(),
            "Path".to_string(),
        ];

        let output = OutputData::table(headers, nodes);
        let mut result = ExecResult::with_output(output);
        // Override canonical output with traditional jobs format
        let mut text = String::new();
        for job in jobs {
            text.push_str(&format!(
                "[{}] {} {}  /v/jobs/{}/\n",
                job.id, job.status, job.command, job.id
            ));
        }
        result.out = text;
        result
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
        assert!(result.out.contains("/v/jobs/1/"));
    }

    #[tokio::test]
    async fn test_jobs_cleanup() {
        let mut ctx = make_ctx();
        let manager = Arc::new(JobManager::new());
        ctx.set_job_manager(manager.clone());

        // Spawn a quick job that will complete
        let id = manager.spawn("quick job".to_string(), async {
            ExecResult::success("")
        });

        // Wait for it to complete
        tokio::time::sleep(Duration::from_millis(10)).await;
        let _ = manager.wait(id).await;

        // Should have 1 completed job
        assert_eq!(manager.list().await.len(), 1);

        // Cleanup
        let mut args = ToolArgs::new();
        args.flags.insert("cleanup".to_string());
        let result = Jobs.execute(args, &mut ctx).await;

        assert!(result.ok());
        assert!(result.out.contains("Cleaned up 1 completed job"));

        // Should have no jobs now
        assert_eq!(manager.list().await.len(), 0);
    }

    #[tokio::test]
    async fn test_jobs_cleanup_preserves_running() {
        let mut ctx = make_ctx();
        let manager = Arc::new(JobManager::new());
        ctx.set_job_manager(manager.clone());

        // Spawn a long-running job
        manager.spawn("long job".to_string(), async {
            tokio::time::sleep(Duration::from_secs(10)).await;
            ExecResult::success("")
        });

        // Wait for registration
        tokio::time::sleep(Duration::from_millis(10)).await;

        // Cleanup should not remove running job
        let mut args = ToolArgs::new();
        args.flags.insert("cleanup".to_string());
        let result = Jobs.execute(args, &mut ctx).await;

        assert!(result.ok());
        assert!(result.out.contains("Cleaned up 0 completed job"));
        assert_eq!(manager.list().await.len(), 1);
    }
}
