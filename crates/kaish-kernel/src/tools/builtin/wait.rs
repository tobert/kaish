//! wait — Wait for background jobs to complete.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::scheduler::JobId;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Wait tool: wait for background jobs.
pub struct Wait;

#[async_trait]
impl Tool for Wait {
    fn name(&self) -> &str {
        "wait"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("wait", "Wait for background jobs to complete")
            .param(ParamSchema::optional(
                "job_id",
                "int",
                Value::Null,
                "Specific job ID to wait for (waits for all if not specified)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let manager = match &ctx.job_manager {
            Some(m) => m.clone(),
            None => return ExecResult::success("(no job manager)\n"),
        };

        // Check if a specific job ID was provided
        if let Some(job_id) = args.get_positional(0) {
            let id = match job_id {
                Value::Int(i) => JobId(*i as u64),
                Value::String(s) => {
                    match s.parse::<u64>() {
                        Ok(i) => JobId(i),
                        Err(_) => return ExecResult::failure(1, format!("wait: invalid job id: {}", s)),
                    }
                }
                _ => return ExecResult::failure(1, "wait: job id must be a number"),
            };

            match manager.wait(id).await {
                Some(result) => {
                    let status = if result.ok() { "Done" } else { "Failed" };
                    ExecResult::success(format!("[{}] {}\n", id, status))
                }
                None => ExecResult::failure(1, format!("wait: job {} not found", id)),
            }
        } else {
            // Wait for all jobs
            let results = manager.wait_all().await;

            if results.is_empty() {
                return ExecResult::success("(no jobs to wait for)\n");
            }

            let mut output = String::new();
            let mut any_failed = false;

            for (id, result) in results {
                let status = if result.ok() {
                    "Done"
                } else {
                    any_failed = true;
                    "Failed"
                };
                output.push_str(&format!("[{}] {}\n", id, status));
            }

            if any_failed {
                ExecResult::from_output(1, output, "")
            } else {
                ExecResult::success(output)
            }
        }
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
    async fn test_wait_no_manager() {
        let mut ctx = make_ctx();
        let result = Wait.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("no job manager"));
    }

    #[tokio::test]
    async fn test_wait_no_jobs() {
        let mut ctx = make_ctx();
        ctx.set_job_manager(Arc::new(JobManager::new()));

        let result = Wait.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("no jobs"));
    }

    #[tokio::test]
    async fn test_wait_all() {
        let mut ctx = make_ctx();
        let manager = Arc::new(JobManager::new());
        ctx.set_job_manager(manager.clone());

        // Spawn jobs
        manager.spawn("job1".to_string(), async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            ExecResult::success("")
        });
        manager.spawn("job2".to_string(), async {
            tokio::time::sleep(Duration::from_millis(5)).await;
            ExecResult::success("")
        });

        // Wait for jobs to register
        tokio::time::sleep(Duration::from_millis(5)).await;

        let result = Wait.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("[1]"));
        assert!(result.out.contains("[2]"));
        assert!(result.out.contains("Done"));
    }

    #[tokio::test]
    async fn test_wait_specific_job() {
        let mut ctx = make_ctx();
        let manager = Arc::new(JobManager::new());
        ctx.set_job_manager(manager.clone());

        // Spawn a job
        let id = manager.spawn("test".to_string(), async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            ExecResult::success("")
        });

        // Wait for job to register
        tokio::time::sleep(Duration::from_millis(5)).await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(id.0 as i64));

        let result = Wait.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains(&format!("[{}]", id)));
    }

    #[tokio::test]
    async fn test_wait_nonexistent_job() {
        let mut ctx = make_ctx();
        ctx.set_job_manager(Arc::new(JobManager::new()));

        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(999));

        let result = Wait.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not found"));
    }

    #[tokio::test]
    async fn test_wait_failed_job() {
        let mut ctx = make_ctx();
        let manager = Arc::new(JobManager::new());
        ctx.set_job_manager(manager.clone());

        // Spawn a failing job
        manager.spawn("fail".to_string(), async {
            ExecResult::failure(1, "intentional failure")
        });

        // Wait for job to register
        tokio::time::sleep(Duration::from_millis(5)).await;

        let result = Wait.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok()); // Overall result fails if any job failed
        assert!(result.out.contains("Failed"));
    }
}
