//! jobs — List and manage background jobs.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::scheduler::JobInfo;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Build `jobs --json` rows: the full serialized `JobInfo` (GH #241 — id,
/// status, command, exit_code, started_at/finished_at, pgids, ... —
/// whatever `JobInfo`'s own `Serialize` impl emits) plus one bolt-on `path`
/// field.
///
/// `path` (`/v/jobs/N/`) is deliberately NOT a `JobInfo` field: it names
/// *this VFS mount's* convention for reaching the job's observability nodes,
/// not an intrinsic property of the job itself, so the builtin adds it here
/// rather than the type baking in a `jobs`-specific presentation detail.
/// Before GH #241 this function hand-built every field with `serde_json::json!`
/// (including re-deriving `status` from `Display`)
/// because `JobInfo` couldn't serialize itself — now that it can, the only
/// thing left to bolt on is `path`.
fn job_rows_json(jobs: &[JobInfo]) -> Vec<serde_json::Value> {
    jobs.iter()
        .map(|job| {
            // JobInfo's fields are all plain scalars/strings/SystemTime —
            // serialization cannot fail in practice. Per CLAUDE.md, an
            // error that can never happen in practice may be hidden, but the
            // program must panic on the outside case rather than silently
            // dropping the row.
            #[allow(clippy::expect_used)]
            let mut row = serde_json::to_value(job)
                .expect("JobInfo serializes to plain JSON scalars/strings/timestamps — never fails");
            row["path"] = serde_json::Value::String(format!("/v/jobs/{}/", job.id));
            row
        })
        .collect()
}

/// Jobs tool: list and manage background jobs.
pub struct Jobs;

/// clap-derived argv layer for jobs.
#[derive(Parser, Debug)]
#[command(name = "jobs", about = "List and manage background jobs")]
struct JobsArgs {
    /// Remove completed jobs from tracking.
    #[arg(long = "cleanup")]
    cleanup: bool,

    #[command(flatten)]
    global: GlobalFlags,
}

#[async_trait]
impl Tool for Jobs {
    fn name(&self) -> &str {
        "jobs"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &JobsArgs::command(),
            "jobs",
            "List and manage background jobs",
            [
                ("List background jobs", "jobs"),
                ("Clean up completed jobs", "jobs --cleanup"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("jobs: {e}")),
        };
        let parsed = match JobsArgs::try_parse_from(
            std::iter::once("jobs".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("jobs: {e}")),
        };
        parsed.global.apply(ctx);

        let manager = match &ctx.job_manager {
            Some(m) => m,
            None => return ExecResult::with_output(OutputData::text("(no job manager)")),
        };

        if parsed.cleanup {
            let before = manager.list().await.len();
            manager.cleanup().await;
            let remaining = manager.list().await;
            let removed = before - remaining.len();
            let msg = format!("Cleaned up {} completed job(s)\n", removed);
            return ExecResult::with_output(OutputData::text(msg));
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
            "STATUS".to_string(),
            "COMMAND".to_string(),
            "PATH".to_string(),
        ];

        // Computed from `&jobs` before the text loop below consumes it by value.
        let rows = job_rows_json(&jobs);
        let output = OutputData::table(headers, nodes)
            .with_rich_json(serde_json::Value::Array(rows));
        let mut text = String::new();
        for job in jobs {
            text.push_str(&format!(
                "[{}] {} {}  /v/jobs/{}/\n",
                job.id, job.status, job.command, job.id
            ));
        }
        ExecResult::with_output_and_text(output, text)
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

    #[test]
    fn job_rows_json_carries_exit_code_on_failure() {
        // GH #243(a): the audit verified `jobs --json` for a job that exited
        // 42 reported only `{"status":"Failed"}` — the exit code was lost
        // entirely. Now it must ride along as JobInfo.exit_code.
        use crate::scheduler::{JobId, JobStatus};

        let jobs = vec![
            JobInfo::new(JobId(1), "/bin/sh -c 'exit 42'", JobStatus::Failed)
                .with_exit_code(Some(42)),
        ];

        let rows = job_rows_json(&jobs);
        assert_eq!(rows[0]["status"], "failed");
        assert_eq!(
            rows[0]["exit_code"], 42,
            "the exit code must be reachable from jobs --json, not just \"Failed\": {}",
            rows[0]
        );
    }

    #[tokio::test]
    async fn test_jobs_no_manager() {
        let mut ctx = make_ctx();
        let result = Jobs.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("no job manager"));
    }

    #[tokio::test]
    async fn test_jobs_empty() {
        let mut ctx = make_ctx();
        ctx.set_job_manager(Arc::new(JobManager::new()));

        let result = Jobs.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("no jobs"));
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
        }).await;

        // Wait for job to register
        tokio::time::sleep(Duration::from_millis(10)).await;

        let result = Jobs.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("[1]"));
        assert!(result.text_out().contains("test command"));
        assert!(result.text_out().contains("Running"));
        assert!(result.text_out().contains("/v/jobs/1/"));
    }

    #[tokio::test]
    async fn test_jobs_cleanup() {
        let mut ctx = make_ctx();
        let manager = Arc::new(JobManager::new());
        ctx.set_job_manager(manager.clone());

        // Spawn a quick job that will complete
        let id = manager.spawn("quick job".to_string(), async {
            ExecResult::success("")
        }).await;

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
        assert!(result.text_out().contains("Cleaned up 1 completed job"));

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
        }).await;

        // Wait for registration
        tokio::time::sleep(Duration::from_millis(10)).await;

        // Cleanup should not remove running job
        let mut args = ToolArgs::new();
        args.flags.insert("cleanup".to_string());
        let result = Jobs.execute(args, &mut ctx).await;

        assert!(result.ok());
        assert!(result.text_out().contains("Cleaned up 0 completed job"));
        assert_eq!(manager.list().await.len(), 1);
    }
}
