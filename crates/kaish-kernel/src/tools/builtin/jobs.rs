//! jobs — List and manage background jobs.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::scheduler::JobInfo;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Build `jobs --json` rows: `{"id", "status", "command", "path", "latch"?}`.
/// `latch` (nonce/paths/hint/ttl) is present only for a `Latched` row — a
/// caller can act on a gated job straight from `jobs --json` instead of a
/// second `/v/jobs/N/latch` read (GH #124 part 2). A pure function so the
/// row shape is unit-testable without a `JobManager`/kernel round trip.
fn job_rows_json(jobs: &[JobInfo]) -> Vec<serde_json::Value> {
    jobs.iter()
        .map(|job| {
            let mut row = serde_json::json!({
                "id": job.id.0,
                "status": job.status.to_string(),
                "command": job.command,
                "path": format!("/v/jobs/{}/", job.id),
            });
            // Infallible: LatchRequest is String/Vec<String>/u64 fields only.
            if let Some(latch) = &job.latch
                && let Ok(v) = serde_json::to_value(latch)
            {
                row["latch"] = v;
            }
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
        let parsed = match JobsArgs::try_parse_from(
            std::iter::once("jobs".to_string()).chain(args.to_argv()),
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
            let latched = remaining.iter().filter(|j| j.latch.is_some()).count();
            let mut msg = format!("Cleaned up {} completed job(s)\n", removed);
            if latched > 0 {
                msg.push_str(&format!(
                    "Kept {latched} latched job(s) awaiting confirmation — \
                     confirm via the nonce or abandon with kill --discard %N\n"
                ));
            }
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

        // rich_json rows carry `latch` for a `Latched` row (GH #124 part 2) —
        // computed from `&jobs` before the text loop below consumes it by value.
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
    fn job_rows_json_carries_latch_only_on_latched_rows() {
        use crate::interpreter::LatchRequest;
        use crate::scheduler::{JobId, JobStatus};

        let latch = LatchRequest {
            nonce: "a3f7b2c1".to_string(),
            command: "rm".to_string(),
            paths: vec!["precious.txt".to_string()],
            hint: "rm --confirm=\"a3f7b2c1\" precious.txt".to_string(),
            tool: "rm".to_string(),
            argv: vec!["precious.txt".to_string()],
            ttl: 60,
            job_id: Some(1),
        };
        let jobs = vec![
            JobInfo::new(JobId(1), "rm precious.txt", JobStatus::Latched).with_latch(Some(latch)),
            JobInfo::new(JobId(2), "sleep 5", JobStatus::Running),
        ];

        let rows = job_rows_json(&jobs);
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0]["id"], 1);
        assert_eq!(rows[0]["status"], "Latched");
        assert_eq!(rows[0]["path"], "/v/jobs/1/");
        assert_eq!(
            rows[0]["latch"]["nonce"], "a3f7b2c1",
            "a latched row must carry the nonce: {}",
            rows[0]
        );
        assert_eq!(
            rows[0]["latch"]["job_id"], 1,
            "the row's latch must carry the job_id back-reference (GH #124 part 4): {}",
            rows[0]
        );
        assert!(
            rows[1].get("latch").is_none(),
            "a non-latched row must NOT carry a latch key: {}",
            rows[1]
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
