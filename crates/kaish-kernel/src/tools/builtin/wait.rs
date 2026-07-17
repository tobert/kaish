//! wait — Wait for background jobs to complete.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::{ExecResult, LatchRequest, OutputData};
use crate::scheduler::JobId;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Wait tool: wait for background jobs.
pub struct Wait;

/// clap-derived argv layer for wait.
#[derive(Parser, Debug)]
#[command(name = "wait", about = "Wait for background jobs to complete")]
struct WaitArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Job specifier (e.g. `%1`) or PID; waits for all background jobs when omitted.
    job: Vec<String>,
}

#[async_trait]
impl Tool for Wait {
    fn name(&self) -> &str {
        "wait"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &WaitArgs::command(),
            "wait",
            "Wait for background jobs to complete",
            [
                ("Wait for all jobs", "wait"),
                ("Wait for specific job", "wait 1"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("wait: {e}")),
        };
        let parsed = match WaitArgs::try_parse_from(
            std::iter::once("wait".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("wait: {e}")),
        };
        parsed.global.apply(ctx);

        let manager = match &ctx.job_manager {
            Some(m) => m.clone(),
            None => return ExecResult::with_output(OutputData::text("(no job manager)\n")),
        };

        // Wait for the specific job ids provided (`wait 1`, `wait %1 %2`).
        if !args.positional.is_empty() {
            let mut output = String::new();
            let mut any_failed = false;
            let mut latch: Option<LatchRequest> = None;

            for spec in &args.positional {
                let id = match spec {
                    Value::Int(i) => JobId(*i as u64),
                    Value::String(s) => {
                        // Accept the bash jobspec form `%N` as well as a bare
                        // number; the `%` is a job marker, not part of the id.
                        let digits = s.strip_prefix('%').unwrap_or(s);
                        match digits.parse::<u64>() {
                            Ok(i) => JobId(i),
                            Err(_) => return ExecResult::failure(1, format!("wait: invalid job id: {}", s)),
                        }
                    }
                    _ => return ExecResult::failure(1, "wait: job id must be a number"),
                };

                match manager.wait(id).await {
                    Some(result) => {
                        let status = classify(id, &result, &mut any_failed, &mut latch);
                        output.push_str(&format!("[{}] {}\n", id, status));
                    }
                    None => return ExecResult::failure(1, format!("wait: job {} not found", id)),
                }
            }

            finish(output, any_failed, latch)
        } else {
            // Wait for all jobs
            let results = manager.wait_all().await;

            if results.is_empty() {
                return ExecResult::with_output(OutputData::text("(no jobs to wait for)\n"));
            }

            let mut output = String::new();
            let mut any_failed = false;
            let mut latch: Option<LatchRequest> = None;

            for (id, result) in results {
                let status = classify(id, &result, &mut any_failed, &mut latch);
                output.push_str(&format!("[{}] {}\n", id, status));
            }

            finish(output, any_failed, latch)
        }
    }
}

/// Classify one waited job's result into a display word, threading the
/// aggregate `any_failed` flag and the first-seen backgrounded latch. A gated
/// job (`set -o latch`, exit 2 with a stored request) is `Latched`, *not*
/// `Failed` — the op is held, and the request must reach the caller so a
/// backgrounded gate is fulfillable (GH #96).
///
/// `wait` reads the job's raw cached `ExecResult` straight from
/// `JobManager::wait`/`wait_all`, bypassing `Job::latch()` (the chokepoint
/// that stamps `job_id` for `jobs`/`/v/jobs/{id}/latch`) — so this stamps it
/// here too (GH #124 part 4), or a latch surfaced via `wait` would carry no
/// back-reference for `Kernel::confirm` to retire the job with.
fn classify(
    id: JobId,
    result: &ExecResult,
    any_failed: &mut bool,
    latch: &mut Option<LatchRequest>,
) -> &'static str {
    if result.ok() {
        "Done"
    } else if let Some(mut lr) = result.latch_request() {
        lr.job_id = Some(id.0);
        // First latch wins if several jobs are gated — `.latch` holds one, and
        // an embedder waiting on multiple gated jobs is an unusual pattern.
        latch.get_or_insert(lr);
        "Latched"
    } else {
        *any_failed = true;
        "Failed"
    }
}

/// Assemble `wait`'s result: a surfaced backgrounded latch wins (exit 2 with
/// the request on the control-plane `.latch` field, mirroring a foreground
/// gate); otherwise any failure is exit 1; otherwise success.
fn finish(output: String, any_failed: bool, latch: Option<LatchRequest>) -> ExecResult {
    if let Some(lr) = latch {
        let mut result = ExecResult::from_output(2, output.clone(), "");
        result.set_output(Some(OutputData::text(output)));
        result.latch = Some(Box::new(lr));
        result
    } else if any_failed {
        let mut result = ExecResult::from_output(1, output.clone(), "");
        result.set_output(Some(OutputData::text(output)));
        result
    } else {
        ExecResult::with_output(OutputData::text(output))
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
        assert!(result.text_out().contains("no job manager"));
    }

    #[tokio::test]
    async fn test_wait_no_jobs() {
        let mut ctx = make_ctx();
        ctx.set_job_manager(Arc::new(JobManager::new()));

        let result = Wait.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("no jobs"));
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
        }).await;
        manager.spawn("job2".to_string(), async {
            tokio::time::sleep(Duration::from_millis(5)).await;
            ExecResult::success("")
        }).await;

        // Wait for jobs to register
        tokio::time::sleep(Duration::from_millis(5)).await;

        let result = Wait.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("[1]"));
        assert!(result.text_out().contains("[2]"));
        assert!(result.text_out().contains("Done"));
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
        }).await;

        // Wait for job to register
        tokio::time::sleep(Duration::from_millis(5)).await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(id.0 as i64));

        let result = Wait.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains(&format!("[{}]", id)));
    }

    #[tokio::test]
    async fn test_wait_jobspec_percent_form() {
        // `wait %1` — the `%` is a job marker, stripped before lookup.
        let mut ctx = make_ctx();
        let manager = Arc::new(JobManager::new());
        ctx.set_job_manager(manager.clone());

        let id = manager.spawn("test".to_string(), async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            ExecResult::success("")
        }).await;
        tokio::time::sleep(Duration::from_millis(5)).await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(format!("%{}", id.0)));

        let result = Wait.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains(&format!("[{}]", id)));
    }

    #[tokio::test]
    async fn test_wait_multiple_jobspecs() {
        // `wait %1 %2` waits for each named job, not just the first.
        let mut ctx = make_ctx();
        let manager = Arc::new(JobManager::new());
        ctx.set_job_manager(manager.clone());

        let id1 = manager.spawn("j1".to_string(), async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            ExecResult::success("")
        }).await;
        let id2 = manager.spawn("j2".to_string(), async {
            tokio::time::sleep(Duration::from_millis(5)).await;
            ExecResult::success("")
        }).await;
        tokio::time::sleep(Duration::from_millis(5)).await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String(format!("%{}", id1.0)));
        args.positional.push(Value::String(format!("%{}", id2.0)));

        let result = Wait.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains(&format!("[{}]", id1)));
        assert!(result.text_out().contains(&format!("[{}]", id2)));
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
        }).await;

        // Wait for job to register
        tokio::time::sleep(Duration::from_millis(5)).await;

        let result = Wait.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok()); // Overall result fails if any job failed
        assert!(result.text_out().contains("Failed"));
    }
}
