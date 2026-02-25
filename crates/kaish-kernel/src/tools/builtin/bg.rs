//! bg — Resume a stopped job in the background.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::scheduler::JobId;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Bg tool: resume a stopped job in the background.
pub struct Bg;

#[async_trait]
impl Tool for Bg {
    fn name(&self) -> &str {
        "bg"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("bg", "Resume a stopped job in the background")
            .param(ParamSchema::optional(
                "job_id",
                "int",
                Value::Null,
                "Job ID to resume (defaults to most recently stopped job)",
            ))
            .example("Resume last stopped job", "bg")
            .example("Resume specific job", "bg 2")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        #[cfg(not(unix))]
        {
            let _ = (args, ctx);
            return ExecResult::failure(1, "bg: job control not supported on this platform");
        }

        #[cfg(unix)]
        {
            let manager = match &ctx.job_manager {
                Some(m) => m.clone(),
                None => return ExecResult::failure(1, "bg: no job manager"),
            };

            // Resolve job ID
            let job_id = if let Some(val) = args.get_positional(0) {
                match val {
                    Value::Int(i) => JobId(*i as u64),
                    Value::String(s) => match s.parse::<u64>() {
                        Ok(i) => JobId(i),
                        Err(_) => return ExecResult::failure(1, format!("bg: invalid job id: {}", s)),
                    },
                    _ => return ExecResult::failure(1, "bg: job id must be a number"),
                }
            } else {
                match manager.last_stopped().await {
                    Some(id) => id,
                    None => return ExecResult::failure(1, "bg: no stopped jobs"),
                }
            };

            // Get process info
            let (_pid_raw, pgid_raw) = match manager.get_process_info(job_id).await {
                Some(info) => info,
                None => return ExecResult::failure(1, format!("bg: job {} not found or not stopped", job_id)),
            };

            let cmd = manager.get_command(job_id).await.unwrap_or_default();
            let pgid = nix::unistd::Pid::from_raw(pgid_raw as i32);

            // Mark as resumed (no terminal transfer — it runs in background)
            manager.resume_job(job_id).await;

            // Send SIGCONT
            if let Err(e) = nix::sys::signal::killpg(pgid, nix::sys::signal::Signal::SIGCONT) {
                return ExecResult::failure(1, format!("bg: failed to continue job: {}", e));
            }

            // Spawn a background reaper task
            let jobs = manager.clone();
            let pid = nix::unistd::Pid::from_raw(_pid_raw as i32);
            tokio::spawn(async move {
                let result = tokio::task::block_in_place(|| {
                    // Wait without WUNTRACED — we don't care about stops in bg
                    loop {
                        match nix::sys::wait::waitpid(pid, None) {
                            Ok(nix::sys::wait::WaitStatus::Exited(_, _))
                            | Ok(nix::sys::wait::WaitStatus::Signaled(_, _, _)) => break,
                            Ok(_) => continue,
                            Err(nix::errno::Errno::EINTR) => continue,
                            Err(_) => break,
                        }
                    }
                });
                let _ = result;
                jobs.remove(job_id).await;
            });

            ExecResult::success(format!("[{}] {} &\n", job_id, cmd))
        }
    }
}
