//! fg — Resume a stopped job in the foreground.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::scheduler::JobId;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Fg tool: resume a stopped job in the foreground.
pub struct Fg;

#[async_trait]
impl Tool for Fg {
    fn name(&self) -> &str {
        "fg"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("fg", "Resume a stopped job in the foreground")
            .param(ParamSchema::optional(
                "job_id",
                "int",
                Value::Null,
                "Job ID to resume (defaults to most recently stopped job)",
            ))
            .example("Resume last stopped job", "fg")
            .example("Resume specific job", "fg 2")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        #[cfg(not(unix))]
        {
            let _ = (args, ctx);
            return ExecResult::failure(1, "fg: job control not supported on this platform");
        }

        #[cfg(unix)]
        {
            let manager = match &ctx.job_manager {
                Some(m) => m.clone(),
                None => return ExecResult::failure(1, "fg: no job manager"),
            };

            let term = match &ctx.terminal_state {
                Some(t) => t.clone(),
                None => return ExecResult::failure(1, "fg: not an interactive terminal"),
            };

            // Resolve job ID
            let job_id = if let Some(val) = args.get_positional(0) {
                match val {
                    Value::Int(i) => JobId(*i as u64),
                    Value::String(s) => match s.parse::<u64>() {
                        Ok(i) => JobId(i),
                        Err(_) => return ExecResult::failure(1, format!("fg: invalid job id: {}", s)),
                    },
                    _ => return ExecResult::failure(1, "fg: job id must be a number"),
                }
            } else {
                match manager.last_stopped().await {
                    Some(id) => id,
                    None => return ExecResult::failure(1, "fg: no stopped jobs"),
                }
            };

            // Get process info
            let (pid_raw, pgid_raw) = match manager.get_process_info(job_id).await {
                Some(info) => info,
                None => return ExecResult::failure(1, format!("fg: job {} not found or not stopped", job_id)),
            };

            let cmd = manager.get_command(job_id).await.unwrap_or_default();
            let pid = nix::unistd::Pid::from_raw(pid_raw as i32);
            let pgid = nix::unistd::Pid::from_raw(pgid_raw as i32);

            // Give terminal to the job's process group
            if let Err(e) = term.give_terminal_to(pgid) {
                return ExecResult::failure(1, format!("fg: failed to give terminal: {}", e));
            }

            // Mark as resumed and send SIGCONT
            manager.resume_job(job_id).await;
            if let Err(e) = nix::sys::signal::killpg(pgid, nix::sys::signal::Signal::SIGCONT) {
                let _ = term.reclaim_terminal();
                return ExecResult::failure(1, format!("fg: failed to continue job: {}", e));
            }

            eprintln!("{}", cmd);

            // Wait for the process (may stop again)
            let jobs = manager.clone();
            let term_clone = term.clone();
            let code = tokio::task::block_in_place(move || {
                let result = term_clone.wait_for_foreground(pid);

                if let Err(e) = term_clone.reclaim_terminal() {
                    tracing::warn!("failed to reclaim terminal: {}", e);
                }

                match result {
                    crate::terminal::WaitResult::Exited(code) => {
                        // Job finished — remove it
                        let rt = tokio::runtime::Handle::current();
                        rt.block_on(jobs.remove(job_id));
                        code as i64
                    }
                    crate::terminal::WaitResult::Signaled(sig) => {
                        let rt = tokio::runtime::Handle::current();
                        rt.block_on(jobs.remove(job_id));
                        128 + sig as i64
                    }
                    crate::terminal::WaitResult::Stopped(_sig) => {
                        // Stopped again
                        let rt = tokio::runtime::Handle::current();
                        rt.block_on(jobs.stop_job(job_id, pid_raw, pgid_raw));
                        eprintln!("\n[{}]+ Stopped\t{}", job_id, cmd);
                        148
                    }
                }
            });

            ExecResult::from_output(code, String::new(), String::new())
        }
    }
}
