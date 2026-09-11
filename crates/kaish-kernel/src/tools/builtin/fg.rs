//! fg — Resume a stopped job in the foreground.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

#[cfg(unix)]
use crate::ast::Value;
use crate::interpreter::ExecResult;
#[cfg(unix)]
use crate::scheduler::JobId;
use crate::tools::{exec_context, schema_from_clap, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Fg tool: resume a stopped job in the foreground.
pub struct Fg;

/// Give a stopped job the foreground and confirm it actually resumed,
/// end to end — decoupled from the concrete terminal/signal syscalls so
/// the *ordering* can be unit-tested without a real controlling terminal
/// (`TerminalState::init()` needs one and has no test-friendly
/// constructor; GH #161).
///
/// Contract: `give_terminal` runs first (standard job-control practice —
/// the process group must already be the terminal's foreground group
/// before it's continued, or it risks a `SIGTTIN`/`SIGTTOU` the instant it
/// touches the terminal). `send_sigcont` runs next; `mark_running` — which
/// tells the job manager the job is `Running` — only runs if `send_sigcont`
/// succeeded. If either step fails, any terminal handoff already done is
/// undone via `reclaim_terminal` before returning the error. This is the
/// same class of fix as `bg.rs`'s #160 (never mark a job `Running` before
/// its `SIGCONT` is confirmed), extended to `fg`'s extra terminal-ownership
/// surface.
#[cfg(unix)]
async fn resume_stopped_job_for_foreground<GiveTerm, SendCont, Reclaim, MarkRunning, Fut>(
    give_terminal: GiveTerm,
    send_sigcont: SendCont,
    reclaim_terminal: Reclaim,
    mark_running: MarkRunning,
) -> Result<(), String>
where
    GiveTerm: FnOnce() -> nix::Result<()>,
    SendCont: FnOnce() -> nix::Result<()>,
    Reclaim: FnOnce() -> nix::Result<()>,
    MarkRunning: FnOnce() -> Fut,
    Fut: std::future::Future<Output = ()>,
{
    if let Err(e) = give_terminal() {
        return Err(format!("fg: failed to give terminal: {}", e));
    }

    if let Err(e) = send_sigcont() {
        if let Err(e2) = reclaim_terminal() {
            tracing::warn!("fg: failed to reclaim terminal after failed SIGCONT: {}", e2);
        }
        return Err(format!("fg: failed to continue job: {}", e));
    }

    mark_running().await;
    Ok(())
}

/// clap-derived argv layer for fg.
#[derive(Parser, Debug)]
#[command(name = "fg", about = "Resume a stopped job in the foreground")]
struct FgArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Job id, with or without the `%` prefix (`%1` or `1`). Defaults to the
    /// highest-numbered stopped job.
    job: Vec<String>,
}

#[async_trait]
impl Tool for Fg {
    fn name(&self) -> &str {
        "fg"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &FgArgs::command(),
            "fg",
            "Resume a stopped job in the foreground",
            [
                ("Resume last stopped job", "fg"),
                ("Resume specific job", "fg 2"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("fg: {e}")),
        };
        let parsed = match FgArgs::try_parse_from(
            std::iter::once("fg".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("fg: {e}")),
        };
        parsed.global.apply(ctx);

        #[cfg(not(unix))]
        {
            let _ = args;
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
                    Value::String(s) => {
                        // Accept the bash jobspec form `%N` as well as a bare
                        // number; the `%` is a job marker, not part of the id
                        // (mirrors `kill`/`wait`).
                        let digits = s.strip_prefix('%').unwrap_or(s);
                        match digits.parse::<u64>() {
                            Ok(i) => JobId(i),
                            Err(_) => return ExecResult::failure(1, format!("fg: invalid job id: {}", s)),
                        }
                    }
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

            // Give the job the terminal, confirm SIGCONT, and only then mark
            // it Running — see `resume_stopped_job_for_foreground` for why
            // the ordering and the rollback-on-failure matter (GH #161).
            let term_for_signal = term.clone();
            let manager_for_resume = manager.clone();
            let resumed = resume_stopped_job_for_foreground(
                || term.give_terminal_to(pgid),
                || nix::sys::signal::killpg(pgid, nix::sys::signal::Signal::SIGCONT),
                || term_for_signal.reclaim_terminal(),
                || async move { manager_for_resume.resume_job(job_id).await },
            )
            .await;
            if let Err(msg) = resumed {
                return ExecResult::failure(1, msg);
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

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::sync::{Arc, Mutex};

    /// Records the order in which the injected steps actually ran, so the
    /// ordering contract itself — not just the final `Result` — is pinned.
    fn recorder() -> Arc<Mutex<Vec<&'static str>>> {
        Arc::new(Mutex::new(Vec::new()))
    }

    /// GH #161: a failed `SIGCONT` must never let the job manager be told
    /// the job is `Running`, and the terminal handoff that already
    /// happened must be undone. Pre-fix, `mark_running` (the analogue of
    /// `manager.resume_job`) ran unconditionally before the `SIGCONT`
    /// result was even checked; this pins the corrected order.
    #[tokio::test]
    async fn test_failed_sigcont_skips_mark_running_and_reclaims_terminal() {
        let calls = recorder();
        let (c1, c2, c3, c4) = (calls.clone(), calls.clone(), calls.clone(), calls.clone());

        let result = resume_stopped_job_for_foreground(
            move || {
                c1.lock().unwrap().push("give_terminal");
                Ok(())
            },
            move || {
                c2.lock().unwrap().push("send_sigcont");
                Err(nix::errno::Errno::ESRCH)
            },
            move || {
                c3.lock().unwrap().push("reclaim_terminal");
                Ok(())
            },
            move || {
                let c4 = c4.clone();
                async move { c4.lock().unwrap().push("mark_running") }
            },
        )
        .await;

        assert!(result.is_err(), "a failed SIGCONT must surface as an error");
        assert_eq!(
            *calls.lock().unwrap(),
            vec!["give_terminal", "send_sigcont", "reclaim_terminal"],
            "mark_running must never run after a failed SIGCONT, and the \
             terminal handoff must be reclaimed"
        );
    }

    /// If the terminal handoff itself fails, nothing past it should run —
    /// no `SIGCONT`, no reclaim (there was nothing to reclaim), no
    /// `mark_running`.
    #[tokio::test]
    async fn test_terminal_handoff_failure_skips_everything_else() {
        let calls = recorder();
        let (c1, c2, c3, c4) = (calls.clone(), calls.clone(), calls.clone(), calls.clone());

        let result = resume_stopped_job_for_foreground(
            move || {
                c1.lock().unwrap().push("give_terminal");
                Err(nix::errno::Errno::ENOTTY)
            },
            move || {
                c2.lock().unwrap().push("send_sigcont");
                Ok(())
            },
            move || {
                c3.lock().unwrap().push("reclaim_terminal");
                Ok(())
            },
            move || {
                let c4 = c4.clone();
                async move { c4.lock().unwrap().push("mark_running") }
            },
        )
        .await;

        assert!(result.is_err());
        assert_eq!(
            *calls.lock().unwrap(),
            vec!["give_terminal"],
            "a failed terminal handoff must short-circuit before SIGCONT"
        );
    }

    /// The happy path: `mark_running` only runs, and only runs, after a
    /// successful `SIGCONT` — confirming the fix didn't just move the bug
    /// (e.g. by skipping `mark_running` on success too).
    #[tokio::test]
    async fn test_successful_resume_marks_running_after_sigcont() {
        let calls = recorder();
        let (c1, c2, c3, c4) = (calls.clone(), calls.clone(), calls.clone(), calls.clone());

        let result = resume_stopped_job_for_foreground(
            move || {
                c1.lock().unwrap().push("give_terminal");
                Ok(())
            },
            move || {
                c2.lock().unwrap().push("send_sigcont");
                Ok(())
            },
            move || {
                c3.lock().unwrap().push("reclaim_terminal");
                Ok(())
            },
            move || {
                let c4 = c4.clone();
                async move { c4.lock().unwrap().push("mark_running") }
            },
        )
        .await;

        assert!(result.is_ok());
        assert_eq!(
            *calls.lock().unwrap(),
            vec!["give_terminal", "send_sigcont", "mark_running"],
            "reclaim_terminal must not run when nothing failed"
        );
    }
}
