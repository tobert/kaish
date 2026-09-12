//! bg — Resume a stopped job in the background.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

#[cfg(unix)]
use crate::ast::Value;
use crate::interpreter::ExecResult;
#[cfg(unix)]
use crate::interpreter::OutputData;
#[cfg(unix)]
use crate::scheduler::JobId;
use crate::tools::{exec_context, schema_from_clap, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Bg tool: resume a stopped job in the background.
pub struct Bg;

/// How often the background reaper polls a backgrounded job for
/// completion (GH #162). 200ms keeps `jobs`/`kill %N` state and the async
/// job-manager cleanup reasonably prompt after the process actually exits,
/// without waking up an otherwise-idle task 100+ times a second for every
/// backgrounded job (which, unlike `wait`'s much tighter 5-10ms poll, has
/// no foreground caller blocked on the result and may run for hours).
#[cfg(unix)]
const BG_REAP_POLL_INTERVAL: std::time::Duration = std::time::Duration::from_millis(200);

/// clap-derived argv layer for bg.
#[derive(Parser, Debug)]
#[command(name = "bg", about = "Resume a stopped job in the background")]
struct BgArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Job id, with or without the `%` prefix (`%1` or `1`). Defaults to the
    /// highest-numbered stopped job.
    job: Vec<String>,
}

#[async_trait]
impl Tool for Bg {
    fn name(&self) -> &str {
        "bg"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &BgArgs::command(),
            "bg",
            "Resume a stopped job in the background",
            [
                ("Resume last stopped job", "bg"),
                ("Resume specific job", "bg 2"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("bg: {e}")),
        };
        let parsed = match BgArgs::try_parse_from(
            std::iter::once("bg".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("bg: {e}")),
        };
        parsed.global.apply(ctx);

        #[cfg(not(unix))]
        {
            let _ = args;
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
                    Value::String(s) => {
                        // Accept the bash jobspec form `%N` as well as a bare
                        // number; the `%` is a job marker, not part of the id
                        // (mirrors `kill`/`wait`).
                        let digits = s.strip_prefix('%').unwrap_or(s);
                        match digits.parse::<u64>() {
                            Ok(i) => JobId(i),
                            Err(_) => return ExecResult::failure(1, format!("bg: invalid job id: {}", s)),
                        }
                    }
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

            // Confirm SIGCONT before marking the job Running (no terminal
            // transfer — it runs in background). If the signal fails (e.g.
            // the process already died), the job must stay Stopped rather
            // than be marked Running with no live process and no reaper ever
            // spawned to collect it (GH #126 part B).
            if let Err(e) = nix::sys::signal::killpg(pgid, nix::sys::signal::Signal::SIGCONT) {
                return ExecResult::failure(1, format!("bg: failed to continue job: {}", e));
            }
            manager.resume_job(job_id).await;

            // Spawn a background reaper task.
            //
            // GH #162: this used to be `tokio::task::block_in_place` running
            // a blocking `waitpid(pid, None)` loop. Its `JoinHandle` was
            // (deliberately) dropped, uncollected — but dropping a
            // `JoinHandle` doesn't cancel the task; it kept running on
            // tokio's blocking thread pool for as long as the backgrounded
            // process did. `tokio::runtime::Runtime::drop` blocks the
            // dropping thread until all outstanding blocking-pool work
            // finishes, so exiting the REPL hung for as long as any
            // backgrounded job kept running.
            //
            // Fixed by polling `waitpid(pid, WNOHANG)` on a plain async
            // interval instead: nothing here calls `block_in_place`, so
            // there's no blocking-pool work for `Runtime::drop` to wait on
            // — an unfinished poll loop is simply dropped/cancelled at
            // shutdown like any other async task. This matches bash's own
            // default (`huponexit` off): a backgrounded job outlives the
            // shell, unsignaled and orphaned to init; kaish's reaper never
            // gets to block exit waiting to observe it finish.
            let jobs = manager.clone();
            let pid = nix::unistd::Pid::from_raw(_pid_raw as i32);
            tokio::spawn(async move {
                use nix::sys::wait::{WaitPidFlag, WaitStatus};
                // WUNTRACED matters: a resumed job can be stopped AGAIN (a
                // second Ctrl-Z, or SIGSTOP from anywhere). Without it the stop
                // is invisible here, so the manager keeps reporting Running, the
                // job is unreachable via `last_stopped()`, and this loop sleeps
                // forever against a process that will never exit. `wait_all`
                // then polls that job forever too — the same shutdown hang the
                // `stopped` skip fixes, reached by a route the skip cannot see,
                // because `resume_job` already cleared the flag.
                let flags = WaitPidFlag::WNOHANG | WaitPidFlag::WUNTRACED;
                loop {
                    match nix::sys::wait::waitpid(pid, Some(flags)) {
                        Ok(WaitStatus::Exited(_, _)) | Ok(WaitStatus::Signaled(_, _, _)) => break,
                        // Stopped again: hand the job back to the stopped state
                        // so `jobs`, `fg`, and `last_stopped` all agree with the
                        // OS, and stop polling. Resuming it spawns a fresh
                        // reaper, so exiting here leaks nothing.
                        Ok(WaitStatus::Stopped(_, _)) => {
                            jobs.stop_job(job_id, _pid_raw, _pid_raw).await;
                            return;
                        }
                        // `StillAlive` (WNOHANG, nothing to report yet), any
                        // other transient status, or EINTR: keep polling.
                        Ok(_) | Err(nix::errno::Errno::EINTR) => {
                            tokio::time::sleep(BG_REAP_POLL_INTERVAL).await;
                        }
                        // e.g. ECHILD — already reaped elsewhere.
                        Err(_) => break,
                    }
                }
                jobs.remove(job_id).await;
            });

            ExecResult::with_output(OutputData::text(format!("[{}] {} &\n", job_id, cmd)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools::ExecContext;
    use crate::scheduler::JobManager;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::os::unix::process::CommandExt;
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    /// GH #126 part B: a failed `SIGCONT` must not leave the job looking
    /// `Running` with a dead process behind it. Spawn a real short-lived
    /// process in its own process group (mirroring how a Ctrl-Z'd job is
    /// registered — pgid == pid), register it as stopped, then actually kill
    /// *and reap* it so its pid/pgid no longer exist in the process table:
    /// `killpg(SIGCONT)` on that now-nonexistent group returns ESRCH,
    /// exercising the exact failure path `bg` must handle without flipping
    /// `stopped` to `false` first.
    #[tokio::test]
    async fn test_bg_failed_sigcont_does_not_mark_job_running() {
        let mut child = std::process::Command::new("sleep")
            .arg("5")
            .process_group(0)
            .spawn()
            .expect("spawn sleep");
        let pid = child.id();

        let manager = Arc::new(JobManager::new());
        let job_id = manager.register_stopped("sleep 5".to_string(), pid, pid).await;

        child.kill().expect("kill sleep");
        child.wait().expect("reap sleep");

        let mut ctx = make_ctx();
        ctx.set_job_manager(manager.clone());
        let mut args = ToolArgs::new();
        // A bare `Value::Int` job id, not the `%N` jobspec string form — this
        // test targets the SIGCONT-ordering bug (part B) independent of the
        // `%`-prefix parsing fix (part A), which lands on a separate branch.
        args.positional.push(Value::Int(job_id.0 as i64));

        let result = Bg.execute(args, &mut ctx).await;
        assert!(!result.ok(), "SIGCONT to a dead process group should fail");
        assert!(
            result.err.contains("failed to continue job"),
            "expected a SIGCONT failure, got: {}",
            result.err
        );

        // Pre-fix: `resume_job()` ran before the (failing) `killpg`, so the
        // job was left `stopped = false` (JobStatus::Running) with no live
        // process and no reaper ever spawned to collect it. Post-fix: the
        // job must still show as Stopped.
        let info = manager.get(job_id).await.expect("job still tracked");
        assert_eq!(
            info.status,
            kaish_types::JobStatus::Stopped,
            "job must stay Stopped after a failed SIGCONT, not silently become Running"
        );
    }

    /// The reaper's `WUNTRACED` arm: a job resumed by `bg` that is stopped
    /// AGAIN (SIGSTOP from anywhere) must go back to `Stopped` in the manager.
    /// Without `WUNTRACED` the stop was invisible — the job showed `Running`
    /// forever, was unreachable via `last_stopped()`, and `wait_all` polled it
    /// forever at shutdown.
    #[tokio::test]
    async fn test_bg_reaper_marks_a_restopped_job_stopped_again() {
        let mut child = std::process::Command::new("sleep")
            .arg("30")
            .process_group(0)
            .spawn()
            .expect("spawn sleep");
        let pid = child.id();

        let manager = Arc::new(JobManager::new());
        let job_id = manager.register_stopped("sleep 30".to_string(), pid, pid).await;

        let mut ctx = make_ctx();
        ctx.set_job_manager(manager.clone());
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(job_id.0 as i64));

        // `bg` SIGCONTs the group and spawns the WUNTRACED reaper.
        let result = Bg.execute(args, &mut ctx).await;
        assert!(result.ok(), "bg must resume the live job: {}", result.err);

        // Stop it again from outside — the second-Ctrl-Z shape.
        nix::sys::signal::kill(
            nix::unistd::Pid::from_raw(pid as i32),
            nix::sys::signal::Signal::SIGSTOP,
        )
        .expect("SIGSTOP the resumed job");

        // The reaper polls every 200ms; give it a few cycles.
        let mut status = kaish_types::JobStatus::Running;
        for _ in 0..20 {
            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
            if let Some(info) = manager.get(job_id).await {
                status = info.status;
                if status == kaish_types::JobStatus::Stopped {
                    break;
                }
            }
        }
        assert_eq!(
            status,
            kaish_types::JobStatus::Stopped,
            "the reaper must observe the second stop and hand the job back to Stopped"
        );

        // Cleanup: the job is stopped; kill and reap it so nothing leaks.
        child.kill().expect("kill sleep");
        child.wait().expect("reap sleep");
    }
}
