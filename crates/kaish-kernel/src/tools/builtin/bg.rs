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
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Bg tool: resume a stopped job in the background.
pub struct Bg;

/// clap-derived argv layer for bg.
#[derive(Parser, Debug)]
#[command(name = "bg", about = "Resume a stopped job in the background")]
struct BgArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Job specifier (e.g. `%1`) or PID; defaults to the most recent job.
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
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match BgArgs::try_parse_from(
            std::iter::once("bg".to_string()).chain(args.to_argv()),
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

            ExecResult::with_output(OutputData::text(format!("[{}] {} &\n", job_id, cmd)))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
}
