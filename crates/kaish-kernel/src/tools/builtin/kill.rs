//! kill — Send signals to processes or jobs.
//!
//! Job control over kaish's own jobs (`kill %N`) is a kernel-level concern and
//! works in **every** build, hermetic ones included: a background job is a kaish
//! task with a cancellation token, not necessarily an OS process, so terminating
//! it needs no platform signal support. The `subprocess` capability only adds
//! real OS-signal *fidelity* — delivering an arbitrary signal (`STOP`/`CONT`/…)
//! to an external child's process group, and signalling a bare PID. When
//! external commands are disabled there are no such processes to signal anyway
//! (and when they're enabled, `/bin/kill` is on PATH for raw PIDs).

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::scheduler::{JobId, JobManager};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// Kill tool: send signals to processes or jobs.
pub struct Kill;

/// clap-derived argv layer for kill.
#[derive(Parser, Debug)]
#[command(name = "kill", about = "Send a signal to a process or job")]
struct KillArgs {
    /// Signal name or number: TERM, KILL, STOP, CONT, INT, HUP, USR1, USR2, QUIT (--signal or -s)
    #[arg(short = 's', long, default_value_t = String::from("TERM"))]
    signal: String,

    /// Abandon a latched (confirmation-pending) job. Without this flag, kill
    /// refuses to destroy a job's pending confirmation gate. Conflicts with
    /// --signal: discarding a gate delivers nothing to anyone.
    #[arg(long, conflicts_with = "signal")]
    discard: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Target PID(s) or job specifier(s) (e.g. `%1`).
    targets: Vec<String>,
}

#[async_trait]
impl Tool for Kill {
    fn name(&self) -> &str {
        "kill"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &KillArgs::command(),
            "kill",
            "Send a signal to a process or job",
            [
                ("Terminate a job", "kill %1"),
                ("Kill a process by PID", "kill --signal KILL 1234"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kill: {e}")),
        };
        let parsed = match KillArgs::try_parse_from(
            std::iter::once("kill".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kill: {e}")),
        };
        parsed.global.apply(ctx);

        // Signal from --signal / -s named param, or default to TERM. Prefer
        // args.named so an Int signal keeps its typing.
        //
        // `--signal` is a named/flag value only, never positional, so
        // `ToolArgs::to_argv()` now rejects a `Value::Bytes` signal loudly
        // before `KillArgs::try_parse_from` ever runs (GH #164, closing the
        // root cause behind this GH #116 guard) — a Bytes value can no
        // longer reach this match at all (the positional target form below
        // is guarded separately, since positional Bytes isn't covered by
        // `to_argv()`'s guard).
        let signal_name = match args.named.get("signal") {
            Some(Value::String(s)) => s.clone(),
            Some(Value::Int(i)) => i.to_string(),
            Some(other) => crate::interpreter::value_to_string(other),
            None => parsed.signal,
        };

        let target_str = match args.get_positional(0) {
            Some(Value::String(s)) => s.clone(),
            Some(Value::Int(i)) => i.to_string(),
            Some(_) => return ExecResult::failure(1, "kill: invalid target"),
            None => return ExecResult::failure(1, "kill: usage: kill [--signal SIG] target"),
        };

        // Job reference `%N` — kaish-level job control, available in every build.
        if let Some(job_num) = target_str.strip_prefix('%') {
            let job_id = match job_num.parse::<u64>() {
                Ok(i) => JobId(i),
                Err(_) => {
                    return ExecResult::failure(1, format!("kill: invalid job reference: {target_str}"))
                }
            };
            let manager = match &ctx.job_manager {
                Some(m) => m.clone(),
                None => return ExecResult::failure(1, "kill: no job manager"),
            };
            // A latched job's cached result is the only handle to its pending
            // confirmation — killing it would silently destroy the gate
            // (GH #96). Refuse unless the caller explicitly discards.
            //
            // TOCTOU note: a Running job can race into Latched between this
            // check and kill_job's cancel+remove. That's acceptable — the
            // guard protects an already-visible confirmation request from
            // accidental destruction; a job killed while still running is the
            // caller's stated intent, whatever it was about to become.
            if manager.is_latched(job_id).await {
                if !parsed.discard {
                    return ExecResult::failure(
                        1,
                        format!(
                            "kill: job {job_id} is latched awaiting confirmation — \
                             fulfill it (see /v/jobs/{job_id}/latch) or abandon it \
                             with: kill --discard %{job_id}"
                        ),
                    );
                }
                manager.cancel(job_id).await;
                manager.remove(job_id).await;
                return ExecResult::success(format!(
                    "kill: discarded pending latch for job {job_id}"
                ));
            }
            return kill_job(&manager, job_id, &signal_name).await;
        }

        // Bare PID — signalling an OS process needs the subprocess capability.
        kill_pid(&target_str, &signal_name)
    }
}

/// Classify a signal as terminating (unwinds the job) vs. non-terminating,
/// without depending on platform signal types. Used by the hermetic job path to
/// decide between cancelling the job and refusing a signal it cannot deliver.
/// `None` = unrecognised signal.
///
/// Only the portable POSIX signal numbers (identical across Linux/macOS/BSD) are
/// classified numerically; job-control numerics that vary by platform
/// (`STOP`/`CONT`/`USR*`) are matched by name. An unknown number is treated as
/// non-terminating — safe, since the hermetic path can only ever *cancel*.
#[cfg(not(all(unix, feature = "subprocess")))]
fn signal_is_terminating(name: &str) -> Option<bool> {
    if let Ok(num) = name.parse::<i32>() {
        return match num {
            // Portable terminating signals: HUP, INT, QUIT, ABRT, KILL, TERM.
            1 | 2 | 3 | 6 | 9 | 15 => Some(true),
            n if n > 0 => Some(false),
            _ => None,
        };
    }
    let name = name.strip_prefix("SIG").unwrap_or(name);
    match name {
        "TERM" | "KILL" | "INT" | "HUP" | "QUIT" | "ABRT" => Some(true),
        "STOP" | "CONT" | "USR1" | "USR2" | "TSTP" | "WINCH" => Some(false),
        _ => None,
    }
}

// ─── Hermetic build: job control via cancellation tokens only ───────────────

/// Terminate a kaish job by its cancellation token. No OS-signal support, so
/// only terminating signals are honoured; anything else is refused loudly.
#[cfg(not(all(unix, feature = "subprocess")))]
async fn kill_job(manager: &JobManager, job_id: JobId, signal_name: &str) -> ExecResult {
    match signal_is_terminating(signal_name) {
        Some(true) => {
            if manager.cancel(job_id).await {
                manager.remove(job_id).await;
                ExecResult::success("")
            } else {
                ExecResult::failure(1, format!("kill: job {job_id} not found"))
            }
        }
        Some(false) => ExecResult::failure(
            1,
            format!(
                "kill: job {job_id} is an in-process task; only termination signals \
                 (TERM/KILL/INT/HUP/QUIT) can be delivered, not {signal_name} \
                 (arbitrary-signal delivery needs the subprocess capability)"
            ),
        ),
        None => ExecResult::failure(1, format!("kill: unknown signal: {signal_name}")),
    }
}

/// Without the subprocess capability there are no OS processes kaish spawned to
/// signal, so a bare-PID target cannot be honoured.
#[cfg(not(all(unix, feature = "subprocess")))]
fn kill_pid(target: &str, _signal_name: &str) -> ExecResult {
    ExecResult::failure(
        1,
        format!("kill: {target}: signalling a PID requires the subprocess capability"),
    )
}

// ─── subprocess build: full OS-signal fidelity ──────────────────────────────

/// Signal a kaish job. Prefers the real process group(s) the job spawned (so any
/// signal — STOP/CONT/USR1/… — is delivered faithfully); falls back to the
/// cancellation token for pure in-process jobs (which can only be terminated).
#[cfg(all(unix, feature = "subprocess"))]
async fn kill_job(manager: &JobManager, job_id: JobId, signal_name: &str) -> ExecResult {
    use nix::sys::signal::Signal;

    let signal = match parse_signal(signal_name) {
        Some(s) => s,
        None => return ExecResult::failure(1, format!("kill: unknown signal: {signal_name}")),
    };
    let terminating = matches!(
        signal,
        Signal::SIGTERM | Signal::SIGKILL | Signal::SIGINT | Signal::SIGHUP | Signal::SIGQUIT
    );

    // Real process group(s) recorded for the job — deliver the signal directly.
    let pgids = manager.job_pgids(job_id).await;
    if !pgids.is_empty() {
        let mut last_err = None;
        for pg in &pgids {
            let pgid = nix::unistd::Pid::from_raw(*pg as i32);
            if let Err(e) = nix::sys::signal::killpg(pgid, signal) {
                last_err = Some(e);
            }
        }
        // A terminating signal also unwinds the wrapping task and drops the job.
        if terminating {
            manager.cancel(job_id).await;
            manager.remove(job_id).await;
        }
        return match last_err {
            Some(e) => ExecResult::failure(1, format!("kill: {e}")),
            None => ExecResult::success(""),
        };
    }

    // No process group — a pure in-process job (e.g. `sleep &`, a kaish builtin)
    // or an external whose PGID hasn't registered yet. The cancellation token is
    // the only lever; it can stop the job but not deliver an arbitrary signal.
    if terminating {
        if manager.cancel(job_id).await {
            manager.remove(job_id).await;
            ExecResult::success("")
        } else {
            ExecResult::failure(1, format!("kill: job {job_id} not found"))
        }
    } else {
        ExecResult::failure(
            1,
            format!(
                "kill: job {job_id} is an in-process task with no process group; \
                 only termination signals (TERM/KILL/INT/HUP/QUIT) can be delivered, not {signal_name}"
            ),
        )
    }
}

/// Send a signal to a bare PID via the OS.
#[cfg(all(unix, feature = "subprocess"))]
fn kill_pid(target: &str, signal_name: &str) -> ExecResult {
    let signal = match parse_signal(signal_name) {
        Some(s) => s,
        None => return ExecResult::failure(1, format!("kill: unknown signal: {signal_name}")),
    };
    let pid_num: i32 = match target.parse() {
        Ok(p) => p,
        Err(_) => return ExecResult::failure(1, format!("kill: invalid pid: {target}")),
    };
    let pid = nix::unistd::Pid::from_raw(pid_num);
    if let Err(e) = nix::sys::signal::kill(pid, signal) {
        return ExecResult::failure(1, format!("kill: ({pid_num}): {e}"));
    }
    ExecResult::success("")
}

/// Parse a signal name or number to a `nix` Signal value.
#[cfg(all(unix, feature = "subprocess"))]
fn parse_signal(name: &str) -> Option<nix::sys::signal::Signal> {
    use nix::sys::signal::Signal;

    // Try as number first
    if let Ok(num) = name.parse::<i32>() {
        return Signal::try_from(num).ok();
    }

    // Strip optional "SIG" prefix
    let name = name.strip_prefix("SIG").unwrap_or(name);

    match name {
        "TERM" => Some(Signal::SIGTERM),
        "KILL" => Some(Signal::SIGKILL),
        "STOP" => Some(Signal::SIGSTOP),
        "CONT" => Some(Signal::SIGCONT),
        "INT" => Some(Signal::SIGINT),
        "HUP" => Some(Signal::SIGHUP),
        "USR1" => Some(Signal::SIGUSR1),
        "USR2" => Some(Signal::SIGUSR2),
        "QUIT" => Some(Signal::SIGQUIT),
        _ => None,
    }
}
