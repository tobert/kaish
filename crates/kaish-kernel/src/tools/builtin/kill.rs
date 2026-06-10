//! kill — Send signals to processes or jobs.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

#[cfg(all(unix, feature = "subprocess"))]
use crate::ast::Value;
use crate::interpreter::ExecResult;
#[cfg(all(unix, feature = "subprocess"))]
use crate::scheduler::JobId;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Kill tool: send signals to processes or jobs.
pub struct Kill;

/// clap-derived argv layer for kill. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "kill", about = "Send a signal to a process or job")]
struct KillArgs {
    /// Signal name or number: TERM, KILL, STOP, CONT, INT, HUP, USR1, USR2, QUIT (--signal or -s)
    #[arg(short = 's', long, default_value_t = String::from("TERM"))]
    signal: String,

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
        #[cfg(not(all(unix, feature = "subprocess")))]
        {
            let _ = (args, ctx);
            return ExecResult::failure(1, "kill: not supported on this platform");
        }

        #[cfg(all(unix, feature = "subprocess"))]
        {
            let parsed = match KillArgs::try_parse_from(
                std::iter::once("kill".to_string()).chain(args.to_argv()),
            ) {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(2, format!("kill: {e}")),
            };
            parsed.global.apply(ctx);

            // Get signal from --signal / -s named param, or default to TERM.
            // Prefer args.named (preserves Value typing for Int signals).
            let signal_name = args
                .named
                .get("signal")
                .map(|v| match v {
                    Value::String(s) => s.clone(),
                    Value::Int(i) => i.to_string(),
                    other => crate::interpreter::value_to_string(other),
                })
                .unwrap_or(parsed.signal);

            // Get target from first positional
            let target_str = match args.get_positional(0) {
                Some(Value::String(s)) => s.clone(),
                Some(Value::Int(i)) => i.to_string(),
                Some(_) => return ExecResult::failure(1, "kill: invalid target"),
                None => return ExecResult::failure(1, "kill: usage: kill [--signal SIG] target"),
            };

            // Parse signal
            let signal = match parse_signal(&signal_name) {
                Some(s) => s,
                None => return ExecResult::failure(1, format!("kill: unknown signal: {}", signal_name)),
            };

            // Parse target: %N for job, bare number for PID
            if let Some(job_num) = target_str.strip_prefix('%') {
                // Job reference
                let job_id = match job_num.parse::<u64>() {
                    Ok(i) => JobId(i),
                    Err(_) => return ExecResult::failure(1, format!("kill: invalid job reference: {}", target_str)),
                };

                let manager = match &ctx.job_manager {
                    Some(m) => m.clone(),
                    None => return ExecResult::failure(1, "kill: no job manager"),
                };

                use nix::sys::signal::Signal;
                let terminating = matches!(
                    signal,
                    Signal::SIGTERM | Signal::SIGKILL | Signal::SIGINT | Signal::SIGHUP | Signal::SIGQUIT
                );

                // Prefer signalling the real process group(s) the job spawned —
                // this delivers ANY signal faithfully (STOP/CONT/USR1/…), which
                // is what job control needs.
                let pgids = manager.job_pgids(job_id).await;
                if !pgids.is_empty() {
                    let mut last_err = None;
                    for pg in &pgids {
                        let pgid = nix::unistd::Pid::from_raw(*pg as i32);
                        if let Err(e) = nix::sys::signal::killpg(pgid, signal) {
                            last_err = Some(e);
                        }
                    }
                    // A terminating signal also unwinds the wrapping task and
                    // drops the job from the table.
                    if terminating {
                        manager.cancel(job_id).await;
                        manager.remove(job_id).await;
                    }
                    return match last_err {
                        Some(e) => ExecResult::failure(1, format!("kill: {}", e)),
                        None => ExecResult::success(""),
                    };
                }

                // No process group recorded — a pure in-process job (e.g.
                // `sleep &`, a kaish builtin) or an external whose PGID hasn't
                // registered yet. The cancellation token is the only lever;
                // it can stop the job but cannot deliver an arbitrary signal.
                if terminating {
                    if manager.cancel(job_id).await {
                        manager.remove(job_id).await;
                        ExecResult::success("")
                    } else {
                        ExecResult::failure(1, format!("kill: job {} not found", job_id))
                    }
                } else {
                    ExecResult::failure(1, format!(
                        "kill: job {} is an in-process task with no process group; \
                         only termination signals (TERM/KILL/INT/HUP/QUIT) can be delivered, not {}",
                        job_id, signal_name
                    ))
                }
            } else {
                // PID reference
                let pid_num: i32 = match target_str.parse() {
                    Ok(p) => p,
                    Err(_) => return ExecResult::failure(1, format!("kill: invalid pid: {}", target_str)),
                };

                let pid = nix::unistd::Pid::from_raw(pid_num);
                if let Err(e) = nix::sys::signal::kill(pid, signal) {
                    return ExecResult::failure(1, format!("kill: ({}): {}", pid_num, e));
                }

                ExecResult::success("")
            }
        }
    }
}

/// Parse a signal name or number to a Signal value.
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
