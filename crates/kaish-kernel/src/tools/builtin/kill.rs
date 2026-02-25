//! kill — Send signals to processes or jobs.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::scheduler::JobId;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Kill tool: send signals to processes or jobs.
pub struct Kill;

#[async_trait]
impl Tool for Kill {
    fn name(&self) -> &str {
        "kill"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("kill", "Send a signal to a process or job")
            .param(ParamSchema::optional(
                "signal",
                "string",
                Value::String("TERM".to_string()),
                "Signal name or number: TERM, KILL, STOP, CONT, INT, HUP, USR1, USR2, QUIT (--signal or -s)",
            ))
            .param(ParamSchema::required(
                "target",
                "string",
                "Process ID or %N for job reference",
            ))
            .example("Terminate a job", "kill %1")
            .example("Kill a process by PID", "kill --signal KILL 1234")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        #[cfg(not(unix))]
        {
            let _ = (args, ctx);
            return ExecResult::failure(1, "kill: not supported on this platform");
        }

        #[cfg(unix)]
        {
            // Get signal from --signal / -s named param, or default to TERM
            let signal_name = args.named.get("signal")
                .map(|v| match v {
                    Value::String(s) => s.clone(),
                    Value::Int(i) => i.to_string(),
                    other => crate::interpreter::value_to_string(other),
                })
                .unwrap_or_else(|| "TERM".to_string());

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

                let (_pid, pgid) = match manager.get_process_info(job_id).await {
                    Some(info) => info,
                    None => return ExecResult::failure(1, format!("kill: job {} not found", job_id)),
                };

                let pgid = nix::unistd::Pid::from_raw(pgid as i32);
                if let Err(e) = nix::sys::signal::killpg(pgid, signal) {
                    return ExecResult::failure(1, format!("kill: {}", e));
                }

                // If SIGKILL or SIGTERM, remove the job
                if signal == nix::sys::signal::Signal::SIGKILL || signal == nix::sys::signal::Signal::SIGTERM {
                    manager.remove(job_id).await;
                }

                ExecResult::success("")
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
#[cfg(unix)]
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
