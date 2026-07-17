//! timeout — Run a command with a time limit (kills the child on elapsed).
//!
//! Derives a child cancellation token from `ctx.cancel`, spawns a delay task
//! that cancels it after `duration`, runs the inner command under the child
//! token, and overrides the exit code to 124 (coreutils convention) when the
//! timer fired. The kernel's `try_execute_external` honors the cancelled
//! token by killing the child process group with SIGTERM/grace/SIGKILL.
//!
//! Interaction with `ToolCtx::patient` (the suspendable script watchdog):
//! none, deliberately. This builtin's timer is a one-shot sleep on its own
//! child token, independent of the kernel watchdog — a user who writes
//! `timeout 5 cmd` asked for a hard bound on `cmd`, so a patient hold inside
//! `cmd` does not stretch it. The hold still suspends the *script* budget.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};

use crate::ast::{Arg, Command, Expr, Value};
use crate::duration::parse_duration;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Timeout tool: run a command with a deadline.
pub struct Timeout;

/// clap-derived argv layer for timeout.
///
/// `timeout` wraps a command — its positionals are `DURATION COMMAND ARGS...`.
/// The inner command tokens may themselves look like flags (e.g. `timeout 5
/// echo -n hello`), so the sink accepts arbitrary hyphenated values.
#[derive(Parser, Debug)]
#[command(name = "timeout", about = "Run a command with a time limit; kills the child on elapsed")]
struct TimeoutArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Duration (e.g. `5`, `5s`, `2m`) followed by the command and its arguments.
    duration_and_command: Vec<String>,
}

#[async_trait]
impl Tool for Timeout {
    fn name(&self) -> &str {
        "timeout"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TimeoutArgs::command(),
            "timeout",
            "Run a command with a time limit; kills the child on elapsed",
            [
                ("With seconds", "timeout 5 sleep 10"),
                ("With duration suffix", "timeout 500ms curl example.com"),
                ("Minutes", "timeout 2m cargo build"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("timeout: {e}")),
        };
        let parsed = match TimeoutArgs::try_parse_from(
            std::iter::once("timeout".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("timeout: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.len() < 2 {
            return ExecResult::failure(
                1,
                "timeout: usage: timeout DURATION COMMAND [ARGS...]",
            );
        }

        let duration_str = match &args.positional[0] {
            Value::String(s) => s.clone(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            other => {
                return ExecResult::failure(
                    1,
                    format!("timeout: invalid duration: {:?}", other),
                )
            }
        };

        let duration = match parse_duration(&duration_str) {
            Some(d) => d,
            None => {
                return ExecResult::failure(
                    1,
                    format!(
                        "timeout: invalid duration '{}' (try: 30, 5s, 500ms, 2m, 1h)",
                        duration_str
                    ),
                )
            }
        };

        let cmd_name = match &args.positional[1] {
            Value::String(s) => s.clone(),
            other => {
                return ExecResult::failure(
                    1,
                    format!("timeout: invalid command: {:?}", other),
                )
            }
        };

        let inner_args: Vec<Arg> = args.positional[2..]
            .iter()
            .map(|v| Arg::Positional(Expr::Literal(v.clone())))
            .collect();

        let inner_cmd = Command {
            name: cmd_name,
            args: inner_args,
            redirects: vec![],
        };

        let Some(dispatcher) = ctx.dispatcher.clone() else {
            return ExecResult::failure(
                1,
                "timeout: no dispatcher available (Kernel must be created via into_arc())",
            );
        };

        // Derive a child cancel token from the current ctx token. The timer
        // task cancels it on elapsed; the cascade fires SIGTERM/SIGKILL on
        // any external children via wait_or_kill. Swap the child token onto
        // ctx for the duration of the inner dispatch so cancellation
        // propagates naturally.
        let parent_token = ctx.cancel.clone();
        let child_token = parent_token.child_token();

        let elapsed = Arc::new(AtomicBool::new(false));
        let elapsed_writer = elapsed.clone();
        let timer_token = child_token.clone();
        let timer = tokio::spawn(async move {
            tokio::time::sleep(duration).await;
            elapsed_writer.store(true, Ordering::SeqCst);
            timer_token.cancel();
        });

        let saved = std::mem::replace(&mut ctx.cancel, child_token);
        let dispatch_result = dispatcher.dispatch(&inner_cmd, ctx).await;
        ctx.cancel = saved;
        timer.abort();

        match dispatch_result {
            Ok(mut result) => {
                if elapsed.load(Ordering::SeqCst) {
                    result.code = 124;
                    // The timer firing is the authoritative reason, so always
                    // surface "timed out" — even when the inner command wrote
                    // its own cancellation message on the way down (e.g. a
                    // cancellation-aware builtin like `sleep` returns
                    // "sleep: interrupted"). Append rather than overwrite so
                    // that inner detail isn't lost.
                    let note = format!("timeout: timed out after {}", duration_str);
                    result.err = if result.err.is_empty() {
                        note
                    } else {
                        format!("{}\n{}", note, result.err)
                    };
                }
                result
            }
            Err(e) => ExecResult::failure(1, format!("timeout: {}", e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::kernel::{Kernel, KernelConfig};

    /// Create a Kernel wrapped in Arc for tests that need full dispatch.
    async fn make_kernel() -> std::sync::Arc<Kernel> {
        Kernel::new(KernelConfig::isolated().with_skip_validation(true))
            .unwrap()
            .into_arc()
    }

    #[tokio::test]
    async fn test_timeout_missing_args() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout").await.unwrap();
        assert!(!result.ok());
        assert!(result.err.contains("usage"));
    }

    #[tokio::test]
    async fn test_timeout_invalid_duration() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout abc echo hi").await.unwrap();
        assert!(!result.ok());
        assert!(result.err.contains("invalid duration"));
    }

    #[tokio::test]
    async fn test_timeout_numeric_duration_succeeds() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout 5 echo works").await.unwrap();
        assert!(
            result.ok(),
            "expected ok, got code={} err={:?}",
            result.code,
            result.err
        );
        assert!(result.text_out().contains("works"));
    }

    /// Regression guard for the dispatcher re-entrancy deadlock (docs/issues.md):
    /// `timeout` re-dispatches its inner command through `ctx.dispatcher`, which
    /// needs `exec_ctx.write()`. If `execute_command` ever again holds that write
    /// guard across `tool.execute`, this hangs forever. The outer
    /// `tokio::time::timeout` turns that regression into a clean, fast failure
    /// instead of a wedged test suite.
    #[tokio::test]
    async fn test_redispatch_does_not_deadlock() {
        use std::time::Duration;
        let kernel = make_kernel().await;
        let outcome = tokio::time::timeout(
            Duration::from_secs(10),
            kernel.execute("timeout 5 echo works"),
        )
        .await;
        let result = outcome
            .expect("re-dispatch deadlocked: execute() did not return within 10s")
            .expect("kernel execute errored");
        assert!(result.ok(), "code={} err={:?}", result.code, result.err);
        assert!(result.text_out().contains("works"));
    }

    #[tokio::test]
    async fn test_timeout_suffix_duration_succeeds() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout 5s echo hello").await.unwrap();
        assert!(result.ok());
        assert!(result.text_out().contains("hello"));
    }

    #[tokio::test]
    async fn test_timeout_builtin_times_out() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout 100ms sleep 10").await.unwrap();
        assert_eq!(result.code, 124);
        assert!(result.err.contains("timed out"));
    }

    #[tokio::test]
    async fn test_timeout_command_not_found() {
        let kernel = make_kernel().await;
        let result = kernel
            .execute("timeout 5s not_a_command_xyz_123")
            .await
            .unwrap();
        assert!(!result.ok());
        assert_eq!(result.code, 127);
    }
}
