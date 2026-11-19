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
use crate::interpreter::{ControlFlow, ExecResult};
use kaish_types::ToolFlow;
use crate::tools::{exec_context, schema_from_clap, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Timeout tool: run a command with a deadline.
pub struct Timeout;

/// clap-derived argv layer for timeout.
///
/// `timeout` wraps a command — its positionals are `DURATION COMMAND ARGS...`.
/// The schema sets `options_end_at_operand`, so the binder keeps every word
/// after the duration (flags included) in `positional`, and clap only sees
/// them behind the `--` that `to_argv` writes.
#[derive(Parser, Debug)]
#[command(name = "timeout", about = "Run a command with a time limit. Exits 124 when the limit expires.")]
struct TimeoutArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Duration, then the command and its arguments, passed on as written:
    /// `timeout 5 sh -c 'exit 3'`. Write the duration as `30`, `30s`, `500ms`,
    /// `5m`, or `1h`. Seconds take decimals (`1.5`). Options go before the
    /// duration. A command that finishes in time keeps its own exit code. On
    /// expiry, external children get SIGTERM, then SIGKILL after 2 seconds.
    ///
    /// The 2 seconds is `KernelConfig::kill_grace`'s default. The signals go to
    /// the child's whole process group.
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
            "Run a command with a time limit. Exits 124 when the limit expires.",
            [
                ("Stop a command after 5 seconds", "timeout 5 sleep 10"),
                ("Bound a network call to 500 ms", "timeout 500ms curl example.com"),
                ("Bound a build to 2 minutes", "timeout 2m cargo build"),
                ("Pass flags after the command name", "timeout 10 python3 -c 'print(1)'"),
            ],
        )
        .with_options_end_at_operand()
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        // A direct caller has no script to end: the exit is the result's code.
        self.run(args, ctx, &mut false).await
    }

    async fn execute_flow(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ToolFlow {
        let mut exited = false;
        let result = self.run(args, ctx, &mut exited).await;
        // `timeout` is not a subshell: an `exit` in the function it ran ends the script.
        if exited { ToolFlow::Exit(result) } else { ToolFlow::Normal(result) }
    }
}

impl Timeout {
    /// Run the command; `exited` is set when it ran `exit` before the deadline.
    async fn run(&self, args: ToolArgs, ctx: &mut dyn ToolCtx, exited: &mut bool) -> ExecResult {
        let ctx = exec_context(ctx);
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

        // `timeout 5 -- cmd` has always run `cmd`. GNU reads that `--` as the
        // command's name and fails; kaish skips it.
        let command_index = if matches!(args.positional.get(1), Some(Value::String(s)) if s == "--") { 2 } else { 1 };
        let positional = &args.positional;

        if positional.len() <= command_index {
            return ExecResult::failure(
                2,
                "timeout: usage: timeout DURATION COMMAND [ARGS...]",
            );
        }

        let duration_str = match &positional[0] {
            Value::String(s) => s.clone(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            other => {
                return ExecResult::failure(
                    2,
                    format!(
                        "timeout: the duration is a {}, not a time — write 30, 5s, 500ms, 2m, or 1h",
                        super::type_of::type_name(other)
                    ),
                )
            }
        };

        let duration = match parse_duration(&duration_str) {
            Some(d) => d,
            None => {
                return ExecResult::failure(
                    2,
                    format!(
                        "timeout: invalid duration '{}' (try: 30, 5s, 500ms, 2m, 1h)",
                        duration_str
                    ),
                )
            }
        };

        // `true`, `false`, and numerals bind as typed values. As a command
        // name they mean the word as written, so `timeout 5 true` runs `true`.
        let cmd_name = match &positional[command_index] {
            Value::String(s) => s.clone(),
            Value::Bool(_) | Value::Int(_) | Value::Float(_) => {
                match args.positional_text(command_index) {
                    Some(text) => text,
                    None => unreachable!("positional {command_index} exists: checked against len above"),
                }
            }
            other => {
                return ExecResult::failure(
                    2,
                    format!(
                        "timeout: the command name is a {}, not a word — pass a command, as in `timeout 5 sleep 1`",
                        super::type_of::type_name(other)
                    ),
                )
            }
        };

        let inner_args = words_to_args(&args, command_index + 1);

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
        let dispatch_result = dispatcher.dispatch_flow(&inner_cmd, ctx).await;
        ctx.cancel = saved;
        timer.abort();

        match dispatch_result {
            Ok(flow) => {
                // The deadline's verdict (124) outranks an exit that lost the race.
                let timed_out = elapsed.load(Ordering::SeqCst);
                if matches!(flow, ControlFlow::Exit { .. }) && !timed_out {
                    *exited = true;
                }
                let mut result = flow.into_absorbed_result();
                if timed_out {
                    result.code = 124;
                    // The timer firing is the authoritative reason, so always
                    // surface "timed out" — even when the inner command wrote
                    // its own cancellation message on the way down (e.g. a
                    // cancellation-aware builtin like `sleep` returns
                    // "sleep: interrupted"). Append rather than overwrite so
                    // that inner detail isn't lost.
                    let note = format!("timeout: timed out after {}", duration_str);
                    // The note leads `err`, but the inner stderr may already
                    // be on the job's stream: publish the rest of it, then
                    // the bytes this adds, so all of `err` is published once.
                    ctx.publish_job_stderr(&mut result).await;
                    let inner_published = result.stderr_published_len == result.err.len();
                    let sequence = ctx.output_sequence.clone();
                    let inner_order = kaish_types::StreamOrder::of(&result, &sequence);
                    let inner = std::mem::take(&mut result.err);
                    let mut added = String::new();
                    if !inner.is_empty() && !inner.ends_with('\n') {
                        added.push('\n');
                    }
                    added.push_str(&note);
                    added.push('\n');
                    result.err = format!("{note}\n{inner}");
                    if !result.err.ends_with('\n') {
                        result.err.push('\n');
                    }
                    // The note leads stderr, so it leads the order too.
                    let mut order = kaish_types::StreamOrder::new();
                    order.push(kaish_types::StreamKind::Stderr, note.len() as u64 + 1, &sequence);
                    order.extend(inner_order, &sequence);
                    result.set_stream_order(order);
                    result.stamp_stream_order(&sequence);
                    result.stderr_published_len = 0;
                    if inner_published && ctx.publishes_job_stderr() {
                        ctx.write_job_stderr(added.as_bytes()).await;
                        result.stderr_published_len = result.err.len();
                    }
                }
                result
            }
            // Keep what the command wrote before it faulted, as a failed
            // pipeline stage does.
            Err(e) => crate::scheduler::pipeline::fault_result(e.context("timeout")),
        }
    }
}

/// Forward evaluated words using their original operator kinds and numeral text.
fn words_to_args(args: &ToolArgs, start: usize) -> Vec<Arg> {
    use kaish_types::ArgumentSyntax;
    let expression = |value: &Value, raw: Option<&String>| match raw {
        Some(raw) => Expr::NumericLiteral {
            raw: raw.clone(),
            value: value.clone(),
        },
        None => Expr::Literal(value.clone()),
    };
    args.positional
        .iter()
        .enumerate()
        .skip(start)
        .map(|(index, value)| match args.positional_syntax.get(&index) {
            Some(ArgumentSyntax::ShortFlag(name)) => Arg::ShortFlag(name.clone()),
            Some(ArgumentSyntax::LongFlag(name)) => Arg::LongFlag(name.clone()),
            Some(ArgumentSyntax::Named { key, value, raw }) => Arg::Named {
                key: key.clone(),
                value: expression(value, raw.as_ref()),
            },
            Some(ArgumentSyntax::WordAssign { key, value, raw }) => Arg::WordAssign {
                key: key.clone(),
                value: expression(value, raw.as_ref()),
            },
            Some(ArgumentSyntax::DoubleDash) => Arg::DoubleDash,
            Some(_) => panic!("unsupported forwarded argument syntax"),
            None => Arg::Positional(expression(value, args.positional_raw.get(&index))),
        })
        .collect()
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

    /// Regression guard for the dispatcher re-entrancy deadlock:
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

    /// `true` and `false` bind as booleans. As a command name they run the
    /// builtin of that name, and its exit code passes through.
    #[tokio::test]
    async fn boolean_command_word_runs_that_command() {
        let kernel = make_kernel().await;
        let ok = kernel.execute("timeout 5 true").await.unwrap();
        assert_eq!(ok.code, 0, "err={:?}", ok.err);
        let no = kernel.execute("timeout 5 false").await.unwrap();
        assert_eq!(no.code, 1, "err={:?}", no.err);
        let via_var = kernel.execute("x=true; timeout 5 $x").await.unwrap();
        assert_eq!(via_var.code, 0, "err={:?}", via_var.err);
    }

    /// A list cannot name a command. The refusal names the type and the fix,
    /// never a Rust debug rendering.
    #[tokio::test]
    async fn collection_command_word_is_refused_by_type() {
        let kernel = make_kernel().await;
        let result = kernel.execute("xs=[a b]; timeout 5 $xs").await.unwrap();
        assert_eq!(result.code, 2);
        assert!(result.err.contains("is a list"), "err={:?}", result.err);
        assert!(!result.err.contains("Json("), "err={:?}", result.err);
    }

    #[tokio::test]
    async fn boolean_duration_is_refused_by_type() {
        let kernel = make_kernel().await;
        let result = kernel.execute("timeout true sleep 1").await.unwrap();
        assert_eq!(result.code, 2);
        assert!(result.err.contains("duration is a bool"), "err={:?}", result.err);
        assert!(!result.err.contains("Bool("), "err={:?}", result.err);
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
