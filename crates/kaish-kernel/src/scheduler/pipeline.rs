//! Pipeline execution for kaish.
//!
//! Executes a sequence of commands connected by pipes, where the stdout
//! of each command becomes the stdin of the next.
//!
//! Also handles scatter/gather pipelines for parallel execution.

use std::sync::Arc;

use std::collections::HashMap;

use crate::arithmetic;
use crate::ast::{Arg, Command, Expr, Redirect, RedirectKind, Value};
use crate::dispatch::{CommandDispatcher, PipelinePosition};
use crate::interpreter::{ExecResult, PathError};
use crate::tools::{ExecContext, ToolArgs, ToolRegistry, ToolSchema};
use tokio::io::AsyncWriteExt;

use super::pipe_stream::pipe_stream_default;
use super::scatter::{
    parse_gather_options, parse_scatter_options, ScatterGatherRunner,
};

/// Apply redirects to an execution result.
///
/// Pre-execution redirects (Stdin, HereDoc) should be handled before calling.
/// Post-execution redirects (stdout/stderr to file, merge) applied here.
/// Redirects are processed left-to-right per POSIX.
pub(crate) async fn apply_redirects(
    mut result: ExecResult,
    redirects: &[Redirect],
    ctx: &ExecContext,
    dispatcher: &dyn CommandDispatcher,
) -> ExecResult {
    // Defer materialization of OutputData → result.out to individual redirect
    // handlers. File redirects (Overwrite/Append) can stream OutputData directly
    // to disk via write_canonical(), avoiding OOM on large structured output.
    // Merge redirects and the fallthrough path materialize on demand.
    for redir in redirects {
        match redir.kind {
            RedirectKind::MergeStderr => {
                // 2>&1 - append stderr to stdout
                // Ensure output is materialized for merge
                result.materialize();
                if !result.err.is_empty() {
                    let err = std::mem::take(&mut result.err);
                    result.push_out(&err);
                }
            }
            RedirectKind::MergeStdout => {
                // 1>&2 or >&2 - append stdout to stderr (a text stream).
                // Binary stdout can't be folded into text stderr without
                // corruption — fail loud instead.
                if result.is_bytes() {
                    return ExecResult::failure(
                        1,
                        "redirect: cannot merge binary stdout into stderr (1>&2) — \
                         redirect it to a file or pipe through base64/xxd",
                    );
                }
                result.materialize();
                if !result.text_out().is_empty() {
                    let out = result.text_out().into_owned();
                    result.err.push_str(&out);
                }
                // `1>&2` is still a stdout redirect: stdout went to stderr, so
                // drop out/output AND the .data sideband (same as a file
                // redirect), or a structured result leaks past `x=$(cmd >&2)`
                // and `cmd >&2 | consumer`. Unconditional so a .data-only,
                // empty-.out result is cleared too. clear_stdout preserves a
                // control-plane latch request.
                result.clear_stdout();
            }
            RedirectKind::StdoutOverwrite => {
                let path = match eval_redirect_target(&redir.target, ctx, dispatcher).await {
                    Ok(p) => p,
                    Err(e) => return ExecResult::failure(1, format!("redirect: {e}")),
                };
                // A binary result writes its raw bytes (no lossy decode).
                if let Some(bytes) = result.out_bytes() {
                    if let Err(e) = redirect_write(ctx, &path, bytes).await {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                } else if let Some(output) = result.take_output_for_stream() {
                    // Stream OutputData directly to file if available
                    let mut buf = Vec::new();
                    if let Err(e) = output.write_canonical(&mut buf, None) {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                    if let Err(e) = redirect_write(ctx, &path, &buf).await {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                } else if let Err(e) = redirect_write(ctx, &path, result.text_out().as_bytes()).await {
                    return ExecResult::failure(1, format!("redirect: {e}"));
                }
                // stdout went to the file: drop out/output AND the .data sideband.
                result.clear_stdout();
            }
            RedirectKind::StdoutAppend => {
                let path = match eval_redirect_target(&redir.target, ctx, dispatcher).await {
                    Ok(p) => p,
                    Err(e) => return ExecResult::failure(1, format!("redirect: {e}")),
                };
                // A binary result appends its raw bytes (no lossy decode).
                if let Some(bytes) = result.out_bytes() {
                    if let Err(e) = redirect_append(ctx, &path, bytes).await {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                } else if let Some(output) = result.take_output_for_stream() {
                    // Stream OutputData directly if available
                    let mut buf = Vec::new();
                    if let Err(e) = output.write_canonical(&mut buf, None) {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                    if let Err(e) = redirect_append(ctx, &path, &buf).await {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                } else if let Err(e) = redirect_append(ctx, &path, result.text_out().as_bytes()).await {
                    return ExecResult::failure(1, format!("redirect: {e}"));
                }
                // stdout went to the file: drop out/output AND the .data sideband.
                result.clear_stdout();
            }
            RedirectKind::Stderr => {
                let path = match eval_redirect_target(&redir.target, ctx, dispatcher).await {
                    Ok(p) => p,
                    Err(e) => return ExecResult::failure(1, format!("redirect: {e}")),
                };
                if let Err(e) = redirect_write(ctx, &path, result.err.as_bytes()).await {
                    return ExecResult::failure(1, format!("redirect: {e}"));
                }
                result.err.clear();
            }
            RedirectKind::Both => {
                let path = match eval_redirect_target(&redir.target, ctx, dispatcher).await {
                    Ok(p) => p,
                    Err(e) => return ExecResult::failure(1, format!("redirect: {e}")),
                };
                // Build the combined bytes: raw binary stdout (no lossy decode),
                // or structured output streamed straight to a byte buffer via
                // `take_output_for_stream`/`write_canonical` — same lazy path
                // `>`/`>>` use above — instead of forcing it through one
                // `String` first (`text_out()`'s canonical-string fallback).
                // Falls back to the text form only when neither applies.
                // Followed by stderr.
                let mut combined: Vec<u8> = if let Some(b) = result.out_bytes() {
                    b.to_vec()
                } else if let Some(output) = result.take_output_for_stream() {
                    let mut buf = Vec::new();
                    if let Err(e) = output.write_canonical(&mut buf, None) {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                    buf
                } else {
                    result.text_out().into_owned().into_bytes()
                };
                combined.extend_from_slice(result.err.as_bytes());
                if let Err(e) = redirect_write(ctx, &path, &combined).await {
                    return ExecResult::failure(1, format!("redirect: {e}"));
                }
                // both streams went to the file: drop stdout (incl. .data) + stderr.
                result.clear_stdout();
                result.err.clear();
            }
            // Pre-execution redirects - already handled before command execution
            RedirectKind::Stdin | RedirectKind::HereDoc | RedirectKind::HereString => {}
        }
    }
    // Materialize any remaining OutputData into result.out.
    // Callers (accumulate_result, pipeline piping) expect .out to be populated
    // after apply_redirects returns. File redirects above consume .output directly
    // via streaming; this only fires when no redirect consumed it.
    result.materialize();
    result
}

/// Evaluate a redirect target expression to get the file path (or heredoc body).
///
/// Takes the `dispatcher` explicitly rather than reading `ctx.dispatcher`, so
/// command substitution (`$(...)`) in the target runs on the *same* dispatcher
/// the runner already uses — `cat < $(echo f)`, `echo x > $(echo f)`, and
/// `$(...)` inside a heredoc body. `ctx.dispatcher` is only populated when the
/// kernel is Arc-attached (`into_arc`); a bare `Kernel::execute` — every test,
/// and any embedder holding a `Kernel` by value — left it `None`, so a `$()`
/// target silently fell back to the sync evaluator that can't run it (GH #90).
/// The runner always holds a real dispatcher; thread it through so the behavior
/// no longer depends on how the kernel was constructed.
async fn eval_redirect_target(
    expr: &Expr,
    ctx: &ExecContext,
    dispatcher: &dyn CommandDispatcher,
) -> Result<String, String> {
    let value = dispatcher
        .eval_expr(expr, ctx)
        .await
        .map_err(|e| e.to_string())?;
    // Decision D: a bare collection can't be a redirect target either — same
    // process-boundary guard as external argv (see `structured_boundary_error`).
    if let Some(msg) = crate::interpreter::structured_boundary_error("a redirect target", &value) {
        return Err(msg);
    }
    // Text sink: binary goes loud rather than becoming a file literally named
    // `[binary: N bytes]` — the same guard external argv and env export use.
    crate::interpreter::value_to_text_sink_named(&value, "a redirect target").map_err(|e| e.to_string())
}

/// Write data to a file via the VFS backend.
///
/// The redirect target is resolved against `ctx.cwd` (like every other path
/// operand — see `cat`/`cp`/etc.), so a relative `> f` write and a later
/// relative read agree on the same `$PWD/f`. Without this the router would
/// normalize a bare relative path to `/f`, diverging from cwd-resolved reads.
async fn redirect_write(ctx: &ExecContext, path: &str, data: &[u8]) -> Result<(), String> {
    use crate::backend::WriteMode;
    let resolved = ctx.resolve_path(path);
    ctx.backend.write(&resolved, data, WriteMode::Overwrite).await.map_err(|e| e.to_string())
}

/// Append data to a file via the VFS backend.
///
/// Resolves the target against `ctx.cwd` for the same reason as `redirect_write`.
async fn redirect_append(ctx: &ExecContext, path: &str, data: &[u8]) -> Result<(), String> {
    let resolved = ctx.resolve_path(path);
    ctx.backend.append(&resolved, data).await.map_err(|e| e.to_string())
}

/// Set up stdin from redirects (< file, <<heredoc).
/// Called before command execution.
///
/// `< file` reads through the VFS backend (not the host filesystem) with the
/// target resolved against `ctx.cwd`, mirroring how `cat` and the output
/// redirects resolve their operands. A missing/unreadable file is a hard
/// error — we never silently feed the command empty stdin. Non-UTF-8 content
/// is NOT rejected here (GH #176): `ctx.stdin` is bytes-typed, so the raw
/// bytes flow through to whatever the command actually does with them — a
/// byte-aware builtin (`wc -c`, `cat`, `cmp`, …) consumes them intact, and a
/// text-only builtin refuses loudly at the point it asks for text
/// (`read_stdin_to_text`), not before the command even runs.
async fn setup_stdin_redirects(
    cmd: &Command,
    ctx: &mut ExecContext,
    dispatcher: &dyn CommandDispatcher,
) -> Result<(), String> {
    use std::path::Path;
    for redir in &cmd.redirects {
        match &redir.kind {
            RedirectKind::Stdin => {
                let path = eval_redirect_target(&redir.target, ctx, dispatcher).await?;
                let resolved = ctx.resolve_path(&path);
                let data = ctx
                    .backend
                    .read(Path::new(&resolved), None)
                    .await
                    .map_err(|e| format!("redirect: {path}: {e}"))?;
                ctx.set_stdin(data);
            }
            RedirectKind::HereDoc => {
                match &redir.target {
                    Expr::Literal(Value::String(content)) => {
                        ctx.set_stdin(content.clone());
                    }
                    // Heredoc bodies may contain `$(...)`; route through the
                    // dispatcher so command substitution runs.
                    expr => {
                        let body = eval_redirect_target(expr, ctx, dispatcher).await?;
                        ctx.set_stdin(body);
                    }
                }
            }
            RedirectKind::HereString => {
                // Per bash, here-strings append a trailing newline to the
                // expanded word so the command receives a terminated line.
                let mut s = eval_redirect_target(&redir.target, ctx, dispatcher).await?;
                s.push('\n');
                ctx.set_stdin(s);
            }
            _ => {}
        }
    }
    Ok(())
}

/// Runs pipelines by spawning tasks and connecting them via channels.
#[derive(Clone)]
pub struct PipelineRunner {
    tools: Arc<ToolRegistry>,
}

impl PipelineRunner {
    /// Create a new pipeline runner with the given tool registry.
    pub fn new(tools: Arc<ToolRegistry>) -> Self {
        Self { tools }
    }

    /// Execute a pipeline of commands.
    ///
    /// Each command's stdout becomes the next command's stdin.
    /// If the pipeline contains scatter/gather, delegates to ScatterGatherRunner.
    /// Returns the result of the last command in the pipeline.
    ///
    /// The `dispatcher` handles the full command resolution chain (user tools,
    /// builtins, scripts, external commands, backend tools). The runner handles
    /// I/O routing: stdin redirects, piping between commands, and output redirects.
    pub async fn run(
        &self,
        commands: &[Command],
        ctx: &mut ExecContext,
        dispatcher: &dyn CommandDispatcher,
    ) -> ExecResult {
        if commands.is_empty() {
            return ExecResult::success("");
        }

        // Check for scatter/gather pipeline
        if let Some((scatter_idx, gather_idx)) = find_scatter_gather(commands) {
            return self.run_scatter_gather(commands, scatter_idx, gather_idx, ctx, dispatcher).await;
        }

        self.run_sequential(commands, ctx, dispatcher).await
    }

    /// Execute commands sequentially without scatter/gather detection.
    ///
    /// Used by `ScatterGatherRunner` for pre_scatter, post_gather, and parallel
    /// workers. Breaks the async recursion chain (`run` → scatter → `run`).
    pub async fn run_sequential(
        &self,
        commands: &[Command],
        ctx: &mut ExecContext,
        dispatcher: &dyn CommandDispatcher,
    ) -> ExecResult {
        if commands.is_empty() {
            return ExecResult::success("");
        }

        if commands.len() == 1 {
            // Single command, no piping needed
            return self.run_single(&commands[0], ctx, None, dispatcher).await;
        }

        // Multi-command pipeline
        self.run_pipeline(commands, ctx, dispatcher).await
    }

    /// Run a scatter/gather pipeline.
    async fn run_scatter_gather(
        &self,
        commands: &[Command],
        scatter_idx: usize,
        gather_idx: usize,
        ctx: &mut ExecContext,
        dispatcher: &dyn CommandDispatcher,
    ) -> ExecResult {
        // Split pipeline into parts
        let pre_scatter = &commands[..scatter_idx];
        let scatter_cmd = &commands[scatter_idx];
        let parallel = &commands[scatter_idx + 1..gather_idx];
        let gather_cmd = &commands[gather_idx];
        let post_gather = &commands[gather_idx + 1..];

        // Parse options from scatter and gather commands
        // These are builtins with simple key=value syntax, no schema-driven parsing needed.
        // build_tool_args is fallible: a bad/subscripted collection access in a
        // scatter/gather flag value (`scatter --as ${u[nope]}`) must fail loud here,
        // not silently coalesce to a dropped flag (see docs/issues.md's now-closed
        // "reduced sync path" entry). Mirrors run_single's dispatch-error handling.
        let scatter_schema = self.tools.get("scatter").map(|t| t.schema());
        let gather_schema = self.tools.get("gather").map(|t| t.schema());
        let scatter_args = match build_tool_args(&scatter_cmd.args, ctx, scatter_schema.as_ref()).await {
            Ok(args) => args,
            Err(e) => return ExecResult::failure(1, format!("scatter: {e}")),
        };
        let gather_args = match build_tool_args(&gather_cmd.args, ctx, gather_schema.as_ref()).await {
            Ok(args) => args,
            Err(e) => return ExecResult::failure(1, format!("gather: {e}")),
        };
        let scatter_opts = match parse_scatter_options(&scatter_args) {
            Ok(opts) => opts,
            Err(e) => return ExecResult::failure(2, format!("scatter: {e}")),
        };
        let gather_opts = match parse_gather_options(&gather_args) {
            Ok(opts) => opts,
            Err(e) => return ExecResult::failure(2, format!("gather: {e}")),
        };

        // We need an `Arc<dyn CommandDispatcher>` to hand to `ScatterGatherRunner`.
        // `fork_attached` produces a subkernel whose cancellation token is a
        // child of the parent's, so a parent timeout/cancel cascades into
        // the scatter pipeline (and into worker children via further forks).
        let sequential_dispatcher: Arc<dyn CommandDispatcher> = dispatcher.fork_attached().await;

        let runner = ScatterGatherRunner::new(self.tools.clone(), sequential_dispatcher);
        runner
            .run(
                pre_scatter,
                scatter_opts,
                parallel,
                gather_opts,
                &gather_cmd.redirects,
                post_gather,
                ctx,
            )
            .await
    }

    /// Run a single command with optional stdin.
    ///
    /// The dispatcher handles arg parsing, schema lookup, output format, and execution.
    /// The runner handles stdin setup (redirects + pipeline) and output redirects.
    async fn run_single(
        &self,
        cmd: &Command,
        ctx: &mut ExecContext,
        stdin: Option<Vec<u8>>,
        dispatcher: &dyn CommandDispatcher,
    ) -> ExecResult {
        // Set up stdin from redirects (< file, <<heredoc)
        if let Err(e) = setup_stdin_redirects(cmd, ctx, dispatcher).await {
            return ExecResult::failure(1, e);
        }

        // Set stdin from pipeline (overrides redirect stdin)
        if let Some(input) = stdin {
            ctx.set_stdin(input);
        }

        // Set pipeline position for stdio inheritance decisions
        ctx.pipeline_position = PipelinePosition::Only;

        // Execute via dispatcher (full resolution chain)
        let result = match dispatcher.dispatch(cmd, ctx).await {
            Ok(result) => result,
            Err(e) => ExecResult::failure(1, e.to_string()),
        };

        // Apply post-execution redirects
        apply_redirects(result, &cmd.redirects, ctx, dispatcher).await
    }

    /// Run a multi-command pipeline concurrently.
    ///
    /// Each stage runs in its own tokio task, connected by bounded pipe streams
    /// (64KB ring buffers with backpressure). This provides:
    /// - Bounded memory usage (no buffering entire outputs)
    /// - Backpressure (fast producers wait for slow consumers)
    /// - Early termination (e.g., `seq 1 1000000 | head -n 5`)
    ///
    /// Structured data (`stdin_data`) is passed via oneshot channels alongside pipes.
    async fn run_pipeline(
        &self,
        commands: &[Command],
        ctx: &mut ExecContext,
        dispatcher: &dyn CommandDispatcher,
    ) -> ExecResult {
        let stage_count = commands.len();
        let last_idx = stage_count - 1;

        // Create N-1 pipe pairs connecting adjacent stages
        let mut pipe_writers: Vec<Option<super::pipe_stream::PipeWriter>> = Vec::new();
        let mut pipe_readers: Vec<Option<super::pipe_stream::PipeReader>> = Vec::new();

        for _ in 0..last_idx {
            let (writer, reader) = pipe_stream_default();
            pipe_writers.push(Some(writer));
            pipe_readers.push(Some(reader));
        }

        // Create N-1 oneshot channels for structured data sideband
        let mut data_senders: Vec<Option<tokio::sync::oneshot::Sender<Option<Value>>>> = Vec::new();
        let mut data_receivers: Vec<Option<tokio::sync::oneshot::Receiver<Option<Value>>>> = Vec::new();

        for _ in 0..last_idx {
            let (tx, rx) = tokio::sync::oneshot::channel();
            data_senders.push(Some(tx));
            data_receivers.push(Some(rx));
        }

        let mut handles: Vec<tokio::task::JoinHandle<(ExecResult, ExecContext)>> = Vec::with_capacity(stage_count);

        for (i, cmd) in commands.iter().enumerate() {
            let mut stage_ctx = ctx.child_for_pipeline();
            let cmd = cmd.clone();

            // Fork attached: each concurrent pipeline stage needs independent
            // mutable state, but cancellation should still cascade from the
            // parent (so a request timeout kills externals running in any
            // stage, not just the foreground one).
            let task_dispatcher: Arc<dyn CommandDispatcher> = dispatcher.fork_attached().await;

            // Set up stdin from redirects on the child context. A failure here
            // (e.g. `cmd < missing`) fails this stage; surface it from inside
            // the spawned task so the normal join/collection path reports it.
            let stdin_setup = setup_stdin_redirects(&cmd, &mut stage_ctx, dispatcher).await;

            // Wire pipe_stdin: stage 0 gets parent stdin (if no redirect), others get pipe reader
            if i == 0 {
                // First stage inherits the parent's stdin, but only if redirects didn't
                // already set stdin (e.g., heredoc). Don't overwrite redirect-provided stdin.
                if stage_ctx.stdin.is_none() {
                    stage_ctx.stdin = ctx.stdin.take();
                }
                if stage_ctx.stdin_data.is_none() {
                    stage_ctx.stdin_data = ctx.stdin_data.take();
                }
                // Inherit a frontend-seeded lazy stdin pipe (non-Clone, so moved),
                // unless a redirect already provided stdin — `read_stdin_*` prefers
                // `pipe_stdin`, and `set_stdin` clears it, so `< file` still wins.
                if stage_ctx.stdin.is_none() && stage_ctx.pipe_stdin.is_none() {
                    stage_ctx.pipe_stdin = ctx.pipe_stdin.take();
                }
            } else {
                // Intermediate/last stages read from pipe
                stage_ctx.pipe_stdin = pipe_readers[i - 1].take();
                // Structured data received via oneshot (resolved at start of execution)
            }

            // Wire pipe_stdout: last stage writes to ExecResult, others write to pipe
            if i < last_idx {
                stage_ctx.pipe_stdout = pipe_writers[i].take();
            }

            // Set pipeline position
            stage_ctx.pipeline_position = match i {
                0 => PipelinePosition::First,
                n if n == last_idx => PipelinePosition::Last,
                _ => PipelinePosition::Middle,
            };

            let data_sender = if i < last_idx { data_senders[i].take() } else { None };
            let data_receiver = if i > 0 { data_receivers[i - 1].take() } else { None };

            // Propagate the embedder's trace context across the spawn boundary
            // so each concurrent stage's spans stay in the same trace.
            let handle: tokio::task::JoinHandle<(ExecResult, ExecContext)> =
                tokio::spawn(crate::telemetry::bind_current_context(async move {
                // A stdin-redirect setup failure short-circuits this stage.
                if let Err(e) = stdin_setup {
                    return (ExecResult::failure(1, e), stage_ctx);
                }

                // Hand the structured-data sideband receiver to the stage; do
                // NOT pre-read it. A consuming builtin resolves it via
                // `ctx.resolve_stdin()`, which drains the pipe first (so a
                // streaming upstream can't deadlock) and only then awaits this —
                // by which point the producer has sent it. The old `try_recv`
                // here raced the producer's post-dispatch send and silently
                // dropped structured data (`seq 1 3 | jq .` → text → parse error).
                stage_ctx.stdin_data_rx = data_receiver;

                // Execute the command
                let mut result = match task_dispatcher.dispatch(&cmd, &mut stage_ctx).await {
                    Ok(result) => result,
                    Err(e) => ExecResult::failure(1, e.to_string()),
                };

                // Apply post-execution redirects. Use the stage's own
                // (forked) dispatcher — the borrowed `dispatcher` can't cross
                // the spawn boundary, and `stage_ctx.dispatcher` is `None` on a
                // bare kernel, which is exactly the GH #90 gap.
                result = apply_redirects(result, &cmd.redirects, &stage_ctx, &*task_dispatcher).await;

                // Flush buffered stderr to the kernel's stderr stream.
                // This delivers error output from intermediate pipeline stages
                // in real-time (via the kernel drain) instead of silently discarding it.
                // Redirects like 2>&1 have already cleared result.err, so merged
                // stderr goes through the pipe as expected.
                if !result.err.is_empty() {
                    if let Some(ref stderr) = stage_ctx.stderr {
                        stderr.write_str(&result.err);
                        result.err.clear();
                    }
                }

                // Send structured data to the next stage via the oneshot BEFORE
                // the pipe write. The consumer's `resolve_stdin` drains the pipe
                // FIRST and only THEN awaits this oneshot, so by the time it
                // reads the sideband the value is already here — sending before
                // the (possibly backpressured) pipe write keeps that ordering.
                if let Some(tx) = data_sender {
                    let _ = tx.send(result.data.clone());
                }

                // Write output to pipe for next stage (if not last).
                // Consumer is now unblocked and can drain concurrently.
                if let Some(mut pipe_out) = stage_ctx.pipe_stdout.take() {
                    // A binary result flows through the pipe as raw bytes;
                    // structured output serializes straight to a byte buffer
                    // (`write_canonical`) rather than building the full
                    // canonical `String` first — same lazy path the `>`/`>>`
                    // file redirects use via `take_output_for_stream`. Either
                    // way the next stage gets exactly what was produced — no
                    // lossy round-trip.
                    let bytes: Vec<u8> = if let Some(b) = result.out_bytes() {
                        b.to_vec()
                    } else if let Some(output) = result.take_output_for_stream() {
                        let mut buf = Vec::new();
                        // `Vec<u8>`'s `Write` impl is infallible; a serialize
                        // error here would only come from a future non-memory
                        // writer, so fall back to the same lossy text form the
                        // non-streaming branch already uses rather than
                        // dropping the stage's output outright.
                        if output.write_canonical(&mut buf, None).is_err() {
                            buf = output.to_canonical_string().into_bytes();
                        }
                        buf
                    } else {
                        result.text_out().into_owned().into_bytes()
                    };
                    if !bytes.is_empty() {
                        // Write result to pipe; ignore broken pipe (reader dropped early)
                        let _ = pipe_out.write_all(&bytes).await;
                        let _ = pipe_out.shutdown().await;
                    }
                    // Drop pipe_out signals EOF to next stage's reader
                }

                (result, stage_ctx)
            }));

            handles.push(handle);
        }

        // Await all stages and return last stage's result.
        // Sync the last stage's scope back to the parent context so that
        // variable assignments in the last pipeline stage are visible
        // (e.g., `echo "Alice" | read NAME`).
        let mut last_result = ExecResult::success("");
        let mut panics: Vec<String> = Vec::new();
        // GH #125: a confirmation gate (`set -o latch`) raised by an EARLIER
        // stage must not be swallowed by a later stage's nominal success —
        // `rm x | echo done` used to exit 0 with `.latch` dropped even though
        // `rm` genuinely gated (the op never ran; only its stderr text hinted at
        // the gate). First latch wins if more than one stage gates, mirroring
        // `wait.rs`'s `classify()` "first latch wins" precedent — captured here
        // in stage order, so the first `Some` set below IS the first stage.
        let mut gated: Option<ExecResult> = None;
        for (i, handle) in handles.into_iter().enumerate() {
            match handle.await {
                Ok((result, stage_ctx)) => {
                    if result.latch.is_some() {
                        gated.get_or_insert_with(|| result.clone());
                    }
                    if i == last_idx {
                        last_result = result;
                        // Sync last stage's scope and cwd changes back
                        ctx.scope = stage_ctx.scope;
                        ctx.cwd = stage_ctx.cwd;
                        ctx.prev_cwd = stage_ctx.prev_cwd;
                        ctx.aliases = stage_ctx.aliases;
                    }
                }
                Err(e) => {
                    panics.push(format!("stage {}: {}", i, e));
                }
            }
        }

        // A pending gate is a control-plane fact about the WHOLE pipeline, not
        // stage-local trivia — override the last stage's nominal result so the
        // exit code (2) and the structured `.latch` both survive.
        if let Some(gate) = gated {
            last_result = gate;
        }

        // Checked LAST (so it wins over the latch override above): mirrors the
        // existing precedent that ANY stage panicking overrides `last_result`
        // regardless of which stage, and keeps a gate raised earlier in the same
        // run HELD (unconfirmed) rather than presenting a ready-to-confirm nonce
        // after a crash elsewhere in the pipeline.
        if !panics.is_empty() {
            last_result = ExecResult::failure(
                1,
                format!("pipeline stage(s) panicked: {}", panics.join("; ")),
            );
        }

        last_result
    }
}

/// Extract parameter types from a tool schema.
///
/// Returns a map from param name → param type (e.g., "verbose" → "bool", "output" → "string").
/// Build a map from flag name → (canonical param name, param type).
///
/// Includes both primary names and aliases (with dashes stripped).
/// For short flags like `-n` aliased to `lines`, maps `"n"` → `("lines", "int", 1)`.
/// The third tuple slot is `consumes`: how many positionals the flag pulls
/// per occurrence (1 for standard `--flag value`, 2 for jq's `--arg NAME VAL`).
///
/// Positional params (`positional: true`) are excluded — they're not flags,
/// and including them would mis-route `cat --paths foo.txt` from positional
/// to named, regressing builtins that read from `args.positional`.
/// Walk leading positionals to select the active subcommand leaf of a schema.
///
/// A flat tool (`schema.subcommands` empty) returns the root immediately —
/// today's single-leaf behavior. For a subcommand-aware tool each leading
/// positional, in order, must name a child (by `name` or a command-level
/// alias) to descend; the first positional that names no child is the leaf's
/// own argument, and selection stops there. Multi-level trees fall out by
/// construction (`block edit insert` → two descents).
///
/// Routing is **literal-only**: a subcommand selector must be a bareword or
/// quoted string (both parse to `Expr::Literal(Value::String)`). A *computed*
/// positional (`$(…)`, `$VAR`, a glob) sitting where a subcommand is required
/// is an **error**, not a silent guess — kaish can't see its value at parse
/// time, so picking a leaf from it would misroute the flags that bind against
/// the leaf's params. The fix is to spell the subcommand out, or use the
/// `--flag=value` form (which binds without any schema lookup).
///
/// Returned leaf borrows from `schema`, so its `params`/`subcommands` outlive
/// any `schema_param_lookup` taken from it.
///
/// **Global value flags.** A space-form value flag declared on the *root*
/// (e.g. kj's global `--confirm <nonce>`) can legitimately precede the
/// subcommand path. Its value is a positional in the AST, so routing must not
/// mistake it for a subcommand selector — `select_leaf` skips the value of any
/// root-declared non-bool flag it sees. Leaf-specific value flags can't precede
/// their own subcommand by construction, so only the root's flags need this.
pub fn select_leaf<'a>(schema: &'a ToolSchema, args: &[Arg]) -> anyhow::Result<&'a ToolSchema> {
    // Names + aliases of root-declared value (non-bool, non-positional) flags,
    // whose space-form value is a positional we must skip while routing.
    let root_lookup = schema_param_lookup(schema);
    let is_root_value_flag = |name: &str| -> bool {
        root_lookup.get(name).is_some_and(|(_, typ, ..)| !is_bool_type(typ))
    };

    let mut node = schema;
    let mut skip_next_positional = false;
    for arg in args {
        match arg {
            // Tokens past `--` are raw data, never subcommand selectors.
            Arg::DoubleDash => break,
            // A root value flag in space form consumes the next positional as
            // its value — don't route on that positional.
            Arg::LongFlag(name) if is_root_value_flag(name) => skip_next_positional = true,
            Arg::ShortFlag(name) if is_root_value_flag(name) => skip_next_positional = true,
            Arg::Positional(expr) => {
                if skip_next_positional {
                    skip_next_positional = false;
                    continue; // this positional is the preceding flag's value
                }
                if node.subcommands.is_empty() {
                    break; // leaf reached — remaining positionals are its args
                }
                match classify_subcommand_positional(expr) {
                    SubcommandWord::Word(word) => {
                        match node.subcommands.iter().find(|c| c.matches_command(word)) {
                            Some(child) => node = child, // descend
                            None => break,               // not a subcommand → leaf's own arg
                        }
                    }
                    // A non-string literal (number/bool) can't be a subcommand
                    // name but its value *is* known; treat it as the leaf's own
                    // positional and stop — no misroute risk.
                    SubcommandWord::OtherLiteral => break,
                    SubcommandWord::Computed(kind) => anyhow::bail!(
                        "{}: a subcommand name is required here, but got {kind}. \
                         Subcommands must be literal words — spell it out \
                         (e.g. `{} <subcommand> …`) or use the `--flag=value` form.",
                        node.name,
                        schema.name
                    ),
                }
            }
            // Flags are skipped during routing; they bind against the leaf.
            _ => {}
        }
    }
    Ok(node)
}

/// How a positional reads when a subcommand selector is expected.
enum SubcommandWord<'a> {
    /// A literal word that may name a child.
    Word(&'a str),
    /// A literal but non-string value — a known value, never a subcommand.
    OtherLiteral,
    /// A value computed at runtime; `kind` describes it for the error.
    Computed(&'static str),
}

fn classify_subcommand_positional(expr: &Expr) -> SubcommandWord<'_> {
    match expr {
        Expr::Literal(Value::String(s)) => SubcommandWord::Word(s),
        Expr::Literal(_) => SubcommandWord::OtherLiteral,
        Expr::CommandSubst(_) | Expr::Command(_) => SubcommandWord::Computed("a command substitution `$(…)`"),
        Expr::VarRef(_)
        | Expr::VarWithDefault { .. }
        | Expr::VarLength(_)
        | Expr::Positional(_)
        | Expr::AllArgs
        | Expr::ArgCount
        | Expr::CurrentPid
        | Expr::LastExitCode => SubcommandWord::Computed("a variable reference"),
        Expr::Interpolated(_) | Expr::HereDocBody { .. } => SubcommandWord::Computed("an interpolated string"),
        Expr::GlobPattern(_) => SubcommandWord::Computed("a glob pattern"),
        Expr::Arithmetic(_) => SubcommandWord::Computed("an arithmetic expansion"),
        _ => SubcommandWord::Computed("a value computed at runtime"),
    }
}

pub fn schema_param_lookup(schema: &ToolSchema) -> HashMap<String, (&str, &str, usize, bool)> {
    let mut map = HashMap::new();
    for p in schema.params.iter().filter(|p| !p.positional) {
        map.insert(p.name.clone(), (p.name.as_str(), p.param_type.as_str(), p.consumes, p.repeatable));
        for alias in &p.aliases {
            let stripped = alias.trim_start_matches('-');
            map.insert(stripped.to_string(), (p.name.as_str(), p.param_type.as_str(), p.consumes, p.repeatable));
        }
    }
    map
}

/// Check if a type is considered boolean.
pub fn is_bool_type(param_type: &str) -> bool {
    matches!(param_type.to_lowercase().as_str(), "bool" | "boolean")
}

/// Reduced [`crate::kernel::ArgValueSource`] for `build_tool_args` below:
/// evaluates via this module's own synchronous `eval_simple_expr` (no
/// recursion into the async pipeline, so no command substitution) and never
/// expands globs or tilde — `build_tool_args`'s historical "reduced sync"
/// contract (see its doc comment), preserved exactly. Only the STRUCTURAL
/// flag/positional binding now comes from the one shared
/// `crate::kernel::bind_tool_args` core (GH #188).
struct SyncEvalSource<'a> {
    ctx: &'a ExecContext,
}

#[async_trait::async_trait]
impl crate::kernel::ArgValueSource for SyncEvalSource<'_> {
    async fn eval(&self, expr: &Expr) -> anyhow::Result<Option<Value>> {
        eval_simple_expr(expr, self.ctx).map_err(|e| anyhow::anyhow!(e))
    }

    async fn expand_glob(&self, _pattern: &str) -> anyhow::Result<Option<Vec<String>>> {
        // This reduced context has never expanded globs (bare patterns bind
        // as literal text via `eval_simple_expr`'s `GlobPattern` arm) —
        // scatter/gather's own flag values (`--as`, `--limit`, `--timeout`)
        // are never file globs, so there's nothing to fix here (GH #188
        // scoped this out; see the PR description).
        Ok(None)
    }

    async fn home(&self) -> Option<String> {
        // No tilde expansion in this reduced context — unchanged from
        // before GH #188 (scatter/gather's own flag values are never paths).
        None
    }
}

/// Build ToolArgs from AST Args, evaluating expressions — the reduced sync
/// wrapper around the shared [`crate::kernel::bind_tool_args`] core. Used by
/// scatter/gather's own option parsing (`run_scatter_gather`, below —
/// before any worker forks, so it can't recurse back into
/// `PipelineRunner::run` for command substitution) and the `#[cfg(test)]`
/// `BackendDispatcher` (`dispatch.rs`).
///
/// GH #188: this used to be a hand-rolled twin of `Kernel::build_args_async`'s
/// flag/positional-binding logic that could — and did — drift from it (no
/// undeclared-space-flag guard, no glued-short-flag handling, no
/// `consumes`/`repeatable` accumulation). Now it's a thin wrapper: the
/// binding logic itself is shared via [`SyncEvalSource`], and only
/// expression evaluation differs (this context can't run `$(...)`).
pub async fn build_tool_args(
    args: &[Arg],
    ctx: &ExecContext,
    schema: Option<&ToolSchema>,
) -> Result<ToolArgs, String> {
    crate::kernel::bind_tool_args(args, schema, &SyncEvalSource { ctx })
        .await
        .map_err(|e| e.to_string())
}

/// Simple expression evaluation for args (without full scope access).
///
/// `Ok(None)` means "not representable in this reduced sync context" (only
/// binary ops fall here now — everything else this reduced binder can't
/// evaluate, like command substitution, has its own explicit `Err` arm
/// below; callers treat `None` the same as before, e.g. falling back to a
/// bare flag). `Err` means a genuine failure — a [`PathError`]
/// (undefined-subscripted-root, a missing key, a shape mismatch), a bad
/// `$((...))` arithmetic expansion, or an unsupported `$(...)`/`$(cmd)` — and
/// MUST propagate loud, the same as the async `build_args_async`/
/// `eval_expr_async` (`kernel.rs`) and the sync interpreter (`eval.rs`) treat
/// it. Before this, every arm here discarded the error via
/// `.ok()`/`if let Ok(..)`, so a bad subscript OR a bad arithmetic expansion
/// in a scatter/gather flag value silently dropped the argument instead of
/// failing (docs/issues.md, now closed; the arithmetic swallow was GH #183).
pub(crate) fn eval_simple_expr(expr: &Expr, ctx: &ExecContext) -> Result<Option<Value>, String> {
    match expr {
        Expr::Literal(value) => Ok(Some(eval_literal(value, ctx))),
        Expr::VarRef(path) => match ctx.scope.resolve_path(path) {
            Ok(v) => Ok(Some(v)),
            // Unset BARE variable: coalesces (skip-the-arg) — this reduced
            // context's bash-compatible convention. Bare-only on purpose: an
            // undefined root under a SUBSCRIPTED path is loud below, the same
            // split `resolve_length` draws — `scatter --as ${x[key]}` with a
            // typo'd root must not silently drop the flag (kaibo review
            // finding, PR #85).
            Err(PathError::UndefinedRoot(_)) if path.segments.len() <= 1 => Ok(None),
            Err(PathError::UndefinedRoot(_)) => Err(format!(
                "{}: undefined variable",
                crate::interpreter::format_path(path)
            )),
            // A loud path error (absence or shape) carries its own actionable
            // message — never swallowed.
            Err(PathError::Absence(msg)) | Err(PathError::Shape(msg)) => Err(msg),
        },
        Expr::Interpolated(parts) => Ok(Some(Value::String(eval_string_parts_sync(parts, ctx)?))),
        // Bare (unquoted whole-token) forms — `scatter --limit ${#tags}`,
        // `scatter --as ${cfg[name]:-N}` — reuse the same shared path resolver
        // the async path calls (`eval_expr_async`'s `VarLength`/`VarWithDefault`
        // arms), so length/default semantics agree between the two paths.
        Expr::VarLength(path) => {
            crate::interpreter::resolve_length(&ctx.scope, path).map(|n| Some(Value::Int(n)))
        }
        Expr::VarWithDefault { path, default } => {
            match crate::interpreter::resolve_default(&ctx.scope, path)? {
                Some(value) => Ok(Some(value)),
                None => Ok(Some(Value::String(eval_string_parts_sync(default, ctx)?))),
            }
        }
        Expr::GlobPattern(s) => Ok(Some(Value::String(s.clone()))),
        // Bare arithmetic expansion (`scatter --limit $((1+1))`) — mirrors
        // the async `eval_expr_async`'s `Expr::Arithmetic` arm (kernel.rs),
        // which already propagates loud. This used to fall into the
        // catch-all `_ => Ok(None)` below (silently "not representable
        // here"), so a bare `$((...))` flag value never bound at all — a
        // valid `--limit $((1+1))` silently ran unlimited, and a bad
        // `--limit $((1/0))` silently did too, instead of failing (GH #183).
        Expr::Arithmetic(expr_str) => arithmetic::eval_arithmetic(expr_str, &ctx.scope)
            .map(|n| Some(Value::Int(n)))
            .map_err(|e| format!("arithmetic error: {e}")),
        Expr::HereDocBody { parts, strip_tabs } => {
            // Heredoc body materialization for redirect targets. `<<-` tab
            // stripping applies to the literal source, not to tabs from a
            // `$var` value — matching the interpreter's eval path.
            let mut asm = crate::interpreter::HeredocAssembler::new(*strip_tabs);
            for sp in parts {
                match &sp.part {
                    crate::ast::StringPart::Literal(s) => asm.push_literal(s),
                    other => {
                        let s = eval_string_parts_sync(std::slice::from_ref(other), ctx)?;
                        asm.push_interpolated(&s);
                    }
                }
            }
            Ok(Some(Value::String(asm.into_string())))
        }
        // Command substitution can't be evaluated here (this reduced sync
        // binder runs before any worker forks, so it can't recurse through
        // the async pipeline) — but that must fail loud, not silently
        // coalesce to a bare boolean flag/dropped value the way an unset
        // bare variable does. `scatter --limit $(echo 5)` used to silently
        // run at the default limit instead of erroring.
        Expr::CommandSubst(_) | Expr::Command(_) => Err(
            "command substitution `$(...)` is not supported in a scatter/gather flag value here; \
             assign it to a variable first (e.g. `n=$(...); scatter --limit $n`)"
                .to_string(),
        ),
        _ => Ok(None), // Binary ops need more context
    }
}

/// Evaluate a literal value.
fn eval_literal(value: &Value, _ctx: &ExecContext) -> Value {
    value.clone()
}

/// Evaluate string parts synchronously (for pipeline context).
///
/// Command substitutions are skipped as they require async. A [`PathError`]
/// (absence or shape) from a subscripted `$var`, `${…:-default}`, or `${#…}`
/// propagates loud via `Err` — matching the async `eval_string_part_async`
/// (`kernel.rs`) and the sync `Interpreter::eval_interpolated` (`eval.rs`).
/// An unset BARE root still expands to empty (bash-compatible), unchanged.
fn eval_string_parts_sync(parts: &[crate::ast::StringPart], ctx: &ExecContext) -> Result<String, String> {
    let mut result = String::new();
    for part in parts {
        match part {
            crate::ast::StringPart::Literal(s) => result.push_str(s),
            crate::ast::StringPart::Var(path) => match ctx.scope.resolve_path(path) {
                // Text sink: binary goes loud, never the `[binary: N bytes]`
                // placeholder — matches the async `eval_string_part_async`
                // (kernel.rs) and sync `eval_interpolated` (eval.rs).
                Ok(value) => result.push_str(
                    &crate::interpreter::value_to_text_sink(&value).map_err(|e| e.to_string())?,
                ),
                // Unconditional (even subscripted) on purpose: in STRING
                // context both primary sites — async `eval_string_part_async`
                // (kernel.rs) and sync `eval_interpolated` (eval.rs) — expand
                // an undefined root to empty, bash-compatibly ("a${nope[k]}b"
                // → "ab"). The bare-only restriction applies to the
                // whole-token `Expr::VarRef` arm above, matching the primary
                // sites' loud whole-token behavior.
                Err(PathError::UndefinedRoot(_)) => {}
                Err(PathError::Absence(msg)) | Err(PathError::Shape(msg)) => return Err(msg),
            },
            crate::ast::StringPart::VarWithDefault { path, default } => {
                match crate::interpreter::resolve_default(&ctx.scope, path)? {
                    Some(value) => result.push_str(
                        &crate::interpreter::value_to_text_sink(&value).map_err(|e| e.to_string())?,
                    ),
                    None => result.push_str(&eval_string_parts_sync(default, ctx)?),
                }
            }
            crate::ast::StringPart::VarLength(path) => {
                // Element/key count for collections, byte count for binary;
                // unset BARE root → 0 (bash parity). A shape/absence error on a
                // SUBSCRIPTED path now propagates loud instead of silently
                // omitting the length (the fixed "silent 0" gap).
                let len = crate::interpreter::resolve_length(&ctx.scope, path)?;
                result.push_str(&len.to_string());
            }
            crate::ast::StringPart::Positional(n) => {
                if let Some(s) = ctx.scope.get_positional(*n) {
                    result.push_str(s);
                }
            }
            crate::ast::StringPart::AllArgs => {
                result.push_str(&ctx.scope.all_args().join(" "));
            }
            crate::ast::StringPart::ArgCount => {
                result.push_str(&ctx.scope.arg_count().to_string());
            }
            crate::ast::StringPart::Arithmetic(expr) => {
                // Loud on purpose (GH #183): this used to be `if let Ok(..)`,
                // silently omitting the digits on error — a quoted
                // `--limit "$((1/0))"` used to surface only as scatter's own
                // generic int-parse complaint on the resulting "", masking
                // the real arithmetic error. Matches the bare
                // `Expr::Arithmetic` arm above and the async
                // `eval_string_part_async` (kernel.rs).
                let value = arithmetic::eval_arithmetic(expr, &ctx.scope)
                    .map_err(|e| format!("arithmetic error: {e}"))?;
                result.push_str(&value.to_string());
            }
            crate::ast::StringPart::CommandSubst(_) => {
                // Command substitution can't run in this reduced sync
                // context (see `eval_simple_expr`'s CommandSubst arm) — fail
                // loud instead of silently splicing in nothing.
                // `scatter --as "W$(suffix)"` used to bind the plain "W"
                // with the substitution silently dropped.
                return Err(
                    "command substitution `$(...)` is not supported inside a scatter/gather \
                     flag's interpolated value here; assign it to a variable first"
                        .to_string(),
                );
            }
            crate::ast::StringPart::LastExitCode => {
                result.push_str(&ctx.scope.last_result().code.to_string());
            }
            crate::ast::StringPart::CurrentPid => {
                result.push_str(&ctx.scope.pid().to_string());
            }
        }
    }
    Ok(result)
}

/// Find scatter and gather commands in a pipeline.
///
/// Returns Some((scatter_index, gather_index)) if both are found with scatter before gather.
/// Returns None if the pipeline doesn't have a valid scatter/gather pattern.
fn find_scatter_gather(commands: &[Command]) -> Option<(usize, usize)> {
    let scatter_idx = commands.iter().position(|c| c.name == "scatter")?;
    let gather_idx = commands.iter().position(|c| c.name == "gather")?;

    // Gather must come after scatter
    if gather_idx > scatter_idx {
        Some((scatter_idx, gather_idx))
    } else {
        None
    }
}

#[cfg(test)]
mod select_leaf_tests {
    use super::*;
    use crate::tools::ParamSchema;

    /// `kj`-shaped tree: kj → context (alias ctx) → {list (alias ls), create}.
    /// Root carries a global `--confirm <nonce>` value flag and a `--verbose`
    /// bool; `create` carries a leaf `--type` value flag — enough to exercise
    /// global-flag skipping and leaf binding.
    fn kj_schema() -> ToolSchema {
        ToolSchema::new("kj", "kaijutsu")
            .param(ParamSchema::new("confirm", "string"))
            .param(ParamSchema::new("verbose", "bool"))
            .subcommand(
                ToolSchema::new("context", "context ops")
                    .with_command_aliases(["ctx"])
                    .subcommand(ToolSchema::new("list", "list").with_command_aliases(["ls"]))
                    .subcommand(
                        ToolSchema::new("create", "create").param(
                            ParamSchema::new("type", "string").with_aliases(["t"]),
                        ),
                    ),
            )
    }

    fn word(s: &str) -> Arg {
        Arg::Positional(Expr::Literal(Value::String(s.to_string())))
    }

    #[test]
    fn flat_tool_returns_root() {
        let schema = ToolSchema::new("cat", "concat")
            .param(ParamSchema::required("path", "string", "f").positional());
        let leaf = select_leaf(&schema, &[word("foo.txt")]).expect("flat ok");
        assert_eq!(leaf.name, "cat");
    }

    #[test]
    fn single_hop() {
        let schema = kj_schema();
        let leaf = select_leaf(&schema, &[word("context")]).expect("ok");
        assert_eq!(leaf.name, "context");
    }

    #[test]
    fn two_hops() {
        let schema = kj_schema();
        let leaf = select_leaf(&schema, &[word("context"), word("create")]).expect("ok");
        assert_eq!(leaf.name, "create");
        assert!(leaf.params.iter().any(|p| p.name == "type"), "leaf has --type");
    }

    #[test]
    fn alias_hops_route() {
        let schema = kj_schema();
        // `kj ctx ls` → context.list via command aliases.
        let leaf = select_leaf(&schema, &[word("ctx"), word("ls")]).expect("ok");
        assert_eq!(leaf.name, "list");
    }

    #[test]
    fn unknown_subcommand_stops_at_current_node() {
        let schema = kj_schema();
        // `context nonesuch` — `nonesuch` names no child, so context is the leaf
        // and `nonesuch` is context's own positional. No error.
        let leaf = select_leaf(&schema, &[word("context"), word("nonesuch")]).expect("ok");
        assert_eq!(leaf.name, "context");
    }

    #[test]
    fn root_bool_flag_before_path_does_not_disrupt_routing() {
        let schema = kj_schema();
        // `kj --verbose context create` — a root bool flag is skipped, both
        // positionals route to create.
        let args = vec![Arg::LongFlag("verbose".into()), word("context"), word("create")];
        let leaf = select_leaf(&schema, &args).expect("ok");
        assert_eq!(leaf.name, "create");
    }

    #[test]
    fn root_value_flag_space_form_before_path_skips_its_value() {
        let schema = kj_schema();
        // `kj --confirm nonce context create` — `nonce` is --confirm's value,
        // NOT a subcommand selector; routing skips it and reaches create.
        let args = vec![
            Arg::LongFlag("confirm".into()),
            word("nonce"),
            word("context"),
            word("create"),
        ];
        let leaf = select_leaf(&schema, &args).expect("ok");
        assert_eq!(leaf.name, "create");
    }

    #[test]
    fn leaf_value_flag_after_path_routes_to_leaf() {
        let schema = kj_schema();
        // `kj context create --type x` — the natural form: path first, leaf flag
        // after. Routing reaches create; --type then binds against create.
        let args = vec![
            word("context"),
            word("create"),
            Arg::LongFlag("type".into()),
            word("x"),
        ];
        let leaf = select_leaf(&schema, &args).expect("ok");
        assert_eq!(leaf.name, "create");
        assert!(leaf.params.iter().any(|p| p.name == "type"));
    }

    #[test]
    fn double_dash_stops_routing() {
        let schema = kj_schema();
        // `kj -- context` — after `--`, `context` is raw data, not a subcommand.
        let leaf = select_leaf(&schema, &[Arg::DoubleDash, word("context")]).expect("ok");
        assert_eq!(leaf.name, "kj");
    }

    #[test]
    fn computed_subcommand_selector_errors() {
        let schema = kj_schema();
        // `kj $(echo context)` — a command substitution where a subcommand name
        // is required must fail loud, not silently pick a leaf.
        let args = vec![Arg::Positional(Expr::CommandSubst(vec![
            crate::ast::Stmt::Command(crate::ast::Command {
                name: "echo".into(),
                args: vec![],
                redirects: vec![],
            }),
        ]))];
        let err = select_leaf(&schema, &args).expect_err("must error");
        let msg = err.to_string();
        assert!(msg.contains("subcommand name is required"), "got: {msg}");
        assert!(msg.contains("command substitution"), "names the cause: {msg}");
    }

    #[test]
    fn variable_subcommand_selector_errors() {
        let schema = kj_schema();
        let args = vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("sub")))];
        let err = select_leaf(&schema, &args).expect_err("must error");
        assert!(err.to_string().contains("variable reference"), "got: {err}");
    }

    #[test]
    fn computed_positional_after_leaf_is_fine() {
        let schema = kj_schema();
        // `kj context list $(echo x)` — once at a leaf (list has no children),
        // a computed positional is just an argument; routing already stopped.
        let args = vec![
            word("context"),
            word("list"),
            Arg::Positional(Expr::CommandSubst(vec![crate::ast::Stmt::Command(
                crate::ast::Command { name: "echo".into(), args: vec![], redirects: vec![] },
            )])),
        ];
        let leaf = select_leaf(&schema, &args).expect("ok");
        assert_eq!(leaf.name, "list");
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dispatch::BackendDispatcher;
    use crate::tools::register_builtins;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::path::Path;

    async fn make_runner_and_ctx() -> (PipelineRunner, ExecContext, BackendDispatcher) {
        let mut tools = ToolRegistry::new();
        register_builtins(&mut tools);
        let tools = Arc::new(tools);
        let runner = PipelineRunner::new(tools.clone());
        let dispatcher = BackendDispatcher::new(tools.clone());

        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("test.txt"), b"hello\nworld\nfoo").await.unwrap();
        vfs.mount("/", mem);
        let ctx = ExecContext::with_vfs_and_tools(Arc::new(vfs), tools);

        (runner, ctx, dispatcher)
    }

    fn make_cmd(name: &str, args: Vec<&str>) -> Command {
        Command {
            name: name.to_string(),
            args: args.iter().map(|s| Arg::Positional(Expr::Literal(Value::String(s.to_string())))).collect(),
            redirects: vec![],
        }
    }

    #[tokio::test]
    async fn test_single_command() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;
        let cmd = make_cmd("echo", vec!["hello"]);

        let result = runner.run(&[cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "hello");
    }

    #[tokio::test]
    async fn test_pipeline_echo_grep() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "hello\nworld" | grep pattern="world"
        let echo_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello\nworld".to_string())))],
            redirects: vec![],
        };
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("world".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[echo_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "world");
    }

    #[tokio::test]
    async fn test_pipeline_cat_grep() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // cat /test.txt | grep pattern="hello"
        let cat_cmd = make_cmd("cat", vec!["/test.txt"]);
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[cat_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert!(result.text_out().contains("hello"));
    }

    #[tokio::test]
    async fn test_command_not_found() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;
        let cmd = make_cmd("nonexistent", vec![]);

        let result = runner.run(&[cmd], &mut ctx, &dispatcher).await;
        assert!(!result.ok());
        assert_eq!(result.code, 127);
        assert!(result.err.contains("not found"));
    }

    #[tokio::test]
    async fn test_pipeline_continues_on_failure() {
        // Standard shell semantics: pipeline runs all commands,
        // exit code comes from the last command
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // cat /nonexistent | grep "hello"
        // cat fails but grep still runs (on empty input), grep returns 1 (no match)
        let cat_cmd = make_cmd("cat", vec!["/nonexistent"]);
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[cat_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        // Exit code comes from last command (grep), not from cat
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_pipeline_last_command_exit_code() {
        // echo hello | cat — both succeed, pipeline succeeds
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        let echo_cmd = make_cmd("echo", vec!["hello"]);
        let cat_cmd = make_cmd("cat", vec![]);

        let result = runner.run(&[echo_cmd, cat_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert!(result.text_out().contains("hello"));
    }

    #[tokio::test]
    async fn test_empty_pipeline() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;
        let result = runner.run(&[], &mut ctx, &dispatcher).await;
        assert!(result.ok());
    }

    // === Scatter/Gather Tests ===

    #[test]
    fn test_find_scatter_gather_both_present() {
        let commands = vec![
            make_cmd("echo", vec!["a"]),
            make_cmd("scatter", vec![]),
            make_cmd("process", vec![]),
            make_cmd("gather", vec![]),
        ];
        let result = find_scatter_gather(&commands);
        assert_eq!(result, Some((1, 3)));
    }

    #[test]
    fn test_find_scatter_gather_no_scatter() {
        let commands = vec![
            make_cmd("echo", vec!["a"]),
            make_cmd("gather", vec![]),
        ];
        let result = find_scatter_gather(&commands);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_scatter_gather_no_gather() {
        let commands = vec![
            make_cmd("echo", vec!["a"]),
            make_cmd("scatter", vec![]),
        ];
        let result = find_scatter_gather(&commands);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_scatter_gather_wrong_order() {
        let commands = vec![
            make_cmd("gather", vec![]),
            make_cmd("scatter", vec![]),
        ];
        let result = find_scatter_gather(&commands);
        assert!(result.is_none());
    }

    #[tokio::test]
    async fn test_scatter_gather_simple() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // split "a b c" | scatter | echo ${ITEM} | gather
        let split_cmd = Command {
            name: "split".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("a b c".to_string())))],
            redirects: vec![],
        };
        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[split_cmd, scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok(), "scatter with structured data should succeed: {}", result.err);
        // Each echo should output the item
        assert!(result.text_out().contains("a"));
        assert!(result.text_out().contains("b"));
        assert!(result.text_out().contains("c"));
    }

    #[tokio::test]
    async fn test_scatter_gather_empty_input() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "" | scatter | echo ${ITEM} | gather
        let echo_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("".to_string())))],
            redirects: vec![],
        };
        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[echo_cmd, scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert!(result.text_out().trim().is_empty());
    }

    #[tokio::test]
    async fn test_scatter_gather_with_structured_stdin() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // Set structured stdin data (as if piped from split/seq)
        let data = Value::Json(serde_json::json!(["x", "y", "z"]));
        ctx.set_stdin_with_data("x\ny\nz".to_string(), Some(data));

        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok(), "scatter with structured stdin should succeed: {}", result.err);
        assert!(result.text_out().contains("x"));
        assert!(result.text_out().contains("y"));
        assert!(result.text_out().contains("z"));
    }

    #[tokio::test]
    async fn test_scatter_gather_json_input() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // Structured JSON array input (as if from split/seq)
        let data = Value::Json(serde_json::json!(["one", "two", "three"]));
        ctx.set_stdin_with_data(r#"["one", "two", "three"]"#.to_string(), Some(data));

        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok(), "scatter with JSON data should succeed: {}", result.err);
        assert!(result.text_out().contains("one"));
        assert!(result.text_out().contains("two"));
        assert!(result.text_out().contains("three"));
    }

    #[tokio::test]
    async fn test_scatter_gather_with_post_gather() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // split "a b" | scatter | echo ${ITEM} | gather | grep "a"
        let split_cmd = Command {
            name: "split".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("a b".to_string())))],
            redirects: vec![],
        };
        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("a".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[split_cmd, scatter_cmd, process_cmd, gather_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok(), "scatter with post_gather should succeed: {}", result.err);
        assert!(result.text_out().contains("a"));
        assert!(!result.text_out().contains("b"));
    }

    #[tokio::test]
    async fn test_scatter_custom_var_name() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // Provide structured data (as if from split/seq)
        let data = Value::Json(serde_json::json!(["test1", "test2"]));
        ctx.set_stdin_with_data("test1\ntest2".to_string(), Some(data));

        // scatter --as URL | echo ${URL} | gather
        let scatter_cmd = Command {
            name: "scatter".to_string(),
            args: vec![Arg::Named {
                key: "as".to_string(),
                value: Expr::Literal(Value::String("URL".to_string())),
            }],
            redirects: vec![],
        };
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("URL")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok(), "scatter with custom var should succeed: {}", result.err);
        assert!(result.text_out().contains("test1"));
        assert!(result.text_out().contains("test2"));
    }

    // === Backend Routing Tests ===

    #[tokio::test]
    async fn test_pipeline_routes_through_backend() {
        use crate::backend::testing::MockBackend;
        use std::sync::atomic::Ordering;

        // Create mock backend
        let (backend, call_count) = MockBackend::new();
        let backend: std::sync::Arc<dyn crate::backend::KernelBackend> = std::sync::Arc::new(backend);

        // Create context with mock backend
        let mut ctx = crate::tools::ExecContext::with_backend(backend);

        // BackendDispatcher routes through backend.call_tool()
        let tools = std::sync::Arc::new(ToolRegistry::new());
        let runner = PipelineRunner::new(tools.clone());
        let dispatcher = BackendDispatcher::new(tools);

        // Single command should route through backend
        let cmd = make_cmd("test-tool", vec!["arg1"]);
        let result = runner.run(&[cmd], &mut ctx, &dispatcher).await;

        assert!(result.ok(), "Mock backend should return success");
        assert_eq!(call_count.load(Ordering::SeqCst), 1, "call_tool should be invoked once");
        assert!(result.text_out().contains("mock executed"), "Output should be from mock backend");
    }

    #[tokio::test]
    async fn test_multi_command_pipeline_routes_through_backend() {
        use crate::backend::testing::MockBackend;
        use std::sync::atomic::Ordering;

        let (backend, call_count) = MockBackend::new();
        let backend: std::sync::Arc<dyn crate::backend::KernelBackend> = std::sync::Arc::new(backend);
        let mut ctx = crate::tools::ExecContext::with_backend(backend);

        let tools = std::sync::Arc::new(ToolRegistry::new());
        let runner = PipelineRunner::new(tools.clone());
        let dispatcher = BackendDispatcher::new(tools);

        // Pipeline with 3 commands
        let cmd1 = make_cmd("tool1", vec![]);
        let cmd2 = make_cmd("tool2", vec![]);
        let cmd3 = make_cmd("tool3", vec![]);

        let result = runner.run(&[cmd1, cmd2, cmd3], &mut ctx, &dispatcher).await;

        assert!(result.ok());
        assert_eq!(call_count.load(Ordering::SeqCst), 3, "call_tool should be invoked for each command");
    }

    /// GH #93 item 4: the test-only `BackendDispatcher` used to hand-roll the
    /// `ToolResult` -> `ExecResult` conversion (wrapping `data` unconditionally
    /// as `Value::Json`), diverging from the production path in kernel.rs,
    /// which goes through `ExecResult::from(tool_result)` and unwraps JSON
    /// scalars into native `Value` variants via `json_to_value_no_envelope`.
    /// A scalar `data` payload is where the two paths visibly disagreed.
    #[tokio::test]
    async fn backend_dispatcher_scalar_data_matches_production_unwrap() {
        use crate::backend::testing::MockBackend;
        use crate::backend::ToolResult;

        let (mock, _calls) = MockBackend::new();
        let backend = mock.with_tool_result(|_name| Ok(ToolResult::with_data("", serde_json::json!(42))));
        let backend: Arc<dyn crate::backend::KernelBackend> = Arc::new(backend);
        let mut ctx = ExecContext::with_backend(backend);

        let dispatcher = BackendDispatcher::new(Arc::new(ToolRegistry::new()));
        let cmd = make_cmd("embedder_tool", vec![]);

        let result = dispatcher.dispatch(&cmd, &mut ctx).await.expect("dispatch");
        assert_eq!(
            result.data,
            Some(Value::Int(42)),
            "a scalar ToolResult.data must unwrap to a native Value, matching \
             the production From<ToolResult> path — not stay Value::Json(42)"
        );
    }

    /// Companion to the scalar test above: an object shaped like the binary
    /// byte-envelope must stay a plain structured record (`Value::Json`), not
    /// get auto-decoded into `Value::Bytes`. Pins the same guarantee
    /// `json_to_value_no_envelope` gives the production path, now that the
    /// test dispatcher shares that exact conversion.
    #[tokio::test]
    async fn backend_dispatcher_envelope_shaped_data_stays_structured() {
        use crate::backend::testing::MockBackend;
        use crate::backend::ToolResult;

        let envelope = kaish_types::bytes_to_envelope(&[1u8, 2, 3]);
        let (mock, _calls) = MockBackend::new();
        let backend = mock.with_tool_result(move |_name| Ok(ToolResult::with_data("", envelope.clone())));
        let backend: Arc<dyn crate::backend::KernelBackend> = Arc::new(backend);
        let mut ctx = ExecContext::with_backend(backend);

        let dispatcher = BackendDispatcher::new(Arc::new(ToolRegistry::new()));
        let cmd = make_cmd("embedder_tool", vec![]);

        let result = dispatcher.dispatch(&cmd, &mut ctx).await.expect("dispatch");
        assert!(
            matches!(result.data, Some(Value::Json(_))),
            "envelope-shaped external data must stay structured, not silently \
             decode to Value::Bytes: got {:?}",
            result.data
        );
    }

    /// GH #93 item 3: `did_spill`/`original_code` must survive the
    /// ToolResult <-> ExecResult seam. The old hand-rolled conversion in the
    /// test dispatcher never touched either field, so a capped backend-tool
    /// result silently looked uncapped by the time it reached the kernel.
    #[tokio::test]
    async fn backend_dispatcher_preserves_did_spill_and_original_code() {
        use crate::backend::testing::MockBackend;
        use crate::backend::ToolResult;

        let (mock, _calls) = MockBackend::new();
        let backend = mock.with_tool_result(|_name| {
            Ok(ToolResult::success("truncated...")
                .with_did_spill(true)
                .with_original_code(Some(0)))
        });
        let backend: Arc<dyn crate::backend::KernelBackend> = Arc::new(backend);
        let mut ctx = ExecContext::with_backend(backend);

        let dispatcher = BackendDispatcher::new(Arc::new(ToolRegistry::new()));
        let cmd = make_cmd("embedder_tool", vec![]);

        let result = dispatcher.dispatch(&cmd, &mut ctx).await.expect("dispatch");
        assert!(result.did_spill, "did_spill must survive the backend seam");
        assert_eq!(result.original_code, Some(0), "original_code must survive the backend seam");
    }

    // === Schema-Aware Argument Parsing Tests ===

    use crate::tools::{ParamSchema, ToolSchema};

    fn make_test_schema() -> ToolSchema {
        ToolSchema::new("test-tool", "A test tool for schema-aware parsing")
            .param(ParamSchema::required("query", "string", "Search query"))
            .param(ParamSchema::optional("limit", "int", Value::Int(10), "Max results"))
            .param(ParamSchema::optional("verbose", "bool", Value::Bool(false), "Verbose output"))
            .param(ParamSchema::optional("output", "string", Value::String("stdout".into()), "Output destination"))
            .with_positional_mapping()
    }

    fn make_minimal_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    /// A throwaway dispatcher for `apply_redirects` in tests that exercise
    /// merge redirects (`2>&1`) only — they never evaluate a `$()` target, so
    /// an empty-registry backend dispatcher suffices to satisfy the signature.
    fn test_dispatcher() -> BackendDispatcher {
        BackendDispatcher::new(Arc::new(ToolRegistry::new()))
    }

    #[tokio::test]
    async fn test_schema_aware_string_arg() {
        // --query "test" should become named: {"query": "test"}
        let args = vec![
            Arg::LongFlag("query".to_string()),
            Arg::Positional(Expr::Literal(Value::String("test".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert!(tool_args.flags.is_empty(), "No flags should be set");
        assert!(tool_args.positional.is_empty(), "No positionals - consumed by --query");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("test".to_string())),
            "--query should consume 'test' as its value"
        );
    }

    #[tokio::test]
    async fn test_schema_aware_bool_flag() {
        // --verbose should remain a flag since schema says bool
        let args = vec![
            Arg::LongFlag("verbose".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert!(tool_args.flags.contains("verbose"), "--verbose should be a flag");
        assert!(tool_args.named.is_empty(), "No named args");
        assert!(tool_args.positional.is_empty(), "No positionals");
    }

    #[tokio::test]
    async fn test_schema_aware_mixed() {
        // mcp_tool file.txt --output out.txt --verbose
        // file.txt maps to "query" (first unfilled non-bool schema param)
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("file.txt".to_string()))),
            Arg::LongFlag("output".to_string()),
            Arg::Positional(Expr::Literal(Value::String("out.txt".to_string()))),
            Arg::LongFlag("verbose".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert!(tool_args.positional.is_empty(), "file.txt consumed as query param");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("file.txt".to_string()))
        );
        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("out.txt".to_string()))
        );
        assert!(tool_args.flags.contains("verbose"));
    }

    #[tokio::test]
    async fn test_schema_aware_multiple_string_args() {
        // --query "test" --output "result.json" --verbose --limit 5
        let args = vec![
            Arg::LongFlag("query".to_string()),
            Arg::Positional(Expr::Literal(Value::String("test".to_string()))),
            Arg::LongFlag("output".to_string()),
            Arg::Positional(Expr::Literal(Value::String("result.json".to_string()))),
            Arg::LongFlag("verbose".to_string()),
            Arg::LongFlag("limit".to_string()),
            Arg::Positional(Expr::Literal(Value::Int(5))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert!(tool_args.positional.is_empty(), "All positionals consumed");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("test".to_string()))
        );
        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("result.json".to_string()))
        );
        assert_eq!(
            tool_args.named.get("limit"),
            Some(&Value::Int(5))
        );
        assert!(tool_args.flags.contains("verbose"));
    }

    #[tokio::test]
    async fn test_schema_aware_double_dash() {
        // --output out.txt -- --this-is-data
        // After --, everything is positional
        let args = vec![
            Arg::LongFlag("output".to_string()),
            Arg::Positional(Expr::Literal(Value::String("out.txt".to_string()))),
            Arg::DoubleDash,
            Arg::Positional(Expr::Literal(Value::String("--this-is-data".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("out.txt".to_string()))
        );
        // After --, the --this-is-data is treated as a positional (it's a Positional in the args)
        assert_eq!(
            tool_args.positional,
            vec![Value::String("--this-is-data".to_string())]
        );
    }

    /// GH #116: the sync twin of kernel.rs's async `build_args_async` WordAssign
    /// fallback must also go loud on binary rather than silently reassembling
    /// the `[binary: N bytes]` placeholder into `key=value` (e.g. `dd if=$BIN`
    /// reached via a scatter/gather flag value, which routes through this sync
    /// evaluator instead of the async binder).
    #[tokio::test]
    async fn word_assign_binary_value_is_loud_not_placeholder() {
        let args = vec![Arg::WordAssign {
            key: "if".to_string(),
            value: Expr::Literal(Value::Bytes(vec![0xff, 0x00, 0xfe])),
        }];
        let ctx = make_minimal_ctx();

        // schema=None ⇒ accepts_word_assign is false ⇒ falls to the
        // stringify-to-positional branch under test.
        let err = build_tool_args(&args, &ctx, None).await.expect_err("binary WordAssign must error");
        assert!(
            err.contains("cannot be used as"),
            "error should name the binary problem, got {err:?}"
        );
    }

    /// GH #189 item 1: pin the CURRENT (pre-`--`) behavior first — a
    /// word-assign-accepting tool (`export`, keyed off the root schema name)
    /// binds a bare `key=value` as a named assignment. This is unchanged by
    /// the fix below; only the post-`--` case changes.
    #[tokio::test]
    async fn word_assign_before_double_dash_binds_named_for_export() {
        let args = vec![Arg::WordAssign {
            key: "A".to_string(),
            value: Expr::Literal(Value::String("1".to_string())),
        }];
        let schema = ToolSchema::new("export", "export");
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");
        assert_eq!(tool_args.named.get("A"), Some(&Value::String("1".to_string())));
        assert!(tool_args.positional.is_empty());
    }

    /// GH #189 item 1: `export -- A=1` must NOT bind `A=1` as a named
    /// assignment — `--` marks everything after it as literal data, and the
    /// WordAssign arm used to ignore `past_double_dash` entirely (only the
    /// flag arms checked it). Before the fix, this test's ONLY visible
    /// difference from the one above was replacing `WordAssign` with
    /// `[DoubleDash, WordAssign]` — the fix degrades the value to a
    /// stringified `"A=1"` positional instead, matching how every other
    /// tool treats a `key=value` after `--`.
    #[tokio::test]
    async fn word_assign_after_double_dash_is_positional_even_for_export() {
        let args = vec![
            Arg::DoubleDash,
            Arg::WordAssign {
                key: "A".to_string(),
                value: Expr::Literal(Value::String("1".to_string())),
            },
        ];
        let schema = ToolSchema::new("export", "export");
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");
        assert!(
            tool_args.named.is_empty(),
            "past `--`, A=1 must not become a named assignment: {:?}",
            tool_args.named
        );
        assert_eq!(tool_args.positional, vec![Value::String("A=1".to_string())]);
    }

    /// GH #189 item 3: `--flag=true` on an UNDECLARED flag (this is exactly
    /// `--json`'s situation — `clap_schema::is_skipped` deliberately excludes
    /// it from every builtin's reflected schema) must flagify at bind time:
    /// land in `flags`, not `named` as a literal `Value::Bool` a clap `bool`
    /// field's `SetTrue` action rejects (`seq --json=true` used to exit 2).
    /// Before this fix, only the ~20 builtins that called
    /// `ToolArgs::flagify_bool_named` themselves got this normalization.
    #[tokio::test]
    async fn named_true_on_undeclared_flag_flagifies() {
        let args = vec![Arg::Named {
            key: "json".to_string(),
            value: Expr::Literal(Value::Bool(true)),
        }];
        let schema = make_test_schema(); // declares query/limit/verbose/output, not "json"
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");
        assert!(tool_args.flags.contains("json"), "flags: {:?}", tool_args.flags);
        assert!(!tool_args.named.contains_key("json"), "named: {:?}", tool_args.named);
    }

    /// `--flag=false` on an undeclared flag drops entirely — absence and
    /// explicit false are the same thing, matching `ToolArgs::flagify_bool_named`.
    #[tokio::test]
    async fn named_false_on_undeclared_flag_drops() {
        let args = vec![Arg::Named {
            key: "json".to_string(),
            value: Expr::Literal(Value::Bool(false)),
        }];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");
        assert!(!tool_args.flags.contains("json"));
        assert!(!tool_args.named.contains_key("json"));
    }

    /// A schema-DECLARED bool param (`verbose`) behaves the same as an
    /// undeclared one: `--verbose=true` flagifies instead of landing in
    /// `named`.
    #[tokio::test]
    async fn named_true_on_declared_bool_param_flagifies() {
        let args = vec![Arg::Named {
            key: "verbose".to_string(),
            value: Expr::Literal(Value::Bool(true)),
        }];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");
        assert!(tool_args.flags.contains("verbose"));
        assert!(!tool_args.named.contains_key("verbose"));
    }

    /// A schema-declared VALUE-taking flag's own `=true` literal
    /// (`spawn --command=true`, `output` here — both string-typed in
    /// `make_test_schema`) must NOT flagify — `true` is the flag's actual
    /// value, not a bool-flag presence marker, and clap's `Option<String>`
    /// field for it accepts `--output=true` fine.
    #[tokio::test]
    async fn named_true_on_declared_value_flag_keeps_value() {
        let args = vec![Arg::Named {
            key: "output".to_string(),
            value: Expr::Literal(Value::Bool(true)),
        }];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");
        assert_eq!(tool_args.named.get("output"), Some(&Value::Bool(true)));
        assert!(!tool_args.flags.contains("output"));
    }

    /// The fix must not depend on a schema being present at all — a
    /// completely schemaless invocation (`schema=None`, e.g. a shell
    /// function call) sees an empty `param_lookup`, so `--flag=true` still
    /// flagifies rather than landing in `named`.
    #[tokio::test]
    async fn named_true_with_no_schema_flagifies() {
        let args = vec![Arg::Named {
            key: "verbose".to_string(),
            value: Expr::Literal(Value::Bool(true)),
        }];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, None).await.expect("build_tool_args");
        assert!(tool_args.flags.contains("verbose"));
        assert!(!tool_args.named.contains_key("verbose"));
    }

    #[tokio::test]
    async fn test_no_schema_fallback() {
        // Without schema, all --flags are treated as bool flags
        let args = vec![
            Arg::LongFlag("query".to_string()),
            Arg::Positional(Expr::Literal(Value::String("test".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, None).await.expect("build_tool_args");

        // Without schema, --query is a flag and "test" is a positional
        assert!(tool_args.flags.contains("query"), "--query should be a flag");
        assert_eq!(
            tool_args.positional,
            vec![Value::String("test".to_string())],
            "'test' should be a positional"
        );
    }

    /// GH #188: `--unknown value` under a `map_positionals` schema (real
    /// MCP/backend tools) is ambiguous — kaish can't tell an undeclared
    /// flag's space-form value from a bool flag sitting before a genuine
    /// positional. The pre-#188 reduced sync twin silently defaulted
    /// `--unknown` to a bool flag and mapped "value" onto the first unfilled
    /// param (`query`) instead — exactly the "no undeclared-space-flag
    /// guard" divergence from `Kernel::build_args_async`'s real behavior
    /// that unifying the two binders closes. Production's ambiguous-value
    /// guard (`kernel::bind_tool_args`) now fires here too.
    #[tokio::test]
    async fn test_unknown_flag_ambiguous_space_value_now_errors_loud() {
        let args = vec![
            Arg::LongFlag("unknown".to_string()),
            Arg::Positional(Expr::Literal(Value::String("value".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let err = build_tool_args(&args, &ctx, Some(&schema))
            .await
            .expect_err("an undeclared flag immediately before a positional must be ambiguous, not silently bool");
        assert!(
            err.contains("--unknown is not a declared flag"),
            "got: {err}"
        );
    }

    /// The unambiguous half of the same guard: an undeclared flag with
    /// nothing after it can't be silently swallowing a positional, so it
    /// still defaults to a bare bool flag — unchanged by GH #188.
    #[tokio::test]
    async fn test_unknown_bool_flag_with_no_following_positional_is_fine() {
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("value".to_string()))),
            Arg::LongFlag("unknown".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert!(tool_args.flags.contains("unknown"));
        assert!(tool_args.positional.is_empty(), "value consumed as query param");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("value".to_string()))
        );
    }

    /// GH #189 item 4: the SAME ambiguity guard as
    /// `test_unknown_flag_ambiguous_space_value_now_errors_loud` above, but
    /// for an undeclared SHORT flag. Before the fix, an undeclared short
    /// flag under a `map_positionals` schema always defaulted to a bare bool
    /// (`is_bool = lookup.map(...).unwrap_or(true)`), silently divorcing the
    /// following positional's value (`-t explorer` → flag "t" set, "explorer"
    /// mapped onto the first unfilled param instead of "t"'s value) — the
    /// long-flag half of this was closed by GH #188; this closes the
    /// short-flag half.
    #[tokio::test]
    async fn test_unknown_short_flag_ambiguous_space_value_now_errors_loud() {
        let args = vec![
            Arg::ShortFlag("t".to_string()),
            Arg::Positional(Expr::Literal(Value::String("value".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let err = build_tool_args(&args, &ctx, Some(&schema))
            .await
            .expect_err("an undeclared short flag immediately before a positional must be ambiguous, not silently bool");
        assert!(
            err.contains("-t is not a declared flag"),
            "got: {err}"
        );
    }

    /// The unambiguous half: an undeclared short flag with nothing after it
    /// can't be silently swallowing a positional, so it still defaults to a
    /// bare bool flag.
    #[tokio::test]
    async fn test_unknown_short_bool_flag_with_no_following_positional_is_fine() {
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("value".to_string()))),
            Arg::ShortFlag("t".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert!(tool_args.flags.contains("t"));
        assert!(tool_args.positional.is_empty(), "value consumed as query param");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("value".to_string()))
        );
    }

    /// The guard is specific to `map_positionals` (backend/MCP) schemas — a
    /// builtin (no `map_positionals`) keeps the pre-existing behavior of
    /// treating an undeclared short flag as bare bool, since a builtin
    /// handles its own positionals rather than relying on this ambiguity
    /// class at all.
    #[tokio::test]
    async fn test_unknown_short_flag_not_ambiguous_without_map_positionals() {
        let args = vec![
            Arg::ShortFlag("t".to_string()),
            Arg::Positional(Expr::Literal(Value::String("value".to_string()))),
        ];
        // A builtin-shaped schema: same params as make_test_schema but no
        // positional mapping.
        let schema = ToolSchema::new("test-tool", "A test tool")
            .param(ParamSchema::required("query", "string", "Search query"));
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");
        assert!(tool_args.flags.contains("t"));
        assert_eq!(tool_args.positional, vec![Value::String("value".to_string())]);
    }

    #[tokio::test]
    async fn test_named_args_unchanged() {
        // key=value syntax should work regardless of schema
        let args = vec![
            Arg::Named {
                key: "query".to_string(),
                value: Expr::Literal(Value::String("test".to_string())),
            },
            Arg::LongFlag("verbose".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("test".to_string()))
        );
        assert!(tool_args.flags.contains("verbose"));
    }

    #[tokio::test]
    async fn test_short_flags_unchanged() {
        // Short flags -la should expand regardless of schema; file.txt maps to query
        let args = vec![
            Arg::ShortFlag("la".to_string()),
            Arg::Positional(Expr::Literal(Value::String("file.txt".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert!(tool_args.flags.contains("l"));
        assert!(tool_args.flags.contains("a"));
        assert!(tool_args.positional.is_empty(), "file.txt consumed as query param");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("file.txt".to_string()))
        );
    }

    #[tokio::test]
    async fn test_flag_at_end_no_value() {
        // --output at end with no value available - treat as flag (lenient)
        // file.txt maps to query (first unfilled non-bool param)
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("file.txt".to_string()))),
            Arg::LongFlag("output".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        // output expects a value but none available after it, so it becomes a flag
        assert!(tool_args.flags.contains("output"));
        assert!(tool_args.positional.is_empty(), "file.txt consumed as query param");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("file.txt".to_string()))
        );
    }

    #[tokio::test]
    async fn test_positional_skips_bool_params() {
        // Schema: [query: string, verbose: bool, output: string]
        // Args: "val1" "val2"
        // Expected: query="val1", verbose unset, output="val2"
        let schema = ToolSchema::new("test", "")
            .param(ParamSchema::required("query", "string", ""))
            .param(ParamSchema::optional(
                "verbose",
                "bool",
                Value::Bool(false),
                "",
            ))
            .param(ParamSchema::optional(
                "output",
                "string",
                Value::Null,
                "",
            ))
            .with_positional_mapping();
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("val1".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("val2".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("val1".to_string()))
        );
        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("val2".to_string()))
        );
        assert!(!tool_args.flags.contains("verbose"));
        assert!(tool_args.positional.is_empty());
    }

    #[tokio::test]
    async fn test_positionals_fill_available_slots() {
        // Schema has query (string), limit (int), verbose (bool), output (string).
        // Three positionals fill the 3 non-bool slots.
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("val1".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("val2".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("val3".to_string()))),
        ];
        let schema = make_test_schema(); // query, limit(int), verbose(bool), output
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        // val1 → query, val2 → limit (int param but receives string — tool decides),
        // val3 → output
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("val1".to_string()))
        );
        assert_eq!(
            tool_args.named.get("limit"),
            Some(&Value::String("val2".to_string()))
        );
        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("val3".to_string()))
        );
        assert!(tool_args.positional.is_empty());
    }

    #[tokio::test]
    async fn test_truly_excess_positionals() {
        // More positionals than non-bool schema params — leftovers stay positional
        let schema = ToolSchema::new("test", "")
            .param(ParamSchema::required("name", "string", ""))
            .with_positional_mapping();
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("first".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("second".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("third".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("name"),
            Some(&Value::String("first".to_string()))
        );
        assert_eq!(
            tool_args.positional,
            vec![
                Value::String("second".to_string()),
                Value::String("third".to_string()),
            ]
        );
    }

    #[tokio::test]
    async fn test_double_dash_positional_not_mapped() {
        // `tool val1 -- val2` — val1 maps to query, val2 stays positional (post-dash)
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("val1".to_string()))),
            Arg::DoubleDash,
            Arg::Positional(Expr::Literal(Value::String("val2".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("val1".to_string()))
        );
        // val2 is after --, should NOT be mapped even though schema has unfilled params
        assert_eq!(
            tool_args.positional,
            vec![Value::String("val2".to_string())]
        );
    }

    #[tokio::test]
    async fn test_all_params_filled_by_flags() {
        // All schema params satisfied by explicit flags — no positional mapping needed
        let args = vec![
            Arg::LongFlag("query".to_string()),
            Arg::Positional(Expr::Literal(Value::String("search".to_string()))),
            Arg::LongFlag("output".to_string()),
            Arg::Positional(Expr::Literal(Value::String("out.txt".to_string()))),
            Arg::LongFlag("verbose".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("search".to_string()))
        );
        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("out.txt".to_string()))
        );
        assert!(tool_args.flags.contains("verbose"));
        assert!(tool_args.positional.is_empty());
    }

    #[tokio::test]
    async fn test_mixed_flags_and_positional_fill() {
        // --output foo val1 — output is explicit, val1 maps to query
        let args = vec![
            Arg::LongFlag("output".to_string()),
            Arg::Positional(Expr::Literal(Value::String("foo".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("val1".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("foo".to_string()))
        );
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("val1".to_string()))
        );
        assert!(tool_args.positional.is_empty());
    }

    #[tokio::test]
    async fn test_alias_flag_prevents_mapping_overwrite() {
        // -q "search" "out.txt" — -q is alias for query, so out.txt should map to output
        let schema = ToolSchema::new("test", "")
            .param(ParamSchema::required("query", "string", "").with_aliases(["-q"]))
            .param(ParamSchema::required("output", "string", ""))
            .with_positional_mapping();
        let args = vec![
            Arg::ShortFlag("q".to_string()),
            Arg::Positional(Expr::Literal(Value::String("search".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("out.txt".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("search".to_string()))
        );
        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("out.txt".to_string()))
        );
        assert!(tool_args.positional.is_empty());
    }

    #[tokio::test]
    async fn test_builtin_schema_no_positional_mapping() {
        // Builtins have map_positionals=false — positionals stay positional
        let schema = ToolSchema::new("echo", "")
            .param(ParamSchema::optional("args", "any", Value::Null, ""))
            .param(ParamSchema::optional("no_newline", "bool", Value::Bool(false), ""));
        // Note: no .with_positional_mapping() — this is a builtin
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("hello".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("world".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        // Positionals should NOT be consumed as named params
        assert_eq!(
            tool_args.positional,
            vec![
                Value::String("hello".to_string()),
                Value::String("world".to_string()),
            ]
        );
        assert!(!tool_args.named.contains_key("args"));
    }

    #[tokio::test]
    async fn test_short_flag_with_alias_consumes_value() {
        // `-n 5` where `-n` is aliased to `lines` (type: int)
        // Should produce named: {"lines": 5}, not flags: {"n"} + positional: [5]
        let schema = ToolSchema::new("head", "Output first part of files")
            .param(ParamSchema::optional("lines", "int", Value::Int(10), "Number of lines")
                .with_aliases(["-n"]));
        let args = vec![
            Arg::ShortFlag("n".to_string()),
            Arg::Positional(Expr::Literal(Value::Int(5))),
            Arg::Positional(Expr::Literal(Value::String("/tmp/file.txt".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert!(tool_args.flags.is_empty(), "no boolean flags: {:?}", tool_args.flags);
        assert_eq!(tool_args.named.get("lines"), Some(&Value::Int(5)), "should resolve alias to canonical name");
        assert_eq!(tool_args.positional, vec![Value::String("/tmp/file.txt".to_string())]);
    }

    // === GH #188: divergences the pre-unification sync twin couldn't handle ===
    //
    // The old `scheduler::pipeline::build_tool_args` hand-rolled its own
    // flag/positional binder that never supported glued short-flag values or
    // `consumes`/`repeatable` accumulation (see the removed comment that used
    // to sit on the `LongFlag` arm). Scatter/gather's own schemas never
    // exercised these (scalar flags only), so the gap was real but
    // un-triggerable in production — these tests pin the now-shared
    // `kernel::bind_tool_args` behavior through the reduced sync entry point
    // so the two binders can't quietly drift apart on it again.

    #[tokio::test]
    async fn test_glued_short_flag_value_now_binds() {
        // `-f1` (`cut -f1`-shaped): before #188 this fell to the "combined
        // short flags" arm and produced two bogus bool flags ("f", "1")
        // instead of resolving the declared value-flag's glued value.
        let schema = ToolSchema::new("cut", "")
            .param(ParamSchema::optional("fields", "string", Value::Null, "").with_aliases(["-f"]));
        let args = vec![Arg::ShortFlag("f1".to_string())];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert!(tool_args.flags.is_empty(), "no bogus bool flags: {:?}", tool_args.flags);
        assert_eq!(tool_args.named.get("fields"), Some(&Value::String("1".to_string())));
    }

    #[tokio::test]
    async fn test_repeatable_flag_now_accumulates() {
        // `-e A -e B`: before #188 the sync twin's value-flag path always
        // overwrote `named[canonical]`, silently keeping only the last
        // occurrence ("B"). The shared core accumulates both, matching
        // `Kernel::build_args_async`.
        let schema = ToolSchema::new("sed", "")
            .param(ParamSchema::optional("expression", "string", Value::Null, "")
                .with_aliases(["-e"])
                .with_repeatable(true));
        let args = vec![
            Arg::ShortFlag("e".to_string()),
            Arg::Positional(Expr::Literal(Value::String("A".to_string()))),
            Arg::ShortFlag("e".to_string()),
            Arg::Positional(Expr::Literal(Value::String("B".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("expression"),
            Some(&Value::Json(serde_json::json!(["A", "B"]))),
            "both occurrences must survive, not just the last: {:?}",
            tool_args.named
        );
    }

    #[tokio::test]
    async fn test_multi_consume_flag_now_accumulates() {
        // `--arg NAME VAL` (jq-shaped, `consumes == 2`): before #188 the sync
        // twin only ever consumed a single positional per flag occurrence,
        // so a `consumes: 2` param was unsupported. The shared core
        // recognizes it and accumulates array-of-arrays occurrences.
        let schema = ToolSchema::new("jq", "")
            .param(ParamSchema::optional("arg", "any", Value::Null, "").consumes(2));
        let args = vec![
            Arg::LongFlag("arg".to_string()),
            Arg::Positional(Expr::Literal(Value::String("name".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("val".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema)).await.expect("build_tool_args");

        assert_eq!(
            tool_args.named.get("arg"),
            Some(&Value::Json(serde_json::json!([["name", "val"]]))),
            "got: {:?}",
            tool_args.named
        );
        assert!(tool_args.positional.is_empty());
    }

    // === Redirect Execution Tests ===

    #[tokio::test]
    async fn test_merge_stderr_redirect() {
        // Test that 2>&1 merges stderr into stdout
        let result = ExecResult::from_output(0, "stdout content", "stderr content");

        let redirects = vec![Redirect {
            kind: RedirectKind::MergeStderr,
            target: Expr::Literal(Value::Null),
        }];

        let ctx = make_minimal_ctx();
        let result = apply_redirects(result, &redirects, &ctx, &test_dispatcher()).await;

        assert_eq!(&*result.text_out(), "stdout contentstderr content");
        assert!(result.err.is_empty());
    }

    #[tokio::test]
    async fn test_merge_stderr_with_empty_stderr() {
        // Test that 2>&1 handles empty stderr gracefully
        let result = ExecResult::from_output(0, "stdout only", "");

        let redirects = vec![Redirect {
            kind: RedirectKind::MergeStderr,
            target: Expr::Literal(Value::Null),
        }];

        let ctx = make_minimal_ctx();
        let result = apply_redirects(result, &redirects, &ctx, &test_dispatcher()).await;

        assert_eq!(&*result.text_out(), "stdout only");
        assert!(result.err.is_empty());
    }

    #[tokio::test]
    async fn test_merge_stderr_order_matters() {
        // Test redirect ordering: 2>&1 > file means:
        // 1. First merge stderr into stdout
        // 2. Then write stdout to file (leaving both empty for piping)
        // This verifies left-to-right processing
        let result = ExecResult::from_output(0, "stdout\n", "stderr\n");

        // Just 2>&1 - should merge
        let redirects = vec![Redirect {
            kind: RedirectKind::MergeStderr,
            target: Expr::Literal(Value::Null),
        }];

        let ctx = make_minimal_ctx();
        let result = apply_redirects(result, &redirects, &ctx, &test_dispatcher()).await;

        assert_eq!(&*result.text_out(), "stdout\nstderr\n");
        assert!(result.err.is_empty());
    }

    #[tokio::test]
    async fn test_redirect_with_command_execution() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "hello" with 2>&1 redirect
        let cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello".to_string())))],
            redirects: vec![Redirect {
                kind: RedirectKind::MergeStderr,
                target: Expr::Literal(Value::Null),
            }],
        };

        let result = runner.run(&[cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        // echo produces no stderr, so this just validates the redirect doesn't break anything
        assert!(result.text_out().contains("hello"));
    }

    #[tokio::test]
    async fn test_merge_stderr_in_pipeline() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "output" 2>&1 | grep "output"
        // The 2>&1 should be applied to echo's result, then piped to grep
        let echo_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("output".to_string())))],
            redirects: vec![Redirect {
                kind: RedirectKind::MergeStderr,
                target: Expr::Literal(Value::Null),
            }],
        };
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("output".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[echo_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok(), "result failed: code={}, err={}", result.code, result.err);
        assert!(result.text_out().contains("output"));
    }

    // === Item 6: `&>` (RedirectKind::Both) streams structured output ===
    //
    // `>`/`>>` already stream a command's structured `OutputData` straight to
    // a byte buffer via `take_output_for_stream`/`write_canonical` instead of
    // building the whole `to_canonical_string()` `String` first. `&>` used to
    // skip that path entirely (`result.text_out().into_owned().into_bytes()`,
    // which forces the full-string materialization). These tests lock in that
    // `&>` now takes the same streaming path and — since the file bytes are
    // the only thing observable from outside — that it produces byte-for-byte
    // the same content the old materialize-first code did.

    fn big_table_output(rows: usize) -> crate::interpreter::OutputData {
        use crate::interpreter::OutputNode;
        let headers = vec!["id".to_string(), "name".to_string()];
        let nodes: Vec<OutputNode> = (0..rows)
            .map(|i| OutputNode::new(i.to_string()).with_cells(vec![format!("row-{i}")]))
            .collect();
        crate::interpreter::OutputData::table(headers, nodes)
    }

    #[tokio::test]
    async fn test_both_redirect_streams_structured_output_to_file() {
        // A result with structured `.output` and empty `.out` — exactly the
        // shape `take_output_for_stream` requires, and the shape a real
        // builtin (e.g. `ls`, `find`) hands back before `--json`/materialize
        // ever runs.
        let output = big_table_output(50);
        let expected_stdout = output.to_canonical_string();
        let mut result = ExecResult::with_output(output);
        result.err = "warning: heads up\n".to_string();

        let redirects = vec![Redirect {
            kind: RedirectKind::Both,
            target: Expr::Literal(Value::String("/out.txt".to_string())),
        }];
        let ctx = make_minimal_ctx();
        let result = apply_redirects(result, &redirects, &ctx, &test_dispatcher()).await;

        // Both streams went to the file: stdout (incl. the sideband) and
        // stderr are both dropped from the in-memory result.
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "");
        assert!(result.err.is_empty());
        assert!(!result.has_output());

        let written = ctx.backend.read(Path::new("/out.txt"), None).await.expect("file written");
        let written = String::from_utf8(written).expect("valid utf8");
        // Byte-for-byte the same as the pre-refactor path would have produced:
        // the table's canonical string, followed by stderr, with nothing lost
        // or reordered by streaming it through `write_canonical` instead.
        assert_eq!(written, format!("{expected_stdout}warning: heads up\n"));
    }

    #[tokio::test]
    async fn test_both_redirect_streams_large_structured_output_intact() {
        // A much bigger table than any single test needs to *pass*, but large
        // enough that a regression re-introducing a size-limited or
        // truncating path (rather than genuinely streaming) would be caught:
        // every row must survive the round-trip through `&>`.
        let rows = 5_000;
        let output = big_table_output(rows);
        let expected_stdout = output.to_canonical_string();
        let result = ExecResult::with_output(output);

        let redirects = vec![Redirect {
            kind: RedirectKind::Both,
            target: Expr::Literal(Value::String("/big.txt".to_string())),
        }];
        let ctx = make_minimal_ctx();
        let result = apply_redirects(result, &redirects, &ctx, &test_dispatcher()).await;
        assert!(result.ok());

        let written = ctx.backend.read(Path::new("/big.txt"), None).await.expect("file written");
        let written = String::from_utf8(written).expect("valid utf8");
        assert_eq!(written, expected_stdout);
        assert!(written.contains("row-0"));
        assert!(written.contains(&format!("row-{}", rows - 1)));
    }

    #[tokio::test]
    async fn test_both_redirect_still_writes_binary_stdout_raw() {
        // Unchanged branch (`out_bytes()`), covered here so the refactor
        // can't accidentally regress the binary path while touching the
        // structured-output branch next to it.
        let result = ExecResult::success_text_or_bytes(vec![0xff, 0x00, 0xfe, b'x']);
        let redirects = vec![Redirect {
            kind: RedirectKind::Both,
            target: Expr::Literal(Value::String("/bin.out".to_string())),
        }];
        let ctx = make_minimal_ctx();
        let result = apply_redirects(result, &redirects, &ctx, &test_dispatcher()).await;
        assert!(result.ok());

        let written = ctx.backend.read(Path::new("/bin.out"), None).await.expect("file written");
        assert_eq!(written, vec![0xff, 0x00, 0xfe, b'x']);
    }
}
