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
use crate::interpreter::ExecResult;
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
async fn apply_redirects(
    mut result: ExecResult,
    redirects: &[Redirect],
    ctx: &ExecContext,
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
                    result.clear_out();
                }
            }
            RedirectKind::StdoutOverwrite => {
                let path = match eval_redirect_target(&redir.target, ctx).await {
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
                result.clear_out();
                result.set_output(None);
            }
            RedirectKind::StdoutAppend => {
                let path = match eval_redirect_target(&redir.target, ctx).await {
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
                result.clear_out();
                result.set_output(None);
            }
            RedirectKind::Stderr => {
                let path = match eval_redirect_target(&redir.target, ctx).await {
                    Ok(p) => p,
                    Err(e) => return ExecResult::failure(1, format!("redirect: {e}")),
                };
                if let Err(e) = redirect_write(ctx, &path, result.err.as_bytes()).await {
                    return ExecResult::failure(1, format!("redirect: {e}"));
                }
                result.err.clear();
            }
            RedirectKind::Both => {
                let path = match eval_redirect_target(&redir.target, ctx).await {
                    Ok(p) => p,
                    Err(e) => return ExecResult::failure(1, format!("redirect: {e}")),
                };
                // Build the combined bytes: raw binary stdout (no lossy decode)
                // or text stdout, followed by stderr.
                let mut combined: Vec<u8> = match result.out_bytes() {
                    Some(b) => b.to_vec(),
                    None => result.text_out().into_owned().into_bytes(),
                };
                combined.extend_from_slice(result.err.as_bytes());
                if let Err(e) = redirect_write(ctx, &path, &combined).await {
                    return ExecResult::failure(1, format!("redirect: {e}"));
                }
                result.clear_out();
                result.set_output(None);
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
/// Routes through `ctx.dispatcher` so command substitution (`$(...)`) in the
/// target runs — e.g. `cat < $(echo f)`, `echo x > $(echo f)`, and `$(...)`
/// inside a heredoc body. Falls back to the sync evaluator (which skips
/// command substitution) only when no dispatcher is attached.
async fn eval_redirect_target(expr: &Expr, ctx: &ExecContext) -> Result<String, String> {
    if let Some(dispatcher) = &ctx.dispatcher {
        dispatcher
            .eval_expr(expr, ctx)
            .await
            .map(|v| value_to_string(&v))
            .map_err(|e| e.to_string())
    } else {
        eval_simple_expr(expr, ctx)
            .map(|v| value_to_string(&v))
            .ok_or_else(|| "could not evaluate redirect target".to_string())
    }
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
/// redirects resolve their operands. A missing/unreadable file or non-UTF-8
/// content is a hard error — we never silently feed the command empty stdin.
async fn setup_stdin_redirects(cmd: &Command, ctx: &mut ExecContext) -> Result<(), String> {
    use std::path::Path;
    for redir in &cmd.redirects {
        match &redir.kind {
            RedirectKind::Stdin => {
                let path = eval_redirect_target(&redir.target, ctx).await?;
                let resolved = ctx.resolve_path(&path);
                let data = ctx
                    .backend
                    .read(Path::new(&resolved), None)
                    .await
                    .map_err(|e| format!("redirect: {path}: {e}"))?;
                let content = String::from_utf8(data)
                    .map_err(|_| format!("redirect: {path}: invalid UTF-8"))?;
                ctx.set_stdin(content);
            }
            RedirectKind::HereDoc => {
                match &redir.target {
                    Expr::Literal(Value::String(content)) => {
                        ctx.set_stdin(content.clone());
                    }
                    // Heredoc bodies may contain `$(...)`; route through the
                    // dispatcher so command substitution runs.
                    expr => {
                        let body = eval_redirect_target(expr, ctx).await?;
                        ctx.set_stdin(body);
                    }
                }
            }
            RedirectKind::HereString => {
                // Per bash, here-strings append a trailing newline to the
                // expanded word so the command receives a terminated line.
                let mut s = eval_redirect_target(&redir.target, ctx).await?;
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
    #[tracing::instrument(level = "debug", skip(self, commands, ctx, dispatcher), fields(command_count = commands.len()))]
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
    #[tracing::instrument(level = "debug", skip(self, commands, ctx, dispatcher), fields(command_count = commands.len()))]
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
        // These are builtins with simple key=value syntax, no schema-driven parsing needed
        let scatter_schema = self.tools.get("scatter").map(|t| t.schema());
        let gather_schema = self.tools.get("gather").map(|t| t.schema());
        let scatter_opts = parse_scatter_options(&build_tool_args(&scatter_cmd.args, ctx, scatter_schema.as_ref()));
        let gather_opts = parse_gather_options(&build_tool_args(&gather_cmd.args, ctx, gather_schema.as_ref()));

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
                post_gather,
                ctx,
            )
            .await
    }

    /// Run a single command with optional stdin.
    ///
    /// The dispatcher handles arg parsing, schema lookup, output format, and execution.
    /// The runner handles stdin setup (redirects + pipeline) and output redirects.
    #[tracing::instrument(level = "debug", skip(self, cmd, ctx, stdin, dispatcher), fields(command = %cmd.name))]
    async fn run_single(
        &self,
        cmd: &Command,
        ctx: &mut ExecContext,
        stdin: Option<String>,
        dispatcher: &dyn CommandDispatcher,
    ) -> ExecResult {
        // Set up stdin from redirects (< file, <<heredoc)
        if let Err(e) = setup_stdin_redirects(cmd, ctx).await {
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
        apply_redirects(result, &cmd.redirects, ctx).await
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
    #[tracing::instrument(level = "debug", skip(self, commands, ctx, dispatcher), fields(stage_count = commands.len()))]
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
            let stdin_setup = setup_stdin_redirects(&cmd, &mut stage_ctx).await;

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

                // Apply post-execution redirects
                result = apply_redirects(result, &cmd.redirects, &stage_ctx).await;

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
                    // A binary result flows through the pipe as raw bytes; text
                    // results as their UTF-8 bytes. Either way the next stage
                    // gets exactly what was produced — no lossy round-trip.
                    let bytes: Vec<u8> = match result.out_bytes() {
                        Some(b) => b.to_vec(),
                        None => result.text_out().into_owned().into_bytes(),
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
        for (i, handle) in handles.into_iter().enumerate() {
            match handle.await {
                Ok((result, stage_ctx)) => {
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

/// Build ToolArgs from AST Args, evaluating expressions.
///
/// If a schema is provided, uses it to determine argument types:
/// - For `--flag` where schema says type is non-bool: consume next positional as value
/// - For `--flag` where schema says type is bool (or unknown): treat as boolean flag
///
/// This enables natural shell syntax like `mcp_tool --query "test" --limit 10`.
pub fn build_tool_args(args: &[Arg], ctx: &ExecContext, schema: Option<&ToolSchema>) -> ToolArgs {
    let mut tool_args = ToolArgs::new();
    let param_lookup = schema.map(schema_param_lookup).unwrap_or_default();
    let accepts_word_assign = schema
        .map(|s| crate::tools::accepts_word_assign(s.name.as_str()))
        .unwrap_or(false);

    // Track which positional indices have been consumed as flag values
    let mut consumed_positionals: std::collections::HashSet<usize> = std::collections::HashSet::new();
    let mut past_double_dash = false;

    // First pass: find positional args and their indices
    let mut positional_indices: Vec<(usize, &Expr)> = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        if let Arg::Positional(expr) = arg {
            positional_indices.push((i, expr));
        }
    }

    // Second pass: process all args
    let mut i = 0;
    while i < args.len() {
        let arg = &args[i];

        match arg {
            Arg::DoubleDash => {
                past_double_dash = true;
            }
            Arg::Positional(expr) => {
                // Check if this positional was consumed by a preceding flag
                if !consumed_positionals.contains(&i)
                    && let Some(value) = eval_simple_expr(expr, ctx)
                {
                    tool_args.positional.push(value);
                }
            }
            Arg::Named { key, value } => {
                if let Some(val) = eval_simple_expr(value, ctx) {
                    tool_args.named.insert(key.clone(), val);
                }
            }
            Arg::WordAssign { key, value } => {
                if let Some(val) = eval_simple_expr(value, ctx) {
                    if accepts_word_assign {
                        tool_args.named.insert(key.clone(), val);
                    } else {
                        let val_str = crate::interpreter::value_to_string(&val);
                        tool_args.positional.push(Value::String(format!("{key}={val_str}")));
                    }
                }
            }
            Arg::ShortFlag(name) => {
                if past_double_dash {
                    tool_args.positional.push(Value::String(format!("-{name}")));
                } else if name.len() == 1 {
                    // Single-char short flag: look up schema to check if it takes a value.
                    // e.g., `-n 5` where `-n` is an alias for `lines` (type: int)
                    let flag_name = name.as_str();
                    let lookup = param_lookup.get(flag_name);
                    let is_bool = lookup
                        .map(|(_, typ, ..)| is_bool_type(typ))
                        .unwrap_or(true);

                    if is_bool {
                        tool_args.flags.insert(flag_name.to_string());
                    } else {
                        // Non-bool: consume next positional as value, insert under canonical name
                        let canonical = lookup.map(|(n, ..)| *n).unwrap_or(flag_name);
                        let next_positional = positional_indices
                            .iter()
                            .find(|(idx, _)| *idx > i && !consumed_positionals.contains(idx));

                        if let Some((pos_idx, expr)) = next_positional {
                            if let Some(value) = eval_simple_expr(expr, ctx) {
                                tool_args.named.insert(canonical.to_string(), value);
                                consumed_positionals.insert(*pos_idx);
                            } else {
                                tool_args.flags.insert(flag_name.to_string());
                            }
                        } else {
                            tool_args.flags.insert(flag_name.to_string());
                        }
                    }
                } else if let Some(&(canonical, typ, ..)) = param_lookup.get(name.as_str()) {
                    // Multi-char short flag matches a schema param (POSIX style: -name value)
                    if is_bool_type(typ) {
                        tool_args.flags.insert(canonical.to_string());
                    } else {
                        let next_positional = positional_indices
                            .iter()
                            .find(|(idx, _)| *idx > i && !consumed_positionals.contains(idx));
                        if let Some((pos_idx, expr)) = next_positional {
                            if let Some(value) = eval_simple_expr(expr, ctx) {
                                tool_args.named.insert(canonical.to_string(), value);
                                consumed_positionals.insert(*pos_idx);
                            } else {
                                tool_args.flags.insert(name.clone());
                            }
                        } else {
                            tool_args.flags.insert(name.clone());
                        }
                    }
                } else {
                    // Multi-char combined flags like -la: always boolean
                    for c in name.chars() {
                        tool_args.flags.insert(c.to_string());
                    }
                }
            }
            Arg::LongFlag(name) => {
                if past_double_dash {
                    tool_args.positional.push(Value::String(format!("--{name}")));
                } else {
                    // Look up type in schema (checks name and aliases)
                    let lookup = param_lookup.get(name.as_str());
                    let is_bool = lookup
                        .map(|(_, typ, ..)| is_bool_type(typ))
                        .unwrap_or(true); // Unknown params default to bool

                    if is_bool {
                        tool_args.flags.insert(name.clone());
                    } else {
                        // Non-bool: consume next positional as value, insert under canonical name
                        // Note: the sync build_tool_args does NOT honor `consumes > 1` OR
                        // `repeatable` (it overwrites on a repeated flag). The async
                        // build_args_async in kernel.rs is the only path that supports multi-consume
                        // and repeatable accumulation. Sync callers — scatter/gather option parsing
                        // (scalar flags only) and the test-only BackendDispatcher — don't carry such
                        // flags, so this is safe today; if they ever do, lift the logic via a shared
                        // helper. Tracked in docs/issues.md.
                        let canonical = lookup.map(|(n, ..)| *n).unwrap_or(name.as_str());
                        let next_positional = positional_indices
                            .iter()
                            .find(|(idx, _)| *idx > i && !consumed_positionals.contains(idx));

                        if let Some((pos_idx, expr)) = next_positional {
                            if let Some(value) = eval_simple_expr(expr, ctx) {
                                tool_args.named.insert(canonical.to_string(), value);
                                consumed_positionals.insert(*pos_idx);
                            } else {
                                tool_args.flags.insert(name.clone());
                            }
                        } else {
                            tool_args.flags.insert(name.clone());
                        }
                    }
                }
            }
        }
        i += 1;
    }

    // Map remaining positionals to unfilled non-bool schema params (in order).
    // This enables `drift_push "abc" "hello"` → named["target_ctx"] = "abc", named["content"] = "hello"
    // Positionals that appeared after `--` are never mapped (they're raw data).
    // Only for backend/external tools (map_positionals=true). Builtins handle their own positionals.
    if let Some(schema) = schema.filter(|s| s.map_positionals) {
        // Count how many positionals were added before `--`
        let pre_dash_count = if past_double_dash {
            // Find where the double-dash was in the original args to count pre-dash positionals
            let dash_pos = args.iter().position(|a| matches!(a, Arg::DoubleDash)).unwrap_or(args.len());
            // Count unconsumed positionals before the double-dash
            positional_indices.iter()
                .filter(|(idx, _)| *idx < dash_pos && !consumed_positionals.contains(idx))
                .count()
        } else {
            tool_args.positional.len()
        };

        let mut remaining = Vec::new();
        let mut positional_iter = tool_args.positional.drain(..).enumerate();

        for param in &schema.params {
            if tool_args.named.contains_key(&param.name) || tool_args.flags.contains(&param.name) {
                continue; // Already filled by a flag or named arg
            }
            if is_bool_type(&param.param_type) {
                continue; // Bool params should only be set by flags
            }
            // Take from pre-dash positionals only
            loop {
                match positional_iter.next() {
                    Some((idx, val)) if idx < pre_dash_count => {
                        tool_args.named.insert(param.name.clone(), val);
                        break;
                    }
                    Some((_, val)) => {
                        remaining.push(val); // Post-dash or past limit, keep as positional
                    }
                    None => break,
                }
            }
        }

        // Any leftover positionals stay positional (e.g. `cat file1 file2`)
        remaining.extend(positional_iter.map(|(_, v)| v));
        tool_args.positional = remaining;
    }

    tool_args
}

/// Simple expression evaluation for args (without full scope access).
pub(crate) fn eval_simple_expr(expr: &Expr, ctx: &ExecContext) -> Option<Value> {
    match expr {
        Expr::Literal(value) => Some(eval_literal(value, ctx)),
        // This reduced sync path coalesces any resolution failure to None
        // (undefined or a loud path error alike); the async kernel paths carry
        // the loud collection-access errors.
        Expr::VarRef(path) => ctx.scope.resolve_path(path).ok(),
        Expr::Interpolated(parts) => {
            let mut result = String::new();
            for part in parts {
                match part {
                    crate::ast::StringPart::Literal(s) => result.push_str(s),
                    crate::ast::StringPart::Var(path) => {
                        if let Ok(value) = ctx.scope.resolve_path(path) {
                            result.push_str(&value_to_string(&value));
                        }
                    }
                    crate::ast::StringPart::VarWithDefault { name, default } => {
                        match ctx.scope.get(name) {
                            Some(value) => {
                                let s = value_to_string(value);
                                if s.is_empty() {
                                    result.push_str(&eval_string_parts_sync(default, ctx));
                                } else {
                                    result.push_str(&s);
                                }
                            }
                            None => result.push_str(&eval_string_parts_sync(default, ctx)),
                        }
                    }
                    crate::ast::StringPart::VarLength(name) => {
                        let len = match ctx.scope.get(name) {
                            // Element/key count for collections, byte count for
                            // binary — the same helper the async/interp paths use
                            // (not the string byte-length of a rendered value).
                            Some(value) => crate::interpreter::value_length(value) as usize,
                            None => 0,
                        };
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
                        // Evaluate arithmetic in pipeline context
                        if let Ok(value) = arithmetic::eval_arithmetic(expr, &ctx.scope) {
                            result.push_str(&value.to_string());
                        }
                    }
                    crate::ast::StringPart::CommandSubst(_) => {
                        // Command substitution requires async - skip in sync context
                    }
                    crate::ast::StringPart::LastExitCode => {
                        result.push_str(&ctx.scope.last_result().code.to_string());
                    }
                    crate::ast::StringPart::CurrentPid => {
                        result.push_str(&ctx.scope.pid().to_string());
                    }
                }
            }
            Some(Value::String(result))
        }
        Expr::GlobPattern(s) => Some(Value::String(s.clone())),
        Expr::HereDocBody { parts, strip_tabs } => {
            // Heredoc body materialization for redirect targets. `<<-` tab
            // stripping applies to the literal source, not to tabs from a
            // `$var` value — matching the interpreter's eval path.
            let mut asm = crate::interpreter::HeredocAssembler::new(*strip_tabs);
            for sp in parts {
                match &sp.part {
                    crate::ast::StringPart::Literal(s) => asm.push_literal(s),
                    other => asm.push_interpolated(&eval_string_parts_sync(
                        std::slice::from_ref(other),
                        ctx,
                    )),
                }
            }
            Some(Value::String(asm.into_string()))
        }
        _ => None, // Binary ops and command subst need more context
    }
}

/// Evaluate a literal value.
fn eval_literal(value: &Value, _ctx: &ExecContext) -> Value {
    value.clone()
}

/// Convert a value to a string for interpolation.
fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => "".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Json(json) => json.to_string(),
        Value::Bytes(b) => format!("[binary: {} bytes]", b.len()),
    }
}

/// Evaluate string parts synchronously (for pipeline context).
/// Command substitutions are skipped as they require async.
fn eval_string_parts_sync(parts: &[crate::ast::StringPart], ctx: &ExecContext) -> String {
    let mut result = String::new();
    for part in parts {
        match part {
            crate::ast::StringPart::Literal(s) => result.push_str(s),
            crate::ast::StringPart::Var(path) => {
                if let Ok(value) = ctx.scope.resolve_path(path) {
                    result.push_str(&value_to_string(&value));
                }
            }
            crate::ast::StringPart::VarWithDefault { name, default } => {
                match ctx.scope.get(name) {
                    Some(value) => {
                        let s = value_to_string(value);
                        if s.is_empty() {
                            result.push_str(&eval_string_parts_sync(default, ctx));
                        } else {
                            result.push_str(&s);
                        }
                    }
                    None => result.push_str(&eval_string_parts_sync(default, ctx)),
                }
            }
            crate::ast::StringPart::VarLength(name) => {
                let len = match ctx.scope.get(name) {
                    // Element/key count for collections, byte count for binary —
                    // the same helper the async/interp paths use.
                    Some(value) => crate::interpreter::value_length(value) as usize,
                    None => 0,
                };
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
                if let Ok(value) = arithmetic::eval_arithmetic(expr, &ctx.scope) {
                    result.push_str(&value.to_string());
                }
            }
            crate::ast::StringPart::CommandSubst(_) => {
                // Command substitution requires async - skip in sync context
            }
            crate::ast::StringPart::LastExitCode => {
                result.push_str(&ctx.scope.last_result().code.to_string());
            }
            crate::ast::StringPart::CurrentPid => {
                result.push_str(&ctx.scope.pid().to_string());
            }
        }
    }
    result
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

    #[test]
    fn test_schema_aware_string_arg() {
        // --query "test" should become named: {"query": "test"}
        let args = vec![
            Arg::LongFlag("query".to_string()),
            Arg::Positional(Expr::Literal(Value::String("test".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.flags.is_empty(), "No flags should be set");
        assert!(tool_args.positional.is_empty(), "No positionals - consumed by --query");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("test".to_string())),
            "--query should consume 'test' as its value"
        );
    }

    #[test]
    fn test_schema_aware_bool_flag() {
        // --verbose should remain a flag since schema says bool
        let args = vec![
            Arg::LongFlag("verbose".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.flags.contains("verbose"), "--verbose should be a flag");
        assert!(tool_args.named.is_empty(), "No named args");
        assert!(tool_args.positional.is_empty(), "No positionals");
    }

    #[test]
    fn test_schema_aware_mixed() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_schema_aware_multiple_string_args() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_schema_aware_double_dash() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_no_schema_fallback() {
        // Without schema, all --flags are treated as bool flags
        let args = vec![
            Arg::LongFlag("query".to_string()),
            Arg::Positional(Expr::Literal(Value::String("test".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, None);

        // Without schema, --query is a flag and "test" is a positional
        assert!(tool_args.flags.contains("query"), "--query should be a flag");
        assert_eq!(
            tool_args.positional,
            vec![Value::String("test".to_string())],
            "'test' should be a positional"
        );
    }

    #[test]
    fn test_unknown_flag_in_schema() {
        // --unknown-flag value: --unknown is bool (not in schema), "value" maps to query
        let args = vec![
            Arg::LongFlag("unknown".to_string()),
            Arg::Positional(Expr::Literal(Value::String("value".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.flags.contains("unknown"));
        assert!(tool_args.positional.is_empty(), "value consumed as query param");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("value".to_string()))
        );
    }

    #[test]
    fn test_named_args_unchanged() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("test".to_string()))
        );
        assert!(tool_args.flags.contains("verbose"));
    }

    #[test]
    fn test_short_flags_unchanged() {
        // Short flags -la should expand regardless of schema; file.txt maps to query
        let args = vec![
            Arg::ShortFlag("la".to_string()),
            Arg::Positional(Expr::Literal(Value::String("file.txt".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.flags.contains("l"));
        assert!(tool_args.flags.contains("a"));
        assert!(tool_args.positional.is_empty(), "file.txt consumed as query param");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("file.txt".to_string()))
        );
    }

    #[test]
    fn test_flag_at_end_no_value() {
        // --output at end with no value available - treat as flag (lenient)
        // file.txt maps to query (first unfilled non-bool param)
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("file.txt".to_string()))),
            Arg::LongFlag("output".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        // output expects a value but none available after it, so it becomes a flag
        assert!(tool_args.flags.contains("output"));
        assert!(tool_args.positional.is_empty(), "file.txt consumed as query param");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("file.txt".to_string()))
        );
    }

    #[test]
    fn test_positional_skips_bool_params() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_positionals_fill_available_slots() {
        // Schema has query (string), limit (int), verbose (bool), output (string).
        // Three positionals fill the 3 non-bool slots.
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("val1".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("val2".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("val3".to_string()))),
        ];
        let schema = make_test_schema(); // query, limit(int), verbose(bool), output
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_truly_excess_positionals() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_double_dash_positional_not_mapped() {
        // `tool val1 -- val2` — val1 maps to query, val2 stays positional (post-dash)
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("val1".to_string()))),
            Arg::DoubleDash,
            Arg::Positional(Expr::Literal(Value::String("val2".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_all_params_filled_by_flags() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_mixed_flags_and_positional_fill() {
        // --output foo val1 — output is explicit, val1 maps to query
        let args = vec![
            Arg::LongFlag("output".to_string()),
            Arg::Positional(Expr::Literal(Value::String("foo".to_string()))),
            Arg::Positional(Expr::Literal(Value::String("val1".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_alias_flag_prevents_mapping_overwrite() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_builtin_schema_no_positional_mapping() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

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

    #[test]
    fn test_short_flag_with_alias_consumes_value() {
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

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.flags.is_empty(), "no boolean flags: {:?}", tool_args.flags);
        assert_eq!(tool_args.named.get("lines"), Some(&Value::Int(5)), "should resolve alias to canonical name");
        assert_eq!(tool_args.positional, vec![Value::String("/tmp/file.txt".to_string())]);
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
        let result = apply_redirects(result, &redirects, &ctx).await;

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
        let result = apply_redirects(result, &redirects, &ctx).await;

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
        let result = apply_redirects(result, &redirects, &ctx).await;

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
}
