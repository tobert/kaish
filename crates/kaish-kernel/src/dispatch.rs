//! Command dispatch — the single execution path for all commands.
//!
//! The `CommandDispatcher` trait defines how a single command is resolved and
//! executed. The Kernel implements this trait with the full dispatch chain:
//! user tools → builtins → .kai scripts → external commands → backend tools.
//!
//! `PipelineRunner` calls `dispatcher.dispatch()` for each command in a
//! pipeline, handling I/O routing (stdin piping, redirects) around each call.
//!
//! ```text
//! Stmt::Command ──┐
//!                  ├──▶ execute_pipeline() ──▶ PipelineRunner::run(dispatcher, commands, ctx)
//! Stmt::Pipeline ──┘                                  │
//!                                               for each command:
//!                                                 dispatcher.dispatch(cmd, ctx)
//!                                                     │
//!                                               ┌─────┼──────────────┐
//!                                               │     │              │
//!                                          user_tools builtins  .kai scripts
//!                                                                external cmds
//!                                                                backend tools
//! ```

use std::sync::Arc;

use anyhow::Result;
use async_trait::async_trait;

use crate::ast::{Command, Expr, Value};
use crate::interpreter::ExecResult;
use crate::tools::ExecContext;

// The following imports are only used by the test-only `BackendDispatcher`.
#[cfg(test)]
use crate::ast::Arg;
#[cfg(test)]
use crate::backend::BackendError;
#[cfg(test)]
use crate::interpreter::apply_output_format;
#[cfg(test)]
use crate::scheduler::build_tool_args;
#[cfg(test)]
use crate::tools::{GlobalFlags, ToolRegistry};
#[cfg(all(test, feature = "subprocess"))]
use crate::tools::resolve_in_path;

/// Position of a command within a pipeline.
///
/// Used by external command execution to decide stdio inheritance:
/// - `Only` or `Last` in interactive mode → inherit terminal
/// - `First` or `Middle` → always capture
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum PipelinePosition {
    /// Single command, no pipe.
    #[default]
    Only,
    /// First command in a pipeline (no stdin from pipe).
    First,
    /// Middle of a pipeline (piped stdin, piped stdout).
    Middle,
    /// Last command in a pipeline (piped stdin, final output).
    Last,
}

/// Trait for dispatching a single command through the full resolution chain.
///
/// Implementations handle argument parsing, tool lookup, and execution.
/// The pipeline runner handles I/O routing (stdin, redirects, piping).
#[async_trait]
pub trait CommandDispatcher: Send + Sync {
    /// Dispatch a single command for execution.
    ///
    /// The `ctx` provides stdin (from pipe or redirect), scope, and backend.
    /// Implementations should handle schema-aware argument parsing and
    /// output format extraction internally.
    async fn dispatch(&self, cmd: &Command, ctx: &mut ExecContext) -> Result<ExecResult>;

    /// Evaluate an expression through the full async chain.
    ///
    /// Unlike the runner's sync `eval_simple_expr`, this can run command
    /// substitution (`$(...)`) because it has access to pipeline execution.
    /// Used for redirect targets and heredoc bodies so `cat < $(cmd)`,
    /// `echo x > $(cmd)`, and `$(...)` inside heredoc bodies work. The `ctx`
    /// carries scope/cwd/backend for dispatchers that evaluate against it;
    /// stateful dispatchers (Kernel) snapshot their own session state and
    /// only let command output escape (side effects like `cd` do not).
    async fn eval_expr(&self, expr: &Expr, ctx: &ExecContext) -> Result<Value>;

    /// Fork the dispatcher for concurrent execution (detached).
    ///
    /// Returns a subsidiary dispatcher with independent mutable state, safe
    /// to run concurrently with the parent and other forks without data
    /// races on shared scope/cwd/aliases. Used by background `&` jobs,
    /// where the fork must survive parent cancellation.
    ///
    /// For stateful dispatchers (e.g. Kernel) this snapshots per-session
    /// state into a fresh instance. Stateless dispatchers may clone.
    async fn fork(&self) -> Arc<dyn CommandDispatcher>;

    /// Fork the dispatcher for concurrent execution (attached to parent cancel).
    ///
    /// Like [`Self::fork`] but the fork's cancellation token is a *child* of
    /// the parent's. Cancelling the parent (timeout, Ctrl-C, embedder
    /// `Kernel::cancel`) cascades into the fork, which then kills its own
    /// external children via the usual SIGTERM/SIGKILL discipline.
    ///
    /// Used for foreground concurrency: scatter workers, concurrent pipeline
    /// stages, command substitution. Default implementation delegates to
    /// [`Self::fork`] for stateless dispatchers that don't track cancellation.
    async fn fork_attached(&self) -> Arc<dyn CommandDispatcher> {
        self.fork().await
    }
}

/// Minimal stateless dispatcher used by pipeline/runner unit tests.
///
/// Production code uses `Kernel` (via `Kernel::fork` for concurrent contexts).
/// This test-only dispatcher routes directly through `backend.call_tool()` so
/// the pipeline runner can be exercised without spinning up a full Kernel.
///
/// Limitations (intentional — these are test-only constraints):
/// - No user-defined tools
/// - No .kai script resolution
/// - No async argument evaluation (command substitution in args won't work)
#[cfg(test)]
pub(crate) struct BackendDispatcher {
    tools: Arc<ToolRegistry>,
}

#[cfg(test)]
impl BackendDispatcher {
    /// Create a new backend dispatcher with the given tool registry.
    pub(crate) fn new(tools: Arc<ToolRegistry>) -> Self {
        Self { tools }
    }

    /// Try to execute an external command (PATH lookup + process spawn).
    ///
    /// Used as fallback when no builtin/backend tool matches. Returns None if
    /// the command is not found in PATH. Always captures stdout/stderr (never
    /// inherits terminal — pipeline stages don't need interactive I/O).
    #[cfg(not(feature = "subprocess"))]
    async fn try_external(
        &self,
        _name: &str,
        _args: &[Arg],
        _ctx: &mut ExecContext,
    ) -> Option<ExecResult> {
        None
    }

    /// Try to execute an external command (PATH lookup + process spawn).
    #[cfg(feature = "subprocess")]
    async fn try_external(
        &self,
        name: &str,
        args: &[Arg],
        ctx: &mut ExecContext,
    ) -> Option<ExecResult> {
        if !ctx.allow_external_commands {
            return None;
        }

        // Get real working directory (needed for relative path resolution and child cwd).
        // If the CWD is virtual (no real path), skip external execution entirely.
        let real_cwd = match ctx.backend.resolve_real_path(&ctx.cwd) {
            Some(p) => p,
            None => return None,
        };

        // Resolve command: absolute/relative path or PATH lookup
        let executable = if name.contains('/') {
            // Resolve relative paths (./script, ../bin/tool) against the shell's cwd
            let resolved = if std::path::Path::new(name).is_absolute() {
                std::path::PathBuf::from(name)
            } else {
                real_cwd.join(name)
            };
            if resolved.exists() {
                resolved.to_string_lossy().into_owned()
            } else {
                return Some(ExecResult::failure(127, format!("{}: No such file or directory", name)));
            }
        } else {
            // PATH from scope only — never OS env (keeps this test-only spawn
            // site in sync with kernel.rs::try_execute_external).
            let path_var = ctx.scope.get("PATH")
                .map(crate::interpreter::value_to_string)
                .unwrap_or_default();
            resolve_in_path(name, &path_var)?
        };

        // Build flat argv from args. A for-loop (not filter_map) so the
        // Decision D collection-argv guard can short-circuit the whole spawn
        // — kept in sync with the production build in kernel.rs::build_args_flat.
        let mut argv: Vec<String> = Vec::new();
        for arg in args {
            match arg {
                Arg::Positional(expr) => match expr {
                    Expr::Literal(Value::String(s)) => argv.push(s.clone()),
                    Expr::Literal(Value::Int(i)) => argv.push(i.to_string()),
                    Expr::Literal(Value::Float(f)) => argv.push(f.to_string()),
                    Expr::VarRef(path) => {
                        if let Ok(v) = ctx.scope.resolve_path(path) {
                            if let Some(msg) = crate::interpreter::structured_boundary_error("a command argument", &v) {
                                return Some(ExecResult::failure(1, msg));
                            }
                            // Text sink: binary goes loud (kept in sync with
                            // kernel.rs::build_args_flat).
                            match crate::interpreter::value_to_text_sink(&v) {
                                Ok(s) => argv.push(s),
                                Err(e) => return Some(ExecResult::failure(1, e.to_string())),
                            }
                        }
                    }
                    // Remaining literal types (Bool/Json/Null/Bytes) — kept in
                    // sync with the production build_args_flat, which resolves
                    // every positional through value_to_text_sink (binary loud).
                    Expr::Literal(other) => match crate::interpreter::value_to_text_sink(other) {
                        Ok(s) => argv.push(s),
                        Err(e) => return Some(ExecResult::failure(1, e.to_string())),
                    },
                    _ => {}
                },
                Arg::ShortFlag(f) => argv.push(format!("-{f}")),
                Arg::LongFlag(f) => argv.push(format!("--{f}")),
                Arg::Named { key, value } => match value {
                    Expr::Literal(Value::String(s)) => argv.push(format!("--{key}={s}")),
                    _ => argv.push(format!("--{key}=")),
                },
                Arg::WordAssign { key, value } => match value {
                    Expr::Literal(Value::String(s)) => argv.push(format!("{key}={s}")),
                    _ => argv.push(format!("{key}=")),
                },
                Arg::DoubleDash => argv.push("--".to_string()),
            }
        }

        // Check for streaming pipes
        let has_pipe_stdin = ctx.pipe_stdin.is_some();
        let has_buffered_stdin = ctx.stdin.is_some();

        // Spawn process
        use tokio::process::Command;
        use tokio::io::{AsyncReadExt, AsyncWriteExt};

        let mut cmd = Command::new(&executable);
        cmd.args(&argv);
        cmd.current_dir(&real_cwd);
        cmd.kill_on_drop(true);

        // Hermetic env: child sees only kaish's exported vars, not the kaish
        // process's OS env. Frontends that want OS-env passthrough (REPL, MCP)
        // populate it via KernelConfig::initial_vars at construction.
        cmd.env_clear();
        let exported = ctx.scope.exported_vars();
        // A structured value can't cross the process boundary; refuse rather than
        // silently JSON-serialize it into the child's environment. Kept in sync
        // with the production spawn site in kernel.rs::try_execute_external.
        if let Some(msg) = crate::interpreter::structured_export_error(&exported) {
            return Some(ExecResult::failure(1, msg));
        }
        for (var_name, value) in exported {
            // Binary can't cross the process boundary as an env var value
            // either — loud, not the `[binary: N bytes]` placeholder (kept in
            // sync with the production spawn site).
            match crate::interpreter::value_to_text_sink_named(
                &value,
                "an exported environment variable value",
            ) {
                Ok(s) => {
                    cmd.env(var_name, s);
                }
                Err(e) => return Some(ExecResult::failure(1, e.to_string())),
            }
        }

        // Stdin: pipe_stdin or buffered string or inherit (interactive) or null
        cmd.stdin(if has_pipe_stdin || has_buffered_stdin {
            std::process::Stdio::piped()
        } else if ctx.interactive && matches!(ctx.pipeline_position, PipelinePosition::First | PipelinePosition::Only) {
            std::process::Stdio::inherit()
        } else {
            std::process::Stdio::null()
        });
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        // On Unix, always put the child in its own process group so a
        // cancel can `killpg` the whole tree (the child plus any
        // grandchildren) — matching the production spawn site
        // (kernel.rs::try_execute_external) exactly. Without this, `killpg`
        // targets a group nobody is actually in (an ESRCH no-op), and a
        // grandchild spawned by the child survives cancellation — the exact
        // gap GH #133 item 4 closes. This dispatcher has no job-control
        // terminal integration (no `terminal_state`), so unlike production
        // there is no signal-handler restoration to gate here.
        #[cfg(unix)]
        {
            // SAFETY: setpgid is async-signal-safe per POSIX; safe to call
            // between fork and exec.
            #[allow(unsafe_code)]
            unsafe {
                cmd.pre_exec(|| {
                    nix::unistd::setpgid(nix::unistd::Pid::from_raw(0), nix::unistd::Pid::from_raw(0))
                        .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                    Ok(())
                });
            }
        }

        let mut child = match cmd.spawn() {
            Ok(c) => c,
            Err(e) => return Some(ExecResult::failure(127, format!("{}: {}", name, e))),
        };
        // Open a pidfd (Linux) for race-free direct-child kill via wait_or_kill.
        let kill_target = crate::pidfd::KillTarget::from_child(&child);

        // Stream stdin: copy pipe_stdin → child stdin in chunks (bounded memory)
        let stdin_task: Option<tokio::task::JoinHandle<()>> = if let Some(mut pipe_in) = ctx.pipe_stdin.take() {
            child.stdin.take().map(|mut child_stdin| {
                tokio::spawn(async move {
                    let mut buf = [0u8; 8192];
                    loop {
                        match pipe_in.read(&mut buf).await {
                            Ok(0) => break, // EOF
                            Ok(n) => {
                                if child_stdin.write_all(&buf[..n]).await.is_err() {
                                    break; // child closed stdin
                                }
                            }
                            Err(_) => break,
                        }
                    }
                    // Drop child_stdin signals EOF to child
                })
            })
        } else if let Some(data) = ctx.stdin.take() {
            // Buffered string stdin written from a DETACHED task, not inline:
            // an inline write deadlocks once the stdin pipe fills before the
            // output drain below has spawned (mirrors the kernel.rs fix; keeps
            // the two spawn sites in sync). Drop signals EOF; a broken pipe
            // (child closed stdin early) is fine.
            child.stdin.take().map(|mut child_stdin| {
                tokio::spawn(async move {
                    let _ = child_stdin.write_all(data.as_bytes()).await;
                })
            })
        } else {
            None
        };

        // Capture stdout via the spill-aware collector, regardless of whether
        // this is a pipeline stage (`ctx.pipe_stdout` set) or the last/only
        // stage. This intentionally does NOT special-case `ctx.pipe_stdout`
        // — production's `try_execute_external` never touches that field at
        // all; a middle/first pipeline stage's forwarding to the next stage
        // is entirely `PipelineRunner::run_pipeline`'s job (pipeline.rs),
        // which reads `stage_ctx.pipe_stdout` (still `Some`, untouched here)
        // after `dispatch()` returns and forwards `result.out` itself.
        //
        // Before this fix, this dispatcher special-cased `pipe_stdout` and
        // streamed the child's stdout straight through in 8KB chunks — full
        // fidelity, no cap. Production has no such fast path: every external
        // stage's stdout is captured here first, then forwarded by the
        // runner, so a >10MB intermediate stage silently loses its head in
        // production (the runner's forward goes through the SAME capture,
        // still true after this fix — see GH #133 item 2 for the capture
        // primitive itself). Losing the pipe_stdout special case is what lets
        // a test reproduce that production bug class at all (GH #133 item 3).
        let Some(child_stdout) = child.stdout.take() else {
            return Some(ExecResult::failure(1, "internal: stdout not available"));
        };
        let Some(mut child_stderr) = child.stderr.take() else {
            return Some(ExecResult::failure(1, "internal: stderr not available"));
        };

        // Capture stdout into a fixed 10MB tail-evicting ring (`BoundedStream`
        // + `drain_to_stream`) — the SAME capture primitive the production
        // spawn site uses (kernel.rs::try_execute_external), not the
        // limit-aware `spill_aware_collect` this used to call. Production
        // does not spill-check an external command's own capture inline
        // against `ctx.output_limit`; the pipeline-level post-hoc
        // `spill_if_needed` (`Kernel::execute_pipeline`) is what applies that
        // afterward, and `did_spill` is left `false` here for THAT reason — a
        // caller wanting the limit-aware post-hoc behavior applies it
        // separately, same as the real pipeline path (GH #133 item 2).
        // Independently, `did_spill` CAN still end up `true` below: if the
        // ring itself overflows (unconditionally, regardless of
        // `ctx.output_limit`), that's the GH #191 loud-overflow signal, not
        // the limit-aware spill this comment is about.
        let stdout_stream = Arc::new(crate::scheduler::BoundedStream::new(
            crate::scheduler::DEFAULT_STREAM_MAX_SIZE,
        ));
        let stdout_clone = stdout_stream.clone();
        let stdout_task = tokio::spawn(async move {
            crate::scheduler::drain_to_stream(child_stdout, stdout_clone).await;
        });

        // Stderr streaming is intentionally left as-is (live to
        // `ctx.stderr` when present, else buffered) — production instead
        // caps stderr into its own 10MB ring with no live streaming. That
        // divergence is out of scope for this PR; see GH #133 follow-ups.
        let stderr_stream_handle = ctx.stderr.clone();
        let stderr_task = tokio::spawn(async move {
            let mut buf = Vec::new();
            let mut chunk = [0u8; 8192];
            loop {
                match child_stderr.read(&mut chunk).await {
                    Ok(0) => break,
                    Ok(n) => {
                        if let Some(ref stream) = stderr_stream_handle {
                            stream.write(&chunk[..n]);
                        } else {
                            buf.extend_from_slice(&chunk[..n]);
                        }
                    }
                    Err(_) => break,
                }
            }
            if stderr_stream_handle.is_some() {
                String::new()
            } else {
                String::from_utf8_lossy(&buf).into_owned()
            }
        });

        let cancel = ctx.cancel.clone();
        // Mirror production's cancel-aware drain handling: spawn the
        // drains concurrently with the wait (not after collection
        // completes) so a cancel can actually interrupt a still-running,
        // still-silent child instead of blocking until it produces EOF.
        let cancelled_before_wait = cancel.is_cancelled();
        let status = crate::kernel::wait_or_kill(
            &mut child,
            kill_target.as_ref(),
            &cancel,
            std::time::Duration::from_secs(2),
        ).await;
        if let Some(task) = stdin_task { task.abort(); }
        let mut stderr = if cancelled_before_wait || cancel.is_cancelled() {
            // The child's pipes are gone; late output is lost but
            // predictable death beats partial capture (same tradeoff
            // production makes).
            stdout_task.abort();
            stderr_task.abort();
            String::new()
        } else {
            let _ = stdout_task.await;
            stderr_task.await.unwrap_or_default()
        };

        // Signal-death mapping (128+signal, e.g. SIGKILL→137) must match
        // the production spawn site exactly — kept in sync via the shared
        // `exit_code_from_status` helper (GH #133 item 1). A `wait_or_kill`
        // I/O error (not a signal death) falls back to 1, same as before.
        let code = match status {
            Ok(s) => crate::kernel::exit_code_from_status(&s),
            Err(_) => 1,
        };
        let stdout = stdout_stream.read().await;
        // stdout came back as raw bytes: text if valid UTF-8, else a Bytes
        // result (so `curl url`, `curl url > file.bin`, etc. keep binary intact).
        let mut result = ExecResult::success_text_or_bytes(stdout).with_code(code);

        // Mirror production's overflow signaling (GH #191) for the piece this
        // twin actually shares with `kernel.rs::try_execute_external`: the
        // stdout `BoundedStream` ring. Stderr here is captured differently
        // from production (live-streamed to `ctx.stderr` when set, else an
        // unbounded `Vec` — see the comment above `stderr_stream_handle`,
        // GH #133 follow-up), so there is no stderr `BoundedStream` overflow
        // to mirror; only the stdout side applies. `did_spill` stays `false`
        // otherwise, matching `output_limit_is_not_applied_inline_matching_production`
        // below — this is the fixed-ring overflow signal, not the
        // limit-aware post-hoc spill Kernel::execute_pipeline applies.
        if stdout_stream.has_overflowed().await {
            let stats = stdout_stream.stats().await;
            stderr = format!("{}{stderr}", stats.overflow_marker("stdout"));
            result.did_spill = true;
        }
        result.err = stderr;
        Some(result)
    }
}

#[cfg(test)]
#[async_trait]
impl CommandDispatcher for BackendDispatcher {
    async fn dispatch(&self, cmd: &Command, ctx: &mut ExecContext) -> Result<ExecResult> {
        // Handle built-in true/false
        match cmd.name.as_str() {
            "true" => return Ok(ExecResult::success("")),
            "false" => return Ok(ExecResult::failure(1, "")),
            _ => {}
        }

        // Build tool args with schema-aware parsing (sync — no command substitution).
        // A bad/subscripted collection access is a genuine PathError here too —
        // propagate it via `?` rather than swallowing, same as the production
        // Kernel::dispatch_command's `execute_command(..).await?`.
        let schema = self.tools.get(&cmd.name).map(|t| t.schema());
        let tool_args = build_tool_args(&cmd.args, ctx, schema.as_ref())
            .map_err(|e| anyhow::anyhow!(e))?;

        // Honor --json before the tool runs so a parse failure inside the
        // builtin doesn't drop the format on the floor. See kernel.rs for the
        // matching call in the production path.
        GlobalFlags::apply_from_args(&tool_args, ctx);

        // Execute via backend
        let backend = ctx.backend.clone();
        let result = match backend.call_tool(&cmd.name, tool_args, ctx).await {
            // Route through the same `From<ToolResult> for ExecResult` the
            // production dispatch path uses (kernel.rs) rather than
            // hand-rolling the field-by-field copy: the old inline version
            // wrapped `data` unconditionally as `Value::Json`, which skipped
            // `json_to_value_no_envelope`'s scalar-unwrap (`Value::Int`/
            // `Value::String`/…) and silently dropped `did_spill`/
            // `original_code` — a divergence this test-only dispatcher must
            // not have from the real path (GH #93 item 4).
            Ok(tool_result) => ExecResult::from(tool_result),
            Err(BackendError::ToolNotFound(_)) => {
                // Fall back to external command execution
                match self.try_external(&cmd.name, &cmd.args, ctx).await {
                    Some(result) => result,
                    None => ExecResult::failure(127, format!("command not found: {}", cmd.name)),
                }
            }
            Err(e) => ExecResult::failure(127, e.to_string()),
        };

        // Migrated builtins parse --json via the GlobalFlags flatten and
        // write ctx.output_format. The kernel just applies it.
        let result = match ctx.output_format {
            Some(format) => apply_output_format(result, format),
            None => result,
        };

        Ok(result)
    }

    /// Sync-only evaluation (no command substitution) — matches this
    /// test dispatcher's documented "no async argument evaluation" limit.
    async fn eval_expr(&self, expr: &Expr, ctx: &ExecContext) -> Result<Value> {
        crate::scheduler::pipeline::eval_simple_expr(expr, ctx)
            .map_err(|e| anyhow::anyhow!(e))?
            .ok_or_else(|| anyhow::anyhow!("cannot evaluate expression in test dispatcher"))
    }

    /// BackendDispatcher is stateless, so a fork is just a clone.
    async fn fork(&self) -> Arc<dyn CommandDispatcher> {
        Arc::new(Self { tools: Arc::clone(&self.tools) })
    }
}

/// Tests that spawn real external processes through `try_external`, to catch
/// behavioral drift from the production spawn site (`kernel.rs::try_execute_external`)
/// — GH #133. Unlike the `BackendDispatcher` tests in `scheduler::pipeline`,
/// which exercise builtins over a `MemoryFs` (virtual cwd, so `try_external`
/// never spawns), these give the dispatcher a real tempdir cwd + PATH so the
/// external fallback actually runs a child process.
#[cfg(all(test, feature = "subprocess"))]
mod external_process_tests {
    // Test-fixture helpers (not `#[test]` bodies themselves), so the
    // workspace's usual allow-in-tests clippy.toml carve-out doesn't cover
    // them — see CLAUDE.md's "clap builtin gotchas" / test-code conventions.
    #![allow(clippy::unwrap_used, clippy::expect_used)]
    use super::*;
    use crate::ast::{Arg, Command, Expr, Value};
    use crate::tools::{ExecContext, ToolRegistry};
    use crate::vfs::{LocalFs, VfsRouter};

    /// A `BackendDispatcher` + `ExecContext` rooted at a real tempdir, with an
    /// empty tool registry (every command name falls through to
    /// `try_external`, exactly like a real external command with no matching
    /// builtin/user tool) and PATH seeded from the test process's own OS env.
    /// Reading OS env here is fixture code, not kaish's hermetic runtime — see
    /// CLAUDE.md and `external_command_tests.rs::repl_kernel`.
    fn real_cwd_dispatcher() -> (BackendDispatcher, ExecContext, tempfile::TempDir) {
        let dir = tempfile::tempdir().expect("tempdir");
        let mut vfs = VfsRouter::new();
        vfs.mount("/", LocalFs::new(dir.path().to_path_buf()));
        let tools = Arc::new(ToolRegistry::new());
        let mut ctx = ExecContext::with_vfs_and_tools(Arc::new(vfs), tools.clone());
        // Exported (not just set): try_external's own PATH lookup reads
        // ctx.scope directly, but the CHILD process only inherits exported
        // vars (cmd.env_clear() + exported_vars()) — a script that shells
        // out further (`sh -c "yes | head"`) needs PATH in ITS env too,
        // not just kaish's resolver.
        ctx.scope.set_exported(
            "PATH",
            Value::String(std::env::var("PATH").unwrap_or_default()),
        );
        let dispatcher = BackendDispatcher::new(tools);
        (dispatcher, ctx, dir)
    }

    /// `sh -c <script>` as a `Command`, matching how the parser would build it
    /// from `sh -c 'script'` (a short flag, then a positional literal).
    fn sh_cmd(script: &str) -> Command {
        Command {
            name: "sh".to_string(),
            args: vec![
                Arg::ShortFlag("c".to_string()),
                Arg::Positional(Expr::Literal(Value::String(script.to_string()))),
            ],
            redirects: vec![],
        }
    }

    /// GH #133 item 1: production maps a signal-killed child to `128 + signal`
    /// (SIGKILL -> 137); the twin used to hardcode `code().unwrap_or(1)` -> 1,
    /// so a cancel/timeout test run through this dispatcher observed an exit
    /// code production never actually produces. Fails at `code == 1` pre-fix.
    #[tokio::test]
    async fn signal_killed_child_maps_to_128_plus_signal() {
        let (dispatcher, mut ctx, _dir) = real_cwd_dispatcher();
        let cmd = sh_cmd("kill -KILL $$");
        let result = dispatcher.dispatch(&cmd, &mut ctx).await.expect("dispatch");
        assert_eq!(
            result.code, 137,
            "SIGKILL should map to 128+9=137 (production's mapping), got {}",
            result.code
        );
    }

    /// GH #133 item 2: the twin used to call the limit-aware
    /// `spill_aware_collect` in its non-pipe capture branch, applying
    /// `ctx.output_limit` inline and setting `did_spill` itself. Production's
    /// `try_execute_external` never spill-checks its own capture that way —
    /// spill is a pipeline-level, post-hoc step (`Kernel::execute_pipeline`
    /// calls `spill_if_needed` AFTER the dispatcher returns). So even with a
    /// tiny `output_limit` configured, `try_external` itself must return the
    /// full (up to the 10MB ring) captured output with `did_spill == false`.
    /// Pre-fix, the twin truncated inline and set `did_spill = true` here.
    #[tokio::test]
    async fn output_limit_is_not_applied_inline_matching_production() {
        let (dispatcher, mut ctx, _dir) = real_cwd_dispatcher();
        // A tiny in-memory limit (no disk spill file — CLAUDE.md: no real
        // system paths in tests) — if try_external still spill-checked
        // inline (the bug), this would trigger truncation right here.
        ctx.output_limit = crate::output_limit::OutputLimitConfig::agent().in_memory();
        ctx.output_limit.set_limit(Some(64));

        let cmd = sh_cmd("yes x | head -c 1000");
        let result = dispatcher.dispatch(&cmd, &mut ctx).await.expect("dispatch");

        assert_eq!(result.code, 0, "err: {}", result.err);
        assert_eq!(
            result.text_out().len(),
            1000,
            "try_external must return the full captured output — production \
             defers spill to the post-hoc pipeline step, not its own capture; \
             got {} bytes: {:?}",
            result.text_out().len(),
            result.text_out()
        );
        assert!(
            !result.did_spill,
            "try_external itself must not set did_spill — that's \
             Kernel::execute_pipeline's post-hoc spill_if_needed's job, \
             matching production"
        );
    }

    /// GH #133 item 3: before this fix, `try_external` special-cased
    /// `ctx.pipe_stdout` — taking it out of the context and hand-streaming
    /// the child's stdout straight into it in 8KB chunks, bypassing the
    /// capture logic a non-pipeline external goes through, and always
    /// returning an empty `result.out` ("output was streamed to pipe").
    /// Production's `try_execute_external` has no such special case: it never
    /// reads or writes `ctx.pipe_stdout` at all — `PipelineRunner::run_pipeline`
    /// (pipeline.rs) is solely responsible for reading a stage's captured
    /// `result.out` back out and forwarding it to the next stage.
    #[tokio::test]
    async fn pipeline_stage_leaves_pipe_stdout_for_the_runner_to_forward() {
        let (dispatcher, mut ctx, _dir) = real_cwd_dispatcher();

        // Simulate what PipelineRunner::run_pipeline wires onto a first/middle
        // stage's ctx before calling dispatch(): a pipe_stdout the runner
        // expects to read back out afterward.
        let (writer, reader) = crate::scheduler::pipe_stream_default();
        ctx.pipe_stdout = Some(writer);

        // Drain the reader concurrently — a full-fidelity writer (the old
        // special case) would otherwise still work here for a small payload,
        // but this also lets the pipe close out cleanly either way.
        let drain = tokio::spawn(async move {
            use tokio::io::AsyncReadExt;
            let mut reader = reader;
            let mut buf = Vec::new();
            let _ = reader.read_to_end(&mut buf).await;
            buf
        });

        let cmd = sh_cmd("echo hello");
        // A generous but bounded timeout: a real hang here (e.g. an
        // accidental deadlock reintroduced by a future edit) should fail
        // loud and fast in CI, not stall the suite indefinitely.
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(15),
            dispatcher.dispatch(&cmd, &mut ctx),
        )
        .await
        .expect("dispatch timed out")
        .expect("dispatch");

        assert!(
            ctx.pipe_stdout.is_some(),
            "try_external must leave ctx.pipe_stdout untouched — forwarding \
             to the next stage is PipelineRunner's job, matching production, \
             which never reads or writes this field at all"
        );

        // Drop the writer now (the runner would take it back out and, after
        // forwarding, let it go) so the reader sees EOF and `drain` actually
        // completes — nothing else in this test closes the pipe, since
        // try_external no longer touches it at all post-fix.
        drop(ctx.pipe_stdout.take());
        let _ = drain.await;

        assert!(
            result.text_out().contains("hello"),
            "try_external must capture and return stdout the same way for a \
             pipeline stage as a non-pipeline call (not force it empty \
             because a pipe was attached) — got: {:?}",
            result.text_out()
        );
    }

    /// GH #133 item 3, large-payload consequence: before this fix, a pipeline
    /// stage's stdout went through the hand-rolled full-fidelity streamer,
    /// which ignored any size cap entirely and forwarded byte-for-byte no
    /// matter the size — an intermediate stage had NO cap at all, of any
    /// kind. Post-fix, every stage (pipe or not) goes through the same
    /// capture path a non-pipeline external uses.
    ///
    /// Updated for GH #133 item 2 (landed since this test was written): the
    /// shared capture path now caps via an *unconditional* ~10MB
    /// `BoundedStream` ring regardless of `ctx.output_limit` configuration —
    /// production never spill-checks its own capture inline against
    /// `ctx.output_limit`, deferring THAT to the pipeline-level, post-hoc
    /// `spill_if_needed`. So `ctx.output_limit` is configured below only to
    /// prove it's inert here (matching item 2's contract) — it plays no part
    /// in why this payload gets capped.
    ///
    /// Updated again for GH #191: the fixed ring overflowing IS now loud on
    /// its own terms, independent of `ctx.output_limit`. `did_spill` flips to
    /// `true` (this dispatcher calling `dispatch()` directly, not through
    /// `Kernel::execute_pipeline`, is exactly why `code` stays `0` here — the
    /// exit-3 remap lives in that caller, not in `try_external` itself), and
    /// stderr carries a truncation marker. Stdout still comes back as a
    /// clean, marker-free tail — the marker is never prepended into stdout
    /// (which may be binary), only into stderr. This test still pins the
    /// piece item 3 alone is responsible for: a pipeline stage is no longer
    /// special-cased into a no-cap-of-any-kind fast path.
    #[tokio::test]
    async fn oversized_pipeline_stage_output_is_no_longer_forwarded_losslessly() {
        let (dispatcher, mut ctx, _dir) = real_cwd_dispatcher();

        ctx.output_limit = crate::output_limit::OutputLimitConfig::agent().in_memory();
        ctx.output_limit.set_limit(Some(1024)); // tiny vs. the >10MB payload below

        let (writer, reader) = crate::scheduler::pipe_stream_default();
        ctx.pipe_stdout = Some(writer);

        // Drain the pipe concurrently — a full-fidelity writer would
        // otherwise block on the 64KB pipe capacity well before finishing an
        // 11MB write, deadlocking the test.
        let drain = tokio::spawn(async move {
            use tokio::io::AsyncReadExt;
            let mut reader = reader;
            let mut buf = Vec::new();
            let _ = reader.read_to_end(&mut buf).await;
            buf
        });

        let cmd = sh_cmd("yes x | head -c 11000000");
        // A generous but bounded timeout: a real hang here should fail loud
        // and fast in CI, not stall the suite indefinitely.
        let result = tokio::time::timeout(
            std::time::Duration::from_secs(15),
            dispatcher.dispatch(&cmd, &mut ctx),
        )
        .await
        .expect("dispatch timed out")
        .expect("dispatch");

        // Drop the writer (try_external no longer touches it post-fix, so
        // nothing else will) so the reader sees EOF and `drain` completes.
        drop(ctx.pipe_stdout.take());
        let _ = drain.await;

        // The exit-3 remap lives in `Kernel::execute_pipeline` (`if
        // result.did_spill { code = 3 }`), which this test never calls —
        // it drives `dispatcher.dispatch()` directly. So `code` stays the
        // child's own exit status (0) even though `did_spill` is now `true`.
        assert_eq!(result.code, 0, "err: {}", result.err);
        assert!(
            result.text_out().len() < 11_000_000,
            "an oversized (~11MB) pipeline stage's output must now be capped, \
             not forwarded byte-for-byte losslessly — the pre-fix special \
             case ignored any cap entirely; post-fix it goes through the same \
             capped capture (the unconditional ~10MB ring) a non-pipeline \
             external uses. got {} bytes",
            result.text_out().len()
        );
        assert!(
            !result.text_out().contains("truncated"),
            "the loud-overflow marker (GH #191) must never contaminate stdout \
             — it belongs in stderr only, since stdout may be binary: got {:?}",
            &result.text_out()[..result.text_out().len().min(80)]
        );
        assert!(
            result.did_spill,
            "the fixed ~10MB ring overflowing must set did_spill (GH #191) so \
             a real `Kernel::execute_pipeline` caller remaps to exit 3 — this \
             is independent of ctx.output_limit's own spill_if_needed, which \
             stays out of scope for try_external as before"
        );
        assert!(
            result.err.contains("stdout truncated"),
            "stderr must carry the loud overflow marker (GH #191): {}",
            result.err
        );
    }

    /// GH #133 item 4: production always puts the spawned child in its own
    /// process group (`setpgid(0,0)` in `pre_exec`) so a cancel's `killpg`
    /// reaches the whole tree — the direct child AND any grandchildren it
    /// spawns. Pre-fix, this dispatcher never called `setpgid`, so `killpg`
    /// targeted a process group nobody was actually in (an ESRCH no-op): a
    /// grandchild survived cancellation even though the direct child died.
    /// Any existing test asserting "grandchild cleanup" against this
    /// dispatcher was passing trivially, verifying nothing real.
    ///
    /// # Why this test checks the structural fact, not an end-to-end kill
    ///
    /// The most faithful reproduction of the issue would background a
    /// grandchild (`sleep N &`), cancel mid-flight, and assert the
    /// grandchild dies too — pinning the exact "existing test passes
    /// trivially" symptom. That reproduction turned out to be **blocked by a
    /// separate, pre-existing ordering issue** in this dispatcher, not
    /// introduced by this PR: `try_external`'s output collection used to run
    /// to completion BEFORE `wait_or_kill` was even called, so cancellation
    /// had no observable effect until the child's stdout closed on its own —
    /// which, for a `sh -c '... & wait'` script producing no stdout, only
    /// happened once the whole script finished naturally. GH #133 item 2 (PR
    /// #152, already landed on main alongside this fix) restructured
    /// collection to run *concurrently* with `wait_or_kill`, matching
    /// production — an end-to-end grandchild-kill test is now meaningful and
    /// fast, and remains a natural follow-up. Until then, this test pins the
    /// concrete, fast, unconfounded consequence of *this* PR's diff: the
    /// spawned child's own pgid equals its own pid, i.e. `setpgid(0, 0)` in
    /// `pre_exec` actually took effect. `ps -p $$` runs and exits almost
    /// immediately, producing no stdout for kaish to block draining — so the
    /// ordering issue above never enters into it either way.
    #[cfg(unix)]
    #[tokio::test]
    async fn spawned_child_becomes_its_own_process_group_leader() {
        let tmp = tempfile::tempdir().expect("tempdir");
        let out_file = tmp.path().join("pgid_info");

        let (dispatcher, mut ctx, _dir) = real_cwd_dispatcher();

        // `$$` is the running shell's own PID; `ps -o pid=,pgid= -p $$`
        // reports that shell's pid and process-group id. If setpgid(0,0)
        // took effect in pre_exec (before `ps` even execs), the two must be
        // equal. Redirected straight to a file — sh's own captured stdout
        // (what kaish pipes) stays empty, so collection returns immediately.
        let script = format!("ps -o pid=,pgid= -p $$ > {}", out_file.display());
        let cmd = sh_cmd(&script);

        let result = tokio::time::timeout(
            std::time::Duration::from_secs(10),
            dispatcher.dispatch(&cmd, &mut ctx),
        )
        .await
        .expect("dispatch timed out")
        .expect("dispatch");
        assert_eq!(result.code, 0, "err: {}", result.err);

        let contents = std::fs::read_to_string(&out_file).expect("read pgid info");
        let mut fields = contents.split_whitespace();
        let pid: i32 = fields.next().expect("pid field").parse().expect("pid parse");
        let pgid: i32 = fields.next().expect("pgid field").parse().expect("pgid parse");

        assert_eq!(
            pid, pgid,
            "the spawned child's pgid must equal its own pid — setpgid(0,0) \
             in pre_exec should make it its own process-group leader (so a \
             later killpg reaches it and any of its own children), matching \
             production (kernel.rs::try_execute_external); got pid={pid} \
             pgid={pgid}"
        );
    }
}
