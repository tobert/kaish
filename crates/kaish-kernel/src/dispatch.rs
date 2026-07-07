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
        // pipe_stdout checked later when deciding buffered vs streaming output
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

        // Stream stdout: copy child stdout → pipe_stdout in chunks (bounded memory)
        if let Some(mut pipe_out) = ctx.pipe_stdout.take() {
            // Safety: stdout/stderr were set to piped() above, so take() always returns Some
            let Some(mut child_stdout) = child.stdout.take() else {
                return Some(ExecResult::failure(1, "internal: stdout not available"));
            };
            let Some(mut child_stderr_reader) = child.stderr.take() else {
                return Some(ExecResult::failure(1, "internal: stderr not available"));
            };
            // Stream stderr to the kernel's stderr stream (if available) for
            // real-time delivery. Otherwise buffer with a cap.
            let stderr_stream_handle = ctx.stderr.clone();
            let stderr_task = tokio::spawn(async move {
                let mut buf = Vec::new();
                let mut chunk = [0u8; 8192];
                loop {
                    match child_stderr_reader.read(&mut chunk).await {
                        Ok(0) => break,
                        Ok(n) => {
                            if let Some(ref stream) = stderr_stream_handle {
                                // Stream raw bytes — no decode here, lossy decode at drain site
                                stream.write(&chunk[..n]);
                            } else {
                                buf.extend_from_slice(&chunk[..n]);
                            }
                        }
                        Err(_) => break,
                    }
                }
                if stderr_stream_handle.is_some() {
                    // Already streamed — return empty
                    String::new()
                } else {
                    String::from_utf8_lossy(&buf).into_owned()
                }
            });

            // Copy child stdout → pipe_stdout in chunks
            let mut buf = [0u8; 8192];
            loop {
                match child_stdout.read(&mut buf).await {
                    Ok(0) => break,
                    Ok(n) => {
                        if pipe_out.write_all(&buf[..n]).await.is_err() {
                            break; // next stage dropped its reader (broken pipe)
                        }
                    }
                    Err(_) => break,
                }
            }
            let _ = pipe_out.shutdown().await;
            drop(pipe_out);
            let cancel = ctx.cancel.clone();
            let status = crate::kernel::wait_or_kill(
                &mut child,
                kill_target.as_ref(),
                &cancel,
                std::time::Duration::from_secs(2),
            ).await;
            // Child has exited (naturally or via kill). Abort the stdin writer
            // (nothing more to feed a dead child). Let the stderr drain FINISH
            // — the child's stderr pipe EOFs now that it exited, so awaiting it
            // captures all stderr; aborting first would truncate it. Only abort
            // the drain if we were cancelled (then we don't care about output).
            if let Some(task) = stdin_task { task.abort(); }
            if cancel.is_cancelled() {
                stderr_task.abort();
            }
            let stderr = stderr_task.await.unwrap_or_default();
            let code = status.map(|s| s.code().unwrap_or(1) as i64).unwrap_or(1);
            // Output was streamed to pipe, so result.out is empty
            Some(ExecResult::from_output(code, String::new(), stderr))
        } else {
            // No pipe_stdout — last stage or non-pipeline.
            // Use spill-aware collection if output limits are configured.
            let Some(child_stdout) = child.stdout.take() else {
                return Some(ExecResult::failure(1, "internal: stdout not available"));
            };
            let Some(child_stderr) = child.stderr.take() else {
                return Some(ExecResult::failure(1, "internal: stderr not available"));
            };

            // Always use spill_aware_collect — it handles both limited and
            // unlimited modes, and correctly streams stderr to ctx.stderr.
            // (wait_with_output would bypass stderr streaming.)
            let (stdout, stderr, did_spill) = crate::output_limit::spill_aware_collect(
                child_stdout,
                child_stderr,
                ctx.stderr.clone(),
                &ctx.output_limit,
            ).await;

            let cancel = ctx.cancel.clone();
            let status = crate::kernel::wait_or_kill(
                &mut child,
                kill_target.as_ref(),
                &cancel,
                std::time::Duration::from_secs(2),
            ).await;
            if let Some(task) = stdin_task { task.abort(); }
            let code = status.map(|s| s.code().unwrap_or(1) as i64).unwrap_or(1);
            // stdout came back as raw bytes: text if valid UTF-8, else a Bytes
            // result (so `curl url`, `curl url > file.bin`, etc. keep binary intact).
            let mut result = ExecResult::success_text_or_bytes(stdout).with_code(code);
            result.err = stderr;
            result.did_spill = did_spill;
            Some(result)
        }
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
    /// trivially" symptom. That reproduction turns out to be **blocked by a
    /// separate, pre-existing ordering issue** in this dispatcher, not
    /// introduced by this PR: `try_external`'s output collection
    /// (`spill_aware_collect`) runs to completion BEFORE `wait_or_kill` is
    /// even called, so cancellation has no observable effect until the
    /// child's stdout closes on its own — which, for a `sh -c '... & wait'`
    /// script producing no stdout, only happens once the whole script
    /// naturally finishes. Cancelling earlier does nothing, and a test built
    /// on top of it would either hang for the sleep's full duration or (with
    /// a bounded timeout) fail even with this PR's `setpgid` fix correctly in
    /// place — a false negative caused by an unrelated bug, not evidence the
    /// fix doesn't work.
    ///
    /// This is exactly what GH #133 item 2 (a separate PR in this same
    /// batch) restructures: it moves collection to run *concurrently* with
    /// `wait_or_kill`, matching production, which spawns its drain tasks and
    /// immediately awaits `wait_or_kill` rather than draining to completion
    /// first. Once item 2 lands alongside this fix, cancellation reaches a
    /// still-running child promptly and an end-to-end grandchild-kill test
    /// becomes meaningful (and fast) — worth adding as a follow-up at that
    /// point. Until then, this test pins the concrete, fast, unconfounded
    /// consequence of *this* PR's diff: the spawned child's own pgid equals
    /// its own pid, i.e. `setpgid(0, 0)` in `pre_exec` actually took effect.
    /// `ps -p $$` runs and exits almost immediately, producing no stdout for
    /// kaish to block draining — so the ordering issue above never enters
    /// into it.
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
