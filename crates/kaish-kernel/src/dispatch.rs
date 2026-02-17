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

use crate::ast::{Arg, Command, Expr, Value};
use crate::backend::BackendError;
use crate::interpreter::{apply_output_format, ExecResult};
use crate::scheduler::build_tool_args;
use crate::tools::{extract_output_format, resolve_in_path, ExecContext, ToolRegistry};

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
}

/// Fallback dispatcher that routes through `backend.call_tool()`.
///
/// This provides the same behavior as the old `PipelineRunner` — it dispatches
/// to builtins via the backend's tool registry. Used for background jobs and
/// scatter/gather workers until full `Arc<Kernel>` dispatch is wired up.
///
/// Limitations compared to the Kernel dispatcher:
/// - No user-defined tools
/// - No .kai script resolution
/// - No external command execution
/// - No async argument evaluation (command substitution in args won't work)
pub struct BackendDispatcher {
    tools: Arc<ToolRegistry>,
}

impl BackendDispatcher {
    /// Create a new backend dispatcher with the given tool registry.
    pub fn new(tools: Arc<ToolRegistry>) -> Self {
        Self { tools }
    }

    /// Try to execute an external command (PATH lookup + process spawn).
    ///
    /// Used as fallback when no builtin/backend tool matches. Returns None if
    /// the command is not found in PATH. Always captures stdout/stderr (never
    /// inherits terminal — pipeline stages don't need interactive I/O).
    async fn try_external(
        &self,
        name: &str,
        args: &[Arg],
        ctx: &mut ExecContext,
    ) -> Option<ExecResult> {
        // Get real working directory (needed for relative path resolution and child cwd)
        let real_cwd = ctx.backend.resolve_real_path(&ctx.cwd)
            .unwrap_or_else(|| std::path::PathBuf::from("/"));

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
            let path_var = ctx.scope.get("PATH")
                .map(crate::interpreter::value_to_string)
                .unwrap_or_else(|| std::env::var("PATH").unwrap_or_default());
            resolve_in_path(name, &path_var)?
        };

        // Build flat argv from args
        let argv: Vec<String> = args.iter().filter_map(|arg| {
            match arg {
                Arg::Positional(expr) => match expr {
                    Expr::Literal(Value::String(s)) => Some(s.clone()),
                    Expr::Literal(Value::Int(i)) => Some(i.to_string()),
                    Expr::Literal(Value::Float(f)) => Some(f.to_string()),
                    Expr::VarRef(path) => ctx.scope.resolve_path(path).map(|v| crate::interpreter::value_to_string(&v)),
                    _ => None,
                },
                Arg::ShortFlag(f) => Some(format!("-{f}")),
                Arg::LongFlag(f) => Some(format!("--{f}")),
                Arg::Named { key, value } => match value {
                    Expr::Literal(Value::String(s)) => Some(format!("{key}={s}")),
                    _ => Some(format!("{key}=")),
                },
                Arg::DoubleDash => Some("--".to_string()),
            }
        }).collect();

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

        let mut child = match cmd.spawn() {
            Ok(c) => c,
            Err(e) => return Some(ExecResult::failure(127, format!("{}: {}", name, e))),
        };

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
            // Buffered string stdin
            if let Some(mut child_stdin) = child.stdin.take() {
                let _ = child_stdin.write_all(data.as_bytes()).await;
                // Drop child_stdin signals EOF
            }
            None
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
            let status = child.wait().await;
            // Abort stdin copier if child exited (it may be blocked on pipe_in.read)
            if let Some(task) = stdin_task { task.abort(); }
            let stderr = stderr_task.await.unwrap_or_default();
            let code = status.map(|s| s.code().unwrap_or(1) as i64).unwrap_or(1);
            // Output was streamed to pipe, so result.out is empty
            Some(ExecResult::from_output(code, String::new(), stderr))
        } else {
            // No pipe_stdout — buffer output as before (last stage or non-pipeline)
            let result = match child.wait_with_output().await {
                Ok(output) => {
                    let code = output.status.code().unwrap_or(1) as i64;
                    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
                    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
                    Some(ExecResult::from_output(code, stdout, stderr))
                }
                Err(e) => Some(ExecResult::failure(1, format!("{}: {}", name, e))),
            };
            if let Some(task) = stdin_task { task.abort(); }
            result
        }
    }
}

#[async_trait]
impl CommandDispatcher for BackendDispatcher {
    async fn dispatch(&self, cmd: &Command, ctx: &mut ExecContext) -> Result<ExecResult> {
        // Handle built-in true/false
        match cmd.name.as_str() {
            "true" => return Ok(ExecResult::success("")),
            "false" => return Ok(ExecResult::failure(1, "")),
            _ => {}
        }

        // Build tool args with schema-aware parsing (sync — no command substitution)
        let schema = self.tools.get(&cmd.name).map(|t| t.schema());
        let mut tool_args = build_tool_args(&cmd.args, ctx, schema.as_ref());
        let output_format = extract_output_format(&mut tool_args, schema.as_ref());

        // Execute via backend
        let backend = ctx.backend.clone();
        let result = match backend.call_tool(&cmd.name, tool_args, ctx).await {
            Ok(tool_result) => {
                let mut exec = ExecResult::from_output(
                    tool_result.code as i64,
                    tool_result.stdout,
                    tool_result.stderr,
                );
                exec.output = tool_result.output;
                // Restore structured data from ToolResult (preserved through backend roundtrip)
                if let Some(json_data) = tool_result.data {
                    exec.data = Some(Value::Json(json_data));
                }
                exec
            }
            Err(BackendError::ToolNotFound(_)) => {
                // Fall back to external command execution
                match self.try_external(&cmd.name, &cmd.args, ctx).await {
                    Some(result) => result,
                    None => ExecResult::failure(127, format!("command not found: {}", cmd.name)),
                }
            }
            Err(e) => ExecResult::failure(127, e.to_string()),
        };

        // Apply output format transform
        let result = match output_format {
            Some(format) => apply_output_format(result, format),
            None => result,
        };

        Ok(result)
    }
}
