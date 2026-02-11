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

use crate::ast::Command;
use crate::backend::BackendError;
use crate::interpreter::{apply_output_format, ExecResult};
use crate::scheduler::build_tool_args;
use crate::tools::{extract_output_format, ExecContext, ToolRegistry};

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
                exec
            }
            Err(BackendError::ToolNotFound(_)) => {
                ExecResult::failure(127, format!("command not found: {}", cmd.name))
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
