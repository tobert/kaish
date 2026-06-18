//! exec — Replace the current process (POSIX exec).
//!
//! Replaces the shell process with the specified command via `execvp`.
//! This never returns on success — the process image is replaced entirely.
//!
//! For subprocess spawning with output capture, env/cwd/timeout control,
//! use `spawn` instead.
//!
//! # Examples
//!
//! ```kaish
//! exec cargo build --release    # replace shell with cargo
//! exec /usr/bin/python3 app.py  # replace shell with python
//! exec bash                     # drop into bash, replacing kaish
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

use super::spawn::resolve_in_path;

/// Exec tool: replaces the current process (POSIX `exec`).
pub struct Exec;

/// clap-derived argv layer for exec.
///
/// `trailing_var_arg` + `allow_hyphen_values` because exec is a passthrough —
/// everything after the command name is the child's argv, including flags.
#[derive(Parser, Debug)]
#[command(name = "exec", about = "Replace the current process with a command (POSIX exec)")]
struct ExecArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Command to exec into, followed by its arguments.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    command_argv: Vec<String>,
}

#[async_trait]
impl Tool for Exec {
    fn name(&self) -> &str {
        "exec"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ExecArgs::command(),
            "exec",
            "Replace the current process with a command (POSIX exec)",
            [
                ("Replace shell with bash", "exec bash"),
                ("Replace shell with a command", "exec cargo build --release"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match ExecArgs::try_parse_from(
            std::iter::once("exec".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("exec: {e}")),
        };
        parsed.global.apply(ctx);

        if !ctx.allow_external_commands {
            return ExecResult::failure(1,
                "exec: external commands are disabled (allow_external_commands=false)");
        }

        // First positional is the command, rest are argv
        let command_name = match args.get_string("command", 0) {
            Some(cmd) => cmd,
            None => return ExecResult::failure(1, "exec: missing command"),
        };

        // Resolve command path
        let command = if command_name.starts_with('/') || command_name.starts_with("./") {
            command_name.clone()
        } else {
            let path_var = ctx
                .scope
                .get("PATH")
                .map(value_to_string)
                .unwrap_or_else(|| std::env::var("PATH").unwrap_or_default());

            match resolve_in_path(&command_name, &path_var) {
                Some(resolved) => resolved,
                None => {
                    return ExecResult::failure(
                        127,
                        format!("exec: {}: command not found", command_name),
                    )
                }
            }
        };

        // Remaining positionals become argv
        let argv: Vec<String> = args
            .positional
            .iter()
            .skip(1)
            .map(|v| match v {
                Value::String(s) => s.clone(),
                other => value_to_string(other),
            })
            .collect();

        // Platform-specific: Unix replaces the process, others error
        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt;
            let mut cmd = std::process::Command::new(&command);
            cmd.args(&argv);

            // exec() replaces the process — on success it never returns
            let err = cmd.exec();
            // If we get here, exec failed
            ExecResult::failure(126, format!("exec: {}: {}", command, err))
        }

        #[cfg(not(unix))]
        {
            let _ = (command, argv);
            ExecResult::failure(1, "exec: process replacement not supported on this platform")
        }
    }
}

/// Convert a Value to a string.
fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => String::new(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Json(json) => json.to_string(),
        Value::Bytes(b) => format!("[binary: {} bytes]", b.len()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_exec_missing_command() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing command"));
    }

    #[tokio::test]
    async fn test_exec_command_not_found() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("definitely_not_a_real_command_xyz".into()));

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 127);
        assert!(result.err.contains("command not found"));
    }

    #[tokio::test]
    async fn test_exec_absolute_path_not_found() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/nonexistent/binary".into()));

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok());
        // exec of nonexistent absolute path fails with 126 (exec error)
        assert_eq!(result.code, 126);
    }
}
