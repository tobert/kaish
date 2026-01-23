//! exec — Execute an external command.
//!
//! # Examples
//!
//! ```kaish
//! exec command="/usr/bin/jq" argv=["-r", ".foo"]
//! exec command="/bin/echo" argv=["hello", "world"]
//! exec command="/usr/bin/env" env={"MY_VAR": "value"}
//! ```

use async_trait::async_trait;
use tokio::process::Command;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Exec tool: executes an external command.
pub struct Exec;

#[async_trait]
impl Tool for Exec {
    fn name(&self) -> &str {
        "exec"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("exec", "Execute an external command")
            .param(ParamSchema::required(
                "command",
                "string",
                "Path to the executable",
            ))
            .param(ParamSchema::optional(
                "argv",
                "string",
                Value::Null,
                "Arguments as space-separated string",
            ))
            .param(ParamSchema::optional(
                "env",
                "string",
                Value::Null,
                "Environment variables as JSON string",
            ))
            .param(ParamSchema::optional(
                "clear_env",
                "bool",
                Value::Bool(false),
                "Start with empty environment",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get command (required)
        let command = match args.get_string("command", 0) {
            Some(cmd) => cmd,
            None => return ExecResult::failure(1, "exec: command parameter required"),
        };

        // Get argv (optional)
        let argv = args
            .get_named("argv")
            .or_else(|| args.get_positional(1))
            .map(extract_string_array)
            .unwrap_or_default();

        // Get env (optional)
        let env_vars = args
            .get_named("env")
            .map(extract_string_object)
            .unwrap_or_default();

        // Get clear_env flag
        let clear_env = args.has_flag("clear_env");

        // Build command
        let mut cmd = Command::new(&command);
        cmd.args(&argv);

        if clear_env {
            cmd.env_clear();
        }

        for (key, value) in &env_vars {
            cmd.env(key, value);
        }

        // Handle stdin
        if let Some(stdin_data) = ctx.take_stdin() {
            cmd.stdin(std::process::Stdio::piped());
            cmd.stdout(std::process::Stdio::piped());
            cmd.stderr(std::process::Stdio::piped());

            let mut child = match cmd.spawn() {
                Ok(child) => child,
                Err(e) => return ExecResult::failure(127, format!("exec: failed to spawn: {}", e)),
            };

            // Write stdin
            if let Some(mut stdin) = child.stdin.take() {
                use tokio::io::AsyncWriteExt;
                if let Err(e) = stdin.write_all(stdin_data.as_bytes()).await {
                    return ExecResult::failure(1, format!("exec: failed to write stdin: {}", e));
                }
            }

            // Wait for completion
            match child.wait_with_output().await {
                Ok(output) => {
                    let code = output.status.code().unwrap_or(-1) as i64;
                    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
                    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
                    ExecResult::from_output(code, stdout, stderr)
                }
                Err(e) => ExecResult::failure(1, format!("exec: failed to wait: {}", e)),
            }
        } else {
            // No stdin
            cmd.stdout(std::process::Stdio::piped());
            cmd.stderr(std::process::Stdio::piped());

            match cmd.output().await {
                Ok(output) => {
                    let code = output.status.code().unwrap_or(-1) as i64;
                    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
                    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
                    ExecResult::from_output(code, stdout, stderr)
                }
                Err(e) => ExecResult::failure(127, format!("exec: failed to execute: {}", e)),
            }
        }
    }
}

/// Extract an array of strings from a Value.
///
/// Supports:
/// - String: split on whitespace (shell-style)
/// - JSON array string: parse and extract string items
fn extract_string_array(value: &Value) -> Vec<String> {
    match value {
        Value::String(s) => {
            // Try to parse as JSON array first
            if s.starts_with('[')
                && let Ok(arr) = serde_json::from_str::<Vec<serde_json::Value>>(s) {
                    return arr
                        .iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect();
                }
            // Otherwise split on whitespace
            s.split_whitespace().map(String::from).collect()
        }
        _ => vec![],
    }
}

/// Extract a string→string mapping from a Value.
///
/// Supports:
/// - String: parse as JSON object
fn extract_string_object(value: &Value) -> Vec<(String, String)> {
    match value {
        Value::String(s) => {
            if let Ok(obj) = serde_json::from_str::<serde_json::Map<String, serde_json::Value>>(s) {
                return obj
                    .iter()
                    .filter_map(|(k, v)| v.as_str().map(|s| (k.clone(), s.to_string())))
                    .collect();
            }
            vec![]
        }
        _ => vec![],
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
    async fn test_exec_echo() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("/bin/echo".into()));
        // Args are now space-separated strings or JSON arrays
        args.named.insert(
            "argv".to_string(),
            Value::String("hello".into()),
        );

        let result = Exec.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "hello");
    }

    #[tokio::test]
    async fn test_exec_with_stdin() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello world".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("/bin/cat".into()));

        let result = Exec.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello world");
    }

    #[tokio::test]
    async fn test_exec_with_env() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("/usr/bin/env".into()));
        // Env is now a JSON object string
        args.named.insert(
            "env".to_string(),
            Value::String(r#"{"MY_TEST_VAR": "test_value"}"#.into()),
        );
        args.flags.insert("clear_env".to_string());

        let result = Exec.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("MY_TEST_VAR=test_value"));
    }

    #[tokio::test]
    async fn test_exec_missing_command() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("command parameter required"));
    }

    #[tokio::test]
    async fn test_exec_nonexistent_command() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named.insert(
            "command".to_string(),
            Value::String("/nonexistent/command/path".into()),
        );

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 127);
    }
}
