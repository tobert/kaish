//! exec — Execute an external command.
//!
//! # Examples
//!
//! ```kaish
//! exec command="/usr/bin/jq" argv=["-r", ".foo"]
//! exec command="/bin/echo" argv=["hello", "world"]
//! exec command="/usr/bin/env" env={"MY_VAR": "value"}
//! exec command="cargo" cwd="/workspace"             # with working directory
//! exec command="sleep" argv=["10"] timeout=1000     # with 1 second timeout
//! ```

use async_trait::async_trait;
use std::path::Path;
use std::time::Duration;
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
                "Command to execute (name or path)",
            ))
            .param(ParamSchema::optional(
                "argv",
                "string",
                Value::Null,
                "Arguments as space-separated string or JSON array",
            ))
            .param(ParamSchema::optional(
                "env",
                "string",
                Value::Null,
                "Environment variables as JSON object string",
            ))
            .param(ParamSchema::optional(
                "cwd",
                "string",
                Value::Null,
                "Working directory for the command",
            ))
            .param(ParamSchema::optional(
                "timeout",
                "int",
                Value::Null,
                "Timeout in milliseconds (command killed if exceeded)",
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
        let command_name = match args.get_string("command", 0) {
            Some(cmd) => cmd,
            None => return ExecResult::failure(1, "exec: command parameter required"),
        };

        // Resolve command path (PATH lookup if not absolute)
        let command = if command_name.starts_with('/') || command_name.starts_with("./") {
            command_name.clone()
        } else {
            // Try to find in PATH
            let path_var = ctx
                .scope
                .get("PATH")
                .map(value_to_string)
                .unwrap_or_else(|| std::env::var("PATH").unwrap_or_default());

            match resolve_in_path(&command_name, &path_var) {
                Some(resolved) => resolved,
                None => command_name.clone(), // Fall back to name, let OS report error
            }
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

        // Get cwd (optional)
        let cwd = args.get_string("cwd", usize::MAX);

        // Get timeout (optional, in milliseconds)
        let timeout_ms: Option<u64> = args
            .get_named("timeout")
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as u64),
                Value::String(s) => s.parse().ok(),
                _ => None,
            });

        // Get clear_env flag
        let clear_env = args.has_flag("clear_env");

        // Build command
        let mut cmd = Command::new(&command);
        cmd.args(&argv);

        // Set working directory if specified
        if let Some(ref dir) = cwd {
            let resolved_cwd = ctx.resolve_path(dir);
            cmd.current_dir(&resolved_cwd);
        }

        if clear_env {
            cmd.env_clear();
        }

        for (key, value) in &env_vars {
            cmd.env(key, value);
        }

        // Handle stdin
        let stdin_data = ctx.take_stdin();
        cmd.stdin(if stdin_data.is_some() {
            std::process::Stdio::piped()
        } else {
            std::process::Stdio::null()
        });
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        // Spawn the process
        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(e) => return ExecResult::failure(127, format!("exec: {}: {}", command, e)),
        };

        // Write stdin if present
        if let Some(data) = stdin_data {
            if let Some(mut stdin) = child.stdin.take() {
                use tokio::io::AsyncWriteExt;
                if let Err(e) = stdin.write_all(data.as_bytes()).await {
                    return ExecResult::failure(1, format!("exec: failed to write stdin: {}", e));
                }
            }
        }

        // Wait with optional timeout
        if let Some(ms) = timeout_ms {
            let timeout = Duration::from_millis(ms);
            match tokio::time::timeout(timeout, child.wait_with_output()).await {
                Ok(Ok(output)) => {
                    let code = output.status.code().unwrap_or(-1) as i64;
                    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
                    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
                    ExecResult::from_output(code, stdout, stderr)
                }
                Ok(Err(e)) => ExecResult::failure(1, format!("exec: failed to wait: {}", e)),
                Err(_) => {
                    // Timeout - process is still running but we can't kill it
                    // because wait_with_output took ownership. Return timeout error.
                    ExecResult::failure(124, format!("exec: {}: timed out after {}ms", command, ms))
                }
            }
        } else {
            match child.wait_with_output().await {
                Ok(output) => {
                    let code = output.status.code().unwrap_or(-1) as i64;
                    let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
                    let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
                    ExecResult::from_output(code, stdout, stderr)
                }
                Err(e) => ExecResult::failure(1, format!("exec: failed to wait: {}", e)),
            }
        }
    }
}

/// Resolve a command name in PATH.
fn resolve_in_path(name: &str, path_var: &str) -> Option<String> {
    for dir in path_var.split(':') {
        if dir.is_empty() {
            continue;
        }

        let full_path = format!("{}/{}", dir, name);
        let path = Path::new(&full_path);

        if path.is_file() {
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                if let Ok(metadata) = path.metadata() {
                    let mode = metadata.permissions().mode();
                    if mode & 0o111 != 0 {
                        return Some(full_path);
                    }
                }
            }

            #[cfg(not(unix))]
            {
                return Some(full_path);
            }
        }
    }

    None
}

/// Convert a Value to a string.
fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => String::new(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
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

    #[tokio::test]
    async fn test_exec_path_resolution() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        // Use command name instead of full path
        args.named
            .insert("command".to_string(), Value::String("echo".into()));
        args.named.insert(
            "argv".to_string(),
            Value::String("hello from PATH".into()),
        );

        let result = Exec.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("hello from PATH"));
    }

    #[tokio::test]
    async fn test_exec_with_cwd() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("pwd".into()));
        args.named
            .insert("cwd".to_string(), Value::String("/tmp".into()));

        let result = Exec.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Output should contain /tmp (or its resolved path)
        assert!(result.out.contains("/tmp") || result.out.contains("tmp"));
    }

    #[tokio::test]
    async fn test_exec_with_timeout() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("sleep".into()));
        args.named
            .insert("argv".to_string(), Value::String("10".into()));
        // Timeout after 100ms
        args.named
            .insert("timeout".to_string(), Value::Int(100));

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 124); // Timeout exit code
        assert!(result.err.contains("timed out"));
    }

    #[tokio::test]
    async fn test_exec_no_timeout_when_fast() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("echo".into()));
        args.named
            .insert("argv".to_string(), Value::String("quick".into()));
        // Long timeout that won't trigger
        args.named
            .insert("timeout".to_string(), Value::Int(10000));

        let result = Exec.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("quick"));
    }
}
