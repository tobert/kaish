//! spawn — Spawn an external command as a subprocess.
//!
//! Unlike `exec` (which replaces the process), `spawn` runs a command as a
//! child process and captures its output. Use this when you need explicit
//! control over env, cwd, timeout, or stdin piping.
//!
//! # Examples
//!
//! ```kaish
//! spawn command="/usr/bin/jq" argv=["-r", ".foo"]
//! spawn command="/bin/echo" argv=["hello", "world"]
//! spawn command="/usr/bin/env" env={"MY_VAR": "value"}
//! spawn command="cargo" cwd="/workspace"             # with working directory
//! spawn command="sleep" argv=["10"] timeout=1000     # with 1 second timeout
//! ```

use async_trait::async_trait;
use std::path::Path;
use std::time::Duration;
use tokio::process::Command;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Spawn tool: runs an external command as a subprocess and captures output.
pub struct Spawn;

#[async_trait]
impl Tool for Spawn {
    fn name(&self) -> &str {
        "spawn"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("spawn", "Spawn an external command as a subprocess")
            .param(ParamSchema::required(
                "command",
                "string",
                "Command to execute (name or path)",
            ))
            .param(ParamSchema::optional(
                "argv",
                "string",
                Value::Null,
                "Arguments as JSON array (e.g. [\"arg1\", \"arg2\"]) or single string",
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
            .example("Run a command", "spawn command=\"cargo\" argv=[\"build\"]")
            .example("With timeout", "spawn command=\"sleep\" argv=[\"10\"] timeout=1000")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        if !ctx.allow_external_commands {
            return ExecResult::failure(1,
                "spawn: external commands are disabled (allow_external_commands=false)");
        }

        // Get command (required)
        let command_name = match args.get_string("command", 0) {
            Some(cmd) => cmd,
            None => return ExecResult::failure(1, "spawn: command parameter required"),
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
            let vfs_cwd = ctx.resolve_path(dir);
            // Resolve VFS path to real filesystem path
            let real_cwd = match ctx.backend.resolve_real_path(&vfs_cwd) {
                Some(p) => p,
                None => {
                    return ExecResult::failure(
                        1,
                        format!("spawn: cwd '{}' is not on a real filesystem", vfs_cwd.display()),
                    )
                }
            };
            cmd.current_dir(&real_cwd);
        }

        if clear_env {
            cmd.env_clear();
        }

        for (key, value) in &env_vars {
            cmd.env(key, value);
        }

        // Handle stdin
        let stdin_data = ctx.read_stdin_to_string().await;
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
            Err(e) => return ExecResult::failure(127, format!("spawn: {}: {}", command, e)),
        };

        // Write stdin if present
        if let Some(data) = stdin_data
            && let Some(mut stdin) = child.stdin.take() {
                use tokio::io::AsyncWriteExt;
                if let Err(e) = stdin.write_all(data.as_bytes()).await {
                    return ExecResult::failure(1, format!("spawn: failed to write stdin: {}", e));
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
                Ok(Err(e)) => ExecResult::failure(1, format!("spawn: failed to wait: {}", e)),
                Err(_) => {
                    // Timeout - process is still running but we can't kill it
                    // because wait_with_output took ownership. Return timeout error.
                    ExecResult::failure(124, format!("spawn: {}: timed out after {}ms", command, ms))
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
                Err(e) => ExecResult::failure(1, format!("spawn: failed to wait: {}", e)),
            }
        }
    }
}

/// Resolve a command name in PATH.
///
/// Searches each directory in `path_var` (colon-separated) for an executable
/// named `name`. Returns the full path if found.
pub fn resolve_in_path(name: &str, path_var: &str) -> Option<String> {
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
        Value::Json(json) => json.to_string(),
        Value::Blob(blob) => format!("[blob: {} {}]", blob.formatted_size(), blob.content_type),
    }
}

/// Extract an array of strings from a Value.
///
/// Supports:
/// - JSON array (Value::Json): use elements directly
/// - JSON array string: parse and extract string items
/// - Plain string: one-element array (no implicit splitting)
fn extract_string_array(value: &Value) -> Vec<String> {
    match value {
        Value::Json(serde_json::Value::Array(arr)) => {
            arr.iter().map(|v| match v {
                serde_json::Value::String(s) => s.clone(),
                other => other.to_string(),
            }).collect()
        }
        Value::String(s) => {
            // Try to parse as JSON array
            if s.starts_with('[')
                && let Ok(arr) = serde_json::from_str::<Vec<serde_json::Value>>(s) {
                    return arr
                        .iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect();
                }
            // Plain string is one argument — no implicit whitespace splitting
            vec![s.clone()]
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
    async fn test_spawn_echo() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("/bin/echo".into()));
        // Args are now space-separated strings or JSON arrays
        args.named.insert(
            "argv".to_string(),
            Value::String("hello".into()),
        );

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "hello");
    }

    #[tokio::test]
    async fn test_spawn_with_stdin() {
        let mut ctx = make_ctx();
        ctx.set_stdin("hello world".to_string());

        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("/bin/cat".into()));

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "hello world");
    }

    #[tokio::test]
    async fn test_spawn_with_env() {
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

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("MY_TEST_VAR=test_value"));
    }

    #[tokio::test]
    async fn test_spawn_missing_command() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("command parameter required"));
    }

    #[tokio::test]
    async fn test_spawn_nonexistent_command() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named.insert(
            "command".to_string(),
            Value::String("/nonexistent/command/path".into()),
        );

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 127);
    }

    #[tokio::test]
    async fn test_spawn_path_resolution() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        // Use command name instead of full path
        args.named
            .insert("command".to_string(), Value::String("echo".into()));
        args.named.insert(
            "argv".to_string(),
            Value::String(r#"["hello", "from", "PATH"]"#.into()),
        );

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("hello from PATH"));
    }

    #[tokio::test]
    async fn test_spawn_with_cwd() {
        // Need LocalFs for real path resolution (spawn cwd requires real filesystem)
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        vfs.mount("/tmp", crate::vfs::LocalFs::new("/tmp"));
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("pwd".into()));
        args.named
            .insert("cwd".to_string(), Value::String("/tmp".into()));

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(result.ok(), "spawn failed: {}", result.err);
        // Output should contain /tmp (or its resolved path like /private/tmp on macOS)
        assert!(result.out.contains("tmp"), "expected tmp in output: {}", result.out);
    }

    #[tokio::test]
    async fn test_spawn_with_timeout() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("sleep".into()));
        args.named
            .insert("argv".to_string(), Value::String("10".into()));
        // Timeout after 100ms
        args.named
            .insert("timeout".to_string(), Value::Int(100));

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 124); // Timeout exit code
        assert!(result.err.contains("timed out"));
    }

    #[tokio::test]
    async fn test_spawn_no_timeout_when_fast() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("echo".into()));
        args.named
            .insert("argv".to_string(), Value::String("quick".into()));
        // Long timeout that won't trigger
        args.named
            .insert("timeout".to_string(), Value::Int(10000));

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("quick"));
    }
}
