//! spawn — Spawn an external command as a subprocess.
//!
//! Unlike `exec` (which replaces the process), `spawn` runs a command as a
//! child process and captures its output. Use this when you need explicit
//! control over env, cwd, timeout, or stdin piping.
//!
//! # Examples
//!
//! ```kaish
//! spawn --command /usr/bin/jq --argv '["-r", ".foo"]'
//! spawn --command /bin/echo --argv '["hello", "world"]'
//! spawn --command cargo --cwd /workspace              # with working directory
//! spawn --command sleep --argv 10 --timeout 1000      # with 1 second timeout
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;
use std::time::Duration;
use tokio::process::Command;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::builtin::get_path_string;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Spawn tool: runs an external command as a subprocess and captures output.
pub struct Spawn;

/// clap-derived argv layer for spawn.
#[derive(Parser, Debug)]
#[command(name = "spawn", about = "Spawn an external command as a subprocess")]
struct SpawnArgs {
    /// Command to execute (name or path).
    #[arg(long = "command")]
    command: Option<String>,

    /// Arguments as JSON array or single string.
    #[arg(long = "argv")]
    argv: Option<String>,

    /// Environment variables as JSON object string.
    #[arg(long = "env")]
    env: Option<String>,

    /// Working directory for the command.
    #[arg(long = "cwd")]
    cwd: Option<String>,

    /// Timeout in milliseconds.
    #[arg(long = "timeout")]
    timeout: Option<String>,

    /// Start with empty environment.
    #[arg(long = "clear-env", visible_alias = "clear_env")]
    clear_env: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Command and its arguments (alternative to `--command` / `--argv`).
    command_argv: Vec<String>,
}

#[async_trait]
impl Tool for Spawn {
    fn name(&self) -> &str {
        "spawn"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &SpawnArgs::command(),
            "spawn",
            "Spawn an external command as a subprocess",
            [
                ("Run a command", "spawn --command cargo --argv build"),
                ("With timeout", "spawn --command sleep --argv 10 --timeout 1000"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        args.flagify_bool_named(&self.schema());

        let parsed = match SpawnArgs::try_parse_from(
            std::iter::once("spawn".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("spawn: {e}")),
        };
        parsed.global.apply(ctx);

        if !ctx.allow_external_commands {
            return ExecResult::failure(1,
                "spawn: external commands are disabled (allow_external_commands=false)");
        }

        // Get command (required). A binary value goes loud rather than
        // silently being treated as "not given".
        let command_name = match get_path_string(&args, "command", 0) {
            Ok(Some(cmd)) => cmd,
            Ok(None) => return ExecResult::failure(1, "spawn: command parameter required"),
            Err(e) => return ExecResult::failure(1, format!("spawn: {e}")),
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

        // Get argv (optional). Decision D: a collection *element* (or a record
        // as the whole argv) can't cross the process boundary — loud, not a
        // silent JSON stringify. spawn's argv is legitimately a list of
        // strings, so only nested collections trip the guard.
        let argv = match args.get_named("argv").or_else(|| args.get_positional(1)) {
            Some(v) => match extract_string_array(v) {
                Ok(argv) => argv,
                Err(msg) => return ExecResult::failure(1, format!("spawn: {msg}")),
            },
            None => Vec::new(),
        };

        // Get env (optional)
        let env_vars = args
            .get_named("env")
            .map(extract_string_object)
            .unwrap_or_default();

        // Get cwd (optional). A binary value goes loud rather than silently
        // being treated as "no cwd override".
        let cwd = match get_path_string(&args, "cwd", usize::MAX) {
            Ok(c) => c,
            Err(e) => return ExecResult::failure(1, format!("spawn: {e}")),
        };

        // Get timeout (optional, in milliseconds). A malformed or negative
        // value is a usage error — the old parse().ok() fallback silently
        // DISABLED the timeout, the worst possible reading of a typo.
        let timeout_ms: Option<u64> = match args.get_named("timeout") {
            None => None,
            Some(Value::Int(i)) if *i >= 0 => Some(*i as u64),
            Some(Value::String(s)) => match s.parse::<u64>() {
                Ok(ms) => Some(ms),
                Err(_) => {
                    return ExecResult::failure(
                        2,
                        format!("spawn: invalid timeout '{s}': expected non-negative milliseconds"),
                    )
                }
            },
            Some(other) => {
                return ExecResult::failure(
                    2,
                    format!(
                        "spawn: invalid timeout '{}': expected non-negative milliseconds",
                        crate::interpreter::value_to_string(other)
                    ),
                )
            }
        };

        // Get clear_env flag
        let clear_env = args.has_flag("clear-env");

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

        // Handle stdin — forward raw bytes so binary survives into the child.
        let stdin_data = ctx.read_stdin_to_bytes().await;
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
                if let Err(e) = stdin.write_all(&data).await {
                    return ExecResult::failure(1, format!("spawn: failed to write stdin: {}", e));
                }
            }

        // Wait with optional timeout
        if let Some(ms) = timeout_ms {
            let timeout = Duration::from_millis(ms);
            match tokio::time::timeout(timeout, child.wait_with_output()).await {
                Ok(Ok(output)) => capture_to_result(output.status.code(), output.stdout, output.stderr),
                Ok(Err(e)) => ExecResult::failure(1, format!("spawn: failed to wait: {}", e)),
                Err(_) => {
                    // Timeout - process is still running but we can't kill it
                    // because wait_with_output took ownership. Return timeout error.
                    ExecResult::failure(124, format!("spawn: {}: timed out after {}ms", command, ms))
                }
            }
        } else {
            match child.wait_with_output().await {
                Ok(output) => capture_to_result(output.status.code(), output.stdout, output.stderr),
                Err(e) => ExecResult::failure(1, format!("spawn: failed to wait: {}", e)),
            }
        }
    }
}

/// Build a result from a child's captured stdout/stderr: stdout keeps binary
/// intact (text if valid UTF-8, else a Bytes result); stderr stays text.
fn capture_to_result(code: Option<i32>, stdout: Vec<u8>, stderr: Vec<u8>) -> ExecResult {
    let mut result = ExecResult::success_text_or_bytes(stdout).with_code(code.unwrap_or(-1) as i64);
    result.err = String::from_utf8_lossy(&stderr).into_owned();
    result
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
        Value::Bytes(b) => format!("[binary: {} bytes]", b.len()),
    }
}

/// Extract an array of strings from a Value.
///
/// Supports:
/// - JSON array (Value::Json): use elements directly
/// - JSON array string: parse and extract string items
/// - Plain string: one-element array (no implicit splitting)
///
/// Decision D: a nested-collection element (a list/record *inside* the argv
/// list), or a record used as the whole argv, is a loud error — never a silent
/// JSON stringify or a silently-dropped element. The top-level list itself is
/// legitimate (spawn's argv is a list of strings). Reuses the shared
/// `structured_boundary_error` so the message matches every other boundary.
fn extract_string_array(value: &Value) -> Result<Vec<String>, String> {
    match value {
        Value::Json(serde_json::Value::Array(arr)) => {
            let mut out = Vec::with_capacity(arr.len());
            for v in arr {
                if let Some(msg) = crate::interpreter::structured_boundary_error(
                    "a command argument",
                    &Value::Json(v.clone()),
                ) {
                    return Err(msg);
                }
                out.push(match v {
                    serde_json::Value::String(s) => s.clone(),
                    other => other.to_string(),
                });
            }
            Ok(out)
        }
        Value::Json(obj @ serde_json::Value::Object(_)) => Err(
            crate::interpreter::structured_boundary_error("a command argument", &Value::Json(obj.clone()))
                .unwrap_or_else(|| "argv must be a list of strings".to_string()),
        ),
        Value::String(s) => {
            // Try to parse as JSON array
            if s.starts_with('[')
                && let Ok(arr) = serde_json::from_str::<Vec<serde_json::Value>>(s) {
                    return Ok(arr
                        .iter()
                        .filter_map(|v| v.as_str().map(String::from))
                        .collect());
                }
            // Plain string is one argument — no implicit whitespace splitting
            Ok(vec![s.clone()])
        }
        _ => Ok(vec![]),
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
        assert_eq!(result.text_out().trim(), "hello");
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
        assert_eq!(&*result.text_out(), "hello world");
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
        args.flags.insert("clear-env".to_string());

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("MY_TEST_VAR=test_value"));
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
        assert!(result.text_out().contains("hello from PATH"));
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
        assert!(result.text_out().contains("tmp"), "expected tmp in output: {}", result.text_out());
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
        assert!(result.text_out().contains("quick"));
    }
}
