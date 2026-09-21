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
use std::sync::Arc;
use std::time::Duration;
use tokio::process::Command;
use tokio::sync::Mutex;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::builtin::get_path_string;
use crate::tools::{exec_context,
    external_commands_unavailable_error, schema_from_clap, ExternalCommandsUnavailable,
    GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema,
};

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

    /// Timeout in milliseconds. On expiry the command exits 124 and keeps
    /// whatever the child already wrote; the timeout line is appended to
    /// stderr after the child's own.
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
        let ctx = exec_context(ctx);
        args.flagify_bool_named(&self.schema());

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("spawn: {e}")),
        };
        let parsed = match SpawnArgs::try_parse_from(
            std::iter::once("spawn".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("spawn: {e}")),
        };
        parsed.global.apply(ctx);

        if !ctx.allow_unwrapped_commands {
            // `spawn` is only registered when the `subprocess` capability is
            // compiled in (tools/builtin/mod.rs), so reaching here always
            // means the runtime config turned it off, never that the
            // capability is missing — `ConfiguredOff` is the only reachable
            // reason. Exit 127, same as PATH lookup and `env CMD` — one
            // refusal shape for all four gated sites.
            return external_commands_unavailable_error("spawn", ExternalCommandsUnavailable::ConfiguredOff);
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
        // Backstop: kill the OS process if this Command/Child is dropped
        // before we have waited on it — an early return between the spawn
        // and the wait would otherwise leave the child running, a real leak
        // for a long-lived agent. The timeout arm below kills explicitly so
        // it can read the child's partial output first. Mirrors the same
        // call in dispatch.rs and the "backstop" kill_on_drop in kernel.rs.
        cmd.kill_on_drop(true);

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
        let stdin_data = match ctx.read_stdin_to_bytes().await {
            Ok(d) => d,
            Err(e) => return ExecResult::failure(1, format!("spawn: {e}")),
        };
        cmd.stdin(if stdin_data.is_some() {
            std::process::Stdio::piped()
        } else {
            std::process::Stdio::null()
        });
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        // Put the child in its own process group, same mechanism the
        // shared spawner uses (`crate::spawn::spawn_process`'s
        // `setpgid(0, 0)` in `pre_exec`, also duplicated in
        // `dispatch.rs`) — so a timeout's kill below can reach a
        // grandchild the child backgrounded (`sh -c 'sleep 100 & wait'`),
        // not just the direct child. Without this, `--timeout` killed the
        // direct child and left the grandchild running, and if the
        // grandchild still held the stdout/stderr pipe open, the drains
        // below never reached EOF either.
        #[cfg(unix)]
        // SAFETY: setpgid is async-signal-safe per POSIX; safe to call
        // between fork and exec.
        #[allow(unsafe_code)]
        unsafe {
            cmd.pre_exec(|| {
                nix::unistd::setpgid(nix::unistd::Pid::from_raw(0), nix::unistd::Pid::from_raw(0))
                    .map_err(|e| std::io::Error::from_raw_os_error(e as i32))
            });
        }

        // Spawn the process
        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(e) => return ExecResult::failure(127, format!("spawn: {}: {}", command, e)),
        };
        // Captured right after spawn, before any wait/kill can reap the
        // child and clear `Child::id()`. `setpgid(0, 0)` above makes this
        // pid double as the child's own process-group id.
        #[cfg(unix)]
        let child_pgid = child.id().map(|id| nix::unistd::Pid::from_raw(id as i32));

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

            // Drain both pipes into tasks writing to shared buffers, rather
            // than letting `wait_with_output()` own them. That future owns
            // everything it has read, so the timeout arm used to drop the
            // child's partial output with it: a child that printed a
            // diagnostic and then hung reported 124 and nothing else. Shared
            // buffers also mean an aborted drain still leaves its bytes here.
            let captured_stdout = Arc::new(Mutex::new(Vec::new()));
            let captured_stderr = Arc::new(Mutex::new(Vec::new()));
            let stdout_task = tokio::spawn(drain_pipe(child.stdout.take(), captured_stdout.clone()));
            let stderr_task = tokio::spawn(drain_pipe(child.stderr.take(), captured_stderr.clone()));

            let mut kill_note = None;
            let (exit_code, expired) = match tokio::time::timeout(timeout, child.wait()).await {
                // A child that died by signal has no code. It is not a
                // timeout, and reading `None` as one would report 124 and a
                // "timed out" line for a segfault.
                Ok(Ok(status)) => (status.code(), false),
                Ok(Err(e)) => return ExecResult::failure(1, format!("spawn: failed to wait: {}", e)),
                Err(_) => {
                    // Kill first: a reader reaches EOF only once every write
                    // end of the pipe is closed. Kill the whole process
                    // group — `child.start_kill()` alone only reaches the
                    // direct child, and a grandchild it backgrounded (and
                    // any pipe write-end fd that grandchild inherited)
                    // would otherwise survive the timeout.
                    #[cfg(unix)]
                    let kill_result = match child_pgid {
                        Some(pgid) => nix::sys::signal::killpg(pgid, nix::sys::signal::Signal::SIGKILL)
                            .map_err(|e| std::io::Error::from_raw_os_error(e as i32)),
                        // No pid to target (spawn raced a reap) — nothing
                        // left to kill.
                        None => Ok(()),
                    };
                    #[cfg(not(unix))]
                    let kill_result = child.start_kill();
                    match kill_result {
                        // Unreachable for an unreaped child on Unix. Report
                        // it and skip the wait, which would have nothing to
                        // reap; `kill_on_drop` is still the backstop.
                        Err(e) => kill_note = Some(format!("spawn: failed to kill after timeout: {e}")),
                        Ok(()) => {
                            if let Err(e) = child.wait().await {
                                return ExecResult::failure(1, format!("spawn: failed to wait: {}", e));
                            }
                        }
                    }
                    (None, true)
                }
            };

            // Bounded join. The child is reaped, so everything it wrote is
            // already in the pipe buffer and the drains need only a moment
            // to pick it up. On Unix the process-group kill above reaches
            // a grandchild too, closing its copy of the write end — but
            // the kill and the drain race, and on a non-Unix target
            // `start_kill()` only ever reached the direct child, so a
            // grandchild holding the write end could still keep EOF from
            // arriving. The grace collects what is there and then stops
            // waiting either way; the bytes are in the shared buffers
            // regardless.
            let stdout = finish_drain(stdout_task, &captured_stdout).await;
            let mut stderr = finish_drain(stderr_task, &captured_stderr).await;

            if let Some(note) = kill_note {
                append_line(&mut stderr, note.as_bytes());
            }
            if expired {
                append_line(
                    &mut stderr,
                    format!("spawn: {}: timed out after {}ms", command, ms).as_bytes(),
                );
                // 124 is `timeout(1)`'s code, and the partial output the child
                // did produce rides along with it.
                capture_to_result(Some(124), stdout, stderr)
            } else {
                capture_to_result(exit_code, stdout, stderr)
            }
        } else {
            match child.wait_with_output().await {
                Ok(output) => capture_to_result(output.status.code(), output.stdout, output.stderr),
                Err(e) => ExecResult::failure(1, format!("spawn: failed to wait: {}", e)),
            }
        }
    }
}

/// How long a reaped child's drains get to pick up what is already buffered
/// before the wait is abandoned. Only a grandchild holding the pipe's write
/// end open makes this matter.
const DRAIN_GRACE: Duration = Duration::from_millis(200);

/// Read one child pipe to EOF into a shared buffer.
///
/// The buffer is shared so an abandoned drain still leaves the bytes it read.
/// A read error ends the drain and is returned rather than discarded.
async fn drain_pipe<R>(pipe: Option<R>, into: Arc<Mutex<Vec<u8>>>) -> Option<std::io::Error>
where
    R: tokio::io::AsyncRead + Unpin + Send + 'static,
{
    use tokio::io::AsyncReadExt;
    let mut pipe = pipe?;
    let mut chunk = [0u8; 8192];
    loop {
        match pipe.read(&mut chunk).await {
            Ok(0) => return None,
            Ok(n) => into.lock().await.extend_from_slice(&chunk[..n]),
            Err(e) => return Some(e),
        }
    }
}

/// Give a drain [`DRAIN_GRACE`] to finish, then take what it collected.
///
/// A read error or a panicked reader is appended to the bytes as a
/// diagnostic — neither is dropped, and neither replaces the output.
async fn finish_drain(
    mut task: tokio::task::JoinHandle<Option<std::io::Error>>,
    buffer: &Arc<Mutex<Vec<u8>>>,
) -> Vec<u8> {
    let note = match tokio::time::timeout(DRAIN_GRACE, &mut task).await {
        Ok(Ok(None)) => None,
        Ok(Ok(Some(e))) => Some(format!("spawn: failed to read child output: {e}")),
        Ok(Err(e)) => Some(format!("spawn: output reader did not finish: {e}")),
        // The grace elapsed: a surviving grandchild still holds the pipe's
        // write end, so EOF will not arrive. Abort rather than detach — a
        // detached reader would hold the pipe for the life of the process —
        // and say the output may be short rather than call it complete.
        Err(_) => {
            task.abort();
            Some("spawn: output may be incomplete: a surviving child still holds the pipe".to_string())
        }
    };
    let mut bytes = std::mem::take(&mut *buffer.lock().await);
    if let Some(note) = note {
        append_line(&mut bytes, note.as_bytes());
    }
    bytes
}

/// Append a line to captured output, adding the separator only when the
/// bytes already there do not end in one.
fn append_line(buffer: &mut Vec<u8>, line: &[u8]) {
    if !buffer.is_empty() && !buffer.ends_with(b"\n") {
        buffer.push(b'\n');
    }
    buffer.extend_from_slice(line);
    buffer.push(b'\n');
}

/// Build a result from a child's captured stdout/stderr: stdout keeps binary
/// intact (text if valid UTF-8, else a Bytes result); stderr stays text.
fn capture_to_result(code: Option<i32>, stdout: Vec<u8>, stderr: Vec<u8>) -> ExecResult {
    let mut result = ExecResult::success_text_or_bytes(stdout).with_code(code.unwrap_or(-1) as i64);
    result.err = String::from_utf8_lossy(&stderr).into_owned();
    result
}

/// Friendly, actionable error for the "nowhere to spawn" case: the shell's
/// cwd has no location on the real filesystem (a CoW overlay, an in-memory
/// VFS mount, `/dev`, etc.), so an external process has no real directory to
/// run in. Shared by both external-command spawn sites
/// (`kernel.rs::try_execute_external`, the production path, and
/// `dispatch.rs::BackendDispatcher::try_external`, its test-only twin) so the
/// wording can't drift between them — see CLAUDE.md's two-spawn-sites gotcha.
///
/// Deliberately hedges "if this is a CoW overlay" rather than asserting it:
/// this same guard fires for any virtual cwd (a plain in-memory VFS mount
/// too), and this call site has no way to tell which one it is.
pub fn virtual_cwd_error(name: &str, cwd: &Path) -> ExecResult {
    ExecResult::failure(
        127,
        format!(
            "{name}: can't run external commands here — \"{}\" has no location on \
             the real filesystem, so there's nowhere to spawn a child process. Use a \
             kaish builtin instead, or `cd` to a real directory first; if this is a \
             CoW overlay, `kaish-vfs commit` writes it to disk.",
            cwd.display()
        ),
    )
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
    use crate::tools::ExecContext;
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

    /// GH review residual: `spawn --timeout` used `Child::start_kill()`,
    /// which only signals the direct child — a grandchild the child
    /// backgrounded (`sh -c 'sleep 5 & wait'`) kept running past the
    /// timeout. The fix puts the child in its own process group
    /// (`setpgid(0, 0)` in `pre_exec`, the same mechanism
    /// `crate::spawn::spawn_process` uses) and kills the whole group on
    /// timeout, so the grandchild dies too.
    #[cfg(unix)]
    #[tokio::test]
    async fn timeout_kills_a_grandchild_the_child_backgrounded() {
        let mut ctx = make_ctx();
        let tmp = tempfile::tempdir().expect("tempdir");
        let pid_file = tmp.path().join("grandchild.pid");

        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("/bin/sh".into()));
        args.named.insert(
            "argv".to_string(),
            Value::String(format!(
                r#"["-c", "sleep 5 & echo $! > {} ; wait"]"#,
                pid_file.display()
            )),
        );
        // Well before the grandchild's own 5s sleep would finish on its own.
        args.named.insert("timeout".to_string(), Value::Int(300));

        let result = Spawn.execute(args, &mut ctx).await;
        assert_eq!(result.code, 124, "must report a timeout: {result:?}");

        let grandchild_pid: i32 = std::fs::read_to_string(&pid_file)
            .expect("grandchild pid file")
            .trim()
            .parse()
            .expect("pid file holds a pid");

        // The group kill and the OS actually reaping the process are not
        // synchronous with `spawn`'s return — poll briefly, bounded.
        let pid = nix::unistd::Pid::from_raw(grandchild_pid);
        let alive = |p: nix::unistd::Pid| nix::sys::signal::kill(p, None).is_ok();
        let deadline = std::time::Instant::now() + Duration::from_secs(2);
        while alive(pid) && std::time::Instant::now() < deadline {
            tokio::time::sleep(Duration::from_millis(20)).await;
        }
        assert!(
            !alive(pid),
            "grandchild pid {grandchild_pid} must be gone after the timeout's process-group kill"
        );
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
