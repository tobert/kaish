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
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use std::time::Duration;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::spawn::{hermetic_env, OutputPolicy, SpawnContext, SpawnRequest, StdinPolicy};
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

    /// Environment variables added to the child, as a JSON object string.
    /// Applied on top of kaish's exported variables, or alone with
    /// `--clear-env`.
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

    /// Start the child with no environment, instead of kaish's exported
    /// variables. `--env` entries still apply on top.
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

        // Resolve command path (PATH lookup if not absolute). The kernel
        // never reads the OS env — a frontend that wants host PATH seeds it
        // via `initial_vars` (the REPL does, with `os_env_vars()`), same as
        // the external-command path (`try_execute_external_on_path`). No
        // PATH in scope means nothing resolves; refuse immediately with the
        // same "command not found" shape a PATH-miss reports there, rather
        // than falling through to a bare, unresolved name and letting the
        // OS's own exec report whatever it finds (or an OS-level PATH
        // default kaish has no control over).
        let command = if command_name.starts_with('/') || command_name.starts_with("./") {
            command_name.clone()
        } else {
            let path_var = ctx.scope.get("PATH").map(value_to_string).unwrap_or_default();
            match resolve_in_path(&command_name, &path_var) {
                Some(resolved) => resolved,
                None => {
                    return ExecResult::failure(127, format!("spawn: {}: command not found", command_name))
                }
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

        // Working directory: an explicit `--cwd` resolves through the VFS,
        // same as before. With none given, use the SHELL's cwd (`ctx.cwd`),
        // the same real-path resolution `try_execute_external_on_path` does
        // for every other external command — not the kaish process's own OS
        // cwd, which is a different directory once a script has `cd`ed.
        // `virtual_cwd_error` refuses loudly rather than silently falling
        // back to the process cwd when the shell's cwd has no real
        // filesystem location (an overlay, an in-memory VFS mount, `/dev`).
        let cwd_path = match &cwd {
            Some(dir) => {
                let vfs_cwd = ctx.resolve_path(dir);
                match ctx.backend.resolve_real_path(&vfs_cwd) {
                    Some(p) => p,
                    None => {
                        return ExecResult::failure(
                            1,
                            format!("spawn: cwd '{}' is not on a real filesystem", vfs_cwd.display()),
                        )
                    }
                }
            }
            None => match ctx.backend.resolve_real_path(&ctx.cwd) {
                Some(p) => p,
                None => return virtual_cwd_error(&command_name, &ctx.cwd),
            },
        };

        // Env: hermetic, matching `try_execute_external_on_path` — the
        // kernel never reads the OS env, so the child sees only what kaish
        // has exported (`hermetic_env`, fed by `KernelConfig::initial_vars`
        // and `export`), never this process's own ambient environment.
        // `--clear-env` drops even that, starting empty. `--env` entries
        // apply last either way, so they win on a name collision.
        let mut env: Vec<(String, String)> = if clear_env {
            Vec::new()
        } else {
            match hermetic_env(&ctx.scope) {
                Ok(env) => env,
                Err(e) => return ExecResult::failure(1, format!("spawn: {e}")),
            }
        };
        env.extend(env_vars);

        // Handle stdin — forward raw bytes so binary survives into the child.
        // `StdinPolicy::Null` (not an empty `Piped`) so a command reading
        // stdin sees immediate EOF rather than a pipe that never closes.
        let stdin_data = match ctx.read_stdin_to_bytes().await {
            Ok(d) => d,
            Err(e) => return ExecResult::failure(1, format!("spawn: {e}")),
        };
        let stdin = match stdin_data {
            Some(data) => StdinPolicy::Piped { prefix: Some(data), pipe: None },
            None => StdinPolicy::Null,
        };

        let mut spawn_ctx = SpawnContext::from_exec_context(ctx);

        // `--timeout` is spawn's own deadline, independent of whatever
        // cancellation the caller's `ctx.cancel` already carries. A child
        // token lets `spawn_process`'s SIGTERM-grace-SIGKILL cascade reach
        // this child on either signal, while `timed_out` tells the two
        // apart afterward: `ctx.cancel` itself is never touched, so a real
        // cancellation is still readable from it once `spawn_process`
        // returns. Mirrors the `timeout` builtin's own token derivation.
        //
        // The timer claims the kill only if it is the first cause: a
        // SIGTERM-ignoring child stays alive for the whole `kill_grace`
        // window a `kill %1` (or any other parent cancellation) already
        // opened, and a `--timeout` shorter than that grace elapses squarely
        // inside it. Storing `timed_out = true` unconditionally there
        // reported `killed:124` for a job that `kill %1` — not the deadline —
        // actually killed. Checking the PARENT token (never touched by this
        // swap) at the instant the timer wakes tells the two apart: if it is
        // already cancelled, the real cancellation reached this child's
        // token first via child-token propagation, and the timer backs off.
        let timed_out = Arc::new(AtomicBool::new(false));
        let timer = timeout_ms.map(|ms| {
            let deadline_token = spawn_ctx.cancel.child_token();
            spawn_ctx.cancel = deadline_token.clone();
            let timed_out = timed_out.clone();
            let parent_cancel = ctx.cancel.clone();
            tokio::spawn(async move {
                tokio::time::sleep(Duration::from_millis(ms)).await;
                if !parent_cancel.is_cancelled() {
                    timed_out.store(true, Ordering::SeqCst);
                    deadline_token.cancel();
                }
            })
        });

        let request = SpawnRequest {
            executable: PathBuf::from(&command),
            argv,
            cwd: cwd_path,
            env,
            stdin,
            output: OutputPolicy::Captured,
            label: format!("spawn: {command}"),
        };

        let mut result = crate::spawn::spawn_process(request, &spawn_ctx).await;
        if let Some(timer) = timer {
            timer.abort();
        }

        if timed_out.load(Ordering::SeqCst) {
            // 124 is `timeout(1)`'s code, and the partial output the child
            // did produce rides along with it — `spawn_process` keeps
            // whatever it captured before the kill.
            result.code = 124;
            append_line(
                &mut result.err,
                &format!("spawn: {}: timed out after {}ms", command, timeout_ms.unwrap_or_default()),
            );
        } else if ctx.cancel.is_cancelled() && !result.ok() {
            // 130 is the documented cancellation code (`sleep`'s own
            // `ctx.cancel` arm returns it the same way). A foreground call
            // gets 130 either way — `Kernel::execute_with_options`'s
            // top-level cancel normalization would rewrite any non-ok code
            // once `ctx.cancel` is observed cancelled — but a background
            // job (`spawn --command sleep --argv 60 &` then `kill %1`) never
            // runs that normalization: `execute_background` (kernel.rs)
            // sends the runner's result straight to `JobManager` with no
            // cancel-aware rewrite. Reporting 130 here directly, rather than
            // relying on a normalization only one of spawn's two callers
            // applies, is what makes `killed:130` true for both.
            //
            // `!result.ok()` matches the same guard `Kernel`'s own two cancel
            // sites use (kernel.rs's `execute_streaming_inner` and
            // `execute_with_options`): the token, not the code — a child
            // that already exited 0 before the token tripped keeps its
            // result, rather than a coincidental later cancellation
            // relabeling a real success as killed.
            result.code = 130;
            append_line(&mut result.err, &format!("spawn: {}: cancelled", command));
        }
        result
    }
}

/// Append a line to a text diagnostic, adding the separator only when the
/// text already there does not end in one.
fn append_line(buffer: &mut String, line: &str) {
    if !buffer.is_empty() && !buffer.ends_with('\n') {
        buffer.push('\n');
    }
    buffer.push_str(line);
    buffer.push('\n');
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

    /// `spawn`'s default cwd (no `--cwd` given) is now `ctx.cwd` resolved to
    /// a real filesystem location, matching the external-command path —
    /// so every test here needs one, not just `test_spawn_with_cwd`. `/tmp`
    /// is mounted as `LocalFs` and set as `ctx.cwd`, same real directory
    /// `test_spawn_with_cwd` already points `--cwd` at.
    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        vfs.mount("/tmp", crate::vfs::LocalFs::new("/tmp"));
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.cwd = PathBuf::from("/tmp");
        ctx
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

    /// Unit-level pin on the exact code path `hermetic_env(&ctx.scope)`
    /// takes: a plain (non-exported) scope var never reaches the child, an
    /// exported one does. The integration-level proof, through a real
    /// `Kernel` and `printenv`, lives in
    /// `external_command_tests.rs::spawn_child_does_not_see_an_unexported_os_var`
    /// and `spawn_child_sees_exported_and_initial_vars`.
    #[tokio::test]
    async fn test_spawn_env_is_hermetic_not_ambient() {
        let mut ctx = make_ctx();
        ctx.scope.set_global("NOT_EXPORTED", Value::String("leaked".into()));
        ctx.scope.set_exported("IS_EXPORTED", Value::String("visible".into()));

        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("/usr/bin/env".into()));

        let result = Spawn.execute(args, &mut ctx).await;
        assert!(result.ok(), "spawn failed: {}", result.err);
        assert!(
            !result.text_out().contains("NOT_EXPORTED"),
            "a non-exported scope var must not reach the child: {}",
            result.text_out()
        );
        assert!(
            result.text_out().contains("IS_EXPORTED=visible"),
            "an exported scope var must reach the child: {}",
            result.text_out()
        );
    }

    /// kaibo round-3 finding: the cancel arm rewrote ANY result to 130 on
    /// token state alone. `ctx.cancel` cancels from a concurrent task after
    /// 50ms, not before the call: `wait_or_kill` sends the SIGTERM the
    /// instant it observes a cancelled token, so cancelling before the
    /// child even runs raced its own `trap : TERM` installation and killed
    /// it before the trap existed to ignore anything — a genuine SIGTERM
    /// death (128+15), not the clean-exit case this test means to prove.
    /// 50ms is ample for `sh` to install the trap and be well into its
    /// (builtin-only, no forked `sleep`) busy loop before the cancel fires;
    /// the loop then ignores the SIGTERM and exits 0 on its own, comfortably
    /// inside the 20s grace (generous against a heavily oversubscribed box
    /// stretching the loop's own CPU time). A forked `sleep N` was tried
    /// first and rejected: it sits in `sh`'s own process group and dies
    /// from the group-wide SIGTERM on ITS OWN default disposition
    /// regardless of `sh`'s trap, and `sh` (POSIX, no `set -e`) would then
    /// just continue to exit 0 anyway — passing for the wrong reason,
    /// without the trap ever having to protect anything. The `!result.ok()`
    /// guard (matching `Kernel`'s own two cancel sites) must keep the clean
    /// exit, not relabel it as killed.
    #[tokio::test]
    async fn test_spawn_cancel_arm_keeps_a_clean_exit() {
        let mut ctx = make_ctx();
        ctx.kill_grace = Duration::from_secs(20);
        let cancel = ctx.cancel.clone();
        tokio::spawn(async move {
            tokio::time::sleep(Duration::from_millis(50)).await;
            cancel.cancel();
        });

        let mut args = ToolArgs::new();
        args.named
            .insert("command".to_string(), Value::String("/bin/sh".into()));
        args.named.insert(
            "argv".to_string(),
            Value::String(r#"["-c", "trap : TERM; i=0; while [ $i -lt 200000 ]; do i=$((i+1)); done"]"#.into()),
        );

        let result = Spawn.execute(args, &mut ctx).await;
        assert_eq!(
            result.code, 0,
            "a child that exited 0 on its own, inside the kill grace, must keep \
             that code even though ctx.cancel fired mid-run: {result:?}"
        );
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
        // spawn's own `--command` resolution reads PATH from scope only (the
        // kernel never reads the OS env) — seeding it here from this test
        // PROCESS's real PATH is fixture code reading OS env, which is fine;
        // it is not spawn reaching into the OS on its own.
        ctx.scope
            .set_exported("PATH", Value::String(std::env::var("PATH").unwrap_or_default()));

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
            .insert("command".to_string(), Value::String("/bin/pwd".into()));
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
            .insert("command".to_string(), Value::String("/bin/sleep".into()));
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
            .insert("command".to_string(), Value::String("/bin/echo".into()));
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
