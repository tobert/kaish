//! env — Print environment or run command with modified environment.
//!
//! # Examples
//!
//! ```kaish
//! env                           # Print all environment variables
//! env -0                        # Print with null separator (for xargs -0)
//! env VAR=value command args    # Run command with VAR set
//! env -i command                # Run command with empty environment
//! env -u VAR command            # Run command with VAR unset
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::builtin::read_repeatable_strings;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Env tool: print environment or run command with modified environment.
pub struct Env;

/// clap-derived argv layer for env.
///
/// `args.named` and `args.flags` are read directly for VAR=value & -0/-i/-u
/// because env's positional layer is (env-overrides, command, child-argv) —
/// can't be reflected as named clap positionals cleanly.
#[derive(Parser, Debug)]
#[command(name = "env", about = "Print environment variables or run command with modified environment")]
struct EnvArgs {
    /// Use NUL as separator instead of newline (-0)
    #[arg(short = '0', long = "0")]
    nul: bool,

    /// Start with empty environment (-i)
    #[arg(short = 'i', long = "i")]
    ignore_environment: bool,

    /// Unset variable from environment (-u VAR); repeatable: -u A -u B.
    ///
    /// Clap sees a single occurrence via `to_argv()` (the kernel accumulates
    /// repeated `-u` into a `Value::Json(Array)` that `to_argv()` can't split
    /// back out). The actual collection uses `collect_unset_vars(&args)` which
    /// reads from the raw ToolArgs — same pattern as sed's `collect_expressions`.
    /// This field is a validation sink only.
    #[arg(short = 'u', long = "u", action = clap::ArgAction::Append)]
    u: Vec<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// `VAR=VALUE` assignments followed by an optional command and its arguments.
    args: Vec<String>,
}

#[async_trait]
impl Tool for Env {
    fn name(&self) -> &str {
        "env"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &EnvArgs::command(),
            "env",
            "Print environment variables or run command with modified environment",
            [
                ("Print environment", "env"),
                ("Run with modified env", "env MY_VAR=hello command"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("env: {e}")),
        };
        let parsed = match EnvArgs::try_parse_from(
            std::iter::once("env".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("env: {e}")),
        };
        parsed.global.apply(ctx);

        let null_sep = parsed.nul;
        let clear_env = parsed.ignore_environment;

        // Collect -u names from the raw ToolArgs, not `parsed.u`.
        // The kernel accumulates repeated `-u` into a `Value::Json(Array)` in
        // named["u"], but `to_argv()` re-renders that as a single JSON token
        // that clap can't split back into individual strings. Read the raw map
        // directly (same approach sed uses for repeatable `-e`).
        let unset_vars = match collect_unset_vars(&args) {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("env: -u: {e}")),
        };

        // No positional arguments: print environment with -u/-i applied.
        // -u and -i must filter the listing even when no command is given.
        if args.positional.is_empty() {
            return print_env_with_overrides(ctx, &[], &unset_vars, clear_env, null_sep);
        }

        // Parse arguments: VAR=value pairs, then optional command
        let mut env_overrides: Vec<(String, String)> = Vec::new();
        let mut command_start = None;

        for (i, arg) in args.positional.iter().enumerate() {
            let arg_str = match arg {
                Value::String(s) => s.as_str(),
                _ => {
                    command_start = Some(i);
                    break;
                }
            };

            // Check if it's a VAR=value assignment
            if let Some(eq_pos) = arg_str.find('=') {
                let name = &arg_str[..eq_pos];
                let value = &arg_str[eq_pos + 1..];
                env_overrides.push((name.to_string(), value.to_string()));
            } else {
                // Not an assignment, start of command
                command_start = Some(i);
                break;
            }
        }

        // If no command, just print environment with overrides
        if command_start.is_none() {
            return print_env_with_overrides(ctx, &env_overrides, &unset_vars, clear_env, null_sep);
        }

        // Execute command with modified environment
        // Safety: we checked `command_start.is_none()` above and returned
        let Some(cmd_idx) = command_start else {
            return ExecResult::failure(1, "env: internal error: missing command index");
        };
        // Decision D: a bare collection can't cross the process boundary as the
        // command name or an argv element — loud, not a silent JSON stringify.
        // env consumes typed Values directly (no `build_args_flat`), so the
        // guard lives here at env's own edge.
        for v in &args.positional[cmd_idx..] {
            if let Some(msg) = crate::interpreter::structured_boundary_error("a command argument", v) {
                return ExecResult::failure(1, format!("env: {msg}"));
            }
        }
        let command = match &args.positional[cmd_idx] {
            Value::String(s) => s.clone(),
            other => value_to_string(other),
        };

        let cmd_args: Vec<String> = args.positional[cmd_idx + 1..]
            .iter()
            .map(value_to_string)
            .collect();

        #[cfg(feature = "subprocess")]
        {
            return execute_with_env(
                ctx,
                &command,
                &cmd_args,
                &env_overrides,
                &unset_vars,
                clear_env,
            )
            .await;
        }

        #[cfg(not(feature = "subprocess"))]
        {
            let _ = (ctx, &command, &cmd_args, &env_overrides, &unset_vars, clear_env);
            return ExecResult::failure(1, "env: external commands not available in sandbox mode");
        }
    }
}

/// Collect the list of variable names to unset from `-u` / `--u` flags.
///
/// The kernel accumulates repeated `-u NAME` occurrences into a
/// `Value::Json(Array([...]))` stored under `named["u"]`.  `to_argv()` cannot
/// split that array back into individual `-u=NAME` tokens, so we must read the
/// raw `ToolArgs::named` map directly — the same pattern sed uses for its
/// repeatable `-e` expressions.
///
/// Delegates to [`read_repeatable_strings`] (GH #217): `-u` never takes a
/// `key=value` form, so every occurrence — even the first — is a bare
/// positional that `push_repeatable_value` wraps into the array regardless of
/// occurrence count. The old hand-rolled `filter_map` silently dropped a
/// binary occurrence instead of erroring, so `env -u $BIN` would run as if
/// `-u` had never been given rather than failing loudly.
fn collect_unset_vars(args: &ToolArgs) -> Result<Vec<String>, String> {
    // The kernel canonicalises the flag to the long name; "u" is the only key
    // ever actually populated.
    read_repeatable_strings(args, "u")
}

/// Print environment with overrides applied.
fn print_env_with_overrides(
    ctx: &ExecContext,
    overrides: &[(String, String)],
    unset: &[String],
    clear: bool,
    null_sep: bool,
) -> ExecResult {
    // Start with existing exports unless clearing
    let mut env: std::collections::HashMap<String, String> = if clear {
        std::collections::HashMap::new()
    } else {
        ctx.scope
            .exported_vars()
            .into_iter()
            .map(|(k, v)| (k, value_to_string(&v)))
            .collect()
    };

    // Remove unset variables
    for name in unset {
        env.remove(name);
    }

    // Apply overrides
    for (name, value) in overrides {
        env.insert(name.clone(), value.clone());
    }

    // Sort pairs
    let mut pairs: Vec<_> = env.into_iter().collect();
    pairs.sort_by(|(a, _), (b, _)| a.cmp(b));

    let sep = if null_sep { '\0' } else { '\n' };
    let mut output = String::new();

    for (name, value) in pairs {
        output.push_str(&format!("{}={}{}", name, value, sep));
    }

    // Remove trailing separator for consistency (unless null-sep)
    if !null_sep && !output.is_empty() {
        output.pop();
    }

    ExecResult::with_output(OutputData::text(output))
}

/// Execute a command with modified environment.
#[cfg(feature = "subprocess")]
async fn execute_with_env(
    ctx: &mut ExecContext,
    command: &str,
    args: &[String],
    overrides: &[(String, String)],
    unset: &[String],
    clear: bool,
) -> ExecResult {
    use tokio::process::Command;
    let mut cmd = Command::new(command);
    cmd.args(args);

    if clear {
        cmd.env_clear();
    } else {
        // Set exported variables from scope. A structured value can't cross the
        // process boundary; refuse rather than silently JSON-serializing it into
        // the child's environment. This is an independent spawn path that
        // bypasses `try_execute_external`, so it must run the same
        // `structured_export_error` guard (Decision D).
        let exported: Vec<(String, Value)> = ctx
            .scope
            .exported_vars()
            .into_iter()
            .filter(|(name, _)| !unset.contains(name))
            .collect();
        if let Some(msg) = crate::interpreter::structured_export_error(&exported) {
            return ExecResult::failure(1, msg);
        }
        // Binary can't cross the process boundary as an env var value either
        // — loud, not the `[binary: N bytes]` placeholder (kept in sync with
        // kernel.rs::try_execute_external and dispatch.rs::try_external).
        for (name, value) in exported {
            match crate::interpreter::value_to_text_sink_named(
                &value,
                "an exported environment variable value",
            ) {
                Ok(s) => {
                    cmd.env(&name, s);
                }
                Err(e) => return ExecResult::failure(1, format!("env: {e}")),
            }
        }
    }

    // Remove unset variables
    for name in unset {
        cmd.env_remove(name);
    }

    // Apply overrides
    for (name, value) in overrides {
        cmd.env(name, value);
    }

    // Handle stdin. Forward raw bytes so binary piped into the child
    // (`… | env FOO=bar gzip`) isn't lossy-decoded.
    if let Some(stdin_data) = ctx.read_stdin_to_bytes().await {
        cmd.stdin(std::process::Stdio::piped());
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(e) => return ExecResult::failure(127, format!("env: {}: {}", command, e)),
        };

        if let Some(mut stdin) = child.stdin.take() {
            use tokio::io::AsyncWriteExt;
            if let Err(e) = stdin.write_all(&stdin_data).await {
                return ExecResult::failure(1, format!("env: failed to write stdin: {}", e));
            }
        }

        match child.wait_with_output().await {
            Ok(output) => capture_to_result(output.status.code(), output.stdout, output.stderr),
            Err(e) => ExecResult::failure(1, format!("env: failed to wait: {}", e)),
        }
    } else {
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        match cmd.output().await {
            Ok(output) => capture_to_result(output.status.code(), output.stdout, output.stderr),
            Err(e) => ExecResult::failure(127, format!("env: {}: {}", command, e)),
        }
    }
}

/// Build a result from a child's captured stdout/stderr: stdout keeps binary
/// intact (text if valid UTF-8, else a Bytes result); stderr stays text.
#[cfg(feature = "subprocess")]
fn capture_to_result(code: Option<i32>, stdout: Vec<u8>, stderr: Vec<u8>) -> ExecResult {
    let mut result = ExecResult::success_text_or_bytes(stdout).with_code(code.unwrap_or(-1) as i64);
    result.err = String::from_utf8_lossy(&stderr).into_owned();
    result
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
    async fn test_env_prints_exports() {
        let mut ctx = make_ctx();
        ctx.scope.set_exported("PATH", Value::String("/usr/bin".into()));
        ctx.scope
            .set_exported("HOME", Value::String("/home/user".into()));

        let args = ToolArgs::new();
        let result = Env.execute(args, &mut ctx).await;

        assert!(result.ok());
        assert!(result.text_out().contains("PATH=/usr/bin"));
        assert!(result.text_out().contains("HOME=/home/user"));
    }

    #[tokio::test]
    async fn test_env_empty() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Env.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty());
    }

    #[cfg(feature = "subprocess")]
    #[tokio::test]
    async fn test_env_with_command() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("TEST_VAR=hello".into()));
        args.positional.push(Value::String("/bin/sh".into()));
        args.positional
            .push(Value::String("-c".into()));
        args.positional
            .push(Value::String("echo $TEST_VAR".into()));

        let result = Env.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "hello");
    }

    #[cfg(feature = "subprocess")]
    #[tokio::test]
    async fn test_env_i_clears_environment() {
        let mut ctx = make_ctx();
        ctx.scope
            .set_exported("EXISTING", Value::String("value".into()));

        let mut args = ToolArgs::new();
        args.flags.insert("i".to_string());
        args.positional.push(Value::String("/usr/bin/env".into()));

        let result = Env.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Should be empty or nearly empty (no EXISTING)
        assert!(!result.text_out().contains("EXISTING="));
    }

    #[tokio::test]
    async fn test_env_prints_overrides_without_command() {
        let mut ctx = make_ctx();
        ctx.scope.set_exported("A", Value::String("1".into()));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("B=2".into()));
        args.positional.push(Value::String("C=3".into()));

        let result = Env.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("A=1"));
        assert!(result.text_out().contains("B=2"));
        assert!(result.text_out().contains("C=3"));
    }

    #[tokio::test]
    async fn test_env_null_separator() {
        let mut ctx = make_ctx();
        ctx.scope.set_exported("X", Value::String("1".into()));
        ctx.scope.set_exported("Y", Value::String("2".into()));

        let mut args = ToolArgs::new();
        args.flags.insert("0".to_string());

        let result = Env.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains('\0'));
        assert!(!result.text_out().ends_with('\n'));
    }
}
