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
use tokio::process::Command;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Env tool: print environment or run command with modified environment.
pub struct Env;

#[async_trait]
impl Tool for Env {
    fn name(&self) -> &str {
        "env"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new(
            "env",
            "Print environment variables or run command with modified environment",
        )
        .param(ParamSchema::optional(
            "args",
            "array",
            Value::Null,
            "VAR=value pairs followed by optional command and arguments",
        ))
        .param(ParamSchema::optional(
            "0",
            "bool",
            Value::Bool(false),
            "Use NUL as separator instead of newline (-0)",
        ))
        .param(ParamSchema::optional(
            "i",
            "bool",
            Value::Bool(false),
            "Start with empty environment (-i)",
        ))
        .param(ParamSchema::optional(
            "u",
            "string",
            Value::Null,
            "Unset variable from environment (-u VAR)",
        ))
        .example("Print environment", "env")
        .example("Run with modified env", "env MY_VAR=hello command")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let null_sep = args.has_flag("0");
        let clear_env = args.has_flag("i");

        // Collect -u (unset) value if specified
        let unset_vars: Vec<String> = args
            .get_named("u")
            .iter()
            .filter_map(|v| match v {
                Value::String(s) => Some(s.clone()),
                _ => None,
            })
            .collect();

        // No positional arguments: just print environment
        if args.positional.is_empty() {
            return print_env(ctx, null_sep);
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
        let command = match &args.positional[cmd_idx] {
            Value::String(s) => s.clone(),
            other => value_to_string(other),
        };

        let cmd_args: Vec<String> = args.positional[cmd_idx + 1..]
            .iter()
            .map(value_to_string)
            .collect();

        execute_with_env(
            ctx,
            &command,
            &cmd_args,
            &env_overrides,
            &unset_vars,
            clear_env,
        )
        .await
    }
}

/// Print current environment variables.
fn print_env(ctx: &ExecContext, null_sep: bool) -> ExecResult {
    // Build sorted list of vars
    let mut vars: Vec<_> = ctx.scope.exported_vars().into_iter().collect();
    vars.sort_by(|(a, _), (b, _)| a.cmp(b));

    let sep = if null_sep { '\0' } else { '\n' };
    let mut output = String::new();

    for (name, value) in vars {
        let value_str = value_to_string(&value);
        output.push_str(&format!("{}={}{}", name, value_str, sep));
    }

    // Remove trailing separator for consistency (unless null-sep)
    if !null_sep && !output.is_empty() {
        output.pop();
    }

    ExecResult::with_output(OutputData::text(output))
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
async fn execute_with_env(
    ctx: &mut ExecContext,
    command: &str,
    args: &[String],
    overrides: &[(String, String)],
    unset: &[String],
    clear: bool,
) -> ExecResult {
    let mut cmd = Command::new(command);
    cmd.args(args);

    if clear {
        cmd.env_clear();
    } else {
        // Set exported variables from scope
        for (name, value) in ctx.scope.exported_vars() {
            if !unset.contains(&name) {
                cmd.env(&name, value_to_string(&value));
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

    // Handle stdin
    if let Some(stdin_data) = ctx.read_stdin_to_string().await {
        cmd.stdin(std::process::Stdio::piped());
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(e) => return ExecResult::failure(127, format!("env: {}: {}", command, e)),
        };

        if let Some(mut stdin) = child.stdin.take() {
            use tokio::io::AsyncWriteExt;
            if let Err(e) = stdin.write_all(stdin_data.as_bytes()).await {
                return ExecResult::failure(1, format!("env: failed to write stdin: {}", e));
            }
        }

        match child.wait_with_output().await {
            Ok(output) => {
                let code = output.status.code().unwrap_or(-1) as i64;
                let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
                let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
                ExecResult::from_output(code, stdout, stderr)
            }
            Err(e) => ExecResult::failure(1, format!("env: failed to wait: {}", e)),
        }
    } else {
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());

        match cmd.output().await {
            Ok(output) => {
                let code = output.status.code().unwrap_or(-1) as i64;
                let stdout = String::from_utf8_lossy(&output.stdout).into_owned();
                let stderr = String::from_utf8_lossy(&output.stderr).into_owned();
                ExecResult::from_output(code, stdout, stderr)
            }
            Err(e) => ExecResult::failure(127, format!("env: {}: {}", command, e)),
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
        Value::Blob(blob) => format!("[blob: {} {}]", blob.formatted_size(), blob.content_type),
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
        assert!(result.out.contains("PATH=/usr/bin"));
        assert!(result.out.contains("HOME=/home/user"));
    }

    #[tokio::test]
    async fn test_env_empty() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Env.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.is_empty());
    }

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
        assert_eq!(result.out.trim(), "hello");
    }

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
        assert!(!result.out.contains("EXISTING="));
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
        assert!(result.out.contains("A=1"));
        assert!(result.out.contains("B=2"));
        assert!(result.out.contains("C=3"));
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
        assert!(result.out.contains('\0'));
        assert!(!result.out.ends_with('\n'));
    }
}
