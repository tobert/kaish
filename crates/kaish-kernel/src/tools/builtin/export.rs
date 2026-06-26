//! export — Mark variables for export to child processes.
//!
//! # Examples
//!
//! ```kaish
//! export PATH                    # Mark existing PATH as exported
//! export MY_VAR=value           # Set and export MY_VAR
//! export -p                     # Print all exported variables
//! export A B C                  # Export multiple variables
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Export tool: marks variables for export to child processes.
///
/// Supports:
/// - `export VAR` - mark existing variable as exported
/// - `export VAR=value` - set and export
/// - `export -p` - print all exported variables
pub struct Export;

/// clap-derived argv layer for export.
#[derive(Parser, Debug)]
#[command(name = "export", about = "Mark variables for export to child processes")]
struct ExportArgs {
    /// Print all exported variables (-p)
    #[arg(short = 'p', long = "p")]
    print: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// `NAME=VALUE` assignments and/or bare `NAME` identifiers to export.
    names: Vec<String>,
}

#[async_trait]
impl Tool for Export {
    fn name(&self) -> &str {
        "export"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ExportArgs::command(),
            "export",
            "Mark variables for export to child processes",
            [
                ("Set and export", "export MY_VAR=value"),
                ("Export existing variable", "export PATH"),
                ("List exports", "export -p"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // For export, args.named carries user-defined VAR=value pairs that
        // clap can't know about. Synthesise an argv with just flags so clap
        // parses `-p` / `--json` cleanly; we read VAR=value off args.named
        // directly below.
        let mut clap_argv: Vec<String> = Vec::new();
        let mut sorted_flags: Vec<&String> = args.flags.iter().collect();
        sorted_flags.sort();
        for flag in sorted_flags {
            clap_argv.push(if flag.chars().count() == 1 {
                format!("-{flag}")
            } else {
                format!("--{flag}")
            });
        }
        let parsed = match ExportArgs::try_parse_from(
            std::iter::once("export".to_string()).chain(clap_argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("export: {e}")),
        };
        parsed.global.apply(ctx);

        // Handle -p flag: print all exported variables
        if parsed.print {
            return print_exports(ctx);
        }

        // No arguments and no named args: print exports (like bash)
        if args.positional.is_empty() && args.named.is_empty() {
            return print_exports(ctx);
        }

        // Handle named arguments: `export FOO="bar"` parses as Named { key: "FOO", value: "bar" }
        // This is the common case when the shell parses `export VAR="value"`
        for (name, value) in &args.named {
            if !is_valid_name(name) {
                return ExecResult::failure(
                    1,
                    format!("export: `{}': not a valid identifier", name),
                );
            }
            ctx.scope.set_exported_global(name, value.clone());
        }

        // Process positional arguments (for `export VAR` without value, or `export VAR=value` as single string)
        for arg in &args.positional {
            let arg_str = match arg {
                Value::String(s) => s.as_str(),
                _ => continue,
            };

            // Check for VAR=value syntax (when passed as a single string)
            if let Some(eq_pos) = arg_str.find('=') {
                let name = &arg_str[..eq_pos];
                let value = &arg_str[eq_pos + 1..];

                if !is_valid_name(name) {
                    return ExecResult::failure(
                        1,
                        format!("export: `{}': not a valid identifier", name),
                    );
                }

                ctx.scope.set_exported_global(name, Value::String(value.to_string()));
            } else {
                // Just mark for export
                if !is_valid_name(arg_str) {
                    return ExecResult::failure(
                        1,
                        format!("export: `{}': not a valid identifier", arg_str),
                    );
                }
                ctx.scope.export(arg_str);
            }
        }

        ExecResult::success("")
    }
}

/// Print all exported variables in `declare -x` format (bash-compatible).
fn print_exports(ctx: &ExecContext) -> ExecResult {
    let mut output = String::new();

    for (name, value) in ctx.scope.exported_vars() {
        let value_str = format_value(&value);
        output.push_str(&format!("declare -x {}={}\n", name, value_str));
    }

    // Also show exported names without values
    for name in ctx.scope.exported_names() {
        if ctx.scope.get(name).is_none() {
            output.push_str(&format!("declare -x {}\n", name));
        }
    }

    ExecResult::with_output(OutputData::text(output.trim_end()))
}

/// Format a value for shell output.
fn format_value(value: &Value) -> String {
    match value {
        Value::Null => "".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\"")),
        Value::Json(json) => format!("'{}'", json.to_string().replace('\'', "'\\''")),
        Value::Bytes(b) => format!("[binary: {} bytes]", b.len()),
    }
}

/// Check if a name is a valid shell variable identifier.
fn is_valid_name(name: &str) -> bool {
    if name.is_empty() {
        return false;
    }

    let mut chars = name.chars();

    // First character must be letter or underscore
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }

    // Remaining characters must be alphanumeric or underscore
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
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
    async fn test_export_marks_variable() {
        let mut ctx = make_ctx();
        ctx.scope.set("X", Value::Int(42));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("X".into()));

        let result = Export.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.is_exported("X"));
    }

    #[tokio::test]
    async fn test_export_with_value() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("MY_VAR=hello world".into()));

        let result = Export.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.is_exported("MY_VAR"));
        assert_eq!(
            ctx.scope.get("MY_VAR"),
            Some(&Value::String("hello world".into()))
        );
    }

    #[tokio::test]
    async fn test_export_multiple() {
        let mut ctx = make_ctx();
        ctx.scope.set("A", Value::Int(1));
        ctx.scope.set("B", Value::Int(2));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("A".into()));
        args.positional.push(Value::String("B".into()));

        let result = Export.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.is_exported("A"));
        assert!(ctx.scope.is_exported("B"));
    }

    #[tokio::test]
    async fn test_export_p_prints_exports() {
        let mut ctx = make_ctx();
        ctx.scope.set_exported("PATH", Value::String("/usr/bin".into()));
        ctx.scope.set_exported("HOME", Value::String("/home/user".into()));

        let mut args = ToolArgs::new();
        args.flags.insert("p".to_string());

        let result = Export.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("declare -x HOME="));
        assert!(result.text_out().contains("declare -x PATH="));
    }

    #[tokio::test]
    async fn test_export_no_args_prints_exports() {
        let mut ctx = make_ctx();
        ctx.scope.set_exported("VAR", Value::String("value".into()));

        let args = ToolArgs::new();
        let result = Export.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("declare -x VAR="));
    }

    #[tokio::test]
    async fn test_export_invalid_name() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("123invalid".into()));

        let result = Export.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not a valid identifier"));
    }

    #[tokio::test]
    async fn test_export_empty_value() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("EMPTY=".into()));

        let result = Export.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.is_exported("EMPTY"));
        assert_eq!(ctx.scope.get("EMPTY"), Some(&Value::String("".into())));
    }
}
