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
use crate::tools::{schema_from_clap, validate_against_schema, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use crate::validator::ValidationIssue;

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

    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        let mut issues = validate_against_schema(args, &self.schema());
        // `export PATH` is a positional; `export PATH=/bin` is a `key=value`
        // word whose key is the name. A quoted `export "PATH=/bin"` arrives as
        // one positional string, so a positional is cut at the first `=` too.
        //
        // A glued unknown flag (`export --PАTH=foo`) lands in the same map with
        // no way left to tell it from a name, so it warns here as well as
        // through W001. Two warnings on a command clap will refuse anyway is a
        // better trade than a name door that skips a whole map.
        let positional = args.positional.iter().filter_map(|v| match v {
            Value::String(s) => Some(s.split('=').next().unwrap_or(s)),
            _ => None,
        });
        let assigned = args.named.keys().map(String::as_str);
        issues.extend(positional.chain(assigned).filter_map(super::mixed_script_issue));
        issues
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
            if let Err(why) = check_name(name) {
                return ExecResult::failure(1, why);
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

                if let Err(why) = check_name(name) {
                    return ExecResult::failure(1, why);
                }

                ctx.scope.set_exported_global(name, Value::String(value.to_string()));
            } else {
                // Just mark for export
                if let Err(why) = check_name(arg_str) {
                    return ExecResult::failure(1, why);
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

/// Check a name against the same rule an assignment uses, and say why when it
/// fails. The reason is the whole point: `export` is the one door a name can
/// enter through without the parser having seen it as an `Ident`, so a
/// rejection here is the only message the author gets.
fn check_name(name: &str) -> Result<(), String> {
    let mut chars = name.chars();

    // First character must be a letter or underscore — never a digit, which
    // would collide with the positional parameters.
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' || !c.is_ascii() => {}
        Some(_) => return Err(format!("export: `{name}': not a valid identifier")),
        None => return Err("export: a name cannot be empty".to_string()),
    }

    // A dotted name binds nothing readable: `${a.b}` is a loud brackets-only
    // error, so `export a.b=1` would create a variable no read path reaches.
    // Assignment refuses this too, and the two doors have to agree.
    if let Some(dot) = name.find('.') {
        let (root, rest) = (&name[..dot], &name[dot + 1..]);
        return Err(format!(
            "export: `{name}' is not a valid name — kaish uses bracket access, not dots; \
             use `export {root}[{rest}]=value`"
        ));
    }

    // The rest is the same rule every other door uses, so `export café=1`
    // and `café=1` agree about what a name is — including the refusal of a
    // character that does not show itself.
    crate::name::validate(name).map_err(|bad| format!("export: `{name}': {bad}"))
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
