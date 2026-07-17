//! kaish-validate — Validate kaish scripts without executing.
//!
//! # Examples
//!
//! ```kaish
//! kaish-validate script.kai           # Validate a script file
//! kaish-validate -e 'echo $UNDEFINED'  # Validate inline code
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::collections::HashMap;
use std::path::Path;

use crate::ast::ToolDef;
use crate::interpreter::{ExecResult, OutputData};
use crate::parser::parse;
use crate::tools::builtin::get_path_string;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use crate::validator::{Severity, Validator};

/// Validate tool: check kaish scripts for errors before execution.
pub struct Validate;

/// clap-derived argv layer for kaish-validate.
#[derive(Parser, Debug)]
#[command(name = "kaish-validate", about = "Validate kaish scripts without executing")]
struct ValidateArgs {
    /// Inline expression to validate.
    #[arg(id = "expr", short = 'e', long = "expr")]
    _expr: Option<String>,

    /// Only return exit code, no output.
    #[arg(id = "quiet", short = 'q', long = "quiet")]
    _quiet: bool,

    /// Show warnings in addition to errors.
    #[arg(id = "warnings", short = 'w', long = "warnings")]
    warnings: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Script file to validate; pass `--expr` instead for inline expressions.
    path: Vec<String>,
}

#[async_trait]
impl Tool for Validate {
    fn name(&self) -> &str {
        "kaish-validate"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ValidateArgs::command(),
            "kaish-validate",
            "Validate kaish scripts without executing",
            [
                ("Validate a script file", "kaish-validate script.kai"),
                ("Validate inline code", "kaish-validate -e 'grep \"[\" file.txt'"),
                ("Check exit code only", "kaish-validate -q script.kai && echo 'valid'"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kaish-validate: {e}")),
        };
        let parsed = match ValidateArgs::try_parse_from(
            std::iter::once("kaish-validate".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kaish-validate: {e}")),
        };
        parsed.global.apply(ctx);

        // Get registry from context
        let registry = match &ctx.tools {
            Some(r) => r.clone(),
            None => return ExecResult::failure(1, "kaish-validate: tool registry not available"),
        };

        let quiet = args.has_flag("quiet") || args.has_flag("q");
        // Warnings are opt-in via `-w`/`--warnings`. (The old expression was the
        // tautology `A || B || !A` — always true — so `-w` was inert.) Read the
        // parsed clap field, not the raw flag map.
        let show_warnings = parsed.warnings;

        // Get input: from file, -e expression, or stdin. A binary `path`
        // operand goes loud rather than silently falling through to stdin.
        let (source, label) = if let Some(expr) = args.get_string("expr", usize::MAX) {
            (expr, "<expr>".to_string())
        } else {
            match get_path_string(&args, "path", 0) {
                Ok(Some(path)) => {
                    let resolved = ctx.resolve_path(&path);
                    match ctx.backend.read(Path::new(&resolved), None).await {
                        Ok(data) => match String::from_utf8(data) {
                            Ok(content) => (content, path),
                            Err(_) => return ExecResult::failure(1, format!("kaish-validate: {}: invalid UTF-8", path)),
                        },
                        Err(e) => return ExecResult::failure(1, format!("kaish-validate: {}: {}", path, e)),
                    }
                }
                Ok(None) => {
                    // stdin (pipe or buffered — `read_stdin_to_text` prefers the pipe).
                    match ctx.read_stdin_to_text().await {
                        Ok(Some(s)) => (s, "<stdin>".to_string()),
                        Ok(None) => return ExecResult::failure(1, "kaish-validate: no input provided (use path or -e)"),
                        Err(e) => return ExecResult::failure(2, format!("kaish-validate: {e}")),
                    }
                }
                Err(e) => return ExecResult::failure(1, format!("kaish-validate: {e}")),
            }
        };

        // Parse the script
        let program = match parse(&source) {
            Ok(p) => p,
            Err(errors) => {
                if quiet {
                    return ExecResult::failure(2, "");
                }
                let msg = errors
                    .iter()
                    .map(|e| format!("{}: parse error: {}", label, e))
                    .collect::<Vec<_>>()
                    .join("\n");
                return ExecResult::failure(2, msg);
            }
        };

        // Validate the parsed AST
        let user_tools: HashMap<String, ToolDef> = HashMap::new();
        let validator = Validator::new(&registry, &user_tools);
        let issues = validator.validate(&program);

        // Filter based on severity
        let (errors, warnings): (Vec<_>, Vec<_>) = issues
            .into_iter()
            .partition(|i| i.severity == Severity::Error);

        if quiet {
            if errors.is_empty() {
                return ExecResult::success("");
            } else {
                return ExecResult::failure(1, "");
            }
        }

        // Build output
        let mut output = String::new();

        for error in &errors {
            output.push_str(&format!("{}: {}\n", label, error.format(&source)));
        }

        if show_warnings {
            for warning in &warnings {
                output.push_str(&format!("{}: {}\n", label, warning.format(&source)));
            }
        }

        if errors.is_empty() && (warnings.is_empty() || !show_warnings) {
            if !quiet {
                output = format!("{}: valid\n", label);
            }
            ExecResult::with_output(OutputData::text(output))
        } else if errors.is_empty() {
            // Only warnings
            ExecResult::with_output(OutputData::text(output))
        } else {
            ExecResult::failure(1, output)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::tools::{register_builtins, ToolRegistry};
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        vfs.mount("/", mem);
        let vfs = Arc::new(vfs);

        let mut registry = ToolRegistry::new();
        register_builtins(&mut registry);
        let registry = Arc::new(registry);

        ExecContext::with_vfs_and_tools(vfs, registry)
    }

    #[tokio::test]
    async fn test_validate_valid_expr() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named.insert("expr".to_string(), Value::String("echo hello".into()));

        let result = Validate.execute(args, &mut ctx).await;
        assert!(result.ok(), "expected success: {}", result.err);
    }

    #[tokio::test]
    async fn test_validate_invalid_regex() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        // Invalid regex pattern - unclosed bracket
        args.named.insert("expr".to_string(), Value::String("grep '[' file.txt".into()));

        let result = Validate.execute(args, &mut ctx).await;
        // This should fail validation due to invalid regex
        assert!(!result.ok() || result.err.contains("regex") || result.text_out().contains("regex"));
    }

    #[tokio::test]
    async fn test_validate_break_outside_loop() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named.insert("expr".to_string(), Value::String("break".into()));

        let result = Validate.execute(args, &mut ctx).await;
        assert!(!result.ok(), "break outside loop should fail validation");
        assert!(result.err.contains("loop") || result.text_out().contains("loop"));
    }

    #[tokio::test]
    async fn test_validate_quiet_mode() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named.insert("expr".to_string(), Value::String("echo hello".into()));
        args.flags.insert("q".to_string());

        let result = Validate.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().is_empty(), "quiet mode should have no output");
    }
}
