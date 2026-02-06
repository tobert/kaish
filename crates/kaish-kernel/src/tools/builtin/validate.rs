//! validate — Validate kaish scripts without executing.
//!
//! # Examples
//!
//! ```kaish
//! validate script.kai           # Validate a script file
//! validate -e 'echo $UNDEFINED'  # Validate inline code
//! ```

use async_trait::async_trait;
use std::collections::HashMap;
use std::path::Path;

use crate::ast::{ToolDef, Value};
use crate::interpreter::{ExecResult, OutputData};
use crate::parser::parse;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use crate::validator::{Severity, Validator};

/// Validate tool: check kaish scripts for errors before execution.
pub struct Validate;

#[async_trait]
impl Tool for Validate {
    fn name(&self) -> &str {
        "validate"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("validate", "Validate kaish scripts without executing")
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::Null,
                "Script file to validate",
            ))
            .param(ParamSchema::optional(
                "expr",
                "string",
                Value::Null,
                "Inline expression to validate (-e)",
            ).with_aliases(["-e"]))
            .param(ParamSchema::optional(
                "quiet",
                "bool",
                Value::Bool(false),
                "Only return exit code, no output (-q)",
            ).with_aliases(["-q"]))
            .param(ParamSchema::optional(
                "warnings",
                "bool",
                Value::Bool(true),
                "Show warnings in addition to errors",
            ).with_aliases(["-w"]))
            .example("Validate a script file", "validate script.kai")
            .example("Validate inline code", "validate -e 'grep \"[\" file.txt'")
            .example("Check exit code only", "validate -q script.kai && echo 'valid'")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get registry from context
        let registry = match &ctx.tools {
            Some(r) => r.clone(),
            None => return ExecResult::failure(1, "validate: tool registry not available"),
        };

        let quiet = args.has_flag("quiet") || args.has_flag("q");
        let show_warnings = args.has_flag("warnings") || args.has_flag("w") || !args.flags.contains("warnings");

        // Get input: from file, -e expression, or stdin
        let (source, label) = if let Some(expr) = args.get_string("expr", usize::MAX) {
            (expr, "<expr>".to_string())
        } else if let Some(path) = args.get_string("path", 0) {
            let resolved = ctx.resolve_path(&path);
            match ctx.backend.read(Path::new(&resolved), None).await {
                Ok(data) => match String::from_utf8(data) {
                    Ok(content) => (content, path),
                    Err(_) => return ExecResult::failure(1, format!("validate: {}: invalid UTF-8", path)),
                },
                Err(e) => return ExecResult::failure(1, format!("validate: {}: {}", path, e)),
            }
        } else if let Some(stdin) = &ctx.stdin {
            (stdin.clone(), "<stdin>".to_string())
        } else {
            return ExecResult::failure(1, "validate: no input provided (use path or -e)");
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
                return ExecResult::with_output(OutputData::text(""));
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
        assert!(!result.ok() || result.err.contains("regex") || result.out.contains("regex"));
    }

    #[tokio::test]
    async fn test_validate_break_outside_loop() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named.insert("expr".to_string(), Value::String("break".into()));

        let result = Validate.execute(args, &mut ctx).await;
        assert!(!result.ok(), "break outside loop should fail validation");
        assert!(result.err.contains("loop") || result.out.contains("loop"));
    }

    #[tokio::test]
    async fn test_validate_quiet_mode() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named.insert("expr".to_string(), Value::String("echo hello".into()));
        args.flags.insert("q".to_string());

        let result = Validate.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.is_empty(), "quiet mode should have no output");
    }
}
