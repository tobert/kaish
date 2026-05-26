//! Core tool traits and types.

use async_trait::async_trait;
use std::collections::HashSet;

use crate::interpreter::ExecResult;
use crate::validator::{IssueCode, Severity, ValidationIssue};

// Data types re-exported from kaish-types.
pub use kaish_types::{ParamSchema, ToolArgs, ToolSchema};

use super::context::ExecContext;
use crate::ast::Value;

/// A tool that can be executed.
#[async_trait]
pub trait Tool: Send + Sync {
    /// The tool's name (used for lookup).
    fn name(&self) -> &str;

    /// Get the tool's schema.
    fn schema(&self) -> ToolSchema;

    /// Execute the tool with the given arguments and context.
    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult;

    /// Validate arguments without executing.
    ///
    /// Default implementation validates against the schema.
    /// Override this for semantic checks (regex validity, zero increment, etc.).
    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        validate_against_schema(args, &self.schema())
    }
}

/// Validate arguments against a tool schema.
///
/// Checks:
/// - Required parameters are provided
/// - Unknown flags (warnings)
/// - Type compatibility
pub fn validate_against_schema(args: &ToolArgs, schema: &ToolSchema) -> Vec<ValidationIssue> {
    let mut issues = Vec::new();

    // Check required parameters
    for (i, param) in schema.params.iter().enumerate() {
        if !param.required {
            continue;
        }

        // Check named args first, then positional
        let has_named = args.named.contains_key(&param.name);
        let has_positional = args.positional.len() > i;
        let has_flag = param.param_type == "bool" && args.has_flag(&param.name);

        if !has_named && !has_positional && !has_flag {
            let code = IssueCode::MissingRequiredArg;
            issues.push(ValidationIssue {
                severity: code.default_severity(),
                code,
                message: format!("required parameter '{}' not provided", param.name),
                span: None,
                suggestion: Some(format!("add {} or {}=<value>", param.name, param.name)),
            });
        }
    }

    // Check for unknown flags (only warn - tools may accept dynamic flags)
    let known_flags: HashSet<&str> = schema
        .params
        .iter()
        .filter(|p| p.param_type == "bool")
        .flat_map(|p| {
            std::iter::once(p.name.as_str())
                .chain(p.aliases.iter().map(|a| a.as_str()))
        })
        .collect();

    for flag in &args.flags {
        // Strip leading dashes for comparison
        let flag_name = flag.trim_start_matches('-');
        // Global output flags are handled by the kernel, not the tool
        if is_global_output_flag(flag_name) {
            continue;
        }
        if !known_flags.contains(flag_name) && !known_flags.contains(flag.as_str()) {
            // Check if any param matches this flag via aliases
            let matches_alias = schema.params.iter().any(|p| p.matches_flag(flag));
            if !matches_alias {
                issues.push(ValidationIssue {
                    severity: Severity::Warning,
                    code: IssueCode::UnknownFlag,
                    message: format!("unknown flag '{}'", flag),
                    span: None,
                    suggestion: None,
                });
            }
        }
    }

    // Check type compatibility for named args
    for (key, value) in &args.named {
        if let Some(param) = schema.params.iter().find(|p| &p.name == key)
            && let Some(issue) = check_type_compatibility(key, value, &param.param_type) {
                issues.push(issue);
            }
    }

    // Check type compatibility for positional args
    for (i, value) in args.positional.iter().enumerate() {
        if let Some(param) = schema.params.get(i)
            && let Some(issue) = check_type_compatibility(&param.name, value, &param.param_type) {
                issues.push(issue);
            }
    }

    issues
}

// ============================================================
// Global Output Flags (--json)
// ============================================================
//
// `--json` is now declared per-builtin via `GlobalFlags` flatten
// (`crates/kaish-kernel/src/tools/global_flags.rs`). Builtins parse it
// inside execute() and write `ctx.output_format`; the kernel applies the
// format after execute() returns. See `docs/clap-migration.md`.

/// Check if a flag name is the kernel-owned `--json` flag.
///
/// External commands (no schema) bypass clap entirely and the kernel
/// doesn't touch their argv — `cargo --json` and similar work as
/// expected. `is_global_output_flag` is retained for the validator's
/// unknown-flag check.
pub fn is_global_output_flag(name: &str) -> bool {
    name == "json"
}

/// Check if a value is compatible with a type.
fn check_type_compatibility(name: &str, value: &Value, expected_type: &str) -> Option<ValidationIssue> {
    let compatible = match expected_type {
        "any" => true,
        "string" => true, // Everything can be a string
        "int" => matches!(value, Value::Int(_) | Value::String(_)),
        "float" => matches!(value, Value::Float(_) | Value::Int(_) | Value::String(_)),
        "bool" => matches!(value, Value::Bool(_) | Value::String(_)),
        "array" => matches!(value, Value::String(_)), // Arrays are passed as strings in kaish
        "object" => matches!(value, Value::String(_)), // Objects are JSON strings
        _ => true, // Unknown types pass
    };

    if compatible {
        None
    } else {
        let code = IssueCode::InvalidArgType;
        Some(ValidationIssue {
            severity: code.default_severity(),
            code,
            message: format!(
                "argument '{}' has type {:?}, expected {}",
                name, value, expected_type
            ),
            span: None,
            suggestion: None,
        })
    }
}
