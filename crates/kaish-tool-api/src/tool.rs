//! The `Tool` trait and argument validation.

use std::collections::HashSet;

use async_trait::async_trait;

use kaish_types::{ExecResult, ParamSchema, ToolArgs, ToolSchema, Value};

use crate::ctx::ToolCtx;
use crate::issue::{IssueCode, Severity, ValidationIssue};

/// A tool that can be executed.
///
/// Every kaish command — builtin or third-party — implements this trait. The
/// `execute` method receives a `&mut dyn ToolCtx`, the trimmed portable
/// context; tools needing deeper kernel state downcast via
/// [`ToolCtx::as_any_mut`](crate::ToolCtx::as_any_mut).
#[async_trait]
pub trait Tool: Send + Sync {
    /// The tool's name (used for lookup).
    fn name(&self) -> &str;

    /// Get the tool's schema.
    fn schema(&self) -> ToolSchema;

    /// Execute the tool with the given arguments and context.
    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult;

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
/// Splits `schema.params` into positional and named/flag groups so the
/// positional slot index never conflates with the struct-field index. With
/// clap-derived schemas, positionals sit *after* the flags in struct order;
/// the old single-index walk would have falsely failed `mkdir foo` because
/// the path slot lives at struct index 1+.
///
/// Checks:
/// - Required parameters are provided (positionals by slot, flags by name).
/// - Unknown flags (warning).
/// - Type compatibility for both positional and named args.
pub fn validate_against_schema(args: &ToolArgs, schema: &ToolSchema) -> Vec<ValidationIssue> {
    let mut issues = Vec::new();

    let positional_params: Vec<&ParamSchema> = schema.params.iter().filter(|p| p.positional).collect();
    let flag_params: Vec<&ParamSchema> = schema.params.iter().filter(|p| !p.positional).collect();

    // Required positionals: matched by slot among positional params only.
    for (slot, param) in positional_params.iter().enumerate() {
        if !param.required {
            continue;
        }
        let has_positional = args.positional.len() > slot;
        // A required positional can also be supplied as a named arg if the
        // caller knows the param name (e.g. `mkdir paths=foo`).
        let has_named = args.named.contains_key(&param.name);
        if !has_positional && !has_named {
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

    // Required flags: matched by name (or alias) against args.named / args.flags.
    for param in &flag_params {
        if !param.required {
            continue;
        }
        let has_named = args.named.contains_key(&param.name);
        let has_flag = param.param_type == "bool" && args.has_flag(&param.name);
        if !has_named && !has_flag {
            let code = IssueCode::MissingRequiredArg;
            issues.push(ValidationIssue {
                severity: code.default_severity(),
                code,
                message: format!("required parameter '{}' not provided", param.name),
                span: None,
                suggestion: Some(format!("add --{} <value>", param.name)),
            });
        }
    }

    // Check for unknown flags (only warn - tools may accept dynamic flags).
    // Only bool flags are gathered for the strict known-flag set; the
    // alias-fallback below catches value-taking flags via `matches_flag`.
    let known_flags: HashSet<&str> = flag_params
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
            // Check if any flag param matches this flag via aliases
            let matches_alias = flag_params.iter().any(|p| p.matches_flag(flag));
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

    // Type compatibility for named args (search the full schema — callers
    // may name either a positional or a flag param).
    for (key, value) in &args.named {
        if let Some(param) = schema.params.iter().find(|p| &p.name == key)
            && let Some(issue) = check_type_compatibility(key, value, &param.param_type) {
                issues.push(issue);
            }
    }

    // Type compatibility for positional args (matched by slot among
    // positional params). Extra positionals past the schema are ignored —
    // many builtins (cat, cp, mkdir) accept variadic positionals.
    for (slot, value) in args.positional.iter().enumerate() {
        if let Some(param) = positional_params.get(slot)
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
// `--json` is declared per-builtin via `GlobalFlags` flatten
// (`crate::global_flags`). Builtins parse it inside execute() and write the
// output format via `ToolCtx::set_output_format`; the kernel applies the
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

#[cfg(test)]
mod validate_tests {
    use super::*;
    use kaish_types::{ParamSchema, ToolSchema};

    fn schema_with_positionals_after_flags() -> ToolSchema {
        // Mirrors clap-derived order: flag fields first, positionals last.
        ToolSchema::new("demo", "demo")
            .param(ParamSchema {
                name: "verbose".into(),
                param_type: "bool".into(),
                required: false,
                default: Some(Value::Bool(false)),
                description: String::new(),
                aliases: vec!["v".into()],
                consumes: 1,
                positional: false,
            })
            .param(ParamSchema {
                name: "lines".into(),
                param_type: "int".into(),
                required: false,
                default: None,
                description: String::new(),
                aliases: vec!["n".into()],
                consumes: 1,
                positional: false,
            })
            .param(ParamSchema {
                name: "path".into(),
                param_type: "string".into(),
                required: true,
                default: None,
                description: String::new(),
                aliases: Vec::new(),
                consumes: 1,
                positional: true,
            })
    }

    /// Regression for the clap-migration index-mismatch: `cat foo.txt` should
    /// satisfy the required positional `path` even though `path` is at struct
    /// index 2 (after `verbose`/`lines`). The old code matched positional[0]
    /// against `verbose` and required positional[2] to exist.
    #[test]
    fn required_positional_satisfied_when_positional_sits_after_flags() {
        let schema = schema_with_positionals_after_flags();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("foo.txt".into()));

        let issues = validate_against_schema(&args, &schema);
        assert!(
            !issues.iter().any(|i| i.code == IssueCode::MissingRequiredArg),
            "required positional should be satisfied by positional[0]; got {:?}",
            issues
        );
    }

    #[test]
    fn required_positional_missing_when_no_positional_given() {
        let schema = schema_with_positionals_after_flags();
        let mut args = ToolArgs::new();
        args.flags.insert("verbose".into());

        let issues = validate_against_schema(&args, &schema);
        assert!(
            issues.iter().any(|i| i.code == IssueCode::MissingRequiredArg),
            "missing required positional should error; got {:?}",
            issues
        );
    }

    /// Positional type check must look up the positional slot, not the
    /// struct-field index. Here we have a string positional at slot 0; the
    /// old code would have type-checked positional[0] against the int param
    /// `lines` (struct index 1) and emit nothing — but now an int positional
    /// against the string slot must be accepted, and a string positional
    /// against an int positional slot would error.
    #[test]
    fn positional_type_check_targets_positional_slot_not_struct_index() {
        let mut schema = ToolSchema::new("demo", "demo");
        // Two positionals: count (int) then name (string).
        schema = schema
            .param(ParamSchema {
                name: "verbose".into(),
                param_type: "bool".into(),
                required: false,
                default: Some(Value::Bool(false)),
                description: String::new(),
                aliases: Vec::new(),
                consumes: 1,
                positional: false,
            })
            .param(ParamSchema {
                name: "count".into(),
                param_type: "int".into(),
                required: true,
                default: None,
                description: String::new(),
                aliases: Vec::new(),
                consumes: 1,
                positional: true,
            })
            .param(ParamSchema {
                name: "name".into(),
                param_type: "string".into(),
                required: true,
                default: None,
                description: String::new(),
                aliases: Vec::new(),
                consumes: 1,
                positional: true,
            });

        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(5));
        args.positional.push(Value::String("widget".into()));

        let issues = validate_against_schema(&args, &schema);
        assert!(
            !issues.iter().any(|i| matches!(i.code, IssueCode::InvalidArgType)),
            "int->int and string->string slots should validate clean; got {:?}",
            issues
        );
    }

    /// Required *flag* (non-positional) must still fire MissingRequiredArg
    /// when absent — separating the loops shouldn't silently drop the check.
    #[test]
    fn required_flag_still_errors_when_missing() {
        let schema = ToolSchema::new("demo", "demo")
            .param(ParamSchema {
                name: "output".into(),
                param_type: "string".into(),
                required: true,
                default: None,
                description: String::new(),
                aliases: vec!["o".into()],
                consumes: 1,
                positional: false,
            });

        let args = ToolArgs::new();
        let issues = validate_against_schema(&args, &schema);
        assert!(
            issues.iter().any(|i| i.code == IssueCode::MissingRequiredArg),
            "required flag should error when missing; got {:?}",
            issues
        );
    }
}
