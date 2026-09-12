//! Pre-execution validation for kaish scripts.
//!
//! The validator runs after parsing but before execution to catch errors early.
//! It validates:
//!
//! - **Command existence**: Are commands defined (builtin, user-defined, MCP)?
//! - **Argument schemas**: Required params present? Known flags? Type compatibility?
//! - **Semantic constraints**: Regex compiles? seq increment != 0? count > 0?
//! - **Variable bindings**: Warn on possibly undefined variables
//! - **Control flow**: break/continue outside loop? return outside function?
//!
//! # Example
//!
//! ```ignore
//! use kaish_kernel::validator::Validator;
//! use kaish_kernel::parser::parse;
//!
//! let program = parse("nonexistent_cmd arg")?;
//! let validator = Validator::new(&registry, &user_tools, &[]);
//! let issues = validator.validate(&program);
//!
//! for issue in &issues {
//!     println!("{}", issue.format(source));
//! }
//! ```

mod issue;
mod scope_tracker;
mod walker;

pub use issue::{IssueCode, Severity, Span, ValidationIssue};
pub use walker::{build_tool_args_for_validation, Validator};
pub(crate) use walker::{
    classify_command_name, is_runtime_special_form, is_static_command_name, SpecialForm,
};

/// Validate `source` against the builtin catalog, without a kernel.
///
/// This is the mechanism behind `kaish --plan`: a dry run should refuse the
/// same programs the kernel would, and it has no kernel to ask. The registry
/// is the compiled-in builtin set, so an embedder's own tools are absent —
/// a command this cannot resolve is left to the kernel rather than reported
/// as unknown.
///
/// Returns `Err` with the parse errors when `source` does not parse, so a
/// caller reports a parse failure as a parse failure and never as an empty
/// issue list.
pub fn validate_program(
    source: &str,
) -> Result<Vec<ValidationIssue>, Vec<crate::parser::ParseError>> {
    use std::collections::HashMap;

    let program = crate::parser::parse(source)?;
    let mut registry = crate::tools::ToolRegistry::new();
    crate::tools::register_builtins(&mut registry);
    let user_tools: HashMap<String, crate::ast::ToolDef> = HashMap::new();
    Ok(Validator::new(&registry, &user_tools, &[]).validate(&program))
}
