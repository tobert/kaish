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
//! let validator = Validator::new(&registry, &user_tools);
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
pub use scope_tracker::ScopeTracker;
pub use walker::{build_tool_args_for_validation, Validator};
pub(crate) use walker::{classify_command_name, is_runtime_special_form, is_static_command_name};
// Only the drift-guard test consumes the list form directly.
#[cfg(all(test, feature = "subprocess"))]
pub(crate) use walker::RUNTIME_SPECIAL_FORMS;
