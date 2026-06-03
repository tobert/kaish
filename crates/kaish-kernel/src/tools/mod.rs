//! Tool system for kaish.
//!
//! Tools are the primary way to perform actions in kaish. Every command
//! is a tool — builtins and user-defined tools all implement
//! the same `Tool` trait.
//!
//! # Architecture
//!
//! ```text
//! ToolRegistry
//! ├── Builtins (echo, ls, cat, ...)
//! └── User Tools (defined via `tool` statements)
//! ```

mod builtin;
mod clap_schema;
mod context;
mod global_flags;
mod registry;
mod traits;

pub use builtin::register_builtins;
#[cfg(feature = "subprocess")]
pub use builtin::resolve_in_path;
pub use clap_schema::{params_from_clap, schema_from_clap};
pub use context::{ExecContext, OutputContext};
pub use global_flags::GlobalFlags;
pub use registry::ToolRegistry;
pub use traits::{is_global_output_flag, validate_against_schema, Tool, ToolArgs, ToolCtx, ToolSchema, ParamSchema};

/// Commands that consume bareword `key=value` argv (Arg::WordAssign) as
/// shell-assignment pairs and route them through `tool_args.named`. For every
/// other command, `key=value` lands as a positional `"key=value"` string —
/// matching bash (`cat foo=bar` opens a file named `foo=bar`).
///
/// Add to this list only for builtins that have a documented shell-assignment
/// argv contract (`export FOO=bar`, `alias greet='echo hi'`). Long-flag
/// `--key=value` is a separate AST node (`Arg::Named`) and always routes
/// through `tool_args.named` regardless.
pub const WORD_ASSIGN_BUILTINS: &[&str] = &["export", "alias", "unalias"];

pub fn accepts_word_assign(name: &str) -> bool {
    WORD_ASSIGN_BUILTINS.contains(&name)
}
