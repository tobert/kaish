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
#[cfg(feature = "native")]
pub use builtin::resolve_in_path;
pub use clap_schema::{params_from_clap, schema_from_clap};
pub use context::{ExecContext, OutputContext};
pub use global_flags::GlobalFlags;
pub use registry::ToolRegistry;
pub use traits::{is_global_output_flag, validate_against_schema, Tool, ToolArgs, ToolSchema, ParamSchema};
