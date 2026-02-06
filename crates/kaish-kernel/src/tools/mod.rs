//! Tool system for kaish.
//!
//! Tools are the primary way to perform actions in kaish. Every command
//! is a tool — builtins, user-defined tools, and MCP tools all implement
//! the same `Tool` trait.
//!
//! # Architecture
//!
//! ```text
//! ToolRegistry
//! ├── Builtins (echo, ls, cat, ...)
//! ├── User Tools (defined via `tool` statements)
//! └── MCP Tools (from connected MCP servers)
//! ```

mod builtin;
mod context;
mod registry;
mod traits;

pub use builtin::{register_builtins, resolve_in_path};
pub use context::{ExecContext, OutputContext};
pub use registry::ToolRegistry;
pub use traits::{extract_output_format, is_global_output_flag, validate_against_schema, Tool, ToolArgs, ToolSchema, ParamSchema};
