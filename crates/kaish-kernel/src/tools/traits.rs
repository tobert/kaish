//! Core tool traits and types.
//!
//! The `Tool` trait, the trimmed `ToolCtx` execution context, and the
//! argument-validation machinery now live in the leaf `kaish-tool-api` crate
//! so out-of-tree tool bundles can implement them without depending on the
//! kernel. They are re-exported here so existing `crate::tools::…` paths keep
//! working.

pub use kaish_tool_api::{is_global_output_flag, validate_against_schema, Tool, ToolCtx};

// Data types re-exported from kaish-types.
pub use kaish_types::{ParamSchema, ToolArgs, ToolSchema};
