//! ExecResult and output types — re-exported from kaish-types.
//!
//! The type definitions live in `kaish-types` (a leaf crate with no async deps).
//! This module re-exports them so `crate::interpreter::ExecResult` continues to work.

pub use kaish_types::output::{apply_output_format, EntryType, OutputData, OutputFormat, OutputNode};
pub use kaish_types::result::{json_to_value, value_to_json, ExecResult};
