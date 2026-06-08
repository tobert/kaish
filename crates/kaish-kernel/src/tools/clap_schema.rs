//! Bridge from `clap::Command` reflection → `ToolSchema`.
//!
//! Moved to the leaf `kaish-tool-api` crate so out-of-tree tool bundles can
//! reflect their clap structs into schemas the same way builtins do.
//! Re-exported here so existing `crate::tools::{schema_from_clap, …}` paths
//! keep working. See `docs/clap-migration.md`.

pub use kaish_tool_api::{params_from_clap, schema_from_clap, schema_tree_from_clap};
