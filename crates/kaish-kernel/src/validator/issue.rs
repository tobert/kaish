//! Validation issues and formatting.
//!
//! These types moved to the leaf `kaish-tool-api` crate (the `Tool::validate`
//! contract returns them). Re-exported here so existing
//! `crate::validator::{ValidationIssue, IssueCode, Severity, Span}` paths keep
//! working and resolve to the same types the `Tool` trait uses.

pub use kaish_tool_api::{IssueCode, Severity, Span, ValidationIssue};
