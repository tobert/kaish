//! Stable plugin API for kaish tools.
//!
//! This leaf crate defines the contract that every kaish tool — builtin or
//! third-party — implements, without pulling in `kaish-kernel`'s parser,
//! interpreter, or async runtime internals. It exists so out-of-tree tool
//! bundles (`kaish-tools-host`, `kaish-tools-git`, a hypothetical
//! `kaish-tools-perl`) can be written against a small, audited surface.
//!
//! # The surface
//!
//! - [`Tool`] — the trait every command implements. Its `execute` takes a
//!   `&mut dyn ToolCtx`, so the concrete kernel context never leaks into the
//!   public API.
//! - [`ToolCtx`] — the **trimmed** execution context. It exposes only what a
//!   well-behaved, portable tool needs (backend I/O, cwd, variable access,
//!   output format). Trusted in-tree builtins that need deeper kernel state
//!   (job control, pipes, the dispatcher) recover the concrete context via
//!   [`ToolCtx::as_any_mut`] — a documented escape hatch, not part of the
//!   portable contract.
//! - [`KernelBackend`] — the I/O + tool-dispatch backend a tool reaches
//!   through `ctx.backend()`.
//! - [`GlobalFlags`], [`schema_from_clap`], [`validate_against_schema`] — the
//!   clap-reflection and validation machinery shared by all builtins.
//!
//! The pure-data types tools traffic in (`Value`, `ToolArgs`, `ToolSchema`,
//! `ExecResult`, `OutputData`, …) live one layer down in `kaish-types`.

mod backend;
mod clap_schema;
mod ctx;
mod global_flags;
mod issue;
mod tool;

pub use backend::KernelBackend;
pub use clap_schema::{params_from_clap, schema_from_clap, schema_tree_from_clap};
pub use ctx::ToolCtx;
pub use global_flags::GlobalFlags;
pub use issue::{IssueCode, Severity, Span, ValidationIssue};
pub use tool::{is_global_output_flag, validate_against_schema, Tool};

// Re-export the data types tool authors need most often, so a tool crate can
// depend on just `kaish-tool-api` for the common case.
pub use kaish_types::{
    ExecResult, OutputData, OutputFormat, ParamSchema, ToolArgs, ToolSchema, Value,
};
