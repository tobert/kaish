//! Interpreter module for kaish.
//!
//! This module provides expression evaluation, variable scope management,
//! and the structured result type (`$?`) that every command returns.
//!
//! # Architecture
//!
//! The interpreter is built in layers:
//!
//! - **ExecResult**: The structured result of every command, accessible as `$?`
//! - **Scope**: Variable bindings with nested frames and path resolution
//! - **Evaluator**: Reduces expressions to values
//!
//! # Command Substitution
//!
//! `$(pipeline)` expressions are executed by the async evaluator in the kernel
//! (`kernel.rs`), which resolves them to literal values before any synchronous
//! expression evaluation runs. The sync [`eval_expr`] evaluator therefore never
//! sees a `CommandSubst` node; encountering one is a loud error, not a silent
//! empty string.
//!
//! # Example
//!
//! ```
//! use kaish_kernel::interpreter::{Scope, eval_expr};
//! use kaish_kernel::ast::{Expr, Value};
//!
//! let mut scope = Scope::new();
//! scope.set("X", Value::Int(42));
//!
//! let expr = Expr::VarRef(kaish_kernel::ast::VarPath::simple("X"));
//! let result = eval_expr(&expr, &mut scope).unwrap();
//! assert_eq!(result, Value::Int(42));
//! ```

mod control_flow;
mod eval;
mod result;
mod scope;

pub use control_flow::ControlFlow;
pub use eval::{eval_expr, expand_tilde, is_collection, numeric_compare, resolve_default, resolve_length, scalar_test_operand_error, strip_leading_tabs, structured_boundary_error, structured_export_error, value_defaults_on_emptiness, values_equal, value_to_bool, value_to_exit_code, value_length, value_to_string, value_to_string_with_tilde, value_to_text_sink, value_to_text_sink_named, values_to_text_sink_named, EvalError, EvalResult, Evaluator, HeredocAssembler};
pub use result::{apply_output_format, hex_dump, json_to_value, json_to_value_no_envelope, value_to_json, EntryType, ExecResult, LatchRequest, OutputData, OutputFormat, OutputNode, OutputPayload};
pub use scope::{PathError, Scope};
// Crate-internal: the reduced sync evaluator (scheduler/pipeline.rs) reuses the
// resolver error-message shape without widening the public API.
pub(crate) use eval::format_path;
