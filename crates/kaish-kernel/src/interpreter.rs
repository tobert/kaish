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
//! The evaluator supports `$(pipeline)` expressions through the `Executor` trait.
//! Higher layers (L6: Pipes & Jobs) implement this trait to provide actual
//! command execution. For standalone expression evaluation, use `NoOpExecutor`.
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
pub use eval::{eval_expr, expand_tilde, value_to_bool, value_to_string, value_to_string_with_tilde, EvalError, EvalResult, Evaluator, Executor, NoOpExecutor};
pub use result::{apply_output_format, json_to_value, value_to_json, EntryType, ExecResult, OutputData, OutputFormat, OutputNode};
pub use scope::Scope;
