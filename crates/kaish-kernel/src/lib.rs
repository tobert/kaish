//! kaish-kernel (核): The core of 会sh.
//!
//! This crate provides:
//!
//! - **Lexer**: Tokenizes kaish source code using logos
//! - **Parser**: Builds AST from tokens using chumsky
//! - **AST**: Type definitions for the abstract syntax tree
//! - **Interpreter**: Expression evaluation, scopes, and the `$?` result type
//! - **VFS**: Virtual filesystem with mount points
//! - **Tools**: Tool trait, registry, and builtin commands
//! - **Scheduler**: Pipeline execution and background job management
//! - **MCP**: Model Context Protocol client for external tools
//! - **State**: SQLite-backed persistence for kernel state

pub mod arithmetic;
pub mod ast;
pub mod backend;
pub mod glob;
pub mod interpreter;
pub mod kernel;
pub mod lexer;
pub mod mcp;
pub mod parser;
pub mod rpc;
pub mod scheduler;
pub mod state;
pub mod tools;
pub mod vfs;
pub mod walker;

pub use backend::{
    BackendError, BackendResult, EntryInfo, KernelBackend, LocalBackend, PatchOp, ReadRange,
    ToolInfo, ToolResult, WriteMode,
};
pub use kernel::{Kernel, KernelConfig};
pub use rpc::KernelRpcServer;
