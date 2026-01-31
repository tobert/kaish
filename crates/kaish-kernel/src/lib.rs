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
//! - **Paths**: XDG-compliant path helpers

pub mod arithmetic;
pub mod ast;
pub mod backend;
pub mod glob;
pub mod help;
pub mod interpreter;
pub mod kernel;
pub mod lexer;
pub mod parser;
pub mod paths;
pub mod rpc;
pub mod scheduler;
pub mod tools;
pub mod validator;
pub mod vfs;
pub mod walker;

pub use backend::{
    BackendError, BackendResult, EntryInfo, KernelBackend, LocalBackend, PatchOp, ReadRange,
    ToolInfo, ToolResult, WriteMode,
};
pub use kernel::{Kernel, KernelConfig, VfsMountMode};
pub use rpc::KernelRpcServer;

// ═══════════════════════════════════════════════════════════════════════════
// Embedding Conveniences
// ═══════════════════════════════════════════════════════════════════════════

// Git types (for embedders that want direct GitVfs access)
pub use vfs::{FileStatus, GitVfs, LogEntry, StatusSummary, WorktreeInfo};

// Job observability (for embedders capturing command output)
pub use scheduler::{BoundedStream, StreamStats, DEFAULT_STREAM_MAX_SIZE, drain_to_stream};
pub use vfs::JobFs;

// XDG path primitives (embedders compose their own paths)
pub use paths::{home_dir, xdg_cache_home, xdg_config_home, xdg_data_home, xdg_runtime_dir};

// Tilde expansion utility
pub use interpreter::expand_tilde;
