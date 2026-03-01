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
pub(crate) mod backend_walker_fs;
pub mod dispatch;
pub mod help;
pub mod ignore_config;
pub mod interpreter;
pub mod output_limit;
pub mod kernel;
pub mod lexer;
pub mod nonce;
pub mod parser;
pub mod paths;
pub mod scheduler;
pub mod tools;
pub mod validator;
pub mod vfs;
#[cfg(unix)]
pub mod terminal;

// Re-export kaish_glob as our glob/walker modules for backwards compatibility
pub use kaish_glob as glob_crate;

/// Glob pattern matching (re-exported from kaish-glob).
pub mod glob {
    pub use kaish_glob::glob::{contains_glob, expand_braces, glob_match};
}

/// Recursive file walking infrastructure (re-exported from kaish-glob).
pub mod walker {
    pub use kaish_glob::{
        EntryTypes, FileWalker, FilterResult, GlobPath, IgnoreFilter, IncludeExclude,
        PathSegment, PatternError, WalkOptions, WalkerDirEntry, WalkerError, WalkerFs,
    };
    pub use crate::backend_walker_fs::BackendWalkerFs;
}

pub use backend::{
    BackendError, BackendResult, KernelBackend, LocalBackend, PatchOp, ReadRange,
    ToolInfo, ToolResult, VirtualOverlayBackend, WriteMode,
};
pub use dispatch::{BackendDispatcher, CommandDispatcher, PipelinePosition};
pub use ignore_config::{IgnoreConfig, IgnoreScope};
pub use kernel::{Kernel, KernelConfig, VfsMountMode};
pub use output_limit::OutputLimitConfig;

// ═══════════════════════════════════════════════════════════════════════════
// Embedding Conveniences
// ═══════════════════════════════════════════════════════════════════════════

// Backend with /v/* support for embedders
//
// Use `Kernel::with_backend()` to provide a custom backend with automatic
// `/v/*` path support (job observability, blob storage):
//
// ```ignore
// let kernel = Kernel::with_backend(my_backend, config, |vfs| {
//     vfs.mount_arc("/v/docs", docs_fs);
// })?;
// ```

// Git types (for embedders that want direct GitVfs access)
pub use vfs::{FileStatus, GitVfs, LogEntry, StatusSummary, WorktreeInfo};

// Job observability (for embedders capturing command output)
pub use scheduler::{BoundedStream, StreamStats, DEFAULT_STREAM_MAX_SIZE, drain_to_stream};
pub use vfs::JobFs;

// XDG path primitives (embedders compose their own paths)
pub use paths::{home_dir, xdg_cache_home, xdg_config_home, xdg_data_home, xdg_runtime_dir};

// Tilde expansion utility
pub use interpreter::expand_tilde;
