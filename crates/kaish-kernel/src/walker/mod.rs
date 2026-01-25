//! Recursive file walking infrastructure for kaish.
//!
//! This module provides async file walking capabilities that work through VFS:
//!
//! - **GlobPath**: Path-aware glob matching with `**` (globstar) support
//! - **IgnoreFilter**: Gitignore-style pattern matching
//! - **IncludeExclude**: rsync-style include/exclude filters
//! - **FileWalker**: Core async iterator for directory traversal

mod filter;
mod glob_path;
mod ignore;
mod walker;

pub use filter::{FilterResult, IncludeExclude};
pub use glob_path::{GlobPath, PathSegment, PatternError};
pub use ignore::IgnoreFilter;
pub use walker::{EntryTypes, FileWalker, WalkOptions};
