//! kaish-glob: Glob matching and async file walking.
//!
//! Provides:
//! - **glob_match**: Shell-style glob pattern matching with brace expansion
//! - **GlobPath**: Path-aware glob matching with `**` (globstar) support
//! - **FileWalker**: Async recursive directory walker, generic over `WalkerFs`
//! - **IgnoreFilter**: Gitignore-style pattern filtering
//! - **IncludeExclude**: rsync-style include/exclude filters
//!
//! The walker is generic over `WalkerFs`, a minimal read-only filesystem trait.
//! Consumers implement `WalkerFs` to adapt their own filesystem abstraction.

mod filter;
pub mod glob;
mod glob_path;
mod ignore;
mod walker;

pub use filter::{FilterResult, IncludeExclude};
pub use glob::{contains_glob, expand_braces, glob_match};
pub use glob_path::{GlobPath, PathSegment, PatternError};
pub use ignore::IgnoreFilter;
pub use walker::{EntryTypes, ErrorCallback, FileWalker, WalkOptions};

use async_trait::async_trait;
use std::path::{Path, PathBuf};
use thiserror::Error;

/// Errors from filesystem operations within the walker.
#[derive(Debug, Error)]
pub enum WalkerError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("permission denied: {0}")]
    PermissionDenied(String),
    #[error("io error: {0}")]
    Io(String),
    #[error("symlink cycle detected: {0}")]
    SymlinkCycle(String),
}

/// Minimal read-only filesystem abstraction for the walker.
///
/// Implement this trait to adapt your project's filesystem layer
/// (VFS, real FS, CRDT blocks, etc.) to `FileWalker` and `IgnoreFilter`.
#[async_trait]
pub trait WalkerFs: Send + Sync {
    /// The directory entry type returned by `list_dir`.
    type DirEntry: WalkerDirEntry;

    /// List the entries in a directory.
    async fn list_dir(&self, path: &Path) -> Result<Vec<Self::DirEntry>, WalkerError>;

    /// Read the full contents of a file into memory.
    ///
    /// Currently used for loading `.gitignore` files. Implementations SHOULD
    /// impose a reasonable size limit to prevent accidental multi-gigabyte reads.
    async fn read_file(&self, path: &Path) -> Result<Vec<u8>, WalkerError>;

    /// Check if a path is a directory.
    async fn is_dir(&self, path: &Path) -> bool;

    /// Check if a path exists.
    async fn exists(&self, path: &Path) -> bool;

    /// Return the canonical (resolved) path, following symlinks.
    ///
    /// Used by `FileWalker` for symlink cycle detection when `follow_symlinks`
    /// is enabled. Implementations that support symlinks should resolve the path
    /// to its real location. The default returns the path unchanged.
    async fn canonicalize(&self, path: &Path) -> PathBuf {
        path.to_path_buf()
    }
}

/// A single entry returned by `WalkerFs::list_dir`.
pub trait WalkerDirEntry: Send {
    /// The entry name (file or directory name, not full path).
    fn name(&self) -> &str;

    /// True if this entry is a directory.
    fn is_dir(&self) -> bool;

    /// True if this entry is a regular file.
    fn is_file(&self) -> bool;

    /// True if this entry is a symbolic link.
    fn is_symlink(&self) -> bool;
}
