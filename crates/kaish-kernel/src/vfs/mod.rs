//! Virtual Filesystem (VFS) for kaish.
//!
//! The VFS provides a unified interface over multiple filesystem backends:
//!
//! - **MemoryFs**: In-memory ephemeral storage (for `/v`, tests)
//! - **LocalFs**: Real filesystem access (for mounted worktrees)
//! - **VfsRouter**: Routes paths to mounted backends
//!
//! # Design
//!
//! Kaish kernels own `/` in their VFS. Backends are mounted at paths:
//!
//! ```text
//! /                      # kernel root
//! ├── /v/                # MemoryFs (blobs, jobs)
//! ├── /mnt/project/      # LocalFs (worktree, rw)
//! └── /mnt/reference/    # LocalFs (repo, ro)
//! ```
//!
//! The router finds the longest matching mount point and delegates operations.

mod builtin_fs;
#[cfg(feature = "git")]
mod git;
mod jobfs;
#[cfg(feature = "localfs")]
mod local;
mod memory;
mod router;
mod traits;

pub use builtin_fs::BuiltinFs;
#[cfg(feature = "git")]
pub use git::{FileStatus, GitVfs, LogEntry, StatusSummary, WorktreeInfo};
pub use jobfs::JobFs;
#[cfg(feature = "localfs")]
pub use local::LocalFs;
pub use memory::MemoryFs;
pub use router::{MountInfo, VfsRouter};
pub use traits::{DirEntry, DirEntryKind, Filesystem};
