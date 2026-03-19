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
#[cfg(feature = "native")]
mod git;
mod jobfs;
#[cfg(feature = "native")]
mod local;
mod memory;
mod router;
mod traits;

pub use builtin_fs::BuiltinFs;
#[cfg(feature = "native")]
pub use git::{FileStatus, GitVfs, LogEntry, StatusSummary, WorktreeInfo};
pub use jobfs::JobFs;
#[cfg(feature = "native")]
pub use local::LocalFs;
pub use memory::MemoryFs;
pub use router::{MountInfo, VfsRouter};
pub use traits::{DirEntry, DirEntryKind, Filesystem};
