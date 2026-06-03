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
mod memory;
mod router;

pub use builtin_fs::BuiltinFs;
#[cfg(feature = "git")]
pub use git::{FileStatus, GitVfs, LogEntry, StatusSummary, WorktreeInfo};
pub use jobfs::JobFs;
pub use memory::MemoryFs;
pub use router::{MountInfo, VfsRouter};

// The `Filesystem` trait + `LocalFs` moved to the leaf `kaish-vfs` crate so
// out-of-tree backends (notably `GitVfs`, which wraps a `LocalFs` worktree)
// can implement the trait without depending on the kernel. Re-exported here so
// existing `crate::vfs::{Filesystem, DirEntry, LocalFs}` paths keep working.
pub use kaish_vfs::{DirEntry, DirEntryKind, Filesystem};
#[cfg(feature = "localfs")]
pub use kaish_vfs::LocalFs;
