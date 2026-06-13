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
mod jobfs;
mod router;

pub use builtin_fs::BuiltinFs;
// GitVfs + git types live in the kaish-tools-git crate (keeps libgit2 out
// of the kernel). Re-exported so `crate::vfs::GitVfs` paths keep working.
#[cfg(feature = "git")]
pub use kaish_tools_git::{FileStatus, GitVfs, LogEntry, StatusSummary, WorktreeInfo};
pub use jobfs::JobFs;
pub use router::{MountInfo, VfsRouter};

// The `Filesystem` trait + `LocalFs` + `MemoryFs` moved to the leaf `kaish-vfs`
// crate so out-of-tree backends (notably `GitVfs`, which wraps a `LocalFs`
// worktree) and overlay consumers can implement/compose the trait without
// depending on the kernel. Re-exported here so existing
// `crate::vfs::{Filesystem, DirEntry, LocalFs, MemoryFs}` paths keep working.
// `ByteBudget` rides along so a `with_backend` embedder can name the type it
// hands to `MemoryFs::with_budget` without a direct kaish-vfs dependency.
pub use kaish_vfs::{ByteBudget, DevFs, DirEntry, DirEntryKind, Filesystem, MemoryFs};
#[cfg(feature = "localfs")]
pub use kaish_vfs::LocalFs;
