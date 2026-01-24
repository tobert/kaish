//! Virtual Filesystem (VFS) for kaish.
//!
//! The VFS provides a unified interface over multiple filesystem backends:
//!
//! - **MemoryFs**: In-memory ephemeral storage (for `/scratch`)
//! - **LocalFs**: Real filesystem access (for mounted worktrees)
//! - **VfsRouter**: Routes paths to mounted backends
//!
//! # Design
//!
//! Kaish kernels own `/` in their VFS. Backends are mounted at paths:
//!
//! ```text
//! /                      # kernel root
//! ├── /scratch/          # MemoryFs (ephemeral)
//! ├── /mnt/project/      # LocalFs (worktree, rw)
//! └── /mnt/reference/    # LocalFs (repo, ro)
//! ```
//!
//! The router finds the longest matching mount point and delegates operations.

mod local;
mod memory;
mod router;
mod traits;

pub use local::LocalFs;
pub use memory::MemoryFs;
pub use router::{MountInfo, VfsRouter};
pub use traits::{DirEntry, EntryType, Filesystem, Metadata};
