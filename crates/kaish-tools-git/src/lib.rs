//! kaish's git tool bundle.
//!
//! This crate keeps libgit2 (a C dependency) out of `kaish-kernel`. It holds
//! both halves of kaish's git support:
//!
//! - [`GitVfs`] — a [`Filesystem`](kaish_vfs::Filesystem) backend over a git
//!   working tree (wraps a `LocalFs`).
//! - [`Git`] — the `git` builtin ([`Tool`](kaish_tool_api::Tool)), written
//!   entirely against the portable `ToolCtx` surface.
//!
//! The kernel registers [`Git`] behind its `git` feature; embedders that want
//! direct `GitVfs` access get it through the kernel's re-exports.

mod git_tool;
mod git_vfs;

pub use git_tool::Git;
pub use git_vfs::{FileStatus, GitVfs, LogEntry, StatusSummary, WorktreeInfo};
