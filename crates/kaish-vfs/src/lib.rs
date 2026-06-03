//! kaish's virtual filesystem contract.
//!
//! This leaf crate holds the [`Filesystem`] trait every VFS backend implements
//! and the [`LocalFs`] real-filesystem backend. It exists so backends that
//! aren't part of the kernel — most importantly `GitVfs`, which wraps a
//! `LocalFs` working tree — can live in their own crates without depending on
//! `kaish-kernel`.
//!
//! `DirEntry`/`DirEntryKind` (the data the trait traffics in) live one layer
//! down in `kaish-types` and are re-exported here for convenience.

mod traits;

pub use traits::{DirEntry, DirEntryKind, Filesystem};

// `LocalFs` pulls in `tokio/fs`; gated so the in-memory/wasm sandbox build
// (which doesn't enable `localfs`) stays free of a real-filesystem dependency.
#[cfg(feature = "localfs")]
mod local;
#[cfg(feature = "localfs")]
pub use local::LocalFs;
