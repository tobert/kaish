//! kaish-schema: Cap'n Proto type definitions for the 会sh kernel protocol.
//!
//! This crate provides generated types from `schema/kaish.capnp`, including:
//!
//! - **Value types**: The core `Value` union for representing data
//! - **Kernel protocol**: RPC interface for kernel communication
//! - **State types**: Serialization formats for persistence
//!
//! # Usage
//!
//! ```rust,ignore
//! use kaish_schema::kaish_capnp::{Value, ExecResult, Kernel};
//! ```

#[allow(clippy::unwrap_used, clippy::expect_used, dead_code)]
pub mod kaish_capnp {
    #[cfg(feature = "codegen")]
    include!(concat!(env!("OUT_DIR"), "/kaish_capnp.rs"));

    #[cfg(not(feature = "codegen"))]
    include!("kaish_capnp.rs");
}

pub use kaish_capnp::*;
