//! Pure data types for kaish — structured output, values, tool schemas.
//!
//! This crate is a leaf dependency with no async runtime, no parser, no I/O.
//! It exists so that consumers (kaijutsu, external tools) can work with kaish's
//! type system without pulling kaish-kernel's ~60 transitive deps.

pub mod backend;
pub mod bytes;
pub mod clock;
pub mod command;
pub mod dir_entry;
pub mod job;
pub mod kernel;
pub mod output;
pub mod result;
pub mod tool;
pub mod value;

// Flat re-exports for convenience
pub use backend::*;
pub use bytes::{bytes_to_envelope, envelope_to_bytes, hex_dump};
pub use command::*;
pub use dir_entry::*;
pub use job::*;
pub use kernel::*;
pub use output::*;
pub use result::*;
pub use tool::*;
pub use value::*;
