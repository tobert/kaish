//! Client implementations for connecting to kaish kernels.
//!
//! This crate provides the `EmbeddedClient` for direct in-process access
//! to a Kernel instance. Best for embedding kaish in other Rust applications.
//!
//! # Example
//!
//! ```ignore
//! use kaish_client::{KernelClient, EmbeddedClient};
//! use kaish_kernel::{Kernel, KernelConfig};
//!
//! // Embedded client (in-process)
//! let kernel = Kernel::new(KernelConfig::default())?;
//! let client = EmbeddedClient::new(kernel);
//! let result = client.execute("echo hello").await?;
//! ```

mod embedded;
mod traits;

pub use embedded::EmbeddedClient;
pub use traits::{KernelClient, ClientResult, ClientError};
