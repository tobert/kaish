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
//! use kaish_types::approval::{Principal, PrincipalKind};
//!
//! // Embedded client (in-process). Name who this kernel runs for — an
//! // unnamed principal cannot be traced back to anyone once it raises an
//! // approval request.
//! let kernel = Kernel::new(
//!     KernelConfig::default().with_principal(Principal::new("my-agent", PrincipalKind::Agent)),
//! )?;
//! let client = EmbeddedClient::new(kernel);
//! let result = client.execute("echo hello").await?;
//! ```

mod embedded;
mod traits;

pub mod completion;

pub use embedded::EmbeddedClient;
pub use traits::{KernelClient, ClientResult, ClientError};
