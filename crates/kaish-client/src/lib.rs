//! Client implementations for connecting to kaish kernels.
//!
//! This crate provides two ways to interact with a kaish kernel:
//!
//! - **EmbeddedClient**: Direct in-process access to a Kernel instance.
//!   Best for embedding kaish in other Rust applications.
//!
//! - **IpcClient**: Connects to a remote kernel via Unix socket using
//!   Cap'n Proto RPC. Best for CLI tools and external processes.
//!
//! # Example
//!
//! ```ignore
//! use kaish_client::{KernelClient, EmbeddedClient, IpcClient};
//! use kaish_kernel::{Kernel, KernelConfig};
//!
//! // Embedded client (in-process)
//! let kernel = Kernel::new(KernelConfig::default())?;
//! let client = EmbeddedClient::new(kernel);
//! let result = client.execute("echo hello").await?;
//!
//! // IPC client (remote)
//! let client = IpcClient::connect("/run/user/1000/kaish/default.sock").await?;
//! let result = client.execute("echo hello").await?;
//! ```

mod embedded;
mod ipc;
mod traits;

pub use embedded::EmbeddedClient;
pub use ipc::IpcClient;
pub use traits::{KernelClient, ClientResult, ClientError};
