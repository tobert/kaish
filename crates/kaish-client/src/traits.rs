//! Common trait for kernel clients.

use async_trait::async_trait;
use thiserror::Error;

use kaish_kernel::ast::Value;
use kaish_kernel::interpreter::ExecResult;

/// Result type for client operations.
pub type ClientResult<T> = Result<T, ClientError>;

/// Errors that can occur when using a kernel client.
#[derive(Debug, Error)]
pub enum ClientError {
    /// Connection to the kernel failed.
    #[error("connection error: {0}")]
    Connection(String),

    /// The kernel returned an error during execution.
    #[error("execution error: {0}")]
    Execution(String),

    /// Cap'n Proto RPC error.
    #[error("rpc error: {0}")]
    Rpc(#[from] capnp::Error),

    /// I/O error.
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    /// UTF-8 decoding error.
    #[error("utf8 error: {0}")]
    Utf8(#[from] std::str::Utf8Error),

    /// Cap'n Proto schema error.
    #[error("schema error: {0}")]
    Schema(#[from] capnp::NotInSchema),

    /// The kernel is not connected.
    #[error("not connected")]
    NotConnected,

    /// Other errors.
    #[error("{0}")]
    Other(#[from] anyhow::Error),
}

/// Common interface for interacting with a kaish kernel.
///
/// Both `EmbeddedClient` and `IpcClient` implement this trait,
/// allowing code to work with either client type.
#[async_trait(?Send)]
pub trait KernelClient {
    /// Execute kaish source code.
    ///
    /// Returns the result of the last statement executed.
    async fn execute(&self, input: &str) -> ClientResult<ExecResult>;

    /// Get a variable value.
    async fn get_var(&self, name: &str) -> ClientResult<Option<Value>>;

    /// Set a variable value.
    async fn set_var(&self, name: &str, value: Value) -> ClientResult<()>;

    /// List all variables.
    async fn list_vars(&self) -> ClientResult<Vec<(String, Value)>>;

    /// Get the current working directory.
    async fn cwd(&self) -> ClientResult<String>;

    /// Set the current working directory.
    async fn set_cwd(&self, path: &str) -> ClientResult<()>;

    /// Get the last execution result ($?).
    async fn last_result(&self) -> ClientResult<ExecResult>;

    /// Reset the kernel to initial state.
    async fn reset(&self) -> ClientResult<()>;

    /// Ping the kernel (health check).
    async fn ping(&self) -> ClientResult<String>;

    /// Shutdown the kernel.
    async fn shutdown(&self) -> ClientResult<()>;
}
