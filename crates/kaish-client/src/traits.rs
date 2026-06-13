//! Common trait for kernel clients.

use std::collections::HashMap;

use async_trait::async_trait;
use thiserror::Error;

use kaish_kernel::ast::Value;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::tools::ToolSchema;

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

    /// I/O error.
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),

    /// UTF-8 decoding error.
    #[error("utf8 error: {0}")]
    Utf8(#[from] std::str::Utf8Error),

    /// The kernel is not connected.
    #[error("not connected")]
    NotConnected,

    /// Other errors.
    #[error("{0}")]
    Other(#[from] anyhow::Error),
}

/// Common interface for interacting with a kaish kernel.
///
/// Both `EmbeddedClient` and custom client implementations can implement this trait,
/// allowing code to work with any client type.
#[async_trait(?Send)]
pub trait KernelClient {
    /// Execute kaish source code.
    ///
    /// Returns the result of the last statement executed.
    async fn execute(&self, input: &str) -> ClientResult<ExecResult>;

    /// Execute kaish source code with a transient overlay of exported variables.
    ///
    /// The overlay vars are visible (and exported to subprocesses) for the
    /// duration of this call only, then removed. Names already exported in
    /// the persistent state retain their outer value on return.
    async fn execute_with_vars(
        &self,
        input: &str,
        vars: HashMap<String, Value>,
    ) -> ClientResult<ExecResult>;

    /// Get a variable value.
    async fn get_var(&self, name: &str) -> ClientResult<Option<Value>>;

    /// Set a variable value.
    async fn set_var(&self, name: &str, value: Value) -> ClientResult<()>;

    /// List all variables.
    async fn list_vars(&self) -> ClientResult<Vec<(String, Value)>>;

    /// List the schemas of all available tools (builtins + registered tools).
    ///
    /// Each [`ToolSchema`] carries the tool's name, description, parameters
    /// (flags and positionals, with types/aliases/defaults), and examples.
    /// This is the introspection surface embedders use to build command and
    /// option completion, generate help, or validate arguments client-side.
    async fn tool_schemas(&self) -> ClientResult<Vec<ToolSchema>>;

    /// Report whether a user-defined function with the given name exists.
    ///
    /// Lets a frontend probe for optional hooks (e.g. a `kaish_prompt`
    /// function that customizes the prompt) before invoking them.
    async fn has_function(&self, name: &str) -> ClientResult<bool>;

    /// Cancel the kernel's in-flight execution.
    ///
    /// Signals the kernel's cancellation token, which cascades to any
    /// attached forks and external child processes. Used by interactive
    /// frontends to wire Ctrl-C to the running statement. Idempotent and
    /// safe to call when nothing is executing.
    async fn cancel(&self) -> ClientResult<()>;

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

    /// Read a blob by ID.
    ///
    /// Returns the blob contents as raw bytes.
    async fn read_blob(&self, id: &str) -> ClientResult<Vec<u8>>;

    /// Write a blob and return its ID.
    ///
    /// The blob is stored in `/v/blobs/{id}` and can be read back via `read_blob`.
    async fn write_blob(&self, content_type: &str, data: &[u8]) -> ClientResult<String>;

    /// Delete a blob by ID.
    ///
    /// Returns true if the blob was deleted, false if it didn't exist.
    async fn delete_blob(&self, id: &str) -> ClientResult<bool>;
}
