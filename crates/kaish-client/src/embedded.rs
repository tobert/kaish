//! Embedded client for direct in-process kernel access.
//!
//! The `EmbeddedClient` wraps a `Kernel` instance and implements `KernelClient`,
//! allowing direct access without network overhead. This is ideal for:
//!
//! - Embedding kaish in other Rust applications
//! - Unit testing
//! - Single-process use cases

use std::path::PathBuf;
use std::sync::Arc;

use async_trait::async_trait;

use kaish_kernel::ast::Value;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};

use crate::traits::{ClientError, ClientResult, KernelClient};

/// A client that wraps a `Kernel` directly for in-process access.
///
/// # Example
///
/// ```ignore
/// use kaish_client::EmbeddedClient;
/// use kaish_kernel::{Kernel, KernelConfig};
///
/// let kernel = Kernel::new(KernelConfig::transient())?;
/// let client = EmbeddedClient::new(kernel);
///
/// let result = client.execute("X=42").await?;
/// assert!(result.ok());
///
/// let value = client.get_var("X").await?;
/// assert_eq!(value, Some(Value::Int(42)));
/// ```
pub struct EmbeddedClient {
    kernel: Arc<Kernel>,
}

impl EmbeddedClient {
    /// Create a new embedded client wrapping the given kernel.
    pub fn new(kernel: Kernel) -> Self {
        Self {
            kernel: Arc::new(kernel),
        }
    }

    /// Create a new embedded client with a transient (non-persistent) kernel.
    pub fn transient() -> ClientResult<Self> {
        let kernel = Kernel::new(KernelConfig::transient())
            .map_err(ClientError::Other)?;
        Ok(Self::new(kernel))
    }

    /// Create a new embedded client with default configuration.
    pub fn with_defaults() -> ClientResult<Self> {
        let kernel = Kernel::new(KernelConfig::default())
            .map_err(ClientError::Other)?;
        Ok(Self::new(kernel))
    }

    /// Get a reference to the underlying kernel.
    pub fn kernel(&self) -> &Kernel {
        &self.kernel
    }
}

#[async_trait(?Send)]
impl KernelClient for EmbeddedClient {
    async fn execute(&self, input: &str) -> ClientResult<ExecResult> {
        self.kernel
            .execute(input)
            .await
            .map_err(|e| ClientError::Execution(e.to_string()))
    }

    async fn get_var(&self, name: &str) -> ClientResult<Option<Value>> {
        Ok(self.kernel.get_var(name).await)
    }

    async fn set_var(&self, name: &str, value: Value) -> ClientResult<()> {
        self.kernel.set_var(name, value).await;
        Ok(())
    }

    async fn list_vars(&self) -> ClientResult<Vec<(String, Value)>> {
        Ok(self.kernel.list_vars().await)
    }

    async fn cwd(&self) -> ClientResult<String> {
        Ok(self.kernel.cwd().await.to_string_lossy().to_string())
    }

    async fn set_cwd(&self, path: &str) -> ClientResult<()> {
        self.kernel.set_cwd(PathBuf::from(path)).await;
        Ok(())
    }

    async fn last_result(&self) -> ClientResult<ExecResult> {
        Ok(self.kernel.last_result().await)
    }

    async fn reset(&self) -> ClientResult<()> {
        self.kernel
            .reset()
            .await
            .map_err(ClientError::Other)
    }

    async fn ping(&self) -> ClientResult<String> {
        Ok("pong".to_string())
    }

    async fn shutdown(&self) -> ClientResult<()> {
        // For embedded client, shutdown is a no-op since we don't own the kernel lifecycle
        // The kernel will be dropped when the client is dropped
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_embedded_transient() {
        let client = EmbeddedClient::transient().expect("failed to create client");
        let result = client.ping().await.expect("ping failed");
        assert_eq!(result, "pong");
    }

    #[tokio::test]
    async fn test_embedded_execute() {
        let client = EmbeddedClient::transient().expect("failed to create client");
        let result = client.execute("echo hello").await.expect("execute failed");
        assert!(result.ok());
        assert_eq!(result.out.trim(), "hello");
    }

    #[tokio::test]
    async fn test_embedded_variables() {
        let client = EmbeddedClient::transient().expect("failed to create client");

        // Set via execute
        client.execute("X=42").await.expect("set failed");
        let value = client.get_var("X").await.expect("get failed");
        assert_eq!(value, Some(Value::Int(42)));

        // Set via API
        client.set_var("Y", Value::String("hello".into())).await.expect("set_var failed");
        let value = client.get_var("Y").await.expect("get failed");
        assert_eq!(value, Some(Value::String("hello".into())));

        // List vars
        let vars = client.list_vars().await.expect("list failed");
        assert!(vars.iter().any(|(n, _)| n == "X"));
        assert!(vars.iter().any(|(n, _)| n == "Y"));
    }

    #[tokio::test]
    async fn test_embedded_cwd() {
        let client = EmbeddedClient::transient().expect("failed to create client");

        let cwd = client.cwd().await.expect("cwd failed");
        assert_eq!(cwd, "/");

        client.set_cwd("/tmp").await.expect("set_cwd failed");
        let cwd = client.cwd().await.expect("cwd failed");
        assert_eq!(cwd, "/tmp");
    }

    #[tokio::test]
    async fn test_embedded_reset() {
        let client = EmbeddedClient::transient().expect("failed to create client");

        client.execute("X=1").await.expect("set failed");
        assert!(client.get_var("X").await.expect("get failed").is_some());

        client.reset().await.expect("reset failed");
        assert!(client.get_var("X").await.expect("get failed").is_none());
    }

    #[tokio::test]
    async fn test_embedded_last_result() {
        let client = EmbeddedClient::transient().expect("failed to create client");

        client.execute("echo test").await.expect("execute failed");
        let last = client.last_result().await.expect("last_result failed");
        assert!(last.ok());
        assert_eq!(last.out.trim(), "test");
    }
}
