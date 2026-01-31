//! Embedded client for direct in-process kernel access.
//!
//! The `EmbeddedClient` wraps a `Kernel` instance and implements `KernelClient`,
//! allowing direct access without network overhead. This is ideal for:
//!
//! - Embedding kaish in other Rust applications
//! - Unit testing
//! - Single-process use cases

use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use async_trait::async_trait;

use kaish_kernel::ast::Value;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::vfs::Filesystem;
use kaish_kernel::{Kernel, KernelConfig};

use crate::traits::{ClientError, ClientResult, KernelClient};

/// Generate a unique blob ID.
fn generate_blob_id() -> String {
    static COUNTER: AtomicU64 = AtomicU64::new(0);

    let timestamp = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos())
        .unwrap_or(0);
    let count = COUNTER.fetch_add(1, Ordering::SeqCst);

    format!("{:x}-{:x}", timestamp, count)
}

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

    async fn read_blob(&self, id: &str) -> ClientResult<Vec<u8>> {
        let vfs = self.kernel.vfs();
        let path = PathBuf::from(format!("/v/blobs/{}", id));

        vfs.read(&path)
            .await
            .map_err(|e| ClientError::Io(e))
    }

    async fn write_blob(&self, content_type: &str, data: &[u8]) -> ClientResult<String> {
        let vfs = self.kernel.vfs();
        let id = generate_blob_id();
        let path = PathBuf::from(format!("/v/blobs/{}", id));

        // Ensure parent directory exists
        let parent = Path::new("/v/blobs");
        if let Err(e) = vfs.mkdir(parent).await {
            // Ignore "already exists" errors
            if e.kind() != std::io::ErrorKind::AlreadyExists {
                tracing::warn!("Failed to create blob directory: {}", e);
            }
        }

        // Store content type as metadata (could be extended later)
        tracing::debug!("Creating blob {} with content type {}", id, content_type);

        vfs.write(&path, data)
            .await
            .map_err(|e| ClientError::Io(e))?;

        Ok(id)
    }

    async fn delete_blob(&self, id: &str) -> ClientResult<bool> {
        let vfs = self.kernel.vfs();
        let path = PathBuf::from(format!("/v/blobs/{}", id));

        match vfs.remove(&path).await {
            Ok(()) => Ok(true),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(false),
            Err(e) => Err(ClientError::Io(e)),
        }
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

        // Transient kernel uses sandboxed mode with cwd=$HOME
        let cwd = client.cwd().await.expect("cwd failed");
        let home = std::env::var("HOME").unwrap_or_else(|_| "/".to_string());
        assert_eq!(cwd, home);

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

    #[tokio::test]
    async fn test_embedded_blob_write_read() {
        let client = EmbeddedClient::transient().expect("failed to create client");

        let data = b"hello blob world!";
        let id = client.write_blob("text/plain", data).await.expect("write_blob failed");

        assert!(!id.is_empty(), "blob id should not be empty");

        let read_data = client.read_blob(&id).await.expect("read_blob failed");
        assert_eq!(read_data, data);
    }

    #[tokio::test]
    async fn test_embedded_blob_delete() {
        let client = EmbeddedClient::transient().expect("failed to create client");

        let data = b"blob to delete";
        let id = client.write_blob("application/octet-stream", data).await.expect("write_blob failed");

        // Verify it exists
        let read_data = client.read_blob(&id).await.expect("read_blob failed");
        assert_eq!(read_data, data);

        // Delete it
        let deleted = client.delete_blob(&id).await.expect("delete_blob failed");
        assert!(deleted, "blob should have been deleted");

        // Verify it's gone
        let result = client.read_blob(&id).await;
        assert!(result.is_err(), "blob should not exist after deletion");
    }

    #[tokio::test]
    async fn test_embedded_blob_delete_nonexistent() {
        let client = EmbeddedClient::transient().expect("failed to create client");

        let deleted = client.delete_blob("nonexistent-blob-id").await.expect("delete_blob failed");
        assert!(!deleted, "deleting nonexistent blob should return false");
    }

    #[tokio::test]
    async fn test_embedded_blob_large_data() {
        let client = EmbeddedClient::transient().expect("failed to create client");

        // Create 1MB of data
        let data: Vec<u8> = (0..1024 * 1024).map(|i| (i % 256) as u8).collect();
        let id = client.write_blob("application/octet-stream", &data).await.expect("write_blob failed");

        let read_data = client.read_blob(&id).await.expect("read_blob failed");
        assert_eq!(read_data.len(), data.len());
        assert_eq!(read_data, data);
    }
}
