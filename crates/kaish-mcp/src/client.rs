//! MCP client wrapper.

use std::sync::Arc;

use anyhow::{Context, Result};
use rmcp::model::{CallToolRequestParams, CallToolResult, Tool as McpTool};
use rmcp::service::{RoleClient, RunningService, ServiceExt};
use rmcp::transport::{ConfigureCommandExt, TokioChildProcess};
use rmcp::ClientHandler;
use tokio::process::Command;
use tokio::sync::RwLock;

use crate::config::{McpConfig, McpTransport};

/// Minimal client handler that does nothing.
///
/// We only use the client for calling tools, so we don't need to handle
/// any server requests or notifications.
#[derive(Debug, Clone, Copy, Default)]
struct MinimalClientHandler;

impl ClientHandler for MinimalClientHandler {}

/// Type alias for the service wrapped in Arc for concurrent access.
type SharedService = Arc<RunningService<RoleClient, MinimalClientHandler>>;

/// Client for communicating with an MCP server.
///
/// Wraps rmcp's RunningService to provide tool discovery and invocation.
/// The service is wrapped in Arc to allow concurrent tool calls without
/// holding the lock during async operations.
pub struct McpClient {
    /// Server name for identification.
    name: String,
    /// The underlying rmcp running service, wrapped in Arc for sharing.
    service: RwLock<Option<SharedService>>,
    /// Cached tool list.
    tools: RwLock<Option<Vec<McpTool>>>,
    /// Transport config (for reconnection).
    config: McpConfig,
}

impl McpClient {
    /// Create a new MCP client from configuration.
    pub fn new(config: McpConfig) -> Self {
        Self {
            name: config.name.clone(),
            service: RwLock::new(None),
            tools: RwLock::new(None),
            config,
        }
    }

    /// Get the server name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Connect to the MCP server.
    pub async fn connect(&self) -> Result<()> {
        let service = match &self.config.transport {
            McpTransport::Stdio { command, args, env } => {
                let env_clone = env.clone();
                let args_clone = args.clone();

                let transport = TokioChildProcess::new(
                    Command::new(command).configure(move |cmd| {
                        cmd.args(&args_clone);
                        for (key, value) in &env_clone {
                            cmd.env(key, value);
                        }
                    }),
                )
                .context("Failed to create child process transport")?;

                MinimalClientHandler
                    .serve(transport)
                    .await
                    .map_err(|e| anyhow::anyhow!("Failed to initialize MCP connection: {}", e))?
            }
        };

        *self.service.write().await = Some(Arc::new(service));
        Ok(())
    }

    /// Disconnect from the MCP server.
    pub async fn disconnect(&self) -> Result<()> {
        if let Some(service) = self.service.write().await.take() {
            // Try to unwrap the Arc - if we're the only owner, cancel it.
            // If there are other references, they'll complete their calls.
            if let Ok(service) = Arc::try_unwrap(service) {
                service
                    .cancel()
                    .await
                    .map_err(|e| anyhow::anyhow!("Failed to cancel service: {}", e))?;
            }
        }
        *self.tools.write().await = None;
        Ok(())
    }

    /// Check if connected.
    pub async fn is_connected(&self) -> bool {
        self.service.read().await.is_some()
    }

    /// List available tools from the MCP server.
    pub async fn list_tools(&self) -> Result<Vec<McpTool>> {
        // Return cached tools if available
        if let Some(tools) = self.tools.read().await.as_ref() {
            return Ok(tools.clone());
        }

        self.refresh_tools().await
    }

    /// Refresh the tool cache from the MCP server.
    ///
    /// This forces a fresh fetch of tools, invalidating any cached version.
    pub async fn refresh_tools(&self) -> Result<Vec<McpTool>> {
        // Get Arc clone of the service, releasing the lock immediately.
        let service = self
            .service
            .read()
            .await
            .as_ref()
            .context("Not connected to MCP server")?
            .clone();

        let tools = service
            .list_all_tools()
            .await
            .map_err(|e| anyhow::anyhow!("Failed to list tools: {}", e))?;

        // Update the cache
        *self.tools.write().await = Some(tools.clone());

        Ok(tools)
    }

    /// Invalidate the tool cache.
    ///
    /// The next call to `list_tools` will fetch fresh data from the server.
    pub async fn invalidate_tool_cache(&self) {
        *self.tools.write().await = None;
    }

    /// Call a tool on the MCP server.
    ///
    /// Tool calls can run concurrently - we clone the Arc and release
    /// the lock before awaiting the call.
    pub async fn call_tool(
        &self,
        name: &str,
        arguments: Option<serde_json::Map<String, serde_json::Value>>,
    ) -> Result<CallToolResult> {
        // Get Arc clone of the service, releasing the lock immediately.
        // This allows concurrent tool calls instead of serializing them.
        let service = self
            .service
            .read()
            .await
            .as_ref()
            .context("Not connected to MCP server")?
            .clone();

        let result = service
            .call_tool(CallToolRequestParams {
                name: name.to_string().into(),
                arguments,
                task: None,
                meta: None,
            })
            .await
            .map_err(|e| anyhow::anyhow!("Failed to call tool: {}", e))?;

        Ok(result)
    }

    /// Get peer (server) information.
    pub async fn peer_info(&self) -> Result<String> {
        let service = self
            .service
            .read()
            .await
            .as_ref()
            .context("Not connected to MCP server")?
            .clone();

        let info = service.peer_info();
        Ok(format!("{:?}", info))
    }
}

impl std::fmt::Debug for McpClient {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("McpClient")
            .field("name", &self.name)
            .field("config", &self.config)
            .finish()
    }
}

/// Create an McpClient from a stdio command.
pub fn stdio_client(
    name: impl Into<String>,
    command: impl Into<String>,
    args: Vec<String>,
) -> McpClient {
    McpClient::new(McpConfig {
        name: name.into(),
        transport: McpTransport::Stdio {
            command: command.into(),
            args,
            env: Vec::new(),
        },
    })
}

/// Wrap an McpClient in an Arc for sharing.
pub fn shared_client(client: McpClient) -> Arc<McpClient> {
    Arc::new(client)
}
