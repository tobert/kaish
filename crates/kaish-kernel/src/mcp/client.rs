//! MCP client wrapper.

use std::sync::Arc;

use anyhow::{Context, Result};
use rmcp::model::{CallToolRequestParam, CallToolResult, Tool as McpTool};
use rmcp::service::{RoleClient, RunningService, ServiceExt};
use rmcp::transport::{ConfigureCommandExt, TokioChildProcess};
use rmcp::ClientHandler;
use tokio::process::Command;
use tokio::sync::Mutex;

/// Configuration for connecting to an MCP server.
#[derive(Debug, Clone)]
pub struct McpConfig {
    /// Human-readable name for this server.
    pub name: String,
    /// Transport configuration.
    pub transport: McpTransport,
}

/// Transport type for MCP connection.
#[derive(Debug, Clone)]
pub enum McpTransport {
    /// Stdio transport via child process.
    Stdio {
        /// Command to execute.
        command: String,
        /// Arguments to pass.
        args: Vec<String>,
        /// Environment variables.
        env: Vec<(String, String)>,
    },
}

/// Minimal client handler that does nothing.
///
/// We only use the client for calling tools, so we don't need to handle
/// any server requests or notifications.
#[derive(Debug, Clone, Copy, Default)]
struct MinimalClientHandler;

impl ClientHandler for MinimalClientHandler {}

/// Client for communicating with an MCP server.
///
/// Wraps rmcp's RunningService to provide tool discovery and invocation.
pub struct McpClient {
    /// Server name for identification.
    name: String,
    /// The underlying rmcp running service.
    service: Mutex<Option<RunningService<RoleClient, MinimalClientHandler>>>,
    /// Cached tool list.
    tools: Mutex<Option<Vec<McpTool>>>,
    /// Transport config (for reconnection).
    config: McpConfig,
}

impl McpClient {
    /// Create a new MCP client from configuration.
    pub fn new(config: McpConfig) -> Self {
        Self {
            name: config.name.clone(),
            service: Mutex::new(None),
            tools: Mutex::new(None),
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

        *self.service.lock().await = Some(service);
        Ok(())
    }

    /// Disconnect from the MCP server.
    pub async fn disconnect(&self) -> Result<()> {
        if let Some(service) = self.service.lock().await.take() {
            service
                .cancel()
                .await
                .map_err(|e| anyhow::anyhow!("Failed to cancel service: {}", e))?;
        }
        *self.tools.lock().await = None;
        Ok(())
    }

    /// Check if connected.
    pub async fn is_connected(&self) -> bool {
        self.service.lock().await.is_some()
    }

    /// List available tools from the MCP server.
    pub async fn list_tools(&self) -> Result<Vec<McpTool>> {
        // Return cached tools if available
        if let Some(tools) = self.tools.lock().await.as_ref() {
            return Ok(tools.clone());
        }

        let service = self.service.lock().await;
        let service = service
            .as_ref()
            .context("Not connected to MCP server")?;

        let tools = service
            .list_all_tools()
            .await
            .map_err(|e| anyhow::anyhow!("Failed to list tools: {}", e))?;

        // Cache the tools
        *self.tools.lock().await = Some(tools.clone());

        Ok(tools)
    }

    /// Call a tool on the MCP server.
    pub async fn call_tool(
        &self,
        name: &str,
        arguments: Option<serde_json::Map<String, serde_json::Value>>,
    ) -> Result<CallToolResult> {
        let service = self.service.lock().await;
        let service = service
            .as_ref()
            .context("Not connected to MCP server")?;

        let result = service
            .call_tool(CallToolRequestParam {
                name: name.to_string().into(),
                arguments,
                task: None,
            })
            .await
            .map_err(|e| anyhow::anyhow!("Failed to call tool: {}", e))?;

        Ok(result)
    }

    /// Get peer (server) information.
    pub async fn peer_info(&self) -> Result<String> {
        let service = self.service.lock().await;
        let service = service
            .as_ref()
            .context("Not connected to MCP server")?;

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
