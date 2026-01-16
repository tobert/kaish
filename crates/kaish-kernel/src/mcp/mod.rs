//! MCP (Model Context Protocol) client integration.
//!
//! This module provides integration with MCP servers, allowing kaish to
//! discover and call tools exposed by external MCP servers.
//!
//! # Architecture
//!
//! ```text
//! kaish ToolRegistry
//!     └── McpToolWrapper (implements Tool trait)
//!             └── McpClient (wraps rmcp Service)
//!                     └── MCP Server (stdio child process)
//! ```
//!
//! # Usage
//!
//! ```ignore
//! use kaish_kernel::mcp::{McpClient, McpConfig, McpTransport};
//! use kaish_kernel::tools::ToolRegistry;
//!
//! // Create and connect to an MCP server
//! let client = McpClient::new(McpConfig {
//!     name: "git".to_string(),
//!     transport: McpTransport::Stdio {
//!         command: "uvx".to_string(),
//!         args: vec!["mcp-server-git".to_string()],
//!         env: vec![],
//!     },
//! });
//! client.connect().await?;
//!
//! // Register MCP tools in the registry
//! let client = Arc::new(client);
//! register_mcp_tools(&client, &mut registry).await?;
//! ```

mod client;
mod tool;

use std::sync::Arc;

use anyhow::Result;

use crate::tools::ToolRegistry;

pub use client::{McpClient, McpConfig, McpTransport, shared_client, stdio_client};
pub use tool::McpToolWrapper;

/// Register all tools from an MCP client into a ToolRegistry.
///
/// Each tool is registered with a prefixed name: `{client_name}:{tool_name}`.
pub async fn register_mcp_tools(
    client: &Arc<McpClient>,
    registry: &mut ToolRegistry,
) -> Result<usize> {
    let tools = client.list_tools().await?;
    let count = tools.len();

    for mcp_tool in tools {
        let wrapper = McpToolWrapper::new(Arc::clone(client), mcp_tool);
        registry.register(wrapper);
    }

    Ok(count)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stdio_client_creation() {
        let client = stdio_client("test-server", "echo", vec!["hello".to_string()]);
        assert_eq!(client.name(), "test-server");
    }

    #[test]
    fn test_mcp_config() {
        let config = McpConfig {
            name: "my-server".to_string(),
            transport: McpTransport::Stdio {
                command: "python".to_string(),
                args: vec!["-m".to_string(), "mcp_server".to_string()],
                env: vec![("FOO".to_string(), "BAR".to_string())],
            },
        };

        assert_eq!(config.name, "my-server");
        match &config.transport {
            McpTransport::Stdio { command, args, env } => {
                assert_eq!(command, "python");
                assert_eq!(args.len(), 2);
                assert_eq!(env.len(), 1);
            }
        }
    }

    #[tokio::test]
    async fn test_client_not_connected() {
        let client = stdio_client("test", "nonexistent", vec![]);

        // Should fail because not connected
        let result = client.list_tools().await;
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Not connected"));
    }

    #[tokio::test]
    async fn test_client_is_connected_false_initially() {
        let client = stdio_client("test", "echo", vec![]);
        assert!(!client.is_connected().await);
    }
}
