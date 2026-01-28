//! kaish-mcp: MCP server binary for kaish.
//!
//! This binary runs kaish as an MCP server over stdio transport.
//!
//! # Usage
//!
//! ```bash
//! # Run directly
//! kaish-mcp
//!
//! # Configure in Claude Code's .mcp.json:
//! # {
//! #   "mcpServers": {
//! #     "kaish": {
//! #       "command": "kaish-mcp"
//! #     }
//! #   }
//! # }
//! ```

use anyhow::{Context, Result};
use rmcp::service::ServiceExt;
use rmcp::transport::io::stdio;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

use kaish_mcp::server::{KaishServerHandler, McpServerConfig};

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing (logs to stderr)
    tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::io::stderr))
        .with(EnvFilter::from_default_env().add_directive("kaish_mcp=info".parse()?))
        .init();

    tracing::info!("Starting kaish MCP server");

    // Load configuration
    let config = McpServerConfig::load().context("Failed to load configuration")?;

    tracing::info!(
        "Server config: name={}, version={}, external_mcps={}",
        config.name,
        config.version,
        config.mcp_servers.len()
    );

    // Create handler
    let handler = KaishServerHandler::new(config).context("Failed to create server handler")?;

    // Create stdio transport and run the server
    tracing::info!("Serving on stdio");

    let service = handler
        .serve(stdio())
        .await
        .context("Failed to start MCP service")?;

    // Wait for the service to complete
    service.waiting().await?;

    tracing::info!("Server shutdown complete");
    Ok(())
}
