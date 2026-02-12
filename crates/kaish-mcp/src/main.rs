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
use opentelemetry::trace::TracerProvider;
use opentelemetry::KeyValue;
use opentelemetry_sdk::Resource;
use rmcp::service::ServiceExt;
use rmcp::transport::io::stdio;
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

use kaish_mcp::server::{KaishServerHandler, McpServerConfig};

#[tokio::main]
async fn main() -> Result<()> {
    // If OTEL_EXPORTER_OTLP_ENDPOINT is set, export spans via OTLP.
    // Otherwise, just use the fmt layer (no-op OTel).
    let provider = if std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT").is_ok() {
        let exporter = opentelemetry_otlp::SpanExporter::builder()
            .with_tonic()
            .build()
            .context("Failed to build OTLP exporter")?;
        let resource = Resource::builder()
            .with_attributes([
                KeyValue::new("service.name", "kaish-mcp"),
                KeyValue::new("service.version", env!("CARGO_PKG_VERSION")),
            ])
            .build();
        let provider = opentelemetry_sdk::trace::SdkTracerProvider::builder()
            .with_resource(resource)
            .with_batch_exporter(exporter)
            .build();
        opentelemetry::global::set_tracer_provider(provider.clone());
        Some(provider)
    } else {
        None
    };

    let otel_layer = provider
        .as_ref()
        .map(|p| tracing_opentelemetry::layer().with_tracer(p.tracer("kaish-mcp")));

    tracing_subscriber::registry()
        .with(fmt::layer().with_writer(std::io::stderr))
        .with(otel_layer)
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

    if let Some(provider) = provider {
        // Explicitly ignored: shutdown errors are non-fatal at process exit
        let _ = provider.shutdown();
    }

    Ok(())
}
