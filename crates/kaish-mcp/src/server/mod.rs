//! MCP server functionality for kaish.
//!
//! This module exposes kaish as an MCP server, allowing MCP clients
//! like Claude Code to execute kaish scripts.
//!
//! # Features
//!
//! - **execute tool**: Run kaish scripts in a fresh, isolated kernel
//! - **VFS resources**: Access filesystem via MCP resource protocol
//! - **MCP chaining**: Configure external MCP servers for tool integration
//!
//! # Example
//!
//! ```ignore
//! use kaish_mcp::server::{KaishServerHandler, McpServerConfig};
//! use rmcp::transport::io::stdio;
//! use rmcp::service::ServiceExt;
//!
//! #[tokio::main]
//! async fn main() -> anyhow::Result<()> {
//!     let config = McpServerConfig::load()?;
//!     let handler = KaishServerHandler::new(config)?;
//!
//!     let transport = stdio();
//!     handler.serve(transport).await?;
//!     Ok(())
//! }
//! ```

pub mod config;
pub mod execute;
pub mod handler;
pub mod resources;

pub use config::McpServerConfig;
pub use handler::KaishServerHandler;
