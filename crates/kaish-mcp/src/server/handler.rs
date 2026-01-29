//! MCP server handler implementation.
//!
//! Implements the rmcp::ServerHandler trait to expose kaish as an MCP server.

use std::path::PathBuf;
use std::sync::Arc;

use rmcp::handler::server::wrapper::Parameters;
use rmcp::ErrorData as McpError;
use rmcp::handler::server::router::tool::ToolRouter;
use rmcp::model::{
    Annotated, CallToolResult, Content, Implementation, ListResourcesResult, PaginatedRequestParam,
    ProtocolVersion, RawResource, ReadResourceRequestParam, ReadResourceResult, ResourceContents,
    ServerCapabilities, ServerInfo,
};
use rmcp::schemars::{self, JsonSchema};
use rmcp::service::{RequestContext, RoleServer};
use rmcp::{tool, tool_handler, tool_router};
use serde::{Deserialize, Serialize};

use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};

use super::config::McpServerConfig;
use super::execute::{self, ExecuteParams};
use super::resources::{self, parse_resource_uri, ResourceContent};

/// The kaish MCP server handler.
#[derive(Clone)]
pub struct KaishServerHandler {
    /// Server configuration.
    config: McpServerConfig,
    /// VFS router for resource access.
    vfs: Arc<VfsRouter>,
    /// Tool router.
    tool_router: ToolRouter<Self>,
}

impl KaishServerHandler {
    /// Create a new handler with the given configuration.
    pub fn new(config: McpServerConfig) -> anyhow::Result<Self> {
        // Create a VFS for resource access
        let mut vfs = VfsRouter::new();

        // Mount memory at root
        vfs.mount("/", MemoryFs::new());
        vfs.mount("/tmp", MemoryFs::new());

        // Mount local filesystem at user's home directory.
        // If HOME is not set, use a safe temp directory instead of exposing root.
        let local_root = std::env::var("HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| {
                tracing::warn!("HOME not set, mounting temp directory for /mnt/local");
                std::env::temp_dir()
            });
        vfs.mount("/mnt/local", LocalFs::new(local_root));

        Ok(Self {
            config,
            vfs: Arc::new(vfs),
            tool_router: Self::tool_router(),
        })
    }
}

/// Execute tool input schema.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct ExecuteInput {
    /// Kaish script or command to execute.
    #[schemars(description = "Kaish script or command to execute")]
    pub script: String,

    /// Initial working directory (VFS path, default: /mnt/local).
    #[schemars(description = "Initial working directory (VFS path, default: /mnt/local)")]
    pub cwd: Option<String>,

    /// Environment variables to set.
    #[schemars(description = "Environment variables to set")]
    pub env: Option<std::collections::HashMap<String, String>>,

    /// Timeout in milliseconds (default: 30000).
    #[schemars(description = "Timeout in milliseconds (default: 30000)")]
    pub timeout_ms: Option<u64>,
}

#[tool_router]
impl KaishServerHandler {
    /// Execute kaish shell scripts.
    ///
    /// Each call runs in a fresh, isolated environment. Supports Bourne-compatible
    /// syntax plus kaish extensions (scatter/gather, typed params, MCP tool calls).
    #[tool(description = "Execute kaish shell scripts. Each call runs in a fresh, isolated environment. Supports Bourne-compatible syntax plus kaish extensions (scatter/gather, typed params, MCP tool calls).")]
    async fn execute(&self, input: Parameters<ExecuteInput>) -> Result<CallToolResult, McpError> {
        let params = ExecuteParams {
            script: input.0.script,
            cwd: input.0.cwd,
            env: input.0.env,
            timeout_ms: input.0.timeout_ms,
        };

        let result = execute::execute(params, &self.config.mcp_servers, self.config.default_timeout_ms)
            .await
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        // Format the result as JSON for structured output
        let result_json = serde_json::to_string_pretty(&result)
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        if result.ok {
            Ok(CallToolResult::success(vec![Content::text(result_json)]))
        } else {
            // Still return the result, but mark as error
            Ok(CallToolResult {
                content: vec![Content::text(result_json)],
                is_error: Some(true),
                structured_content: None,
                meta: None,
            })
        }
    }
}

#[tool_handler]
impl rmcp::ServerHandler for KaishServerHandler {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            protocol_version: ProtocolVersion::V_2024_11_05,
            capabilities: ServerCapabilities::builder()
                .enable_tools()
                .enable_resources()
                .build(),
            server_info: Implementation::from_build_env(),
            instructions: Some(
                "kaish is an 80/20 shell implementation for MCP tool orchestration. \
                 Use the execute tool to run shell scripts with Bourne-compatible syntax."
                    .to_string(),
            ),
        }
    }

    async fn list_resources(
        &self,
        _request: Option<PaginatedRequestParam>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListResourcesResult, McpError> {
        // List root VFS resources
        let resources = resources::list_resources(&self.vfs, std::path::Path::new("/"))
            .await
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        let mcp_resources: Vec<Annotated<RawResource>> = resources
            .into_iter()
            .map(|r| {
                Annotated {
                    raw: RawResource {
                        uri: r.uri,
                        name: r.name,
                        title: None,
                        description: r.description,
                        mime_type: r.mime_type,
                        size: None,
                        icons: None,
                        meta: None,
                    },
                    annotations: None,
                }
            })
            .collect();

        Ok(ListResourcesResult {
            resources: mcp_resources,
            next_cursor: None,
            meta: None,
        })
    }

    async fn read_resource(
        &self,
        request: ReadResourceRequestParam,
        _context: RequestContext<RoleServer>,
    ) -> Result<ReadResourceResult, McpError> {
        let uri = request.uri.as_str();

        let path = parse_resource_uri(uri).ok_or_else(|| {
            McpError::invalid_request(format!("Invalid resource URI: {}", uri), None)
        })?;

        let content = resources::read_resource(&self.vfs, &path)
            .await
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        let contents = match content {
            ResourceContent::Text { text, mime_type } => {
                vec![ResourceContents::TextResourceContents {
                    uri: uri.to_string(),
                    mime_type: Some(mime_type),
                    text,
                    meta: None,
                }]
            }
            ResourceContent::Blob { data, mime_type } => {
                use base64::Engine;
                let encoded = base64::engine::general_purpose::STANDARD.encode(&data);
                vec![ResourceContents::BlobResourceContents {
                    uri: uri.to_string(),
                    mime_type: Some(mime_type),
                    blob: encoded,
                    meta: None,
                }]
            }
        };

        Ok(ReadResourceResult { contents })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_handler_creation() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");
        assert_eq!(handler.config.name, "kaish");
    }

    #[test]
    fn test_get_info() {
        use rmcp::ServerHandler;

        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");
        let info = handler.get_info();
        assert!(info.instructions.is_some());
    }
}
