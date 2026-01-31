//! MCP server handler implementation.
//!
//! Implements the rmcp::ServerHandler trait to expose kaish as an MCP server.

use std::path::PathBuf;
use std::sync::Arc;

use rmcp::handler::server::router::tool::ToolRouter;
use rmcp::handler::server::wrapper::Parameters;
use rmcp::model::{
    Annotated, CallToolResult, Content, Implementation, ListResourcesResult, PaginatedRequestParam,
    ProtocolVersion, RawResource, ReadResourceRequestParam, ReadResourceResult, ResourceContents,
    ServerCapabilities, ServerInfo,
};
use rmcp::schemars::{self, JsonSchema};
use rmcp::service::{RequestContext, RoleServer};
use rmcp::ErrorData as McpError;
use rmcp::{tool, tool_handler, tool_router};
use serde::{Deserialize, Serialize};

use kaish_kernel::help::{get_help, HelpTopic};
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
        // Create a VFS for resource access in sandboxed mode.
        // Paths appear native (e.g., /home/user/...) but access is restricted.
        let mut vfs = VfsRouter::new();

        // Mount memory at root for safety (catches paths outside sandbox)
        vfs.mount("/", MemoryFs::new());
        vfs.mount("/v", MemoryFs::new());
        vfs.mount("/scratch", MemoryFs::new());

        // Real /tmp for interop with other processes
        vfs.mount("/tmp", LocalFs::new(PathBuf::from("/tmp")));

        // Mount local filesystem at its real path for transparent access.
        // If HOME is not set, use a safe temp directory.
        let local_root = std::env::var("HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| {
                tracing::warn!("HOME not set, mounting temp directory");
                std::env::temp_dir()
            });
        let mount_point = local_root.to_string_lossy().to_string();
        vfs.mount(&mount_point, LocalFs::new(local_root));

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

    /// Initial working directory (default: $HOME).
    #[schemars(description = "Initial working directory (default: $HOME)")]
    pub cwd: Option<String>,

    /// Environment variables to set.
    #[schemars(description = "Environment variables to set")]
    pub env: Option<std::collections::HashMap<String, String>>,

    /// Timeout in milliseconds (default: 30000).
    #[schemars(description = "Timeout in milliseconds (default: 30000)")]
    pub timeout_ms: Option<u64>,
}

/// Help tool input schema.
#[derive(Debug, Clone, Serialize, Deserialize, JsonSchema)]
pub struct HelpInput {
    /// Help topic or tool name.
    #[schemars(
        description = "Topic: 'overview', 'syntax', 'builtins', 'vfs', 'scatter', 'limits', or a tool name"
    )]
    pub topic: Option<String>,
}

#[tool_router]
impl KaishServerHandler {
    /// Execute kaish shell scripts.
    ///
    /// Each call runs in a fresh, isolated environment. Supports restricted/modified
    /// Bourne syntax plus kaish extensions (scatter/gather, typed params, MCP tool calls).
    #[tool(description = "Execute kaish shell scripts. Fresh isolated environment per call.\n\nSupports: pipes, redirects, here-docs, if/for/while, functions, 54 builtins (grep, jq, git, find, sed, awk, cat, ls, etc.), ${VAR:-default}, $((arithmetic)), scatter/gather parallelism.\n\nNOT supported: process substitution <(), backticks, eval, aliases, implicit word splitting.\n\nPaths: Native paths work within $HOME (e.g., /home/user/src/project). /scratch/ = ephemeral memory. Use 'help' tool for details.")]
    async fn execute(&self, input: Parameters<ExecuteInput>) -> Result<CallToolResult, McpError> {
        let params = ExecuteParams {
            script: input.0.script,
            cwd: input.0.cwd,
            env: input.0.env,
            timeout_ms: input.0.timeout_ms,
        };

        let result =
            execute::execute(params, &self.config.mcp_servers, self.config.default_timeout_ms)
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

    /// Get help for kaish syntax, builtins, VFS, and capabilities.
    #[tool(
        description = "Get help for kaish syntax, builtins, VFS, and capabilities. Call without topic for overview."
    )]
    async fn help(&self, input: Parameters<HelpInput>) -> Result<CallToolResult, McpError> {
        let topic_str = input.0.topic.as_deref().unwrap_or("overview");
        let topic = HelpTopic::from_str(topic_str);

        // For builtins topic, we need tool schemas from a temporary kernel
        let content = if matches!(topic, HelpTopic::Builtins) || matches!(topic, HelpTopic::Tool(_))
        {
            // Create a temporary kernel to get tool schemas (isolated, no local fs needed)
            let config = kaish_kernel::KernelConfig::isolated()
                .with_skip_validation(true);
            let kernel = kaish_kernel::Kernel::new(config)
                .map_err(|e| McpError::internal_error(e.to_string(), None))?;
            let schemas = kernel.tool_schemas();
            get_help(&topic, &schemas)
        } else {
            get_help(&topic, &[])
        };

        Ok(CallToolResult::success(vec![Content::text(content)]))
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
                "kaish (会sh) — Predictable shell for MCP tool orchestration.\n\n\
                 Bourne-like syntax without the gotchas (no word splitting, no glob expansion, \
                 no backticks). Strict validation catches errors before execution. \
                 54 builtins run in-process; use `exec` for external commands.\n\n\
                 Tools:\n\
                 • execute — Run shell scripts (pipes, redirects, 54 builtins, loops, functions)\n\
                 • help — Discover syntax, builtins, VFS mounts, capabilities\n\n\
                 Use 'help' first to learn what's available."
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
    use rmcp::model::RawContent;

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
        let instructions = info.instructions.unwrap();
        assert!(instructions.contains("execute"));
        assert!(instructions.contains("help"));
    }

    #[tokio::test]
    async fn test_help_overview() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");

        let input = Parameters(HelpInput { topic: None });
        let result = handler.help(input).await.expect("help failed");

        assert!(!result.is_error.unwrap_or(false));
        assert!(!result.content.is_empty());

        // Check that the overview content is returned
        if let RawContent::Text(text) = &result.content[0].raw {
            assert!(text.text.contains("kaish"));
            assert!(text.text.contains("help syntax"));
        } else {
            panic!("Expected text content");
        }
    }

    #[tokio::test]
    async fn test_help_syntax() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");

        let input = Parameters(HelpInput {
            topic: Some("syntax".to_string()),
        });
        let result = handler.help(input).await.expect("help failed");

        assert!(!result.is_error.unwrap_or(false));
        if let RawContent::Text(text) = &result.content[0].raw {
            assert!(text.text.contains("Variables"));
        }
    }

    #[tokio::test]
    async fn test_help_builtins() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");

        let input = Parameters(HelpInput {
            topic: Some("builtins".to_string()),
        });
        let result = handler.help(input).await.expect("help failed");

        assert!(!result.is_error.unwrap_or(false));
        if let RawContent::Text(text) = &result.content[0].raw {
            // Should list actual builtins from the kernel
            assert!(text.text.contains("echo"));
            assert!(text.text.contains("grep"));
        }
    }
}
