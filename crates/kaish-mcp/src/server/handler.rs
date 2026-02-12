//! MCP server handler implementation.
//!
//! Implements the rmcp::ServerHandler trait to expose kaish as an MCP server.

use std::path::PathBuf;
use std::sync::Arc;

use rmcp::handler::server::router::tool::ToolRouter;
use rmcp::handler::server::wrapper::Parameters;
use rmcp::model::{
    Annotated, CallToolResult, Content, Implementation, ListResourceTemplatesResult,
    ListResourcesResult, PaginatedRequestParams, ProtocolVersion, RawResource,
    RawResourceTemplate, ReadResourceRequestParams, ReadResourceResult, ResourceContents,
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
    #[tool(description = "Execute kaish shell scripts. Fresh isolated environment per call.\n\nSupports: pipes, redirects, here-docs, if/for/while, functions, builtins (grep, jq, git, find, sed, awk, cat, ls, etc.), ${VAR:-default}, $((arithmetic)), scatter/gather parallelism.\n\nNOT supported: process substitution <(), backticks, eval, aliases, implicit word splitting.\n\nPaths: Native paths work within $HOME (e.g., /home/user/src/project). /scratch/ = ephemeral memory. Use 'help' tool for details.")]
    async fn execute(&self, input: Parameters<ExecuteInput>) -> Result<CallToolResult, McpError> {
        tracing::info!(
            script_len = input.0.script.len(),
            cwd = ?input.0.cwd,
            "mcp.execute"
        );

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

        // Content blocks: plain text for human/LLM consumption
        let mut content = Vec::new();
        content.push(Content::text(&result.stdout));
        if !result.stderr.is_empty() {
            content.push(Content::text(format!("[stderr] {}", result.stderr)));
        }

        // Only include structured metadata when there's something beyond stdout
        // (errors, stderr, non-zero exit). Clean success → just the text.
        let structured_content = if !result.ok || !result.stderr.is_empty() {
            let structured = serde_json::to_value(&result)
                .map_err(|e| McpError::internal_error(e.to_string(), None))?;
            Some(structured)
        } else {
            None
        };

        Ok(CallToolResult {
            content,
            structured_content,
            is_error: Some(!result.ok),
            meta: None,
        })
    }

    /// Get help for kaish syntax, builtins, VFS, and capabilities.
    #[tool(
        description = "Get help for kaish syntax, builtins, VFS, and capabilities. Call without topic for overview."
    )]
    async fn help(&self, input: Parameters<HelpInput>) -> Result<CallToolResult, McpError> {
        let topic_str = input.0.topic.as_deref().unwrap_or("overview");
        let topic = HelpTopic::parse_topic(topic_str);

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
            protocol_version: ProtocolVersion::LATEST,
            capabilities: ServerCapabilities::builder()
                .enable_tools()
                .enable_resources()
                .build(),
            server_info: Implementation::from_build_env(),
            instructions: Some(
                "kaish (会sh) — Predictable shell for MCP tool orchestration.\n\n\
                 Bourne-like syntax without the gotchas (no word splitting, no glob expansion, \
                 no backticks). Strict validation catches errors before execution. \
                 Builtins run in-process; external commands work via PATH fallback \
                 (just type `cargo build`, `git status`, etc.).\n\n\
                 Tools:\n\
                 • execute — Run shell scripts (pipes, redirects, builtins, loops, functions)\n\
                 • help — Discover syntax, builtins, VFS mounts, capabilities\n\n\
                 Resources available via `kaish://vfs/{path}` URIs.\n\n\
                 Use 'help' first to learn what's available."
                    .to_string(),
            ),
        }
    }

    async fn list_resource_templates(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListResourceTemplatesResult, McpError> {
        Ok(ListResourceTemplatesResult {
            resource_templates: vec![Annotated {
                raw: RawResourceTemplate {
                    uri_template: "kaish://vfs/{+path}".to_string(),
                    name: "VFS File".to_string(),
                    title: Some("Virtual Filesystem".to_string()),
                    description: Some(
                        "Access files and directories through kaish's VFS. \
                         Paths mirror the native filesystem under $HOME."
                            .to_string(),
                    ),
                    mime_type: None,
                    icons: None,
                },
                annotations: None,
            }],
            next_cursor: None,
            meta: None,
        })
    }

    async fn list_resources(
        &self,
        _request: Option<PaginatedRequestParams>,
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
        request: ReadResourceRequestParams,
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

    #[tokio::test]
    async fn test_execute_output_format() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");

        let input = Parameters(ExecuteInput {
            script: "echo hello".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        });
        let result = handler.execute(input).await.expect("execute failed");

        // content[0] should be plain text (echo is simple text, not TOON-encoded)
        if let RawContent::Text(text) = &result.content[0].raw {
            assert_eq!(text.text.trim(), "hello");
            assert!(
                !text.text.contains(r#""code""#),
                "content should be plain text, not JSON metadata"
            );
        } else {
            panic!("Expected text content");
        }

        // Clean success → no structured_content (just text content blocks)
        assert!(result.structured_content.is_none(), "success should not have structured_content");

        // is_error should be false for success
        assert_eq!(result.is_error, Some(false));
    }

    #[tokio::test]
    async fn test_execute_error_format() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");

        let input = Parameters(ExecuteInput {
            script: "nonexistent_command_xyz".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        });
        let result = handler.execute(input).await.expect("execute failed");

        // is_error should be true
        assert_eq!(result.is_error, Some(true));

        // structured_content should have error details
        let structured = result.structured_content.expect("should have structured_content");
        assert_eq!(structured["ok"], false);
        assert_eq!(structured["code"], 127);
    }

    #[tokio::test]
    async fn test_execute_stderr_content() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");

        // A command that fails produces stderr
        let input = Parameters(ExecuteInput {
            script: "nonexistent_command_xyz".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        });
        let result = handler.execute(input).await.expect("execute failed");

        // Should have a stderr content block with [stderr] prefix
        let stderr_block = result.content.iter().find(|c| {
            if let RawContent::Text(t) = &c.raw {
                t.text.starts_with("[stderr]")
            } else {
                false
            }
        });
        assert!(stderr_block.is_some(), "should have a [stderr] content block");
    }
}
