//! MCP server handler implementation.
//!
//! Implements the rmcp::ServerHandler trait to expose kaish as an MCP server.
//! Manual impl (no `#[tool_handler]`) for full control over progress
//! notifications, prompt routing, and resource subscriptions.

use std::path::PathBuf;
use std::sync::{Arc, OnceLock};

use anyhow::Context as _;

use rmcp::handler::server::router::prompt::PromptRouter;
use rmcp::handler::server::router::tool::ToolRouter;
use rmcp::handler::server::tool::ToolCallContext;
use rmcp::handler::server::wrapper::Parameters;
use rmcp::model::{
    Annotated, CallToolRequestParams, CallToolResult, Content, GetPromptRequestParams,
    GetPromptResult, Implementation, ListPromptsResult, ListResourceTemplatesResult,
    ListResourcesResult, ListToolsResult, LoggingLevel, LoggingMessageNotificationParam,
    PaginatedRequestParams, ProgressNotificationParam, ProtocolVersion, RawResource,
    RawResourceTemplate, ReadResourceRequestParams, ReadResourceResult, ResourceContents,
    ServerCapabilities, ServerInfo, SetLevelRequestParams, SubscribeRequestParams,
    UnsubscribeRequestParams,
};
use rmcp::schemars::{self, JsonSchema};
use rmcp::service::{NotificationContext, Peer, RequestContext, RoleServer};
use rmcp::ErrorData as McpError;
use rmcp::model::{PromptMessage, PromptMessageRole};
use rmcp::{prompt, prompt_router, tool, tool_router};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tokio::sync::RwLock;

use kaish_kernel::help::{get_help, HelpTopic};
use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};

use super::config::McpServerConfig;
use super::execute::{self, ExecuteParams};
use super::resources::{self, parse_resource_uri, ResourceContent};
use super::subscriptions::ResourceWatcher;

/// The kaish MCP server handler.
#[derive(Clone)]
pub struct KaishServerHandler {
    /// Server configuration.
    config: McpServerConfig,
    /// VFS router for resource access. RwLock allows roots to mount dynamically.
    vfs: Arc<RwLock<VfsRouter>>,
    /// Tool router.
    tool_router: ToolRouter<Self>,
    /// Prompt router.
    prompt_router: PromptRouter<Self>,
    /// Resource watcher (subscription tracking + file watching).
    watcher: Arc<ResourceWatcher>,
    /// MCP peer handle, shared with ResourceWatcher.
    peer: Arc<OnceLock<Peer<RoleServer>>>,
    /// Minimum logging level requested by client. None = logging not requested.
    log_level: Arc<RwLock<Option<LoggingLevel>>>,
}

/// Map LoggingLevel to a numeric severity for comparison.
/// Debug=0 (least severe) through Emergency=7 (most severe).
fn severity(level: LoggingLevel) -> u8 {
    match level {
        LoggingLevel::Debug => 0,
        LoggingLevel::Info => 1,
        LoggingLevel::Notice => 2,
        LoggingLevel::Warning => 3,
        LoggingLevel::Error => 4,
        LoggingLevel::Critical => 5,
        LoggingLevel::Alert => 6,
        LoggingLevel::Emergency => 7,
    }
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

        // Per-handler /tmp — isolated but on-disk for interop with external commands.
        // Uses PID for uniqueness (one process = one session in stdio mode).
        let tmp_dir = std::env::temp_dir().join(format!("kaish-{}", std::process::id()));
        std::fs::create_dir_all(&tmp_dir)
            .context("Failed to create per-handler /tmp directory")?;
        vfs.mount("/tmp", LocalFs::new(tmp_dir));

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

        let peer = Arc::new(OnceLock::new());

        Ok(Self {
            config,
            vfs: Arc::new(RwLock::new(vfs)),
            tool_router: Self::tool_router(),
            prompt_router: Self::prompt_router(),
            watcher: ResourceWatcher::new(peer.clone()),
            peer,
            log_level: Arc::new(RwLock::new(None)),
        })
    }

    /// Capture the peer handle on first contact. Idempotent.
    fn ensure_peer(&self, peer: &Peer<RoleServer>) {
        // OnceLock::set returns Err if already set — that's fine, same peer.
        let _ = self.peer.set(peer.clone());
    }

    /// Send a structured log message to the client if logging is enabled
    /// and the message meets the minimum severity threshold.
    async fn log_to_client(&self, level: LoggingLevel, logger: &str, data: impl Into<Value>) {
        let min_level = *self.log_level.read().await;
        if min_level.is_none_or(|min| severity(level) < severity(min)) {
            return;
        }
        if let Some(peer) = self.peer.get() {
            // Explicitly ignored: logging notifications are best-effort
            let _ = peer
                .notify_logging_message(LoggingMessageNotificationParam {
                    level,
                    logger: Some(logger.to_string()),
                    data: data.into(),
                })
                .await;
        }
    }

    /// Fetch client roots and mount them in the VFS.
    async fn refresh_roots(&self) {
        let Some(peer) = self.peer.get() else { return };
        let roots = match peer.list_roots().await {
            Ok(result) => result.roots,
            Err(e) => {
                tracing::warn!(error = %e, "Failed to list roots");
                return;
            }
        };

        let mut vfs = self.vfs.write().await;
        for root in &roots {
            if let Some(path_str) = root.uri.strip_prefix("file://") {
                let path = PathBuf::from(path_str);
                let mount_point = path.to_string_lossy().to_string();
                // Only mount if not already mounted (don't clobber existing mounts)
                if !vfs.list_mounts().iter().any(|m| m.path == path) && path.exists() {
                    tracing::info!(path = %path.display(), name = ?root.name, "Mounting client root");
                    vfs.mount(&mount_point, LocalFs::new(path));
                }
            }
        }
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

#[tool_router]
impl KaishServerHandler {
    /// Execute kaish shell scripts.
    ///
    /// Each call runs in a fresh, isolated environment. Supports restricted/modified
    /// Bourne syntax plus kaish extensions (scatter/gather, typed params, MCP tool calls).
    #[tool(description = "Execute kaish shell scripts. Fresh isolated environment per call.\n\nSupports: pipes, redirects, here-docs, if/for/while, functions, builtins (grep, jq, git, find, sed, awk, cat, ls, etc.), ${VAR:-default}, $((arithmetic)), scatter/gather parallelism.\n\nNOT supported: process substitution <(), backticks, eval, implicit word splitting.\n\nPaths: Native paths work within $HOME (e.g., /home/user/src/project). /scratch/ = ephemeral memory. Use 'help' tool for details.")]
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

        let structured_content = {
            let structured = serde_json::to_value(&result)
                .map_err(|e| McpError::internal_error(e.to_string(), None))?;
            Some(structured)
        };

        Ok(CallToolResult {
            content,
            structured_content,
            is_error: Some(!result.ok),
            meta: None,
        })
    }
}

#[prompt_router(vis = "pub(crate)")]
impl KaishServerHandler {
    #[prompt(
        name = "kaish-overview",
        description = "What kaish is, topic list, quick examples"
    )]
    pub(crate) async fn prompt_overview(&self) -> Result<GetPromptResult, McpError> {
        let content = get_help(&HelpTopic::Overview, &[]);
        Ok(GetPromptResult {
            description: Some("kaish overview and quick reference".to_string()),
            messages: vec![PromptMessage::new_text(PromptMessageRole::User, content)],
        })
    }

    #[prompt(
        name = "kaish-syntax",
        description = "Variables, quoting, pipes, control flow reference"
    )]
    pub(crate) async fn prompt_syntax(&self) -> Result<GetPromptResult, McpError> {
        let content = get_help(&HelpTopic::Syntax, &[]);
        Ok(GetPromptResult {
            description: Some("kaish language syntax reference".to_string()),
            messages: vec![PromptMessage::new_text(PromptMessageRole::User, content)],
        })
    }

    #[prompt(
        name = "kaish-builtins",
        description = "List of all available builtins with descriptions"
    )]
    pub(crate) async fn prompt_builtins(
        &self,
        Parameters(params): Parameters<super::prompts::BuiltinsParams>,
    ) -> Result<GetPromptResult, McpError> {
        let (topic, description) = if let Some(tool_name) = params.tool {
            (
                HelpTopic::Tool(tool_name.clone()),
                format!("Help for builtin: {}", tool_name),
            )
        } else {
            (
                HelpTopic::Builtins,
                "All available kaish builtins".to_string(),
            )
        };

        // Create a temporary kernel to get tool schemas
        let config = kaish_kernel::KernelConfig::isolated().with_skip_validation(true);
        let kernel = kaish_kernel::Kernel::new(config)
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;
        let schemas = kernel.tool_schemas();
        let content = get_help(&topic, &schemas);

        Ok(GetPromptResult {
            description: Some(description),
            messages: vec![PromptMessage::new_text(PromptMessageRole::User, content)],
        })
    }

    #[prompt(
        name = "kaish-vfs",
        description = "Virtual filesystem mounts, paths, backends"
    )]
    pub(crate) async fn prompt_vfs(&self) -> Result<GetPromptResult, McpError> {
        let content = get_help(&HelpTopic::Vfs, &[]);
        Ok(GetPromptResult {
            description: Some("kaish VFS (virtual filesystem) reference".to_string()),
            messages: vec![PromptMessage::new_text(PromptMessageRole::User, content)],
        })
    }

    #[prompt(
        name = "kaish-scatter",
        description = "Parallel processing with scatter/gather (散/集)"
    )]
    pub(crate) async fn prompt_scatter(&self) -> Result<GetPromptResult, McpError> {
        let content = get_help(&HelpTopic::Scatter, &[]);
        Ok(GetPromptResult {
            description: Some("Scatter/gather parallel processing reference".to_string()),
            messages: vec![PromptMessage::new_text(PromptMessageRole::User, content)],
        })
    }

    #[prompt(
        name = "kaish-limits",
        description = "Known limitations and workarounds"
    )]
    pub(crate) async fn prompt_limits(&self) -> Result<GetPromptResult, McpError> {
        let content = get_help(&HelpTopic::Limits, &[]);
        Ok(GetPromptResult {
            description: Some("Known limitations and workarounds".to_string()),
            messages: vec![PromptMessage::new_text(PromptMessageRole::User, content)],
        })
    }
}

// Manual ServerHandler impl — replaces #[tool_handler] for full control
// over progress notifications, prompts, subscriptions, logging, and roots.
impl rmcp::ServerHandler for KaishServerHandler {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            protocol_version: ProtocolVersion::LATEST,
            capabilities: ServerCapabilities::builder()
                .enable_tools()
                .enable_resources()
                .enable_resources_subscribe()
                .enable_prompts()
                .enable_logging()
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
                 Use 'help' tool for details."
                    .to_string(),
            ),
        }
    }

    // -- Lifecycle --

    fn on_initialized(
        &self,
        context: NotificationContext<RoleServer>,
    ) -> impl std::future::Future<Output = ()> + Send + '_ {
        self.ensure_peer(&context.peer);
        // Spawn as a background task — awaiting peer requests inline in a
        // notification handler deadlocks the server's event loop because
        // the loop can't process the response while it's blocked here.
        let this = self.clone();
        tokio::spawn(async move {
            this.refresh_roots().await;
        });
        async {}
    }

    fn on_roots_list_changed(
        &self,
        context: NotificationContext<RoleServer>,
    ) -> impl std::future::Future<Output = ()> + Send + '_ {
        self.ensure_peer(&context.peer);
        let this = self.clone();
        tokio::spawn(async move {
            this.refresh_roots().await;
        });
        async {}
    }

    // -- Logging --

    fn set_level(
        &self,
        request: SetLevelRequestParams,
        context: RequestContext<RoleServer>,
    ) -> impl std::future::Future<Output = Result<(), McpError>> + Send + '_ {
        self.ensure_peer(&context.peer);
        async move {
            *self.log_level.write().await = Some(request.level);
            tracing::info!(level = ?request.level, "Client set logging level");
            Ok(())
        }
    }

    // -- Tools (manual dispatch with progress notifications) --

    async fn list_tools(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListToolsResult, McpError> {
        Ok(ListToolsResult {
            tools: self.tool_router.list_all(),
            meta: None,
            next_cursor: None,
        })
    }

    async fn call_tool(
        &self,
        request: CallToolRequestParams,
        context: RequestContext<RoleServer>,
    ) -> Result<CallToolResult, McpError> {
        self.ensure_peer(&context.peer);

        let tool_name = request.name.clone();

        // Extract progress token from request metadata
        use rmcp::model::RequestParamsMeta;
        let progress_token = request.progress_token();

        // Send "starting" progress notification
        if let Some(ref token) = progress_token {
            // Explicitly ignored: progress notifications are best-effort
            let _ = context
                .peer
                .notify_progress(ProgressNotificationParam {
                    progress_token: token.clone(),
                    progress: 0.0,
                    total: Some(1.0),
                    message: Some("Starting".to_string()),
                })
                .await;
        }

        self.log_to_client(
            LoggingLevel::Info,
            "kaish",
            Value::String(format!("Executing: {}", tool_name)),
        )
        .await;

        // Dispatch to tool router
        let tcc = ToolCallContext::new(self, request, context);
        let result = self.tool_router.call(tcc).await;

        // Post-call logging
        match &result {
            Ok(_) => {
                self.log_to_client(
                    LoggingLevel::Debug,
                    "kaish",
                    Value::String(format!("Complete: {}", tool_name)),
                )
                .await;
            }
            Err(e) => {
                self.log_to_client(
                    LoggingLevel::Warning,
                    "kaish",
                    Value::String(format!("Error in {}: {}", tool_name, e.message)),
                )
                .await;
            }
        }

        // Notify resource list changed — scripts may create/delete files
        if let Some(peer) = self.peer.get() {
            // Explicitly ignored: notification is best-effort
            let _ = peer.notify_resource_list_changed().await;
        }

        // Log progress completion
        if let Some(token) = progress_token {
            tracing::debug!(
                progress_token = ?token,
                "Tool call complete (progress token tracked)"
            );
        }

        result
    }

    fn get_tool(&self, name: &str) -> Option<rmcp::model::Tool> {
        self.tool_router.get(name).cloned()
    }

    // -- Prompts --

    async fn list_prompts(
        &self,
        _request: Option<PaginatedRequestParams>,
        _context: RequestContext<RoleServer>,
    ) -> Result<ListPromptsResult, McpError> {
        Ok(ListPromptsResult {
            prompts: self.prompt_router.list_all(),
            meta: None,
            next_cursor: None,
        })
    }

    async fn get_prompt(
        &self,
        request: GetPromptRequestParams,
        context: RequestContext<RoleServer>,
    ) -> Result<GetPromptResult, McpError> {
        let prompt_context = rmcp::handler::server::prompt::PromptContext::new(
            self,
            request.name,
            request.arguments,
            context,
        );
        self.prompt_router.get_prompt(prompt_context).await
    }

    // -- Resources --

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
        let vfs = self.vfs.read().await;
        let resources = resources::list_resources(&vfs, std::path::Path::new("/"))
            .await
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        let mcp_resources: Vec<Annotated<RawResource>> = resources
            .into_iter()
            .map(|r| Annotated {
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

        let vfs = self.vfs.read().await;
        let content = resources::read_resource(&vfs, &path)
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

    // -- Subscriptions --

    async fn subscribe(
        &self,
        request: SubscribeRequestParams,
        context: RequestContext<RoleServer>,
    ) -> Result<(), McpError> {
        tracing::info!(uri = %request.uri, "Resource subscription added");
        self.ensure_peer(&context.peer);

        let vfs = self.vfs.read().await;
        let real_path = parse_resource_uri(&request.uri)
            .and_then(|vfs_path| vfs.resolve_real_path(&vfs_path));

        self.watcher.subscribe(request.uri, real_path).await;
        Ok(())
    }

    async fn unsubscribe(
        &self,
        request: UnsubscribeRequestParams,
        _context: RequestContext<RoleServer>,
    ) -> Result<(), McpError> {
        tracing::info!(uri = %request.uri, "Resource subscription removed");
        self.watcher.unsubscribe(&request.uri).await;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rmcp::model::RawContent;

    #[tokio::test]
    async fn test_handler_creation() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");
        assert_eq!(handler.config.name, "kaish");
    }

    #[tokio::test]
    async fn test_get_info() {
        use rmcp::ServerHandler;

        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");
        let info = handler.get_info();
        assert!(info.instructions.is_some());
        let instructions = info.instructions.unwrap();
        assert!(instructions.contains("execute"));
    }

    #[tokio::test]
    async fn test_get_info_capabilities() {
        use rmcp::ServerHandler;

        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");
        let info = handler.get_info();

        // Verify all expected capabilities are enabled
        assert!(info.capabilities.tools.is_some());
        assert!(info.capabilities.resources.is_some());
        assert!(info.capabilities.prompts.is_some());
        assert!(info.capabilities.logging.is_some());

        // Subscribe is advertised (backed by notify file watcher)
        let resources = info.capabilities.resources.unwrap();
        assert!(resources.subscribe.unwrap_or(false));
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

        // structured_content always present — carries OutputData for rendering
        let structured = result
            .structured_content
            .as_ref()
            .expect("should have structured_content");
        assert_eq!(structured["ok"], true);
        assert_eq!(structured["code"], 0);
        // echo produces OutputData with a text node
        assert!(structured["output"].is_object(), "should have output field");

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
        let structured = result
            .structured_content
            .expect("should have structured_content");
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
        assert!(
            stderr_block.is_some(),
            "should have a [stderr] content block"
        );
    }

    #[test]
    fn test_severity_ordering() {
        assert!(severity(LoggingLevel::Debug) < severity(LoggingLevel::Info));
        assert!(severity(LoggingLevel::Info) < severity(LoggingLevel::Notice));
        assert!(severity(LoggingLevel::Notice) < severity(LoggingLevel::Warning));
        assert!(severity(LoggingLevel::Warning) < severity(LoggingLevel::Error));
        assert!(severity(LoggingLevel::Error) < severity(LoggingLevel::Critical));
        assert!(severity(LoggingLevel::Critical) < severity(LoggingLevel::Alert));
        assert!(severity(LoggingLevel::Alert) < severity(LoggingLevel::Emergency));
    }

    #[tokio::test]
    async fn test_set_level_stores_level() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");

        // Initially no level set
        assert!(handler.log_level.read().await.is_none());

        // Simulate set_level by writing directly (no peer needed for storage test)
        *handler.log_level.write().await = Some(LoggingLevel::Warning);
        assert_eq!(*handler.log_level.read().await, Some(LoggingLevel::Warning));

        // Update to a different level
        *handler.log_level.write().await = Some(LoggingLevel::Debug);
        assert_eq!(*handler.log_level.read().await, Some(LoggingLevel::Debug));
    }

    #[tokio::test]
    async fn test_log_to_client_without_peer() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config).expect("handler creation failed");

        // Enable logging at Debug level
        *handler.log_level.write().await = Some(LoggingLevel::Debug);

        // Should not panic even without a peer — just a no-op
        handler
            .log_to_client(LoggingLevel::Info, "test", Value::String("hello".into()))
            .await;
    }
}
