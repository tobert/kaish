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
    ToolAnnotations, UnsubscribeRequestParams,
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
use kaish_kernel::nonce::NonceStore;
use kaish_kernel::tools::ToolSchema;
use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};

use super::config::McpServerConfig;
use super::execute::{self, ExecuteParams, ExecuteResult};
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
    /// Cached tool schemas (static — avoids creating a kernel per prompt_builtins call).
    tool_schemas: Vec<ToolSchema>,
    /// Shared nonce store so confirmation latch nonces survive across execute() calls.
    nonce_store: NonceStore,
    /// Paths to init scripts loaded before each execute() call.
    /// Re-read from disk on each call so edits take effect without restart.
    init_paths: Vec<PathBuf>,
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
    ///
    /// `init_paths` are .kai scripts loaded before each `execute()` call (like ~/.bashrc).
    /// Paths are validated at startup; files are re-read on each call.
    pub fn new(config: McpServerConfig, init_paths: Vec<PathBuf>) -> anyhow::Result<Self> {
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

        // Pre-compute tool schemas once — they're static and used by prompt_builtins.
        let schema_kernel = kaish_kernel::Kernel::new(
            kaish_kernel::KernelConfig::isolated().with_skip_validation(true),
        )
        .context("Failed to create schema kernel")?;
        let tool_schemas = schema_kernel.tool_schemas();

        Ok(Self {
            config,
            vfs: Arc::new(RwLock::new(vfs)),
            tool_router: Self::tool_router(),
            prompt_router: Self::prompt_router(),
            watcher: ResourceWatcher::new(peer.clone()),
            peer,
            log_level: Arc::new(RwLock::new(None)),
            tool_schemas,
            nonce_store: NonceStore::new(),
            init_paths,
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
    #[tool(description = "Run shell commands with pre-validation — syntax errors are caught before anything executes.\n\nEach call runs in a fresh kernel (variables reset, cwd reset). Confirmation nonces persist across calls.\n\nKey advantages over bash:\n• No word splitting — $VAR with spaces just works, no quoting bugs\n• Bare glob expansion — *.txt expands to matching files (disable: set +o glob)\n• All builtins support --json for structured output (ls --json, ps --json, etc.)\n• Destructive commands (rm) can be gated behind confirmation nonces (set -o latch)\n\nBuiltins: grep, jq, git, find, sed, awk, cat, ls, tree, stat, diff, and 50+ more.\nAlso supports: pipes, redirects, here-docs, if/for/while, functions, ${VAR:-default}, $((arithmetic)).\n\nNot supported: process substitution <(), backticks, eval.\n\nPaths: Native paths work within $HOME. /v/ = ephemeral memory.\n\nFirst time? Run: help builtins")]
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
            execute::execute(
                params,
                &self.config.mcp_servers,
                self.config.default_timeout_ms,
                Some(self.nonce_store.clone()),
                &self.init_paths,
            )
                .await
                .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        // Content blocks: plain text for human/LLM consumption.
        // Priority hints tell clients what to surface:
        //   stdout: lower priority when structured_content carries the same data
        //   stderr: always relevant — errors, warnings, diagnostics
        //
        // On error, stderr comes first (it's the most useful content).
        // Empty stdout is omitted to avoid "no output" display in clients.
        let mut content = Vec::new();
        if !result.ok && !result.stderr.is_empty() {
            content.push(Content::text(&result.stderr).with_priority(1.0));
        }
        if !result.stdout.is_empty() {
            content.push(Content::text(&result.stdout).with_priority(0.3));
        }
        if result.ok && !result.stderr.is_empty() {
            content.push(Content::text(format!("[stderr] {}", result.stderr)).with_priority(0.8));
        }
        // Ensure at least one content block (MCP spec requires non-empty content)
        if content.is_empty() {
            content.push(Content::text("(no output)").with_priority(0.1));
        }

        let structured_content = {
            let mut structured = serde_json::to_value(&result)
                .map_err(|e| McpError::internal_error(e.to_string(), None))?;
            // Suppress `stdout` from structured_content when a structured
            // representation already carries the same information:
            //
            // 1. `output` with headers/multiple nodes → tabular/list data
            //    (builtins like wc, ls, ps). stdout is just text_out() of it.
            //    Simple text output (echo) keeps stdout for easy client access.
            // 2. `data` matches stdout → --json results or auto-detected JSON
            //    where data is the parsed form of exactly what's in stdout.
            //
            // In both cases, stdout in structured_content is redundant noise.
            // The text is still available in the Content blocks for display.
            let output_is_structured = result.output.as_ref().is_some_and(|o| {
                o.headers.is_some() || o.root.len() > 1 || !o.is_simple_text()
            });
            let suppress_stdout = output_is_structured
                || result.data.as_ref().is_some_and(|data| {
                    serde_json::to_string(data)
                        .is_ok_and(|data_json| result.stdout.trim() == data_json)
                });
            if suppress_stdout {
                if let Some(obj) = structured.as_object_mut() {
                    obj.remove("stdout");
                }
            }
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

        let content = get_help(&topic, &self.tool_schemas);

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
                "kaish (会sh) — Shell with pre-validation and structured output for MCP tool orchestration.\n\n\
                 Why use kaish instead of a raw shell:\n\
                 • Syntax errors are caught before execution — no half-run commands\n\
                 • No word splitting — $VAR with spaces just works\n\
                 • All builtins support --json for structured output (no parsing ls/ps text)\n\
                 • Destructive operations can require confirmation nonces (set -o latch)\n\
                 • External commands work via PATH (cargo build, git status, etc.)\n\n\
                 Tools:\n\
                 • execute — Run commands. First time? Run `help builtins` to see what's available.\n\n\
                 Configuration:\n\
                 • --init <path> — load a .kai script before each command (set defaults, aliases, etc.). Repeatable.\n\n\
                 Resources available via `kaish://vfs/{path}` URIs."
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
        let mut tools = self.tool_router.list_all();

        for tool in &mut tools {
            if tool.name == "execute" {
                // Output schema: lets clients validate/type-check the response
                match rmcp::handler::server::tool::schema_for_output::<ExecuteResult>() {
                    Ok(schema) => tool.output_schema = Some(schema),
                    Err(e) => tracing::warn!("Failed to generate ExecuteResult schema: {e}"),
                }
                // Tool annotations: execute runs arbitrary commands
                tool.annotations = Some(
                    ToolAnnotations::new()
                        .destructive(true)
                        .idempotent(false)
                        .open_world(true),
                );
            }
        }

        Ok(ListToolsResult {
            tools,
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

        // Send completion progress notification
        if let (Some(token), Some(peer)) = (&progress_token, self.peer.get()) {
            // Explicitly ignored: progress notifications are best-effort
            let _ = peer
                .notify_progress(ProgressNotificationParam {
                    progress_token: token.clone(),
                    progress: 1.0,
                    total: Some(1.0),
                    message: Some("Complete".to_string()),
                })
                .await;
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
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");
        assert_eq!(handler.config.name, "kaish");
    }

    #[tokio::test]
    async fn test_get_info() {
        use rmcp::ServerHandler;

        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");
        let info = handler.get_info();
        assert!(info.instructions.is_some());
        let instructions = info.instructions.unwrap();
        assert!(instructions.contains("execute"));
    }

    #[tokio::test]
    async fn test_get_info_capabilities() {
        use rmcp::ServerHandler;

        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");
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
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");

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
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");

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
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");

        // A command that fails produces stderr
        let input = Parameters(ExecuteInput {
            script: "nonexistent_command_xyz".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        });
        let result = handler.execute(input).await.expect("execute failed");

        // On error, stderr is the first content block (no [stderr] prefix)
        if let RawContent::Text(text) = &result.content[0].raw {
            assert!(
                text.text.contains("not found") || text.text.contains("nonexistent"),
                "first content block on error should be stderr: got {:?}",
                text.text
            );
        } else {
            panic!("Expected text content");
        }
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
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");

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
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");

        // Enable logging at Debug level
        *handler.log_level.write().await = Some(LoggingLevel::Debug);

        // Should not panic even without a peer — just a no-op
        handler
            .log_to_client(LoggingLevel::Info, "test", Value::String("hello".into()))
            .await;
    }

    #[tokio::test]
    async fn test_execute_tool_annotations_and_schema() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");

        // Simulate what list_tools() does: get tools from router, then annotate
        let mut tools = handler.tool_router.list_all();
        for tool in &mut tools {
            if tool.name == "execute" {
                match rmcp::handler::server::tool::schema_for_output::<ExecuteResult>() {
                    Ok(schema) => tool.output_schema = Some(schema),
                    Err(e) => panic!("schema generation failed: {e}"),
                }
                tool.annotations = Some(
                    ToolAnnotations::new()
                        .destructive(true)
                        .idempotent(false)
                        .open_world(true),
                );
            }
        }

        let execute_tool = tools.iter().find(|t| t.name == "execute")
            .expect("execute tool should be listed");

        // Tool annotations: execute is destructive, non-idempotent, open-world
        let annotations = execute_tool.annotations.as_ref()
            .expect("execute should have annotations");
        assert_eq!(annotations.destructive_hint, Some(true));
        assert_eq!(annotations.idempotent_hint, Some(false));
        assert_eq!(annotations.open_world_hint, Some(true));

        // Output schema should be present and valid
        let schema = execute_tool.output_schema.as_ref()
            .expect("execute should have an output schema");
        // Schema must contain the expected fields from ExecuteResult
        let schema_str = serde_json::to_string(schema.as_ref())
            .expect("schema should serialize");
        assert!(schema_str.contains("stdout"), "schema should include stdout field");
        assert!(schema_str.contains("stderr"), "schema should include stderr field");
        assert!(schema_str.contains("code"), "schema should include code field");
        assert!(schema_str.contains("ok"), "schema should include ok field");
    }

    #[tokio::test]
    async fn test_execute_stdout_priority() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");

        let input = Parameters(ExecuteInput {
            script: "echo hello".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        });
        let result = handler.execute(input).await.expect("execute failed");

        // stdout content should have low priority (structured_content is primary)
        assert_eq!(
            result.content[0].annotations.as_ref()
                .and_then(|a| a.priority),
            Some(0.3),
            "stdout should have priority 0.3"
        );
    }

    #[tokio::test]
    async fn test_execute_stderr_priority() {
        let config = McpServerConfig::default();
        let handler = KaishServerHandler::new(config, vec![]).expect("handler creation failed");

        let input = Parameters(ExecuteInput {
            script: "nonexistent_command_xyz".to_string(),
            cwd: None,
            env: None,
            timeout_ms: None,
        });
        let result = handler.execute(input).await.expect("execute failed");

        // On error, stderr is the first content block with high priority
        assert_eq!(
            result.content[0].annotations.as_ref()
                .and_then(|a| a.priority),
            Some(1.0),
            "stderr should have priority 1.0"
        );
    }

}
