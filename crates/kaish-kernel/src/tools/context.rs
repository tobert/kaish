//! Execution context for tools.

use std::collections::HashMap;
use std::path::{Component, PathBuf};
use std::sync::Arc;

use crate::ast::Value;
use crate::backend::{KernelBackend, LocalBackend};
use crate::dispatch::PipelinePosition;
use crate::ignore_config::IgnoreConfig;
use crate::interpreter::Scope;
use crate::scheduler::{JobManager, PipeReader, PipeWriter, StderrStream};
use crate::tools::ToolRegistry;
use crate::vfs::VfsRouter;

use super::traits::ToolSchema;

/// Output context determines how command output should be formatted.
///
/// Different contexts prefer different output formats:
/// - **Interactive** — Pretty columns, colors, traditional tree (TTY/REPL)
/// - **Piped** — Raw output for pipeline processing
/// - **Model** — Token-efficient compact formats (MCP server / agent context)
/// - **Script** — Non-interactive script execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OutputContext {
    /// Interactive TTY/REPL - use human-friendly format with colors.
    #[default]
    Interactive,
    /// Output to another command - use raw output for pipes.
    Piped,
    /// MCP server / agent context - use token-efficient model format.
    Model,
    /// Non-interactive script - use raw output.
    Script,
}

/// Execution context passed to tools.
///
/// Provides access to the backend (for file operations and tool dispatch),
/// scope, and other kernel state.
pub struct ExecContext {
    /// Kernel backend for I/O operations.
    ///
    /// This is the preferred way to access filesystem operations.
    /// Use `backend.read()`, `backend.write()`, etc.
    pub backend: Arc<dyn KernelBackend>,
    /// Variable scope.
    pub scope: Scope,
    /// Current working directory (VFS path).
    pub cwd: PathBuf,
    /// Previous working directory (for `cd -`).
    pub prev_cwd: Option<PathBuf>,
    /// Standard input for the tool (from pipeline).
    pub stdin: Option<String>,
    /// Structured data from pipeline (pre-parsed JSON from previous command).
    /// Tools can check this before parsing stdin to avoid redundant JSON parsing.
    pub stdin_data: Option<Value>,
    /// Streaming pipe input (set when this command is in a concurrent pipeline).
    pub pipe_stdin: Option<PipeReader>,
    /// Streaming pipe output (set when this command is in a concurrent pipeline).
    pub pipe_stdout: Option<PipeWriter>,
    /// Tool schemas for help command.
    pub tool_schemas: Vec<ToolSchema>,
    /// Tool registry reference (for tools that need to inspect available tools).
    pub tools: Option<Arc<ToolRegistry>>,
    /// Job manager for background jobs (optional).
    pub job_manager: Option<Arc<JobManager>>,
    /// Kernel stderr stream for real-time error output from pipeline stages.
    ///
    /// When set, pipeline stages write stderr here instead of buffering in
    /// `ExecResult.err`. This allows stderr from all stages to stream to
    /// the terminal (or other sink) concurrently, matching bash behavior.
    pub stderr: Option<StderrStream>,
    /// Position of this command within a pipeline (for stdio decisions).
    pub pipeline_position: PipelinePosition,
    /// Whether we're running in interactive (REPL) mode.
    pub interactive: bool,
    /// Command aliases (name → expansion string).
    pub aliases: HashMap<String, String>,
    /// Ignore file configuration for file-walking tools.
    pub ignore_config: IgnoreConfig,
    /// Terminal state for job control (interactive mode, Unix only).
    #[cfg(unix)]
    pub terminal_state: Option<std::sync::Arc<crate::terminal::TerminalState>>,
}

impl ExecContext {
    /// Create a new execution context with a VFS (uses LocalBackend without tools).
    ///
    /// This constructor is for backward compatibility and tests that don't need tool dispatch.
    /// For full tool support, use `with_vfs_and_tools`.
    pub fn new(vfs: Arc<VfsRouter>) -> Self {
        Self {
            backend: Arc::new(LocalBackend::new(vfs)),
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            #[cfg(unix)]
            terminal_state: None,
        }
    }

    /// Create a new execution context with VFS and tool registry.
    ///
    /// This is the preferred constructor for full kaish operation where
    /// tools need to be dispatched through the backend.
    pub fn with_vfs_and_tools(vfs: Arc<VfsRouter>, tools: Arc<ToolRegistry>) -> Self {
        Self {
            backend: Arc::new(LocalBackend::with_tools(vfs, tools.clone())),
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new(),
            tools: Some(tools),
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            #[cfg(unix)]
            terminal_state: None,
        }
    }

    /// Create a new execution context with a custom backend.
    pub fn with_backend(backend: Arc<dyn KernelBackend>) -> Self {
        Self {
            backend,
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            #[cfg(unix)]
            terminal_state: None,
        }
    }

    /// Create a context with VFS, tools, and a specific scope.
    pub fn with_vfs_tools_and_scope(vfs: Arc<VfsRouter>, tools: Arc<ToolRegistry>, scope: Scope) -> Self {
        Self {
            backend: Arc::new(LocalBackend::with_tools(vfs, tools.clone())),
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new(),
            tools: Some(tools),
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            #[cfg(unix)]
            terminal_state: None,
        }
    }

    /// Create a context with a specific scope (uses LocalBackend without tools).
    ///
    /// For tests that don't need tool dispatch. For full tool support,
    /// use `with_vfs_tools_and_scope`.
    pub fn with_scope(vfs: Arc<VfsRouter>, scope: Scope) -> Self {
        Self {
            backend: Arc::new(LocalBackend::new(vfs)),
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            #[cfg(unix)]
            terminal_state: None,
        }
    }

    /// Create a context with a custom backend and scope.
    pub fn with_backend_and_scope(backend: Arc<dyn KernelBackend>, scope: Scope) -> Self {
        Self {
            backend,
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            #[cfg(unix)]
            terminal_state: None,
        }
    }

    /// Set the available tool schemas (for help command).
    pub fn set_tool_schemas(&mut self, schemas: Vec<ToolSchema>) {
        self.tool_schemas = schemas;
    }

    /// Set the tool registry reference.
    pub fn set_tools(&mut self, tools: Arc<ToolRegistry>) {
        self.tools = Some(tools);
    }

    /// Set the job manager for background job tracking.
    pub fn set_job_manager(&mut self, manager: Arc<JobManager>) {
        self.job_manager = Some(manager);
    }

    /// Set stdin for this execution.
    pub fn set_stdin(&mut self, stdin: String) {
        self.stdin = Some(stdin);
    }

    /// Get stdin, consuming it.
    pub fn take_stdin(&mut self) -> Option<String> {
        self.stdin.take()
    }

    /// Set both text stdin and structured data.
    ///
    /// Use this when passing output through a pipeline where the previous
    /// command produced structured data (e.g., JSON from MCP tools).
    pub fn set_stdin_with_data(&mut self, text: String, data: Option<Value>) {
        self.stdin = Some(text);
        self.stdin_data = data;
    }

    /// Take structured data if available, consuming it.
    ///
    /// Tools can use this to avoid re-parsing JSON that was already parsed
    /// by a previous command in the pipeline.
    pub fn take_stdin_data(&mut self) -> Option<Value> {
        self.stdin_data.take()
    }

    /// Resolve a path relative to cwd, normalizing `.` and `..` components.
    pub fn resolve_path(&self, path: &str) -> PathBuf {
        let raw = if path.starts_with('/') {
            PathBuf::from(path)
        } else {
            self.cwd.join(path)
        };
        normalize_path(&raw)
    }

    /// Change the current working directory.
    ///
    /// Saves the old directory for `cd -` support.
    pub fn set_cwd(&mut self, path: PathBuf) {
        self.prev_cwd = Some(self.cwd.clone());
        self.cwd = path;
    }

    /// Get the previous working directory (for `cd -`).
    pub fn get_prev_cwd(&self) -> Option<&PathBuf> {
        self.prev_cwd.as_ref()
    }

    /// Read all stdin (pipe or buffered string) into a String.
    ///
    /// Prefers pipe_stdin if set (streaming pipeline), otherwise falls back
    /// to the buffered stdin string. Consumes the source.
    pub async fn read_stdin_to_string(&mut self) -> Option<String> {
        if let Some(mut reader) = self.pipe_stdin.take() {
            use tokio::io::AsyncReadExt;
            let mut buf = Vec::new();
            reader.read_to_end(&mut buf).await.ok()?;
            Some(String::from_utf8_lossy(&buf).into_owned())
        } else {
            self.stdin.take()
        }
    }

    /// Create a child context for a pipeline stage.
    ///
    /// Shares backend, tools, job_manager, aliases, cwd, and scope
    /// but has independent stdin/stdout pipes.
    pub fn child_for_pipeline(&self) -> Self {
        Self {
            backend: self.backend.clone(),
            scope: self.scope.clone(),
            cwd: self.cwd.clone(),
            prev_cwd: self.prev_cwd.clone(),
            stdin: None,
            stdin_data: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: self.stderr.clone(),
            tool_schemas: self.tool_schemas.clone(),
            tools: self.tools.clone(),
            job_manager: self.job_manager.clone(),
            pipeline_position: PipelinePosition::Only,
            interactive: self.interactive,
            aliases: self.aliases.clone(),
            ignore_config: self.ignore_config.clone(),
            #[cfg(unix)]
            terminal_state: self.terminal_state.clone(),
        }
    }

    /// Build an `IgnoreFilter` from the current ignore configuration.
    ///
    /// Returns `None` if no filtering is configured.
    pub async fn build_ignore_filter(&self, root: &std::path::Path) -> Option<crate::walker::IgnoreFilter> {
        use crate::backend_walker_fs::BackendWalkerFs;
        let fs = BackendWalkerFs(self.backend.as_ref());
        self.ignore_config.build_filter(root, &fs).await
    }

    /// Expand a glob pattern to matching file paths.
    ///
    /// Returns the matched paths (absolute). Used by builtins that accept glob
    /// patterns in their path arguments (ls, cat, head, tail, wc, etc.).
    pub async fn expand_glob(&self, pattern: &str) -> Result<Vec<PathBuf>, String> {
        use crate::backend_walker_fs::BackendWalkerFs;
        use crate::walker::{EntryTypes, FileWalker, GlobPath, WalkOptions};

        let glob = GlobPath::new(pattern).map_err(|e| format!("invalid pattern: {}", e))?;

        let root = if glob.is_anchored() {
            self.resolve_path("/")
        } else {
            self.resolve_path(".")
        };

        let options = WalkOptions {
            entry_types: EntryTypes::all(),
            respect_gitignore: self.ignore_config.auto_gitignore(),
            ..WalkOptions::default()
        };

        let fs = BackendWalkerFs(self.backend.as_ref());
        let mut walker = FileWalker::new(&fs, &root)
            .with_pattern(glob)
            .with_options(options);

        // Note: if ignore_files contains ".gitignore" AND auto_gitignore is true,
        // the root .gitignore is loaded twice (once here, once by the walker).
        // This is harmless — merge is additive and rules are idempotent.
        if let Some(filter) = self.ignore_config.build_filter(&root, &fs).await {
            walker = walker.with_ignore(filter);
        }

        walker.collect().await.map_err(|e| e.to_string())
    }
}

/// Normalize a path by resolving `.` and `..` components lexically (no filesystem access).
fn normalize_path(path: &std::path::Path) -> PathBuf {
    let mut parts: Vec<Component> = Vec::new();
    for component in path.components() {
        match component {
            Component::CurDir => {} // skip `.`
            Component::ParentDir => {
                // Pop the last normal component, but don't pop past root
                if let Some(Component::Normal(_)) = parts.last() {
                    parts.pop();
                } else {
                    parts.push(component);
                }
            }
            _ => parts.push(component),
        }
    }
    if parts.is_empty() {
        PathBuf::from("/")
    } else {
        parts.iter().collect()
    }
}
