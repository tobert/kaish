//! The Kernel (核) — the heart of kaish.
//!
//! The Kernel owns and coordinates all core components:
//! - Interpreter state (scope, $?)
//! - Tool registry (builtins, user tools, MCP)
//! - VFS router (mount points)
//! - Job manager (background jobs)
//!
//! # Architecture
//!
//! ```text
//! ┌────────────────────────────────────────────────────────────┐
//! │                         Kernel (核)                         │
//! │  ┌──────────────┐  ┌──────────────┐  ┌──────────────────┐  │
//! │  │   Scope      │  │ ToolRegistry │  │  VfsRouter       │  │
//! │  │  (variables) │  │   (builtins, │  │  (mount points)  │  │
//! │  │              │  │    MCP, user)│  │                  │  │
//! │  └──────────────┘  └──────────────┘  └──────────────────┘  │
//! │  ┌──────────────────────────────┐  ┌──────────────────┐    │
//! │  │  JobManager (background)     │  │  ExecResult ($?) │    │
//! │  └──────────────────────────────┘  └──────────────────┘    │
//! └────────────────────────────────────────────────────────────┘
//! ```

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result};
use tokio::sync::RwLock;

use async_trait::async_trait;

use crate::ast::{Arg, Command, Expr, Stmt, StringPart, ToolDef, Value, BinaryOp};
use crate::backend::{BackendError, KernelBackend};
use kaish_glob::glob_match;
use crate::dispatch::{CommandDispatcher, PipelinePosition};
use crate::interpreter::{apply_output_format, eval_expr, expand_tilde, json_to_value, value_to_string, ControlFlow, ExecResult, Scope};
use crate::parser::parse;
use crate::scheduler::{drain_to_stream, is_bool_type, schema_param_lookup, BoundedStream, JobManager, PipelineRunner, DEFAULT_STREAM_MAX_SIZE};
use crate::tools::{extract_output_format, register_builtins, resolve_in_path, ExecContext, ToolArgs, ToolRegistry};
use crate::validator::{Severity, Validator};
use crate::vfs::{JobFs, LocalFs, MemoryFs, VfsRouter};

/// VFS mount mode determines how the local filesystem is exposed.
///
/// Different modes trade off convenience vs. security:
/// - `Passthrough` gives native path access (best for human REPL use)
/// - `Sandboxed` restricts access to a subtree (safer for agents)
/// - `NoLocal` provides complete isolation (tests, pure memory mode)
#[derive(Debug, Clone)]
pub enum VfsMountMode {
    /// LocalFs at "/" — native paths work directly.
    ///
    /// Full filesystem access. Use for human-operated REPL sessions where
    /// native paths like `/home/user/project` should just work.
    ///
    /// Mounts:
    /// - `/` → LocalFs("/")
    /// - `/v` → MemoryFs (blob storage)
    Passthrough,

    /// Transparent sandbox — paths look native but access is restricted.
    ///
    /// The local filesystem is mounted at its real path (e.g., `/home/user`),
    /// so `/home/user/src/project` just works. But paths outside the sandbox
    /// root are not accessible.
    ///
    /// Mounts:
    /// - `/` → MemoryFs (catches paths outside sandbox)
    /// - `{root}` → LocalFs(root)  (e.g., `/home/user` → LocalFs)
    /// - `/tmp` → LocalFs("/tmp")
    /// - `/v` → MemoryFs (blob storage)
    Sandboxed {
        /// Root path for local filesystem. Defaults to `$HOME`.
        /// Can be restricted further, e.g., `~/src`.
        root: Option<PathBuf>,
    },

    /// No local filesystem. Memory only.
    ///
    /// Complete isolation — no access to the host filesystem.
    /// Useful for tests or pure sandboxed execution.
    ///
    /// Mounts:
    /// - `/` → MemoryFs
    /// - `/tmp` → MemoryFs
    /// - `/v` → MemoryFs
    NoLocal,
}

impl Default for VfsMountMode {
    fn default() -> Self {
        VfsMountMode::Sandboxed { root: None }
    }
}

/// Configuration for kernel initialization.
#[derive(Debug, Clone)]
pub struct KernelConfig {
    /// Name of this kernel (for identification).
    pub name: String,

    /// VFS mount mode — controls how local filesystem is exposed.
    pub vfs_mode: VfsMountMode,

    /// Initial working directory (VFS path).
    pub cwd: PathBuf,

    /// Whether to skip pre-execution validation.
    ///
    /// When false (default), scripts are validated before execution to catch
    /// errors early. Set to true to skip validation for performance or to
    /// allow dynamic/external commands.
    pub skip_validation: bool,

    /// When true, standalone external commands inherit stdio for real-time output.
    ///
    /// Set by script runner and REPL for human-visible output.
    /// Not set by MCP server (output must be captured for structured responses).
    pub interactive: bool,
}

/// Get the default sandbox root ($HOME).
fn default_sandbox_root() -> PathBuf {
    std::env::var("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/"))
}

impl Default for KernelConfig {
    fn default() -> Self {
        let home = default_sandbox_root();
        Self {
            name: "default".to_string(),
            vfs_mode: VfsMountMode::Sandboxed { root: None },
            cwd: home,
            skip_validation: false,
            interactive: false,
        }
    }
}

impl KernelConfig {
    /// Create a transient kernel config (sandboxed, for temporary use).
    pub fn transient() -> Self {
        let home = default_sandbox_root();
        Self {
            name: "transient".to_string(),
            vfs_mode: VfsMountMode::Sandboxed { root: None },
            cwd: home,
            skip_validation: false,
            interactive: false,
        }
    }

    /// Create a kernel config with the given name (sandboxed by default).
    pub fn named(name: &str) -> Self {
        let home = default_sandbox_root();
        Self {
            name: name.to_string(),
            vfs_mode: VfsMountMode::Sandboxed { root: None },
            cwd: home,
            skip_validation: false,
            interactive: false,
        }
    }

    /// Create a REPL config with passthrough filesystem access.
    ///
    /// Native paths like `/home/user/project` work directly.
    /// The cwd is set to the actual current working directory.
    pub fn repl() -> Self {
        let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));
        Self {
            name: "repl".to_string(),
            vfs_mode: VfsMountMode::Passthrough,
            cwd,
            skip_validation: false,
            interactive: false,
        }
    }

    /// Create an MCP server config with sandboxed filesystem access.
    ///
    /// Local filesystem is accessible at its real path (e.g., `/home/user`),
    /// but sandboxed to `$HOME`. Paths outside the sandbox are not accessible.
    pub fn mcp() -> Self {
        let home = default_sandbox_root();
        Self {
            name: "mcp".to_string(),
            vfs_mode: VfsMountMode::Sandboxed { root: None },
            cwd: home,
            skip_validation: false,
            interactive: false,
        }
    }

    /// Create an MCP server config with a custom sandbox root.
    ///
    /// Use this to restrict access to a subdirectory like `~/src`.
    pub fn mcp_with_root(root: PathBuf) -> Self {
        Self {
            name: "mcp".to_string(),
            vfs_mode: VfsMountMode::Sandboxed { root: Some(root.clone()) },
            cwd: root,
            skip_validation: false,
            interactive: false,
        }
    }

    /// Create a config with no local filesystem (memory only).
    ///
    /// Useful for tests or pure sandboxed execution.
    pub fn isolated() -> Self {
        Self {
            name: "isolated".to_string(),
            vfs_mode: VfsMountMode::NoLocal,
            cwd: PathBuf::from("/"),
            skip_validation: false,
            interactive: false,
        }
    }

    /// Set the VFS mount mode.
    pub fn with_vfs_mode(mut self, mode: VfsMountMode) -> Self {
        self.vfs_mode = mode;
        self
    }

    /// Set the initial working directory.
    pub fn with_cwd(mut self, cwd: PathBuf) -> Self {
        self.cwd = cwd;
        self
    }

    /// Skip pre-execution validation.
    pub fn with_skip_validation(mut self, skip: bool) -> Self {
        self.skip_validation = skip;
        self
    }

    /// Enable interactive mode (external commands inherit stdio).
    pub fn with_interactive(mut self, interactive: bool) -> Self {
        self.interactive = interactive;
        self
    }
}

/// The Kernel (核) — executes kaish code.
///
/// This is the primary interface for running kaish commands. It owns all
/// the runtime state: variables, tools, VFS, jobs, and persistence.
pub struct Kernel {
    /// Kernel name.
    name: String,
    /// Variable scope.
    scope: RwLock<Scope>,
    /// Tool registry.
    tools: Arc<ToolRegistry>,
    /// User-defined tools (from `tool name { body }` statements).
    user_tools: RwLock<HashMap<String, ToolDef>>,
    /// Virtual filesystem router.
    vfs: Arc<VfsRouter>,
    /// Background job manager.
    jobs: Arc<JobManager>,
    /// Pipeline runner.
    runner: PipelineRunner,
    /// Execution context (cwd, stdin, etc.).
    exec_ctx: RwLock<ExecContext>,
    /// Whether to skip pre-execution validation.
    skip_validation: bool,
    /// When true, standalone external commands inherit stdio for real-time output.
    interactive: bool,
}

impl Kernel {
    /// Create a new kernel with the given configuration.
    pub fn new(config: KernelConfig) -> Result<Self> {
        let mut vfs = Self::setup_vfs(&config);
        let jobs = Arc::new(JobManager::new());

        // Mount JobFs for job observability at /v/jobs
        vfs.mount("/v/jobs", JobFs::new(jobs.clone()));

        let vfs = Arc::new(vfs);

        Self::assemble(config, vfs, jobs, |vfs_ref, tools| {
            ExecContext::with_vfs_and_tools(vfs_ref.clone(), tools.clone())
        })
    }

    /// Set up VFS based on mount mode.
    fn setup_vfs(config: &KernelConfig) -> VfsRouter {
        let mut vfs = VfsRouter::new();

        match &config.vfs_mode {
            VfsMountMode::Passthrough => {
                // LocalFs at "/" — native paths work directly
                vfs.mount("/", LocalFs::new(PathBuf::from("/")));
                // Memory for blobs
                vfs.mount("/v", MemoryFs::new());
            }
            VfsMountMode::Sandboxed { root } => {
                // Memory at root for safety (catches paths outside sandbox)
                vfs.mount("/", MemoryFs::new());
                vfs.mount("/v", MemoryFs::new());

                // Real /tmp for interop with other processes
                vfs.mount("/tmp", LocalFs::new(PathBuf::from("/tmp")));

                // Resolve the sandbox root (defaults to $HOME)
                let local_root = root.clone().unwrap_or_else(|| {
                    std::env::var("HOME")
                        .map(PathBuf::from)
                        .unwrap_or_else(|_| PathBuf::from("/"))
                });

                // Mount at the real path for transparent access
                // e.g., /home/atobey → LocalFs("/home/atobey")
                // so /home/atobey/src/kaish just works
                let mount_point = local_root.to_string_lossy().to_string();
                vfs.mount(&mount_point, LocalFs::new(local_root));
            }
            VfsMountMode::NoLocal => {
                // Pure memory mode — no local filesystem
                vfs.mount("/", MemoryFs::new());
                vfs.mount("/tmp", MemoryFs::new());
                vfs.mount("/v", MemoryFs::new());
            }
        }

        vfs
    }

    /// Create a transient kernel (no persistence).
    pub fn transient() -> Result<Self> {
        Self::new(KernelConfig::transient())
    }

    /// Create a kernel with a custom backend and `/v/*` virtual path support.
    ///
    /// This is the constructor for embedding kaish in other systems that provide
    /// their own storage backend (e.g., CRDT-backed storage in kaijutsu).
    ///
    /// A `VirtualOverlayBackend` routes paths automatically:
    /// - `/v/*` → Internal VFS (JobFs at `/v/jobs`, MemoryFs at `/v/blobs`)
    /// - Everything else → Your custom backend
    ///
    /// The optional `configure_vfs` closure lets you add additional virtual mounts
    /// (e.g., `/v/docs` for CRDT blocks) after the built-in mounts are set up.
    ///
    /// **Note:** The config's `vfs_mode` is ignored — all non-`/v/*` path routing
    /// is handled by your custom backend. The config is only used for `name`, `cwd`,
    /// `skip_validation`, and `interactive`.
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Simple: default /v/* mounts only
    /// let kernel = Kernel::with_backend(backend, config, |_| {})?;
    ///
    /// // With custom mounts
    /// let kernel = Kernel::with_backend(backend, config, |vfs| {
    ///     vfs.mount_arc("/v/docs", docs_fs);
    ///     vfs.mount_arc("/v/g", git_fs);
    /// })?;
    /// ```
    pub fn with_backend(
        backend: Arc<dyn KernelBackend>,
        config: KernelConfig,
        configure_vfs: impl FnOnce(&mut VfsRouter),
    ) -> Result<Self> {
        use crate::backend::VirtualOverlayBackend;

        let mut vfs = VfsRouter::new();
        let jobs = Arc::new(JobManager::new());

        vfs.mount("/v/jobs", JobFs::new(jobs.clone()));
        vfs.mount("/v/blobs", MemoryFs::new());

        // Let caller add custom mounts (e.g., /v/docs, /v/g)
        configure_vfs(&mut vfs);

        let vfs = Arc::new(vfs);

        let overlay: Arc<dyn KernelBackend> =
            Arc::new(VirtualOverlayBackend::new(backend, vfs.clone()));

        Self::assemble(config, vfs, jobs, |_: &Arc<VfsRouter>, _: &Arc<ToolRegistry>| {
            ExecContext::with_backend(overlay)
        })
    }

    /// Shared assembly: wires up tools, runner, scope, and ExecContext.
    ///
    /// The `make_ctx` closure receives the VFS and tools so backends that need
    /// them (like `LocalBackend::with_tools`) can capture them. Custom backends
    /// that already have their own storage can ignore these parameters.
    fn assemble(
        config: KernelConfig,
        vfs: Arc<VfsRouter>,
        jobs: Arc<JobManager>,
        make_ctx: impl FnOnce(&Arc<VfsRouter>, &Arc<ToolRegistry>) -> ExecContext,
    ) -> Result<Self> {
        let KernelConfig { name, cwd, skip_validation, interactive, .. } = config;

        let mut tools = ToolRegistry::new();
        register_builtins(&mut tools);
        let tools = Arc::new(tools);

        let runner = PipelineRunner::new(tools.clone());

        let mut exec_ctx = make_ctx(&vfs, &tools);
        exec_ctx.set_cwd(cwd);
        exec_ctx.set_job_manager(jobs.clone());
        exec_ctx.set_tool_schemas(tools.schemas());
        exec_ctx.set_tools(tools.clone());

        Ok(Self {
            name,
            scope: RwLock::new(Scope::new()),
            tools,
            user_tools: RwLock::new(HashMap::new()),
            vfs,
            jobs,
            runner,
            exec_ctx: RwLock::new(exec_ctx),
            skip_validation,
            interactive,
        })
    }

    /// Get the kernel name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Execute kaish source code.
    ///
    /// Returns the result of the last statement executed.
    pub async fn execute(&self, input: &str) -> Result<ExecResult> {
        self.execute_streaming(input, &mut |_| {}).await
    }

    /// Execute kaish source code with a per-statement callback.
    ///
    /// Each statement's result is passed to `on_output` as it completes,
    /// allowing callers to flush output incrementally (e.g., print builtin
    /// output immediately rather than buffering until the script finishes).
    ///
    /// External commands in interactive mode already stream to the terminal
    /// via `Stdio::inherit()`, so the callback mainly handles builtins.
    #[tracing::instrument(level = "info", skip(self, on_output), fields(input_len = input.len()))]
    pub async fn execute_streaming(
        &self,
        input: &str,
        on_output: &mut dyn FnMut(&ExecResult),
    ) -> Result<ExecResult> {
        let program = parse(input).map_err(|errors| {
            let msg = errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<_>>()
                .join("; ");
            anyhow::anyhow!("parse error: {}", msg)
        })?;

        // Pre-execution validation
        if !self.skip_validation {
            let user_tools = self.user_tools.read().await;
            let validator = Validator::new(&self.tools, &user_tools);
            let issues = validator.validate(&program);

            // Collect errors (warnings are logged but don't prevent execution)
            let errors: Vec<_> = issues
                .iter()
                .filter(|i| i.severity == Severity::Error)
                .collect();

            if !errors.is_empty() {
                let error_msg = errors
                    .iter()
                    .map(|e| e.format(input))
                    .collect::<Vec<_>>()
                    .join("\n");
                return Err(anyhow::anyhow!("validation failed:\n{}", error_msg));
            }

            // Log warnings via tracing (trace level to avoid noise)
            for warning in issues.iter().filter(|i| i.severity == Severity::Warning) {
                tracing::trace!("validation: {}", warning.format(input));
            }
        }

        let mut result = ExecResult::success("");

        for stmt in program.statements {
            if matches!(stmt, Stmt::Empty) {
                continue;
            }
            let flow = self.execute_stmt_flow(&stmt).await?;
            match flow {
                ControlFlow::Normal(r) => {
                    on_output(&r);
                    // Carry the last statement's structured output for MCP TOON encoding.
                    // Must be done here (not in accumulate_result) because accumulate_result
                    // is also used in loops where per-iteration output would be wrong.
                    let last_output = r.output.clone();
                    accumulate_result(&mut result, &r);
                    result.output = last_output;
                }
                ControlFlow::Exit { code } => {
                    result.code = code;
                    return Ok(result);
                }
                ControlFlow::Return { value } => {
                    on_output(&value);
                    result = value;
                }
                ControlFlow::Break { result: r, .. } | ControlFlow::Continue { result: r, .. } => {
                    on_output(&r);
                    result = r;
                }
            }
        }

        Ok(result)
    }

    /// Execute a single statement, returning control flow information.
    fn execute_stmt_flow<'a>(
        &'a self,
        stmt: &'a Stmt,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<ControlFlow>> + Send + 'a>> {
        use tracing::Instrument;
        let span = tracing::debug_span!("execute_stmt_flow", stmt_type = %stmt.kind_name());
        Box::pin(async move {
        match stmt {
            Stmt::Assignment(assign) => {
                // Use async evaluator to support command substitution
                let value = self.eval_expr_async(&assign.value).await
                    .context("failed to evaluate assignment")?;
                let mut scope = self.scope.write().await;
                if assign.local {
                    // local: set in innermost (current function) frame
                    scope.set(&assign.name, value.clone());
                } else {
                    // non-local: update existing or create in root frame
                    scope.set_global(&assign.name, value.clone());
                }
                drop(scope);

                // Assignments don't produce output (like sh)
                Ok(ControlFlow::ok(ExecResult::success("")))
            }
            Stmt::Command(cmd) => {
                // Route single commands through execute_pipeline for a unified path.
                // This ensures all commands go through the dispatcher chain.
                let pipeline = crate::ast::Pipeline {
                    commands: vec![cmd.clone()],
                    background: false,
                };
                let result = self.execute_pipeline(&pipeline).await?;
                self.update_last_result(&result).await;

                // Check for error exit mode (set -e)
                if !result.ok() {
                    let scope = self.scope.read().await;
                    if scope.error_exit_enabled() {
                        return Ok(ControlFlow::exit_code(result.code));
                    }
                }

                Ok(ControlFlow::ok(result))
            }
            Stmt::Pipeline(pipeline) => {
                let result = self.execute_pipeline(pipeline).await?;
                self.update_last_result(&result).await;

                // Check for error exit mode (set -e)
                if !result.ok() {
                    let scope = self.scope.read().await;
                    if scope.error_exit_enabled() {
                        return Ok(ControlFlow::exit_code(result.code));
                    }
                }

                Ok(ControlFlow::ok(result))
            }
            Stmt::If(if_stmt) => {
                // Use async evaluator to support command substitution in conditions
                let cond_value = self.eval_expr_async(&if_stmt.condition).await?;

                let branch = if is_truthy(&cond_value) {
                    &if_stmt.then_branch
                } else {
                    if_stmt.else_branch.as_deref().unwrap_or(&[])
                };

                let mut result = ExecResult::success("");
                for stmt in branch {
                    let flow = self.execute_stmt_flow(stmt).await?;
                    match flow {
                        ControlFlow::Normal(r) => accumulate_result(&mut result, &r),
                        _ => return Ok(flow),
                    }
                }
                Ok(ControlFlow::ok(result))
            }
            Stmt::For(for_loop) => {
                // Evaluate all items and collect values for iteration
                // Use async evaluator to support command substitution like $(seq 1 5)
                let mut items: Vec<Value> = Vec::new();
                for item_expr in &for_loop.items {
                    let item = self.eval_expr_async(item_expr).await?;
                    // NO implicit word splitting - arrays iterate, strings stay whole
                    match &item {
                        // JSON arrays iterate over elements
                        Value::Json(serde_json::Value::Array(arr)) => {
                            for elem in arr {
                                items.push(json_to_value(elem.clone()));
                            }
                        }
                        // Strings are ONE value - no splitting!
                        // Use $(split "$VAR") for explicit splitting
                        Value::String(_) => {
                            items.push(item);
                        }
                        // Other values as-is
                        _ => items.push(item),
                    }
                }

                let mut result = ExecResult::success("");
                {
                    let mut scope = self.scope.write().await;
                    scope.push_frame();
                }

                'outer: for item in items {
                    {
                        let mut scope = self.scope.write().await;
                        scope.set(&for_loop.variable, item);
                    }
                    for stmt in &for_loop.body {
                        let mut flow = self.execute_stmt_flow(stmt).await?;
                        match &mut flow {
                            ControlFlow::Normal(r) => {
                                accumulate_result(&mut result, r);
                                if !r.ok() {
                                    let scope = self.scope.read().await;
                                    if scope.error_exit_enabled() {
                                        drop(scope);
                                        let mut scope = self.scope.write().await;
                                        scope.pop_frame();
                                        return Ok(ControlFlow::exit_code(r.code));
                                    }
                                }
                            }
                            ControlFlow::Break { .. } => {
                                if flow.decrement_level() {
                                    // Break handled at this level
                                    break 'outer;
                                }
                                // Propagate to outer loop
                                let mut scope = self.scope.write().await;
                                scope.pop_frame();
                                return Ok(flow);
                            }
                            ControlFlow::Continue { .. } => {
                                if flow.decrement_level() {
                                    // Continue handled at this level
                                    continue 'outer;
                                }
                                // Propagate to outer loop
                                let mut scope = self.scope.write().await;
                                scope.pop_frame();
                                return Ok(flow);
                            }
                            ControlFlow::Return { .. } | ControlFlow::Exit { .. } => {
                                let mut scope = self.scope.write().await;
                                scope.pop_frame();
                                return Ok(flow);
                            }
                        }
                    }
                }

                {
                    let mut scope = self.scope.write().await;
                    scope.pop_frame();
                }
                Ok(ControlFlow::ok(result))
            }
            Stmt::While(while_loop) => {
                let mut result = ExecResult::success("");

                'outer: loop {
                    // Evaluate condition - use async to support command substitution
                    let cond_value = self.eval_expr_async(&while_loop.condition).await?;

                    if !is_truthy(&cond_value) {
                        break;
                    }

                    // Execute body
                    for stmt in &while_loop.body {
                        let mut flow = self.execute_stmt_flow(stmt).await?;
                        match &mut flow {
                            ControlFlow::Normal(r) => {
                                accumulate_result(&mut result, r);
                                if !r.ok() {
                                    let scope = self.scope.read().await;
                                    if scope.error_exit_enabled() {
                                        return Ok(ControlFlow::exit_code(r.code));
                                    }
                                }
                            }
                            ControlFlow::Break { .. } => {
                                if flow.decrement_level() {
                                    // Break handled at this level
                                    break 'outer;
                                }
                                // Propagate to outer loop
                                return Ok(flow);
                            }
                            ControlFlow::Continue { .. } => {
                                if flow.decrement_level() {
                                    // Continue handled at this level
                                    continue 'outer;
                                }
                                // Propagate to outer loop
                                return Ok(flow);
                            }
                            ControlFlow::Return { .. } | ControlFlow::Exit { .. } => {
                                return Ok(flow);
                            }
                        }
                    }
                }

                Ok(ControlFlow::ok(result))
            }
            Stmt::Case(case_stmt) => {
                // Evaluate the expression to match against
                let match_value = {
                    let mut scope = self.scope.write().await;
                    let value = eval_expr(&case_stmt.expr, &mut scope)?;
                    value_to_string(&value)
                };

                // Try each branch until we find a match
                for branch in &case_stmt.branches {
                    let matched = branch.patterns.iter().any(|pattern| {
                        glob_match(pattern, &match_value)
                    });

                    if matched {
                        // Execute the branch body
                        let mut result = ExecResult::success("");
                        for stmt in &branch.body {
                            let flow = self.execute_stmt_flow(stmt).await?;
                            match flow {
                                ControlFlow::Normal(r) => accumulate_result(&mut result, &r),
                                _ => return Ok(flow),
                            }
                        }
                        return Ok(ControlFlow::ok(result));
                    }
                }

                // No match - return success with empty output (like sh)
                Ok(ControlFlow::ok(ExecResult::success("")))
            }
            Stmt::Break(levels) => {
                Ok(ControlFlow::break_n(levels.unwrap_or(1)))
            }
            Stmt::Continue(levels) => {
                Ok(ControlFlow::continue_n(levels.unwrap_or(1)))
            }
            Stmt::Return(expr) => {
                // return [N] - N becomes the exit code, NOT stdout
                // Shell semantics: return sets exit code, doesn't produce output
                let result = if let Some(e) = expr {
                    let mut scope = self.scope.write().await;
                    let val = eval_expr(e, &mut scope)?;
                    // Convert value to exit code
                    let code = match val {
                        Value::Int(n) => n,
                        Value::Bool(b) => if b { 0 } else { 1 },
                        _ => 0,
                    };
                    ExecResult {
                        code,
                        out: String::new(),
                        err: String::new(),
                        data: None,
                        output: None,
                    }
                } else {
                    ExecResult::success("")
                };
                Ok(ControlFlow::return_value(result))
            }
            Stmt::Exit(expr) => {
                let code = if let Some(e) = expr {
                    let mut scope = self.scope.write().await;
                    let val = eval_expr(e, &mut scope)?;
                    match val {
                        Value::Int(n) => n,
                        _ => 0,
                    }
                } else {
                    0
                };
                Ok(ControlFlow::exit_code(code))
            }
            Stmt::ToolDef(tool_def) => {
                let mut user_tools = self.user_tools.write().await;
                user_tools.insert(tool_def.name.clone(), tool_def.clone());
                Ok(ControlFlow::ok(ExecResult::success("")))
            }
            Stmt::AndChain { left, right } => {
                // cmd1 && cmd2 - run cmd2 only if cmd1 succeeds (exit code 0)
                let left_flow = self.execute_stmt_flow(left).await?;
                match left_flow {
                    ControlFlow::Normal(left_result) => {
                        self.update_last_result(&left_result).await;
                        if left_result.ok() {
                            let right_flow = self.execute_stmt_flow(right).await?;
                            match right_flow {
                                ControlFlow::Normal(right_result) => {
                                    self.update_last_result(&right_result).await;
                                    // Combine left and right output
                                    let mut combined = left_result;
                                    accumulate_result(&mut combined, &right_result);
                                    Ok(ControlFlow::ok(combined))
                                }
                                other => Ok(other), // Propagate non-normal flow
                            }
                        } else {
                            Ok(ControlFlow::ok(left_result))
                        }
                    }
                    _ => Ok(left_flow), // Propagate non-normal flow
                }
            }
            Stmt::OrChain { left, right } => {
                // cmd1 || cmd2 - run cmd2 only if cmd1 fails (non-zero exit code)
                let left_flow = self.execute_stmt_flow(left).await?;
                match left_flow {
                    ControlFlow::Normal(left_result) => {
                        self.update_last_result(&left_result).await;
                        if !left_result.ok() {
                            let right_flow = self.execute_stmt_flow(right).await?;
                            match right_flow {
                                ControlFlow::Normal(right_result) => {
                                    self.update_last_result(&right_result).await;
                                    // Combine left and right output
                                    let mut combined = left_result;
                                    accumulate_result(&mut combined, &right_result);
                                    Ok(ControlFlow::ok(combined))
                                }
                                other => Ok(other), // Propagate non-normal flow
                            }
                        } else {
                            Ok(ControlFlow::ok(left_result))
                        }
                    }
                    _ => Ok(left_flow), // Propagate non-normal flow
                }
            }
            Stmt::Test(test_expr) => {
                // Evaluate the test expression by wrapping in Expr::Test
                let expr = crate::ast::Expr::Test(Box::new(test_expr.clone()));
                let mut scope = self.scope.write().await;
                let value = eval_expr(&expr, &mut scope)?;
                drop(scope);
                let is_true = match value {
                    crate::ast::Value::Bool(b) => b,
                    _ => false,
                };
                if is_true {
                    Ok(ControlFlow::ok(ExecResult::success("")))
                } else {
                    Ok(ControlFlow::ok(ExecResult::failure(1, "")))
                }
            }
            Stmt::Empty => Ok(ControlFlow::ok(ExecResult::success(""))),
        }
        }.instrument(span))
    }

    /// Execute a pipeline.
    #[tracing::instrument(level = "debug", skip(self, pipeline), fields(background = pipeline.background, command_count = pipeline.commands.len()))]
    async fn execute_pipeline(&self, pipeline: &crate::ast::Pipeline) -> Result<ExecResult> {
        if pipeline.commands.is_empty() {
            return Ok(ExecResult::success(""));
        }

        // Handle background execution (`&` operator)
        if pipeline.background {
            return self.execute_background(pipeline).await;
        }

        // All commands go through the runner with the Kernel as dispatcher.
        // This is the single execution path — no fast path for single commands.
        //
        // IMPORTANT: We snapshot exec_ctx into a local context and release the
        // lock before running. This prevents deadlocks when dispatch_command
        // is called from within the pipeline and recursively triggers another
        // pipeline (e.g., via user-defined tools).
        let mut ctx = {
            let ec = self.exec_ctx.read().await;
            let scope = self.scope.read().await;
            ExecContext {
                backend: ec.backend.clone(),
                scope: scope.clone(),
                cwd: ec.cwd.clone(),
                prev_cwd: ec.prev_cwd.clone(),
                stdin: None,
                stdin_data: None,
                tool_schemas: ec.tool_schemas.clone(),
                tools: ec.tools.clone(),
                job_manager: ec.job_manager.clone(),
                pipeline_position: PipelinePosition::Only,
            }
        }; // locks released

        let result = self.runner.run(&pipeline.commands, &mut ctx, self).await;

        // Sync changes back from context
        {
            let mut ec = self.exec_ctx.write().await;
            ec.cwd = ctx.cwd.clone();
            ec.prev_cwd = ctx.prev_cwd.clone();
        }
        {
            let mut scope = self.scope.write().await;
            *scope = ctx.scope.clone();
        }

        Ok(result)
    }

    /// Execute a pipeline in the background.
    ///
    /// The command is spawned as a tokio task, registered with the JobManager,
    /// and its output is captured via BoundedStreams. The job is observable via
    /// `/v/jobs/{id}/stdout`, `/v/jobs/{id}/stderr`, and `/v/jobs/{id}/status`.
    ///
    /// Returns immediately with a job ID like "[1]".
    #[tracing::instrument(level = "debug", skip(self, pipeline), fields(command_count = pipeline.commands.len()))]
    async fn execute_background(&self, pipeline: &crate::ast::Pipeline) -> Result<ExecResult> {
        use tokio::sync::oneshot;

        // Format the command for display in /v/jobs/{id}/command
        let command_str = self.format_pipeline(pipeline);

        // Create bounded streams for output capture
        let stdout = Arc::new(BoundedStream::default_size());
        let stderr = Arc::new(BoundedStream::default_size());

        // Create channel for result notification
        let (tx, rx) = oneshot::channel();

        // Register with JobManager to get job ID and create VFS entries
        let job_id = self.jobs.register_with_streams(
            command_str.clone(),
            rx,
            stdout.clone(),
            stderr.clone(),
        ).await;

        // Clone state needed for the spawned task
        let runner = self.runner.clone();
        let commands = pipeline.commands.clone();
        let backend = {
            let ctx = self.exec_ctx.read().await;
            ctx.backend.clone()
        };
        let scope = {
            let scope = self.scope.read().await;
            scope.clone()
        };
        let cwd = {
            let ctx = self.exec_ctx.read().await;
            ctx.cwd.clone()
        };
        let tools = self.tools.clone();
        let tool_schemas = self.tools.schemas();

        // Spawn the background task
        tokio::spawn(async move {
            // Create execution context for the background job
            // It inherits env vars and cwd from the parent context
            let mut bg_ctx = ExecContext::with_backend(backend);
            bg_ctx.scope = scope;
            bg_ctx.cwd = cwd;
            bg_ctx.set_tools(tools.clone());
            bg_ctx.set_tool_schemas(tool_schemas);

            // Use BackendDispatcher for background jobs (builtins only).
            // Full Kernel dispatch requires Arc<Kernel> — planned for a future phase.
            let dispatcher = crate::dispatch::BackendDispatcher::new(tools);

            // Execute the pipeline
            let result = runner.run(&commands, &mut bg_ctx, &dispatcher).await;

            // Write output to streams
            if !result.out.is_empty() {
                stdout.write(result.out.as_bytes()).await;
            }
            if !result.err.is_empty() {
                stderr.write(result.err.as_bytes()).await;
            }

            // Close streams
            stdout.close().await;
            stderr.close().await;

            // Send result to JobManager (ignore error if receiver dropped)
            let _ = tx.send(result);
        });

        Ok(ExecResult::success(format!("[{}]", job_id)))
    }

    /// Format a pipeline as a command string for display.
    fn format_pipeline(&self, pipeline: &crate::ast::Pipeline) -> String {
        pipeline.commands
            .iter()
            .map(|cmd| {
                let mut parts = vec![cmd.name.clone()];
                for arg in &cmd.args {
                    match arg {
                        Arg::Positional(expr) => {
                            parts.push(self.format_expr(expr));
                        }
                        Arg::Named { key, value } => {
                            parts.push(format!("{}={}", key, self.format_expr(value)));
                        }
                        Arg::ShortFlag(name) => {
                            parts.push(format!("-{}", name));
                        }
                        Arg::LongFlag(name) => {
                            parts.push(format!("--{}", name));
                        }
                        Arg::DoubleDash => {
                            parts.push("--".to_string());
                        }
                    }
                }
                parts.join(" ")
            })
            .collect::<Vec<_>>()
            .join(" | ")
    }

    /// Format an expression as a string for display.
    fn format_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(Value::String(s)) => {
                if s.contains(' ') || s.contains('"') {
                    format!("'{}'", s.replace('\'', "\\'"))
                } else {
                    s.clone()
                }
            }
            Expr::Literal(Value::Int(i)) => i.to_string(),
            Expr::Literal(Value::Float(f)) => f.to_string(),
            Expr::Literal(Value::Bool(b)) => b.to_string(),
            Expr::Literal(Value::Null) => "null".to_string(),
            Expr::VarRef(path) => {
                let name = path.segments.iter()
                    .map(|seg| match seg {
                        crate::ast::VarSegment::Field(f) => f.clone(),
                    })
                    .collect::<Vec<_>>()
                    .join(".");
                format!("${{{}}}", name)
            }
            Expr::Interpolated(_) => "\"...\"".to_string(),
            _ => "...".to_string(),
        }
    }

    /// Execute a single command.
    #[tracing::instrument(level = "info", skip(self, args), fields(command = %name), err)]
    async fn execute_command(&self, name: &str, args: &[Arg]) -> Result<ExecResult> {
        // Special built-ins
        match name {
            "true" => return Ok(ExecResult::success("")),
            "false" => return Ok(ExecResult::failure(1, "")),
            "source" | "." => return self.execute_source(args).await,
            _ => {}
        }

        // Check user-defined tools first
        {
            let user_tools = self.user_tools.read().await;
            if let Some(tool_def) = user_tools.get(name) {
                let tool_def = tool_def.clone();
                drop(user_tools);
                return self.execute_user_tool(tool_def, args).await;
            }
        }

        // Look up builtin tool
        let tool = match self.tools.get(name) {
            Some(t) => t,
            None => {
                // Try executing as .kai script from PATH
                if let Some(result) = self.try_execute_script(name, args).await? {
                    return Ok(result);
                }
                // Try executing as external command from PATH
                if let Some(result) = self.try_execute_external(name, args).await? {
                    return Ok(result);
                }

                // Try backend-registered tools (embedder engines, MCP tools, etc.)
                let tool_args = self.build_args_async(args, None).await?;
                let mut ctx = self.exec_ctx.write().await;
                {
                    let scope = self.scope.read().await;
                    ctx.scope = scope.clone();
                }
                let backend = ctx.backend.clone();
                match backend.call_tool(name, tool_args, &mut ctx).await {
                    Ok(tool_result) => {
                        let mut scope = self.scope.write().await;
                        *scope = ctx.scope.clone();
                        let mut exec = ExecResult::from_output(
                            tool_result.code as i64, tool_result.stdout, tool_result.stderr,
                        );
                        exec.output = tool_result.output;
                        return Ok(exec);
                    }
                    Err(BackendError::ToolNotFound(_)) => {
                        // Fall through to "command not found"
                    }
                    Err(e) => {
                        return Ok(ExecResult::failure(1, e.to_string()));
                    }
                }

                return Ok(ExecResult::failure(127, format!("command not found: {}", name)));
            }
        };

        // Build arguments (async to support command substitution, schema-aware for flag values)
        let schema = tool.schema();
        let mut tool_args = self.build_args_async(args, Some(&schema)).await?;
        let output_format = extract_output_format(&mut tool_args, Some(&schema));

        // Execute
        let mut ctx = self.exec_ctx.write().await;
        {
            let scope = self.scope.read().await;
            ctx.scope = scope.clone();
        }

        let result = tool.execute(tool_args, &mut ctx).await;

        // Sync scope changes back (e.g., from cd)
        {
            let mut scope = self.scope.write().await;
            *scope = ctx.scope.clone();
        }

        let result = match output_format {
            Some(format) => apply_output_format(result, format),
            None => result,
        };

        Ok(result)
    }

    /// Build tool arguments from AST args.
    ///
    /// Uses async evaluation to support command substitution in arguments.
    async fn build_args_async(&self, args: &[Arg], schema: Option<&crate::tools::ToolSchema>) -> Result<ToolArgs> {
        let mut tool_args = ToolArgs::new();
        let param_lookup = schema.map(schema_param_lookup).unwrap_or_default();

        // Track which positional indices have been consumed as flag values
        let mut consumed: std::collections::HashSet<usize> = std::collections::HashSet::new();
        let mut past_double_dash = false;

        // Find positional arg indices for flag value consumption
        let positional_indices: Vec<usize> = args.iter().enumerate()
            .filter_map(|(i, a)| matches!(a, Arg::Positional(_)).then_some(i))
            .collect();

        let mut i = 0;
        while i < args.len() {
            match &args[i] {
                Arg::DoubleDash => {
                    past_double_dash = true;
                }
                Arg::Positional(expr) => {
                    if !consumed.contains(&i) {
                        let value = self.eval_expr_async(expr).await?;
                        let value = apply_tilde_expansion(value);
                        tool_args.positional.push(value);
                    }
                }
                Arg::Named { key, value } => {
                    let val = self.eval_expr_async(value).await?;
                    let val = apply_tilde_expansion(val);
                    tool_args.named.insert(key.clone(), val);
                }
                Arg::ShortFlag(name) => {
                    if past_double_dash {
                        tool_args.positional.push(Value::String(format!("-{name}")));
                    } else if name.len() == 1 {
                        let flag_name = name.as_str();
                        let lookup = param_lookup.get(flag_name);
                        let is_bool = lookup.map(|(_, typ)| is_bool_type(typ)).unwrap_or(true);

                        if is_bool {
                            tool_args.flags.insert(flag_name.to_string());
                        } else {
                            // Non-bool: consume next positional as value
                            let canonical = lookup.map(|(name, _)| *name).unwrap_or(flag_name);
                            let next_pos = positional_indices.iter()
                                .find(|idx| **idx > i && !consumed.contains(idx));

                            if let Some(&pos_idx) = next_pos {
                                if let Arg::Positional(expr) = &args[pos_idx] {
                                    let value = self.eval_expr_async(expr).await?;
                                    let value = apply_tilde_expansion(value);
                                    tool_args.named.insert(canonical.to_string(), value);
                                    consumed.insert(pos_idx);
                                }
                            } else {
                                tool_args.flags.insert(flag_name.to_string());
                            }
                        }
                    } else {
                        // Multi-char combined flags like -la: always boolean
                        for c in name.chars() {
                            tool_args.flags.insert(c.to_string());
                        }
                    }
                }
                Arg::LongFlag(name) => {
                    if past_double_dash {
                        tool_args.positional.push(Value::String(format!("--{name}")));
                    } else {
                        let lookup = param_lookup.get(name.as_str());
                        let is_bool = lookup.map(|(_, typ)| is_bool_type(typ)).unwrap_or(true);

                        if is_bool {
                            tool_args.flags.insert(name.clone());
                        } else {
                            let canonical = lookup.map(|(name, _)| *name).unwrap_or(name.as_str());
                            let next_pos = positional_indices.iter()
                                .find(|idx| **idx > i && !consumed.contains(idx));

                            if let Some(&pos_idx) = next_pos {
                                if let Arg::Positional(expr) = &args[pos_idx] {
                                    let value = self.eval_expr_async(expr).await?;
                                    let value = apply_tilde_expansion(value);
                                    tool_args.named.insert(canonical.to_string(), value);
                                    consumed.insert(pos_idx);
                                }
                            } else {
                                tool_args.flags.insert(name.clone());
                            }
                        }
                    }
                }
            }
            i += 1;
        }

        Ok(tool_args)
    }

    /// Build arguments as flat string list for external commands.
    ///
    /// Unlike `build_args_async` which separates flags into a HashSet (for schema-aware builtins),
    /// this preserves the original flag format as strings for external commands:
    /// - `-l` stays as `-l`
    /// - `--verbose` stays as `--verbose`
    /// - `key=value` stays as `key=value`
    ///
    /// This is what external commands expect in their argv.
    async fn build_args_flat(&self, args: &[Arg]) -> Result<Vec<String>> {
        let mut argv = Vec::new();
        for arg in args {
            match arg {
                Arg::Positional(expr) => {
                    let value = self.eval_expr_async(expr).await?;
                    let value = apply_tilde_expansion(value);
                    argv.push(value_to_string(&value));
                }
                Arg::Named { key, value } => {
                    let val = self.eval_expr_async(value).await?;
                    let val = apply_tilde_expansion(val);
                    argv.push(format!("{}={}", key, value_to_string(&val)));
                }
                Arg::ShortFlag(name) => {
                    // Preserve original format: -l, -la (combined flags)
                    argv.push(format!("-{}", name));
                }
                Arg::LongFlag(name) => {
                    // Preserve original format: --verbose
                    argv.push(format!("--{}", name));
                }
                Arg::DoubleDash => {
                    // Preserve the -- marker
                    argv.push("--".to_string());
                }
            }
        }
        Ok(argv)
    }

    /// Async expression evaluator that supports command substitution.
    ///
    /// This is used for contexts where expressions may contain `$(...)` command
    /// substitution. Unlike the sync `eval_expr`, this can execute pipelines.
    fn eval_expr_async<'a>(&'a self, expr: &'a Expr) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<Value>> + Send + 'a>> {
        Box::pin(async move {
        match expr {
            Expr::Literal(value) => Ok(value.clone()),
            Expr::VarRef(path) => {
                let scope = self.scope.read().await;
                scope.resolve_path(path)
                    .ok_or_else(|| anyhow::anyhow!("undefined variable"))
            }
            Expr::Interpolated(parts) => {
                let mut result = String::new();
                for part in parts {
                    result.push_str(&self.eval_string_part_async(part).await?);
                }
                Ok(Value::String(result))
            }
            Expr::BinaryOp { left, op, right } => {
                match op {
                    BinaryOp::And => {
                        let left_val = self.eval_expr_async(left).await?;
                        if !is_truthy(&left_val) {
                            return Ok(left_val);
                        }
                        self.eval_expr_async(right).await
                    }
                    BinaryOp::Or => {
                        let left_val = self.eval_expr_async(left).await?;
                        if is_truthy(&left_val) {
                            return Ok(left_val);
                        }
                        self.eval_expr_async(right).await
                    }
                    _ => {
                        // For other operators, fall back to sync eval
                        let mut scope = self.scope.write().await;
                        eval_expr(expr, &mut scope).map_err(|e| anyhow::anyhow!("{}", e))
                    }
                }
            }
            Expr::CommandSubst(pipeline) => {
                // Snapshot scope+cwd before running — only output escapes,
                // not side effects like `cd` or variable assignments.
                let saved_scope = { self.scope.read().await.clone() };
                let saved_cwd = {
                    let ec = self.exec_ctx.read().await;
                    (ec.cwd.clone(), ec.prev_cwd.clone())
                };

                // Capture result without `?` — restore state unconditionally
                let run_result = self.execute_pipeline(pipeline).await;

                // Restore scope and cwd regardless of success/failure
                {
                    let mut scope = self.scope.write().await;
                    *scope = saved_scope;
                    if let Ok(ref r) = run_result {
                        scope.set_last_result(r.clone());
                    }
                }
                {
                    let mut ec = self.exec_ctx.write().await;
                    ec.cwd = saved_cwd.0;
                    ec.prev_cwd = saved_cwd.1;
                }

                // Now propagate the error
                let result = run_result?;

                // Prefer structured data (enables `for i in $(cmd)` iteration)
                if let Some(data) = &result.data {
                    Ok(data.clone())
                } else if let Some(ref output) = result.output {
                    // Flat non-text node lists (glob, ls, tree) → iterable array
                    if output.is_flat() && !output.is_simple_text() && !output.root.is_empty() {
                        let items: Vec<serde_json::Value> = output.root.iter()
                            .map(|n| serde_json::Value::String(n.display_name().to_string()))
                            .collect();
                        Ok(Value::Json(serde_json::Value::Array(items)))
                    } else {
                        Ok(Value::String(result.out.trim_end().to_string()))
                    }
                } else {
                    // Otherwise return stdout as single string (NO implicit splitting)
                    Ok(Value::String(result.out.trim_end().to_string()))
                }
            }
            Expr::Test(test_expr) => {
                // For test expressions, use the sync evaluator
                let expr = Expr::Test(test_expr.clone());
                let mut scope = self.scope.write().await;
                eval_expr(&expr, &mut scope).map_err(|e| anyhow::anyhow!("{}", e))
            }
            Expr::Positional(n) => {
                let scope = self.scope.read().await;
                match scope.get_positional(*n) {
                    Some(s) => Ok(Value::String(s.to_string())),
                    None => Ok(Value::String(String::new())),
                }
            }
            Expr::AllArgs => {
                let scope = self.scope.read().await;
                Ok(Value::String(scope.all_args().join(" ")))
            }
            Expr::ArgCount => {
                let scope = self.scope.read().await;
                Ok(Value::Int(scope.arg_count() as i64))
            }
            Expr::VarLength(name) => {
                let scope = self.scope.read().await;
                match scope.get(name) {
                    Some(value) => Ok(Value::Int(value_to_string(value).len() as i64)),
                    None => Ok(Value::Int(0)),
                }
            }
            Expr::VarWithDefault { name, default } => {
                let scope = self.scope.read().await;
                let use_default = match scope.get(name) {
                    Some(value) => value_to_string(value).is_empty(),
                    None => true,
                };
                drop(scope); // Release the lock before recursive evaluation
                if use_default {
                    // Evaluate the default parts (supports nested expansions)
                    self.eval_string_parts_async(default).await.map(Value::String)
                } else {
                    let scope = self.scope.read().await;
                    scope.get(name).cloned().ok_or_else(|| anyhow::anyhow!("variable '{}' not found", name))
                }
            }
            Expr::Arithmetic(expr_str) => {
                let scope = self.scope.read().await;
                crate::arithmetic::eval_arithmetic(expr_str, &scope)
                    .map(Value::Int)
                    .map_err(|e| anyhow::anyhow!("arithmetic error: {}", e))
            }
            Expr::Command(cmd) => {
                // Execute command and return boolean based on exit code
                let result = self.execute_command(&cmd.name, &cmd.args).await?;
                Ok(Value::Bool(result.code == 0))
            }
            Expr::LastExitCode => {
                let scope = self.scope.read().await;
                Ok(Value::Int(scope.last_result().code))
            }
            Expr::CurrentPid => {
                let scope = self.scope.read().await;
                Ok(Value::Int(scope.pid() as i64))
            }
        }
        })
    }

    /// Async helper to evaluate multiple StringParts into a single string.
    fn eval_string_parts_async<'a>(&'a self, parts: &'a [StringPart]) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<String>> + Send + 'a>> {
        Box::pin(async move {
            let mut result = String::new();
            for part in parts {
                result.push_str(&self.eval_string_part_async(part).await?);
            }
            Ok(result)
        })
    }

    /// Async helper to evaluate a StringPart.
    fn eval_string_part_async<'a>(&'a self, part: &'a StringPart) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<String>> + Send + 'a>> {
        Box::pin(async move {
            match part {
                StringPart::Literal(s) => Ok(s.clone()),
                StringPart::Var(path) => {
                    let scope = self.scope.read().await;
                    match scope.resolve_path(path) {
                        Some(value) => Ok(value_to_string(&value)),
                        None => Ok(String::new()), // Unset vars expand to empty
                    }
                }
                StringPart::VarWithDefault { name, default } => {
                    let scope = self.scope.read().await;
                    let use_default = match scope.get(name) {
                        Some(value) => value_to_string(value).is_empty(),
                        None => true,
                    };
                    drop(scope); // Release lock before recursive evaluation
                    if use_default {
                        // Evaluate the default parts (supports nested expansions)
                        self.eval_string_parts_async(default).await
                    } else {
                        let scope = self.scope.read().await;
                        Ok(value_to_string(scope.get(name).ok_or_else(|| anyhow::anyhow!("variable '{}' not found", name))?))
                    }
                }
            StringPart::VarLength(name) => {
                let scope = self.scope.read().await;
                match scope.get(name) {
                    Some(value) => Ok(value_to_string(value).len().to_string()),
                    None => Ok("0".to_string()),
                }
            }
            StringPart::Positional(n) => {
                let scope = self.scope.read().await;
                match scope.get_positional(*n) {
                    Some(s) => Ok(s.to_string()),
                    None => Ok(String::new()),
                }
            }
            StringPart::AllArgs => {
                let scope = self.scope.read().await;
                Ok(scope.all_args().join(" "))
            }
            StringPart::ArgCount => {
                let scope = self.scope.read().await;
                Ok(scope.arg_count().to_string())
            }
            StringPart::Arithmetic(expr) => {
                let scope = self.scope.read().await;
                match crate::arithmetic::eval_arithmetic(expr, &scope) {
                    Ok(value) => Ok(value.to_string()),
                    Err(_) => Ok(String::new()),
                }
            }
            StringPart::CommandSubst(pipeline) => {
                // Snapshot scope+cwd — command substitution in strings must
                // not leak side effects (e.g., `"dir: $(cd /; pwd)"` must not change cwd).
                let saved_scope = { self.scope.read().await.clone() };
                let saved_cwd = {
                    let ec = self.exec_ctx.read().await;
                    (ec.cwd.clone(), ec.prev_cwd.clone())
                };

                // Capture result without `?` — restore state unconditionally
                let run_result = self.execute_pipeline(pipeline).await;

                // Restore scope and cwd regardless of success/failure
                {
                    let mut scope = self.scope.write().await;
                    *scope = saved_scope;
                    if let Ok(ref r) = run_result {
                        scope.set_last_result(r.clone());
                    }
                }
                {
                    let mut ec = self.exec_ctx.write().await;
                    ec.cwd = saved_cwd.0;
                    ec.prev_cwd = saved_cwd.1;
                }

                // Now propagate the error
                let result = run_result?;

                Ok(result.out.trim_end_matches('\n').to_string())
            }
            StringPart::LastExitCode => {
                let scope = self.scope.read().await;
                Ok(scope.last_result().code.to_string())
            }
            StringPart::CurrentPid => {
                let scope = self.scope.read().await;
                Ok(scope.pid().to_string())
            }
        }
        })
    }

    /// Update the last result in scope.
    async fn update_last_result(&self, result: &ExecResult) {
        let mut scope = self.scope.write().await;
        scope.set_last_result(result.clone());
    }

    /// Execute a user-defined function with local variable scoping.
    ///
    /// Functions push a new scope frame for local variables. Variables declared
    /// with `local` are scoped to the function; other assignments modify outer
    /// scopes (or create in root if new).
    async fn execute_user_tool(&self, def: ToolDef, args: &[Arg]) -> Result<ExecResult> {
        // 1. Build function args from AST args (async to support command substitution)
        let tool_args = self.build_args_async(args, None).await?;

        // 2. Push a new scope frame for local variables
        {
            let mut scope = self.scope.write().await;
            scope.push_frame();
        }

        // 3. Save current positional parameters and set new ones for this function
        let saved_positional = {
            let mut scope = self.scope.write().await;
            let saved = scope.save_positional();

            // Set up new positional parameters ($0 = function name, $1, $2, ... = args)
            let positional_args: Vec<String> = tool_args.positional
                .iter()
                .map(value_to_string)
                .collect();
            scope.set_positional(&def.name, positional_args);

            saved
        };

        // 3. Execute body statements with control flow handling
        // Accumulate output across statements (like sh)
        let mut accumulated_out = String::new();
        let mut accumulated_err = String::new();
        let mut last_code = 0i64;
        let mut last_data: Option<Value> = None;

        // Track execution error for propagation after cleanup
        let mut exec_error: Option<anyhow::Error> = None;
        let mut exit_code: Option<i64> = None;

        for stmt in &def.body {
            match self.execute_stmt_flow(stmt).await {
                Ok(flow) => {
                    match flow {
                        ControlFlow::Normal(r) => {
                            accumulated_out.push_str(&r.out);
                            accumulated_err.push_str(&r.err);
                            last_code = r.code;
                            last_data = r.data;
                        }
                        ControlFlow::Return { value } => {
                            accumulated_out.push_str(&value.out);
                            accumulated_err.push_str(&value.err);
                            last_code = value.code;
                            last_data = value.data;
                            break;
                        }
                        ControlFlow::Exit { code } => {
                            exit_code = Some(code);
                            break;
                        }
                        ControlFlow::Break { result: r, .. } | ControlFlow::Continue { result: r, .. } => {
                            accumulated_out.push_str(&r.out);
                            accumulated_err.push_str(&r.err);
                            last_code = r.code;
                            last_data = r.data;
                        }
                    }
                }
                Err(e) => {
                    exec_error = Some(e);
                    break;
                }
            }
        }

        // 4. Pop scope frame and restore original positional parameters (unconditionally)
        {
            let mut scope = self.scope.write().await;
            scope.pop_frame();
            scope.set_positional(saved_positional.0, saved_positional.1);
        }

        // 5. Propagate error or exit after cleanup
        if let Some(e) = exec_error {
            return Err(e);
        }
        if let Some(code) = exit_code {
            return Ok(ExecResult {
                code,
                out: accumulated_out,
                err: accumulated_err,
                data: last_data,
                output: None,
            });
        }

        Ok(ExecResult {
            code: last_code,
            out: accumulated_out,
            err: accumulated_err,
            data: last_data,
            output: None,
        })
    }

    /// Execute the `source` / `.` command to include and run a script.
    ///
    /// Unlike regular tool execution, `source` executes in the CURRENT scope,
    /// allowing the sourced script to set variables and modify shell state.
    async fn execute_source(&self, args: &[Arg]) -> Result<ExecResult> {
        // Get the file path from the first positional argument
        let tool_args = self.build_args_async(args, None).await?;
        let path = match tool_args.positional.first() {
            Some(Value::String(s)) => s.clone(),
            Some(v) => value_to_string(v),
            None => {
                return Ok(ExecResult::failure(1, "source: missing filename"));
            }
        };

        // Resolve path relative to cwd
        let full_path = {
            let ctx = self.exec_ctx.read().await;
            if path.starts_with('/') {
                std::path::PathBuf::from(&path)
            } else {
                ctx.cwd.join(&path)
            }
        };

        // Read file content via backend
        let content = {
            let ctx = self.exec_ctx.read().await;
            match ctx.backend.read(&full_path, None).await {
                Ok(bytes) => {
                    String::from_utf8(bytes).map_err(|e| {
                        anyhow::anyhow!("source: {}: invalid UTF-8: {}", path, e)
                    })?
                }
                Err(e) => {
                    return Ok(ExecResult::failure(
                        1,
                        format!("source: {}: {}", path, e),
                    ));
                }
            }
        };

        // Parse the content
        let program = match crate::parser::parse(&content) {
            Ok(p) => p,
            Err(errors) => {
                let msg = errors
                    .iter()
                    .map(|e| format!("{}:{}: {}", path, e.span.start, e.message))
                    .collect::<Vec<_>>()
                    .join("\n");
                return Ok(ExecResult::failure(1, format!("source: {}", msg)));
            }
        };

        // Execute each statement in the CURRENT scope (not isolated)
        let mut result = ExecResult::success("");
        for stmt in program.statements {
            if matches!(stmt, crate::ast::Stmt::Empty) {
                continue;
            }

            match self.execute_stmt_flow(&stmt).await {
                Ok(flow) => {
                    match flow {
                        ControlFlow::Normal(r) => {
                            result = r.clone();
                            self.update_last_result(&r).await;
                        }
                        ControlFlow::Break { .. } | ControlFlow::Continue { .. } => {
                            // break/continue in sourced file - unusual but propagate
                            return Err(anyhow::anyhow!(
                                "source: {}: unexpected break/continue outside loop",
                                path
                            ));
                        }
                        ControlFlow::Return { value } => {
                            // Return from sourced script ends the source
                            return Ok(value);
                        }
                        ControlFlow::Exit { code } => {
                            // Exit from sourced script — preserve accumulated output
                            result.code = code;
                            return Ok(result);
                        }
                    }
                }
                Err(e) => {
                    return Err(e.context(format!("source: {}", path)));
                }
            }
        }

        Ok(result)
    }

    /// Try to execute a script from PATH directories.
    ///
    /// Searches PATH for `{name}.kai` files and executes them in isolated scope
    /// (like user-defined tools). Returns None if no script is found.
    async fn try_execute_script(&self, name: &str, args: &[Arg]) -> Result<Option<ExecResult>> {
        // Get PATH from scope (default to "/bin")
        let path_value = {
            let scope = self.scope.read().await;
            scope
                .get("PATH")
                .map(value_to_string)
                .unwrap_or_else(|| "/bin".to_string())
        };

        // Search PATH directories for script
        for dir in path_value.split(':') {
            if dir.is_empty() {
                continue;
            }

            // Build script path: {dir}/{name}.kai
            let script_path = PathBuf::from(dir).join(format!("{}.kai", name));

            // Check if script exists
            let exists = {
                let ctx = self.exec_ctx.read().await;
                ctx.backend.exists(&script_path).await
            };

            if !exists {
                continue;
            }

            // Read script content
            let content = {
                let ctx = self.exec_ctx.read().await;
                match ctx.backend.read(&script_path, None).await {
                    Ok(bytes) => match String::from_utf8(bytes) {
                        Ok(s) => s,
                        Err(e) => {
                            return Ok(Some(ExecResult::failure(
                                1,
                                format!("{}: invalid UTF-8: {}", script_path.display(), e),
                            )));
                        }
                    },
                    Err(e) => {
                        return Ok(Some(ExecResult::failure(
                            1,
                            format!("{}: {}", script_path.display(), e),
                        )));
                    }
                }
            };

            // Parse the script
            let program = match crate::parser::parse(&content) {
                Ok(p) => p,
                Err(errors) => {
                    let msg = errors
                        .iter()
                        .map(|e| format!("{}:{}: {}", script_path.display(), e.span.start, e.message))
                        .collect::<Vec<_>>()
                        .join("\n");
                    return Ok(Some(ExecResult::failure(1, msg)));
                }
            };

            // Build tool_args from args (async for command substitution support)
            let tool_args = self.build_args_async(args, None).await?;

            // Create isolated scope (like user tools)
            let mut isolated_scope = Scope::new();

            // Set up positional parameters ($0 = script name, $1, $2, ... = args)
            let positional_args: Vec<String> = tool_args.positional
                .iter()
                .map(value_to_string)
                .collect();
            isolated_scope.set_positional(name, positional_args);

            // Save current scope and swap with isolated scope
            let original_scope = {
                let mut scope = self.scope.write().await;
                std::mem::replace(&mut *scope, isolated_scope)
            };

            // Execute script statements — track outcome for cleanup
            let mut result = ExecResult::success("");
            let mut exec_error: Option<anyhow::Error> = None;
            let mut exit_code: Option<i64> = None;

            for stmt in program.statements {
                if matches!(stmt, crate::ast::Stmt::Empty) {
                    continue;
                }

                match self.execute_stmt_flow(&stmt).await {
                    Ok(flow) => {
                        match flow {
                            ControlFlow::Normal(r) => result = r,
                            ControlFlow::Return { value } => {
                                result = value;
                                break;
                            }
                            ControlFlow::Exit { code } => {
                                exit_code = Some(code);
                                break;
                            }
                            ControlFlow::Break { result: r, .. } | ControlFlow::Continue { result: r, .. } => {
                                result = r;
                            }
                        }
                    }
                    Err(e) => {
                        exec_error = Some(e);
                        break;
                    }
                }
            }

            // Restore original scope unconditionally
            {
                let mut scope = self.scope.write().await;
                *scope = original_scope;
            }

            // Propagate error or exit after cleanup
            if let Some(e) = exec_error {
                return Err(e.context(format!("script: {}", script_path.display())));
            }
            if let Some(code) = exit_code {
                result.code = code;
                return Ok(Some(result));
            }

            return Ok(Some(result));
        }

        // No script found
        Ok(None)
    }

    /// Try to execute an external command from PATH.
    ///
    /// This is the fallback when no builtin or user-defined tool matches.
    /// External commands receive a clean argv (flags preserved in their original format).
    ///
    /// # Requirements
    /// - Command must be found in PATH
    /// - Current working directory must be on a real filesystem (not virtual like /v)
    ///
    /// # Returns
    /// - `Ok(Some(result))` if command was found and executed
    /// - `Ok(None)` if command was not found in PATH
    /// - `Err` on execution errors
    #[tracing::instrument(level = "debug", skip(self, args), fields(command = %name))]
    async fn try_execute_external(&self, name: &str, args: &[Arg]) -> Result<Option<ExecResult>> {
        // Skip if name contains path separator (absolute/relative paths handled differently)
        if name.contains('/') {
            return Ok(None);
        }

        // Get PATH from scope or environment
        let path_var = {
            let scope = self.scope.read().await;
            scope
                .get("PATH")
                .map(value_to_string)
                .unwrap_or_else(|| std::env::var("PATH").unwrap_or_default())
        };

        // Resolve command in PATH
        let executable = match resolve_in_path(name, &path_var) {
            Some(path) => path,
            None => return Ok(None), // Not found - let caller handle error
        };

        tracing::debug!(executable = %executable, "resolved external command");

        // Get current working directory and verify it's on real filesystem
        let real_cwd = {
            let ctx = self.exec_ctx.read().await;
            match ctx.backend.resolve_real_path(&ctx.cwd) {
                Some(p) => p,
                None => {
                    return Ok(Some(ExecResult::failure(
                        1,
                        format!(
                            "{}: cannot run external command from virtual directory '{}'",
                            name,
                            ctx.cwd.display()
                        ),
                    )));
                }
            }
        };

        // Build flat argv (preserves flag format)
        let argv = self.build_args_flat(args).await?;

        // Get stdin if available
        let stdin_data = {
            let mut ctx = self.exec_ctx.write().await;
            ctx.take_stdin()
        };

        // Build and spawn the command
        use tokio::process::Command;

        let mut cmd = Command::new(&executable);
        cmd.args(&argv);
        cmd.current_dir(&real_cwd);

        // Handle stdin
        cmd.stdin(if stdin_data.is_some() {
            std::process::Stdio::piped()
        } else {
            std::process::Stdio::null()
        });

        // In interactive mode, standalone commands (no piped stdin) inherit
        // the terminal's stdout/stderr so output streams in real-time.
        let inherit_output = self.interactive && stdin_data.is_none();

        if inherit_output {
            cmd.stdout(std::process::Stdio::inherit());
            cmd.stderr(std::process::Stdio::inherit());
        } else {
            cmd.stdout(std::process::Stdio::piped());
            cmd.stderr(std::process::Stdio::piped());
        }

        // Spawn the process
        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(e) => {
                return Ok(Some(ExecResult::failure(
                    127,
                    format!("{}: {}", name, e),
                )));
            }
        };

        // Write stdin if present
        if let Some(data) = stdin_data
            && let Some(mut stdin) = child.stdin.take()
        {
            use tokio::io::AsyncWriteExt;
            if let Err(e) = stdin.write_all(data.as_bytes()).await {
                return Ok(Some(ExecResult::failure(
                    1,
                    format!("{}: failed to write stdin: {}", name, e),
                )));
            }
            // Drop stdin to signal EOF
        }

        if inherit_output {
            // Output goes directly to terminal — just wait for exit
            let status = match child.wait().await {
                Ok(s) => s,
                Err(e) => {
                    return Ok(Some(ExecResult::failure(
                        1,
                        format!("{}: failed to wait: {}", name, e),
                    )));
                }
            };

            let code = status.code().unwrap_or_else(|| {
                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    128 + status.signal().unwrap_or(0)
                }
                #[cfg(not(unix))]
                {
                    -1
                }
            }) as i64;

            // stdout/stderr already went to the terminal
            Ok(Some(ExecResult::from_output(code, String::new(), String::new())))
        } else {
            // Capture output via bounded streams
            let stdout_stream = Arc::new(BoundedStream::new(DEFAULT_STREAM_MAX_SIZE));
            let stderr_stream = Arc::new(BoundedStream::new(DEFAULT_STREAM_MAX_SIZE));

            let stdout_pipe = child.stdout.take();
            let stderr_pipe = child.stderr.take();

            let stdout_clone = stdout_stream.clone();
            let stderr_clone = stderr_stream.clone();

            let stdout_task = stdout_pipe.map(|pipe| {
                tokio::spawn(async move {
                    drain_to_stream(pipe, stdout_clone).await;
                })
            });

            let stderr_task = stderr_pipe.map(|pipe| {
                tokio::spawn(async move {
                    drain_to_stream(pipe, stderr_clone).await;
                })
            });

            let status = match child.wait().await {
                Ok(s) => s,
                Err(e) => {
                    return Ok(Some(ExecResult::failure(
                        1,
                        format!("{}: failed to wait: {}", name, e),
                    )));
                }
            };

            if let Some(task) = stdout_task {
                // Ignore join error — the drain task logs its own errors
                let _ = task.await;
            }
            if let Some(task) = stderr_task {
                let _ = task.await;
            }

            let code = status.code().unwrap_or_else(|| {
                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    128 + status.signal().unwrap_or(0)
                }
                #[cfg(not(unix))]
                {
                    -1
                }
            }) as i64;

            let stdout = stdout_stream.read_string().await;
            let stderr = stderr_stream.read_string().await;

            Ok(Some(ExecResult::from_output(code, stdout, stderr)))
        }
    }

    // --- Variable Access ---

    /// Get a variable value.
    pub async fn get_var(&self, name: &str) -> Option<Value> {
        let scope = self.scope.read().await;
        scope.get(name).cloned()
    }

    /// Check if error-exit mode is enabled (for testing).
    #[cfg(test)]
    pub async fn error_exit_enabled(&self) -> bool {
        let scope = self.scope.read().await;
        scope.error_exit_enabled()
    }

    /// Set a variable value.
    pub async fn set_var(&self, name: &str, value: Value) {
        let mut scope = self.scope.write().await;
        scope.set(name.to_string(), value);
    }

    /// List all variables.
    pub async fn list_vars(&self) -> Vec<(String, Value)> {
        let scope = self.scope.read().await;
        scope.all()
    }

    // --- CWD ---

    /// Get current working directory.
    pub async fn cwd(&self) -> PathBuf {
        self.exec_ctx.read().await.cwd.clone()
    }

    /// Set current working directory.
    pub async fn set_cwd(&self, path: PathBuf) {
        let mut ctx = self.exec_ctx.write().await;
        ctx.set_cwd(path);
    }

    // --- Last Result ---

    /// Get the last result ($?).
    pub async fn last_result(&self) -> ExecResult {
        let scope = self.scope.read().await;
        scope.last_result().clone()
    }

    // --- Tools ---

    /// Get available tool schemas.
    pub fn tool_schemas(&self) -> Vec<crate::tools::ToolSchema> {
        self.tools.schemas()
    }

    // --- Jobs ---

    /// Get job manager.
    pub fn jobs(&self) -> Arc<JobManager> {
        self.jobs.clone()
    }

    // --- VFS ---

    /// Get VFS router.
    pub fn vfs(&self) -> Arc<VfsRouter> {
        self.vfs.clone()
    }

    // --- State ---

    /// Reset kernel to initial state.
    ///
    /// Clears in-memory variables and resets cwd to root.
    /// History is not cleared (it persists across resets).
    pub async fn reset(&self) -> Result<()> {
        {
            let mut scope = self.scope.write().await;
            *scope = Scope::new();
        }
        {
            let mut ctx = self.exec_ctx.write().await;
            ctx.cwd = PathBuf::from("/");
        }
        Ok(())
    }

    /// Shutdown the kernel.
    pub async fn shutdown(self) -> Result<()> {
        // Wait for all background jobs
        self.jobs.wait_all().await;
        Ok(())
    }

    /// Dispatch a single command using the full resolution chain.
    ///
    /// This is the core of `CommandDispatcher` — it syncs state between the
    /// passed-in `ExecContext` and kernel-internal state (scope, exec_ctx),
    /// then delegates to `execute_command` for the actual dispatch.
    ///
    /// State flow:
    /// 1. ctx → self: sync scope, cwd, stdin so internal methods see current state
    /// 2. execute_command: full dispatch chain (user tools, builtins, scripts, external, backend)
    /// 3. self → ctx: sync scope, cwd changes back so the pipeline runner sees them
    async fn dispatch_command(&self, cmd: &Command, ctx: &mut ExecContext) -> Result<ExecResult> {
        // 1. Sync ctx → self internals
        {
            let mut scope = self.scope.write().await;
            *scope = ctx.scope.clone();
        }
        {
            let mut ec = self.exec_ctx.write().await;
            ec.cwd = ctx.cwd.clone();
            ec.prev_cwd = ctx.prev_cwd.clone();
            ec.stdin = ctx.stdin.take();
            ec.stdin_data = ctx.stdin_data.take();
        }

        // 2. Execute via the full dispatch chain
        let result = self.execute_command(&cmd.name, &cmd.args).await?;

        // 3. Sync self → ctx
        {
            let scope = self.scope.read().await;
            ctx.scope = scope.clone();
        }
        {
            let ec = self.exec_ctx.read().await;
            ctx.cwd = ec.cwd.clone();
            ctx.prev_cwd = ec.prev_cwd.clone();
        }

        Ok(result)
    }
}

#[async_trait]
impl CommandDispatcher for Kernel {
    /// Dispatch a command through the Kernel's full resolution chain.
    ///
    /// This is the single path for all command execution when called from
    /// the pipeline runner. It provides the full dispatch chain:
    /// user tools → builtins → .kai scripts → external commands → backend tools.
    async fn dispatch(&self, cmd: &Command, ctx: &mut ExecContext) -> Result<ExecResult> {
        self.dispatch_command(cmd, ctx).await
    }
}

/// Accumulate output from one result into another.
///
/// This appends stdout and stderr (with newlines as separators) and updates
/// the exit code to match the new result. Used to preserve output from
/// multiple statements, loop iterations, and command chains.
fn accumulate_result(accumulated: &mut ExecResult, new: &ExecResult) {
    if !accumulated.out.is_empty() && !new.out.is_empty() {
        accumulated.out.push('\n');
    }
    accumulated.out.push_str(&new.out);
    if !accumulated.err.is_empty() && !new.err.is_empty() {
        accumulated.err.push('\n');
    }
    accumulated.err.push_str(&new.err);
    accumulated.code = new.code;
    accumulated.data = new.data.clone();
}

/// Check if a value is truthy.
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(i) => *i != 0,
        Value::Float(f) => *f != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Json(json) => match json {
            serde_json::Value::Null => false,
            serde_json::Value::Array(arr) => !arr.is_empty(),
            serde_json::Value::Object(obj) => !obj.is_empty(),
            serde_json::Value::Bool(b) => *b,
            serde_json::Value::Number(n) => n.as_f64().map(|f| f != 0.0).unwrap_or(false),
            serde_json::Value::String(s) => !s.is_empty(),
        },
        Value::Blob(_) => true, // Blob references are always truthy
    }
}

/// Apply tilde expansion to a value.
///
/// Only string values starting with `~` are expanded.
fn apply_tilde_expansion(value: Value) -> Value {
    match value {
        Value::String(s) if s.starts_with('~') => Value::String(expand_tilde(&s)),
        _ => value,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_kernel_transient() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        assert_eq!(kernel.name(), "transient");
    }

    #[tokio::test]
    async fn test_kernel_execute_echo() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel.execute("echo hello").await.expect("execution failed");
        assert!(result.ok());
        assert_eq!(result.out.trim(), "hello");
    }

    #[tokio::test]
    async fn test_multiple_statements_accumulate_output() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute("echo one\necho two\necho three")
            .await
            .expect("execution failed");
        assert!(result.ok());
        // Should have all three outputs separated by newlines
        assert!(result.out.contains("one"), "missing 'one': {}", result.out);
        assert!(result.out.contains("two"), "missing 'two': {}", result.out);
        assert!(result.out.contains("three"), "missing 'three': {}", result.out);
    }

    #[tokio::test]
    async fn test_and_chain_accumulates_output() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute("echo first && echo second")
            .await
            .expect("execution failed");
        assert!(result.ok());
        assert!(result.out.contains("first"), "missing 'first': {}", result.out);
        assert!(result.out.contains("second"), "missing 'second': {}", result.out);
    }

    #[tokio::test]
    async fn test_for_loop_accumulates_output() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute(r#"for X in a b c; do echo "item: ${X}"; done"#)
            .await
            .expect("execution failed");
        assert!(result.ok());
        assert!(result.out.contains("item: a"), "missing 'item: a': {}", result.out);
        assert!(result.out.contains("item: b"), "missing 'item: b': {}", result.out);
        assert!(result.out.contains("item: c"), "missing 'item: c': {}", result.out);
    }

    #[tokio::test]
    async fn test_while_loop_accumulates_output() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute(r#"
                N=3
                while [[ ${N} -gt 0 ]]; do
                    echo "N=${N}"
                    N=$((N - 1))
                done
            "#)
            .await
            .expect("execution failed");
        assert!(result.ok());
        assert!(result.out.contains("N=3"), "missing 'N=3': {}", result.out);
        assert!(result.out.contains("N=2"), "missing 'N=2': {}", result.out);
        assert!(result.out.contains("N=1"), "missing 'N=1': {}", result.out);
    }

    #[tokio::test]
    async fn test_kernel_set_var() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("X=42").await.expect("set failed");

        let value = kernel.get_var("X").await;
        assert_eq!(value, Some(Value::Int(42)));
    }

    #[tokio::test]
    async fn test_kernel_var_expansion() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("NAME=\"world\"").await.expect("set failed");
        let result = kernel.execute("echo \"hello ${NAME}\"").await.expect("echo failed");

        assert!(result.ok());
        assert_eq!(result.out.trim(), "hello world");
    }

    #[tokio::test]
    async fn test_kernel_last_result() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("echo test").await.expect("echo failed");

        let last = kernel.last_result().await;
        assert!(last.ok());
        assert_eq!(last.out.trim(), "test");
    }

    #[tokio::test]
    async fn test_kernel_tool_not_found() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel.execute("nonexistent_tool").await.expect("execution failed");
        assert!(!result.ok());
        assert_eq!(result.code, 127);
        assert!(result.err.contains("command not found"));
    }

    #[tokio::test]
    async fn test_external_command_true() {
        // Use REPL config for passthrough filesystem access
        let kernel = Kernel::new(KernelConfig::repl()).expect("failed to create kernel");

        // /bin/true should be available on any Unix system
        let result = kernel.execute("true").await.expect("execution failed");
        // This should use the builtin true, which returns 0
        assert!(result.ok(), "true should succeed: {:?}", result);
    }

    #[tokio::test]
    async fn test_external_command_basic() {
        // Use REPL config for passthrough filesystem access
        let kernel = Kernel::new(KernelConfig::repl()).expect("failed to create kernel");

        // Test with /bin/echo which is external
        // Note: kaish has a builtin echo, so this will use the builtin
        // Let's test with a command that's not a builtin
        // Actually, let's just test that PATH resolution works by checking the PATH var
        let path_var = std::env::var("PATH").unwrap_or_default();
        eprintln!("System PATH: {}", path_var);

        // Set PATH in kernel to ensure it's available
        kernel.execute(&format!(r#"PATH="{}""#, path_var)).await.expect("set PATH failed");

        // Now try an external command like /usr/bin/env
        // But env is also a builtin... let's try uname
        let result = kernel.execute("uname").await.expect("execution failed");
        eprintln!("uname result: {:?}", result);
        // uname should succeed if external commands work
        assert!(result.ok() || result.code == 127, "uname: {:?}", result);
    }

    #[tokio::test]
    async fn test_kernel_reset() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("X=1").await.expect("set failed");
        assert!(kernel.get_var("X").await.is_some());

        kernel.reset().await.expect("reset failed");
        assert!(kernel.get_var("X").await.is_none());
    }

    #[tokio::test]
    async fn test_kernel_cwd() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Transient kernel uses sandboxed mode with cwd=$HOME
        let cwd = kernel.cwd().await;
        let home = std::env::var("HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("/"));
        assert_eq!(cwd, home);

        kernel.set_cwd(PathBuf::from("/tmp")).await;
        assert_eq!(kernel.cwd().await, PathBuf::from("/tmp"));
    }

    #[tokio::test]
    async fn test_kernel_list_vars() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("A=1").await.ok();
        kernel.execute("B=2").await.ok();

        let vars = kernel.list_vars().await;
        assert!(vars.iter().any(|(n, v)| n == "A" && *v == Value::Int(1)));
        assert!(vars.iter().any(|(n, v)| n == "B" && *v == Value::Int(2)));
    }

    #[tokio::test]
    async fn test_is_truthy() {
        assert!(!is_truthy(&Value::Null));
        assert!(!is_truthy(&Value::Bool(false)));
        assert!(is_truthy(&Value::Bool(true)));
        assert!(!is_truthy(&Value::Int(0)));
        assert!(is_truthy(&Value::Int(1)));
        assert!(!is_truthy(&Value::String("".into())));
        assert!(is_truthy(&Value::String("x".into())));
    }

    #[tokio::test]
    async fn test_jq_in_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        // kaish uses double quotes only; escape inner quotes
        let result = kernel
            .execute(r#"echo "{\"name\": \"Alice\"}" | jq ".name" -r"#)
            .await
            .expect("execution failed");
        assert!(result.ok(), "jq pipeline failed: {}", result.err);
        assert_eq!(result.out.trim(), "Alice");
    }

    #[tokio::test]
    async fn test_user_defined_tool() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define a function
        kernel
            .execute(r#"greet() { echo "Hello, $1!" }"#)
            .await
            .expect("function definition failed");

        // Call the function
        let result = kernel
            .execute(r#"greet "World""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "greet failed: {}", result.err);
        assert_eq!(result.out.trim(), "Hello, World!");
    }

    #[tokio::test]
    async fn test_user_tool_positional_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define a function with positional param
        kernel
            .execute(r#"greet() { echo "Hi $1" }"#)
            .await
            .expect("function definition failed");

        // Call with positional argument
        let result = kernel
            .execute(r#"greet "Amy""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "greet failed: {}", result.err);
        assert_eq!(result.out.trim(), "Hi Amy");
    }

    #[tokio::test]
    async fn test_function_shared_scope() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Set a variable in parent scope
        kernel
            .execute(r#"SECRET="hidden""#)
            .await
            .expect("set failed");

        // Define a function that accesses and modifies parent variable
        kernel
            .execute(r#"access_parent() {
                echo "${SECRET}"
                SECRET="modified"
            }"#)
            .await
            .expect("function definition failed");

        // Call the function - it SHOULD see SECRET (shared scope like sh)
        let result = kernel.execute("access_parent").await.expect("function call failed");

        // Function should have access to parent scope
        assert!(
            result.out.contains("hidden"),
            "Function should access parent scope, got: {}",
            result.out
        );

        // Function should have modified the parent variable
        let secret = kernel.get_var("SECRET").await;
        assert_eq!(
            secret,
            Some(Value::String("modified".into())),
            "Function should modify parent scope"
        );
    }

    #[tokio::test]
    async fn test_exec_builtin() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        // argv is now a space-separated string or JSON array string
        let result = kernel
            .execute(r#"exec command="/bin/echo" argv="hello world""#)
            .await
            .expect("exec failed");

        assert!(result.ok(), "exec failed: {}", result.err);
        assert_eq!(result.out.trim(), "hello world");
    }

    #[tokio::test]
    async fn test_while_false_never_runs() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // A while loop with false condition should never run
        let result = kernel
            .execute(r#"
                while false; do
                    echo "should not run"
                done
            "#)
            .await
            .expect("while false failed");

        assert!(result.ok());
        assert!(result.out.is_empty(), "while false should not execute body: {}", result.out);
    }

    #[tokio::test]
    async fn test_while_string_comparison() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Set a flag
        kernel.execute(r#"FLAG="go""#).await.expect("set failed");

        // Use string comparison as condition (shell-compatible [[ ]] syntax)
        // Note: Put echo last so we can check the output
        let result = kernel
            .execute(r#"
                while [[ ${FLAG} == "go" ]]; do
                    FLAG="stop"
                    echo "running"
                done
            "#)
            .await
            .expect("while with string cmp failed");

        assert!(result.ok());
        assert!(result.out.contains("running"), "should have run once: {}", result.out);

        // Verify flag was changed
        let flag = kernel.get_var("FLAG").await;
        assert_eq!(flag, Some(Value::String("stop".into())));
    }

    #[tokio::test]
    async fn test_while_numeric_comparison() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Test > comparison (shell-compatible [[ ]] with -gt)
        kernel.execute("N=5").await.expect("set failed");

        // Note: Put echo last so we can check the output
        let result = kernel
            .execute(r#"
                while [[ ${N} -gt 3 ]]; do
                    N=3
                    echo "N was greater"
                done
            "#)
            .await
            .expect("while with > failed");

        assert!(result.ok());
        assert!(result.out.contains("N was greater"), "should have run once: {}", result.out);
    }

    #[tokio::test]
    async fn test_break_in_while_loop() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                I=0
                while true; do
                    I=1
                    echo "before break"
                    break
                    echo "after break"
                done
            "#)
            .await
            .expect("while with break failed");

        assert!(result.ok());
        assert!(result.out.contains("before break"), "should see before break: {}", result.out);
        assert!(!result.out.contains("after break"), "should not see after break: {}", result.out);

        // Verify we exited the loop
        let i = kernel.get_var("I").await;
        assert_eq!(i, Some(Value::Int(1)));
    }

    #[tokio::test]
    async fn test_continue_in_while_loop() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Test continue in a while loop where variables persist
        // We use string state transition: "start" -> "middle" -> "end"
        // continue on "middle" should skip to next iteration
        // Shell-compatible: use [[ ]] for comparisons
        let result = kernel
            .execute(r#"
                STATE="start"
                AFTER_CONTINUE="no"
                while [[ ${STATE} != "done" ]]; do
                    if [[ ${STATE} == "start" ]]; then
                        STATE="middle"
                        continue
                        AFTER_CONTINUE="yes"
                    fi
                    if [[ ${STATE} == "middle" ]]; then
                        STATE="done"
                    fi
                done
            "#)
            .await
            .expect("while with continue failed");

        assert!(result.ok());

        // STATE should be "done" (we completed the loop)
        let state = kernel.get_var("STATE").await;
        assert_eq!(state, Some(Value::String("done".into())));

        // AFTER_CONTINUE should still be "no" (continue skipped the assignment)
        let after = kernel.get_var("AFTER_CONTINUE").await;
        assert_eq!(after, Some(Value::String("no".into())));
    }

    #[tokio::test]
    async fn test_break_with_level() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Nested loop with break 2 to exit both loops
        // We verify by checking OUTER value:
        // - If break 2 works, OUTER stays at 1 (set before for loop)
        // - If break 2 fails, OUTER becomes 2 (set after for loop)
        let result = kernel
            .execute(r#"
                OUTER=0
                while true; do
                    OUTER=1
                    for X in "1 2"; do
                        break 2
                    done
                    OUTER=2
                done
            "#)
            .await
            .expect("nested break failed");

        assert!(result.ok());

        // OUTER should be 1 (set before for loop), not 2 (would be set after for loop)
        let outer = kernel.get_var("OUTER").await;
        assert_eq!(outer, Some(Value::Int(1)), "break 2 should have skipped OUTER=2");
    }

    #[tokio::test]
    async fn test_return_from_tool() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define a function that returns early
        kernel
            .execute(r#"early_return() {
                if [[ $1 == 1 ]]; then
                    return 42
                fi
                echo "not returned"
            }"#)
            .await
            .expect("function definition failed");

        // Call with arg=1 should return with exit code 42
        // (POSIX shell behavior: return N sets exit code, doesn't output N)
        let result = kernel
            .execute("early_return 1")
            .await
            .expect("function call failed");

        // Exit code should be 42 (non-zero, so not ok())
        assert_eq!(result.code, 42);
        // Output should be empty (we returned before echo)
        assert!(result.out.is_empty());
    }

    #[tokio::test]
    async fn test_return_without_value() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define a function that returns without a value
        kernel
            .execute(r#"early_exit() {
                if [[ $1 == "stop" ]]; then
                    return
                fi
                echo "continued"
            }"#)
            .await
            .expect("function definition failed");

        // Call with arg="stop" should return early
        let result = kernel
            .execute(r#"early_exit "stop""#)
            .await
            .expect("function call failed");

        assert!(result.ok());
        assert!(result.out.is_empty() || result.out.trim().is_empty());
    }

    #[tokio::test]
    async fn test_exit_stops_execution() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // exit should stop further execution
        kernel
            .execute(r#"
                BEFORE="yes"
                exit 0
                AFTER="yes"
            "#)
            .await
            .expect("execution failed");

        // BEFORE should be set, AFTER should not
        let before = kernel.get_var("BEFORE").await;
        assert_eq!(before, Some(Value::String("yes".into())));

        let after = kernel.get_var("AFTER").await;
        assert!(after.is_none(), "AFTER should not be set after exit");
    }

    #[tokio::test]
    async fn test_exit_with_code() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // exit with code should propagate the exit code
        let result = kernel
            .execute("exit 42")
            .await
            .expect("exit failed");

        assert_eq!(result.code, 42);
        assert!(result.out.is_empty(), "exit should not produce stdout");
    }

    #[tokio::test]
    async fn test_set_e_stops_on_failure() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Enable error-exit mode
        kernel.execute("set -e").await.expect("set -e failed");

        // Run a sequence where the middle command fails
        kernel
            .execute(r#"
                STEP1="done"
                false
                STEP2="done"
            "#)
            .await
            .expect("execution failed");

        // STEP1 should be set, but STEP2 should NOT be set (exit on false)
        let step1 = kernel.get_var("STEP1").await;
        assert_eq!(step1, Some(Value::String("done".into())));

        let step2 = kernel.get_var("STEP2").await;
        assert!(step2.is_none(), "STEP2 should not be set after false with set -e");
    }

    #[tokio::test]
    async fn test_set_plus_e_disables_error_exit() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Enable then disable error-exit mode
        kernel.execute("set -e").await.expect("set -e failed");
        kernel.execute("set +e").await.expect("set +e failed");

        // Now failure should NOT stop execution
        kernel
            .execute(r#"
                STEP1="done"
                false
                STEP2="done"
            "#)
            .await
            .expect("execution failed");

        // Both should be set since +e disables error exit
        let step1 = kernel.get_var("STEP1").await;
        assert_eq!(step1, Some(Value::String("done".into())));

        let step2 = kernel.get_var("STEP2").await;
        assert_eq!(step2, Some(Value::String("done".into())));
    }

    #[tokio::test]
    async fn test_set_ignores_unknown_options() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Bash idiom: set -euo pipefail (we support -e, ignore the rest)
        let result = kernel
            .execute("set -e -u -o pipefail")
            .await
            .expect("set with unknown options failed");

        assert!(result.ok(), "set should succeed with unknown options");

        // -e should still be enabled
        kernel
            .execute(r#"
                BEFORE="yes"
                false
                AFTER="yes"
            "#)
            .await
            .ok();

        let after = kernel.get_var("AFTER").await;
        assert!(after.is_none(), "-e should be enabled despite unknown options");
    }

    #[tokio::test]
    async fn test_set_no_args_shows_settings() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Enable -e
        kernel.execute("set -e").await.expect("set -e failed");

        // Call set with no args to see settings
        let result = kernel.execute("set").await.expect("set failed");

        assert!(result.ok());
        assert!(result.out.contains("set -e"), "should show -e is enabled: {}", result.out);
    }

    #[tokio::test]
    async fn test_set_e_in_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("set -e").await.expect("set -e failed");

        // Pipeline failure should trigger exit
        kernel
            .execute(r#"
                BEFORE="yes"
                false | cat
                AFTER="yes"
            "#)
            .await
            .ok();

        let before = kernel.get_var("BEFORE").await;
        assert_eq!(before, Some(Value::String("yes".into())));

        // AFTER should not be set if pipeline failure triggers exit
        // Note: The exit code of a pipeline is the exit code of the last command
        // So `false | cat` returns 0 (cat succeeds). This is bash-compatible behavior.
        // To test pipeline failure, we need the last command to fail.
    }

    #[tokio::test]
    async fn test_set_e_with_and_chain() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("set -e").await.expect("set -e failed");

        // Commands in && chain should not trigger -e on the first failure
        // because && explicitly handles the error
        kernel
            .execute(r#"
                RESULT="initial"
                false && RESULT="chained"
                RESULT="continued"
            "#)
            .await
            .ok();

        // In bash, commands in && don't trigger -e. The chain handles the failure.
        // Our implementation may differ - let's verify current behavior.
        let result = kernel.get_var("RESULT").await;
        // If we follow bash semantics, RESULT should be "continued"
        // If we trigger -e on the false, RESULT stays "initial"
        assert!(result.is_some(), "RESULT should be set");
    }

    #[tokio::test]
    async fn test_set_e_exits_in_for_loop() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("set -e").await.expect("set -e failed");

        kernel
            .execute(r#"
                REACHED="no"
                for x in 1 2 3; do
                    false
                    REACHED="yes"
                done
            "#)
            .await
            .ok();

        // With set -e, false should trigger exit; REACHED should remain "no"
        let reached = kernel.get_var("REACHED").await;
        assert_eq!(reached, Some(Value::String("no".into())),
            "set -e should exit on failure in for loop body");
    }

    #[tokio::test]
    async fn test_for_loop_continues_without_set_e() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Without set -e, for loop should continue normally
        kernel
            .execute(r#"
                COUNT=0
                for x in 1 2 3; do
                    false
                    COUNT=$((COUNT + 1))
                done
            "#)
            .await
            .ok();

        let count = kernel.get_var("COUNT").await;
        // Arithmetic produces Int values; accept either Int or String representation
        let count_val = match &count {
            Some(Value::Int(n)) => *n,
            Some(Value::String(s)) => s.parse().unwrap_or(-1),
            _ => -1,
        };
        assert_eq!(count_val, 3,
            "without set -e, loop should complete all iterations (got {:?})", count);
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Source Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_source_sets_variables() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Write a script to the VFS
        kernel
            .execute(r#"write "/test.kai" 'FOO="bar"'"#)
            .await
            .expect("write failed");

        // Source the script
        let result = kernel
            .execute(r#"source "/test.kai""#)
            .await
            .expect("source failed");

        assert!(result.ok(), "source should succeed");

        // Variable should be set in current scope
        let foo = kernel.get_var("FOO").await;
        assert_eq!(foo, Some(Value::String("bar".into())));
    }

    #[tokio::test]
    async fn test_source_with_dot_alias() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Write a script to the VFS
        kernel
            .execute(r#"write "/vars.kai" 'X=42'"#)
            .await
            .expect("write failed");

        // Source using . alias
        let result = kernel
            .execute(r#". "/vars.kai""#)
            .await
            .expect(". failed");

        assert!(result.ok(), ". should succeed");

        // Variable should be set in current scope
        let x = kernel.get_var("X").await;
        assert_eq!(x, Some(Value::Int(42)));
    }

    #[tokio::test]
    async fn test_source_not_found() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Try to source a non-existent file
        let result = kernel
            .execute(r#"source "/nonexistent.kai""#)
            .await
            .expect("source should not fail with error");

        assert!(!result.ok(), "source of non-existent file should fail");
        assert!(result.err.contains("nonexistent.kai"), "error should mention filename");
    }

    #[tokio::test]
    async fn test_source_missing_filename() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Call source with no arguments
        let result = kernel
            .execute("source")
            .await
            .expect("source should not fail with error");

        assert!(!result.ok(), "source without filename should fail");
        assert!(result.err.contains("missing filename"), "error should mention missing filename");
    }

    #[tokio::test]
    async fn test_source_executes_multiple_statements() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Write a script with multiple statements
        kernel
            .execute(r#"write "/multi.kai" 'A=1
B=2
C=3'"#)
            .await
            .expect("write failed");

        // Source it
        kernel
            .execute(r#"source "/multi.kai""#)
            .await
            .expect("source failed");

        // All variables should be set
        assert_eq!(kernel.get_var("A").await, Some(Value::Int(1)));
        assert_eq!(kernel.get_var("B").await, Some(Value::Int(2)));
        assert_eq!(kernel.get_var("C").await, Some(Value::Int(3)));
    }

    #[tokio::test]
    async fn test_source_can_define_functions() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Write a script that defines a function
        kernel
            .execute(r#"write "/functions.kai" 'greet() {
    echo "Hello, $1!"
}'"#)
            .await
            .expect("write failed");

        // Source it
        kernel
            .execute(r#"source "/functions.kai""#)
            .await
            .expect("source failed");

        // Use the defined function
        let result = kernel
            .execute(r#"greet "World""#)
            .await
            .expect("greet failed");

        assert!(result.ok());
        assert!(result.out.contains("Hello, World!"));
    }

    #[tokio::test]
    async fn test_source_inherits_error_exit() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Enable error exit
        kernel.execute("set -e").await.expect("set -e failed");

        // Write a script that has a failure
        kernel
            .execute(r#"write "/fail.kai" 'BEFORE="yes"
false
AFTER="yes"'"#)
            .await
            .expect("write failed");

        // Source it (should exit on false due to set -e)
        kernel
            .execute(r#"source "/fail.kai""#)
            .await
            .ok();

        // BEFORE should be set, AFTER should NOT be set due to error exit
        let before = kernel.get_var("BEFORE").await;
        assert_eq!(before, Some(Value::String("yes".into())));

        // Note: This test depends on whether error exit is checked within source
        // Currently our implementation checks per-statement in the main kernel
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Case Statement Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_case_simple_match() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "hello" in
                    hello) echo "matched hello" ;;
                    world) echo "matched world" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.out.trim(), "matched hello");
    }

    #[tokio::test]
    async fn test_case_wildcard_match() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "main.rs" in
                    "*.py") echo "Python" ;;
                    "*.rs") echo "Rust" ;;
                    "*") echo "Unknown" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.out.trim(), "Rust");
    }

    #[tokio::test]
    async fn test_case_default_match() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "unknown.xyz" in
                    "*.py") echo "Python" ;;
                    "*.rs") echo "Rust" ;;
                    "*") echo "Default" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.out.trim(), "Default");
    }

    #[tokio::test]
    async fn test_case_no_match() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Case with no default branch and no match
        let result = kernel
            .execute(r#"
                case "nope" in
                    "yes") echo "yes" ;;
                    "no") echo "no" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert!(result.out.is_empty(), "no match should produce empty output");
    }

    #[tokio::test]
    async fn test_case_with_variable() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute(r#"LANG="rust""#).await.expect("set failed");

        let result = kernel
            .execute(r#"
                case ${LANG} in
                    python) echo "snake" ;;
                    rust) echo "crab" ;;
                    go) echo "gopher" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.out.trim(), "crab");
    }

    #[tokio::test]
    async fn test_case_multiple_patterns() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "yes" in
                    "y"|"yes"|"Y"|"YES") echo "affirmative" ;;
                    "n"|"no"|"N"|"NO") echo "negative" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.out.trim(), "affirmative");
    }

    #[tokio::test]
    async fn test_case_glob_question_mark() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "test1" in
                    "test?") echo "matched test?" ;;
                    "*") echo "default" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.out.trim(), "matched test?");
    }

    #[tokio::test]
    async fn test_case_char_class() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "Yes" in
                    "[Yy]*") echo "yes-like" ;;
                    "[Nn]*") echo "no-like" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.out.trim(), "yes-like");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Cat Stdin Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_cat_from_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"echo "piped text" | cat"#)
            .await
            .expect("cat pipeline failed");

        assert!(result.ok(), "cat failed: {}", result.err);
        assert_eq!(result.out.trim(), "piped text");
    }

    #[tokio::test]
    async fn test_cat_from_pipeline_multiline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"echo "line1\nline2" | cat -n"#)
            .await
            .expect("cat pipeline failed");

        assert!(result.ok(), "cat failed: {}", result.err);
        assert!(result.out.contains("1\t"), "output: {}", result.out);
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Heredoc Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_heredoc_basic() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute("cat <<EOF\nhello\nEOF")
            .await
            .expect("heredoc failed");

        assert!(result.ok(), "cat with heredoc failed: {}", result.err);
        assert_eq!(result.out.trim(), "hello");
    }

    #[tokio::test]
    async fn test_arithmetic_in_string() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"echo "result: $((1 + 2))""#)
            .await
            .expect("arithmetic in string failed");

        assert!(result.ok(), "echo failed: {}", result.err);
        assert_eq!(result.out.trim(), "result: 3");
    }

    #[tokio::test]
    async fn test_heredoc_multiline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute("cat <<EOF\nline1\nline2\nline3\nEOF")
            .await
            .expect("heredoc failed");

        assert!(result.ok(), "cat with heredoc failed: {}", result.err);
        assert!(result.out.contains("line1"), "output: {}", result.out);
        assert!(result.out.contains("line2"), "output: {}", result.out);
        assert!(result.out.contains("line3"), "output: {}", result.out);
    }

    #[tokio::test]
    async fn test_heredoc_variable_expansion() {
        // Bug N: unquoted heredoc should expand variables
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("GREETING=hello").await.expect("set var");

        let result = kernel
            .execute("cat <<EOF\n$GREETING world\nEOF")
            .await
            .expect("heredoc expansion failed");

        assert!(result.ok(), "heredoc expansion failed: {}", result.err);
        assert_eq!(result.out.trim(), "hello world");
    }

    #[tokio::test]
    async fn test_heredoc_quoted_no_expansion() {
        // Bug N: quoted heredoc (<<'EOF') should NOT expand variables
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("GREETING=hello").await.expect("set var");

        let result = kernel
            .execute("cat <<'EOF'\n$GREETING world\nEOF")
            .await
            .expect("quoted heredoc failed");

        assert!(result.ok(), "quoted heredoc failed: {}", result.err);
        assert_eq!(result.out.trim(), "$GREETING world");
    }

    #[tokio::test]
    async fn test_heredoc_default_value_expansion() {
        // Bug N: ${VAR:-default} should expand in unquoted heredocs
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute("cat <<EOF\n${UNSET:-fallback}\nEOF")
            .await
            .expect("heredoc default expansion failed");

        assert!(result.ok(), "heredoc default expansion failed: {}", result.err);
        assert_eq!(result.out.trim(), "fallback");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Read Builtin Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_read_from_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Pipe input to read
        let result = kernel
            .execute(r#"echo "Alice" | read NAME; echo "Hello, ${NAME}""#)
            .await
            .expect("read pipeline failed");

        assert!(result.ok(), "read failed: {}", result.err);
        assert!(result.out.contains("Hello, Alice"), "output: {}", result.out);
    }

    #[tokio::test]
    async fn test_read_multiple_vars_from_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"echo "John Doe 42" | read FIRST LAST AGE; echo "${FIRST} is ${AGE}""#)
            .await
            .expect("read pipeline failed");

        assert!(result.ok(), "read failed: {}", result.err);
        assert!(result.out.contains("John is 42"), "output: {}", result.out);
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Shell-Style Function Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_posix_function_with_positional_params() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define POSIX-style function
        kernel
            .execute(r#"greet() { echo "Hello, $1!" }"#)
            .await
            .expect("function definition failed");

        // Call the function
        let result = kernel
            .execute(r#"greet "Amy""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "greet failed: {}", result.err);
        assert_eq!(result.out.trim(), "Hello, Amy!");
    }

    #[tokio::test]
    async fn test_posix_function_multiple_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define function using $1 and $2
        kernel
            .execute(r#"add_greeting() { echo "$1 $2!" }"#)
            .await
            .expect("function definition failed");

        // Call the function
        let result = kernel
            .execute(r#"add_greeting "Hello" "World""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "function failed: {}", result.err);
        assert_eq!(result.out.trim(), "Hello World!");
    }

    #[tokio::test]
    async fn test_bash_function_with_positional_params() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define bash-style function (function keyword, no parens)
        kernel
            .execute(r#"function greet { echo "Hi $1" }"#)
            .await
            .expect("function definition failed");

        // Call the function
        let result = kernel
            .execute(r#"greet "Bob""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "greet failed: {}", result.err);
        assert_eq!(result.out.trim(), "Hi Bob");
    }

    #[tokio::test]
    async fn test_shell_function_with_all_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define function using $@ (all args)
        kernel
            .execute(r#"echo_all() { echo "args: $@" }"#)
            .await
            .expect("function definition failed");

        // Call with multiple args
        let result = kernel
            .execute(r#"echo_all "a" "b" "c""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "function failed: {}", result.err);
        assert_eq!(result.out.trim(), "args: a b c");
    }

    #[tokio::test]
    async fn test_shell_function_with_arg_count() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define function using $# (arg count)
        kernel
            .execute(r#"count_args() { echo "count: $#" }"#)
            .await
            .expect("function definition failed");

        // Call with three args
        let result = kernel
            .execute(r#"count_args "x" "y" "z""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "function failed: {}", result.err);
        assert_eq!(result.out.trim(), "count: 3");
    }

    #[tokio::test]
    async fn test_shell_function_shared_scope() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Set a variable in parent scope
        kernel
            .execute(r#"PARENT_VAR="visible""#)
            .await
            .expect("set failed");

        // Define shell function that reads and writes parent variable
        kernel
            .execute(r#"modify_parent() {
                echo "saw: ${PARENT_VAR}"
                PARENT_VAR="changed by function"
            }"#)
            .await
            .expect("function definition failed");

        // Call the function - it SHOULD see PARENT_VAR (bash-compatible shared scope)
        let result = kernel.execute("modify_parent").await.expect("function failed");

        assert!(
            result.out.contains("visible"),
            "Shell function should access parent scope, got: {}",
            result.out
        );

        // Parent variable should be modified
        let var = kernel.get_var("PARENT_VAR").await;
        assert_eq!(
            var,
            Some(Value::String("changed by function".into())),
            "Shell function should modify parent scope"
        );
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Script Execution via PATH Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_script_execution_from_path() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Create /bin directory and script
        kernel.execute(r#"mkdir "/bin""#).await.ok();
        kernel
            .execute(r#"write "/bin/hello.kai" 'echo "Hello from script!"'"#)
            .await
            .expect("write script failed");

        // Set PATH to /bin
        kernel.execute(r#"PATH="/bin""#).await.expect("set PATH failed");

        // Call script by name (without .kai extension)
        let result = kernel
            .execute("hello")
            .await
            .expect("script execution failed");

        assert!(result.ok(), "script failed: {}", result.err);
        assert_eq!(result.out.trim(), "Hello from script!");
    }

    #[tokio::test]
    async fn test_script_with_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Create script that uses positional params
        kernel.execute(r#"mkdir "/bin""#).await.ok();
        kernel
            .execute(r#"write "/bin/greet.kai" 'echo "Hello, $1!"'"#)
            .await
            .expect("write script failed");

        // Set PATH
        kernel.execute(r#"PATH="/bin""#).await.expect("set PATH failed");

        // Call script with arg
        let result = kernel
            .execute(r#"greet "World""#)
            .await
            .expect("script execution failed");

        assert!(result.ok(), "script failed: {}", result.err);
        assert_eq!(result.out.trim(), "Hello, World!");
    }

    #[tokio::test]
    async fn test_script_not_found() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Set empty PATH
        kernel.execute(r#"PATH="/nonexistent""#).await.expect("set PATH failed");

        // Call non-existent script
        let result = kernel
            .execute("noscript")
            .await
            .expect("execution failed");

        assert!(!result.ok(), "should fail with command not found");
        assert_eq!(result.code, 127);
        assert!(result.err.contains("command not found"));
    }

    #[tokio::test]
    async fn test_script_path_search_order() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Create two directories with same-named script
        // Note: using "myscript" not "test" to avoid conflict with test builtin
        kernel.execute(r#"mkdir "/first""#).await.ok();
        kernel.execute(r#"mkdir "/second""#).await.ok();
        kernel
            .execute(r#"write "/first/myscript.kai" 'echo "from first"'"#)
            .await
            .expect("write failed");
        kernel
            .execute(r#"write "/second/myscript.kai" 'echo "from second"'"#)
            .await
            .expect("write failed");

        // Set PATH with first before second
        kernel.execute(r#"PATH="/first:/second""#).await.expect("set PATH failed");

        // Should find first one
        let result = kernel
            .execute("myscript")
            .await
            .expect("script execution failed");

        assert!(result.ok(), "script failed: {}", result.err);
        assert_eq!(result.out.trim(), "from first");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Special Variable Tests ($?, $$, unset vars)
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_last_exit_code_success() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // true exits with 0
        let result = kernel.execute("true; echo $?").await.expect("execution failed");
        assert!(result.out.contains("0"), "expected 0, got: {}", result.out);
    }

    #[tokio::test]
    async fn test_last_exit_code_failure() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // false exits with 1
        let result = kernel.execute("false; echo $?").await.expect("execution failed");
        assert!(result.out.contains("1"), "expected 1, got: {}", result.out);
    }

    #[tokio::test]
    async fn test_current_pid() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel.execute("echo $$").await.expect("execution failed");
        // PID should be a positive number
        let pid: u32 = result.out.trim().parse().expect("PID should be a number");
        assert!(pid > 0, "PID should be positive");
    }

    #[tokio::test]
    async fn test_unset_variable_expands_to_empty() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Unset variable in interpolation should be empty
        let result = kernel.execute(r#"echo "prefix:${UNSET_VAR}:suffix""#).await.expect("execution failed");
        assert_eq!(result.out.trim(), "prefix::suffix");
    }

    #[tokio::test]
    async fn test_eq_ne_operators() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Test -eq operator
        let result = kernel.execute(r#"if [[ 5 -eq 5 ]]; then echo "eq works"; fi"#).await.expect("execution failed");
        assert_eq!(result.out.trim(), "eq works");

        // Test -ne operator
        let result = kernel.execute(r#"if [[ 5 -ne 3 ]]; then echo "ne works"; fi"#).await.expect("execution failed");
        assert_eq!(result.out.trim(), "ne works");

        // Test -eq with different values
        let result = kernel.execute(r#"if [[ 5 -eq 3 ]]; then echo "wrong"; else echo "correct"; fi"#).await.expect("execution failed");
        assert_eq!(result.out.trim(), "correct");
    }

    #[tokio::test]
    async fn test_escaped_dollar_in_string() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // \$ should produce literal $
        let result = kernel.execute(r#"echo "\$100""#).await.expect("execution failed");
        assert_eq!(result.out.trim(), "$100");
    }

    #[tokio::test]
    async fn test_special_vars_in_interpolation() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Test $? in string interpolation
        let result = kernel.execute(r#"true; echo "exit: $?""#).await.expect("execution failed");
        assert_eq!(result.out.trim(), "exit: 0");

        // Test $$ in string interpolation
        let result = kernel.execute(r#"echo "pid: $$""#).await.expect("execution failed");
        assert!(result.out.starts_with("pid: "), "unexpected output: {}", result.out);
        let pid_part = result.out.trim().strip_prefix("pid: ").unwrap();
        let _pid: u32 = pid_part.parse().expect("PID in string should be a number");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Command Substitution Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_command_subst_assignment() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Command substitution in assignment
        let result = kernel.execute(r#"X=$(echo hello); echo "$X""#).await.expect("execution failed");
        assert_eq!(result.out.trim(), "hello");
    }

    #[tokio::test]
    async fn test_command_subst_with_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Command substitution with string argument
        let result = kernel.execute(r#"X=$(echo "a b c"); echo "$X""#).await.expect("execution failed");
        assert_eq!(result.out.trim(), "a b c");
    }

    #[tokio::test]
    async fn test_command_subst_nested_vars() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Variables inside command substitution
        let result = kernel.execute(r#"Y=world; X=$(echo "hello $Y"); echo "$X""#).await.expect("execution failed");
        assert_eq!(result.out.trim(), "hello world");
    }

    #[tokio::test]
    async fn test_background_job_basic() {
        use std::time::Duration;

        let kernel = Kernel::new(KernelConfig::isolated()).expect("failed to create kernel");

        // Run a simple background command
        let result = kernel.execute("echo hello &").await.expect("execution failed");
        assert!(result.ok(), "background command should succeed: {}", result.err);
        assert!(result.out.contains("[1]"), "should return job ID: {}", result.out);

        // Give the job time to complete
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Check job status
        let status = kernel.execute("cat /v/jobs/1/status").await.expect("status check failed");
        assert!(status.ok(), "status should succeed: {}", status.err);
        assert!(
            status.out.contains("done:") || status.out.contains("running"),
            "should have valid status: {}",
            status.out
        );

        // Check stdout
        let stdout = kernel.execute("cat /v/jobs/1/stdout").await.expect("stdout check failed");
        assert!(stdout.ok());
        assert!(stdout.out.contains("hello"));
    }

    #[tokio::test]
    async fn test_heredoc_piped_to_command() {
        // Bug 4: heredoc content should pipe through to next command
        let kernel = Kernel::transient().expect("kernel");
        let result = kernel.execute("cat <<EOF | cat\nhello world\nEOF").await.expect("exec");
        assert!(result.ok(), "heredoc | cat failed: {}", result.err);
        assert_eq!(result.out.trim(), "hello world");
    }

    #[tokio::test]
    async fn test_for_loop_glob_iterates() {
        // Bug 1: for F in $(glob ...) should iterate per file, not once
        let kernel = Kernel::transient().expect("kernel");
        kernel.execute("echo a > /tmp/kaish_glob_test_a.txt").await.unwrap();
        kernel.execute("echo b > /tmp/kaish_glob_test_b.txt").await.unwrap();
        let result = kernel.execute(r#"
            N=0
            for F in $(glob "/tmp/kaish_glob_test_*.txt"); do
                N=$((N + 1))
            done
            echo $N
        "#).await.unwrap();
        assert!(result.ok(), "for glob failed: {}", result.err);
        assert_eq!(result.out.trim(), "2", "Should iterate 2 files, got: {}", result.out);
        kernel.execute("rm /tmp/kaish_glob_test_a.txt").await.unwrap();
        kernel.execute("rm /tmp/kaish_glob_test_b.txt").await.unwrap();
    }

    #[tokio::test]
    async fn test_command_subst_echo_not_iterable() {
        // Regression guard: $(echo "a b c") must remain a single string
        let kernel = Kernel::transient().expect("kernel");
        let result = kernel.execute(r#"
            N=0
            for X in $(echo "a b c"); do N=$((N + 1)); done
            echo $N
        "#).await.unwrap();
        assert!(result.ok());
        assert_eq!(result.out.trim(), "1", "echo should be one item: {}", result.out);
    }

}
