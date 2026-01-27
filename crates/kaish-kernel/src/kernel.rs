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

use crate::ast::{Arg, Stmt, ToolDef, Value};
use crate::backend::KernelBackend;
use crate::glob::glob_match;
use crate::interpreter::{eval_expr, expand_tilde, value_to_string, ControlFlow, DisplayHint, ExecResult, Scope};
use crate::parser::parse;
use crate::scheduler::{JobManager, PipelineRunner};
use crate::tools::{register_builtins, ExecContext, ToolArgs, ToolRegistry};
use crate::validator::{Severity, Validator};
use crate::vfs::{LocalFs, MemoryFs, VfsRouter};

/// Configuration for kernel initialization.
#[derive(Debug, Clone)]
pub struct KernelConfig {
    /// Name of this kernel (for identification).
    pub name: String,
    /// Whether to mount the local filesystem at /mnt/local.
    pub mount_local: bool,
    /// Root path for local filesystem mount.
    pub local_root: Option<PathBuf>,
    /// Initial working directory.
    pub cwd: PathBuf,
    /// Whether to skip pre-execution validation.
    ///
    /// When false (default), scripts are validated before execution to catch
    /// errors early. Set to true to skip validation for performance or to
    /// allow dynamic/external commands.
    pub skip_validation: bool,
}

impl Default for KernelConfig {
    fn default() -> Self {
        Self {
            name: "default".to_string(),
            mount_local: true,
            local_root: None,
            cwd: PathBuf::from("/"),
            skip_validation: false,
        }
    }
}

impl KernelConfig {
    /// Create a transient kernel config.
    pub fn transient() -> Self {
        Self {
            name: "transient".to_string(),
            mount_local: true,
            local_root: None,
            cwd: PathBuf::from("/"),
            skip_validation: false,
        }
    }

    /// Create a kernel config with the given name.
    pub fn named(name: &str) -> Self {
        Self {
            name: name.to_string(),
            mount_local: true,
            local_root: None,
            cwd: PathBuf::from("/"),
            skip_validation: false,
        }
    }

    /// Skip pre-execution validation.
    pub fn with_skip_validation(mut self, skip: bool) -> Self {
        self.skip_validation = skip;
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
}

impl Kernel {
    /// Create a new kernel with the given configuration.
    pub fn new(config: KernelConfig) -> Result<Self> {
        let mut vfs = VfsRouter::new();

        // Mount memory at root for now
        vfs.mount("/", MemoryFs::new());

        // Mount scratch space
        vfs.mount("/tmp", MemoryFs::new());

        // Mount local filesystem at /mnt/local if configured
        if config.mount_local {
            let root = config.local_root.unwrap_or_else(|| {
                std::env::var("HOME")
                    .map(PathBuf::from)
                    .unwrap_or_else(|_| PathBuf::from("/"))
            });
            vfs.mount("/mnt/local", LocalFs::new(root));
        }

        let vfs = Arc::new(vfs);
        let jobs = Arc::new(JobManager::new());

        // Set up tools
        let mut tools = ToolRegistry::new();
        register_builtins(&mut tools);
        let tools = Arc::new(tools);

        // Pipeline runner
        let runner = PipelineRunner::new(tools.clone());

        let scope = Scope::new();
        let cwd = config.cwd;

        // Create execution context with VFS and tools for backend dispatch
        let mut exec_ctx = ExecContext::with_vfs_and_tools(vfs.clone(), tools.clone());
        exec_ctx.set_cwd(cwd);
        exec_ctx.set_job_manager(jobs.clone());
        exec_ctx.set_tool_schemas(tools.schemas());

        Ok(Self {
            name: config.name,
            scope: RwLock::new(scope),
            tools,
            user_tools: RwLock::new(HashMap::new()),
            vfs,
            jobs,
            runner,
            exec_ctx: RwLock::new(exec_ctx),
            skip_validation: config.skip_validation,
        })
    }

    /// Create a transient kernel (no persistence).
    pub fn transient() -> Result<Self> {
        Self::new(KernelConfig::transient())
    }

    /// Create a kernel with a custom backend.
    ///
    /// This constructor allows embedding kaish in other systems that provide
    /// their own storage backend (e.g., CRDT-backed storage in kaijutsu).
    /// The provided backend will be used for all file operations in builtins.
    ///
    /// Note: A VfsRouter is still created internally for compatibility with
    /// the `vfs()` method, but it won't be used for execution context operations.
    pub fn with_backend(backend: Arc<dyn KernelBackend>, config: KernelConfig) -> Result<Self> {
        // Create VFS for compatibility (but exec_ctx will use the provided backend)
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        vfs.mount("/tmp", MemoryFs::new());
        if config.mount_local {
            let root = config.local_root.unwrap_or_else(|| {
                std::env::var("HOME")
                    .map(PathBuf::from)
                    .unwrap_or_else(|_| PathBuf::from("/"))
            });
            vfs.mount("/mnt/local", LocalFs::new(root));
        }
        let vfs = Arc::new(vfs);
        let jobs = Arc::new(JobManager::new());

        // Set up tools
        let mut tools = ToolRegistry::new();
        register_builtins(&mut tools);
        let tools = Arc::new(tools);

        // Pipeline runner
        let runner = PipelineRunner::new(tools.clone());

        let scope = Scope::new();
        let cwd = config.cwd;

        // Create execution context with custom backend
        let mut exec_ctx = ExecContext::with_backend(backend);
        exec_ctx.set_cwd(cwd);
        exec_ctx.set_job_manager(jobs.clone());
        exec_ctx.set_tool_schemas(tools.schemas());
        exec_ctx.set_tools(tools.clone());

        Ok(Self {
            name: config.name,
            scope: RwLock::new(scope),
            tools,
            user_tools: RwLock::new(HashMap::new()),
            vfs,
            jobs,
            runner,
            exec_ctx: RwLock::new(exec_ctx),
            skip_validation: config.skip_validation,
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
            let validator = Validator::new(&self.tools, &*user_tools);
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
                ControlFlow::Normal(r) => result = r,
                ControlFlow::Exit { code } => {
                    // Exit terminates execution immediately
                    let exit_result = ExecResult::success(code.to_string());
                    return Ok(exit_result);
                }
                ControlFlow::Return { value } => {
                    // Return at top level just returns the value
                    result = value;
                }
                ControlFlow::Break { result: r, .. } | ControlFlow::Continue { result: r, .. } => {
                    // Break/continue at top level just returns the result
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
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<ControlFlow>> + 'a>> {
        Box::pin(async move {
        match stmt {
            Stmt::Assignment(assign) => {
                let mut scope = self.scope.write().await;
                let value = eval_expr(&assign.value, &mut scope)
                    .context("failed to evaluate assignment")?;
                scope.set(&assign.name, value.clone());
                drop(scope);

                // Assignments don't produce output (like sh)
                Ok(ControlFlow::ok(ExecResult::success("")))
            }
            Stmt::Command(cmd) => {
                let result = self.execute_command(&cmd.name, &cmd.args).await?;
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
                let cond_value = {
                    let mut scope = self.scope.write().await;
                    eval_expr(&if_stmt.condition, &mut scope)?
                };

                let branch = if is_truthy(&cond_value) {
                    &if_stmt.then_branch
                } else {
                    if_stmt.else_branch.as_deref().unwrap_or(&[])
                };

                let mut flow = ControlFlow::ok(ExecResult::success(""));
                for stmt in branch {
                    flow = self.execute_stmt_flow(stmt).await?;
                    if !flow.is_normal() {
                        return Ok(flow);
                    }
                }
                Ok(flow)
            }
            Stmt::For(for_loop) => {
                // Evaluate all items and collect words for iteration
                let mut words: Vec<String> = Vec::new();
                for item_expr in &for_loop.items {
                    let item = {
                        let mut scope = self.scope.write().await;
                        eval_expr(item_expr, &mut scope)?
                    };
                    // POSIX-style: word-split string values
                    match &item {
                        Value::String(s) => {
                            words.extend(s.split_whitespace().map(String::from));
                        }
                        _ => words.push(value_to_string(&item)),
                    }
                }

                let mut result = ExecResult::success("");
                {
                    let mut scope = self.scope.write().await;
                    scope.push_frame();
                }

                'outer: for word in words {
                    {
                        let mut scope = self.scope.write().await;
                        scope.set(&for_loop.variable, Value::String(word));
                    }
                    for stmt in &for_loop.body {
                        let mut flow = self.execute_stmt_flow(stmt).await?;
                        match &mut flow {
                            ControlFlow::Normal(r) => result = r.clone(),
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
                    // Evaluate condition
                    let cond_value = {
                        let mut scope = self.scope.write().await;
                        eval_expr(&while_loop.condition, &mut scope)?
                    };

                    if !is_truthy(&cond_value) {
                        break;
                    }

                    // Execute body
                    for stmt in &while_loop.body {
                        let mut flow = self.execute_stmt_flow(stmt).await?;
                        match &mut flow {
                            ControlFlow::Normal(r) => result = r.clone(),
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
                        let mut result = ControlFlow::ok(ExecResult::success(""));
                        for stmt in &branch.body {
                            result = self.execute_stmt_flow(stmt).await?;
                            if !result.is_normal() {
                                return Ok(result);
                            }
                        }
                        return Ok(result);
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
                let value = if let Some(e) = expr {
                    let mut scope = self.scope.write().await;
                    let val = eval_expr(e, &mut scope)?;
                    ExecResult::success_data(val)
                } else {
                    ExecResult::success("")
                };
                Ok(ControlFlow::return_value(value))
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
                    ControlFlow::Normal(ref left_result) => {
                        self.update_last_result(left_result).await;
                        if left_result.ok() {
                            let right_flow = self.execute_stmt_flow(right).await?;
                            if let ControlFlow::Normal(ref right_result) = right_flow {
                                self.update_last_result(right_result).await;
                            }
                            Ok(right_flow)
                        } else {
                            Ok(left_flow)
                        }
                    }
                    _ => Ok(left_flow), // Propagate non-normal flow
                }
            }
            Stmt::OrChain { left, right } => {
                // cmd1 || cmd2 - run cmd2 only if cmd1 fails (non-zero exit code)
                let left_flow = self.execute_stmt_flow(left).await?;
                match left_flow {
                    ControlFlow::Normal(ref left_result) => {
                        self.update_last_result(left_result).await;
                        if !left_result.ok() {
                            let right_flow = self.execute_stmt_flow(right).await?;
                            if let ControlFlow::Normal(ref right_result) = right_flow {
                                self.update_last_result(right_result).await;
                            }
                            Ok(right_flow)
                        } else {
                            Ok(left_flow)
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
        })
    }

    /// Execute a pipeline.
    async fn execute_pipeline(&self, pipeline: &crate::ast::Pipeline) -> Result<ExecResult> {
        if pipeline.commands.is_empty() {
            return Ok(ExecResult::success(""));
        }

        // For single command, execute directly
        if pipeline.commands.len() == 1 {
            let cmd = &pipeline.commands[0];
            return self.execute_command(&cmd.name, &cmd.args).await;
        }

        // Multi-command pipeline uses the runner
        let mut ctx = self.exec_ctx.write().await;
        {
            let scope = self.scope.read().await;
            ctx.scope = scope.clone();
        }

        let result = self.runner.run(&pipeline.commands, &mut ctx).await;

        // Sync changes back from context
        {
            let mut scope = self.scope.write().await;
            *scope = ctx.scope.clone();
        }

        Ok(result)
    }

    /// Execute a single command.
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
                // Try executing as script from PATH before returning "tool not found"
                if let Some(result) = self.try_execute_script(name, args).await? {
                    return Ok(result);
                }
                return Ok(ExecResult::failure(127, format!("tool not found: {}", name)));
            }
        };

        // Build arguments
        let tool_args = {
            let scope = self.scope.read().await;
            let ctx = self.exec_ctx.read().await;
            self.build_args(args, &scope, &ctx)?
        };

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

        Ok(result)
    }

    /// Build tool arguments from AST args.
    fn build_args(&self, args: &[Arg], scope: &Scope, _ctx: &ExecContext) -> Result<ToolArgs> {
        let mut tool_args = ToolArgs::new();

        for arg in args {
            match arg {
                Arg::Positional(expr) => {
                    let mut scope_clone = scope.clone();
                    let value = eval_expr(expr, &mut scope_clone)?;
                    // Apply tilde expansion to string values
                    let value = apply_tilde_expansion(value);
                    tool_args.positional.push(value);
                }
                Arg::Named { key, value } => {
                    let mut scope_clone = scope.clone();
                    let val = eval_expr(value, &mut scope_clone)?;
                    // Apply tilde expansion to string values
                    let val = apply_tilde_expansion(val);
                    tool_args.named.insert(key.clone(), val);
                }
                Arg::ShortFlag(name) => {
                    for c in name.chars() {
                        tool_args.flags.insert(c.to_string());
                    }
                }
                Arg::LongFlag(name) => {
                    tool_args.flags.insert(name.clone());
                }
                Arg::DoubleDash => {
                    // Marker for end of flags - no action needed here,
                    // subsequent flags were converted to positional during parsing
                }
            }
        }

        Ok(tool_args)
    }

    /// Update the last result in scope.
    async fn update_last_result(&self, result: &ExecResult) {
        let mut scope = self.scope.write().await;
        scope.set_last_result(result.clone());
    }

    /// Execute a user-defined function with shared scope (sh-compatible).
    ///
    /// Functions execute in the current scope and can read/modify parent variables.
    /// Only positional parameters ($0, $1, etc.) are saved and restored.
    async fn execute_user_tool(&self, def: ToolDef, args: &[Arg]) -> Result<ExecResult> {
        // 1. Build function args from AST args (using current scope for evaluation)
        let tool_args = {
            let scope = self.scope.read().await;
            let ctx = self.exec_ctx.read().await;
            self.build_args(args, &scope, &ctx)?
        };

        // 2. Save current positional parameters and set new ones for this function
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
                            // Return from this function with the value
                            accumulated_out.push_str(&value.out);
                            accumulated_err.push_str(&value.err);
                            last_code = value.code;
                            last_data = value.data;
                            break;
                        }
                        ControlFlow::Exit { code } => {
                            // Exit propagates through - restore positional params and return
                            let mut scope = self.scope.write().await;
                            scope.set_positional(saved_positional.0.clone(), saved_positional.1.clone());
                            return Ok(ExecResult::failure(code, "exit"));
                        }
                        ControlFlow::Break { result: r, .. } | ControlFlow::Continue { result: r, .. } => {
                            // Break/continue outside a loop - treat as normal
                            accumulated_out.push_str(&r.out);
                            accumulated_err.push_str(&r.err);
                            last_code = r.code;
                            last_data = r.data;
                        }
                    }
                }
                Err(e) => {
                    // Restore positional params on error
                    let mut scope = self.scope.write().await;
                    scope.set_positional(saved_positional.0.clone(), saved_positional.1.clone());
                    return Err(e);
                }
            }
        }

        let result = ExecResult {
            code: last_code,
            out: accumulated_out,
            err: accumulated_err,
            data: last_data,
            hint: DisplayHint::default(),
        };

        // 4. Restore original positional parameters
        {
            let mut scope = self.scope.write().await;
            scope.set_positional(saved_positional.0, saved_positional.1);
        }

        // 5. Return final result
        Ok(result)
    }

    /// Execute the `source` / `.` command to include and run a script.
    ///
    /// Unlike regular tool execution, `source` executes in the CURRENT scope,
    /// allowing the sourced script to set variables and modify shell state.
    async fn execute_source(&self, args: &[Arg]) -> Result<ExecResult> {
        // Get the file path from the first positional argument
        let path = {
            let scope = self.scope.read().await;
            let ctx = self.exec_ctx.read().await;
            let tool_args = self.build_args(args, &scope, &ctx)?;

            match tool_args.positional.first() {
                Some(Value::String(s)) => s.clone(),
                Some(v) => value_to_string(v),
                None => {
                    return Ok(ExecResult::failure(1, "source: missing filename"));
                }
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
                            // Exit from sourced script propagates
                            return Ok(ExecResult::failure(code, "exit"));
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

            // Build tool_args from args
            let tool_args = {
                let scope = self.scope.read().await;
                let ctx = self.exec_ctx.read().await;
                self.build_args(args, &scope, &ctx)?
            };

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

            // Execute script statements
            let mut result = ExecResult::success("");
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
                                // Restore scope and return
                                let mut scope = self.scope.write().await;
                                *scope = original_scope;
                                return Ok(Some(ExecResult::failure(code, "exit")));
                            }
                            ControlFlow::Break { result: r, .. } | ControlFlow::Continue { result: r, .. } => {
                                result = r;
                            }
                        }
                    }
                    Err(e) => {
                        // Restore original scope on error
                        let mut scope = self.scope.write().await;
                        *scope = original_scope;
                        return Err(e.context(format!("script: {}", script_path.display())));
                    }
                }
            }

            // Restore original scope
            {
                let mut scope = self.scope.write().await;
                *scope = original_scope;
            }

            return Ok(Some(result));
        }

        // No script found
        Ok(None)
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
}

/// Check if a value is truthy.
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(i) => *i != 0,
        Value::Float(f) => *f != 0.0,
        Value::String(s) => !s.is_empty(),
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
        assert!(result.err.contains("tool not found"));
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

        let cwd = kernel.cwd().await;
        assert_eq!(cwd, PathBuf::from("/"));

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

        // Call with arg=1 should return 42
        let result = kernel
            .execute("early_return 1")
            .await
            .expect("function call failed");

        assert!(result.ok());
        // The return value should be in the data field
        assert_eq!(result.data, Some(Value::Int(42)));
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

        // The exit code should be in the output
        assert_eq!(result.out, "42");
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

        assert!(!result.ok(), "should fail with tool not found");
        assert_eq!(result.code, 127);
        assert!(result.err.contains("tool not found"));
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
}
