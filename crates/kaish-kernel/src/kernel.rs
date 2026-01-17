//! The Kernel (核) — the heart of kaish.
//!
//! The Kernel owns and coordinates all core components:
//! - Interpreter state (scope, $?)
//! - Tool registry (builtins, user tools, MCP)
//! - VFS router (mount points)
//! - Job manager (background jobs)
//! - State store (SQLite persistence)
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
//! │  ┌──────────────┐  ┌──────────────┐  ┌──────────────────┐  │
//! │  │  JobManager  │  │  StateStore  │  │  ExecResult ($?) │  │
//! │  │ (background) │  │  (SQLite)    │  │                  │  │
//! │  └──────────────┘  └──────────────┘  └──────────────────┘  │
//! └────────────────────────────────────────────────────────────┘
//! ```

use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result};
use tokio::sync::RwLock;

use crate::ast::{Arg, Expr, Stmt, Value};
use crate::interpreter::{eval_expr, ExecResult, Scope};
use crate::parser::parse;
use crate::scheduler::{JobManager, PipelineRunner};
use crate::state::{paths as state_paths, StateStore};
use crate::tools::{register_builtins, ExecContext, ToolArgs, ToolRegistry};
use crate::vfs::{LocalFs, MemoryFs, VfsRouter};

/// Configuration for kernel initialization.
#[derive(Debug, Clone)]
pub struct KernelConfig {
    /// Name of this kernel (used for state file naming).
    pub name: String,
    /// Whether to persist state to SQLite.
    pub persist: bool,
    /// Whether to mount the local filesystem at /mnt/local.
    pub mount_local: bool,
    /// Root path for local filesystem mount.
    pub local_root: Option<PathBuf>,
    /// Initial working directory.
    pub cwd: PathBuf,
}

impl Default for KernelConfig {
    fn default() -> Self {
        Self {
            name: "default".to_string(),
            persist: true,
            mount_local: true,
            local_root: None,
            cwd: PathBuf::from("/"),
        }
    }
}

impl KernelConfig {
    /// Create a transient (non-persistent) kernel config.
    pub fn transient() -> Self {
        Self {
            name: "transient".to_string(),
            persist: false,
            mount_local: true,
            local_root: None,
            cwd: PathBuf::from("/"),
        }
    }

    /// Create a persistent kernel config with the given name.
    pub fn persistent(name: &str) -> Self {
        Self {
            name: name.to_string(),
            persist: true,
            mount_local: true,
            local_root: None,
            cwd: PathBuf::from("/"),
        }
    }
}

/// The Kernel (核) — executes kaish code.
///
/// This is the primary interface for running kaish commands. It owns all
/// the runtime state: variables, tools, VFS, jobs, and persistence.
pub struct Kernel {
    /// Kernel name (for state file).
    name: String,
    /// Variable scope.
    scope: RwLock<Scope>,
    /// Tool registry.
    tools: Arc<ToolRegistry>,
    /// Virtual filesystem router.
    vfs: Arc<VfsRouter>,
    /// Background job manager.
    jobs: Arc<JobManager>,
    /// Pipeline runner.
    runner: PipelineRunner,
    /// Execution context (cwd, stdin, etc.).
    exec_ctx: RwLock<ExecContext>,
    /// Persistent state store (optional).
    state: Option<StateStore>,
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

        // Set up state store if persistent
        let state = if config.persist {
            let state_dir = state_paths::kernels_dir();
            std::fs::create_dir_all(&state_dir).ok();
            let db_path = state_dir.join(format!("{}.db", config.name));
            StateStore::open(&db_path).ok()
        } else {
            None
        };

        // Load scope from state if available
        let scope = if let Some(ref store) = state {
            let mut scope = Scope::new();
            if let Ok(vars) = store.load_all_variables() {
                for (name, value) in vars {
                    scope.set(name, value);
                }
            }
            scope
        } else {
            Scope::new()
        };

        // Load cwd from state if available, or use config
        let cwd = if let Some(ref store) = state {
            let stored = store.get_cwd().unwrap_or_default();
            if stored.is_empty() || stored == "/" {
                config.cwd
            } else {
                PathBuf::from(stored)
            }
        } else {
            config.cwd
        };

        // Create execution context
        let mut exec_ctx = ExecContext::new(vfs.clone());
        exec_ctx.set_cwd(cwd);
        exec_ctx.set_job_manager(jobs.clone());
        exec_ctx.set_tool_schemas(tools.schemas());

        Ok(Self {
            name: config.name,
            scope: RwLock::new(scope),
            tools,
            vfs,
            jobs,
            runner,
            exec_ctx: RwLock::new(exec_ctx),
            state,
        })
    }

    /// Create a transient kernel (no persistence).
    pub fn transient() -> Result<Self> {
        Self::new(KernelConfig::transient())
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

        let mut result = ExecResult::success("");

        for stmt in program.statements {
            if matches!(stmt, Stmt::Empty) {
                continue;
            }
            result = self.execute_stmt(&stmt).await?;
        }

        Ok(result)
    }

    /// Execute a single statement.
    fn execute_stmt<'a>(
        &'a self,
        stmt: &'a Stmt,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<ExecResult>> + 'a>> {
        Box::pin(async move {
        match stmt {
            Stmt::Assignment(assign) => {
                let mut scope = self.scope.write().await;
                let value = eval_expr(&assign.value, &mut scope)
                    .context("failed to evaluate assignment")?;
                scope.set(&assign.name, value.clone());
                drop(scope);

                // Persist variable
                if let Some(ref store) = self.state {
                    store.set_variable(&assign.name, &value).ok();
                }

                Ok(ExecResult::success_data(value))
            }
            Stmt::Command(cmd) => {
                let result = self.execute_command(&cmd.name, &cmd.args).await?;
                self.update_last_result(&result).await;
                Ok(result)
            }
            Stmt::Pipeline(pipeline) => {
                let result = self.execute_pipeline(pipeline).await?;
                self.update_last_result(&result).await;
                Ok(result)
            }
            Stmt::If(if_stmt) => {
                let cond_value = {
                    let mut scope = self.scope.write().await;
                    eval_expr(&if_stmt.condition, &mut scope)?
                };

                let branch = if is_truthy(&cond_value) {
                    &if_stmt.then_branch
                } else {
                    if_stmt.else_branch.as_ref().map(|v| v.as_slice()).unwrap_or(&[])
                };

                let mut result = ExecResult::success("");
                for stmt in branch {
                    result = self.execute_stmt(stmt).await?;
                }
                Ok(result)
            }
            Stmt::For(for_loop) => {
                let iterable = {
                    let mut scope = self.scope.write().await;
                    eval_expr(&for_loop.iterable, &mut scope)?
                };

                let items = match iterable {
                    Value::Array(items) => items,
                    _ => return Ok(ExecResult::failure(1, "for loop requires an array")),
                };

                let mut result = ExecResult::success("");
                {
                    let mut scope = self.scope.write().await;
                    scope.push_frame();
                }

                for item in items {
                    if let Expr::Literal(value) = item {
                        {
                            let mut scope = self.scope.write().await;
                            scope.set(&for_loop.variable, value);
                        }
                        for stmt in &for_loop.body {
                            result = self.execute_stmt(stmt).await?;
                        }
                    }
                }

                {
                    let mut scope = self.scope.write().await;
                    scope.pop_frame();
                }
                Ok(result)
            }
            Stmt::ToolDef(_tool) => {
                // TODO: Register user-defined tool
                Ok(ExecResult::failure(1, "tool definitions not yet implemented"))
            }
            Stmt::Empty => Ok(ExecResult::success("")),
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

        // Persist cwd if changed
        if let Some(ref store) = self.state {
            store.set_cwd(&ctx.cwd.to_string_lossy()).ok();
        }

        Ok(result)
    }

    /// Execute a single command.
    async fn execute_command(&self, name: &str, args: &[Arg]) -> Result<ExecResult> {
        // Special built-ins
        match name {
            "true" => return Ok(ExecResult::success("")),
            "false" => return Ok(ExecResult::failure(1, "")),
            _ => {}
        }

        // Look up tool
        let tool = match self.tools.get(name) {
            Some(t) => t,
            None => return Ok(ExecResult::failure(127, format!("tool not found: {}", name))),
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

        // Persist cwd if cd was called
        if name == "cd" && result.ok() {
            if let Some(ref store) = self.state {
                store.set_cwd(&ctx.cwd.to_string_lossy()).ok();
            }
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
                    tool_args.positional.push(value);
                }
                Arg::Named { key, value } => {
                    let mut scope_clone = scope.clone();
                    let val = eval_expr(value, &mut scope_clone)?;
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
            }
        }

        Ok(tool_args)
    }

    /// Update the last result in scope.
    async fn update_last_result(&self, result: &ExecResult) {
        let mut scope = self.scope.write().await;
        scope.set_last_result(result.clone());

        if let Some(ref store) = self.state {
            store.set_last_result(result).ok();
        }
    }

    // --- Variable Access ---

    /// Get a variable value.
    pub async fn get_var(&self, name: &str) -> Option<Value> {
        let scope = self.scope.read().await;
        scope.get(name).cloned()
    }

    /// Set a variable value.
    pub async fn set_var(&self, name: &str, value: Value) {
        let mut scope = self.scope.write().await;
        scope.set(name.to_string(), value.clone());

        if let Some(ref store) = self.state {
            store.set_variable(name, &value).ok();
        }
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
        ctx.set_cwd(path.clone());

        if let Some(ref store) = self.state {
            store.set_cwd(&path.to_string_lossy()).ok();
        }
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
    pub async fn reset(&self) -> Result<()> {
        {
            let mut scope = self.scope.write().await;
            *scope = Scope::new();
        }
        {
            let mut ctx = self.exec_ctx.write().await;
            ctx.cwd = PathBuf::from("/");
        }

        if let Some(ref store) = self.state {
            store.delete_all_variables()?;
            store.set_cwd("/").ok();
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
        Value::Array(a) => !a.is_empty(),
        Value::Object(o) => !o.is_empty(),
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

        kernel.execute("set X = 42").await.expect("set failed");

        let value = kernel.get_var("X").await;
        assert_eq!(value, Some(Value::Int(42)));
    }

    #[tokio::test]
    async fn test_kernel_var_expansion() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("set NAME = \"world\"").await.expect("set failed");
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

        kernel.execute("set X = 1").await.expect("set failed");
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

        kernel.execute("set A = 1").await.ok();
        kernel.execute("set B = 2").await.ok();

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
}
