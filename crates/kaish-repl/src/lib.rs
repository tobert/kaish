//! kaish REPL — Interactive shell for 会sh.
//!
//! This is an evolving REPL that grows with each layer of the kaish project.
//! Currently (L10), it provides:
//!
//! - Parse input and display AST (`/ast` toggle)
//! - Evaluate expressions with persistent Scope
//! - `set X = value` assignments
//! - Real tool execution via VFS
//! - Pipeline execution (`a | b | c`)
//! - Background jobs (`cmd &`) with `jobs` and `wait` commands
//! - MCP tool integration via pre-configured ToolRegistry
//! - SQLite-backed session persistence
//! - Meta-commands: `/help`, `/quit`, `/ast`, `/scope`, `/cwd`, `/jobs`, `/tools`, `/state`

use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result};
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;
use rustyline::Editor;
use tokio::runtime::Runtime;

use kaish_kernel::ast::{Arg, Expr, Pipeline, Stmt, Value};
use kaish_kernel::interpreter::{ExecResult, Scope};
use kaish_kernel::parser::parse;
use kaish_kernel::scheduler::{JobManager, PipelineRunner};
use kaish_kernel::state::{StateStore, paths as state_paths};
use kaish_kernel::tools::{ExecContext, ToolArgs, ToolRegistry, register_builtins};
use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};

/// REPL configuration and state.
pub struct Repl {
    scope: Scope,
    show_ast: bool,
    tools: Arc<ToolRegistry>,
    exec_ctx: ExecContext,
    runtime: Runtime,
    pipeline_runner: PipelineRunner,
    job_manager: Arc<JobManager>,
    /// SQLite-backed state persistence (None for ephemeral sessions).
    state: Option<StateStore>,
}

impl Repl {
    /// Create a new REPL instance with VFS rooted at current directory.
    pub fn new() -> Result<Self> {
        let cwd = std::env::current_dir().context("Failed to get current directory")?;
        Self::with_root(cwd)
    }

    /// Create a new REPL with VFS rooted at the given path.
    pub fn with_root(root: PathBuf) -> Result<Self> {
        let mut tools = ToolRegistry::new();
        register_builtins(&mut tools);
        Self::with_tools(root, tools)
    }

    /// Create a new REPL with a custom ToolRegistry.
    ///
    /// This allows MCP tools to be pre-registered before starting the REPL.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use kaish_kernel::mcp::{McpClient, McpConfig, McpTransport, register_mcp_tools};
    /// use kaish_kernel::tools::{ToolRegistry, register_builtins};
    ///
    /// let mut tools = ToolRegistry::new();
    /// register_builtins(&mut tools);
    ///
    /// // Connect to an MCP server and register its tools
    /// let client = Arc::new(McpClient::new(McpConfig { ... }));
    /// client.connect().await?;
    /// register_mcp_tools(&client, &mut tools).await?;
    ///
    /// // Create REPL with MCP tools
    /// let repl = Repl::with_tools(root, tools)?;
    /// ```
    pub fn with_tools(root: PathBuf, tools: ToolRegistry) -> Result<Self> {
        // Build the VFS
        let mut vfs = VfsRouter::new();

        // Mount the real filesystem at /mnt/local
        let local_fs = LocalFs::new(root.clone());
        vfs.mount("/mnt/local", local_fs);

        // Mount a memory fs at /scratch for ephemeral data
        vfs.mount("/scratch", MemoryFs::new());

        // Mount root as memory fs (for now)
        vfs.mount("/", MemoryFs::new());

        // Create execution context starting at /mnt/local
        let mut exec_ctx = ExecContext::new(Arc::new(vfs));
        exec_ctx.set_cwd(PathBuf::from("/mnt/local"));

        // Wrap tools in Arc
        let tools = Arc::new(tools);

        // Create job manager and add to context
        let job_manager = Arc::new(JobManager::new());
        exec_ctx.set_job_manager(job_manager.clone());

        // Create pipeline runner
        let pipeline_runner = PipelineRunner::new(tools.clone());

        // Create tokio runtime for async tool execution
        let runtime = Runtime::new().context("Failed to create tokio runtime")?;

        // Open or create state database
        let (state, scope) = Self::init_state()?;

        // Restore cwd from state if available
        if let Some(ref store) = state {
            if let Ok(cwd) = store.get_cwd() {
                if cwd != "/" && !cwd.is_empty() {
                    exec_ctx.set_cwd(PathBuf::from(&cwd));
                }
            }
        }

        Ok(Self {
            scope,
            show_ast: false,
            tools,
            exec_ctx,
            runtime,
            pipeline_runner,
            job_manager,
            state,
        })
    }

    /// Initialize state storage and load persisted variables.
    fn init_state() -> Result<(Option<StateStore>, Scope)> {
        let state_dir = state_paths::kernels_dir();
        let db_path = state_dir.join("repl.db");

        // Try to open/create state database
        let store = match StateStore::open(&db_path) {
            Ok(s) => Some(s),
            Err(e) => {
                tracing::warn!("Could not open state database at {}: {}", db_path.display(), e);
                None
            }
        };

        // Load variables into scope
        let mut scope = Scope::new();
        if let Some(ref store) = store {
            if let Ok(vars) = store.load_all_variables() {
                for (name, value) in vars {
                    scope.set(name, value);
                }
            }
        }

        Ok((store, scope))
    }

    /// Process a single line of input.
    pub fn process_line(&mut self, line: &str) -> Result<Option<String>> {
        let trimmed = line.trim();

        // Handle meta-commands
        if trimmed.starts_with('/') {
            return self.handle_meta_command(trimmed);
        }

        // Skip empty lines
        if trimmed.is_empty() {
            return Ok(None);
        }

        // Parse the input
        let program = match parse(trimmed) {
            Ok(prog) => prog,
            Err(errors) => {
                let mut msg = String::from("Parse error:\n");
                for err in errors {
                    msg.push_str(&format!("  {err}\n"));
                }
                return Ok(Some(msg));
            }
        };

        // Show AST if enabled
        if self.show_ast {
            return Ok(Some(format!("{:#?}", program)));
        }

        // Execute each statement
        let mut output = String::new();
        for stmt in program.statements {
            if let Some(result) = self.execute_stmt(&stmt)? {
                if !output.is_empty() {
                    output.push('\n');
                }
                output.push_str(&result);
            }
        }

        if output.is_empty() {
            Ok(None)
        } else {
            Ok(Some(output))
        }
    }

    /// Execute a single statement.
    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<Option<String>> {
        match stmt {
            Stmt::Assignment(assign) => {
                let value = self.eval_expr(&assign.value)?;
                self.scope.set(&assign.name, value.clone());
                // Persist to state store
                if let Some(ref store) = self.state {
                    if let Err(e) = store.set_variable(&assign.name, &value) {
                        tracing::warn!("Failed to persist variable {}: {}", assign.name, e);
                    }
                }
                Ok(Some(format!("{} = {}", assign.name, format_value(&value))))
            }
            Stmt::Command(cmd) => {
                let result = self.execute_command(&cmd.name, &cmd.args)?;
                self.scope.set_last_result(result.clone());
                Ok(Some(format_result(&result)))
            }
            Stmt::Pipeline(pipeline) => {
                let result = self.execute_pipeline(pipeline)?;
                self.scope.set_last_result(result.clone());
                Ok(Some(format_result(&result)))
            }
            Stmt::If(if_stmt) => {
                let cond_value = self.eval_expr(&if_stmt.condition)?;
                let branch = if is_truthy(&cond_value) {
                    &if_stmt.then_branch
                } else {
                    if_stmt.else_branch.as_ref().map(|v| v.as_slice()).unwrap_or(&[])
                };

                let mut output = String::new();
                for stmt in branch {
                    if let Some(result) = self.execute_stmt(stmt)? {
                        if !output.is_empty() {
                            output.push('\n');
                        }
                        output.push_str(&result);
                    }
                }
                Ok(if output.is_empty() { None } else { Some(output) })
            }
            Stmt::For(for_loop) => {
                let iterable = self.eval_expr(&for_loop.iterable)?;
                let items = match iterable {
                    Value::Array(items) => items,
                    _ => return Ok(Some("Error: for loop requires an array".into())),
                };

                self.scope.push_frame();
                let mut output = String::new();

                for item in items {
                    if let Expr::Literal(value) = item {
                        self.scope.set(&for_loop.variable, value);
                        for stmt in &for_loop.body {
                            if let Some(result) = self.execute_stmt(stmt)? {
                                if !output.is_empty() {
                                    output.push('\n');
                                }
                                output.push_str(&result);
                            }
                        }
                    }
                }

                self.scope.pop_frame();
                Ok(if output.is_empty() { None } else { Some(output) })
            }
            Stmt::ToolDef(tool) => {
                Ok(Some(format!("Defined tool: {}", tool.name)))
            }
            Stmt::Empty => Ok(None),
        }
    }

    /// Execute a command using the tool registry.
    fn execute_command(&mut self, name: &str, args: &[Arg]) -> Result<ExecResult> {
        // Special built-ins that don't need the tool registry
        match name {
            "true" => return Ok(ExecResult::success("")),
            "false" => return Ok(ExecResult::failure(1, "")),
            _ => {}
        }

        // Look up tool in registry
        let tool = match self.tools.get(name) {
            Some(t) => t,
            None => {
                return Ok(ExecResult::failure(
                    127,
                    format!("{}: command not found", name),
                ));
            }
        };

        // Convert AST args to ToolArgs
        let mut tool_args = ToolArgs::new();
        for arg in args {
            match arg {
                Arg::Positional(expr) => {
                    let value = self.eval_expr(expr)?;
                    tool_args.positional.push(value);
                }
                Arg::Named { key, value } => {
                    let val = self.eval_expr(value)?;
                    tool_args.named.insert(key.clone(), val);
                }
                Arg::ShortFlag(name) => {
                    // Expand combined flags like -la into individual flags
                    for c in name.chars() {
                        tool_args.flags.insert(c.to_string());
                    }
                }
                Arg::LongFlag(name) => {
                    tool_args.flags.insert(name.clone());
                }
            }
        }

        // Execute the tool asynchronously
        let result = self.runtime.block_on(tool.execute(tool_args, &mut self.exec_ctx));

        // Sync cwd back to scope if cd was called
        if name == "cd" && result.ok() {
            let cwd_str = self.exec_ctx.cwd.to_string_lossy().to_string();
            // Update scope with new cwd for display
            self.scope.set("CWD", Value::String(cwd_str.clone()));
            // Persist to state store
            if let Some(ref store) = self.state {
                if let Err(e) = store.set_cwd(&cwd_str) {
                    tracing::warn!("Failed to persist cwd: {}", e);
                }
            }
        }

        Ok(result)
    }

    /// Execute a pipeline using the PipelineRunner.
    fn execute_pipeline(&mut self, pipeline: &Pipeline) -> Result<ExecResult> {
        let cmd_names: Vec<_> = pipeline.commands.iter().map(|c| c.name.as_str()).collect();
        let pipeline_str = cmd_names.join(" | ");

        if pipeline.background {
            // Spawn as background job
            // We need to clone what we need for the async task
            let commands = pipeline.commands.clone();
            let tools = self.tools.clone();
            let vfs = self.exec_ctx.vfs.clone();
            let cwd = self.exec_ctx.cwd.clone();
            let scope = self.exec_ctx.scope.clone();
            let job_manager = self.job_manager.clone();

            let job_id = self.runtime.block_on(async {
                let (tx, rx) = tokio::sync::oneshot::channel();

                let id = job_manager.register(pipeline_str.clone(), rx).await;

                tokio::spawn(async move {
                    let mut ctx = ExecContext::with_scope(vfs, scope);
                    ctx.set_cwd(cwd);
                    ctx.set_job_manager(job_manager);

                    let runner = PipelineRunner::new(tools);
                    let result = runner.run(&commands, &mut ctx).await;
                    let _ = tx.send(result);
                });

                id
            });

            Ok(ExecResult::success(format!("[{}] {}\n", job_id, pipeline_str)))
        } else {
            // Run synchronously
            let result = self.runtime.block_on(
                self.pipeline_runner.run(&pipeline.commands, &mut self.exec_ctx)
            );
            Ok(result)
        }
    }

    /// Evaluate an expression using the scope.
    fn eval_expr(&mut self, expr: &Expr) -> Result<Value> {
        // Simple evaluation without the full Evaluator (avoids borrow issues)
        // Command substitution will be stubbed
        self.eval_expr_inner(expr)
    }

    fn eval_expr_inner(&mut self, expr: &Expr) -> Result<Value> {
        match expr {
            Expr::Literal(value) => self.eval_literal(value),
            Expr::VarRef(path) => {
                self.scope.resolve_path(path)
                    .ok_or_else(|| anyhow::anyhow!("undefined variable"))
            }
            Expr::Interpolated(parts) => {
                let mut result = String::new();
                for part in parts {
                    match part {
                        kaish_kernel::ast::StringPart::Literal(s) => result.push_str(s),
                        kaish_kernel::ast::StringPart::Var(path) => {
                            let value = self.scope.resolve_path(path)
                                .ok_or_else(|| anyhow::anyhow!("undefined variable in interpolation"))?;
                            result.push_str(&format_value_unquoted(&value));
                        }
                    }
                }
                Ok(Value::String(result))
            }
            Expr::BinaryOp { left, op, right } => {
                use kaish_kernel::ast::BinaryOp;
                match op {
                    BinaryOp::And => {
                        let left_val = self.eval_expr_inner(left)?;
                        if !is_truthy(&left_val) {
                            return Ok(left_val);
                        }
                        self.eval_expr_inner(right)
                    }
                    BinaryOp::Or => {
                        let left_val = self.eval_expr_inner(left)?;
                        if is_truthy(&left_val) {
                            return Ok(left_val);
                        }
                        self.eval_expr_inner(right)
                    }
                    BinaryOp::Eq => {
                        let l = self.eval_expr_inner(left)?;
                        let r = self.eval_expr_inner(right)?;
                        Ok(Value::Bool(values_equal(&l, &r)))
                    }
                    BinaryOp::NotEq => {
                        let l = self.eval_expr_inner(left)?;
                        let r = self.eval_expr_inner(right)?;
                        Ok(Value::Bool(!values_equal(&l, &r)))
                    }
                    BinaryOp::Lt | BinaryOp::Gt | BinaryOp::LtEq | BinaryOp::GtEq => {
                        let l = self.eval_expr_inner(left)?;
                        let r = self.eval_expr_inner(right)?;
                        let ord = compare_values(&l, &r)?;
                        let result = match op {
                            BinaryOp::Lt => ord.is_lt(),
                            BinaryOp::Gt => ord.is_gt(),
                            BinaryOp::LtEq => ord.is_le(),
                            BinaryOp::GtEq => ord.is_ge(),
                            _ => unreachable!(),
                        };
                        Ok(Value::Bool(result))
                    }
                }
            }
            Expr::CommandSubst(pipeline) => {
                // Execute the command and return its result as an object
                let result = self.execute_pipeline(pipeline)?;
                self.scope.set_last_result(result.clone());
                Ok(result_to_value(&result))
            }
        }
    }

    fn eval_literal(&mut self, value: &Value) -> Result<Value> {
        match value {
            Value::Array(items) => {
                let evaluated: Result<Vec<_>> = items
                    .iter()
                    .map(|expr| self.eval_expr_inner(expr).map(|v| Expr::Literal(v)))
                    .collect();
                Ok(Value::Array(evaluated?))
            }
            Value::Object(fields) => {
                let evaluated: Result<Vec<_>> = fields
                    .iter()
                    .map(|(k, expr)| self.eval_expr_inner(expr).map(|v| (k.clone(), Expr::Literal(v))))
                    .collect();
                Ok(Value::Object(evaluated?))
            }
            _ => Ok(value.clone()),
        }
    }

    /// Handle a meta-command (starts with /).
    fn handle_meta_command(&mut self, cmd: &str) -> Result<Option<String>> {
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        let command = parts.first().copied().unwrap_or("");

        match command {
            "/quit" | "/q" | "/exit" => {
                std::process::exit(0);
            }
            "/help" | "/h" | "/?" => {
                Ok(Some(HELP_TEXT.to_string()))
            }
            "/ast" => {
                self.show_ast = !self.show_ast;
                Ok(Some(format!("AST mode: {}", if self.show_ast { "ON" } else { "OFF" })))
            }
            "/scope" | "/vars" => {
                let names = self.scope.all_names();
                if names.is_empty() {
                    Ok(Some("(no variables set)".to_string()))
                } else {
                    let mut output = String::from("Variables:\n");
                    for name in names {
                        if let Some(value) = self.scope.get(name) {
                            output.push_str(&format!("  {} = {}\n", name, format_value(value)));
                        }
                    }
                    Ok(Some(output.trim_end().to_string()))
                }
            }
            "/result" | "/$?" => {
                let result = self.scope.last_result();
                Ok(Some(format_result(result)))
            }
            "/cwd" => {
                Ok(Some(self.exec_ctx.cwd.to_string_lossy().to_string()))
            }
            "/tools" => {
                let names = self.tools.names();
                Ok(Some(format!("Available tools: {}", names.join(", "))))
            }
            "/jobs" => {
                let jobs = self.runtime.block_on(self.job_manager.list());
                if jobs.is_empty() {
                    Ok(Some("(no background jobs)".to_string()))
                } else {
                    let mut output = String::from("Background jobs:\n");
                    for job in jobs {
                        output.push_str(&format!("  [{}] {} {}\n", job.id, job.status, job.command));
                    }
                    Ok(Some(output.trim_end().to_string()))
                }
            }
            "/state" | "/session" => {
                match &self.state {
                    Some(store) => {
                        let session_id = store.session_id().unwrap_or_else(|_| "unknown".into());
                        let vars = store.list_variables().unwrap_or_default();
                        let db_path = state_paths::kernels_dir().join("repl.db");
                        Ok(Some(format!(
                            "Session: {}\nDatabase: {}\nPersisted variables: {}\nState: active",
                            session_id,
                            db_path.display(),
                            vars.len()
                        )))
                    }
                    None => {
                        Ok(Some("State: ephemeral (no persistence)".to_string()))
                    }
                }
            }
            "/clear-state" => {
                if let Some(ref store) = self.state {
                    // Delete all variables from state
                    if let Ok(vars) = store.list_variables() {
                        for name in vars {
                            let _ = store.delete_variable(&name);
                        }
                    }
                    // Clear scope
                    self.scope = Scope::new();
                    Ok(Some("State cleared".to_string()))
                } else {
                    Ok(Some("No state to clear (ephemeral session)".to_string()))
                }
            }
            _ => {
                Ok(Some(format!("Unknown command: {}\nType /help for available commands.", command)))
            }
        }
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new().expect("Failed to create REPL")
    }
}

/// Format a Value for display (with quotes on strings).
fn format_value(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::Array(items) => {
            let formatted: Vec<String> = items
                .iter()
                .filter_map(|e| {
                    if let Expr::Literal(v) = e {
                        Some(format_value(v))
                    } else {
                        Some("<expr>".to_string())
                    }
                })
                .collect();
            format!("[{}]", formatted.join(", "))
        }
        Value::Object(fields) => {
            let formatted: Vec<String> = fields
                .iter()
                .map(|(k, e)| {
                    let v = if let Expr::Literal(v) = e {
                        format_value(v)
                    } else {
                        "<expr>".to_string()
                    };
                    format!("\"{}\": {}", k, v)
                })
                .collect();
            format!("{{{}}}", formatted.join(", "))
        }
    }
}

/// Format a Value for display (without quotes on strings, for echo).
fn format_value_unquoted(value: &Value) -> String {
    match value {
        Value::String(s) => s.clone(),
        _ => format_value(value),
    }
}

/// Format an ExecResult for display.
fn format_result(result: &ExecResult) -> String {
    let status = if result.ok() { "✓" } else { "✗" };
    let mut output = format!("{} code={}", status, result.code);

    if !result.out.is_empty() {
        if result.out.contains('\n') {
            output.push_str(&format!("\n{}", result.out));
        } else {
            output.push_str(&format!(" out={}", result.out));
        }
    }

    if !result.err.is_empty() {
        output.push_str(&format!(" err=\"{}\"", result.err));
    }

    output
}

/// Check if a value is truthy.
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(i) => *i != 0,
        Value::Float(f) => *f != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Array(arr) => !arr.is_empty(),
        Value::Object(_) => true,
    }
}

/// Check if two values are equal.
fn values_equal(left: &Value, right: &Value) -> bool {
    match (left, right) {
        (Value::Null, Value::Null) => true,
        (Value::Bool(a), Value::Bool(b)) => a == b,
        (Value::Int(a), Value::Int(b)) => a == b,
        (Value::Float(a), Value::Float(b)) => (a - b).abs() < f64::EPSILON,
        (Value::Int(a), Value::Float(b)) | (Value::Float(b), Value::Int(a)) => {
            (*a as f64 - b).abs() < f64::EPSILON
        }
        (Value::String(a), Value::String(b)) => a == b,
        _ => false,
    }
}

/// Compare two values for ordering.
fn compare_values(left: &Value, right: &Value) -> Result<std::cmp::Ordering> {
    match (left, right) {
        (Value::Int(a), Value::Int(b)) => Ok(a.cmp(b)),
        (Value::Float(a), Value::Float(b)) => {
            a.partial_cmp(b).ok_or_else(|| anyhow::anyhow!("NaN comparison"))
        }
        (Value::Int(a), Value::Float(b)) => {
            (*a as f64).partial_cmp(b).ok_or_else(|| anyhow::anyhow!("NaN comparison"))
        }
        (Value::Float(a), Value::Int(b)) => {
            a.partial_cmp(&(*b as f64)).ok_or_else(|| anyhow::anyhow!("NaN comparison"))
        }
        (Value::String(a), Value::String(b)) => Ok(a.cmp(b)),
        _ => Err(anyhow::anyhow!("cannot compare these types")),
    }
}

/// Convert an ExecResult to a Value.
fn result_to_value(result: &ExecResult) -> Value {
    let mut fields = vec![
        ("code".into(), Expr::Literal(Value::Int(result.code))),
        ("ok".into(), Expr::Literal(Value::Bool(result.ok()))),
        ("out".into(), Expr::Literal(Value::String(result.out.clone()))),
        ("err".into(), Expr::Literal(Value::String(result.err.clone()))),
    ];
    if let Some(data) = &result.data {
        fields.push(("data".into(), Expr::Literal(data.clone())));
    }
    Value::Object(fields)
}

const HELP_TEXT: &str = r#"会sh — kaish REPL (Layer 10: State & Persistence)

Meta Commands:
  /help, /h, /?     Show this help
  /quit, /q, /exit  Exit the REPL
  /ast              Toggle AST display mode
  /scope, /vars     Show all variables
  /result, /$?      Show last command result
  /cwd              Show current working directory
  /tools            List available tools
  /jobs             List background jobs
  /state, /session  Show session/state info
  /clear-state      Clear all persisted state

Built-in Tools:
  echo [args...]    Print arguments
  cat <path> [-n]   Read file contents (-n for line numbers)
  ls [path] [-la]   List directory (-a hidden, -l long)
  cd [path | -]     Change directory (- for previous)
  pwd               Print working directory
  mkdir <path>      Create directory
  rm <path> [-rf]   Remove file/directory
  cp <src> <dst> [-r]  Copy file/directory
  mv <src> <dst>    Move/rename
  grep <pattern> [path] [-inv]  Search patterns
  write <path> <content>  Write to file
  date [format]     Current date/time
  assert <cond>     Assert condition (for tests)
  help [tool]       Show tool help
  jobs              List background jobs
  wait [job_id]     Wait for background jobs

Language:
  set X = value     Assign a variable
  ${VAR}            Variable reference
  ${VAR.field}      Nested access
  ${?.ok}           Last result access
  a | b | c         Pipeline (connects stdout → stdin)
  cmd &             Run in background
  if cond; then ... fi
  for X in arr; do ... done

Examples:
  ls                         # List current directory
  cat file.txt | grep hello  # Pipeline: search in file
  echo hello | grep ell      # Pipeline: filter text
  sleep 5 &                  # Background job
  jobs                       # List running jobs
  wait                       # Wait for all jobs
"#;

/// Run the REPL.
pub fn run() -> Result<()> {
    println!("会sh — kaish v{} (Layer 10: State & Persistence)", env!("CARGO_PKG_VERSION"));
    println!("Type /help for commands, /quit to exit.");

    let mut rl: Editor<(), DefaultHistory> = Editor::new()
        .context("Failed to create editor")?;

    // Load history if it exists
    let history_path = dirs::data_dir()
        .map(|p| p.join("kaish").join("history.txt"));
    if let Some(ref path) = history_path {
        let _ = rl.load_history(path);
    }

    let mut repl = Repl::new()?;

    // Show state status
    if repl.state.is_some() {
        let vars = repl.scope.all_names();
        if !vars.is_empty() {
            println!("Restored {} variables from previous session.", vars.len());
        }
    }
    println!();

    loop {
        let prompt = "会sh> ";

        match rl.readline(prompt) {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());

                match repl.process_line(&line) {
                    Ok(Some(output)) => println!("{}", output),
                    Ok(None) => {}
                    Err(e) => eprintln!("Error: {}", e),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            }
            Err(err) => {
                eprintln!("Error: {}", err);
                break;
            }
        }
    }

    // Save history
    if let Some(ref path) = history_path {
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        let _ = rl.save_history(path);
    }

    Ok(())
}

/// Run a REPL connected to a remote kernel via IpcClient.
///
/// This REPL forwards commands to the remote kernel and displays results.
/// The `LocalSet` is required because IpcClient uses spawn_local internally.
pub fn run_with_client(
    client: kaish_client::IpcClient,
    rt: &Runtime,
    local: &tokio::task::LocalSet,
) -> Result<()> {
    use kaish_client::KernelClient;

    let mut rl: Editor<(), DefaultHistory> = Editor::new()
        .context("Failed to create editor")?;

    // Load history
    let history_path = dirs::data_dir()
        .map(|p| p.join("kaish").join("history.txt"));
    if let Some(ref path) = history_path {
        let _ = rl.load_history(path);
    }

    loop {
        let prompt = "会sh> ";

        match rl.readline(prompt) {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());
                let trimmed = line.trim();

                // Handle local meta-commands
                if trimmed == "/quit" || trimmed == "/q" || trimmed == "/exit" {
                    break;
                }

                if trimmed == "/help" || trimmed == "/h" || trimmed == "/?" {
                    println!("{}", CONNECTED_HELP_TEXT);
                    continue;
                }

                if trimmed == "/ping" {
                    match local.block_on(rt, client.ping()) {
                        Ok(pong) => println!("Kernel: {pong}"),
                        Err(e) => eprintln!("Ping failed: {e}"),
                    }
                    continue;
                }

                if trimmed == "/vars" || trimmed == "/scope" {
                    match local.block_on(rt, client.list_vars()) {
                        Ok(vars) => {
                            if vars.is_empty() {
                                println!("(no variables set)");
                            } else {
                                println!("Variables:");
                                for (name, value) in vars {
                                    println!("  {} = {}", name, format_value(&value));
                                }
                            }
                        }
                        Err(e) => eprintln!("Error listing variables: {e}"),
                    }
                    continue;
                }

                if trimmed == "/cwd" {
                    match local.block_on(rt, client.cwd()) {
                        Ok(cwd) => println!("{cwd}"),
                        Err(e) => eprintln!("Error getting cwd: {e}"),
                    }
                    continue;
                }

                if trimmed == "/shutdown" {
                    match local.block_on(rt, client.shutdown()) {
                        Ok(()) => {
                            println!("Kernel shutdown requested.");
                            break;
                        }
                        Err(e) => eprintln!("Shutdown failed: {e}"),
                    }
                    continue;
                }

                // Skip empty lines
                if trimmed.is_empty() {
                    continue;
                }

                // Skip unknown meta-commands
                if trimmed.starts_with('/') {
                    println!("Unknown command: {trimmed}");
                    println!("Type /help for available commands.");
                    continue;
                }

                // Execute on remote kernel
                match local.block_on(rt, client.execute(&line)) {
                    Ok(result) => {
                        if !result.out.is_empty() {
                            print!("{}", result.out);
                            if !result.out.ends_with('\n') {
                                println!();
                            }
                        }
                        if !result.err.is_empty() {
                            eprintln!("{}", result.err);
                        }
                        if !result.ok() {
                            println!("✗ exit code: {}", result.code);
                        }
                    }
                    Err(e) => eprintln!("Execution error: {e}"),
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            }
            Err(err) => {
                eprintln!("Error: {err}");
                break;
            }
        }
    }

    // Save history
    if let Some(ref path) = history_path {
        if let Some(parent) = path.parent() {
            let _ = std::fs::create_dir_all(parent);
        }
        let _ = rl.save_history(path);
    }

    Ok(())
}

const CONNECTED_HELP_TEXT: &str = r#"会sh — Connected REPL

Meta Commands:
  /help, /h, /?     Show this help
  /quit, /q, /exit  Exit the REPL
  /ping             Ping the kernel
  /vars, /scope     List all variables
  /cwd              Show current working directory
  /shutdown         Shutdown the remote kernel

All other input is sent to the remote kernel for execution.
"#;
