//! kaish REPL — Interactive shell for 会sh.
//!
//! This is an evolving REPL that grows with each layer of the kaish project.
//! Currently (L14), it provides:
//!
//! - Parse input and display AST (`/ast` toggle)
//! - Evaluate expressions with persistent scope (via ExecContext)
//! - `X=value` assignments
//! - Real tool execution via VFS
//! - Pipeline execution (`a | b | c`)
//! - Background jobs (`cmd &`) with `jobs` and `wait` commands
//! - MCP tool integration via pre-configured ToolRegistry
//! - Introspection builtins: `vars`, `tools`, `mounts`
//! - Meta-commands: `/help`, `/quit`, `/ast`, `/scope`, `/cwd`, `/jobs`, `/tools`
//! - Smart output formatting with DisplayHints for human vs. model audiences

pub mod format;

use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result};
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;
use rustyline::Editor;
use tokio::runtime::Runtime;

use kaish_kernel::ast::{Arg, Expr, Pipeline, Stmt, Value};
use kaish_kernel::interpreter::{ExecResult, Scope};

/// Result of executing a statement in the REPL.
/// Used to propagate break/continue signals from loops.
#[derive(Debug, Clone)]
enum StmtResult {
    /// Normal output from statement execution
    Output(Option<String>),
    /// Break from loop with remaining levels to break
    Break(usize),
    /// Continue to next iteration with remaining levels to skip
    Continue(usize),
}

/// Result from meta-command handling.
#[derive(Debug)]
enum MetaResult {
    /// Continue with optional output
    Continue(Option<String>),
    /// Exit the REPL (caller should save history and exit)
    Exit,
}
use kaish_kernel::parser::parse;
use kaish_kernel::scheduler::{JobManager, PipelineRunner};
use kaish_kernel::tools::{ExecContext, ToolArgs, ToolRegistry, register_builtins};
use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};

/// REPL configuration and state.
pub struct Repl {
    show_ast: bool,
    tools: Arc<ToolRegistry>,
    exec_ctx: ExecContext,
    runtime: Runtime,
    pipeline_runner: PipelineRunner,
    job_manager: Arc<JobManager>,
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
        // Build the VFS in passthrough mode — native paths work directly
        // This gives users full filesystem access, appropriate for a human-operated REPL.
        let mut vfs = VfsRouter::new();

        // Mount the real filesystem at root — /home/user/project just works
        vfs.mount("/", LocalFs::new(PathBuf::from("/")));

        // Mount memory for blobs and scratch
        vfs.mount("/v", MemoryFs::new());
        vfs.mount("/scratch", MemoryFs::new());

        // Wrap tools in Arc
        let tools = Arc::new(tools);

        // Create execution context with VFS and tools for backend dispatch
        // Use the actual cwd as passed in (from std::env::current_dir())
        let mut exec_ctx = ExecContext::with_vfs_and_tools(Arc::new(vfs), tools.clone());
        exec_ctx.set_cwd(root.clone());
        exec_ctx.set_tool_schemas(tools.schemas());

        // Create job manager and add to context
        let job_manager = Arc::new(JobManager::new());
        exec_ctx.set_job_manager(job_manager.clone());

        // Create pipeline runner
        let pipeline_runner = PipelineRunner::new(tools.clone());

        // Create tokio runtime for async tool execution
        let runtime = Runtime::new().context("Failed to create tokio runtime")?;

        Ok(Self {
            show_ast: false,
            tools,
            exec_ctx,
            runtime,
            pipeline_runner,
            job_manager,
        })
    }

    /// Process a single line of input.
    /// Returns Ok(None) for normal output, Ok(Some(output)) for output to display,
    /// or Err with ProcessResult::Exit to signal the REPL should exit.
    pub fn process_line(&mut self, line: &str) -> Result<Option<String>> {
        let trimmed = line.trim();

        // Handle meta-commands (both /cmd and cmd forms for common ones)
        if trimmed.starts_with('/') {
            return match self.handle_meta_command(trimmed) {
                MetaResult::Continue(output) => Ok(output),
                MetaResult::Exit => Err(anyhow::anyhow!("__REPL_EXIT__")),
            };
        }

        // Also support shell-style meta-commands without slash
        if let Some(meta_result) = self.try_shell_style_command(trimmed) {
            return match meta_result {
                MetaResult::Continue(output) => Ok(output),
                MetaResult::Exit => Err(anyhow::anyhow!("__REPL_EXIT__")),
            };
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
            match self.execute_stmt(&stmt)? {
                StmtResult::Output(Some(result)) => {
                    if !output.is_empty() {
                        output.push('\n');
                    }
                    output.push_str(&result);
                }
                StmtResult::Output(None) => {}
                // Break/continue at top level - just ignore (no enclosing loop)
                StmtResult::Break(_) | StmtResult::Continue(_) => {}
            }
        }

        if output.is_empty() {
            Ok(None)
        } else {
            Ok(Some(output))
        }
    }

    /// Execute a single statement.
    fn execute_stmt(&mut self, stmt: &Stmt) -> Result<StmtResult> {
        match stmt {
            Stmt::Assignment(assign) => {
                let value = self.eval_expr(&assign.value)?;
                self.exec_ctx.scope.set(&assign.name, value.clone());
                Ok(StmtResult::Output(Some(format!("{} = {}", assign.name, format_value(&value)))))
            }
            Stmt::Command(cmd) => {
                let result = self.execute_command(&cmd.name, &cmd.args)?;
                self.exec_ctx.scope.set_last_result(result.clone());
                Ok(StmtResult::Output(Some(format_result(&result))))
            }
            Stmt::Pipeline(pipeline) => {
                let result = self.execute_pipeline(pipeline)?;
                self.exec_ctx.scope.set_last_result(result.clone());
                Ok(StmtResult::Output(Some(format_result(&result))))
            }
            Stmt::If(if_stmt) => {
                let cond_value = self.eval_expr(&if_stmt.condition)?;
                let branch = if is_truthy(&cond_value) {
                    &if_stmt.then_branch
                } else {
                    if_stmt.else_branch.as_deref().unwrap_or(&[])
                };

                let mut output = String::new();
                for stmt in branch {
                    match self.execute_stmt(stmt)? {
                        StmtResult::Output(Some(result)) => {
                            if !output.is_empty() {
                                output.push('\n');
                            }
                            output.push_str(&result);
                        }
                        StmtResult::Output(None) => {}
                        // Propagate break/continue up
                        flow @ (StmtResult::Break(_) | StmtResult::Continue(_)) => {
                            return Ok(flow);
                        }
                    }
                }
                Ok(StmtResult::Output(if output.is_empty() { None } else { Some(output) }))
            }
            Stmt::For(for_loop) => {
                // Evaluate all items and collect words for iteration
                let mut words: Vec<String> = Vec::new();
                for item_expr in &for_loop.items {
                    let item = self.eval_expr(item_expr)?;
                    // POSIX-style word splitting
                    match &item {
                        Value::String(s) => {
                            words.extend(s.split_whitespace().map(String::from));
                        }
                        _ => words.push(format_value_unquoted(&item)),
                    }
                }

                self.exec_ctx.scope.push_frame();
                let mut output = String::new();

                'outer: for word in words {
                    self.exec_ctx.scope.set(&for_loop.variable, Value::String(word));
                    for stmt in &for_loop.body {
                        match self.execute_stmt(stmt)? {
                            StmtResult::Output(Some(result)) => {
                                if !output.is_empty() {
                                    output.push('\n');
                                }
                                output.push_str(&result);
                            }
                            StmtResult::Output(None) => {}
                            StmtResult::Break(levels) => {
                                if levels <= 1 {
                                    // Break this loop
                                    break 'outer;
                                } else {
                                    // Break outer loop(s)
                                    self.exec_ctx.scope.pop_frame();
                                    return Ok(StmtResult::Break(levels - 1));
                                }
                            }
                            StmtResult::Continue(levels) => {
                                if levels <= 1 {
                                    // Continue to next iteration
                                    continue 'outer;
                                } else {
                                    // Continue in outer loop
                                    self.exec_ctx.scope.pop_frame();
                                    return Ok(StmtResult::Continue(levels - 1));
                                }
                            }
                        }
                    }
                }

                self.exec_ctx.scope.pop_frame();
                Ok(StmtResult::Output(if output.is_empty() { None } else { Some(output) }))
            }
            Stmt::ToolDef(tool) => {
                Ok(StmtResult::Output(Some(format!("Defined tool: {}", tool.name))))
            }
            Stmt::AndChain { left, right } => {
                // Run right only if left succeeds
                let left_result = self.execute_stmt(left)?;
                // Check if we should run right (last result was success)
                if self.exec_ctx.scope.last_result().ok() {
                    self.execute_stmt(right)
                } else {
                    Ok(left_result)
                }
            }
            Stmt::OrChain { left, right } => {
                // Run right only if left fails
                let left_result = self.execute_stmt(left)?;
                // Check if we should run right (last result was failure)
                if !self.exec_ctx.scope.last_result().ok() {
                    self.execute_stmt(right)
                } else {
                    Ok(left_result)
                }
            }
            Stmt::While(while_loop) => {
                let mut output = String::new();
                'while_loop: loop {
                    let cond_value = self.eval_expr(&while_loop.condition)?;
                    if !is_truthy(&cond_value) {
                        break;
                    }
                    for stmt in &while_loop.body {
                        match self.execute_stmt(stmt)? {
                            StmtResult::Output(Some(result)) => {
                                if !output.is_empty() {
                                    output.push('\n');
                                }
                                output.push_str(&result);
                            }
                            StmtResult::Output(None) => {}
                            StmtResult::Break(levels) => {
                                if levels <= 1 {
                                    break 'while_loop;
                                } else {
                                    return Ok(StmtResult::Break(levels - 1));
                                }
                            }
                            StmtResult::Continue(levels) => {
                                if levels <= 1 {
                                    continue 'while_loop;
                                } else {
                                    return Ok(StmtResult::Continue(levels - 1));
                                }
                            }
                        }
                    }
                }
                Ok(StmtResult::Output(if output.is_empty() { None } else { Some(output) }))
            }
            Stmt::Case(case_stmt) => {
                // Evaluate the expression to match against
                let match_value = {
                    let value = self.eval_expr(&case_stmt.expr)?;
                    // Use unquoted format for matching - quotes would break pattern matching
                    format_value_unquoted(&value)
                };

                // Try each branch until we find a match
                for branch in &case_stmt.branches {
                    let matched = branch.patterns.iter().any(|pattern| {
                        kaish_kernel::glob::glob_match(pattern, &match_value)
                    });

                    if matched {
                        // Execute the branch body
                        let mut output = String::new();
                        for stmt in &branch.body {
                            match self.execute_stmt(stmt)? {
                                StmtResult::Output(Some(result)) => {
                                    if !output.is_empty() {
                                        output.push('\n');
                                    }
                                    output.push_str(&result);
                                }
                                StmtResult::Output(None) => {}
                                // Propagate break/continue from case branches
                                flow @ (StmtResult::Break(_) | StmtResult::Continue(_)) => {
                                    return Ok(flow);
                                }
                            }
                        }
                        return Ok(StmtResult::Output(if output.is_empty() { None } else { Some(output) }));
                    }
                }

                // No match - return nothing (like bash)
                Ok(StmtResult::Output(None))
            }
            Stmt::Break(levels) => {
                // Break from loop - return break signal with level count
                Ok(StmtResult::Break(levels.unwrap_or(1)))
            }
            Stmt::Continue(levels) => {
                // Continue in loop - return continue signal with level count
                Ok(StmtResult::Continue(levels.unwrap_or(1)))
            }
            Stmt::Return(expr) => {
                // Return in REPL context - evaluate and return value if present
                if let Some(e) = expr {
                    let value = self.eval_expr(e)?;
                    Ok(StmtResult::Output(Some(format!("return {}", format_value(&value)))))
                } else {
                    Ok(StmtResult::Output(Some("return".into())))
                }
            }
            Stmt::Exit(expr) => {
                // Exit in REPL context - just show message
                if let Some(e) = expr {
                    let value = self.eval_expr(e)?;
                    Ok(StmtResult::Output(Some(format!("exit {}", format_value(&value)))))
                } else {
                    Ok(StmtResult::Output(Some("exit".into())))
                }
            }
            Stmt::Test(test_expr) => {
                // Evaluate the test expression
                let expr = kaish_kernel::ast::Expr::Test(Box::new(test_expr.clone()));
                let value = self.eval_expr(&expr)?;
                let is_true = match value {
                    Value::Bool(b) => b,
                    _ => false,
                };
                Ok(StmtResult::Output(Some(format!("test: {}", is_true))))
            }
            Stmt::Empty => Ok(StmtResult::Output(None)),
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
                Arg::DoubleDash => {
                    // Marker for end of flags - no action needed
                }
            }
        }

        // Execute the tool asynchronously
        let result = self.runtime.block_on(tool.execute(tool_args, &mut self.exec_ctx));

        // Sync cwd back to scope if cd was called
        if name == "cd" && result.ok() {
            let cwd_str = self.exec_ctx.cwd.to_string_lossy().to_string();
            // Update scope with new cwd for display
            self.exec_ctx.scope.set("CWD", Value::String(cwd_str));
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
            let backend = self.exec_ctx.backend.clone();
            let cwd = self.exec_ctx.cwd.clone();
            let scope = self.exec_ctx.scope.clone();
            let job_manager = self.job_manager.clone();

            let job_id = self.runtime.block_on(async {
                let (tx, rx) = tokio::sync::oneshot::channel();

                let id = job_manager.register(pipeline_str.clone(), rx).await;

                tokio::spawn(async move {
                    let mut ctx = ExecContext::with_backend_and_scope(backend, scope);
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
                self.exec_ctx.scope.resolve_path(path)
                    .ok_or_else(|| anyhow::anyhow!("undefined variable"))
            }
            Expr::Interpolated(parts) => {
                let mut result = String::new();
                for part in parts {
                    match part {
                        kaish_kernel::ast::StringPart::Literal(s) => result.push_str(s),
                        kaish_kernel::ast::StringPart::Var(path) => {
                            let value = self.exec_ctx.scope.resolve_path(path)
                                .ok_or_else(|| anyhow::anyhow!("undefined variable in interpolation"))?;
                            result.push_str(&format_value_unquoted(&value));
                        }
                        kaish_kernel::ast::StringPart::VarWithDefault { name, default } => {
                            match self.exec_ctx.scope.get(name) {
                                Some(value) => {
                                    let s = format_value_unquoted(value);
                                    if s.is_empty() {
                                        result.push_str(&self.eval_string_parts(default)?);
                                    } else {
                                        result.push_str(&s);
                                    }
                                }
                                None => result.push_str(&self.eval_string_parts(default)?),
                            }
                        }
                        kaish_kernel::ast::StringPart::VarLength(name) => {
                            let len = match self.exec_ctx.scope.get(name) {
                                Some(value) => format_value_unquoted(value).len(),
                                None => 0,
                            };
                            result.push_str(&len.to_string());
                        }
                        kaish_kernel::ast::StringPart::Positional(n) => {
                            if let Some(s) = self.exec_ctx.scope.get_positional(*n) {
                                result.push_str(s);
                            }
                        }
                        kaish_kernel::ast::StringPart::AllArgs => {
                            result.push_str(&self.exec_ctx.scope.all_args().join(" "));
                        }
                        kaish_kernel::ast::StringPart::ArgCount => {
                            result.push_str(&self.exec_ctx.scope.arg_count().to_string());
                        }
                        kaish_kernel::ast::StringPart::Arithmetic(expr) => {
                            // Evaluate arithmetic in REPL context
                            if let Ok(value) = kaish_kernel::arithmetic::eval_arithmetic(expr, &self.exec_ctx.scope) {
                                result.push_str(&value.to_string());
                            }
                        }
                        kaish_kernel::ast::StringPart::CommandSubst(pipeline) => {
                            // Execute the pipeline and capture its output
                            let exec_result = self.execute_pipeline(pipeline)?;
                            // Use stdout as the substitution value, with trailing newline stripped
                            result.push_str(exec_result.out.trim_end_matches('\n'));
                        }
                        kaish_kernel::ast::StringPart::LastExitCode => {
                            result.push_str(&self.exec_ctx.scope.last_result().code.to_string());
                        }
                        kaish_kernel::ast::StringPart::CurrentPid => {
                            result.push_str(&self.exec_ctx.scope.pid().to_string());
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
                    BinaryOp::Match | BinaryOp::NotMatch => {
                        let l = self.eval_expr_inner(left)?;
                        let r = self.eval_expr_inner(right)?;
                        let text = match &l {
                            Value::String(s) => s.as_str(),
                            _ => anyhow::bail!("=~ requires string on left, got {:?}", l),
                        };
                        let pattern = match &r {
                            Value::String(s) => s.as_str(),
                            _ => anyhow::bail!("=~ requires regex pattern string on right, got {:?}", r),
                        };
                        let re = regex::Regex::new(pattern)
                            .map_err(|e| anyhow::anyhow!("invalid regex: {}", e))?;
                        let matches = re.is_match(text);
                        let result = match op {
                            BinaryOp::Match => matches,
                            BinaryOp::NotMatch => !matches,
                            _ => unreachable!(),
                        };
                        Ok(Value::Bool(result))
                    }
                }
            }
            Expr::CommandSubst(pipeline) => {
                // Execute the command and return its result as an object
                let result = self.execute_pipeline(pipeline)?;
                self.exec_ctx.scope.set_last_result(result.clone());
                Ok(result_to_value(&result))
            }
            Expr::Test(test_expr) => {
                let result = self.eval_test_expr(test_expr)?;
                Ok(Value::Bool(result))
            }
            Expr::Positional(n) => {
                // Positional parameters not supported in REPL context
                match self.exec_ctx.scope.get_positional(*n) {
                    Some(s) => Ok(Value::String(s.to_string())),
                    None => Ok(Value::String(String::new())),
                }
            }
            Expr::AllArgs => {
                // Return all args as space-separated string (POSIX-style)
                let args = self.exec_ctx.scope.all_args();
                Ok(Value::String(args.join(" ")))
            }
            Expr::ArgCount => {
                Ok(Value::Int(self.exec_ctx.scope.arg_count() as i64))
            }
            Expr::VarLength(name) => {
                match self.exec_ctx.scope.get(name) {
                    Some(value) => {
                        let s = format_value_unquoted(value);
                        Ok(Value::Int(s.len() as i64))
                    }
                    None => Ok(Value::Int(0)),
                }
            }
            Expr::VarWithDefault { name, default } => {
                match self.exec_ctx.scope.get(name) {
                    Some(value) => {
                        let s = format_value_unquoted(value);
                        if s.is_empty() {
                            Ok(Value::String(self.eval_string_parts(default)?))
                        } else {
                            Ok(value.clone())
                        }
                    }
                    None => Ok(Value::String(self.eval_string_parts(default)?)),
                }
            }
            Expr::Arithmetic(expr_str) => {
                kaish_kernel::arithmetic::eval_arithmetic(expr_str, &self.exec_ctx.scope)
                    .map(Value::Int)
                    .map_err(|e| anyhow::anyhow!("arithmetic error: {}", e))
            }
            Expr::Command(cmd) => {
                // Execute command and check exit code for truthiness
                let result = self.execute_command(&cmd.name, &cmd.args)?;
                Ok(Value::Bool(result.code == 0))
            }
            Expr::LastExitCode => {
                Ok(Value::Int(self.exec_ctx.scope.last_result().code))
            }
            Expr::CurrentPid => {
                Ok(Value::Int(self.exec_ctx.scope.pid() as i64))
            }
        }
    }

    fn eval_literal(&mut self, value: &Value) -> Result<Value> {
        Ok(value.clone())
    }

    /// Evaluate a test expression `[[ ... ]]` to a boolean.
    fn eval_test_expr(&mut self, test_expr: &kaish_kernel::ast::TestExpr) -> Result<bool> {
        use kaish_kernel::ast::{TestExpr, FileTestOp, StringTestOp, TestCmpOp};
        use std::path::Path;

        match test_expr {
            TestExpr::FileTest { op, path } => {
                let path_val = self.eval_expr_inner(path)?;
                let path_str = format_value_unquoted(&path_val);
                let p = Path::new(&path_str);
                Ok(match op {
                    FileTestOp::Exists => p.exists(),
                    FileTestOp::IsFile => p.is_file(),
                    FileTestOp::IsDir => p.is_dir(),
                    FileTestOp::Readable => p.exists(),
                    FileTestOp::Writable => p.exists() && std::fs::OpenOptions::new().write(true).open(p).is_ok(),
                    FileTestOp::Executable => {
                        #[cfg(unix)]
                        {
                            use std::os::unix::fs::PermissionsExt;
                            p.metadata().map(|m| m.permissions().mode() & 0o111 != 0).unwrap_or(false)
                        }
                        #[cfg(not(unix))]
                        { p.exists() }
                    }
                })
            }
            TestExpr::StringTest { op, value } => {
                let val = self.eval_expr_inner(value)?;
                let s = format_value_unquoted(&val);
                Ok(match op {
                    StringTestOp::IsEmpty => s.is_empty(),
                    StringTestOp::IsNonEmpty => !s.is_empty(),
                })
            }
            TestExpr::Comparison { left, op, right } => {
                let l = self.eval_expr_inner(left)?;
                let r = self.eval_expr_inner(right)?;
                Ok(match op {
                    TestCmpOp::Eq => values_equal(&l, &r),
                    TestCmpOp::NotEq => !values_equal(&l, &r),
                    TestCmpOp::Match | TestCmpOp::NotMatch => {
                        let text = match &l {
                            Value::String(s) => s.as_str(),
                            _ => anyhow::bail!("=~ requires string on left, got {:?}", l),
                        };
                        let pattern = match &r {
                            Value::String(s) => s.as_str(),
                            _ => anyhow::bail!("=~ requires regex pattern string on right, got {:?}", r),
                        };
                        let re = regex::Regex::new(pattern)
                            .map_err(|e| anyhow::anyhow!("invalid regex: {}", e))?;
                        let matches = re.is_match(text);
                        match op {
                            TestCmpOp::Match => matches,
                            TestCmpOp::NotMatch => !matches,
                            _ => unreachable!(),
                        }
                    }
                    TestCmpOp::Gt | TestCmpOp::Lt | TestCmpOp::GtEq | TestCmpOp::LtEq => {
                        let lnum = value_to_f64(&l);
                        let rnum = value_to_f64(&r);
                        match op {
                            TestCmpOp::Gt => lnum > rnum,
                            TestCmpOp::Lt => lnum < rnum,
                            TestCmpOp::GtEq => lnum >= rnum,
                            TestCmpOp::LtEq => lnum <= rnum,
                            _ => unreachable!(),
                        }
                    }
                })
            }
            TestExpr::And { left, right } => {
                // Short-circuit: if left is false, don't evaluate right
                if !self.eval_test_expr(left)? {
                    Ok(false)
                } else {
                    self.eval_test_expr(right)
                }
            }
            TestExpr::Or { left, right } => {
                // Short-circuit: if left is true, don't evaluate right
                if self.eval_test_expr(left)? {
                    Ok(true)
                } else {
                    self.eval_test_expr(right)
                }
            }
            TestExpr::Not { expr } => {
                Ok(!self.eval_test_expr(expr)?)
            }
        }
    }

    /// Evaluate multiple string parts into a single string (for nested default values).
    fn eval_string_parts(&mut self, parts: &[kaish_kernel::ast::StringPart]) -> Result<String> {
        let mut result = String::new();
        for part in parts {
            match part {
                kaish_kernel::ast::StringPart::Literal(s) => result.push_str(s),
                kaish_kernel::ast::StringPart::Var(path) => {
                    if let Some(value) = self.exec_ctx.scope.resolve_path(path) {
                        result.push_str(&format_value_unquoted(&value));
                    }
                }
                kaish_kernel::ast::StringPart::VarWithDefault { name, default } => {
                    match self.exec_ctx.scope.get(name) {
                        Some(value) => {
                            let s = format_value_unquoted(value);
                            if s.is_empty() {
                                result.push_str(&self.eval_string_parts(default)?);
                            } else {
                                result.push_str(&s);
                            }
                        }
                        None => result.push_str(&self.eval_string_parts(default)?),
                    }
                }
                kaish_kernel::ast::StringPart::VarLength(name) => {
                    let len = match self.exec_ctx.scope.get(name) {
                        Some(value) => format_value_unquoted(value).len(),
                        None => 0,
                    };
                    result.push_str(&len.to_string());
                }
                kaish_kernel::ast::StringPart::Positional(n) => {
                    if let Some(s) = self.exec_ctx.scope.get_positional(*n) {
                        result.push_str(s);
                    }
                }
                kaish_kernel::ast::StringPart::AllArgs => {
                    result.push_str(&self.exec_ctx.scope.all_args().join(" "));
                }
                kaish_kernel::ast::StringPart::ArgCount => {
                    result.push_str(&self.exec_ctx.scope.arg_count().to_string());
                }
                kaish_kernel::ast::StringPart::Arithmetic(expr) => {
                    if let Ok(value) = kaish_kernel::arithmetic::eval_arithmetic(expr, &self.exec_ctx.scope) {
                        result.push_str(&value.to_string());
                    }
                }
                kaish_kernel::ast::StringPart::CommandSubst(pipeline) => {
                    // Execute the pipeline and capture its output
                    let exec_result = self.execute_pipeline(pipeline)?;
                    // Use stdout as the substitution value, with trailing newline stripped
                    result.push_str(exec_result.out.trim_end_matches('\n'));
                }
                kaish_kernel::ast::StringPart::LastExitCode => {
                    result.push_str(&self.exec_ctx.scope.last_result().code.to_string());
                }
                kaish_kernel::ast::StringPart::CurrentPid => {
                    result.push_str(&self.exec_ctx.scope.pid().to_string());
                }
            }
        }
        Ok(result)
    }

    /// Handle a meta-command (starts with /).
    fn handle_meta_command(&mut self, cmd: &str) -> MetaResult {
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        let command = parts.first().copied().unwrap_or("");

        match command {
            "/quit" | "/q" | "/exit" => {
                MetaResult::Exit
            }
            "/help" | "/h" | "/?" => {
                MetaResult::Continue(Some(HELP_TEXT.to_string()))
            }
            "/ast" => {
                self.show_ast = !self.show_ast;
                MetaResult::Continue(Some(format!("AST mode: {}", if self.show_ast { "ON" } else { "OFF" })))
            }
            "/scope" | "/vars" => {
                let names = self.exec_ctx.scope.all_names();
                if names.is_empty() {
                    MetaResult::Continue(Some("(no variables set)".to_string()))
                } else {
                    let mut output = String::from("Variables:\n");
                    for name in names {
                        if let Some(value) = self.exec_ctx.scope.get(name) {
                            output.push_str(&format!("  {} = {}\n", name, format_value(value)));
                        }
                    }
                    MetaResult::Continue(Some(output.trim_end().to_string()))
                }
            }
            "/result" | "/$?" => {
                let result = self.exec_ctx.scope.last_result();
                MetaResult::Continue(Some(format_result(result)))
            }
            "/cwd" => {
                MetaResult::Continue(Some(self.exec_ctx.cwd.to_string_lossy().to_string()))
            }
            "/tools" => {
                let names = self.tools.names();
                MetaResult::Continue(Some(format!("Available tools: {}", names.join(", "))))
            }
            "/jobs" => {
                let jobs = self.runtime.block_on(self.job_manager.list());
                if jobs.is_empty() {
                    MetaResult::Continue(Some("(no background jobs)".to_string()))
                } else {
                    let mut output = String::from("Background jobs:\n");
                    for job in jobs {
                        output.push_str(&format!("  [{}] {} {}\n", job.id, job.status, job.command));
                    }
                    MetaResult::Continue(Some(output.trim_end().to_string()))
                }
            }
            "/state" | "/session" => {
                MetaResult::Continue(Some(format!(
                    "State persistence disabled\nVariables (in-memory): {}",
                    self.exec_ctx.scope.all().len()
                )))
            }
            "/clear-state" | "/reset" => {
                // Clear in-memory scope
                self.exec_ctx.scope = Scope::new();
                MetaResult::Continue(Some("Session reset (variables cleared)".to_string()))
            }
            _ => {
                MetaResult::Continue(Some(format!("Unknown command: {}\nType /help or help for available commands.", command)))
            }
        }
    }

    /// Try to handle a shell-style command (without leading /).
    /// Returns Some(result) if it was a recognized command, None otherwise.
    ///
    /// Note: We only intercept commands that don't have builtin tool equivalents.
    /// For example, `vars` is a real builtin tool, so we don't intercept it.
    fn try_shell_style_command(&mut self, cmd: &str) -> Option<MetaResult> {
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        let command = parts.first().copied().unwrap_or("");

        match command {
            "quit" | "exit" => Some(self.handle_meta_command("/quit")),
            "help" => Some(self.handle_meta_command("/help")),
            "reset" => Some(self.handle_meta_command("/reset")),
            _ => None,
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
        Value::Json(json) => json.to_string(),
        Value::Blob(blob) => format!("[blob: {} {}]", blob.formatted_size(), blob.content_type),
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
///
/// Uses display hints when available, otherwise falls back to status+output format.
fn format_result(result: &ExecResult) -> String {
    use kaish_kernel::interpreter::DisplayHint;

    // If there's a display hint, use the formatter
    if !matches!(result.hint, DisplayHint::None) {
        let context = format::detect_context();
        let formatted = format::format_output(result, context);

        // For failures, append error info
        if !result.ok() && !result.err.is_empty() {
            return format!("{}\n✗ code={} err=\"{}\"", formatted, result.code, result.err);
        }
        return formatted;
    }

    // No display hint - use classic status format
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
    // Delegate to kaish_kernel's exported value_to_bool function
    kaish_kernel::interpreter::value_to_bool(value)
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

/// Convert a value to f64 for numeric comparisons.
fn value_to_f64(value: &Value) -> f64 {
    match value {
        Value::Int(i) => *i as f64,
        Value::Float(f) => *f,
        Value::String(s) => s.parse::<f64>().unwrap_or(0.0),
        Value::Bool(b) => if *b { 1.0 } else { 0.0 },
        _ => 0.0,
    }
}

/// Convert an ExecResult to a Value (bash-compatible: return stdout string).
fn result_to_value(result: &ExecResult) -> Value {
    Value::String(result.out.trim_end().to_string())
}

const HELP_TEXT: &str = r#"会sh — kaish REPL

Meta Commands (use with or without /):
  help, /help, /?   Show this help
  quit, /quit, /q   Exit the REPL
  reset, /reset     Clear in-memory state

Slash-only commands:
  /ast              Toggle AST display mode
  /scope, /vars     Show all variables (alt: `vars` builtin)
  /result, /$?      Show last command result
  /cwd              Show current working directory
  /tools            List available tools (alt: `tools` builtin)
  /jobs             List background jobs
  /state, /session  Show session info

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
  X=value     Assign a variable
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

/// Save REPL history to disk.
fn save_history(rl: &mut Editor<(), DefaultHistory>, history_path: &Option<PathBuf>) {
    if let Some(path) = history_path {
        if let Some(parent) = path.parent() {
            if let Err(e) = std::fs::create_dir_all(parent) {
                tracing::warn!("Failed to create history directory: {}", e);
            }
        }
        if let Err(e) = rl.save_history(path) {
            tracing::warn!("Failed to save history: {}", e);
        }
    }
}

/// Run the REPL.
pub fn run() -> Result<()> {
    println!("会sh — kaish v{}", env!("CARGO_PKG_VERSION"));
    println!("Type /help for commands, /quit to exit.");

    let mut rl: Editor<(), DefaultHistory> = Editor::new()
        .context("Failed to create editor")?;

    // Load history if it exists
    let history_path = directories::BaseDirs::new()
        .map(|b| b.data_dir().join("kaish").join("history.txt"));
    if let Some(ref path) = history_path {
        if let Err(e) = rl.load_history(path) {
            // Only log if it's not a "file not found" error (expected on first run)
            let is_not_found = matches!(&e, ReadlineError::Io(io_err) if io_err.kind() == std::io::ErrorKind::NotFound);
            if !is_not_found {
                tracing::warn!("Failed to load history: {}", e);
            }
        }
    }

    let mut repl = Repl::new()?;
    println!();

    loop {
        let prompt = "会sh> ";

        match rl.readline(prompt) {
            Ok(line) => {
                if let Err(e) = rl.add_history_entry(line.as_str()) {
                    tracing::warn!("Failed to add history entry: {}", e);
                }

                match repl.process_line(&line) {
                    Ok(Some(output)) => println!("{}", output),
                    Ok(None) => {}
                    Err(e) if e.to_string() == "__REPL_EXIT__" => {
                        // User requested exit - save history and break
                        save_history(&mut rl, &history_path);
                        return Ok(());
                    }
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
    save_history(&mut rl, &history_path);

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
    let history_path = directories::BaseDirs::new()
        .map(|b| b.data_dir().join("kaish").join("history.txt"));
    if let Some(ref path) = history_path {
        if let Err(e) = rl.load_history(path) {
            let is_not_found = matches!(&e, ReadlineError::Io(io_err) if io_err.kind() == std::io::ErrorKind::NotFound);
            if !is_not_found {
                tracing::warn!("Failed to load history: {}", e);
            }
        }
    }

    loop {
        let prompt = "会sh> ";

        match rl.readline(prompt) {
            Ok(line) => {
                if let Err(e) = rl.add_history_entry(line.as_str()) {
                    tracing::warn!("Failed to add history entry: {}", e);
                }
                let trimmed = line.trim();

                // Handle local meta-commands
                if trimmed == "/quit" || trimmed == "/q" || trimmed == "/exit" {
                    save_history(&mut rl, &history_path);
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
                            save_history(&mut rl, &history_path);
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
    save_history(&mut rl, &history_path);

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
