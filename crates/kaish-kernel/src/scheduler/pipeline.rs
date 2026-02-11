//! Pipeline execution for kaish.
//!
//! Executes a sequence of commands connected by pipes, where the stdout
//! of each command becomes the stdin of the next.
//!
//! Also handles scatter/gather pipelines for parallel execution.

use std::sync::Arc;

use std::collections::HashMap;

use crate::arithmetic;
use crate::ast::{Arg, Command, Expr, Redirect, RedirectKind, Value};
use crate::dispatch::{CommandDispatcher, PipelinePosition};
use crate::interpreter::{apply_output_format, ExecResult};
use crate::tools::{extract_output_format, ExecContext, ToolArgs, ToolRegistry, ToolSchema};
use tokio::io::AsyncWriteExt;

use super::scatter::{
    parse_gather_options, parse_scatter_options, ScatterGatherRunner,
};

/// Apply redirects to an execution result.
///
/// Pre-execution redirects (Stdin, HereDoc) should be handled before calling.
/// Post-execution redirects (stdout/stderr to file, merge) applied here.
/// Redirects are processed left-to-right per POSIX.
async fn apply_redirects(
    mut result: ExecResult,
    redirects: &[Redirect],
    ctx: &ExecContext,
) -> ExecResult {
    for redir in redirects {
        match redir.kind {
            RedirectKind::MergeStderr => {
                // 2>&1 - append stderr to stdout
                if !result.err.is_empty() {
                    result.out.push_str(&result.err);
                    result.err.clear();
                }
            }
            RedirectKind::MergeStdout => {
                // 1>&2 or >&2 - append stdout to stderr
                if !result.out.is_empty() {
                    result.err.push_str(&result.out);
                    result.out.clear();
                }
            }
            RedirectKind::StdoutOverwrite => {
                if let Some(path) = eval_redirect_target(&redir.target, ctx) {
                    if let Err(e) = tokio::fs::write(&path, &result.out).await {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                    result.out.clear();
                }
            }
            RedirectKind::StdoutAppend => {
                if let Some(path) = eval_redirect_target(&redir.target, ctx) {
                    let file = tokio::fs::OpenOptions::new()
                        .append(true)
                        .create(true)
                        .open(&path)
                        .await;
                    match file {
                        Ok(mut f) => {
                            if let Err(e) = f.write_all(result.out.as_bytes()).await {
                                return ExecResult::failure(1, format!("redirect: {e}"));
                            }
                        }
                        Err(e) => return ExecResult::failure(1, format!("redirect: {e}")),
                    }
                    result.out.clear();
                }
            }
            RedirectKind::Stderr => {
                if let Some(path) = eval_redirect_target(&redir.target, ctx) {
                    if let Err(e) = tokio::fs::write(&path, &result.err).await {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                    result.err.clear();
                }
            }
            RedirectKind::Both => {
                if let Some(path) = eval_redirect_target(&redir.target, ctx) {
                    let combined = format!("{}{}", result.out, result.err);
                    if let Err(e) = tokio::fs::write(&path, combined).await {
                        return ExecResult::failure(1, format!("redirect: {e}"));
                    }
                    result.out.clear();
                    result.err.clear();
                }
            }
            // Pre-execution redirects - already handled before command execution
            RedirectKind::Stdin | RedirectKind::HereDoc => {}
        }
    }
    result
}

/// Evaluate a redirect target expression to get the file path.
fn eval_redirect_target(expr: &Expr, ctx: &ExecContext) -> Option<String> {
    eval_simple_expr(expr, ctx).map(|v| value_to_string(&v))
}

/// Set up stdin from redirects (< file, <<heredoc).
/// Called before command execution.
fn setup_stdin_redirects(cmd: &Command, ctx: &mut ExecContext) {
    for redir in &cmd.redirects {
        match &redir.kind {
            RedirectKind::Stdin => {
                if let Some(path) = eval_redirect_target(&redir.target, ctx)
                    && let Ok(content) = std::fs::read_to_string(&path) {
                        ctx.set_stdin(content);
                    }
            }
            RedirectKind::HereDoc => {
                if let Expr::Literal(Value::String(content)) = &redir.target {
                    ctx.set_stdin(content.clone());
                }
            }
            _ => {}
        }
    }
}

/// Runs pipelines by spawning tasks and connecting them via channels.
#[derive(Clone)]
pub struct PipelineRunner {
    tools: Arc<ToolRegistry>,
}

impl PipelineRunner {
    /// Create a new pipeline runner with the given tool registry.
    pub fn new(tools: Arc<ToolRegistry>) -> Self {
        Self { tools }
    }

    /// Execute a pipeline of commands.
    ///
    /// Each command's stdout becomes the next command's stdin.
    /// If the pipeline contains scatter/gather, delegates to ScatterGatherRunner.
    /// Returns the result of the last command in the pipeline.
    ///
    /// The `dispatcher` handles the full command resolution chain (user tools,
    /// builtins, scripts, external commands, backend tools). The runner handles
    /// I/O routing: stdin redirects, piping between commands, and output redirects.
    pub async fn run(
        &self,
        commands: &[Command],
        ctx: &mut ExecContext,
        dispatcher: &dyn CommandDispatcher,
    ) -> ExecResult {
        if commands.is_empty() {
            return ExecResult::success("");
        }

        // Check for scatter/gather pipeline
        if let Some((scatter_idx, gather_idx)) = find_scatter_gather(commands) {
            return self.run_scatter_gather(commands, scatter_idx, gather_idx, ctx).await;
        }

        if commands.len() == 1 {
            // Single command, no piping needed
            return self.run_single(&commands[0], ctx, None, dispatcher).await;
        }

        // Multi-command pipeline
        self.run_pipeline(commands, ctx, dispatcher).await
    }

    /// Run a scatter/gather pipeline.
    async fn run_scatter_gather(
        &self,
        commands: &[Command],
        scatter_idx: usize,
        gather_idx: usize,
        ctx: &mut ExecContext,
    ) -> ExecResult {
        // Split pipeline into parts
        let pre_scatter = &commands[..scatter_idx];
        let scatter_cmd = &commands[scatter_idx];
        let parallel = &commands[scatter_idx + 1..gather_idx];
        let gather_cmd = &commands[gather_idx];
        let post_gather = &commands[gather_idx + 1..];

        // Parse options from scatter and gather commands
        // These are builtins with simple key=value syntax, no schema-driven parsing needed
        let scatter_schema = self.tools.get("scatter").map(|t| t.schema());
        let gather_schema = self.tools.get("gather").map(|t| t.schema());
        let scatter_opts = parse_scatter_options(&build_tool_args(&scatter_cmd.args, ctx, scatter_schema.as_ref()));
        let gather_opts = parse_gather_options(&build_tool_args(&gather_cmd.args, ctx, gather_schema.as_ref()));

        // Run with ScatterGatherRunner
        let runner = ScatterGatherRunner::new(self.tools.clone());
        runner
            .run(
                pre_scatter,
                scatter_opts,
                parallel,
                gather_opts,
                post_gather,
                ctx,
            )
            .await
    }

    /// Run a single command with optional stdin.
    ///
    /// The dispatcher handles arg parsing, schema lookup, output format, and execution.
    /// The runner handles stdin setup (redirects + pipeline) and output redirects.
    async fn run_single(
        &self,
        cmd: &Command,
        ctx: &mut ExecContext,
        stdin: Option<String>,
        dispatcher: &dyn CommandDispatcher,
    ) -> ExecResult {
        // Set up stdin from redirects (< file, <<heredoc)
        setup_stdin_redirects(cmd, ctx);

        // Set stdin from pipeline (overrides redirect stdin)
        if let Some(input) = stdin {
            ctx.set_stdin(input);
        }

        // Set pipeline position for stdio inheritance decisions
        ctx.pipeline_position = PipelinePosition::Only;

        // Execute via dispatcher (full resolution chain)
        let result = match dispatcher.dispatch(cmd, ctx).await {
            Ok(result) => result,
            Err(e) => ExecResult::failure(1, e.to_string()),
        };

        // Apply post-execution redirects
        apply_redirects(result, &cmd.redirects, ctx).await
    }

    /// Run a multi-command pipeline.
    ///
    /// Each command's stdout becomes the next command's stdin.
    /// The dispatcher handles execution; the runner handles I/O routing.
    async fn run_pipeline(
        &self,
        commands: &[Command],
        ctx: &mut ExecContext,
        dispatcher: &dyn CommandDispatcher,
    ) -> ExecResult {
        let mut current_stdin: Option<String> = None;
        let mut current_data: Option<Value> = None;
        let mut last_result = ExecResult::success("");
        let last_idx = commands.len() - 1;

        for (i, cmd) in commands.iter().enumerate() {
            // Set up stdin from redirects (< file, <<heredoc)
            setup_stdin_redirects(cmd, ctx);

            // Set stdin from previous command's stdout (overrides redirect stdin)
            // Also pass structured data if available from previous command
            if let Some(input) = current_stdin.take() {
                ctx.set_stdin_with_data(input, current_data.take());
            }

            // Set pipeline position for stdio inheritance decisions
            ctx.pipeline_position = match i {
                0 => PipelinePosition::First,
                n if n == last_idx => PipelinePosition::Last,
                _ => PipelinePosition::Middle,
            };

            // Execute via dispatcher (full resolution chain)
            last_result = match dispatcher.dispatch(cmd, ctx).await {
                Ok(result) => result,
                Err(e) => ExecResult::failure(1, e.to_string()),
            };

            // Apply post-execution redirects
            last_result = apply_redirects(last_result, &cmd.redirects, ctx).await;

            // If command failed, stop the pipeline
            if !last_result.ok() {
                return last_result;
            }

            // Pass stdout and structured data to next command's stdin (unless last command)
            if i < last_idx {
                current_stdin = Some(last_result.out.clone());
                current_data = last_result.data.clone();
            }
        }

        last_result
    }
}

/// Extract parameter types from a tool schema.
///
/// Returns a map from param name → param type (e.g., "verbose" → "bool", "output" → "string").
/// Build a map from flag name → (canonical param name, param type).
///
/// Includes both primary names and aliases (with dashes stripped).
/// For short flags like `-n` aliased to `lines`, maps `"n"` → `("lines", "int")`.
pub fn schema_param_lookup(schema: &ToolSchema) -> HashMap<String, (&str, &str)> {
    let mut map = HashMap::new();
    for p in &schema.params {
        map.insert(p.name.clone(), (p.name.as_str(), p.param_type.as_str()));
        for alias in &p.aliases {
            let stripped = alias.trim_start_matches('-');
            map.insert(stripped.to_string(), (p.name.as_str(), p.param_type.as_str()));
        }
    }
    map
}

/// Check if a type is considered boolean.
pub fn is_bool_type(param_type: &str) -> bool {
    matches!(param_type.to_lowercase().as_str(), "bool" | "boolean")
}

/// Build ToolArgs from AST Args, evaluating expressions.
///
/// If a schema is provided, uses it to determine argument types:
/// - For `--flag` where schema says type is non-bool: consume next positional as value
/// - For `--flag` where schema says type is bool (or unknown): treat as boolean flag
///
/// This enables natural shell syntax like `mcp_tool --query "test" --limit 10`.
pub fn build_tool_args(args: &[Arg], ctx: &ExecContext, schema: Option<&ToolSchema>) -> ToolArgs {
    let mut tool_args = ToolArgs::new();
    let param_lookup = schema.map(schema_param_lookup).unwrap_or_default();

    // Track which positional indices have been consumed as flag values
    let mut consumed_positionals: std::collections::HashSet<usize> = std::collections::HashSet::new();
    let mut past_double_dash = false;

    // First pass: find positional args and their indices
    let mut positional_indices: Vec<(usize, &Expr)> = Vec::new();
    for (i, arg) in args.iter().enumerate() {
        if let Arg::Positional(expr) = arg {
            positional_indices.push((i, expr));
        }
    }

    // Second pass: process all args
    let mut i = 0;
    while i < args.len() {
        let arg = &args[i];

        match arg {
            Arg::DoubleDash => {
                past_double_dash = true;
            }
            Arg::Positional(expr) => {
                // Check if this positional was consumed by a preceding flag
                if !consumed_positionals.contains(&i)
                    && let Some(value) = eval_simple_expr(expr, ctx)
                {
                    tool_args.positional.push(value);
                }
            }
            Arg::Named { key, value } => {
                if let Some(val) = eval_simple_expr(value, ctx) {
                    tool_args.named.insert(key.clone(), val);
                }
            }
            Arg::ShortFlag(name) => {
                if past_double_dash {
                    tool_args.positional.push(Value::String(format!("-{name}")));
                } else if name.len() == 1 {
                    // Single-char short flag: look up schema to check if it takes a value.
                    // e.g., `-n 5` where `-n` is an alias for `lines` (type: int)
                    let flag_name = name.as_str();
                    let lookup = param_lookup.get(flag_name);
                    let is_bool = lookup
                        .map(|(_, typ)| is_bool_type(typ))
                        .unwrap_or(true);

                    if is_bool {
                        tool_args.flags.insert(flag_name.to_string());
                    } else {
                        // Non-bool: consume next positional as value, insert under canonical name
                        let canonical = lookup.map(|(name, _)| *name).unwrap_or(flag_name);
                        let next_positional = positional_indices
                            .iter()
                            .find(|(idx, _)| *idx > i && !consumed_positionals.contains(idx));

                        if let Some((pos_idx, expr)) = next_positional {
                            if let Some(value) = eval_simple_expr(expr, ctx) {
                                tool_args.named.insert(canonical.to_string(), value);
                                consumed_positionals.insert(*pos_idx);
                            } else {
                                tool_args.flags.insert(flag_name.to_string());
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
                    // Look up type in schema (checks name and aliases)
                    let lookup = param_lookup.get(name.as_str());
                    let is_bool = lookup
                        .map(|(_, typ)| is_bool_type(typ))
                        .unwrap_or(true); // Unknown params default to bool

                    if is_bool {
                        tool_args.flags.insert(name.clone());
                    } else {
                        // Non-bool: consume next positional as value, insert under canonical name
                        let canonical = lookup.map(|(name, _)| *name).unwrap_or(name.as_str());
                        let next_positional = positional_indices
                            .iter()
                            .find(|(idx, _)| *idx > i && !consumed_positionals.contains(idx));

                        if let Some((pos_idx, expr)) = next_positional {
                            if let Some(value) = eval_simple_expr(expr, ctx) {
                                tool_args.named.insert(canonical.to_string(), value);
                                consumed_positionals.insert(*pos_idx);
                            } else {
                                tool_args.flags.insert(name.clone());
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

    tool_args
}

/// Simple expression evaluation for args (without full scope access).
fn eval_simple_expr(expr: &Expr, ctx: &ExecContext) -> Option<Value> {
    match expr {
        Expr::Literal(value) => Some(eval_literal(value, ctx)),
        Expr::VarRef(path) => ctx.scope.resolve_path(path),
        Expr::Interpolated(parts) => {
            let mut result = String::new();
            for part in parts {
                match part {
                    crate::ast::StringPart::Literal(s) => result.push_str(s),
                    crate::ast::StringPart::Var(path) => {
                        if let Some(value) = ctx.scope.resolve_path(path) {
                            result.push_str(&value_to_string(&value));
                        }
                    }
                    crate::ast::StringPart::VarWithDefault { name, default } => {
                        match ctx.scope.get(name) {
                            Some(value) => {
                                let s = value_to_string(value);
                                if s.is_empty() {
                                    result.push_str(&eval_string_parts_sync(default, ctx));
                                } else {
                                    result.push_str(&s);
                                }
                            }
                            None => result.push_str(&eval_string_parts_sync(default, ctx)),
                        }
                    }
                    crate::ast::StringPart::VarLength(name) => {
                        let len = match ctx.scope.get(name) {
                            Some(value) => value_to_string(value).len(),
                            None => 0,
                        };
                        result.push_str(&len.to_string());
                    }
                    crate::ast::StringPart::Positional(n) => {
                        if let Some(s) = ctx.scope.get_positional(*n) {
                            result.push_str(s);
                        }
                    }
                    crate::ast::StringPart::AllArgs => {
                        result.push_str(&ctx.scope.all_args().join(" "));
                    }
                    crate::ast::StringPart::ArgCount => {
                        result.push_str(&ctx.scope.arg_count().to_string());
                    }
                    crate::ast::StringPart::Arithmetic(expr) => {
                        // Evaluate arithmetic in pipeline context
                        if let Ok(value) = arithmetic::eval_arithmetic(expr, &ctx.scope) {
                            result.push_str(&value.to_string());
                        }
                    }
                    crate::ast::StringPart::CommandSubst(_) => {
                        // Command substitution requires async - skip in sync context
                    }
                    crate::ast::StringPart::LastExitCode => {
                        result.push_str(&ctx.scope.last_result().code.to_string());
                    }
                    crate::ast::StringPart::CurrentPid => {
                        result.push_str(&ctx.scope.pid().to_string());
                    }
                }
            }
            Some(Value::String(result))
        }
        _ => None, // Binary ops and command subst need more context
    }
}

/// Evaluate a literal value.
fn eval_literal(value: &Value, _ctx: &ExecContext) -> Value {
    value.clone()
}

/// Convert a value to a string for interpolation.
fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => "".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Json(json) => json.to_string(),
        Value::Blob(blob) => format!("[blob: {} {}]", blob.formatted_size(), blob.content_type),
    }
}

/// Evaluate string parts synchronously (for pipeline context).
/// Command substitutions are skipped as they require async.
fn eval_string_parts_sync(parts: &[crate::ast::StringPart], ctx: &ExecContext) -> String {
    let mut result = String::new();
    for part in parts {
        match part {
            crate::ast::StringPart::Literal(s) => result.push_str(s),
            crate::ast::StringPart::Var(path) => {
                if let Some(value) = ctx.scope.resolve_path(path) {
                    result.push_str(&value_to_string(&value));
                }
            }
            crate::ast::StringPart::VarWithDefault { name, default } => {
                match ctx.scope.get(name) {
                    Some(value) => {
                        let s = value_to_string(value);
                        if s.is_empty() {
                            result.push_str(&eval_string_parts_sync(default, ctx));
                        } else {
                            result.push_str(&s);
                        }
                    }
                    None => result.push_str(&eval_string_parts_sync(default, ctx)),
                }
            }
            crate::ast::StringPart::VarLength(name) => {
                let len = match ctx.scope.get(name) {
                    Some(value) => value_to_string(value).len(),
                    None => 0,
                };
                result.push_str(&len.to_string());
            }
            crate::ast::StringPart::Positional(n) => {
                if let Some(s) = ctx.scope.get_positional(*n) {
                    result.push_str(s);
                }
            }
            crate::ast::StringPart::AllArgs => {
                result.push_str(&ctx.scope.all_args().join(" "));
            }
            crate::ast::StringPart::ArgCount => {
                result.push_str(&ctx.scope.arg_count().to_string());
            }
            crate::ast::StringPart::Arithmetic(expr) => {
                if let Ok(value) = arithmetic::eval_arithmetic(expr, &ctx.scope) {
                    result.push_str(&value.to_string());
                }
            }
            crate::ast::StringPart::CommandSubst(_) => {
                // Command substitution requires async - skip in sync context
            }
            crate::ast::StringPart::LastExitCode => {
                result.push_str(&ctx.scope.last_result().code.to_string());
            }
            crate::ast::StringPart::CurrentPid => {
                result.push_str(&ctx.scope.pid().to_string());
            }
        }
    }
    result
}

/// Find scatter and gather commands in a pipeline.
///
/// Returns Some((scatter_index, gather_index)) if both are found with scatter before gather.
/// Returns None if the pipeline doesn't have a valid scatter/gather pattern.
fn find_scatter_gather(commands: &[Command]) -> Option<(usize, usize)> {
    let scatter_idx = commands.iter().position(|c| c.name == "scatter")?;
    let gather_idx = commands.iter().position(|c| c.name == "gather")?;

    // Gather must come after scatter
    if gather_idx > scatter_idx {
        Some((scatter_idx, gather_idx))
    } else {
        None
    }
}

/// Run a pipeline sequentially without scatter/gather detection.
///
/// This is used internally by ScatterGatherRunner to avoid recursion.
/// The `tools` parameter is used for schema lookup to enable schema-aware argument parsing.
pub async fn run_sequential_pipeline(
    tools: &Arc<ToolRegistry>,
    commands: &[Command],
    ctx: &mut ExecContext,
) -> ExecResult {
    if commands.is_empty() {
        return ExecResult::success("");
    }

    let mut current_stdin: Option<String> = ctx.take_stdin();
    let mut current_data: Option<Value> = ctx.take_stdin_data();
    let mut last_result = ExecResult::success("");

    for (i, cmd) in commands.iter().enumerate() {
        // Handle built-in true/false
        match cmd.name.as_str() {
            "true" => {
                last_result = ExecResult::success("");
                if i < commands.len() - 1 {
                    current_stdin = Some(last_result.out.clone());
                    current_data = last_result.data.clone();
                }
                continue;
            }
            "false" => {
                return ExecResult::failure(1, "");
            }
            _ => {}
        }

        // Build tool args with schema-aware parsing
        let schema = tools.get(&cmd.name).map(|t| t.schema());
        let mut tool_args = build_tool_args(&cmd.args, ctx, schema.as_ref());
        let output_format = extract_output_format(&mut tool_args, schema.as_ref());

        // Set up stdin from redirects (< file, <<heredoc)
        setup_stdin_redirects(cmd, ctx);

        // Set stdin from previous command's stdout (overrides redirect stdin)
        // Also pass structured data if available from previous command
        if let Some(input) = current_stdin.take() {
            ctx.set_stdin_with_data(input, current_data.take());
        }

        // Execute via backend (clone Arc to avoid borrow conflict)
        let backend = ctx.backend.clone();
        last_result = match backend.call_tool(&cmd.name, tool_args, ctx).await {
            Ok(tool_result) => {
                let mut exec = ExecResult::from_output(
                    tool_result.code as i64, tool_result.stdout, tool_result.stderr,
                );
                exec.output = tool_result.output;
                exec
            }
            Err(e) => ExecResult::failure(127, e.to_string()),
        };

        // Apply output format transform
        last_result = match output_format {
            Some(format) => apply_output_format(last_result, format),
            None => last_result,
        };

        // Apply post-execution redirects
        last_result = apply_redirects(last_result, &cmd.redirects, ctx).await;

        // If command failed, stop the pipeline
        if !last_result.ok() {
            return last_result;
        }

        // Pass stdout and structured data to next command's stdin (unless last command)
        if i < commands.len() - 1 {
            current_stdin = Some(last_result.out.clone());
            current_data = last_result.data.clone();
        }
    }

    last_result
}

/// Run a pipeline sequentially with owned data (for spawned tasks).
///
/// This is used by scatter workers to run commands in parallel.
pub async fn run_sequential_pipeline_owned(
    tools: Arc<ToolRegistry>,
    commands: Vec<Command>,
    ctx: &mut ExecContext,
) -> ExecResult {
    run_sequential_pipeline(&tools, &commands, ctx).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::dispatch::BackendDispatcher;
    use crate::tools::register_builtins;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::path::Path;

    async fn make_runner_and_ctx() -> (PipelineRunner, ExecContext, BackendDispatcher) {
        let mut tools = ToolRegistry::new();
        register_builtins(&mut tools);
        let tools = Arc::new(tools);
        let runner = PipelineRunner::new(tools.clone());
        let dispatcher = BackendDispatcher::new(tools.clone());

        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("test.txt"), b"hello\nworld\nfoo").await.unwrap();
        vfs.mount("/", mem);
        let ctx = ExecContext::with_vfs_and_tools(Arc::new(vfs), tools);

        (runner, ctx, dispatcher)
    }

    fn make_cmd(name: &str, args: Vec<&str>) -> Command {
        Command {
            name: name.to_string(),
            args: args.iter().map(|s| Arg::Positional(Expr::Literal(Value::String(s.to_string())))).collect(),
            redirects: vec![],
        }
    }

    #[tokio::test]
    async fn test_single_command() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;
        let cmd = make_cmd("echo", vec!["hello"]);

        let result = runner.run(&[cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "hello");
    }

    #[tokio::test]
    async fn test_pipeline_echo_grep() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "hello\nworld" | grep pattern="world"
        let echo_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello\nworld".to_string())))],
            redirects: vec![],
        };
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("world".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[echo_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "world");
    }

    #[tokio::test]
    async fn test_pipeline_cat_grep() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // cat /test.txt | grep pattern="hello"
        let cat_cmd = make_cmd("cat", vec!["/test.txt"]);
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[cat_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert!(result.out.contains("hello"));
    }

    #[tokio::test]
    async fn test_command_not_found() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;
        let cmd = make_cmd("nonexistent", vec![]);

        let result = runner.run(&[cmd], &mut ctx, &dispatcher).await;
        assert!(!result.ok());
        assert_eq!(result.code, 127);
        assert!(result.err.contains("not found"));
    }

    #[tokio::test]
    async fn test_pipeline_stops_on_failure() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // cat /nonexistent | grep "hello"
        let cat_cmd = make_cmd("cat", vec!["/nonexistent"]);
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[cat_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_empty_pipeline() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;
        let result = runner.run(&[], &mut ctx, &dispatcher).await;
        assert!(result.ok());
    }

    // === Scatter/Gather Tests ===

    #[test]
    fn test_find_scatter_gather_both_present() {
        let commands = vec![
            make_cmd("echo", vec!["a"]),
            make_cmd("scatter", vec![]),
            make_cmd("process", vec![]),
            make_cmd("gather", vec![]),
        ];
        let result = find_scatter_gather(&commands);
        assert_eq!(result, Some((1, 3)));
    }

    #[test]
    fn test_find_scatter_gather_no_scatter() {
        let commands = vec![
            make_cmd("echo", vec!["a"]),
            make_cmd("gather", vec![]),
        ];
        let result = find_scatter_gather(&commands);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_scatter_gather_no_gather() {
        let commands = vec![
            make_cmd("echo", vec!["a"]),
            make_cmd("scatter", vec![]),
        ];
        let result = find_scatter_gather(&commands);
        assert!(result.is_none());
    }

    #[test]
    fn test_find_scatter_gather_wrong_order() {
        let commands = vec![
            make_cmd("gather", vec![]),
            make_cmd("scatter", vec![]),
        ];
        let result = find_scatter_gather(&commands);
        assert!(result.is_none());
    }

    #[tokio::test]
    async fn test_scatter_gather_simple() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "a\nb\nc" | scatter | echo ${ITEM} | gather
        let echo_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("a\nb\nc".to_string())))],
            redirects: vec![],
        };
        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[echo_cmd, scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        // Each echo should output the item
        assert!(result.out.contains("a"));
        assert!(result.out.contains("b"));
        assert!(result.out.contains("c"));
    }

    #[tokio::test]
    async fn test_scatter_gather_empty_input() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "" | scatter | echo ${ITEM} | gather
        let echo_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("".to_string())))],
            redirects: vec![],
        };
        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[echo_cmd, scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert!(result.out.trim().is_empty());
    }

    #[tokio::test]
    async fn test_scatter_gather_with_stdin() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // Set stdin directly and use scatter | echo | gather
        ctx.set_stdin("x\ny\nz".to_string());

        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert!(result.out.contains("x"));
        assert!(result.out.contains("y"));
        assert!(result.out.contains("z"));
    }

    #[tokio::test]
    async fn test_scatter_gather_json_input() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // JSON array input
        ctx.set_stdin(r#"["one", "two", "three"]"#.to_string());

        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert!(result.out.contains("one"));
        assert!(result.out.contains("two"));
        assert!(result.out.contains("three"));
    }

    #[tokio::test]
    async fn test_scatter_gather_with_post_gather() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "a\nb" | scatter | echo ${ITEM} | gather | grep "a"
        let echo_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("a\nb".to_string())))],
            redirects: vec![],
        };
        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("a".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[echo_cmd, scatter_cmd, process_cmd, gather_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert!(result.out.contains("a"));
        assert!(!result.out.contains("b"));
    }

    #[tokio::test]
    async fn test_scatter_custom_var_name() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        ctx.set_stdin("test1\ntest2".to_string());

        // scatter as=URL | echo ${URL} | gather
        let scatter_cmd = Command {
            name: "scatter".to_string(),
            args: vec![Arg::Named {
                key: "as".to_string(),
                value: Expr::Literal(Value::String("URL".to_string())),
            }],
            redirects: vec![],
        };
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("URL")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[scatter_cmd, process_cmd, gather_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        assert!(result.out.contains("test1"));
        assert!(result.out.contains("test2"));
    }

    // === Backend Routing Tests ===

    #[tokio::test]
    async fn test_pipeline_routes_through_backend() {
        use crate::backend::testing::MockBackend;
        use std::sync::atomic::Ordering;

        // Create mock backend
        let (backend, call_count) = MockBackend::new();
        let backend: std::sync::Arc<dyn crate::backend::KernelBackend> = std::sync::Arc::new(backend);

        // Create context with mock backend
        let mut ctx = crate::tools::ExecContext::with_backend(backend);

        // BackendDispatcher routes through backend.call_tool()
        let tools = std::sync::Arc::new(ToolRegistry::new());
        let runner = PipelineRunner::new(tools.clone());
        let dispatcher = BackendDispatcher::new(tools);

        // Single command should route through backend
        let cmd = make_cmd("test-tool", vec!["arg1"]);
        let result = runner.run(&[cmd], &mut ctx, &dispatcher).await;

        assert!(result.ok(), "Mock backend should return success");
        assert_eq!(call_count.load(Ordering::SeqCst), 1, "call_tool should be invoked once");
        assert!(result.out.contains("mock executed"), "Output should be from mock backend");
    }

    #[tokio::test]
    async fn test_multi_command_pipeline_routes_through_backend() {
        use crate::backend::testing::MockBackend;
        use std::sync::atomic::Ordering;

        let (backend, call_count) = MockBackend::new();
        let backend: std::sync::Arc<dyn crate::backend::KernelBackend> = std::sync::Arc::new(backend);
        let mut ctx = crate::tools::ExecContext::with_backend(backend);

        let tools = std::sync::Arc::new(ToolRegistry::new());
        let runner = PipelineRunner::new(tools.clone());
        let dispatcher = BackendDispatcher::new(tools);

        // Pipeline with 3 commands
        let cmd1 = make_cmd("tool1", vec![]);
        let cmd2 = make_cmd("tool2", vec![]);
        let cmd3 = make_cmd("tool3", vec![]);

        let result = runner.run(&[cmd1, cmd2, cmd3], &mut ctx, &dispatcher).await;

        assert!(result.ok());
        assert_eq!(call_count.load(Ordering::SeqCst), 3, "call_tool should be invoked for each command");
    }

    // === Schema-Aware Argument Parsing Tests ===

    use crate::tools::{ParamSchema, ToolSchema};

    fn make_test_schema() -> ToolSchema {
        ToolSchema::new("test-tool", "A test tool for schema-aware parsing")
            .param(ParamSchema::required("query", "string", "Search query"))
            .param(ParamSchema::optional("limit", "int", Value::Int(10), "Max results"))
            .param(ParamSchema::optional("verbose", "bool", Value::Bool(false), "Verbose output"))
            .param(ParamSchema::optional("output", "string", Value::String("stdout".into()), "Output destination"))
    }

    fn make_minimal_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[test]
    fn test_schema_aware_string_arg() {
        // --query "test" should become named: {"query": "test"}
        let args = vec![
            Arg::LongFlag("query".to_string()),
            Arg::Positional(Expr::Literal(Value::String("test".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.flags.is_empty(), "No flags should be set");
        assert!(tool_args.positional.is_empty(), "No positionals - consumed by --query");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("test".to_string())),
            "--query should consume 'test' as its value"
        );
    }

    #[test]
    fn test_schema_aware_bool_flag() {
        // --verbose should remain a flag since schema says bool
        let args = vec![
            Arg::LongFlag("verbose".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.flags.contains("verbose"), "--verbose should be a flag");
        assert!(tool_args.named.is_empty(), "No named args");
        assert!(tool_args.positional.is_empty(), "No positionals");
    }

    #[test]
    fn test_schema_aware_mixed() {
        // mcp_tool file.txt --output out.txt --verbose
        // Should produce: positional: ["file.txt"], named: {"output": "out.txt"}, flags: {"verbose"}
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("file.txt".to_string()))),
            Arg::LongFlag("output".to_string()),
            Arg::Positional(Expr::Literal(Value::String("out.txt".to_string()))),
            Arg::LongFlag("verbose".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert_eq!(tool_args.positional, vec![Value::String("file.txt".to_string())]);
        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("out.txt".to_string()))
        );
        assert!(tool_args.flags.contains("verbose"));
    }

    #[test]
    fn test_schema_aware_multiple_string_args() {
        // --query "test" --output "result.json" --verbose --limit 5
        let args = vec![
            Arg::LongFlag("query".to_string()),
            Arg::Positional(Expr::Literal(Value::String("test".to_string()))),
            Arg::LongFlag("output".to_string()),
            Arg::Positional(Expr::Literal(Value::String("result.json".to_string()))),
            Arg::LongFlag("verbose".to_string()),
            Arg::LongFlag("limit".to_string()),
            Arg::Positional(Expr::Literal(Value::Int(5))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.positional.is_empty(), "All positionals consumed");
        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("test".to_string()))
        );
        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("result.json".to_string()))
        );
        assert_eq!(
            tool_args.named.get("limit"),
            Some(&Value::Int(5))
        );
        assert!(tool_args.flags.contains("verbose"));
    }

    #[test]
    fn test_schema_aware_double_dash() {
        // --output out.txt -- --this-is-data
        // After --, everything is positional
        let args = vec![
            Arg::LongFlag("output".to_string()),
            Arg::Positional(Expr::Literal(Value::String("out.txt".to_string()))),
            Arg::DoubleDash,
            Arg::Positional(Expr::Literal(Value::String("--this-is-data".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert_eq!(
            tool_args.named.get("output"),
            Some(&Value::String("out.txt".to_string()))
        );
        // After --, the --this-is-data is treated as a positional (it's a Positional in the args)
        assert_eq!(
            tool_args.positional,
            vec![Value::String("--this-is-data".to_string())]
        );
    }

    #[test]
    fn test_no_schema_fallback() {
        // Without schema, all --flags are treated as bool flags
        let args = vec![
            Arg::LongFlag("query".to_string()),
            Arg::Positional(Expr::Literal(Value::String("test".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, None);

        // Without schema, --query is a flag and "test" is a positional
        assert!(tool_args.flags.contains("query"), "--query should be a flag");
        assert_eq!(
            tool_args.positional,
            vec![Value::String("test".to_string())],
            "'test' should be a positional"
        );
    }

    #[test]
    fn test_unknown_flag_in_schema() {
        // --unknown-flag value should treat --unknown-flag as bool (not in schema)
        let args = vec![
            Arg::LongFlag("unknown".to_string()),
            Arg::Positional(Expr::Literal(Value::String("value".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        // Unknown flag defaults to bool behavior
        assert!(tool_args.flags.contains("unknown"));
        assert_eq!(
            tool_args.positional,
            vec![Value::String("value".to_string())]
        );
    }

    #[test]
    fn test_named_args_unchanged() {
        // key=value syntax should work regardless of schema
        let args = vec![
            Arg::Named {
                key: "query".to_string(),
                value: Expr::Literal(Value::String("test".to_string())),
            },
            Arg::LongFlag("verbose".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert_eq!(
            tool_args.named.get("query"),
            Some(&Value::String("test".to_string()))
        );
        assert!(tool_args.flags.contains("verbose"));
    }

    #[test]
    fn test_short_flags_unchanged() {
        // Short flags -la should expand regardless of schema
        let args = vec![
            Arg::ShortFlag("la".to_string()),
            Arg::Positional(Expr::Literal(Value::String("file.txt".to_string()))),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.flags.contains("l"));
        assert!(tool_args.flags.contains("a"));
        assert_eq!(
            tool_args.positional,
            vec![Value::String("file.txt".to_string())]
        );
    }

    #[test]
    fn test_flag_at_end_no_value() {
        // --output at end with no value available - treat as flag (lenient)
        let args = vec![
            Arg::Positional(Expr::Literal(Value::String("file.txt".to_string()))),
            Arg::LongFlag("output".to_string()),
        ];
        let schema = make_test_schema();
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        // output expects a value but none available after it, so it becomes a flag
        assert!(tool_args.flags.contains("output"));
        assert_eq!(
            tool_args.positional,
            vec![Value::String("file.txt".to_string())]
        );
    }

    #[test]
    fn test_short_flag_with_alias_consumes_value() {
        // `-n 5` where `-n` is aliased to `lines` (type: int)
        // Should produce named: {"lines": 5}, not flags: {"n"} + positional: [5]
        let schema = ToolSchema::new("head", "Output first part of files")
            .param(ParamSchema::optional("lines", "int", Value::Int(10), "Number of lines")
                .with_aliases(["-n"]));
        let args = vec![
            Arg::ShortFlag("n".to_string()),
            Arg::Positional(Expr::Literal(Value::Int(5))),
            Arg::Positional(Expr::Literal(Value::String("/tmp/file.txt".to_string()))),
        ];
        let ctx = make_minimal_ctx();

        let tool_args = build_tool_args(&args, &ctx, Some(&schema));

        assert!(tool_args.flags.is_empty(), "no boolean flags: {:?}", tool_args.flags);
        assert_eq!(tool_args.named.get("lines"), Some(&Value::Int(5)), "should resolve alias to canonical name");
        assert_eq!(tool_args.positional, vec![Value::String("/tmp/file.txt".to_string())]);
    }

    // === Redirect Execution Tests ===

    #[tokio::test]
    async fn test_merge_stderr_redirect() {
        // Test that 2>&1 merges stderr into stdout
        let result = ExecResult::from_output(0, "stdout content", "stderr content");

        let redirects = vec![Redirect {
            kind: RedirectKind::MergeStderr,
            target: Expr::Literal(Value::Null),
        }];

        let ctx = make_minimal_ctx();
        let result = apply_redirects(result, &redirects, &ctx).await;

        assert_eq!(result.out, "stdout contentstderr content");
        assert!(result.err.is_empty());
    }

    #[tokio::test]
    async fn test_merge_stderr_with_empty_stderr() {
        // Test that 2>&1 handles empty stderr gracefully
        let result = ExecResult::from_output(0, "stdout only", "");

        let redirects = vec![Redirect {
            kind: RedirectKind::MergeStderr,
            target: Expr::Literal(Value::Null),
        }];

        let ctx = make_minimal_ctx();
        let result = apply_redirects(result, &redirects, &ctx).await;

        assert_eq!(result.out, "stdout only");
        assert!(result.err.is_empty());
    }

    #[tokio::test]
    async fn test_merge_stderr_order_matters() {
        // Test redirect ordering: 2>&1 > file means:
        // 1. First merge stderr into stdout
        // 2. Then write stdout to file (leaving both empty for piping)
        // This verifies left-to-right processing
        let result = ExecResult::from_output(0, "stdout\n", "stderr\n");

        // Just 2>&1 - should merge
        let redirects = vec![Redirect {
            kind: RedirectKind::MergeStderr,
            target: Expr::Literal(Value::Null),
        }];

        let ctx = make_minimal_ctx();
        let result = apply_redirects(result, &redirects, &ctx).await;

        assert_eq!(result.out, "stdout\nstderr\n");
        assert!(result.err.is_empty());
    }

    #[tokio::test]
    async fn test_redirect_with_command_execution() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "hello" with 2>&1 redirect
        let cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello".to_string())))],
            redirects: vec![Redirect {
                kind: RedirectKind::MergeStderr,
                target: Expr::Literal(Value::Null),
            }],
        };

        let result = runner.run(&[cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok());
        // echo produces no stderr, so this just validates the redirect doesn't break anything
        assert!(result.out.contains("hello"));
    }

    #[tokio::test]
    async fn test_merge_stderr_in_pipeline() {
        let (runner, mut ctx, dispatcher) = make_runner_and_ctx().await;

        // echo "output" 2>&1 | grep "output"
        // The 2>&1 should be applied to echo's result, then piped to grep
        let echo_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("output".to_string())))],
            redirects: vec![Redirect {
                kind: RedirectKind::MergeStderr,
                target: Expr::Literal(Value::Null),
            }],
        };
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("output".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[echo_cmd, grep_cmd], &mut ctx, &dispatcher).await;
        assert!(result.ok(), "result failed: code={}, err={}", result.code, result.err);
        assert!(result.out.contains("output"));
    }
}
