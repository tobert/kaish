//! Pipeline execution for kaish.
//!
//! Executes a sequence of commands connected by pipes, where the stdout
//! of each command becomes the stdin of the next.
//!
//! Also handles scatter/gather pipelines for parallel execution.

use std::sync::Arc;

use crate::ast::{Arg, Command, Expr, Value};
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ToolArgs, ToolRegistry};

use super::scatter::{
    parse_gather_options, parse_scatter_options, ScatterGatherRunner,
};

/// Runs pipelines by spawning tasks and connecting them via channels.
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
    pub async fn run(
        &self,
        commands: &[Command],
        ctx: &mut ExecContext,
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
            return self.run_single(&commands[0], ctx, None).await;
        }

        // Multi-command pipeline
        self.run_pipeline(commands, ctx).await
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
        let scatter_opts = parse_scatter_options(&build_tool_args(&scatter_cmd.args, ctx));
        let gather_opts = parse_gather_options(&build_tool_args(&gather_cmd.args, ctx));

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
    async fn run_single(
        &self,
        cmd: &Command,
        ctx: &mut ExecContext,
        stdin: Option<String>,
    ) -> ExecResult {
        // Handle built-in true/false
        match cmd.name.as_str() {
            "true" => return ExecResult::success(""),
            "false" => return ExecResult::failure(1, ""),
            _ => {}
        }

        // Look up tool
        let tool = match self.tools.get(&cmd.name) {
            Some(t) => t,
            None => {
                return ExecResult::failure(
                    127,
                    format!("{}: command not found", cmd.name),
                );
            }
        };

        // Build tool args
        let tool_args = build_tool_args(&cmd.args, ctx);

        // Set stdin if provided
        if let Some(input) = stdin {
            ctx.set_stdin(input);
        }

        // Execute
        tool.execute(tool_args, ctx).await
    }

    /// Run a multi-command pipeline.
    async fn run_pipeline(
        &self,
        commands: &[Command],
        ctx: &mut ExecContext,
    ) -> ExecResult {
        // We'll run commands sequentially for simplicity, passing stdout as stdin
        // A more sophisticated implementation would use tokio::spawn for true parallelism

        let mut current_stdin: Option<String> = None;
        let mut last_result = ExecResult::success("");

        for (i, cmd) in commands.iter().enumerate() {
            // Clone context for this command (we need separate stdin per command)
            // For now, we'll just update stdin on the shared context

            // Look up tool
            let tool = match self.tools.get(&cmd.name) {
                Some(t) => t,
                None => {
                    return ExecResult::failure(
                        127,
                        format!("{}: command not found", cmd.name),
                    );
                }
            };

            // Build tool args
            let tool_args = build_tool_args(&cmd.args, ctx);

            // Set stdin from previous command's stdout
            if let Some(input) = current_stdin.take() {
                ctx.set_stdin(input);
            }

            // Execute
            last_result = tool.execute(tool_args, ctx).await;

            // If command failed, stop the pipeline
            if !last_result.ok() {
                return last_result;
            }

            // Pass stdout to next command's stdin (unless this is the last command)
            if i < commands.len() - 1 {
                current_stdin = Some(last_result.out.clone());
            }
        }

        last_result
    }
}

/// Build ToolArgs from AST Args, evaluating expressions.
pub fn build_tool_args(args: &[Arg], ctx: &ExecContext) -> ToolArgs {
    let mut tool_args = ToolArgs::new();

    for arg in args {
        match arg {
            Arg::Positional(expr) => {
                if let Some(value) = eval_simple_expr(expr, ctx) {
                    tool_args.positional.push(value);
                }
            }
            Arg::Named { key, value } => {
                if let Some(val) = eval_simple_expr(value, ctx) {
                    tool_args.named.insert(key.clone(), val);
                }
            }
            Arg::ShortFlag(name) => {
                // Expand combined flags like -la
                for c in name.chars() {
                    tool_args.flags.insert(c.to_string());
                }
            }
            Arg::LongFlag(name) => {
                tool_args.flags.insert(name.clone());
            }
        }
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
                }
            }
            Some(Value::String(result))
        }
        _ => None, // Binary ops and command subst need more context
    }
}

/// Evaluate a literal value, recursively evaluating nested expressions.
fn eval_literal(value: &Value, ctx: &ExecContext) -> Value {
    match value {
        Value::Array(items) => {
            let evaluated: Vec<_> = items
                .iter()
                .filter_map(|e| eval_simple_expr(e, ctx).map(|v| Expr::Literal(v)))
                .collect();
            Value::Array(evaluated)
        }
        Value::Object(fields) => {
            let evaluated: Vec<_> = fields
                .iter()
                .filter_map(|(k, e)| {
                    eval_simple_expr(e, ctx).map(|v| (k.clone(), Expr::Literal(v)))
                })
                .collect();
            Value::Object(evaluated)
        }
        _ => value.clone(),
    }
}

/// Convert a value to a string for interpolation.
fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => "".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Array(_) => "[array]".to_string(),
        Value::Object(_) => "{object}".to_string(),
    }
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
pub async fn run_sequential_pipeline(
    tools: &Arc<ToolRegistry>,
    commands: &[Command],
    ctx: &mut ExecContext,
) -> ExecResult {
    if commands.is_empty() {
        return ExecResult::success("");
    }

    let mut current_stdin: Option<String> = ctx.take_stdin();
    let mut last_result = ExecResult::success("");

    for (i, cmd) in commands.iter().enumerate() {
        // Handle built-in true/false
        match cmd.name.as_str() {
            "true" => {
                last_result = ExecResult::success("");
                if i < commands.len() - 1 {
                    current_stdin = Some(last_result.out.clone());
                }
                continue;
            }
            "false" => {
                return ExecResult::failure(1, "");
            }
            _ => {}
        }

        // Look up tool
        let tool = match tools.get(&cmd.name) {
            Some(t) => t,
            None => {
                return ExecResult::failure(127, format!("{}: command not found", cmd.name));
            }
        };

        // Build tool args
        let tool_args = build_tool_args(&cmd.args, ctx);

        // Set stdin from previous command's stdout
        if let Some(input) = current_stdin.take() {
            ctx.set_stdin(input);
        }

        // Execute
        last_result = tool.execute(tool_args, ctx).await;

        // If command failed, stop the pipeline
        if !last_result.ok() {
            return last_result;
        }

        // Pass stdout to next command's stdin (unless this is the last command)
        if i < commands.len() - 1 {
            current_stdin = Some(last_result.out.clone());
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
    use crate::tools::register_builtins;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::path::Path;

    async fn make_runner_and_ctx() -> (PipelineRunner, ExecContext) {
        let mut tools = ToolRegistry::new();
        register_builtins(&mut tools);
        let runner = PipelineRunner::new(Arc::new(tools));

        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("test.txt"), b"hello\nworld\nfoo").await.unwrap();
        vfs.mount("/", mem);
        let ctx = ExecContext::new(Arc::new(vfs));

        (runner, ctx)
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
        let (runner, mut ctx) = make_runner_and_ctx().await;
        let cmd = make_cmd("echo", vec!["hello"]);

        let result = runner.run(&[cmd], &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "hello");
    }

    #[tokio::test]
    async fn test_pipeline_echo_grep() {
        let (runner, mut ctx) = make_runner_and_ctx().await;

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

        let result = runner.run(&[echo_cmd, grep_cmd], &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "world");
    }

    #[tokio::test]
    async fn test_pipeline_cat_grep() {
        let (runner, mut ctx) = make_runner_and_ctx().await;

        // cat /test.txt | grep pattern="hello"
        let cat_cmd = make_cmd("cat", vec!["/test.txt"]);
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[cat_cmd, grep_cmd], &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("hello"));
    }

    #[tokio::test]
    async fn test_command_not_found() {
        let (runner, mut ctx) = make_runner_and_ctx().await;
        let cmd = make_cmd("nonexistent", vec![]);

        let result = runner.run(&[cmd], &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 127);
        assert!(result.err.contains("not found"));
    }

    #[tokio::test]
    async fn test_pipeline_stops_on_failure() {
        let (runner, mut ctx) = make_runner_and_ctx().await;

        // cat /nonexistent | grep "hello"
        let cat_cmd = make_cmd("cat", vec!["/nonexistent"]);
        let grep_cmd = Command {
            name: "grep".to_string(),
            args: vec![Arg::Positional(Expr::Literal(Value::String("hello".to_string())))],
            redirects: vec![],
        };

        let result = runner.run(&[cat_cmd, grep_cmd], &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_empty_pipeline() {
        let (runner, mut ctx) = make_runner_and_ctx().await;
        let result = runner.run(&[], &mut ctx).await;
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
        let (runner, mut ctx) = make_runner_and_ctx().await;

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

        let result = runner.run(&[echo_cmd, scatter_cmd, process_cmd, gather_cmd], &mut ctx).await;
        assert!(result.ok());
        // Each echo should output the item
        assert!(result.out.contains("a"));
        assert!(result.out.contains("b"));
        assert!(result.out.contains("c"));
    }

    #[tokio::test]
    async fn test_scatter_gather_empty_input() {
        let (runner, mut ctx) = make_runner_and_ctx().await;

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

        let result = runner.run(&[echo_cmd, scatter_cmd, process_cmd, gather_cmd], &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.trim().is_empty());
    }

    #[tokio::test]
    async fn test_scatter_gather_with_stdin() {
        let (runner, mut ctx) = make_runner_and_ctx().await;

        // Set stdin directly and use scatter | echo | gather
        ctx.set_stdin("x\ny\nz".to_string());

        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[scatter_cmd, process_cmd, gather_cmd], &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("x"));
        assert!(result.out.contains("y"));
        assert!(result.out.contains("z"));
    }

    #[tokio::test]
    async fn test_scatter_gather_json_input() {
        let (runner, mut ctx) = make_runner_and_ctx().await;

        // JSON array input
        ctx.set_stdin(r#"["one", "two", "three"]"#.to_string());

        let scatter_cmd = make_cmd("scatter", vec![]);
        let process_cmd = Command {
            name: "echo".to_string(),
            args: vec![Arg::Positional(Expr::VarRef(crate::ast::VarPath::simple("ITEM")))],
            redirects: vec![],
        };
        let gather_cmd = make_cmd("gather", vec![]);

        let result = runner.run(&[scatter_cmd, process_cmd, gather_cmd], &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("one"));
        assert!(result.out.contains("two"));
        assert!(result.out.contains("three"));
    }

    #[tokio::test]
    async fn test_scatter_gather_with_post_gather() {
        let (runner, mut ctx) = make_runner_and_ctx().await;

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

        let result = runner.run(&[echo_cmd, scatter_cmd, process_cmd, gather_cmd, grep_cmd], &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("a"));
        assert!(!result.out.contains("b"));
    }

    #[tokio::test]
    async fn test_scatter_custom_var_name() {
        let (runner, mut ctx) = make_runner_and_ctx().await;

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

        let result = runner.run(&[scatter_cmd, process_cmd, gather_cmd], &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("test1"));
        assert!(result.out.contains("test2"));
    }
}
