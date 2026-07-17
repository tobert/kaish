//! echo — Print arguments to stdout.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Echo tool: prints arguments to stdout.
pub struct Echo;

/// clap-derived argv layer for echo.
#[derive(Parser, Debug)]
#[command(name = "echo", about = "Print arguments to standard output")]
struct EchoArgs {
    /// Do not output trailing newline.
    #[arg(short = 'n', long = "no-newline", visible_alias = "no_newline")]
    no_newline: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Tokens to print, separated by spaces.
    words: Vec<String>,
}

#[async_trait]
impl Tool for Echo {
    fn name(&self) -> &str {
        "echo"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &EchoArgs::command(),
            "echo",
            "Print arguments to standard output",
            [
                ("Print a message", "echo hello world"),
                ("No trailing newline", "echo -n \"prompt: \""),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // Preserve the original Value rendering (true/false/3.14/etc.) before
        // collapsing through to_argv()'s string layer — echo formats numerics
        // specially and we don't want clap to re-stringify them. `echo` is a
        // pure TEXT SINK, so a binary (`Value::Bytes`) positional goes LOUD via
        // `value_to_text_sink` rather than rendering the `[binary: N bytes]`
        // placeholder — printing that placeholder where the user's real bytes
        // should be is silent corruption. (Path-coercing builtins like `mkdir`
        // and env export remain deferred — the binary-at-text-sinks cluster.)
        let mut words = Vec::with_capacity(args.positional.len());
        for value in &args.positional {
            match crate::interpreter::value_to_text_sink(value) {
                Ok(s) => words.push(s),
                Err(e) => return ExecResult::failure(1, format!("echo: {e}")),
            }
        }

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("echo: {e}")),
        };
        let parsed = match EchoArgs::try_parse_from(
            std::iter::once("echo".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("echo: {e}")),
        };
        parsed.global.apply(ctx);

        let mut output = words.join(" ");
        if !parsed.no_newline && !output.is_empty() {
            output.push('\n');
        }

        ExecResult::with_output(OutputData::text(output))
    }
}

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_echo_simple() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hello\n");
    }

    #[tokio::test]
    async fn test_echo_multiple() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("world".into()));

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hello world\n");
    }

    #[tokio::test]
    async fn test_echo_types() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(42));
        args.positional.push(Value::Bool(true));
        args.positional.push(Value::Float(3.14));

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "42 true 3.14\n");
    }

    #[tokio::test]
    async fn test_echo_empty() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), ""); // Empty output, no newline added
    }

    #[tokio::test]
    async fn test_echo_n_no_newline() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.flags.insert("n".to_string());

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hello"); // No trailing newline
    }

    #[tokio::test]
    async fn test_echo_no_newline_flag() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("test".into()));
        args.flags.insert("no-newline".to_string());

        let result = Echo.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "test");
    }
}
