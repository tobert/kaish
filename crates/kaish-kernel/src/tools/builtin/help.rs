//! help — Display help for topics and tools.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::help::{get_help, HelpTopic};
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Help tool: display help for topics and tools.
pub struct Help;

/// clap-derived argv layer for help.
#[derive(Parser, Debug)]
#[command(name = "help", about = "Display help for kaish topics and tools")]
struct HelpArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Topic to show help for (`syntax`, `limits`, `builtins`, or a tool name).
    topic: Vec<String>,
}

#[async_trait]
impl Tool for Help {
    fn name(&self) -> &str {
        "help"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &HelpArgs::command(),
            "help",
            "Display help for kaish topics and tools",
            [
                ("Show overview", "help"),
                ("Syntax reference", "help syntax"),
                ("Help for a tool", "help cat"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("help: {e}")),
        };
        let parsed = match HelpArgs::try_parse_from(
            std::iter::once("help".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("help: {e}")),
        };
        parsed.global.apply(ctx);

        let topic_str = args.get_string("topic", 0).unwrap_or_default();
        let topic = HelpTopic::parse_topic(&topic_str);
        let content = get_help(&topic, &ctx.tool_schemas);
        ExecResult::with_output(OutputData::text(content))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::tools::ParamSchema;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx_with_schemas() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        // Add some test schemas
        ctx.set_tool_schemas(vec![
            ToolSchema::new("echo", "Print arguments to stdout").param(ParamSchema::optional(
                "args",
                "any",
                Value::Null,
                "Values to print",
            )),
            ToolSchema::new("cat", "Read and output file contents")
                .param(ParamSchema::required("path", "string", "File path to read")),
            ToolSchema::new("ls", "List directory contents"),
        ]);

        ctx
    }

    #[tokio::test]
    async fn test_help_overview() {
        let mut ctx = make_ctx_with_schemas();
        let args = ToolArgs::new();

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("kaish"));
        assert!(result.text_out().contains("help syntax"));
    }

    #[tokio::test]
    async fn test_help_syntax() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("syntax".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("Variables"));
        assert!(result.text_out().contains("Quoting"));
    }

    #[tokio::test]
    async fn test_help_builtins() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("builtins".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("echo"));
        assert!(result.text_out().contains("cat"));
        assert!(result.text_out().contains("ls"));
    }

    #[tokio::test]
    async fn test_help_specific_tool() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("cat".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("cat —"));
        assert!(result.text_out().contains("path"));
        assert!(result.text_out().contains("required"));
    }

    #[tokio::test]
    async fn test_help_unknown_topic() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("nonexistent".into()));

        let result = Help.execute(args, &mut ctx).await;
        // Unknown topics still succeed, just with a helpful message
        assert!(result.ok());
        assert!(result.text_out().contains("Unknown topic or tool"));
    }

    #[tokio::test]
    async fn test_help_vfs() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("vfs".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("Modes"));
    }

    #[tokio::test]
    async fn test_help_scatter() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("scatter".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("scatter"));
        assert!(result.text_out().contains("gather"));
    }

    #[tokio::test]
    async fn test_help_limits() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("limits".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("Limitations"));
    }
}
