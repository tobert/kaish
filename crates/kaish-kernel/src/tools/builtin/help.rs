//! help — Display help for topics and tools.

use async_trait::async_trait;

use crate::ast::Value;
use crate::help::{get_help, HelpTopic};
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Help tool: display help for topics and tools.
pub struct Help;

#[async_trait]
impl Tool for Help {
    fn name(&self) -> &str {
        "help"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new(
            "help",
            "Display help for kaish topics and tools",
        )
        .param(ParamSchema::optional(
            "topic",
            "string",
            Value::Null,
            "Topic or tool name: overview, syntax, builtins, vfs, scatter, limits, or a tool name",
        ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let topic_str = args.get_string("topic", 0).unwrap_or_default();
        let topic = HelpTopic::from_str(&topic_str);
        let content = get_help(&topic, &ctx.tool_schemas);
        ExecResult::success(content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx_with_schemas() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        // Add some test schemas
        ctx.tool_schemas = vec![
            ToolSchema::new("echo", "Print arguments to stdout").param(ParamSchema::optional(
                "args",
                "any",
                Value::Null,
                "Values to print",
            )),
            ToolSchema::new("cat", "Read and output file contents")
                .param(ParamSchema::required("path", "string", "File path to read")),
            ToolSchema::new("ls", "List directory contents"),
        ];

        ctx
    }

    #[tokio::test]
    async fn test_help_overview() {
        let mut ctx = make_ctx_with_schemas();
        let args = ToolArgs::new();

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("kaish"));
        assert!(result.out.contains("help syntax"));
    }

    #[tokio::test]
    async fn test_help_syntax() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("syntax".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("Variables"));
        assert!(result.out.contains("Quoting"));
    }

    #[tokio::test]
    async fn test_help_builtins() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("builtins".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("echo"));
        assert!(result.out.contains("cat"));
        assert!(result.out.contains("ls"));
    }

    #[tokio::test]
    async fn test_help_specific_tool() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("cat".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("cat —"));
        assert!(result.out.contains("path"));
        assert!(result.out.contains("required"));
    }

    #[tokio::test]
    async fn test_help_unknown_topic() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("nonexistent".into()));

        let result = Help.execute(args, &mut ctx).await;
        // Unknown topics still succeed, just with a helpful message
        assert!(result.ok());
        assert!(result.out.contains("Unknown topic or tool"));
    }

    #[tokio::test]
    async fn test_help_vfs() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("vfs".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("Mount Points"));
    }

    #[tokio::test]
    async fn test_help_scatter() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("scatter".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("scatter"));
        assert!(result.out.contains("gather"));
    }

    #[tokio::test]
    async fn test_help_limits() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("limits".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("Limitations"));
    }
}
