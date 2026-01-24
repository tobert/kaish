//! help — Display help for tools.

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Help tool: display tool help.
pub struct Help;

#[async_trait]
impl Tool for Help {
    fn name(&self) -> &str {
        "help"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("help", "Display help for tools")
            .param(ParamSchema::optional(
                "tool",
                "string",
                Value::Null,
                "Tool name to get help for (lists all if not specified)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let tool_name = args.get_string("tool", 0);

        match tool_name {
            Some(name) => {
                // Show help for specific tool
                match ctx.tool_schemas.iter().find(|s| s.name == name) {
                    Some(schema) => ExecResult::success(format_tool_help(schema)),
                    None => ExecResult::failure(1, format!("help: unknown tool: {}", name)),
                }
            }
            None => {
                // List all tools
                ExecResult::success(format_tool_list(&ctx.tool_schemas))
            }
        }
    }
}

/// Format help for a single tool.
fn format_tool_help(schema: &ToolSchema) -> String {
    let mut output = String::new();

    output.push_str(&format!("{} — {}\n\n", schema.name, schema.description));

    if schema.params.is_empty() {
        output.push_str("No parameters.\n");
    } else {
        output.push_str("Parameters:\n");
        for param in &schema.params {
            let req = if param.required { " (required)" } else { "" };
            output.push_str(&format!(
                "  {} : {}{}\n    {}\n",
                param.name, param.param_type, req, param.description
            ));
        }
    }

    if !schema.examples.is_empty() {
        output.push_str("\nExamples:\n");
        for example in &schema.examples {
            output.push_str(&format!("  # {}\n", example.description));
            output.push_str(&format!("  {}\n\n", example.code));
        }
    }

    output
}

/// Format a list of all tools.
fn format_tool_list(schemas: &[ToolSchema]) -> String {
    let mut output = String::new();

    output.push_str("Available tools:\n\n");

    // Find max name length for alignment
    let max_len = schemas.iter().map(|s| s.name.len()).max().unwrap_or(0);

    for schema in schemas {
        output.push_str(&format!(
            "  {:width$}  {}\n",
            schema.name,
            schema.description,
            width = max_len
        ));
    }

    output.push_str("\nUse 'help <tool>' for detailed help.\n");
    output
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
            ToolSchema::new("echo", "Print arguments to stdout")
                .param(ParamSchema::optional(
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
    async fn test_help_list_all() {
        let mut ctx = make_ctx_with_schemas();
        let args = ToolArgs::new();

        let result = Help.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("Available tools:"));
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
    async fn test_help_unknown_tool() {
        let mut ctx = make_ctx_with_schemas();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("nonexistent".into()));

        let result = Help.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("unknown tool"));
    }

    #[tokio::test]
    async fn test_help_empty_registry() {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = Help.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("Available tools:"));
    }
}
