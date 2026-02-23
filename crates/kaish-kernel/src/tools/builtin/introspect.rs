//! Introspection builtins for kaish.
//!
//! These builtins provide visibility into kernel state for context generation.

use async_trait::async_trait;

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

// ============================================================================
// kaish-tools — List available tools
// ============================================================================

/// Tools builtin: lists all available tools and their schemas.
pub struct Tools;

#[async_trait]
impl Tool for Tools {
    fn name(&self) -> &str {
        "kaish-tools"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("kaish-tools", "List available tools and their schemas")
            .param(ParamSchema::optional(
                "name",
                "string",
                crate::ast::Value::Null,
                "Show detailed schema for a specific tool",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let tool_name = args.get_string("name", 0);

        if let Some(name) = tool_name {
            format_tool_detail(&ctx.tool_schemas, &name)
        } else {
            format_tool_list(&ctx.tool_schemas)
        }
    }
}

fn format_tool_list(schemas: &[ToolSchema]) -> ExecResult {
    let headers = vec![
        "NAME".to_string(),
        "DESCRIPTION".to_string(),
        "PARAMS".to_string(),
    ];

    let nodes: Vec<OutputNode> = schemas
        .iter()
        .map(|s| {
            let param_count = s.params.len().to_string();
            OutputNode::new(&s.name).with_cells(vec![s.description.clone(), param_count])
        })
        .collect();

    ExecResult::with_output(OutputData::table(headers, nodes))
}

fn format_tool_detail(schemas: &[ToolSchema], name: &str) -> ExecResult {
    let schema = schemas.iter().find(|s| s.name == name);

    match schema {
        Some(s) => {
            let mut output = format!("{}\n{}\n\n", s.name, s.description);

            if !s.params.is_empty() {
                output.push_str("Parameters:\n");
                for p in &s.params {
                    let required = if p.required { "(required)" } else { "(optional)" };
                    output.push_str(&format!(
                        "  {} : {} {}\n    {}\n",
                        p.name, p.param_type, required, p.description
                    ));
                }
            }

            ExecResult::with_output(OutputData::text(output))
        }
        None => ExecResult::failure(1, format!("tool not found: {}", name)),
    }
}

// ============================================================================
// kaish-mounts — List VFS mount points
// ============================================================================

/// Mounts builtin: lists all VFS mount points.
pub struct Mounts;

#[async_trait]
impl Tool for Mounts {
    fn name(&self) -> &str {
        "kaish-mounts"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("kaish-mounts", "List VFS mount points")
    }

    async fn execute(&self, _args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let mounts = ctx.backend.mounts();

        let headers = vec![
            "PATH".to_string(),
            "MODE".to_string(),
        ];

        let nodes: Vec<OutputNode> = mounts
            .iter()
            .map(|m| {
                let mode = if m.read_only { "ro" } else { "rw" };
                OutputNode::new(m.path.to_string_lossy()).with_cells(vec![mode.to_string()])
            })
            .collect();

        ExecResult::with_output(OutputData::table(headers, nodes))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::interpreter::{apply_output_format, OutputFormat};
    use crate::tools::ToolSchema as TS;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        vfs.mount("/tmp", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_tool_schemas(vec![
            TS::new("echo", "Print arguments"),
            TS::new("cat", "Concatenate files"),
        ]);
        ctx
    }

    // ============================
    // tools tests
    // ============================

    #[tokio::test]
    async fn test_tools_list_plain() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Tools.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("echo"));
        assert!(result.out.contains("cat"));
        // Should have structured OutputData with table headers
        let output = result.output.as_ref().expect("should have OutputData");
        assert!(output.headers.is_some());
    }

    #[tokio::test]
    async fn test_tools_list_json_via_global_flag() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Tools.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Simulate global --json (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);
        let data: Vec<serde_json::Value> = serde_json::from_str(&result.out).expect("valid JSON");
        assert_eq!(data.len(), 2);
        assert!(data[0].get("NAME").is_some());
    }

    #[tokio::test]
    async fn test_tools_detail() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("echo".into()));

        let result = Tools.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("echo"));
        assert!(result.out.contains("Print arguments"));
    }

    #[tokio::test]
    async fn test_tools_detail_not_found() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("nonexistent".into()));

        let result = Tools.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("tool not found"));
    }

    // ============================
    // mounts tests
    // ============================

    #[tokio::test]
    async fn test_mounts_list_plain() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Mounts.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("/"));
        assert!(result.out.contains("/tmp"));
        assert!(result.out.contains("rw"));
    }

    #[tokio::test]
    async fn test_mounts_list_json_via_global_flag() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Mounts.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Simulate global --json (handled by kernel)
        let result = apply_output_format(result, OutputFormat::Json);
        let data: Vec<serde_json::Value> = serde_json::from_str(&result.out).expect("valid JSON");
        assert!(!data.is_empty());

        let paths: Vec<&str> = data
            .iter()
            .filter_map(|v| v.get("PATH").and_then(|p| p.as_str()))
            .collect();
        assert!(paths.contains(&"/"));
        assert!(paths.contains(&"/tmp"));
    }
}
