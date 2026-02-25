//! scatter — Fan out input items for parallel processing.
//!
//! Scatter requires structured data from upstream (split/seq/glob/find).
//! kaish does not split implicitly — pipe through `split` first.
//!
//! # Usage
//!
//! ```text
//! split "a,b,c" "," | scatter | process ${ITEM} | gather
//! seq 1 10 | scatter as=N limit=4 | process ${N} | gather
//! ```

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::scheduler::{extract_items, parse_scatter_options};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Scatter tool: fan out items for parallel processing.
///
/// Note: This tool is a marker for the pipeline runner. The actual
/// parallel execution is handled by ScatterGatherRunner when it
/// detects scatter in a pipeline.
pub struct Scatter;

#[async_trait]
impl Tool for Scatter {
    fn name(&self) -> &str {
        "scatter"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("scatter", "Fan out input items for parallel processing")
            .param(ParamSchema::optional(
                "as",
                "string",
                Value::String("ITEM".to_string()),
                "Variable name to bind each item to",
            ))
            .param(ParamSchema::optional(
                "limit",
                "int",
                Value::Int(8),
                "Maximum parallelism (concurrent workers)",
            ))
            .example("Parallel processing", "seq 1 10 | scatter | echo ${ITEM} | gather")
            .example("Custom variable name", "split \"a,b,c\" \",\" | scatter as=X | echo ${X} | gather")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Parse options for reporting
        let opts = parse_scatter_options(&args);

        // Get structured data and text from stdin
        let data = ctx.take_stdin_data();
        let text = ctx.read_stdin_to_string().await.unwrap_or_default();

        let items = match extract_items(data.as_ref(), &text) {
            Ok(items) => items,
            Err(msg) => return ExecResult::failure(1, msg),
        };

        if items.is_empty() {
            return ExecResult::with_output(OutputData::text(""));
        }

        // In standalone mode (not in a scatter/gather pipeline),
        // just output each item on a line with the variable binding info
        let output = format!(
            "scatter: {} items (as=${}, limit={})\n{}",
            items.len(),
            opts.var_name,
            opts.limit,
            items.join("\n")
        );

        ExecResult::with_output(OutputData::text(output))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_scatter_with_structured_data() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        // Simulate output from split/seq — structured JSON array
        let data = Value::Json(serde_json::json!(["item1", "item2", "item3"]));
        ctx.set_stdin_with_data("item1\nitem2\nitem3".to_string(), Some(data));

        let result = Scatter.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("3 items"));
        assert!(result.out.contains("item1"));
    }

    #[tokio::test]
    async fn test_scatter_empty_input() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = Scatter.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.is_empty());
    }

    #[tokio::test]
    async fn test_scatter_multiline_is_one_item() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        // Multi-line text without structured data = one item (no implicit splitting)
        ctx.set_stdin("a\nb\nc".to_string());

        let result = Scatter.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("1 items"), "should be 1 item: {}", result.out);
    }

    #[tokio::test]
    async fn test_scatter_single_line_text() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        // Single-line text is fine — one item
        ctx.set_stdin("hello".to_string());

        let result = Scatter.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("1 items"));
        assert!(result.out.contains("hello"));
    }

    #[tokio::test]
    async fn test_scatter_with_options() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        let data = Value::Json(serde_json::json!(["a", "b"]));
        ctx.set_stdin_with_data("a\nb".to_string(), Some(data));

        let mut args = ToolArgs::new();
        args.named.insert("as".to_string(), Value::String("URL".to_string()));
        args.named.insert("limit".to_string(), Value::Int(4));

        let result = Scatter.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("as=$URL"));
        assert!(result.out.contains("limit=4"));
    }
}
