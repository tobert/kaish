//! scatter — 散 — Fan out input items for parallel processing.
//!
//! Scatter reads input (lines or JSON array), splits into items,
//! and sets up parallel execution context. Used with `gather` to
//! collect results.
//!
//! # Usage
//!
//! ```text
//! echo "a\nb\nc" | scatter | process ${ITEM} | gather
//! scatter as=URL limit=4                      # bind to ${URL}, max 4 parallel
//! ```

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::scheduler::parse_scatter_options;
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
        ToolSchema::new("scatter", "散 — Fan out input items for parallel processing")
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
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Parse options for reporting
        let opts = parse_scatter_options(&args);

        // Get input
        let input = ctx.take_stdin().unwrap_or_default();
        let items = split_items(&input);

        if items.is_empty() {
            return ExecResult::success("");
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

        ExecResult::success(output)
    }
}

/// Split input into items (by newlines or JSON array).
fn split_items(input: &str) -> Vec<String> {
    let trimmed = input.trim();

    // Try to parse as JSON array first
    if trimmed.starts_with('[')
        && let Ok(arr) = serde_json::from_str::<Vec<serde_json::Value>>(trimmed) {
            return arr
                .into_iter()
                .map(|v| match v {
                    serde_json::Value::String(s) => s,
                    other => other.to_string(),
                })
                .collect();
        }

    // Fall back to line splitting
    trimmed
        .lines()
        .map(|s| s.to_string())
        .filter(|s| !s.is_empty())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_split_items_lines() {
        let items = split_items("one\ntwo\nthree");
        assert_eq!(items, vec!["one", "two", "three"]);
    }

    #[test]
    fn test_split_items_json() {
        let items = split_items(r#"["a", "b", "c"]"#);
        assert_eq!(items, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_split_items_empty() {
        let items = split_items("");
        assert!(items.is_empty());
    }

    #[tokio::test]
    async fn test_scatter_with_input() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_stdin("item1\nitem2\nitem3".to_string());

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
    async fn test_scatter_with_options() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_stdin("a\nb".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("as".to_string(), Value::String("URL".to_string()));
        args.named.insert("limit".to_string(), Value::Int(4));

        let result = Scatter.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("as=$URL"));
        assert!(result.out.contains("limit=4"));
    }
}
