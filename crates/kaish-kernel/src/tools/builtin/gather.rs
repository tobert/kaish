//! gather — 集 — Collect results from parallel scatter processing.
//!
//! Gather collects results from parallel scatter workers and outputs
//! them as lines or JSON.
//!
//! # Usage
//!
//! ```text
//! echo "a\nb\nc" | scatter | process ${ITEM} | gather
//! gather --json                               # output as JSON array
//! gather first=5                              # take first 5 results
//! gather --progress                           # show progress
//! ```

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::scheduler::{parse_gather_options, GatherOptions};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Gather tool: collect results from parallel processing.
///
/// Note: This tool is a marker for the pipeline runner. The actual
/// result collection is handled by ScatterGatherRunner when it
/// detects gather in a pipeline.
pub struct Gather;

#[async_trait]
impl Tool for Gather {
    fn name(&self) -> &str {
        "gather"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("gather", "集 — Collect results from parallel scatter processing")
            .param(ParamSchema::optional(
                "first",
                "int",
                Value::Int(0),
                "Take first N results only (0 = all)",
            ))
            .param(ParamSchema::optional(
                "format",
                "string",
                Value::String("lines".to_string()),
                "Output format: 'lines' or 'json'",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Parse options
        let opts = parse_gather_options(&args);

        // Get input (in standalone mode, just pass through)
        let input = ctx.take_stdin().unwrap_or_default();

        if input.is_empty() {
            return ExecResult::success("");
        }

        // In standalone mode (not in a scatter/gather pipeline),
        // format the input according to options
        let output = format_output(&input, &opts);

        ExecResult::success(output)
    }
}

/// Format output according to gather options.
fn format_output(input: &str, opts: &GatherOptions) -> String {
    let lines: Vec<&str> = input.lines().collect();

    let lines_to_use = if opts.first > 0 && opts.first < lines.len() {
        &lines[..opts.first]
    } else {
        &lines[..]
    };

    if opts.format == "json" {
        // Output as JSON array of strings
        let json_arr: Vec<serde_json::Value> = lines_to_use
            .iter()
            .map(|s| serde_json::Value::String(s.to_string()))
            .collect();

        serde_json::to_string_pretty(&json_arr).unwrap_or_default()
    } else {
        // Output as lines
        lines_to_use.join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_output_lines() {
        let opts = GatherOptions::default();
        let output = format_output("a\nb\nc", &opts);
        assert_eq!(output, "a\nb\nc");
    }

    #[test]
    fn test_format_output_json() {
        let opts = GatherOptions {
            format: "json".to_string(),
            ..Default::default()
        };
        let output = format_output("a\nb", &opts);
        assert!(output.contains("\"a\""));
        assert!(output.contains("\"b\""));
    }

    #[test]
    fn test_format_output_first_n() {
        let opts = GatherOptions {
            first: 2,
            ..Default::default()
        };
        let output = format_output("a\nb\nc\nd", &opts);
        assert_eq!(output, "a\nb");
    }

    #[test]
    fn test_format_output_first_n_json() {
        let opts = GatherOptions {
            first: 2,
            format: "json".to_string(),
            ..Default::default()
        };
        let output = format_output("a\nb\nc", &opts);
        // Should only have 2 items
        let arr: Vec<String> = serde_json::from_str(&output).unwrap();
        assert_eq!(arr.len(), 2);
        assert_eq!(arr[0], "a");
        assert_eq!(arr[1], "b");
    }

    #[tokio::test]
    async fn test_gather_passthrough() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_stdin("result1\nresult2\nresult3".to_string());

        let result = Gather.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "result1\nresult2\nresult3");
    }

    #[tokio::test]
    async fn test_gather_json_flag() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_stdin("a\nb".to_string());

        let mut args = ToolArgs::new();
        args.flags.insert("json".to_string());

        let result = Gather.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("["));
        assert!(result.out.contains("\"a\""));
    }

    #[tokio::test]
    async fn test_gather_first_option() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_stdin("1\n2\n3\n4\n5".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("first".to_string(), Value::Int(3));

        let result = Gather.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out, "1\n2\n3");
    }

    #[tokio::test]
    async fn test_gather_empty_input() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let result = Gather.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.is_empty());
    }
}
