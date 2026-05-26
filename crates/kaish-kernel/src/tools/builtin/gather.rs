//! gather — Collect results from parallel scatter processing.
//!
//! Gather collects results from parallel scatter workers and outputs
//! them as lines or JSON.
//!
//! # Usage
//!
//! ```text
//! echo "a\nb\nc" | scatter | process ${ITEM} | gather
//! gather --json                               # output as JSON array
//! gather --first 5                            # take first 5 results
//! gather --progress                           # show progress
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData};
use crate::scheduler::{parse_gather_options, GatherOptions};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Gather tool: collect results from parallel processing.
///
/// Note: This tool is a marker for the pipeline runner. The actual
/// result collection is handled by ScatterGatherRunner when it
/// detects gather in a pipeline.
pub struct Gather;

/// clap-derived argv layer for gather. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "gather", about = "Collect results from parallel scatter processing")]
struct GatherArgs {
    /// Take first N results only (0 = all).
    #[arg(id = "first", long = "first")]
    _first: Option<String>,

    /// Output format: 'lines' or 'json'.
    #[arg(id = "format", long = "format")]
    _format: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — option values read off args.named to preserve Value typing.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for Gather {
    fn name(&self) -> &str {
        "gather"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &GatherArgs::command(),
            "gather",
            "Collect results from parallel scatter processing",
            [
                ("Collect scatter results", "seq 1 10 | scatter | echo ${ITEM} | gather"),
                ("Collect first 5 results", "gather --first 5"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let parsed = match GatherArgs::try_parse_from(
            std::iter::once("gather".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("gather: {e}")),
        };
        parsed.global.apply(ctx);

        // Parse options (still reads from args.named to preserve Value::Int etc.)
        let opts = parse_gather_options(&args);

        // Get input (in standalone mode, just pass through)
        let input = ctx.read_stdin_to_string().await.unwrap_or_default();

        if input.is_empty() {
            return ExecResult::success("");
        }

        // In standalone mode (not in a scatter/gather pipeline),
        // format the input according to options
        let output = format_output(&input, &opts);

        ExecResult::with_output(OutputData::text(output))
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
    use crate::ast::Value;

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
        assert_eq!(&*result.text_out(), "result1\nresult2\nresult3");
    }

    #[tokio::test]
    async fn test_gather_format_json() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_stdin("a\nb".to_string());

        let mut args = ToolArgs::new();
        args.named.insert("format".to_string(), Value::String("json".to_string()));

        let result = Gather.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("["));
        assert!(result.text_out().contains("\"a\""));
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
        assert_eq!(&*result.text_out(), "1\n2\n3");
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
        assert!(result.text_out().is_empty());
    }
}
