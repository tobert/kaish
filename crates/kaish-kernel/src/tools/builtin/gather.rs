//! gather — Collect results from parallel scatter processing.
//!
//! Gather emits one JSONL result record per worker, in item order, every
//! worker including failures (GH #73). `--lines` is the raw-text escape hatch
//! (successful workers' stdout; hard error if any worker failed). The kernel
//! `--json` flag renders the same records as one JSON array.
//!
//! # Usage
//!
//! ```text
//! seq 1 5 | scatter | process "$ITEM" | gather
//! ... | gather | jq -r 'select(.ok) | .out'    # successes' outputs
//! ... | gather --lines                         # raw outputs; loud on failure
//! ... | gather --json                          # one JSON array (kernel flag)
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData};
use crate::scheduler::parse_gather_options;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Gather tool: collect results from parallel processing.
///
/// Note: This tool is a marker for the pipeline runner. The actual
/// result collection is handled by ScatterGatherRunner when it
/// detects gather in a pipeline.
pub struct Gather;

/// clap-derived argv layer for gather.
#[derive(Parser, Debug)]
#[command(name = "gather", about = "Collect results from parallel scatter processing")]
struct GatherArgs {
    /// Raw text mode: each successful worker's stdout in item order.
    /// Hard error (exit 123) if ANY worker failed — bare lines can't
    /// represent a failure. Default is JSONL result records.
    #[arg(id = "lines", long = "lines")]
    _lines: bool,

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
                ("Collect scatter results (JSONL rows)", "seq 1 10 | scatter | work \"$ITEM\" | gather"),
                ("Successes' outputs only", "... | gather | jq -r 'select(.ok) | .out'"),
                ("Raw outputs, loud on any failure", "... | gather --lines"),
                ("One JSON array of records", "... | gather --json"),
            ],
        )
        // scatter/gather own their output: the runner renders JSONL/array/lines
        // itself, and the kernel-wide --json flag must REACH the tool (it
        // selects the array view) instead of being consumed upstream.
        .with_owned_output()
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match GatherArgs::try_parse_from(
            std::iter::once("gather".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("gather: {e}")),
        };
        parsed.global.apply(ctx);

        // Validate flags even standalone (parse_gather_options is the runner's
        // reader; here it only needs to not reject).
        let _opts = parse_gather_options(&args);

        // Standalone (not in a scatter/gather pipeline): pass stdin through.
        // The real result-record rendering lives in the ScatterGatherRunner.
        let input = match ctx.read_stdin_to_text().await {
            Ok(s) => s.unwrap_or_default(),
            Err(e) => return ExecResult::failure(2, format!("gather: {e}")),
        };

        if input.is_empty() {
            return ExecResult::success("");
        }

        ExecResult::with_output(OutputData::text(input))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_gather_standalone_passthrough() {
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
    async fn test_gather_standalone_lines_flag_accepted() {
        use crate::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.set_stdin("a\nb".to_string());

        let mut args = ToolArgs::new();
        args.flags.insert("lines".to_string());
        let result = Gather.execute(args, &mut ctx).await;
        assert!(result.ok(), "--lines must parse standalone: {}", result.err);
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
