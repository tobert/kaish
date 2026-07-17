//! kaish-version — Print kaish version.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// kaish-version: prints the kaish version string.
pub struct KaishVersion;

/// clap-derived argv layer for kaish-version.
#[derive(Parser, Debug)]
#[command(name = "kaish-version", about = "Print kaish version")]
struct KaishVersionArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — to_argv() always emits `--` before positionals.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for KaishVersion {
    fn name(&self) -> &str {
        "kaish-version"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &KaishVersionArgs::command(),
            "kaish-version",
            "Print kaish version",
            [("Show version", "kaish-version")],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kaish-version: {e}")),
        };
        let parsed = match KaishVersionArgs::try_parse_from(
            std::iter::once("kaish-version".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kaish-version: {e}")),
        };
        parsed.global.apply(ctx);

        let version = env!("CARGO_PKG_VERSION");
        ExecResult::with_output(OutputData::text(format!("kaish {version}\n")))
    }
}
