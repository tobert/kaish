//! Global flags shared by every builtin via `#[command(flatten)]`.
//!
//! Today this is just `--json`. Every builtin flattens `GlobalFlags` into its
//! own clap struct and calls `parsed.global.apply(ctx)` after parsing; the
//! kernel reads the output format the flag set (via
//! [`ToolCtx::set_output_format`](crate::ToolCtx::set_output_format)) after
//! `execute()` returns and applies it. See `docs/clap-migration.md`.

use clap::Args;

use kaish_types::OutputFormat;

use crate::ctx::ToolCtx;
use kaish_types::ToolArgs;

/// Flags injected into every migrated builtin via `#[command(flatten)] global: GlobalFlags`.
///
/// Builtins call `parsed.global.apply(ctx)` after their own argv parse so the
/// dispatcher can read the output format post-execute and apply it.
#[derive(Args, Debug, Clone, Default)]
pub struct GlobalFlags {
    /// Render structured output as JSON.
    #[arg(long)]
    pub json: bool,
}

impl GlobalFlags {
    /// Apply the flags to `ctx` so the dispatcher can pick them up after the
    /// builtin's `execute()` returns.
    pub fn apply(&self, ctx: &mut dyn ToolCtx) {
        if self.json {
            ctx.set_output_format(OutputFormat::Json);
        }
    }

    /// Honor `--json` straight off `ToolArgs` before any per-builtin clap parse.
    ///
    /// The kernel calls this just before `tool.execute()` so the format is set
    /// even when a builtin's own `try_parse_from` rejects argv and returns
    /// before `parsed.global.apply(ctx)` would have run. Idempotent with the
    /// per-builtin apply: both writing `OutputFormat::Json` yields the same
    /// state.
    pub fn apply_from_args(args: &ToolArgs, ctx: &mut dyn ToolCtx) {
        if args.has_flag("json") {
            ctx.set_output_format(OutputFormat::Json);
        }
    }
}
