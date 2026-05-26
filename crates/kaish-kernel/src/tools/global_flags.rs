//! Global flags shared by every builtin via `#[command(flatten)]`.
//!
//! Today this is just `--json`. Every builtin flattens `GlobalFlags` into its
//! own clap struct and calls `parsed.global.apply(ctx)` after parsing; the
//! kernel reads `ctx.output_format` after `execute()` returns and applies the
//! format via `apply_output_format`. See `docs/clap-migration.md`.

use clap::Args;

use crate::interpreter::OutputFormat;
use crate::tools::ExecContext;

/// Flags injected into every migrated builtin via `#[command(flatten)] global: GlobalFlags`.
///
/// Builtins call `parsed.global.apply(ctx)` after their own argv parse so the
/// dispatcher can read `ctx.output_format` post-execute and apply the format.
#[derive(Args, Debug, Clone, Default)]
pub struct GlobalFlags {
    /// Render structured output as JSON.
    #[arg(long)]
    pub json: bool,
}

impl GlobalFlags {
    /// Apply the flags to `ctx` so the dispatcher can pick them up after the
    /// builtin's `execute()` returns.
    pub fn apply(&self, ctx: &mut ExecContext) {
        if self.json {
            ctx.output_format = Some(OutputFormat::Json);
        }
    }
}
