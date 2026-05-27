//! Global flags shared by every builtin via `#[command(flatten)]`.
//!
//! Today this is just `--json`. Every builtin flattens `GlobalFlags` into its
//! own clap struct and calls `parsed.global.apply(ctx)` after parsing; the
//! kernel reads `ctx.output_format` after `execute()` returns and applies the
//! format via `apply_output_format`. See `docs/clap-migration.md`.

use clap::Args;

use crate::interpreter::OutputFormat;
use crate::tools::{ExecContext, ToolArgs};

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

    /// Honor `--json` straight off `ToolArgs` before any per-builtin clap parse.
    ///
    /// The kernel calls this just before `tool.execute()` so the format is set
    /// even when a builtin's own `try_parse_from` rejects argv and returns
    /// before `parsed.global.apply(ctx)` would have run. Idempotent with the
    /// per-builtin apply: both writing `OutputFormat::Json` yields the same
    /// state.
    pub fn apply_from_args(args: &ToolArgs, ctx: &mut ExecContext) {
        if args.has_flag("json") {
            ctx.output_format = Some(OutputFormat::Json);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[test]
    fn apply_from_args_sets_json_when_flag_present() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("json".to_string());

        GlobalFlags::apply_from_args(&args, &mut ctx);
        assert!(matches!(ctx.output_format, Some(OutputFormat::Json)));
    }

    #[test]
    fn apply_from_args_leaves_format_alone_when_absent() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();
        GlobalFlags::apply_from_args(&args, &mut ctx);
        assert!(ctx.output_format.is_none());
    }

    #[test]
    fn apply_from_args_idempotent_with_apply() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("json".to_string());

        // Simulate kernel pre-apply followed by builtin's parsed.global.apply.
        GlobalFlags::apply_from_args(&args, &mut ctx);
        let gf = GlobalFlags { json: true };
        gf.apply(&mut ctx);
        assert!(matches!(ctx.output_format, Some(OutputFormat::Json)));
    }
}
