//! Global flags shared by every builtin via `#[command(flatten)]`.
//!
//! Today this is just `--json`. Every builtin flattens `GlobalFlags` into its
//! own clap struct and calls `parsed.global.apply(ctx)` after parsing; the
//! kernel reads the output format the flag set (via
//! [`ToolCtx::set_output_format`](crate::ToolCtx::set_output_format)) after
//! `execute()` returns and applies it.

use clap::Args;

use kaish_types::{OutputFormat, Value};

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
    ///
    /// `raw_argv` is [`ToolSchema::raw_argv`](kaish_types::ToolSchema::raw_argv)
    /// for the tool being dispatched, and it decides whether `positional` is
    /// searched at all. Only a `raw_argv` tool keeps `--json` — and the `--`
    /// marker that bounds it — among its positionals. For every other tool a
    /// positional `--json` got there by being an operand after `--`, where the
    /// binder drops the marker, so searching would read the operand back as
    /// the kernel's flag and `echo -- --json hi` would answer in JSON.
    pub fn apply_from_args(args: &ToolArgs, raw_argv: bool, ctx: &mut dyn ToolCtx) {
        if args.has_flag("json") || (raw_argv && positional_json_flag(args)) {
            ctx.set_output_format(OutputFormat::Json);
        }
    }
}

/// `--json`/`--json=VALUE` surviving as a literal string in `args.positional`
/// is the `raw_argv` case (GH #198): a `raw_argv` tool's binder deliberately
/// does not lift ANY flag out of source order (that's the whole point — see
/// `ToolSchema::raw_argv`), so `--json` lands in `positional` instead of
/// `flags`. Without this, a raw_argv builtin (`test`, and now `kill`) would
/// silently ignore `--json` — a real, user-visible regression discovered
/// while adding `kill`'s signal shorthand.
///
/// Stops scanning at a literal `"--"` token: a real end-of-options marker
/// makes every following token an operand, not a flag — for `raw_argv` tools
/// that's `kill -- --json foo` (foo is a job/PID literally spelled
/// `--json`); for a NORMAL (non-raw_argv) tool it's the pre-existing case of
/// a post-`--` `--json` operand (`echo -- --json`), which the ordinary
/// binder already relegates to `positional` too (`past_double_dash` in
/// `bind_tool_args`). Without this boundary, the scan would reinterpret that
/// literal operand as the global JSON flag for every builtin, not just
/// raw_argv ones — a real regression this comment exists to prevent
/// reintroducing.
///
/// `--json=VALUE`'s truthiness mirrors `ToolArgs::has_flag`'s String-value
/// rule exactly (truthy unless empty, `"false"`, or `"0"`), so the two paths
/// agree on what counts as "on".
fn positional_json_flag(args: &ToolArgs) -> bool {
    args.positional
        .iter()
        .take_while(|v| !matches!(v, Value::String(s) if s == "--"))
        .any(|v| {
            let Value::String(s) = v else { return false };
            if s == "--json" {
                return true;
            }
            s.strip_prefix("--json=")
                .is_some_and(|val| !val.is_empty() && val != "false" && val != "0")
        })
}
