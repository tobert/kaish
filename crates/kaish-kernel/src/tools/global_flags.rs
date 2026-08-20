//! Global flags shared by every builtin via `#[command(flatten)]`.
//!
//! `GlobalFlags` moved to the leaf `kaish-tool-api` crate so out-of-tree tools
//! flatten the same `--json` surface. Re-exported here so existing
//! `crate::tools::GlobalFlags` paths keep working.

pub use kaish_tool_api::GlobalFlags;

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::OutputFormat;
    use crate::tools::{ExecContext, ToolArgs};
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

        GlobalFlags::apply_from_args(&args, false, &mut ctx);
        assert!(matches!(ctx.output_format, Some(OutputFormat::Json)));
    }

    #[test]
    fn apply_from_args_leaves_format_alone_when_absent() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();
        GlobalFlags::apply_from_args(&args, false, &mut ctx);
        assert!(ctx.output_format.is_none());
    }

    #[test]
    fn apply_from_args_idempotent_with_apply() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("json".to_string());

        // Simulate kernel pre-apply followed by builtin's parsed.global.apply.
        GlobalFlags::apply_from_args(&args, false, &mut ctx);
        let gf = GlobalFlags { json: true };
        gf.apply(&mut ctx);
        assert!(matches!(ctx.output_format, Some(OutputFormat::Json)));
    }

    /// A `raw_argv` builtin (`test`, `kill` — GH #198) never lifts `--json`
    /// into `args.flags`; it survives as a literal string in `positional`
    /// instead. Without the `positional_json_flag` fallback this silently
    /// dropped `--json` for every raw_argv tool.
    #[test]
    fn apply_from_args_sets_json_from_raw_argv_positional() {
        use crate::ast::Value;

        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("--json".to_string()));

        GlobalFlags::apply_from_args(&args, true, &mut ctx);
        assert!(matches!(ctx.output_format, Some(OutputFormat::Json)));
    }

    /// `--json=yes`/`--json=1` must be recognized under raw_argv too, with
    /// the same truthiness `ToolArgs::has_flag` uses for the normal
    /// (non-raw_argv) `named` path — found via kaibo review of GH #198's
    /// `positional_json_flag` (it originally only matched the literal
    /// `"--json=true"`).
    #[test]
    fn apply_from_args_sets_json_from_raw_argv_positional_truthy_value() {
        use crate::ast::Value;

        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("--json=yes".to_string()));

        GlobalFlags::apply_from_args(&args, true, &mut ctx);
        assert!(matches!(ctx.output_format, Some(OutputFormat::Json)));
    }

    /// `--json=false`/`--json=0` under raw_argv must NOT enable JSON —
    /// mirrors `has_flag`'s falsy strings exactly.
    #[test]
    fn apply_from_args_leaves_format_alone_for_raw_argv_positional_falsy_value() {
        use crate::ast::Value;

        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("--json=false".to_string()));

        GlobalFlags::apply_from_args(&args, true, &mut ctx);
        assert!(ctx.output_format.is_none());
    }

    /// A literal `--json` AFTER a real `--` end-of-options marker is an
    /// OPERAND (e.g. a job/PID literally spelled `--json`), never the global
    /// flag — for both raw_argv tools (`kill -- --json`) and the
    /// pre-existing non-raw_argv case (`echo -- --json`, which the ordinary
    /// binder already relegates to `positional` via `past_double_dash`).
    /// Found via kaibo review of GH #198's `--` handling: an unscoped
    /// positional scan would have silently turned that literal operand into
    /// the JSON flag for every builtin, not just raw_argv ones.
    #[test]
    fn apply_from_args_ignores_json_positional_after_double_dash() {
        use crate::ast::Value;

        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("--".to_string()));
        args.positional.push(Value::String("--json".to_string()));

        GlobalFlags::apply_from_args(&args, true, &mut ctx);
        assert!(ctx.output_format.is_none());
    }

    /// The positional scan is for `raw_argv` tools only. A typed tool's
    /// positional `--json` is an operand after `--` — the binder consumes the
    /// marker, so nothing bounds the scan — and reading it back as the flag
    /// made `echo -- --json hi` answer in JSON.
    #[test]
    fn apply_from_args_ignores_a_typed_tools_positional_json() {
        use crate::ast::Value;

        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("--json".to_string()));

        GlobalFlags::apply_from_args(&args, false, &mut ctx);
        assert!(ctx.output_format.is_none());
    }
}
