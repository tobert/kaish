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
