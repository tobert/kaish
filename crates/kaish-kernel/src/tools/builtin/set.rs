//! set — Set shell options (like set -e).

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Set tool: configure shell options.
///
/// Currently supports:
/// - `-e` / `+e`: Enable/disable error-exit mode (exit on command failure)
///
/// Unrecognized options are silently ignored for bash compatibility.
pub struct Set;

#[async_trait]
impl Tool for Set {
    fn name(&self) -> &str {
        "set"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("set", "Set shell options")
            .param(ParamSchema::optional(
                "options",
                "string",
                Value::Null,
                "Shell options (-e, +e, etc.)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // No arguments: show current settings
        if args.positional.is_empty() && args.flags.is_empty() {
            let mut output = String::new();
            if ctx.scope.error_exit_enabled() {
                output.push_str("set -e\n");
            }
            return ExecResult::success(output.trim_end());
        }

        // Process options from both flags and positional args
        // From parser: ShortFlag("e") -> args.flags contains "e"
        // From parser: PlusFlag("e") -> args.positional contains String("+e")
        // From direct call: args.positional might contain String("-e") or String("+e")
        for flag in &args.flags {
            match flag.as_str() {
                "e" => ctx.scope.set_error_exit(true),
                // Silently ignore unrecognized flags for bash compatibility
                _ => {}
            }
        }

        for arg in &args.positional {
            let opt = match arg {
                Value::String(s) => s.as_str(),
                _ => continue,
            };

            match opt {
                "-e" => ctx.scope.set_error_exit(true),
                "+e" => ctx.scope.set_error_exit(false),
                // Silently ignore unrecognized options for bash compatibility:
                // -u, -o, -x, -v, pipefail, etc.
                _ => {}
            }
        }

        ExecResult::success("")
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

    #[tokio::test]
    async fn test_set_e_enables_error_exit() {
        let mut ctx = make_ctx();
        assert!(!ctx.scope.error_exit_enabled());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-e".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.error_exit_enabled());
    }

    #[tokio::test]
    async fn test_set_plus_e_disables_error_exit() {
        let mut ctx = make_ctx();
        ctx.scope.set_error_exit(true);
        assert!(ctx.scope.error_exit_enabled());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("+e".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.scope.error_exit_enabled());
    }

    #[tokio::test]
    async fn test_set_ignores_unknown_options() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-u".into()));
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("pipefail".into()));
        args.positional.push(Value::String("-x".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn test_set_no_args_shows_settings() {
        let mut ctx = make_ctx();
        ctx.scope.set_error_exit(true);

        let args = ToolArgs::new();
        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("set -e"));
    }

    #[tokio::test]
    async fn test_set_euo_pipefail() {
        // Common bash idiom: set -euo pipefail
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-e".into()));
        args.positional.push(Value::String("-u".into()));
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("pipefail".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.error_exit_enabled());
    }
}
