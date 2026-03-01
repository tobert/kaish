//! set — Set shell options (like set -e, set -o latch).

use async_trait::async_trait;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Set tool: configure shell options.
///
/// Supports:
/// - `-e` / `+e`: Enable/disable error-exit mode (exit on command failure)
/// - `-o latch` / `+o latch`: Enable/disable confirmation latch for dangerous ops
/// - `-o trash` / `+o trash`: Enable/disable trash-on-delete for rm
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
                "Shell options (-e, +e, -o latch, -o trash, etc.)",
            ))
            .example("Exit on error", "set -e")
            .example("Disable exit on error", "set +e")
            .example("Enable confirmation latch", "set -o latch")
            .example("Enable trash-on-delete", "set -o trash")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // No arguments: show current settings
        if args.positional.is_empty() && args.flags.is_empty() {
            let mut output = String::new();
            if ctx.scope.error_exit_enabled() {
                output.push_str("set -e\n");
            }
            if ctx.scope.latch_enabled() {
                output.push_str("set -o latch\n");
            }
            if ctx.scope.trash_enabled() {
                output.push_str("set -o trash\n");
            }
            return ExecResult::with_output(OutputData::text(output.trim_end()));
        }

        // Process flags (from parser: ShortFlag("e") -> args.flags contains "e")
        for flag in &args.flags {
            match flag.as_str() {
                "e" => ctx.scope.set_error_exit(true),
                "o" => {} // handled below with positional args
                _ => {}   // silently ignore for bash compatibility
            }
        }

        // Process positional args.
        // From parser: PlusFlag("e") -> String("+e"), String("-o") followed by String("latch"), etc.
        let positionals: Vec<&str> = args
            .positional
            .iter()
            .filter_map(|v| match v {
                Value::String(s) => Some(s.as_str()),
                _ => None,
            })
            .collect();

        let mut i = 0;
        while i < positionals.len() {
            let opt = positionals[i];
            match opt {
                "-e" => ctx.scope.set_error_exit(true),
                "+e" => ctx.scope.set_error_exit(false),
                "-o" => {
                    // Consume next positional as option name
                    if let Some(&name) = positionals.get(i + 1) {
                        match name {
                            "latch" => ctx.scope.set_latch_enabled(true),
                            "trash" => ctx.scope.set_trash_enabled(true),
                            _ => {} // silently ignore (pipefail, etc.)
                        }
                        i += 1; // skip the option name
                    }
                }
                "+o" => {
                    if let Some(&name) = positionals.get(i + 1) {
                        match name {
                            "latch" => ctx.scope.set_latch_enabled(false),
                            "trash" => ctx.scope.set_trash_enabled(false),
                            _ => {}
                        }
                        i += 1;
                    }
                }
                _ => {} // silently ignore
            }
            i += 1;
        }

        // Handle case where parser split `-o` into flags and the option name
        // ended up as a bare positional (flags=["o"], positional=["latch"]).
        // Only fire if no "-o" or "+o" appeared in positionals (which would have
        // already consumed the option name above).
        if args.flags.contains("o")
            && !positionals.iter().any(|p| *p == "-o" || *p == "+o")
        {
            // The first positional that matches a known option name gets enabled
            for &name in &positionals {
                match name {
                    "latch" => { ctx.scope.set_latch_enabled(true); break; }
                    "trash" => { ctx.scope.set_trash_enabled(true); break; }
                    _ => {}
                }
            }
        }

        ExecResult::with_output(OutputData::text(""))
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

    #[tokio::test]
    async fn test_set_o_latch_enables() {
        let mut ctx = make_ctx();
        assert!(!ctx.scope.latch_enabled());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("latch".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.latch_enabled());
    }

    #[tokio::test]
    async fn test_set_plus_o_latch_disables() {
        let mut ctx = make_ctx();
        ctx.scope.set_latch_enabled(true);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("+o".into()));
        args.positional.push(Value::String("latch".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.scope.latch_enabled());
    }

    #[tokio::test]
    async fn test_set_o_trash_enables() {
        let mut ctx = make_ctx();
        assert!(!ctx.scope.trash_enabled());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("trash".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.scope.trash_enabled());
    }

    #[tokio::test]
    async fn test_set_plus_o_trash_disables() {
        let mut ctx = make_ctx();
        ctx.scope.set_trash_enabled(true);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("+o".into()));
        args.positional.push(Value::String("trash".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.scope.trash_enabled());
    }

    #[tokio::test]
    async fn test_set_no_args_shows_all_options() {
        let mut ctx = make_ctx();
        ctx.scope.set_latch_enabled(true);
        ctx.scope.set_trash_enabled(true);

        let args = ToolArgs::new();
        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("set -o latch"));
        assert!(result.out.contains("set -o trash"));
    }

    #[tokio::test]
    async fn test_set_o_unknown_ignored() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-o".into()));
        args.positional.push(Value::String("pipefail".into()));

        let result = Set.execute(args, &mut ctx).await;
        assert!(result.ok());
    }
}
