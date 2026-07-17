//! unset — Remove variables from scope.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Unset tool: removes variables from the current scope.
pub struct Unset;

/// clap-derived argv layer for unset.
#[derive(Parser, Debug)]
#[command(name = "unset", about = "Remove variables from scope")]
struct UnsetArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Variable names to remove from the current scope.
    names: Vec<String>,
}

#[async_trait]
impl Tool for Unset {
    fn name(&self) -> &str {
        "unset"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &UnsetArgs::command(),
            "unset",
            "Remove variables from scope",
            [
                ("Remove a variable", "unset MY_VAR"),
                ("Remove multiple", "unset A B C"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("unset: {e}")),
        };
        let parsed = match UnsetArgs::try_parse_from(
            std::iter::once("unset".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("unset: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(1, "unset: missing variable name");
        }

        for arg in &args.positional {
            // Loud on binary (GH #116): `unset $BIN` must not silently try to
            // remove a variable literally named `[binary: N bytes]`.
            let name = match arg {
                Value::String(s) => s.clone(),
                other => match crate::interpreter::value_to_text_sink_named(other, "a variable name") {
                    Ok(s) => s,
                    Err(e) => return ExecResult::failure(1, format!("unset: {e}")),
                },
            };

            // POSIX: unsetting a nonexistent variable is not an error.
            ctx.scope.remove(&name);
        }

        // POSIX unset is silent — emitting the removed-count polluted
        // `$()` captures and `--json` output with a stray number
        // (found by the --json sweep 2026-06-11).
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
    async fn test_unset_existing_variable() {
        let mut ctx = make_ctx();
        ctx.scope.set("X", Value::Int(42));
        assert!(ctx.scope.get("X").is_some());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("X".into()));

        let result = Unset.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "", "POSIX unset is silent");
        assert!(ctx.scope.get("X").is_none());
    }

    #[tokio::test]
    async fn test_unset_nonexistent_variable() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("MISSING".into()));

        let result = Unset.execute(args, &mut ctx).await;
        assert!(result.ok(), "unsetting a missing variable is not an error");
        assert_eq!(&*result.text_out(), "", "POSIX unset is silent");
    }

    #[tokio::test]
    async fn test_unset_multiple_variables() {
        let mut ctx = make_ctx();
        ctx.scope.set("A", Value::Int(1));
        ctx.scope.set("B", Value::Int(2));
        ctx.scope.set("C", Value::Int(3));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("A".into()));
        args.positional.push(Value::String("B".into()));
        args.positional.push(Value::String("D".into())); // doesn't exist

        let result = Unset.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "", "POSIX unset is silent");
        assert!(ctx.scope.get("A").is_none());
        assert!(ctx.scope.get("B").is_none());
        assert!(ctx.scope.get("C").is_some());
    }

    #[tokio::test]
    async fn test_unset_no_args() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Unset.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }
}
