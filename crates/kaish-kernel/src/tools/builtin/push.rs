//! push — append value(s) to a list variable, in place.
//!
//! Part of the native-collections OPS surface (see `docs/arrays-and-hashes.md`,
//! "Append — `push` (in-place); spelling frozen, pure `append` dropped"). `push`
//! mutates the named list in place, so it takes the variable **name**
//! (bareword, like `read`/`unset`), not `$name` — `push xs date`, not
//! `push $xs date`. The pure functional `append` builtin was deliberately
//! never added: its only niche (build a new list with an extra element) is
//! already covered by `...` spread (`new=[...$xs date]`), and a return-value
//! `append` re-imports the silent-discard trap the design doc's evidence #6
//! called out (a model wrote `append $colors purple` and threw the result
//! away, then reported the OLD length).
//!
//! `push` to an undefined target, or a target that isn't a list, is a loud
//! runtime error — never a silent create (see `Scope::walk_append`).
//!
//! **Scope note — bareword target only.** `push services[web][tags] item`
//! (a bracket-path target) is deferred: the target isn't followed by `=`, so
//! the lvalue lexer suppression never fires and `services[web][tags]` fuses
//! into a `GlobWord` that glob-expands (and fails as "no matches") before
//! `push` ever runs. Solving that needs its own lexer/parser design pass —
//! see `docs/issues.md`.
//!
//! # Examples
//!
//! ```kaish
//! xs=[a b]
//! push xs c              # xs is now [a b c]
//! push xs $rec           # values are read as typed Values, not stringified
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// push tool: append value(s) to a list variable, in place.
pub struct Push;

/// clap-derived argv layer for push.
#[derive(Parser, Debug)]
#[command(name = "push", about = "Append value(s) to a list variable, in place")]
struct PushArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Target name and values. Hidden sink — the real values are read off
    /// `args.positional` (typed `Value`s, not stringified) per the
    /// Value-typed positional rule.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for Push {
    fn name(&self) -> &str {
        "push"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &PushArgs::command(),
            "push",
            "Append value(s) to a list variable, in place",
            [
                ("Append one element", "push xs date"),
                ("Append a record value", "push xs $rec"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("push: {e}")),
        };
        let parsed = match PushArgs::try_parse_from(
            std::iter::once("push".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("push: {e}")),
        };
        parsed.global.apply(ctx);

        // Values MUST come from `args.positional` (typed), not the clap
        // struct — `to_argv()` stringifies, so `push xs $rec` would push the
        // stringified record instead of the record itself.
        let Some(target) = args.positional.first() else {
            return ExecResult::failure(2, "push: usage: push NAME VALUE...");
        };
        let name = match target {
            Value::String(s) => s.clone(),
            other => {
                return ExecResult::failure(
                    2,
                    format!(
                        "push: target must be a bareword variable name (e.g. `push xs val`), \
                         not {other:?}"
                    ),
                )
            }
        };

        let values: Vec<Value> = args.positional[1..].to_vec();
        if values.is_empty() {
            return ExecResult::failure(2, "push: usage: push NAME VALUE...");
        }

        match ctx.scope.walk_append(&name, values) {
            Ok(()) => ExecResult::success(""),
            Err(msg) => ExecResult::failure(1, msg),
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

    #[tokio::test]
    async fn push_appends_to_list_in_place() {
        let mut ctx = make_ctx();
        ctx.scope.set(
            "xs",
            Value::Json(serde_json::json!(["a", "b"])),
        );

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("xs".into()));
        args.positional.push(Value::String("c".into()));

        let result = Push.execute(args, &mut ctx).await;
        assert!(result.ok(), "push failed: {}", result.err);
        assert_eq!(
            ctx.scope.get("xs"),
            Some(&Value::Json(serde_json::json!(["a", "b", "c"])))
        );
    }

    #[tokio::test]
    async fn push_pushes_a_record_value_untouched() {
        let mut ctx = make_ctx();
        ctx.scope.set("xs", Value::Json(serde_json::json!([])));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("xs".into()));
        args.positional
            .push(Value::Json(serde_json::json!({"k": "v"})));

        let result = Push.execute(args, &mut ctx).await;
        assert!(result.ok(), "push failed: {}", result.err);
        assert_eq!(
            ctx.scope.get("xs"),
            Some(&Value::Json(serde_json::json!([{"k": "v"}])))
        );
    }

    #[tokio::test]
    async fn push_undefined_target_is_a_loud_error() {
        let mut ctx = make_ctx();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("nope".into()));
        args.positional.push(Value::String("x".into()));

        let result = Push.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not defined"), "got: {}", result.err);
    }

    #[tokio::test]
    async fn push_non_list_target_is_a_loud_error() {
        let mut ctx = make_ctx();
        ctx.scope.set("y", Value::String("hi".into()));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("y".into()));
        args.positional.push(Value::String("z".into()));

        let result = Push.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not a list"), "got: {}", result.err);
    }
}
