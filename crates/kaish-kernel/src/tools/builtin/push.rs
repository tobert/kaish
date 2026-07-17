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
//! **Bracket-path targets.** `push services[web][tags] item` walks the
//! nested path the same way an assignment lvalue does — a missing
//! intermediate key or a non-list leaf is a loud error, never autoviv (see
//! `Scope::walk_append`). The lexer recognizes `push`'s target with its own
//! trigger (independent of the `=`-followed lvalue trigger an assignment
//! uses — the target has no trailing `=` to key off) so
//! `services[web][tags]` fuses verbatim into a path to walk instead of
//! glob-expanding against the filesystem (GH #183); see
//! `lexer::PushTarget`.
//!
//! # Examples
//!
//! ```kaish
//! xs=[a b]
//! push xs c                        # xs is now [a b c]
//! push xs $rec                     # values are read as typed Values, not stringified
//! push services[web][tags] canary  # bracket-path target
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

        // The lexer hands a bareword (`xs`) and a bracket path
        // (`services[web][tags]`) through identically — a fused `Ident`
        // token, verbatim — so both are parsed the same way here via the
        // shared `${...}` path grammar (`services[web][tags]` round-trips
        // through it exactly like `${services[web][tags]}`'s interior).
        let path = crate::parser::parse_varpath(&format!("${{{name}}}"));

        let values: Vec<Value> = args.positional[1..].to_vec();
        if values.is_empty() {
            return ExecResult::failure(2, "push: usage: push NAME VALUE...");
        }

        match ctx.scope.walk_append(&path, values) {
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

    /// Bracket-path target (GH #183): the target string arrives already
    /// fused verbatim by the lexer (`lexer::PushTarget`) — this pins the
    /// Tool→`Scope::walk_append` wiring directly, independent of lexing.
    #[tokio::test]
    async fn push_bracket_path_target_extends_the_nested_list() {
        let mut ctx = make_ctx();
        ctx.scope.set(
            "services",
            Value::Json(serde_json::json!({"web": {"tags": ["a"]}})),
        );

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("services[web][tags]".into()));
        args.positional.push(Value::String("b".into()));

        let result = Push.execute(args, &mut ctx).await;
        assert!(result.ok(), "push failed: {}", result.err);
        assert_eq!(
            ctx.scope.get("services"),
            Some(&Value::Json(serde_json::json!({"web": {"tags": ["a", "b"]}})))
        );
    }
}
