//! values — the values of a collection, insertion order.
//!
//! See `docs/arrays-and-hashes.md`, "OPS — keys/values are builtins; length is
//! the param-expansion `${#…}`". `values $collection` accepts a collection
//! directly (never nested through another builtin), same rationale as
//! [`super::keys`].
//!
//! jq semantics for the two collection shapes:
//! - a **record** → its values, insertion order (pairwise-aligned with `keys`);
//! - a **list** → its elements (the list itself, element-wise).
//!
//! This makes `$(values $c)` the uniform element-iteration idiom over ANY
//! collection.
//!
//! Pure data transform — no OS, no VFS — so it belongs in every capability
//! build, same footing as `fromjson`/`tojson`.
//!
//! # Examples
//!
//! ```kaish
//! user=$(fromjson '{"name":"amy","role":"maintainer"}')
//! vs=$(values $user)                    # ["amy","maintainer"]
//! xs=$(fromjson '["a","b","c"]')
//! for x in $(values $xs); do echo $x; done          # a b c (list elements)
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

use super::keys::describe_kind;

/// values tool: the values of a collection, insertion order.
pub struct Values;

/// clap-derived argv layer for values.
#[derive(Parser, Debug)]
#[command(name = "values", about = "The values of a collection, in insertion order")]
struct ValuesArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// The collection. Hidden sink — the real value is read off
    /// `args.positional` per the Value-typed positional rule.
    #[arg(hide = true)]
    collection: Vec<String>,
}

#[async_trait]
impl Tool for Values {
    fn name(&self) -> &str {
        "values"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ValuesArgs::command(),
            "values",
            "The values of a collection — record values or list elements (in .data)",
            [
                ("Record values, insertion order", "vs=$(values $user)"),
                ("List elements", "for x in $(values $xs); do echo $x; done"),
                (
                    "Pairwise-aligned with keys",
                    "for v in $(values $user); do echo $v; done",
                ),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("values: {e}")),
        };
        let parsed = match ValuesArgs::try_parse_from(
            std::iter::once("values".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("values: {e}")),
        };
        parsed.global.apply(ctx);

        match args.positional.first() {
            // A record → its values, insertion order (pairwise-aligned with keys).
            Some(Value::Json(serde_json::Value::Object(map))) => {
                let values: Vec<serde_json::Value> = map.values().cloned().collect();
                ExecResult::success_data(Value::Json(serde_json::Value::Array(values)))
            }
            // A list → its elements (jq semantics: the list itself, element-wise).
            // Makes `for x in $(values $xs)` the uniform element-iteration idiom.
            Some(Value::Json(serde_json::Value::Array(items))) => {
                ExecResult::success_data(Value::Json(serde_json::Value::Array(items.clone())))
            }
            Some(other) => ExecResult::failure(
                1,
                format!(
                    "values: expected a record or list, got {}",
                    describe_kind(other)
                ),
            ),
            None => ExecResult::failure(1, "values: no argument (expected a record or list)"),
        }
    }
}
