//! values — the value list of a record, insertion order, pairwise-aligned
//! with `keys`.
//!
//! See `docs/arrays-and-hashes.md`, "OPS — keys/values are builtins; length is
//! the param-expansion `${#…}`". `values $record` accepts a record directly
//! (never nested through another builtin), same rationale as [`super::keys`].
//!
//! Pure data transform — no OS, no VFS — so it belongs in every capability
//! build, same footing as `fromjson`/`tojson`.
//!
//! # Examples
//!
//! ```kaish
//! user=$(fromjson '{"name":"amy","role":"maintainer"}')
//! vs=$(values $user)                    # ["amy","maintainer"]
//! for v in $(values $user); do echo $v; done
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

use super::keys::describe_kind;

/// values tool: the value list of a record, insertion order.
pub struct Values;

/// clap-derived argv layer for values.
#[derive(Parser, Debug)]
#[command(name = "values", about = "The value list of a record, in insertion order")]
struct ValuesArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// The record. Hidden sink — the real value is read off
    /// `args.positional` per the Value-typed positional rule.
    #[arg(hide = true)]
    record: Vec<String>,
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
            "The value list of a record, in insertion order (in .data)",
            [
                ("Capture the value list", "vs=$(values $user)"),
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
        let parsed = match ValuesArgs::try_parse_from(
            std::iter::once("values".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("values: {e}")),
        };
        parsed.global.apply(ctx);

        match args.positional.first() {
            Some(Value::Json(serde_json::Value::Object(map))) => {
                let values: Vec<serde_json::Value> = map.values().cloned().collect();
                ExecResult::success_data(Value::Json(serde_json::Value::Array(values)))
            }
            Some(other) => ExecResult::failure(
                1,
                format!("values: expected a record, got {}", describe_kind(other)),
            ),
            None => ExecResult::failure(1, "values: no argument (expected a record)"),
        }
    }
}
