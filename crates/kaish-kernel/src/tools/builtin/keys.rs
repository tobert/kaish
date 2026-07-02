//! keys — the key list of a record, in insertion order.
//!
//! Part of the native-collections OPS surface (see `docs/arrays-and-hashes.md`,
//! "OPS — keys/values are builtins; length is the param-expansion `${#…}`").
//! `keys $record` accepts a record **directly** (never nested through another
//! builtin) so it never falls into the `len $(keys $r)`-style nesting trap
//! that motivated this design.
//!
//! Pure data transform — no OS, no VFS — so it belongs in every capability
//! build, same footing as `fromjson`/`tojson`.
//!
//! # Examples
//!
//! ```kaish
//! user=$(fromjson '{"name":"amy","role":"maintainer"}')
//! ks=$(keys $user)                     # ["name","role"]
//! for k in $user; do echo "$k=${user[$k]}"; done   # for-head iterates keys directly
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// keys tool: the key list of a record, in insertion order.
pub struct Keys;

/// clap-derived argv layer for keys.
#[derive(Parser, Debug)]
#[command(name = "keys", about = "The key list of a record, in insertion order")]
struct KeysArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// The record. Hidden sink — the real value is read off
    /// `args.positional` per the Value-typed positional rule.
    #[arg(hide = true)]
    record: Vec<String>,
}

/// A human-readable type name for the loud "expected a record" error.
/// Mirrors the `type_name` idiom already duplicated per-file in this crate
/// (`interpreter/scope.rs`, `interpreter/eval.rs`, `tools/builtin/vars.rs`) —
/// each site names the distinction it cares about, and this one cares about
/// list-vs-record-vs-scalar.
pub(crate) fn describe_kind(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "a bool",
        Value::Int(_) | Value::Float(_) => "a number",
        Value::String(_) => "a string",
        Value::Json(serde_json::Value::Array(_)) => "a list",
        Value::Json(serde_json::Value::Object(_)) => "a record",
        Value::Json(_) => "a scalar",
        Value::Bytes(_) => "binary data",
    }
}

#[async_trait]
impl Tool for Keys {
    fn name(&self) -> &str {
        "keys"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &KeysArgs::command(),
            "keys",
            "The key list of a record, in insertion order (in .data)",
            [
                ("Capture the key list", "ks=$(keys $user)"),
                (
                    "Pairwise-aligned with values",
                    "for k in $(keys $user); do echo $k; done",
                ),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match KeysArgs::try_parse_from(
            std::iter::once("keys".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("keys: {e}")),
        };
        parsed.global.apply(ctx);

        match args.positional.first() {
            Some(Value::Json(serde_json::Value::Object(map))) => {
                let keys: Vec<serde_json::Value> = map
                    .keys()
                    .map(|k| serde_json::Value::String(k.clone()))
                    .collect();
                ExecResult::success_data(Value::Json(serde_json::Value::Array(keys)))
            }
            Some(other) => ExecResult::failure(
                1,
                format!("keys: expected a record, got {}", describe_kind(other)),
            ),
            None => ExecResult::failure(1, "keys: no argument (expected a record)"),
        }
    }
}
