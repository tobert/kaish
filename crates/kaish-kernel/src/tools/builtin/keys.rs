//! keys — the keys of a collection, in insertion order.
//!
//! Part of the native-collections OPS surface (see `docs/arrays-and-hashes.md`,
//! "OPS — keys/values are builtins; length is the param-expansion `${#…}`").
//! `keys $collection` accepts a collection **directly** (never nested through
//! another builtin) so it never falls into the `len $(keys $r)`-style nesting
//! trap that motivated this design.
//!
//! jq semantics for the two collection shapes:
//! - a **record** → its keys, insertion order;
//! - a **list** → its indices as integers, `0..N-1`.
//!
//! This makes `$(keys $c)` the uniform, unambiguous iteration idiom over ANY
//! collection — so bare-var `for x in $list` in a for-head is never needed.
//!
//! Pure data transform — no OS, no VFS — so it belongs in every capability
//! build, same footing as `fromjson`/`tojson`.
//!
//! # Examples
//!
//! ```kaish
//! user=$(fromjson '{"name":"amy","role":"maintainer"}')
//! ks=$(keys $user)                     # ["name","role"]
//! xs=$(fromjson '["a","b","c"]')
//! for i in $(keys $xs); do echo $i; done           # 0 1 2 (list indices)
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// keys tool: the keys of a collection (record keys / list indices).
pub struct Keys;

/// clap-derived argv layer for keys.
#[derive(Parser, Debug)]
#[command(name = "keys", about = "The keys of a collection (record keys / list indices)")]
struct KeysArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// The collection. Hidden sink — the real value is read off
    /// `args.positional` per the Value-typed positional rule.
    #[arg(hide = true)]
    collection: Vec<String>,
}

/// A human-readable type name for the loud "expected a record or list" error.
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
        // JSON scalars normally unwrap to native `Value`s at the access boundary,
        // but name them precisely if one reaches here rather than lumping them
        // all under "a scalar".
        Value::Json(serde_json::Value::Null) => "null",
        Value::Json(serde_json::Value::Bool(_)) => "a bool",
        Value::Json(serde_json::Value::Number(_)) => "a number",
        Value::Json(serde_json::Value::String(_)) => "a string",
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
            "The keys of a collection — record keys or list indices (in .data)",
            [
                ("Record keys, insertion order", "ks=$(keys $user)"),
                ("List indices (0..N-1)", "for i in $(keys $xs); do echo $i; done"),
                (
                    "Uniform key iteration",
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
            // A record → its keys, insertion order.
            Some(Value::Json(serde_json::Value::Object(map))) => {
                let keys: Vec<serde_json::Value> = map
                    .keys()
                    .map(|k| serde_json::Value::String(k.clone()))
                    .collect();
                ExecResult::success_data(Value::Json(serde_json::Value::Array(keys)))
            }
            // A list → its indices as integers, 0..N-1 (jq semantics). This makes
            // `for i in $(keys $xs)` the uniform index-iteration idiom.
            Some(Value::Json(serde_json::Value::Array(items))) => {
                let indices: Vec<serde_json::Value> = (0..items.len())
                    .map(|i| serde_json::Value::Number(i.into()))
                    .collect();
                ExecResult::success_data(Value::Json(serde_json::Value::Array(indices)))
            }
            Some(other) => ExecResult::failure(
                1,
                format!(
                    "keys: expected a record or list, got {}",
                    describe_kind(other)
                ),
            ),
            None => ExecResult::failure(1, "keys: no argument (expected a record or list)"),
        }
    }
}
