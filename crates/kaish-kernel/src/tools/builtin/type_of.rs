//! typeof — the shape guard: a value's type as a plain type name.
//!
//! Part of the native-collections shape-guard surface (see
//! `docs/arrays-and-hashes.md`, decision F: "Ship a shape guard (`typeof` /
//! `[[ -list ]]` / `[[ -record ]]`) — antidote to the keys-on-list footgun
//! and the API-shape-variance trap"). `typeof $x` answers "what did this
//! value actually turn out to be" before an agent commits to `keys`/`values`
//! or a `for` loop over it — the trap this guards against is an API call
//! that sometimes returns a list and sometimes returns a single record.
//!
//! Type names are deliberately coarse — `number` covers both `Int` and
//! `Float` (no int/float split), matching jq/JSON's single numeric type.
//! Pair with `[[ -list $x ]]` / `[[ -record $x ]]` for the common two-shape
//! guard idiom without a `case` statement.
//!
//! Pure data transform — no OS, no VFS — so it belongs in every capability
//! build, same footing as `fromjson`/`tojson`/`keys`/`values`.
//!
//! # Examples
//!
//! ```kaish
//! t=$(typeof $x)                 # "list" | "record" | "string" | "number" | "bool" | "null" | "bytes"
//! if [[ -record $data ]]; then
//!   for k in $(keys $data); do echo $k; done
//! elif [[ -list $data ]]; then
//!   for x in $(values $data); do echo $x; done
//! fi
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// typeof tool: a value's type as a plain type name — the shape guard.
pub struct TypeOf;

/// clap-derived argv layer for typeof.
#[derive(Parser, Debug)]
#[command(name = "typeof", about = "The value's type — the shape guard")]
struct TypeOfArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// The value. Hidden sink — the real value is read off
    /// `args.positional` per the Value-typed positional rule.
    #[arg(hide = true)]
    value: Vec<String>,
}

/// The exact type name for the shape guard — `list`/`record`/`string`/
/// `number`/`bool`/`null`/`bytes`. No int/float split (jq/JSON have one
/// numeric type). `Value::Json` scalars are named precisely rather than
/// lumped under a generic "json" bucket — they normally unwrap to native
/// `Value`s at the access boundary, but this stays exhaustive in case one
/// reaches here directly (e.g. via `fromjson` on a bare scalar).
pub(crate) fn type_name(value: &Value) -> &'static str {
    match value {
        Value::Null => "null",
        Value::Bool(_) => "bool",
        Value::Int(_) | Value::Float(_) => "number",
        Value::String(_) => "string",
        Value::Json(serde_json::Value::Array(_)) => "list",
        Value::Json(serde_json::Value::Object(_)) => "record",
        Value::Json(serde_json::Value::Null) => "null",
        Value::Json(serde_json::Value::Bool(_)) => "bool",
        Value::Json(serde_json::Value::Number(_)) => "number",
        Value::Json(serde_json::Value::String(_)) => "string",
        Value::Bytes(_) => "bytes",
    }
}

#[async_trait]
impl Tool for TypeOf {
    fn name(&self) -> &str {
        "typeof"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TypeOfArgs::command(),
            "typeof",
            "The value's type — list/record/string/number/bool/null/bytes (in .data)",
            [
                ("Assign the type name", "t=$(typeof $x)"),
                (
                    "Shape guard idiom",
                    "if [[ -record $data ]]; then echo record; elif [[ -list $data ]]; then echo list; fi",
                ),
                (
                    "Dispatch on shape",
                    "case $(typeof $x) in\n  list) ... ;;\n  record) ... ;;\nesac",
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
            Err(e) => return ExecResult::failure(2, format!("typeof: {e}")),
        };
        let parsed = match TypeOfArgs::try_parse_from(
            std::iter::once("typeof".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("typeof: {e}")),
        };
        parsed.global.apply(ctx);

        match args.positional.first() {
            Some(value) => {
                let name = type_name(value);
                ExecResult::success_with_data(name, Value::String(name.to_string()))
            }
            None => ExecResult::failure(1, "typeof: no argument (expected a value)"),
        }
    }
}
