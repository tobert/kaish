//! tojson — serialize a kaish value to a JSON document.
//!
//! The JSON *egress* half of the value model's text boundary (the ingress half
//! is [`super::fromjson`]). Takes one value and emits its JSON text — the
//! "serialize explicitly first" escape hatch for putting structured data in an
//! OS environment variable (which `export` refuses to do silently):
//! `export CFG_JSON=$(tojson $cfg)`.
//!
//! - **One value positional**, read off `args.positional` per the Value-typed
//!   rule (`to_argv()` stringifies, so the clap sink is validation-only).
//! - **Compact by default**, `--pretty` for files/humans.
//! - **Text out, no `.data`** — the whole point is a JSON *string*; setting
//!   `.data` would make `$(tojson …)` round-trip straight back to a value.
//! - **`Value::Bytes` is a loud error** — bytes are not JSON.
//!
//! # Examples
//!
//! ```kaish
//! export CFG_JSON=$(tojson $cfg)     # the export-error escape hatch
//! tojson --pretty $cfg | tee config.json
//! tojson hello                       # "hello"  (a scalar is valid JSON)
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// tojson tool: serialize a value to a JSON document.
pub struct ToJson;

/// clap-derived argv layer for tojson.
#[derive(Parser, Debug)]
#[command(name = "tojson", about = "Serialize a value to a JSON document")]
struct ToJsonArgs {
    /// Pretty-print with two-space indentation (default is compact).
    #[arg(long)]
    pretty: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// The value to serialize. Hidden sink — the real value is read off
    /// `args.positional` per the Value-typed positional rule.
    #[arg(hide = true)]
    value: Vec<String>,
}

#[async_trait]
impl Tool for ToJson {
    fn name(&self) -> &str {
        "tojson"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ToJsonArgs::command(),
            "tojson",
            "Serialize a value to a JSON document (text out)",
            [
                ("Serialize a value", "tojson $cfg"),
                ("Pretty-print to a file", "tojson --pretty $cfg | tee config.json"),
                ("Escape hatch for export", "export CFG_JSON=$(tojson $cfg)"),
                ("A scalar is valid JSON", "tojson hello"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("tojson: {e}")),
        };
        let parsed = match ToJsonArgs::try_parse_from(
            std::iter::once("tojson".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("tojson: {e}")),
        };
        parsed.global.apply(ctx);

        let value = match args.positional.first() {
            Some(v) => v,
            None => {
                return ExecResult::failure(1, "tojson: no value (pass a value to serialize)")
            }
        };

        // Bytes are not JSON — refuse loudly rather than emit a base64 envelope
        // that would silently masquerade as the data.
        if let Value::Bytes(b) = value {
            return ExecResult::failure(
                1,
                format!("tojson: cannot serialize {} bytes of binary — use base64", b.len()),
            );
        }

        let json = kaish_types::value_to_json(value);
        let text = if parsed.pretty {
            serde_json::to_string_pretty(&json)
        } else {
            serde_json::to_string(&json)
        };
        match text {
            // Text out only — no .data, so $(tojson …) captures the JSON string.
            Ok(s) => ExecResult::success(s),
            Err(e) => ExecResult::failure(1, format!("tojson: {e}")),
        }
    }
}
