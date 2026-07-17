//! fromjson — parse one JSON document into a structured kaish value.
//!
//! The JSON *ingress* half of the value model's text boundary (the egress half
//! is [`super::tojson`]). Reads a single JSON document from a positional string
//! argument or stdin, and lands the result in `.data` so it can be captured or
//! iterated: `cfg=$(curl -s api/config | fromjson)`.
//!
//! Non-negotiables (see `docs/arrays-and-hashes.md`):
//! - **Envelope-free.** External JSON is converted with
//!   [`kaish_types::json_to_value_no_envelope`] — a base64 byte-envelope-shaped
//!   object stays a plain record, never silently becomes `Value::Bytes`.
//! - **One document, one value.** The whole input is exactly one JSON document;
//!   empty input or trailing garbage is a loud error with line:column, never a
//!   silent `null`. (Contrast `$(… | jq .)`, which array-wraps a multi-value
//!   stream.)
//! - **Same unwrap law as native access.** JSON scalars unwrap to native
//!   `Value` variants; objects/arrays stay `Value::Json`.
//!
//! # Examples
//!
//! ```kaish
//! cfg=$(curl -s api/config | fromjson)   # from a pipe
//! v=$(fromjson "$text")                  # from a string argument
//! echo ${cfg[port]}                      # (native access, once it lands)
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// fromjson tool: parse one JSON document into a structured value.
pub struct FromJson;

/// clap-derived argv layer for fromjson.
#[derive(Parser, Debug)]
#[command(name = "fromjson", about = "Parse one JSON document into a value")]
struct FromJsonArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// JSON text to parse (or pipe it on stdin). Hidden sink — the real value
    /// is read off `args.positional` per the Value-typed positional rule.
    #[arg(hide = true)]
    input: Vec<String>,
}

#[async_trait]
impl Tool for FromJson {
    fn name(&self) -> &str {
        "fromjson"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &FromJsonArgs::command(),
            "fromjson",
            "Parse one JSON document into a structured value (in .data)",
            [
                ("Parse a string", "fromjson '{\"name\": \"amy\"}'"),
                ("Parse a pipe", "curl -s api/config | fromjson"),
                ("Capture for access", "cfg=$(fromjson \"$text\")"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("fromjson: {e}")),
        };
        let parsed = match FromJsonArgs::try_parse_from(
            std::iter::once("fromjson".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("fromjson: {e}")),
        };
        parsed.global.apply(ctx);

        // Input text: an explicit positional argument wins; otherwise stdin.
        let input = match args.positional.first() {
            Some(Value::String(s)) => s.clone(),
            Some(Value::Bytes(b)) => match std::str::from_utf8(b) {
                Ok(s) => s.to_string(),
                Err(_) => {
                    return ExecResult::failure(1, "fromjson: input is binary, not text")
                }
            },
            // An already-structured value: re-serialize its JSON text so
            // `fromjson` is idempotent on values that are already typed.
            Some(other) => kaish_types::value_to_json(other).to_string(),
            None => match ctx.read_stdin_to_text().await {
                Ok(Some(s)) => s,
                Ok(None) => {
                    return ExecResult::failure(
                        1,
                        "fromjson: no input (pass a JSON string or pipe stdin)",
                    )
                }
                Err(e) => return ExecResult::failure(2, format!("fromjson: {e}")),
            },
        };

        if input.trim().is_empty() {
            return ExecResult::failure(1, "fromjson: empty input (expected one JSON document)");
        }

        // One document, one value: serde_json::from_str rejects trailing garbage
        // ("trailing characters"), and its Display already carries "at line L
        // column C" — a loud, positioned error, never a silent null.
        let json: serde_json::Value = match serde_json::from_str(&input) {
            Ok(j) => j,
            Err(e) => return ExecResult::failure(1, format!("fromjson: invalid JSON: {e}")),
        };

        // Envelope-free: an envelope-shaped object stays a record, never Bytes.
        let value = kaish_types::json_to_value_no_envelope(json);
        ExecResult::success_data(value)
    }
}
