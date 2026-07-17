//! tojsonl — serialize a kaish list to compact JSONL text, one document per line.
//!
//! The JSONL *egress* half of the stream-boundary door pair (the ingress half
//! is [`super::fromjsonl`]); GH #80, worked back from scatter/gather (#73)
//! needing a lingua franca to hand its typed rows back out as line-oriented
//! text. Mirrors `tojson`'s conventions (see that module for the sibling
//! egress half) for the list shape specifically:
//!
//! - **Input must be a list.** A record or scalar is one JSON document —
//!   that's `tojson`'s job, or wrap it in a list first (`[$x]`). Loud error
//!   naming the fix, never a silent single-line fallback.
//! - **One Value positional, or piped `.data`** — read off `args.positional`
//!   per the Value-typed positional rule when given explicitly; otherwise
//!   pull the upstream pipeline's structured `.data` so `tojsonl` composes
//!   as a real pipeline stage (`curl … | fromjson | tojsonl`), not just a
//!   `$(…)`-capture-then-serialize step.
//! - **Text out, no `.data`** — the whole point is the JSONL *text* form;
//!   setting `.data` would make `$(tojsonl …)` round-trip straight back to
//!   a value instead of producing text.
//! - **Always compact, no `--pretty`** — pretty-printed JSONL isn't a thing.
//! - **`Value::Bytes` is a loud error** — bytes are not JSON.
//! - **Empty list → empty output, exit 0** — round-trips with `fromjsonl`.
//!
//! # Examples
//!
//! ```kaish
//! xs=$(fromjson '[1,2,3]')
//! tojsonl $xs                                # "1\n2\n3\n"
//! cat results.jsonl | fromjsonl | tojsonl    # round-trip
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

use super::keys::describe_kind;

/// tojsonl tool: serialize a list to compact JSONL text.
pub struct ToJsonl;

/// clap-derived argv layer for tojsonl.
#[derive(Parser, Debug)]
#[command(name = "tojsonl", about = "Serialize a list to JSONL text, one document per line")]
struct ToJsonlArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// The list to serialize. Hidden sink — the real value is read off
    /// `args.positional` per the Value-typed positional rule.
    #[arg(hide = true)]
    value: Vec<String>,
}

#[async_trait]
impl Tool for ToJsonl {
    fn name(&self) -> &str {
        "tojsonl"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ToJsonlArgs::command(),
            "tojsonl",
            "Serialize a list to JSONL text, one compact document per line (text out)",
            [
                ("Serialize a list", "tojsonl $xs"),
                ("Round-trip through fromjsonl", "cat results.jsonl | fromjsonl | tojsonl"),
                ("A record is one document — wrap it", "tojsonl [$record]"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("tojsonl: {e}")),
        };
        let parsed = match ToJsonlArgs::try_parse_from(
            std::iter::once("tojsonl".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("tojsonl: {e}")),
        };
        parsed.global.apply(ctx);

        let value = match args.positional.first() {
            Some(v) => v.clone(),
            None => match ctx.resolve_stdin().await {
                Ok((Some(data), _)) => data,
                Ok((None, _)) => {
                    return ExecResult::failure(
                        1,
                        "tojsonl: no value (pass a list, or pipe .data from an upstream builtin)",
                    )
                }
                Err(e) => return ExecResult::failure(2, format!("tojsonl: {e}")),
            },
        };

        // Bytes are not JSON — refuse loudly rather than emit a base64
        // envelope that would silently masquerade as the data (same rule as
        // tojson).
        if let Value::Bytes(b) = &value {
            return ExecResult::failure(
                1,
                format!("tojsonl: cannot serialize {} bytes of binary — use base64", b.len()),
            );
        }

        let elements = match &value {
            Value::Json(serde_json::Value::Array(items)) => items,
            other => {
                return ExecResult::failure(
                    1,
                    format!(
                        "tojsonl: expected a list, got {} — a record/scalar is one document: \
                         use `tojson`, or wrap it in a list first",
                        describe_kind(other)
                    ),
                )
            }
        };

        let mut out = String::new();
        for element in elements {
            match serde_json::to_string(element) {
                Ok(line) => {
                    out.push_str(&line);
                    out.push('\n');
                }
                // Every element originated from a serde_json::Value, which is
                // always serializable — this is unreachable in practice, but
                // propagate rather than silently drop the line if it ever
                // isn't (e.g. a future NaN/Infinity float representation).
                Err(e) => return ExecResult::failure(1, format!("tojsonl: {e}")),
            }
        }

        // Text out only — no .data, so `$(tojsonl …)` captures the JSONL text.
        ExecResult::success(out)
    }
}

