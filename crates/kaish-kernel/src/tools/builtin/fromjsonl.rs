//! fromjsonl — parse strictly line-oriented JSONL text into a typed list.
//!
//! The JSONL *ingress* half of the stream-boundary door pair (the egress half
//! is [`super::tojsonl`]). GH #80, worked back from scatter/gather (#73):
//! external NDJSON (an API stream, a log file, kaish's own `gather` output)
//! has no path into typed `.data` today — `fromjson`/`jq` both hard-reject
//! multi-document text. `fromjsonl` is that door.
//!
//! Mirrors `fromjson`'s conventions (see that module for the sibling
//! ingress half): string positional or piped stdin text, envelope-free
//! conversion, `.data` out. The differences are the whole point:
//!
//! - **Strictly line-oriented** (the name is the contract): each non-blank
//!   line parses as exactly one complete JSON document → one element of the
//!   output list. A pretty-printed, multi-line document is a loud error
//!   pointing at `fromjson` (one document) or `jq -s` (many) — that's not
//!   what this tool is for.
//! - **Blank lines are skipped**, same rule as scatter's text ingress.
//! - **Any unparseable line — including a truncated trailing line — is a
//!   loud error naming the line number.** Never a silent skip.
//! - **`null` elements are allowed** — it's data; scatter guards nulls at its
//!   own boundary.
//! - **Empty input → empty list, exit 0.** A stream of zero documents is a
//!   legitimate stream. This is a deliberate asymmetry with `fromjson`
//!   (where empty input is loud) — it falls out naturally here because an
//!   empty document stream is just "every line was blank" (there are none).
//!
//! # Examples
//!
//! ```kaish
//! curl -s api/events.ndjson | fromjsonl | scatter -j8 -- restart ${ITEM[host]}
//! cat results.jsonl | fromjsonl | jq '.[] | select(.ok | not)'
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// fromjsonl tool: parse JSONL text into a typed list.
pub struct FromJsonl;

/// clap-derived argv layer for fromjsonl.
#[derive(Parser, Debug)]
#[command(name = "fromjsonl", about = "Parse JSONL text (one document per line) into a list")]
struct FromJsonlArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// JSONL text to parse (or pipe it on stdin). Hidden sink — the real
    /// value is read off `args.positional` per the Value-typed positional
    /// rule.
    #[arg(hide = true)]
    input: Vec<String>,
}

#[async_trait]
impl Tool for FromJsonl {
    fn name(&self) -> &str {
        "fromjsonl"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &FromJsonlArgs::command(),
            "fromjsonl",
            "Parse JSONL text (one JSON document per line) into a typed list (in .data)",
            [
                ("Fan a JSONL stream out to workers", "curl -s api/events.ndjson | fromjsonl | scatter"),
                ("Re-read kaish's own gather output", "cat results.jsonl | fromjsonl | jq '.[] | select(.ok | not)'"),
                ("Parse a literal string", "fromjsonl $'{\"a\":1}\\n{\"a\":2}'"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("fromjsonl: {e}")),
        };
        let parsed = match FromJsonlArgs::try_parse_from(
            std::iter::once("fromjsonl".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("fromjsonl: {e}")),
        };
        parsed.global.apply(ctx);

        // Input text: an explicit positional argument wins; otherwise stdin.
        // Mirrors fromjson's resolution exactly (see that module).
        let input = match args.positional.first() {
            Some(Value::String(s)) => s.clone(),
            Some(Value::Bytes(b)) => match std::str::from_utf8(b) {
                Ok(s) => s.to_string(),
                Err(_) => return ExecResult::failure(1, "fromjsonl: input is binary, not text"),
            },
            // An already-structured value: re-serialize its compact JSON text,
            // which then parses as a single-line, ONE-document stream — so
            // `fromjsonl $v` yields `[$v]` (a one-element wrap), not `$v`
            // back. Typed values that want to stay themselves don't need a
            // door at all.
            Some(other) => kaish_types::value_to_json(other).to_string(),
            None => match ctx.read_stdin_to_text().await {
                Ok(Some(s)) => s,
                // No stdin connected at all (as opposed to an empty pipe,
                // which is legitimate zero-document input) is a usage error.
                Ok(None) => {
                    return ExecResult::failure(
                        1,
                        "fromjsonl: no input (pass JSONL text or pipe stdin)",
                    )
                }
                Err(e) => return ExecResult::failure(2, format!("fromjsonl: {e}")),
            },
        };

        let mut elements: Vec<serde_json::Value> = Vec::new();
        let mut offset = 0usize;
        let mut line_no = 0usize;
        for raw_line in input.split_inclusive('\n') {
            line_no += 1;
            let start = offset;
            offset += raw_line.len();

            let line = raw_line.strip_suffix('\n').unwrap_or(raw_line);
            let line = line.strip_suffix('\r').unwrap_or(line);
            if line.trim().is_empty() {
                continue;
            }

            match serde_json::from_str::<serde_json::Value>(line) {
                Ok(v) => elements.push(v),
                Err(e) => {
                    return ExecResult::failure(1, diagnose_line_error(&input[start..], line_no, &e));
                }
            }
        }

        // Envelope-free, same law as fromjson: an envelope-shaped object
        // stays a plain record inside the list, never silently decoded to
        // Value::Bytes.
        let list = kaish_types::json_to_value_no_envelope(serde_json::Value::Array(elements));
        ExecResult::success_data(list)
    }
}

/// Diagnose a per-line JSON parse failure.
///
/// `remaining` is the original input starting at the byte offset of the
/// failing line. If the failure is actually the start of a valid JSON value
/// that only completes further down the input — a pretty-printed multi-line
/// document — say so explicitly and point at `fromjson` (one document) or
/// `jq -s` (many) rather than reporting a bare per-line parse error. Genuine
/// garbage (including a truncated trailing line) falls through to the plain
/// line-numbered message.
fn diagnose_line_error(remaining: &str, line_no: usize, err: &serde_json::Error) -> String {
    let first_line_len = remaining.lines().next().map(str::len).unwrap_or(0);
    let mut stream = serde_json::Deserializer::from_str(remaining).into_iter::<serde_json::Value>();
    let spans_further_lines =
        matches!(stream.next(), Some(Ok(_))) && stream.byte_offset() > first_line_len;

    if spans_further_lines {
        format!(
            "fromjsonl: line {line_no}: looks like a multi-line pretty-printed JSON document — \
             fromjsonl is strictly line-oriented (one JSON document per line); use `fromjson` \
             for a single document, or `jq -s` to slurp multiple documents"
        )
    } else {
        format!("fromjsonl: line {line_no}: invalid JSON: {err}")
    }
}

