//! A rendered plan must re-parse to the same value it rendered from.
//!
//! `render_literal`/`quote_word` (`ast/plan.rs`) rendered a `Value::String`
//! bare whenever its text held none of the shell-special characters. That
//! missed a second reason a word needs quotes: a numeral-looking or
//! boolean-looking string reads back as a DIFFERENT type once unquoted —
//! `"1"` re-lexes as `Int(1)`, `"1.5"` as `Float(1.5)`, `"0.10"` as a
//! non-canonical `NumericLiteral`, and `"true"`/`"false"` as a boolean
//! keyword. `quote_word` now also quotes a string whose own text would
//! re-lex as something other than a string.
//!
//! A leading-zero numeral (`"01"`, `"007"`) already lexes bare as
//! `Token::NumberIdent`, which parses back to `Value::String`
//! (`docs/LANGUAGE.md`, "A leading zero is text") — it round-tripped before
//! this fix and stays unquoted after it; it is the control case here.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::ast::{Arg, Expr, Stmt, Value};
use kaish_kernel::parser::parse;
use kaish_kernel::plan_program;
use rstest::rstest;

/// Parse `source` as a single `echo`-style command and pull the `Value` a
/// positional literal argument carries. Panics on anything else — every
/// case here is built to be exactly this shape.
fn literal_value_of(source: &str) -> Value {
    let program = parse(source).unwrap_or_else(|e| panic!("{source:?} must parse: {e:?}"));
    let Stmt::Command(command) = &program.statements[0] else {
        panic!("{source:?}: expected a command statement, got {:?}", program.statements[0]);
    };
    let Arg::Positional(Expr::Literal(value)) = &command.args[0] else {
        panic!("{source:?}: expected a positional literal argument, got {:?}", command.args);
    };
    value.clone()
}

/// The quoted source parses to a `Value::String`, its plan renders, and the
/// rendered text re-parses to that exact same `Value::String` — same type,
/// same content.
#[rstest]
// The reported case, and the sibling forms the same rule catches.
#[case("1")]
#[case("-1")]
#[case("0")]
#[case("1.5")]
#[case("-0.5")]
#[case("3.14")]
#[case("0.10")]
#[case("1.0")]
#[case("-0")]
#[case("-0.0")]
#[case("true")]
#[case("false")]
// The control: a leading-zero numeral already round-tripped before this fix
// (it lexes bare as `NumberIdent`, which parses back to a string) and must
// keep doing so, unquoted.
#[case("01")]
#[case("007")]
#[case("010")]
// Ordinary text is unaffected — this class of fix must not start quoting
// words that never needed it.
#[case("abc")]
#[case("hello-world")]
fn a_quoted_string_literal_round_trips_through_plan(#[case] value: &str) {
    let source = format!("echo \"{value}\"");
    let original = literal_value_of(&source);
    assert_eq!(
        original,
        Value::String(value.to_string()),
        "sanity: {source:?} must itself parse as a string literal"
    );

    let planned = plan_program(&source).expect("plans");
    let rendered = &planned[0].plan.rendered;

    let round_tripped = literal_value_of(rendered);
    assert_eq!(
        round_tripped, original,
        "{value:?} rendered as {rendered:?}, which re-parses to a different value: {round_tripped:?}"
    );
}

/// A leading-zero numeral is the one case that must NOT gain quotes it
/// didn't already need — pinning the exact rendered text, not just the
/// round-tripped value, so a future change cannot start over-quoting it.
#[rstest]
#[case("01")]
#[case("007")]
#[case("abc")]
fn unaffected_words_render_bare(#[case] value: &str) {
    let source = format!("echo \"{value}\"");
    let planned = plan_program(&source).expect("plans");
    assert_eq!(planned[0].plan.rendered, format!("echo {value}"));
}
