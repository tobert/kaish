//! The parser graph is built once per thread and reused, so anything it
//! carried between calls would be a new class of bug (GH #255).
//!
//! chumsky combinators are stateless by design, but "by design" is what these
//! tests exist to check: a cached parser that accumulated state would give a
//! different answer the second time, and every other test in the suite parses
//! once.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::parser::parse;

/// The same source parses to the same AST however many times a thread asks.
#[test]
fn repeated_parses_agree_with_the_first() {
    let source = "for f in a b; do echo ${f} | grep -q x && echo $(date +%s); done";
    let first = parse(source).expect("parses");
    for round in 1..50 {
        let again = parse(source).expect("parses");
        assert_eq!(again, first, "parse {round} differed from the first");
    }
}

/// A failed parse must not leave anything behind that changes the next one.
/// Error recovery is where a stateful parser would leak, so alternate a
/// broken source with a good one and check both stay stable.
#[test]
fn a_failed_parse_does_not_disturb_the_next_one() {
    let good = "echo hello world";
    let expected = parse(good).expect("parses");

    for _ in 0..20 {
        let errors = parse("for f in").expect_err("incomplete for");
        assert!(!errors.is_empty(), "a failure must carry its errors");
        let errors2 = parse("python3 <<'PY'\nunterminated").expect_err("unterminated heredoc");
        assert!(!errors2.is_empty());
        assert_eq!(parse(good).expect("parses"), expected);
    }
}

/// Error spans stay byte offsets into the source. The cached parser reads an
/// owned token stream, whose own spans are *token indices* — the mapping that
/// keeps byte offsets is load-bearing, and this is what notices if it goes.
///
/// Asserted as an exact span rather than a range: a range wide enough to
/// exclude token indices is also wide enough to admit an offset that is
/// simply wrong, and the offsets feed a consumer that slices source text.
#[test]
fn error_spans_are_byte_offsets_into_the_source() {
    let source = "echo ok\necho ok\nfor f in";
    assert_eq!(source.len(), 24, "fixture moved; the spans below are byte counts");
    let errors = parse(source).expect_err("incomplete for");
    let span = errors[0].span;
    // End of input, at byte 24 — not token index 8.
    assert_eq!(
        (span.start, span.end),
        (24, 24),
        "span {span:?} is not the byte offset of end-of-input"
    );
}

/// An input past `Stream`'s 512-token batch boundary parses the same as a
/// short one. The old slice input had every token available at once; the
/// cached parser pulls them in batches, so a combinator that backtracked
/// across a batch edge would fail only on long input — and every other
/// parser test in the suite is far shorter than one batch.
#[test]
fn an_input_past_the_batch_boundary_parses() {
    // ~4 tokens per statement, so 400 statements is well past 512 tokens.
    let source = (0..400)
        .map(|i| format!("echo item{i}"))
        .collect::<Vec<_>>()
        .join("\n");
    let program = parse(&source).expect("parses");
    let statements = program.statements.len();
    assert!(statements >= 400, "expected 400 statements, got {statements}");

    // And one long single statement, so the backtracking is inside one
    // parser rather than across statement boundaries.
    let args = (0..400).map(|i| format!("a{i}")).collect::<Vec<_>>().join(" ");
    let one = format!("echo {args}");
    parse(&one).expect("a 400-argument command parses");
}

/// A parse error past the batch boundary still reports a byte offset from
/// the far end of the input, not a truncated or cursor-derived one.
#[test]
fn an_error_past_the_batch_boundary_keeps_its_byte_offset() {
    let prefix = (0..400)
        .map(|i| format!("echo item{i}"))
        .collect::<Vec<_>>()
        .join("\n");
    let source = format!("{prefix}\nfor f in");
    let errors = parse(&source).expect_err("incomplete for at the end");
    assert_eq!(
        (errors[0].span.start, errors[0].span.end),
        (source.len(), source.len()),
        "a late error must carry a late byte offset, not a token index"
    );
}

/// Each thread builds its own graph. Parsing from several at once must agree
/// with parsing from one.
#[test]
fn parses_from_many_threads_agree() {
    let source = "echo hello | grep -q h && echo found";
    let expected = parse(source).expect("parses");
    let handles: Vec<_> = (0..8)
        .map(|_| {
            std::thread::spawn(move || parse("echo hello | grep -q h && echo found").expect("parses"))
        })
        .collect();
    for handle in handles {
        assert_eq!(handle.join().expect("thread"), expected);
    }
}
