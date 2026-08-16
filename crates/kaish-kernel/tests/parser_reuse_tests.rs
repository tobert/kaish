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
#[test]
fn error_spans_are_byte_offsets_into_the_source() {
    let source = "echo ok\necho ok\nfor f in";
    let errors = parse(source).expect_err("incomplete for");
    let span = errors[0].span;
    assert!(
        span.start >= 16 && span.end <= source.len(),
        "span {span:?} is not a byte offset into a {}-byte source",
        source.len()
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
