//! A double-quoted string may hold `$(…)` whose body has its own quoted words.
//!
//! `echo "$(basename "$p")"` was a parse error: "unterminated command
//! substitution: missing `)`". The double-quoted string was a flat logos
//! regex, `r#""([^"\\]|\\.)*""#`, with no notion of `$()` nesting — so for
//! `echo "$(echo "hi")"` the lexer ended the string at the INNER quote and
//! the rest of the line made no sense.
//!
//! The rule was narrow and worth stating exactly: BOTH levels had to be
//! double-quoted. `echo $(echo "hi")` and `echo "$(echo 'hi')"` were always
//! fine, which is why the first report ("any literal double quote between
//! `$(` and `)`") sent people to unquote variables inside `$()` — a real
//! hazard adopted for nothing.
//!
//! The fix follows the sibling already in `lexer.rs`: `${…}` matches only its
//! opener and a callback extends the token to the balanced close (GH #173).
//! A double-quoted string does the same, tracking `$(` depth so a quote inside
//! a substitution belongs to the substitution.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

async fn out_of(script: &str) -> String {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let r = k
        .execute(script)
        .await
        .unwrap_or_else(|e| panic!("`{script}` must parse and run: {e}"));
    r.text_out().trim_end().to_string()
}

/// The forms that were already legal stay legal and unchanged. They are the
/// control: a fix that only makes new programs work is not a fix.
#[rstest]
#[case(r#"echo "$(echo hi)""#, "hi")]
#[case(r#"echo $(echo "hi")"#, "hi")]
#[case(r#"echo "$(echo 'hi')""#, "hi")]
#[case(r#"x=$(echo "hi"); echo $x"#, "hi")]
#[case(r#"echo "plain string""#, "plain string")]
#[case(r#"echo "a\"b""#, "a\"b")]
#[case(r#"echo "$(echo \"esc\")""#, "esc")]
#[tokio::test]
async fn forms_that_already_worked_still_work(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(out_of(script).await, expected, "`{script}`");
}

/// The bug: both levels double-quoted.
#[rstest]
#[case(r#"echo "$(echo "hi")""#, "hi")]
#[case(r#"p=/a/b.txt; echo "$(basename "$p")""#, "b.txt")]
#[case(r#"x="$(echo "hi")"; echo $x"#, "hi")]
#[case(r#"echo "pre $(echo "hi") post""#, "pre hi post")]
#[case(r#"echo "$(echo "a  b")""#, "a  b")]
#[case(r#"echo "$(echo "it's")""#, "it's")]
#[tokio::test]
async fn a_quoted_word_inside_a_substitution_parses(
    #[case] script: &str,
    #[case] expected: &str,
) {
    assert_eq!(out_of(script).await, expected, "`{script}`");
}

/// Nesting is not special-cased at one level: a substitution inside a quoted
/// word inside a substitution has to work, or the counter is a hack.
#[rstest]
#[case(r#"echo "$(echo "$(echo deep)")""#, "deep")]
#[case(r#"echo "$(echo "$(echo "$(echo deeper)")")""#, "deeper")]
#[case(r#"p=/x/y.txt; echo "outer $(echo "in $(basename "$p") out") done""#, "outer in y.txt out done")]
#[tokio::test]
async fn nesting_goes_all_the_way_down(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(out_of(script).await, expected, "`{script}`");
}

/// A single-quoted region inside a substitution is literal, so a `"` in it
/// closes nothing.
#[rstest]
#[case(r#"echo "$(echo '"')""#, "\"")]
#[case(r#"echo "$(echo 'a"b')""#, "a\"b")]
#[tokio::test]
async fn a_quote_inside_single_quotes_closes_nothing(
    #[case] script: &str,
    #[case] expected: &str,
) {
    assert_eq!(out_of(script).await, expected, "`{script}`");
}

/// A string that really is unterminated must still fail, and fail at parse
/// time. Scanning further for a closing quote must not turn a typo into a
/// program that runs.
#[rstest]
#[case(r#"echo "unterminated"#)]
#[case(r#"echo "$(echo hi"#)]
#[case(r#"echo "$(echo "hi")"#)]
#[case(r#"echo "$(echo "hi""#)]
#[tokio::test]
async fn an_unterminated_string_is_still_an_error(#[case] script: &str) {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let err = k
        .execute(script)
        .await
        .err()
        .unwrap_or_else(|| panic!("`{script}` must be refused"))
        .to_string();
    // NOT `contains("parse")` — every kernel parse error carries that word, so
    // the assertion would hold even if the curated diagnostic regressed to a
    // generic "unexpected character".
    assert!(
        err.contains("unterminated"),
        "`{script}` should name what is unterminated, got: {err}"
    );
}
