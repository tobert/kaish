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

/// Single quotes inside a `$(…)` are a COMMAND's quotes, so nothing in them
/// expands — including arithmetic.
///
/// `scan()` pre-extracts `$((…))` out of a double-quoted string before logos
/// runs, replacing it with a marker that `resolve_markers` swaps back. Its
/// string arm had no notion of a `$(…)` body, so it reached into one and
/// rewrote arithmetic that was sitting inside single quotes — where a command
/// would have left it alone. `echo "$(echo '$((1+1))')"` printed
/// `${__ARITH:1+1__}`: an internal name, in place of the author's text, with
/// no error. Wrong output that nothing reports is the worst kind, so this is
/// pinned by value and not by "it doesn't crash".
///
/// Single quotes inside a double-quoted string but NOT inside a substitution
/// are ordinary characters, and arithmetic there still expands —
/// `echo "'$((1+1))'"` is `'2'` in bash too. That case is the control.
#[rstest]
#[case(r#"echo "$(echo '$((1+1))')""#, "$((1+1))")]
#[case(r#"echo "$(echo 'a$((2))b')""#, "a$((2))b")]
#[case(r#"echo "$(printf '%s' '$((9))')""#, "$((9))")]
#[case(r#"echo "$(echo '$((1+1))' '$((2+2))')""#, "$((1+1)) $((2+2))")]
// Control: not inside a substitution, so it expands, exactly as bash does.
#[case(r#"echo "'$((1+1))'""#, "'2'")]
// Control: inside a substitution but NOT single-quoted, so it still expands.
#[case(r#"echo "$(echo $((1+1)))""#, "2")]
#[case(r#"echo "$(echo "$((1+1))")""#, "2")]
#[tokio::test]
async fn arithmetic_in_single_quotes_inside_a_substitution_is_literal(
    #[case] script: &str,
    #[case] expected: &str,
) {
    assert_eq!(out_of(script).await, expected, "`{script}`");
}

/// The marker must never reach a user by any route. If one does, the scanner
/// rewrote something it had no business rewriting.
#[tokio::test]
async fn no_internal_marker_ever_reaches_the_output() {
    for script in [
        r#"echo "$(echo '$((1+1))')""#,
        r#"echo "$(echo '$(( ))')""#,
        r#"echo "$(echo 'x$((1))y$((2))z')""#,
    ] {
        let out = out_of(script).await;
        assert!(
            !out.contains("__ARITH") && !out.contains("__KAISH"),
            "`{script}` leaked an internal name: {out}"
        );
    }
}

/// A quoted word inside a substitution can itself open another substitution,
/// so finding the substitution's closing `)` needs the same region stack
/// `lex_string` uses one pass later — not a flat "skip to the next quote of
/// the same kind" loop, which mistakes the inner opener for the outer closer.
///
/// The visible symptom is only ever a mis-placed resumption of arithmetic
/// extraction, because this pass copies rather than parses: bytes are
/// identical either way, and the outer loop copies whatever the helper did
/// not. So the case that shows it needs arithmetic AFTER the paren the flat
/// loop miscounts.
#[rstest]
#[case(r#"echo "$(echo "$(echo ")")" $((1+1)))""#, ") 2")]
#[case(r#"echo "$(echo "$(echo "(")" $((2+2)))""#, "( 4")]
#[case(r#"echo "$(echo ")" $((3+3)))""#, ") 6")]
#[case(r#"echo "$(echo ')' $((4+4)))""#, ") 8")]
#[case(r#"echo "$(echo hi; echo $((2+2)))""#, "hi\n4")]
#[tokio::test]
async fn a_paren_in_a_nested_quoted_word_does_not_close_the_substitution(
    #[case] script: &str,
    #[case] expected: &str,
) {
    assert_eq!(out_of(script).await, expected, "`{script}`");
}
