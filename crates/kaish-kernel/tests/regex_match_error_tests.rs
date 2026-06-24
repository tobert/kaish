//! Kernel-routed tests for `[[ ... =~ ... ]]` regex match behaviour.
//!
//! Three cases matter:
//!  1. Valid regex that matches   → exit 0, no error
//!  2. Valid regex that mismatches → exit 1 ("false"), no error
//!  3. Invalid / uncompilable regex → LOUD error (Err return from kernel.execute)
//!
//! The P1 bug was that case 3 silently returned false (exit 1, no error
//! message), indistinguishable from case 2. After the fix, case 3 must
//! return `Err` from `kernel.execute()` and the error text must name the
//! regex problem.
//!
//! Tests route through `kernel.execute()` so the full dispatch chain runs
//! (lex → parse → validate → eval_test_async → eval_expr).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::Kernel;

/// A valid regex that matches the string: exit code 0, no error.
#[tokio::test]
async fn regex_match_valid_matching_pattern_exits_zero() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ "abc" =~ "a.c" ]]"#).await.expect("should succeed");
    assert_eq!(result.code, 0, "matching regex should exit 0; stderr: {:?}", result.err);
    assert!(result.err.is_empty(), "no error on a valid match: {:?}", result.err);
}

/// A valid regex that does NOT match the string: exit code 1, no error.
#[tokio::test]
async fn regex_match_valid_nonmatching_pattern_exits_one() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ "abc" =~ "xyz" ]]"#).await.expect("should succeed");
    assert_eq!(result.code, 1, "non-matching regex should exit 1; stderr: {:?}", result.err);
    assert!(result.err.is_empty(), "no error on a valid non-match: {:?}", result.err);
}

/// An uncompilable regex (unbalanced open paren) must produce a LOUD error,
/// not a silent false. Before the fix this returned Ok(exit=1, stderr="").
#[tokio::test]
async fn regex_match_uncompilable_pattern_is_loud_error() {
    let kernel = Kernel::transient().unwrap();
    // "(" is an unbalanced paren — regex::Regex::new will reject it.
    let result = kernel.execute(r#"[[ "abc" =~ "(" ]]"#).await;
    assert!(result.is_err(), "uncompilable regex must return Err, not silent false");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(
        msg.to_lowercase().contains("regex") || msg.to_lowercase().contains("pattern") || msg.to_lowercase().contains("paren"),
        "error message must name the regex problem: {msg}"
    );
}

/// The NOT-match operator (!~) with an uncompilable regex must also be loud,
/// not silently true.
#[tokio::test]
async fn regex_notmatch_uncompilable_pattern_is_loud_error() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ "abc" != "(" ]]"#).await;
    // Note: "!=" is string-not-equal, not regex-not-match.
    // We need the actual !~ / NotMatch operator.
    // kaish uses "!~" for regex not-match:
    drop(result);
    let result2 = kernel.execute(r#"[[ "abc" !~ "(" ]]"#).await;
    assert!(result2.is_err(), "uncompilable regex in !~ must return Err, not silent true");
    let msg = format!("{:#}", result2.unwrap_err());
    assert!(
        msg.to_lowercase().contains("regex") || msg.to_lowercase().contains("pattern") || msg.to_lowercase().contains("paren"),
        "error message must name the regex problem: {msg}"
    );
}

/// Another uncompilable pattern: unclosed character class `[`.
#[tokio::test]
async fn regex_match_unclosed_bracket_is_loud_error() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ "abc" =~ "[" ]]"#).await;
    assert!(result.is_err(), "unclosed bracket regex must return Err, not silent false");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(
        msg.to_lowercase().contains("regex") || msg.to_lowercase().contains("pattern") || msg.to_lowercase().contains("bracket") || msg.to_lowercase().contains("class"),
        "error message must name the regex problem: {msg}"
    );
}
