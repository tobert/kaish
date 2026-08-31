//! Kernel-routed tests for `[[ ... =~ ... ]]` regex match behaviour.
//!
//! Three cases matter:
//!  1. Valid regex that matches   → exit 0, no error
//!  2. Valid regex that mismatches → exit 1 ("false"), no error
//!  3. Invalid / uncompilable regex → LOUD error (exit 2, message on stderr)
//!
//! The P1 bug was that case 3 silently returned false (exit 1, no error
//! message), indistinguishable from case 2. What these tests protect is that
//! distinction, so they assert it directly: case 3 is exit 2, never case 2's
//! exit 1, and its text names the regex problem.
//!
//! Case 3 used to leave `kernel.execute()` as `Err`. It is now a result like
//! any other `[[ ]]` fault — the same exit 2 a bad numeric operand gets, and
//! the same shape `(( ))` and `test` already used. That is a strictly louder
//! report than the old one, whose message reached a user only under a generic
//! "execution failed" wrapper.
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
    let result = kernel.execute(r#"[[ "abc" =~ "(" ]]"#).await.expect("a fault is a result");
    assert_eq!(result.code, 2, "uncompilable regex is a fault, not a false reading");
    assert_ne!(result.code, 1, "it must not collapse into the non-match code");
    let msg = result.err.to_lowercase();
    assert!(
        msg.contains("regex") || msg.contains("pattern") || msg.contains("paren"),
        "error message must name the regex problem: {:?}", result.err
    );
}

/// The NOT-match operator (!~) with an uncompilable regex must also be loud,
/// not silently true.
#[tokio::test]
async fn regex_notmatch_uncompilable_pattern_is_loud_error() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ "abc" !~ "(" ]]"#).await.expect("a fault is a result");
    assert_eq!(result.code, 2, "uncompilable regex in !~ is a fault, not a true reading");
    assert_ne!(result.code, 0, "it must not collapse into the no-match-is-true code");
    let msg = result.err.to_lowercase();
    assert!(
        msg.contains("regex") || msg.contains("pattern") || msg.contains("paren"),
        "error message must name the regex problem: {:?}", result.err
    );
}

/// Another uncompilable pattern: unclosed character class `[`.
#[tokio::test]
async fn regex_match_unclosed_bracket_is_loud_error() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute(r#"[[ "abc" =~ "[" ]]"#).await.expect("a fault is a result");
    assert_eq!(result.code, 2, "unclosed bracket is a fault, not a false reading");
    assert_ne!(result.code, 1, "it must not collapse into the non-match code");
    let msg = result.err.to_lowercase();
    assert!(
        msg.contains("regex") || msg.contains("pattern") || msg.contains("bracket") || msg.contains("class"),
        "error message must name the regex problem: {:?}", result.err
    );
}
