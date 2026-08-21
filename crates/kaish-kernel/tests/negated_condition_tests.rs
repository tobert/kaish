//! `if ! cmd; then …` — negating a condition.
//!
//! It was a parse error: "found '!' expected condition". `[[ ! -f x ]]` worked,
//! because `!` lives in the `[[ ]]` grammar, but the plain-command form had no
//! production at all — so the idiomatic "run this unless" had to be written
//! backwards through an empty `then` branch, or with `[[ ]]` around something
//! that is not a test.
//!
//! Precedence is bash's and it is the part worth pinning: `!` binds to the
//! command that follows it, not to the whole chain. `bash -c 'if ! true &&
//! true'` takes the else branch, because it reads `(! true) && true`. Binding
//! it to the chain would make that the then branch, and nothing in the source
//! would show which reading applied.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

async fn out(script: &str) -> String {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let r = k
        .execute(script)
        .await
        .unwrap_or_else(|e| panic!("`{script}` must parse and run: {e}"));
    r.text_out().trim_end().to_string()
}

/// Every expectation here was taken from `bash -c`, not from reading a manual.
#[rstest]
#[case("if ! true; then echo Y; else echo N; fi", "N")]
#[case("if ! false; then echo Y; else echo N; fi", "Y")]
#[case("if ! ! true; then echo Y; else echo N; fi", "Y")]
// `!` binds to the command, so this is `(! true) && true` — the else branch.
#[case("if ! true && true; then echo Y; else echo N; fi", "N")]
#[case("if ! false && true; then echo Y; else echo N; fi", "Y")]
#[case("if ! true || true; then echo Y; else echo N; fi", "Y")]
#[tokio::test]
async fn negation_matches_bash(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(out(script).await, expected, "`{script}`");
}

/// It works in a `while` head too, which is where "until" is usually spelled
/// in a shell that has no `until`.
#[tokio::test]
async fn negation_works_in_a_while_head() {
    assert_eq!(out("while ! true; do echo never; done; echo done").await, "done");
    assert_eq!(
        out("i=0; while ! [[ $i -ge 2 ]]; do i=$((i+1)); echo \"i=$i\"; done").await,
        "i=1\ni=2"
    );
}

/// `[[ ]]` and a plain command both negate, and a negated condition's output
/// still reaches the statement — the condition-output rule does not stop at a
/// `!`.
#[tokio::test]
async fn negation_composes_with_the_rest() {
    assert_eq!(out("if ! [[ -f /nope ]]; then echo Y; fi").await, "Y");
    assert_eq!(out("if ! echo COND; then echo Y; else echo N; fi").await, "COND\nN");
}

/// A failing command negates to true, and its diagnostic still arrives —
/// this is the shape `! cmd` is usually reached for.
#[tokio::test]
async fn a_failing_command_negates_to_true_and_still_reports() {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let r = k
        .execute("if ! cat /nope-neg; then echo TOOK_IT; fi")
        .await
        .expect("execute");
    assert_eq!(r.text_out().trim_end(), "TOOK_IT");
    assert!(r.err.contains("/nope-neg"), "the reason must still arrive: {:?}", r.err);
}
