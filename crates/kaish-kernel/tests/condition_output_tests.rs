//! A condition command's output belongs to the enclosing statement.
//!
//! `if cmd; then …` used only `cmd`'s exit code and dropped its `ExecResult`
//! on the floor, so every diagnostic a condition produced disappeared:
//!
//! ```text
//! if cat /nonexistent; then echo y; fi     # printed NOTHING
//! ```
//!
//! bash prints `cat: /nonexistent: No such file or directory`. A shell that
//! swallows the reason a condition failed turns a loud command into a silent
//! false — which is how `test a = a -o b = c`'s clear exit-2 error reached
//! nobody (see `test_compound_tests`), and it was never specific to `test`.
//!
//! The rule already existed for the sibling case. `Expr::CommandSubst` says
//! it in a comment: "A substitution's stderr belongs to the enclosing
//! statement, never to its value." A condition's output is the same case, and
//! `Expr::Command` simply never applied it.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::ExecResult;

async fn run(script: &str) -> ExecResult {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    k.execute(script).await.expect("kernel execute")
}

#[tokio::test]
async fn if_condition_stderr_reaches_the_statement() {
    let r = run("if cat /nonexistent; then echo y; else echo n; fi").await;
    assert!(
        r.err.contains("/nonexistent"),
        "the condition's diagnostic must not be swallowed, got err={:?}",
        r.err
    );
    assert_eq!(r.text_out().trim_end(), "n", "the branch itself is unchanged");
}

#[tokio::test]
async fn while_condition_stderr_reaches_the_statement() {
    let r = run("while cat /nonexistent; do break; done").await;
    assert!(r.err.contains("/nonexistent"), "err={:?}", r.err);
}

/// The `else` branch's own stderr still arrives too — the condition's must be
/// added to it, not replace it.
#[tokio::test]
async fn condition_and_body_stderr_both_arrive() {
    let r = run("if cat /nope-cond; then echo y; else cat /nope-body; fi").await;
    assert!(r.err.contains("/nope-cond"), "condition stderr missing: {:?}", r.err);
    assert!(r.err.contains("/nope-body"), "body stderr missing: {:?}", r.err);
}

/// A condition that succeeds and says nothing stays silent — the fix must not
/// invent output.
#[tokio::test]
async fn a_quiet_condition_stays_quiet() {
    let r = run("if true; then echo y; fi").await;
    assert!(r.err.is_empty(), "err={:?}", r.err);
    assert_eq!(r.text_out().trim_end(), "y");
}

/// `$(…)` in a condition already routed its stderr correctly; that must keep
/// working rather than doubling.
#[tokio::test]
async fn command_substitution_in_a_condition_reports_once() {
    let r = run("if [[ -n \"$(cat /nope-subst)\" ]]; then echo y; else echo n; fi").await;
    let hits = r.err.matches("/nope-subst").count();
    assert_eq!(hits, 1, "expected exactly one report, got {hits} in {:?}", r.err);
}

/// A condition's **stdout** is still dropped, and this pins that on purpose.
///
/// bash prints `COND` then `BODY`; kaish prints only `BODY`. Fixing it is not
/// the same shape of change as stderr: stderr rides a stream the statement
/// already drains, while a statement's stdout is its own value, so carrying a
/// condition's would mean a new shared slot on `ExecContext` — the pattern GH
/// #369 exists to remove. It is left for that work rather than added to it.
///
/// **If this test starts failing, condition stdout began flowing.** That is
/// the desired end state — update the expectation to `"COND\nBODY"` and
/// delete this note, rather than restoring the old behavior.
#[tokio::test]
async fn condition_stdout_is_still_dropped_pending_gh_369() {
    let r = run("if echo COND; then echo BODY; fi").await;
    assert_eq!(r.text_out().trim_end(), "BODY", "see this test's comment");
}
