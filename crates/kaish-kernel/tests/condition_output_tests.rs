//! A condition command's output belongs to the enclosing statement.
//!
//! `if cmd; then …` used only `cmd`'s exit code and dropped its `ExecResult`
//! on the floor, so everything a condition produced disappeared:
//!
//! ```text
//! if cat /nonexistent; then echo y; fi     # printed NOTHING
//! if echo COND; then echo BODY; fi         # printed only BODY
//! ```
//!
//! bash prints `cat: /nonexistent: No such file or directory`, and `COND`
//! before `BODY`. A shell that swallows the reason a condition failed turns a
//! loud command into a silent false — which is how `test a = a -o b = c`'s
//! clear exit-2 error reached nobody (see `test_compound_tests`), and it was
//! never specific to `test`.
//!
//! The rule already existed for the sibling case. `Expr::CommandSubst` says
//! it in a comment: "A substitution's stderr belongs to the enclosing
//! statement, never to its value." A condition's output is the same case, and
//! `Expr::Command` simply never applied it.
//!
//! The two halves arrive by different routes, because they are different
//! things. stderr rides the statement's stderr stream, which the statement
//! already drains. stdout is the statement's own value, so `eval_condition_async`
//! hands it back to the `if`/`while` arm as data and the arm folds it into the
//! result — no shared slot, and every consumer of that result (a pipe, a
//! `$(…)` capture, a redirect) carries it without learning what a condition is.

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

/// bash prints `COND` then `BODY`; so does kaish now.
///
/// A condition's stdout used to be dropped: `eval_expr_async` returns a
/// `Value`, so the bytes an `Expr::Command` wrote had nowhere to go. The fix
/// hands them back to the caller instead — the `if`/`while` arm folds them
/// into the statement's own result, which is what makes them flow into a
/// pipe, a `$(…)` capture, and a redirect without any of those learning
/// about conditions.
#[tokio::test]
async fn if_condition_stdout_reaches_the_statement() {
    let r = run("if echo COND; then echo BODY; fi").await;
    assert_eq!(r.text_out().trim_end(), "COND\nBODY");
}

#[tokio::test]
async fn an_elif_condition_prints_too() {
    let r = run("if false; then echo y; elif echo ELIF; then echo E; fi").await;
    assert_eq!(r.text_out().trim_end(), "ELIF\nE");
}

/// A `while` condition runs once per iteration, so its output interleaves
/// with the body's rather than arriving in one block up front.
#[tokio::test]
async fn while_condition_stdout_interleaves_with_the_body() {
    let r = run("i=0; while echo C; do i=$((i+1)); echo B; if [[ $i -ge 2 ]]; then break; fi; done").await;
    assert_eq!(r.text_out().trim_end(), "C\nB\nC\nB");
}

/// Both sides of a `&&` chain in condition position print, and a
/// short-circuited side prints nothing because it never ran.
#[tokio::test]
async fn both_sides_of_a_condition_chain_print() {
    let r = run("if echo A && echo B; then echo C; fi").await;
    assert_eq!(r.text_out().trim_end(), "A\nB\nC");

    let r = run("if false && echo NEVER; then echo y; else echo n; fi").await;
    assert_eq!(r.text_out().trim_end(), "n");

    let r = run("if echo A || echo NEVER; then echo C; fi").await;
    assert_eq!(r.text_out().trim_end(), "A\nC");
}

/// The statement's stdout is its value, so a condition's share of it has to
/// reach every consumer of that value — not just the terminal.
#[tokio::test]
async fn condition_stdout_flows_into_a_pipe_and_a_capture() {
    let r = run("if echo COND; then echo BODY; fi | cat").await;
    assert_eq!(r.text_out().trim_end(), "COND\nBODY", "into a pipe");

    let r = run("x=$(if echo COND; then echo BODY; fi); echo \"[$x]\"").await;
    assert_eq!(r.text_out().trim_end(), "[COND\nBODY]", "into a capture");
}

/// `$(…)` is the one command in condition position whose stdout is its own
/// value, so folding it in as well would print it twice.
#[tokio::test]
async fn a_substitution_in_a_condition_is_not_printed() {
    let r = run("if [[ -n \"$(echo SUB)\" ]]; then echo Y; fi").await;
    assert_eq!(r.text_out().trim_end(), "Y");
}

/// The condition's EXIT CODE is consumed by the `if`; only its output is
/// carried. A false condition with no `else` still succeeds and says nothing.
#[tokio::test]
async fn a_false_condition_does_not_set_the_statements_code() {
    let r = run("if false; then echo y; fi").await;
    assert_eq!(r.code, 0, "an `if` with no branch taken succeeds");
    assert_eq!(r.text_out().trim_end(), "");

    let r = run("if cat /nonexistent; then echo y; fi").await;
    assert_eq!(r.code, 0, "the condition's failure is the `if`'s answer, not its status");
}

/// A condition that succeeds and says nothing stays silent — the fix must not
/// invent output.
#[tokio::test]
async fn a_quiet_condition_prints_nothing() {
    let r = run("if true; then echo BODY; fi").await;
    assert_eq!(r.text_out().trim_end(), "BODY");
}

/// A condition's output obeys the output limit, like every other output.
///
/// `apply_spill_contract` is documented as "the ONE seam every execution
/// surface that produces a raw `ExecResult` must funnel through". Carrying a
/// condition's stdout made this arm one of those surfaces, and it reached
/// `execute_command` directly — under the pipeline layer that applies the
/// contract. With a 2K limit, `if seq 1 100000; then …; fi` handed back 588KB
/// at exit 0 while the same command on its own was capped at 1.7KB and exit 3.
/// An agent shell whose cap has a hole in it is worse than one with no cap,
/// because the cap is what the caller sized their context against.
#[tokio::test]
async fn a_conditions_output_obeys_the_output_limit() {
    let capped = run("kaish-output-limit set 2K; if seq 1 100000; then echo T; fi").await;
    let plain = run("kaish-output-limit set 2K; seq 1 100000").await;

    let capped_len = capped.text_out().len();
    assert!(
        capped_len < 20_000,
        "a condition's output must be capped like any other; got {capped_len} bytes"
    );
    // The cap is the same cap, not merely some smaller number.
    assert!(
        capped_len <= plain.text_out().len() * 4,
        "condition {capped_len} vs plain {} — same limit, same order of magnitude",
        plain.text_out().len()
    );
    assert!(
        capped.text_out().contains('T'),
        "the branch still ran and its output still arrives"
    );
}

/// A condition succeeds even when its own output was capped. The spill
/// contract remaps a spilled result's code to 3, and reading that as the
/// condition's answer would send `if seq 1 100000` down the `else` branch —
/// the command worked, only its output was too big to keep.
#[tokio::test]
async fn a_capped_condition_is_still_true() {
    let r = run("kaish-output-limit set 2K; if seq 1 100000; then echo THEN; else echo ELSE; fi")
        .await;
    assert!(
        r.text_out().contains("THEN"),
        "a capped condition still succeeded, got: {}",
        r.text_out().chars().rev().take(60).collect::<String>()
    );
}

/// stderr arrives in the order it was produced: the condition ran before the
/// branch, so it reports first. The `if` arm appended the branch's own `err`
/// and only then drained the stream the condition had written to, which put
/// them backwards; `while` already drained first.
#[tokio::test]
async fn condition_stderr_precedes_the_branchs() {
    let r = run("if cat /nope-cond; then echo y; else cat /nope-body; fi").await;
    let cond = r.err.find("nope-cond").expect("condition stderr");
    let body = r.err.find("nope-body").expect("body stderr");
    assert!(
        cond < body,
        "the condition ran first and must report first, got: {}",
        r.err
    );
}
