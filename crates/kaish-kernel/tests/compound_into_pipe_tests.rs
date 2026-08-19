//! A compound statement can feed a pipe: `for … done | cmd`.
//!
//! kaish rejected every compound form in pipeline position with one error,
//! because `for`/`while`/`if`/`case` sit ahead of `pipeline_parser()` in
//! `base_statement` and chumsky's choice is PEG-ordered — the compound parser
//! consumes through `done`, then the `&&`/`||` fold meets `|`:
//!
//! ```text
//! for f in a b; do echo $f; done | wc -l   →  found '|' expected '&&'
//! ```
//!
//! Expectations here are bash's, taken by running each row against bash.
//!
//! Ordering note: this depends on #367 and #368. Built on the pipeline path as
//! it stood before those, a compound stage would have passed a last-stage test
//! and emitted nothing at exit 0 — the feature would have shipped broken in
//! exactly the shape people use.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_trash(false)).expect("failed to create kernel")
}

/// Every compound form as the *first* stage. One grammar site serves all four,
/// so a fix that reaches only `for` would leave three rows red.
#[rstest]
#[case::for_into_wc("for f in a b; do echo $f; done | wc -l", "2\n")]
#[case::for_into_grep("for f in a b c; do echo $f; done | grep b", "b\n")]
#[case::while_no_iterations("while [[ -n \"\" ]]; do echo x; done | wc -l", "0\n")]
#[case::if_then("if true; then echo x; fi | cat", "x\n")]
#[case::if_else("if false; then echo x; else echo y; fi | cat", "y\n")]
#[case::case_stmt("case a in a) echo hit ;; esac | cat", "hit\n")]
// Two pipes deep: a compound must survive a middle stage as well as a first.
#[case::for_through_two_pipes("for f in a b; do echo $f; done | cat | cat", "a\nb\n")]
#[tokio::test]
async fn a_compound_statement_can_feed_a_pipe(#[case] source: &str, #[case] expected: &str) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(
        result.text_out(),
        expected,
        "`{source}` (exit {}, stderr {:?})",
        result.code,
        result.err
    );
}

/// A compound in a *later* stage, which bash also allows. Modeling stages
/// uniformly (rather than a special first-position head) is what makes these
/// fall out of the same change instead of being a second feature later.
#[rstest]
#[case::while_consumes_a_pipe(
    "printf \"a\\nb\\n\" | while read l; do echo \"got $l\"; done",
    "got a\ngot b\n"
)]
#[case::if_in_last_stage("echo x | if true; then cat; fi", "x\n")]
#[case::case_in_last_stage("echo hit | case a in a) cat ;; esac", "hit\n")]
// Compound on both ends of the same pipe.
#[case::compound_both_ends(
    "for f in a b; do echo $f; done | while read l; do echo \"L:$l\"; done",
    "L:a\nL:b\n"
)]
#[tokio::test]
async fn a_compound_can_be_a_later_stage(#[case] source: &str, #[case] expected: &str) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(
        result.text_out(),
        expected,
        "`{source}` (exit {}, stderr {:?})",
        result.code,
        result.err
    );
}

/// A compound stage buffers: its whole output is collected before the next
/// stage sees a byte. bash streams, so `… | head -1` over a long loop exits
/// early there and runs to completion here. Deliberate for now — streaming
/// means plumbing a stage's writer into nested statement execution, which is
/// the shared-slot machinery GH #369 is about. This row pins the *result*, not
/// the timing, so it stays true if streaming lands later.
#[tokio::test]
async fn a_buffered_compound_stage_still_produces_the_right_answer() {
    let kernel = kernel();
    let result = kernel
        .execute("for f in a b c; do echo $f; done | head -n 1")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "a\n");
}

/// A compound stage must not disturb the statement before it. This is the row
/// that fails if the grammar change makes the compound swallow too much.
#[tokio::test]
async fn a_compound_stage_leaves_the_previous_statement_alone() {
    let kernel = kernel();
    let result = kernel
        .execute("echo pre | cat; for f in a; do echo $f; done | cat")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "pre\na\n");
}

/// `case` patterns use `|` as alternation, at a grammar site the pipeline
/// position cannot reach. Widening the pipeline must not make `a|b)` ambiguous.
#[rstest]
#[case::case_alternation("case b in a|b) echo hit ;; esac", "hit\n")]
#[case::case_alternation_into_pipe("case b in a|b) echo hit ;; esac | cat", "hit\n")]
#[tokio::test]
async fn case_alternation_still_parses(#[case] source: &str, #[case] expected: &str) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(result.text_out(), expected, "`{source}` regressed");
}

/// The controls: a plain pipeline and a bare compound both already worked and
/// must keep working. If either breaks, the grammar change went too wide.
#[rstest]
#[case::plain_pipeline("echo plain | cat", "plain\n")]
#[case::bare_compound("for f in a b; do echo $f; done", "a\nb\n")]
#[case::compound_with_and("for f in a; do echo $f; done && echo after", "a\nafter\n")]
#[tokio::test]
async fn the_forms_that_already_worked_keep_working(
    #[case] source: &str,
    #[case] expected: &str,
) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(result.text_out(), expected, "`{source}` regressed");
}
