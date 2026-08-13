//! Spec tests: leaving a loop early never discards what the loop already printed.
//!
//! `break`, `continue`, `return`, and `exit` all leave a loop before it runs
//! out of items. Whichever one is used, the output the loop produced up to
//! that point is part of the result — an early exit says "stop here", not
//! "pretend the iterations that ran never happened".
//!
//! `break` and `continue` already honored this; `return` and `exit` did not,
//! because the loop's accumulated output was dropped on the way out. The
//! `break` cases below are the known-good controls: they passed before the
//! fix, so a regression that breaks all four is distinguishable from one that
//! breaks only the two that were wrong.
//!
//! Notes for the test author:
//! - `Kernel::transient()` exercises the full kernel; no wiring needed.
//! - `exit` sets the script's code, so these assert on `result.code` rather
//!   than `result.ok()`.

use kaish_kernel::Kernel;

// ---------------------------------------------------------------------------
// Controls: break and continue already carried the loop's output out.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn break_keeps_the_output_produced_before_it() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("for f in a b c; do echo $f; break; done")
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    assert_eq!(result.text_out().trim(), "a");
}

#[tokio::test]
async fn continue_keeps_the_output_of_every_iteration() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("for f in a b c; do echo $f; continue; done")
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    assert_eq!(result.text_out().trim(), "a\nb\nc");
}

// `break N`/`continue N` with N > 1 is the case the fold helper was originally
// written for. Widening it to `return`/`exit` must not disturb them, and
// nothing else in the suite would catch it.

#[tokio::test]
async fn break_2_carries_both_loops_output_out_exactly_once() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("for o in x; do echo outer=$o; for i in p q; do echo inner=$i; break 2; done; done")
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    assert_eq!(result.text_out().trim(), "outer=x\ninner=p");
}

#[tokio::test]
async fn continue_2_carries_the_inner_output_out_exactly_once() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("for o in x y; do echo outer=$o; for i in p; do echo inner=$i; continue 2; done; done")
        .await
        .unwrap();
    assert!(result.ok(), "script should succeed: err={}", result.err);
    assert_eq!(
        result.text_out().trim(),
        "outer=x\ninner=p\nouter=y\ninner=p"
    );
}

// ---------------------------------------------------------------------------
// exit: the loop's own output survives the immediate exit.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn exit_from_a_for_loop_keeps_the_output_already_produced() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("for f in a b; do echo $f; exit 3; done")
        .await
        .unwrap();
    assert_eq!(result.code, 3, "exit code should be the one `exit` named");
    assert_eq!(
        result.text_out().trim(),
        "a",
        "the iteration that ran before `exit` printed 'a'; it must not be discarded"
    );
}

#[tokio::test]
async fn exit_from_a_while_loop_keeps_the_output_already_produced() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("while true; do echo hi; exit 3; done")
        .await
        .unwrap();
    assert_eq!(result.code, 3);
    assert_eq!(result.text_out().trim(), "hi");
}

#[tokio::test]
async fn exit_keeps_both_earlier_statements_and_the_loop_output() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("echo pre; for f in a b; do echo $f; exit 3; done")
        .await
        .unwrap();
    assert_eq!(result.code, 3);
    // Exact, not `contains`: a fold that emitted twice would still contain
    // both, and double-emission is the failure this fix could plausibly have.
    assert_eq!(result.text_out().trim(), "pre\na");
}

#[tokio::test]
async fn exit_from_a_nested_loop_emits_each_level_exactly_once() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("for o in x; do echo outer=$o; for i in p q; do echo inner=$i; exit 4; done; done")
        .await
        .unwrap();
    assert_eq!(result.code, 4);
    // The inner loop folds into the signal, then the outer loop folds into the
    // same signal. Exact equality is what proves neither level was duplicated.
    assert_eq!(result.text_out().trim(), "outer=x\ninner=p");
}

// ---------------------------------------------------------------------------
// return: same rule, from inside a function.
// ---------------------------------------------------------------------------

// ---------------------------------------------------------------------------
// A loop is not the only block that accumulates. `if`/`case` branches and the
// left side of a `&&` chain hand a result back the same way, and lost it the
// same way — including with no loop anywhere in sight.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn exit_from_an_if_branch_keeps_the_branch_output() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("if true; then echo x; exit 1; fi")
        .await
        .unwrap();
    assert_eq!(result.code, 1);
    assert_eq!(result.text_out().trim(), "x");
}

#[tokio::test]
async fn exit_from_a_case_branch_keeps_the_branch_output() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("case a in a) echo hit; exit 1;; esac")
        .await
        .unwrap();
    assert_eq!(result.code, 1);
    assert_eq!(result.text_out().trim(), "hit");
}

#[tokio::test]
async fn exit_from_the_right_of_an_and_chain_keeps_the_left_output() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel.execute("echo pre && exit 1").await.unwrap();
    assert_eq!(result.code, 1);
    assert_eq!(result.text_out().trim(), "pre");
}

#[tokio::test]
async fn exit_from_an_if_inside_a_loop_keeps_both() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("for f in a; do echo loop=$f; if true; then echo branch; exit 1; fi; done")
        .await
        .unwrap();
    assert_eq!(result.code, 1);
    assert_eq!(result.text_out().trim(), "loop=a\nbranch");
}

// ---------------------------------------------------------------------------
// return: same rule, from inside a function. The function-body path is a
// different consumption site from the top-level one.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn exit_from_a_loop_in_a_function_keeps_the_output() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("f() { for x in a b; do echo $x; exit 3; done; }; f")
        .await
        .unwrap();
    assert_eq!(result.code, 3);
    assert_eq!(result.text_out().trim(), "a");
}

#[tokio::test]
async fn return_from_a_nested_loop_in_a_function_keeps_both_levels() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("f() { for o in x; do echo outer=$o; for i in p q; do echo inner=$i; return 5; done; done; }; f")
        .await
        .unwrap();
    assert_eq!(result.code, 5);
    assert_eq!(result.text_out().trim(), "outer=x\ninner=p");
}

#[tokio::test]
async fn return_from_a_loop_in_a_function_keeps_the_output() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("f() { for x in a b; do echo $x; return 5; done; }; f")
        .await
        .unwrap();
    assert_eq!(result.code, 5);
    assert_eq!(
        result.text_out().trim(),
        "a",
        "the iteration that ran before `return` printed 'a'; it must not be discarded"
    );
}
