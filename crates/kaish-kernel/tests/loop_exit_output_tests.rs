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
    let text = result.text_out();
    for want in ["a", "b", "c"] {
        assert!(text.contains(want), "missing {want:?} in:\n{text}");
    }
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
    let text = result.text_out();
    assert!(text.contains("pre"), "earlier statement's output lost:\n{text}");
    assert!(text.contains('a'), "loop's own output lost:\n{text}");
}

#[tokio::test]
async fn exit_from_a_nested_loop_keeps_both_levels_of_output() {
    let kernel = Kernel::transient().unwrap();
    let result = kernel
        .execute("for o in x; do echo outer=$o; for i in p q; do echo inner=$i; exit 4; done; done")
        .await
        .unwrap();
    assert_eq!(result.code, 4);
    let text = result.text_out();
    assert!(text.contains("outer=x"), "outer loop output lost:\n{text}");
    assert!(text.contains("inner=p"), "inner loop output lost:\n{text}");
}

// ---------------------------------------------------------------------------
// return: same rule, from inside a function.
// ---------------------------------------------------------------------------

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
