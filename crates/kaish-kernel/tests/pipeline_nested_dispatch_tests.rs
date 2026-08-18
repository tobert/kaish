//! A nested dispatch must not consume the enclosing command's pipe writer.
//!
//! The kernel's `exec_ctx` is one shared slot. A pipeline stage moves its
//! `pipe_stdout` into it for the duration of the dispatch, and anything that
//! dispatches *while that command is still running* — a `$(…)` in its own
//! argument list, a user function's body — snapshots that same slot. The
//! nested dispatch then carried the writer away and dropped it with its own
//! context, so the stage produced correct bytes with nowhere to send them:
//!
//! ```text
//! echo $(echo sub) | cat      →  (nothing), exit 0      bash: sub
//! f() { echo out; }; f | cat  →  (nothing), exit 0      bash: out
//! ```
//!
//! Silent: zero bytes, exit 0, no diagnostic. It shipped in 0.14.1, so these
//! rows are a regression net for behavior that was never right, not a guard on
//! a recent change.
//!
//! The stdin direction is deliberately *not* isolated — bash lets a
//! substitution consume the stage's stdin (`echo hi | echo $(cat)` prints
//! `hi`), and kaish matches. `substitution_still_consumes_the_stage_stdin`
//! pins that, so a future fix here cannot "tidy" both endpoints at once.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_trash(false)).expect("failed to create kernel")
}

/// Every shape where a non-last stage runs a nested dispatch. Each row is a
/// command whose stdout must survive the pipe; the expectation is what bash
/// prints for the same text.
#[rstest]
// A substitution in the stage's own argument list — the reported shape.
#[case::substitution_bare("echo $(echo sub) | cat", "sub\n")]
#[case::substitution_quoted("echo \"$(echo sub)\" | cat", "sub\n")]
// Glued to literal text, so the stage's argv is assembled from both halves.
// Quoted because kaish does no token pasting — bare `echo a$(echo b)` is a
// parse error by design, not a case this fix is allowed to change.
#[case::substitution_glued("echo \"a$(echo b)\" | cat", "ab\n")]
// Literal text on BOTH sides of the substitution. The whole stage's output
// was lost, not merely the substituted part — `x:[` vanished along with `C`.
// This row is what separates the real fix from a plausible partial one that
// delivers the literals and an empty substitution; that shape would pass
// every other case here. Reported by the kaijutsu session.
#[case::literal_text_either_side("echo \"x:[$(echo C)]\" | cat", "x:[C]\n")]
#[case::literal_text_either_side_dashes("echo \"pre-$(echo M)-post\" | cat", "pre-M-post\n")]
// A different builtin, to prove this is not `echo`-specific.
#[case::substitution_feeding_seq("seq 1 $(echo 3) | cat", "1\n2\n3\n")]
// Two pipes deep: the loss must not reappear at a middle stage.
#[case::substitution_through_two_pipes("echo $(echo sub) | cat | cat", "sub\n")]
// A user function as a non-last stage — the body is the nested dispatch.
#[case::function_body("f() { echo out; }; f | cat", "out\n")]
#[case::function_body_multi_statement("f() { echo a; echo b; }; f | cat", "a\nb\n")]
#[tokio::test]
async fn a_non_last_stage_stdout_survives_a_nested_dispatch(
    #[case] source: &str,
    #[case] expected: &str,
) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(
        result.text_out(),
        expected,
        "`{source}` lost its stdout in the pipe (exit {}, stderr {:?})",
        result.code,
        result.err
    );
}

/// The failure was silent, not loud: it exited 0 while dropping the bytes.
/// Pinning the code separately keeps a future fix from "passing" by turning
/// the loss into an error instead of delivering the output.
#[tokio::test]
async fn losing_stage_output_was_silent_so_pin_the_success_code() {
    let kernel = kernel();
    let result = kernel
        .execute("echo $(echo sub) | cat")
        .await
        .expect("execution failed");

    assert_eq!(result.code, 0, "stderr: {:?}", result.err);
    assert!(
        !result.text_out().is_empty(),
        "exit 0 with empty stdout is the exact shape of the bug"
    );
}

/// The stage's *stdin* stays reachable from a substitution, matching bash.
/// This is the boundary of the fix: isolate the write end, never the read end.
#[tokio::test]
async fn substitution_still_consumes_the_stage_stdin() {
    let kernel = kernel();
    let result = kernel
        .execute("echo hi | echo $(cat)")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "hi\n");
}

/// A *pipeline* inside the substitution already worked, and the boundary is
/// sharp enough to get backwards: `echo "$(echo a | cat)" | cat` prints `a`
/// today while `echo "$(echo sub)" | cat` prints nothing. The nested pipeline
/// runs on its own stage contexts (`snapshot_exec_ctx` sets `pipe_stdout:
/// None`), so it never reaches the shared slot; a nested *single command*
/// dispatches straight onto it. Both must work after the fix.
///
/// Boundary reported by the kaijutsu session, which hit this from the
/// embedded kernel and checked which form its own rc scripts use.
#[rstest]
#[case::pipeline_inside_a_substitution("echo \"$(echo a | cat)\" | cat", "a\n")]
// An assignment is safe for a *different* reason worth keeping straight: it
// is not a pipeline stage at all, so there is no writer in the slot to steal.
// Both rows passed before the fix; they pass for unrelated reasons.
#[case::assignment_is_not_a_stage("x=\"$(echo a | cat)\"; echo $x | cat", "a\n")]
#[case::assignment_of_a_bare_substitution("x=\"$(echo a)\"; echo $x | cat", "a\n")]
#[tokio::test]
async fn a_pipeline_inside_a_substitution_keeps_working(
    #[case] source: &str,
    #[case] expected: &str,
) {
    let kernel = kernel();
    let result = kernel.execute(source).await.expect("execution failed");

    assert_eq!(result.text_out(), expected, "`{source}` regressed");
}

/// A substitution in the *last* stage never had the defect. Keeping it here
/// means a fix that isolates too aggressively — clearing the writer the runner
/// still needs — fails visibly instead of passing the rows above.
#[tokio::test]
async fn a_last_stage_substitution_is_unaffected() {
    let kernel = kernel();
    let result = kernel
        .execute("echo x | echo $(echo sub)")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "sub\n");
}

/// A plain stage with no nested dispatch — the control. If this ever fails,
/// the fix broke ordinary piping rather than the case under test.
#[tokio::test]
async fn a_plain_stage_still_pipes() {
    let kernel = kernel();
    let result = kernel
        .execute("echo plain | cat")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "plain\n");
}
