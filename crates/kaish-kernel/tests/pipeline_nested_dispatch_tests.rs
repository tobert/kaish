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
//! The fix carries the writer through `execute_pipeline`'s context for the
//! duration, exactly as `pipe_stdin` was already carried. That asymmetry —
//! stdin carried, stdout left behind — *was* the bug, so the two endpoints now
//! read the same at that site.
//!
//! stdin's own semantics are unchanged and deliberately different: bash lets a
//! substitution consume the stage's stdin (`echo hi | echo $(cat)` prints
//! `hi`), and kaish matches. `substitution_still_consumes_the_stage_stdin`
//! pins that, so nobody "fixes" the read end into isolation later.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;
use tempfile::tempdir;

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
/// runs on its own stage contexts (`child_for_pipeline` sets `pipe_stdout:
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

/// `source` runs its statements on the enclosing dispatch, so it is the same
/// hazard as a function body — and a review caught that the first attempt,
/// which wrapped only the substitution and function paths, left it exposed:
///
/// ```text
/// source foo.kai | cat   →  (nothing), exit 0      bash: hello
/// ```
///
/// Both spellings of the special form are here — `source` and `.` share one
/// implementation, and a fix applied to only one door is exactly the mistake
/// this whole change is about.
///
/// Executing a `.kai` *script* (`./helper.kai | cat`) is a different dispatch
/// path and was never affected; the same review predicted it was broken and
/// running it showed otherwise. It is not covered here because it needs an
/// executable-script setup this harness does not provide — verified by hand
/// against the built binary instead, both by relative path and through PATH.
#[rstest]
#[case::source_keyword("source helper.kai | cat", "hello\n")]
#[case::source_dot(". helper.kai | cat", "hello\n")]
#[tokio::test]
async fn a_sourced_or_scripted_stage_keeps_its_stdout(
    #[case] source_text: &str,
    #[case] expected: &str,
) {
    let dir = tempdir().unwrap();
    std::fs::write(dir.path().join("helper.kai"), "echo hello\n").unwrap();
    let kernel = Kernel::new(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_trash(false),
    )
    .expect("failed to create kernel");

    let result = kernel.execute(source_text).await.expect("execution failed");

    assert_eq!(
        result.text_out(),
        expected,
        "`{source_text}` lost its stdout in the pipe (exit {}, stderr {:?})",
        result.code,
        result.err
    );
}

// --- The rest of the class, pinned ----------------------------------------
//
// `ExecContext` has 28 fields, but only five are per-invocation I/O resources
// with move semantics — the shape that can be stolen from an outer command by
// a nested dispatch. The other 23 are `Arc` handles (cloning is correct, no
// ownership) or configuration that is *supposed* to propagate.
//
//   stdin           consumed-once buffer      pinned below
//   stdin_data      consumed-once value       pinned below
//   stdin_data_rx   oneshot receiver          pinned in pipeline_structured_data_tests.rs
//   pipe_stdin      read end                  pinned below + substitution_still_consumes…
//   pipe_stdout     write end                 pinned above
//
// These rows are the audit's result, not a bug report: they passed when
// written. They are here so that adding a sixth resource to `ExecContext`, or
// changing how one of these crosses a sync site, has to break something
// visible first.
//
// Three predictions from a `kaibo deliberate` (gemini-pro) were tested and all
// three were WRONG on this codebase: that `timeout`'s re-dispatch bypasses the
// fix (it does not — `execute_command` moves the resource into the builtin's
// own context, so it rides along), that cloned `stdin` causes a nested `$()`
// and its parent to each read the same bytes (they do not — kaish matches
// bash), and that background jobs race the shared slot (they fork their own
// kernel). The rows below are what those predictions would have broken.

/// stdin is consumed once across a nested dispatch, not re-served. bash agrees:
/// the second reader gets what the first left, not a replay.
#[tokio::test]
async fn stdin_is_consumed_once_not_duplicated_by_a_nested_dispatch() {
    let kernel = kernel();
    let result = kernel
        .execute("x=$(head -n 1); y=$(head -n 1); echo \"x=$x y=$y\"")
        .await
        .expect("execution failed");

    // No stdin at all here, so both reads come back empty — the point is that
    // they agree, and that neither replays a buffer the other consumed.
    assert_eq!(result.text_out(), "x= y=\n");
}

/// A nested dispatch must not steal the *read* end from the statement that
/// follows it. `echo "pre $(echo z)"; cat` prints `pre z` then the piped input.
#[tokio::test]
async fn a_nested_dispatch_leaves_the_read_end_for_the_next_statement() {
    let kernel = kernel();
    let result = kernel
        .execute("echo \"pre $(echo z)\"; echo tail | cat")
        .await
        .expect("execution failed");

    assert_eq!(result.text_out(), "pre z\ntail\n");
}

/// stderr is shared by design — every writer appends to one stream — so it is
/// not a steal candidate. A function body's stderr still reaches the caller
/// even when the function is a non-last pipeline stage.
#[tokio::test]
async fn stderr_is_shared_not_owned_so_a_stage_body_still_reports() {
    let kernel = kernel();
    let result = kernel
        .execute("f() { echo e1 >&2; }; f | cat")
        .await
        .expect("execution failed");

    assert!(
        result.err.contains("e1"),
        "a non-last stage's stderr should reach the caller, got {:?}",
        result.err
    );
}
