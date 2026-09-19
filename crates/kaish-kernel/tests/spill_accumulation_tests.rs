//! A spill that happened stays reported, even when a later statement did not.
//!
//! `accumulate_result` assigned `did_spill = new.did_spill` rather than OR-ing
//! it, so a statement that spilled and then any ordinary statement after it
//! left the block reporting `did_spill: false`. The truncation had still
//! happened and the output was still incomplete — an embedder reading the flag
//! to decide "did I get everything" was told yes.
//!
//! The exit code is a separate question and is NOT changed here: a script's
//! status is its last statement's, so `seq …; echo after` exiting 0 is
//! ordinary shell behavior, not a bug. `did_spill` is a fact about the OUTPUT,
//! not a status, and facts do not expire because another command ran.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::ExecResult;

async fn run(script: &str) -> ExecResult {
    let k = Kernel::new(KernelConfig::repl()).expect("kernel");
    k.execute(script).await.expect("kernel execute")
}

/// The control: a spill on its own is reported.
#[tokio::test]
async fn a_spill_alone_is_reported() {
    let r = run("kaish-output-limit set 2K; seq 1 100000").await;
    assert!(r.did_spill, "the lone spill must report");
    assert_eq!(r.code, 3, "and remap to the documented exit 3");
}

/// The bug: an ordinary statement after the spill erased the fact.
#[tokio::test]
async fn a_later_statement_does_not_erase_the_spill() {
    let r = run("kaish-output-limit set 2K; seq 1 100000; echo after").await;
    assert!(
        r.did_spill,
        "output was truncated earlier in the block; the flag must survive"
    );
}

/// Several statements later, and with the spill first, in the middle, and last.
#[tokio::test]
async fn the_flag_survives_from_any_position() {
    let first = run("kaish-output-limit set 2K; seq 1 100000; echo a; echo b").await;
    assert!(first.did_spill, "spill first");

    let middle = run("kaish-output-limit set 2K; echo a; seq 1 100000; echo b").await;
    assert!(middle.did_spill, "spill in the middle");

    let last = run("kaish-output-limit set 2K; echo a; seq 1 100000").await;
    assert!(last.did_spill, "spill last");
}

/// A block that never spilled must not claim it did — the fix must not turn
/// the flag into "sticky true" for everyone.
#[tokio::test]
async fn a_block_that_did_not_spill_reports_false() {
    let r = run("kaish-output-limit set 2K; echo a; echo b").await;
    assert!(!r.did_spill, "nothing was truncated here");
    assert_eq!(r.code, 0);
}

// ---------------------------------------------------------------------------
// `original_code` tracks the same statement `code` does — the last one.
//
// `accumulate_result` kept the FIRST `original_code` in a block, so a spill in
// statement 1 left its code standing over every statement after it. An
// embedder reading the documented "real exit of a capped result",
// `original_code.unwrap_or(code)`, then settled `seq 1 5000; false` as
// success: `code` was `false`'s 1, but `original_code` was still `seq`'s 0.
//
// `did_spill` is sticky because truncation is a fact about the OUTPUT that a
// later statement cannot undo. `original_code` is not a fact about output — it
// is what `code` would have been without the spill remap, so it belongs to
// whichever statement `code` belongs to.
// ---------------------------------------------------------------------------

/// What an embedder computes as "the real exit of a capped result"
/// (`docs/EMBEDDING.md`, exit code 3). The tests assert this rather than the
/// field shape: it is the value a caller acts on.
fn real_exit(r: &ExecResult) -> i64 {
    r.original_code.unwrap_or(r.code)
}

/// A statement that spills and fails, so its own `original_code` is non-zero:
/// `pipefail` reports the rightmost failing stage, and `seq`'s output spills.
const FAILING_SPILL: &str = "set -o pipefail; false | seq 1 100000";

/// The bug: a spill in statement 1 hid statement 2's failure.
#[tokio::test]
async fn a_spill_does_not_mask_a_later_failure() {
    let r = run("kaish-output-limit set 2K; seq 1 100000; false").await;
    assert!(r.did_spill, "statement 1 was truncated, and that stays reported");
    assert_eq!(
        real_exit(&r),
        1,
        "`false` exited 1; a spill earlier in the block must not report 0"
    );
}

/// The other direction: the block still reports success when it ends in it.
#[tokio::test]
async fn a_spill_before_a_success_reports_success() {
    let r = run("kaish-output-limit set 2K; seq 1 100000; echo after").await;
    assert!(r.did_spill, "statement 1 was truncated");
    assert_eq!(real_exit(&r), 0, "`echo` exited 0");
}

/// The control: a statement that spills AND fails reports its own failure.
#[tokio::test]
async fn a_failing_spill_alone_reports_its_failure() {
    let r = run(&format!("kaish-output-limit set 2K; {FAILING_SPILL}")).await;
    assert!(r.did_spill, "`seq`'s output was truncated");
    assert_eq!(r.code, 3, "the spill remap owns `code`");
    assert_eq!(
        real_exit(&r),
        1,
        "and `original_code` carries the pipefail status the remap replaced"
    );
}

/// …and a statement after it moves the reported exit, as `code` already does.
#[tokio::test]
async fn a_success_after_a_failing_spill_reports_success() {
    let r = run(&format!("kaish-output-limit set 2K; {FAILING_SPILL}; true")).await;
    assert!(r.did_spill, "the truncation still happened");
    assert_eq!(
        real_exit(&r),
        0,
        "the block's last statement succeeded; an earlier failure is not the block's status"
    );
}

/// A block that never spilled leaves `original_code` unset, as documented.
#[tokio::test]
async fn no_spill_leaves_original_code_unset() {
    let r = run("kaish-output-limit set 2K; echo a; false").await;
    assert!(!r.did_spill);
    assert_eq!(r.code, 1, "ordinary shell status");
    assert_eq!(
        r.original_code, None,
        "`original_code` is present only alongside a spill"
    );
}
