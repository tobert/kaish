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
