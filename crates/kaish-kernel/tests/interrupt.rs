//! `ExecuteOptions::interrupt` — the polled interrupt check for embedders
//! whose thread cannot fire a cancel token while execution runs (the
//! browser: single-threaded wasm polling a SharedArrayBuffer flag).
//!
//! The contract under test: the kernel polls the check at its cancellation
//! checkpoints, an interrupt maps to the same exit-130 path as
//! `Kernel::cancel()`, session state survives, and the check never leaks
//! past its own call.


#![allow(clippy::expect_used)]
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};

fn kernel() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated())
        .expect("kernel")
        .into_arc()
}

/// A runaway `while true` loop stops with exit 130 when the check fires,
/// and everything the session did before and during the loop survives.
#[tokio::test]
async fn interrupt_stops_loop_preserving_state() {
    let k = kernel();
    k.execute("BEFORE=kept").await.expect("set var");

    let polls = Arc::new(AtomicU32::new(0));
    let seen = polls.clone();
    let opts = ExecuteOptions::new()
        .with_interrupt(Arc::new(move || seen.fetch_add(1, Ordering::Relaxed) >= 3));

    let r = k
        .execute_with_options("while true; do DURING=also-kept; done", opts)
        .await
        .expect("interrupted execute returns a result, not an error");
    assert_eq!(r.code, 130, "interrupt maps to SIGINT-style exit 130");
    assert!(polls.load(Ordering::Relaxed) >= 3, "check was actually polled");

    // The point of tier-2 interrupt: the session survives.
    let r = k.execute("echo \"$BEFORE/$DURING\"").await.expect("echo");
    assert_eq!(r.text_out().trim(), "kept/also-kept");
}

/// A check that never fires changes nothing.
#[tokio::test]
async fn inert_interrupt_is_invisible() {
    let k = kernel();
    let opts = ExecuteOptions::new().with_interrupt(Arc::new(|| false));
    let r = k.execute_with_options("echo hello", opts).await.expect("echo");
    assert_eq!(r.code, 0);
    assert_eq!(r.text_out().trim(), "hello");
}

/// The check is per-call: it must not leak into the next execute.
#[tokio::test]
async fn interrupt_does_not_leak_into_later_calls() {
    let k = kernel();
    let opts = ExecuteOptions::new().with_interrupt(Arc::new(|| true));
    let r = k
        .execute_with_options("while true; do true; done", opts)
        .await
        .expect("interrupted");
    assert_eq!(r.code, 130);

    // Same kernel, plain execute: an interrupt stuck in the slot would kill
    // this loop too. Three iterations then exit cleanly proves it's gone.
    let r = k
        .execute("N=0; while test $N -lt 3; do N=$((N + 1)); done; echo $N")
        .await
        .expect("clean run");
    assert_eq!(r.code, 0, "stale interrupt leaked: {}", r.err);
    assert_eq!(r.text_out().trim(), "3");
}
