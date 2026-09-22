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
use std::time::Duration;

use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};

fn kernel() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated())
        .expect("kernel")
        .into_arc()
}

/// Well past what any case below needs when kaish behaves — a regression
/// fails in seconds instead of hanging the test binary.
const HANG: Duration = Duration::from_secs(10);

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

// ============================================================================
// The interrupt reaches a busy BUILTIN, not just kaish's own interpreter
// loops — `ToolCtx::checkpoint` (kaish-tool-api) polls
// `CommandDispatcher::is_cancelled` for exactly this. Every case below is
// wrapped in the module's outer `HANG` timeout so a regression fails in
// seconds instead of hanging the test binary.
// ============================================================================

/// `seq` loops in memory over the count it was asked for — no I/O, so
/// nothing else here could have stopped it. A busy builtin outside a
/// pipeline is the simplest case: no fork is involved, so this alone does
/// not yet prove the fork-copied interrupt (see the pipeline case below).
#[tokio::test]
async fn interrupt_stops_a_busy_builtin() {
    let k = kernel();
    let polls = Arc::new(AtomicU32::new(0));
    let seen = polls.clone();
    let opts = ExecuteOptions::new()
        .with_interrupt(Arc::new(move || seen.fetch_add(1, Ordering::Relaxed) >= 3));

    let r = tokio::time::timeout(HANG, k.execute_with_options("seq 1 50000000", opts))
        .await
        .expect("`seq 1 50000000` did not return within the outer HANG timeout")
        .expect("interrupted execute returns a result, not an error");
    assert_eq!(r.code, 130, "interrupt maps to SIGINT-style exit 130; err: {}", r.err);
    assert!(polls.load(Ordering::Relaxed) >= 3, "check was actually polled");
}

/// A busy builtin inside a pipeline stage. Every stage runs on a forked
/// dispatcher (`fork_attached`), so this is the case that actually depends
/// on the fork copying the parent's interrupt check into its own slot —
/// `interrupt_stops_a_busy_builtin` above never forks at all.
///
/// Both sides are the same busy builtin (rather than `| wc -l`): the last
/// stage decides the pipeline's own exit code, and a downstream builtin
/// whose *own* stdin path isn't yet checkpointed (`wc`/`checksum`; a
/// separate, tracked gap) would finish on whatever partial input arrived and
/// mask an upstream interrupt behind its own exit 0. Using `seq` on both
/// sides means the last stage is interrupted on its own terms too.
#[tokio::test]
async fn interrupt_stops_a_busy_builtin_in_a_pipeline() {
    let k = kernel();
    let polls = Arc::new(AtomicU32::new(0));
    let seen = polls.clone();
    let opts = ExecuteOptions::new()
        .with_interrupt(Arc::new(move || seen.fetch_add(1, Ordering::Relaxed) >= 3));

    let r = tokio::time::timeout(
        HANG,
        k.execute_with_options("seq 1 50000000 | seq 1 50000000", opts),
    )
    .await
    .expect("`seq 1 50000000 | seq 1 50000000` did not return within the outer HANG timeout")
    .expect("interrupted execute returns a result, not an error");
    assert_eq!(r.code, 130, "interrupt maps to SIGINT-style exit 130; err: {}", r.err);
    assert!(polls.load(Ordering::Relaxed) >= 3, "check was actually polled");
}

/// A script's own unbounded loop inside a builtin's mini-language (awk),
/// with no input to scan at all — `BEGIN` runs before any record. Only a
/// per-iteration checkpoint inside awk's own `While`/`For`/`ForIn` evaluator
/// can stop this; there is no file, chunk, or record loop to fall back on.
#[tokio::test]
async fn interrupt_stops_a_busy_awk_program() {
    let k = kernel();
    let polls = Arc::new(AtomicU32::new(0));
    let seen = polls.clone();
    let opts = ExecuteOptions::new()
        .with_interrupt(Arc::new(move || seen.fetch_add(1, Ordering::Relaxed) >= 3));

    let r = tokio::time::timeout(
        HANG,
        k.execute_with_options("awk 'BEGIN { while (1) {} }'", opts),
    )
    .await
    .expect("the awk program did not return within the outer HANG timeout")
    .expect("interrupted execute returns a result, not an error");
    assert_eq!(r.code, 130, "interrupt maps to SIGINT-style exit 130; err: {}", r.err);
    assert!(polls.load(Ordering::Relaxed) >= 3, "check was actually polled");
}

/// The fork gap: kaish's own `while` loop, not a builtin at all, run as a
/// pipeline stage. `while` is a compound statement, so a stage dispatches it
/// through `dispatch_stmt` on the stage's forked `Kernel` (`fork_attached`)
/// — before the fork copied the interrupt check, that fork's `interrupt`
/// slot was always `None` (`fork_inner` starts every fork that way), so this
/// loop was unstoppable by a polled interrupt no matter how kaish's own loop
/// checkpoints changed. Both sides loop, for the same reason
/// `interrupt_stops_a_busy_builtin_in_a_pipeline` uses `seq` on both sides —
/// see its comment.
#[tokio::test]
async fn interrupt_stops_a_busy_loop_inside_a_pipeline_stage() {
    let k = kernel();
    let polls = Arc::new(AtomicU32::new(0));
    let seen = polls.clone();
    let opts = ExecuteOptions::new()
        .with_interrupt(Arc::new(move || seen.fetch_add(1, Ordering::Relaxed) >= 3));

    let r = tokio::time::timeout(
        HANG,
        k.execute_with_options("while true; do :; done | while true; do :; done", opts),
    )
    .await
    .expect("the piped while loops did not return within the outer HANG timeout")
    .expect("interrupted execute returns a result, not an error");
    assert_eq!(r.code, 130, "interrupt maps to SIGINT-style exit 130; err: {}", r.err);
    assert!(polls.load(Ordering::Relaxed) >= 3, "check was actually polled");
}
