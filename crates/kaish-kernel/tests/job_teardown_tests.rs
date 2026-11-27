//! TDD tests for kernel/job-system teardown (GH #245, GH #247 items 2/3).
//!
//! Before this PR:
//! - `Kernel::shutdown()` called `JobManager::wait_all()` with no timeout and
//!   no cancellation — `sleep 3600 &` then `shutdown()` blocked for an hour.
//! - There was no `Kernel::cancel_all_jobs()` — no bulk lever to stop every
//!   tracked job short of `kill %N` one at a time.
//! - `Kernel::reset()` said nothing about background jobs surviving it.
//!
//! These tests drive the fix through `kernel.execute(...)` and the public
//! `Kernel` methods, never a builtin's own `.execute()` — the AGENTS.md
//! convention for exercising the real dispatch path.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::time::Duration;

use kaish_kernel::scheduler::JobId;
use kaish_kernel::{Kernel, KernelConfig};

/// Isolated kernel with a short `kill_grace` so a bounded-wait test does not
/// need to actually wait the production default (2s) grace before deciding
/// a job is unresponsive — `sleep` answers its cancellation token instantly
/// (see `sleep.rs`'s `tokio::select!`), so tests here only exercise the
/// "cancelled promptly" path, never the "grace expired, job abandoned" path.
fn setup() -> Kernel {
    Kernel::new(KernelConfig::isolated().with_kill_grace(Duration::from_millis(50)))
        .expect("failed to create kernel")
}

#[tokio::test]
async fn cancel_all_jobs_stops_a_long_running_background_job() {
    let kernel = setup();
    kernel.execute("sleep 3600 &").await.expect("spawn failed");

    let cancelled = kernel.cancel_all_jobs().await;
    assert_eq!(cancelled, 1, "the one running job should have had its token tripped");

    // The job must unwind almost immediately once cancelled — bound the
    // wait generously (well under the job's nominal 3600s) so a real hang
    // still fails the test instead of running for an hour.
    let result = tokio::time::timeout(Duration::from_secs(5), kernel.jobs().wait(JobId(1)))
        .await
        .expect("job did not unwind within 5s of cancellation")
        .expect("job must produce a result, not vanish");
    assert_eq!(result.code, 130, "a cancelled sleep exits 130, matching kill %N's own path");
}

#[tokio::test]
async fn cancel_all_jobs_is_a_no_op_with_nothing_running() {
    let kernel = setup();
    assert_eq!(kernel.cancel_all_jobs().await, 0);
}

#[tokio::test]
async fn shutdown_returns_promptly_instead_of_hanging_forever() {
    let kernel = setup();
    kernel.execute("sleep 3600 &").await.expect("spawn failed");

    // Before the fix this called `wait_all()` with no timeout at all —
    // this call would have blocked for the job's full 3600s duration.
    let result = tokio::time::timeout(Duration::from_secs(5), kernel.shutdown()).await;
    assert!(
        result.is_ok(),
        "shutdown() must not hang — a single `sleep 3600 &` blocked it for an hour before this fix"
    );
    result.expect("checked above").expect("shutdown itself must not error");
}

#[tokio::test]
async fn shutdown_marks_the_cancelled_job_killed_not_failed() {
    let kernel = setup();
    kernel.execute("sleep 3600 &").await.expect("spawn failed");
    kernel.shutdown().await.expect("shutdown failed");

    let status = kernel
        .jobs()
        .get_status_string(JobId(1))
        .await
        .expect("job should still be tracked after shutdown, awaiting reap");
    assert_eq!(
        status, "killed:130",
        "shutdown's cancellation must colour the terminal status Killed, \
         same as an explicit kill %N, not Failed"
    );
}

#[tokio::test]
async fn shutdown_takes_shared_reference_so_an_arc_held_kernel_can_call_it() {
    // GH #245: EmbeddedClient only holds `Arc<Kernel>`, and background job
    // forks hold their OWN independent `Arc<Kernel>` (from `fork_for_background`),
    // so `Arc::try_unwrap` on the parent's Arc could never be relied on to
    // succeed. `shutdown` must work through a shared reference.
    let kernel = std::sync::Arc::new(setup());
    kernel.execute("echo hi &").await.expect("spawn failed");
    kernel.shutdown().await.expect("shutdown through Arc<Kernel> failed");
}

#[tokio::test]
async fn reset_does_not_touch_background_jobs() {
    let kernel = setup();
    kernel.execute("sleep 3600 &").await.expect("spawn failed");

    kernel.reset().await.expect("reset failed");

    // The job must still be tracked and still running — reset() is a
    // scope/cwd reset, not a session boundary for `&` (documented on
    // `Kernel::reset`, GH #245).
    let status = kernel
        .jobs()
        .get_status_string(JobId(1))
        .await
        .expect("job must survive reset()");
    assert_eq!(status, "running");

    // Clean up: cancel it so the test process doesn't leave a live sleep
    // task behind after the test ends.
    kernel.cancel_all_jobs().await;
}
