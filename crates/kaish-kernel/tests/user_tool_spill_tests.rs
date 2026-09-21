//! A spill inside a user-defined function body must reach the function's own
//! `ExecResult`, not just the inner statement that produced it.
//!
//! `execute_user_tool` accumulates each body statement's output and exit code
//! by hand rather than through `accumulate_result` (it works in raw bytes, not
//! text). The accumulation copied `code` and `.data` but never
//! `did_spill`/`original_code`, so a function whose last statement spilled
//! returned a plain `ExecResult` with `did_spill: false` — the fact was true
//! one call frame down and lost climbing back out. `Job::to_info` reads these
//! two fields straight off the job's cached `ExecResult` (see
//! `scheduler/job.rs`), so the background path loses the same fact a
//! foreground caller does.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;
use std::time::Duration;

use kaish_kernel::scheduler::JobId;
use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated()).expect("failed to create kernel").into_arc()
}

/// Wait for a job to complete by polling status (mirrors
/// `background_execution_tests.rs`).
async fn wait_for_job(kernel: &Kernel, job_id: u64, timeout: Duration) -> String {
    let start = std::time::Instant::now();
    let status_cmd = format!("cat /v/jobs/{}/status", job_id);

    loop {
        let result = kernel.execute(&status_cmd).await.expect("status check failed");
        let text = result.text_out();
        let status = text.trim();

        if status.starts_with("done:") || status.starts_with("failed:") || status.starts_with("killed:") {
            return status.to_string();
        }

        if start.elapsed() > timeout {
            panic!("Job {} did not complete within {:?}", job_id, timeout);
        }

        tokio::time::sleep(Duration::from_millis(10)).await;
    }
}

/// Foreground: calling the function directly must report the spill on the
/// function's own result, exactly as calling `seq` directly would.
#[tokio::test]
async fn a_function_body_spill_survives_to_the_function_s_own_result() {
    let kernel = setup().await;
    kernel.execute("set -o output-limit=64").await.unwrap();
    kernel.execute("function spills { seq 1 5000 }").await.unwrap();

    let result = kernel.execute("spills").await.unwrap();
    assert!(
        result.did_spill,
        "the body's spill must survive to the function's result: {result:?}"
    );
    assert_eq!(
        result.original_code,
        Some(0),
        "seq exited 0 before the remap replaced it: {result:?}"
    );
    assert_eq!(result.code, 3, "the remapped code is still 3");
}

/// Background: `Job::to_info` reads `did_spill`/`original_code` off the same
/// `ExecResult` a foreground call gets — if `execute_user_tool` drops them, a
/// spilled function's job is indistinguishable from one that returned 3 on
/// its own (both report `failed:3`).
#[tokio::test]
async fn a_function_body_spill_survives_as_a_background_job() {
    let kernel = setup().await;
    kernel.execute("set -o output-limit=64").await.unwrap();
    kernel.execute("function spills { seq 1 5000 }").await.unwrap();

    kernel.execute("spills &").await.unwrap();
    let status = wait_for_job(&kernel, 1, Duration::from_secs(5)).await;
    assert_eq!(status, "failed:3");

    let info = kernel.jobs().get(JobId(1)).await.expect("job 1 is still tracked");
    assert!(info.did_spill, "the job must carry the spill fact: {info:?}");
    assert_eq!(
        info.original_code,
        Some(0),
        "seq exited 0 before the remap replaced it: {info:?}"
    );
}

// ── `ControlFlow::Exit` also dropped `original_code` (review follow-up) ────
//
// The three arms above (`Normal`/`Return`/`Break`/`Continue`) assign
// `original_code` alongside `did_spill`. The `Exit` arm — reached by `set -e`
// aborting on a failing statement, or by a bare `exit N` inside the body —
// only OR'd `did_spill` and left `original_code` untouched, so it kept
// whatever an EARLIER statement had set even once that earlier spill had
// nothing to do with the exit's own code.

/// `set -e` aborts on the spilling statement itself, so `code`, `did_spill`,
/// and `original_code` all belong to that SAME result — `original_code` must
/// come through as `Some(0)`, not stay `None` (which would make
/// `original_code.unwrap_or(code)` misreport the real exit as 3).
#[tokio::test]
async fn errexit_on_a_spilling_first_statement_keeps_its_original_code() {
    let kernel = setup().await;
    kernel.execute("set -o output-limit=64").await.unwrap();
    kernel
        .execute("function spills_then_errexits { set -e; seq 1 5000; echo unreachable }")
        .await
        .unwrap();

    let result = kernel.execute("spills_then_errexits").await.unwrap();
    assert_eq!(result.code, 3, "errexit aborts on seq's own (remapped) code: {result:?}");
    assert!(result.did_spill, "the spill still happened: {result:?}");
    assert_eq!(
        result.original_code,
        Some(0),
        "seq exited 0 before the remap replaced it — `unwrap_or` must not fall back to 3: {result:?}"
    );
}

/// A spill in statement 1, then an unrelated `exit 5`: `did_spill` stays
/// sticky (truncation is a fact about output already produced), but
/// `original_code` must NOT still answer `Some(0)` for a code `exit 5` set on
/// its own terms.
///
/// `real_exit` (`original_code.unwrap_or(code)`) is what an embedder actually
/// reads (`docs/EMBEDDING.md`, exit code 3) — mirroring
/// `spill_accumulation_tests.rs`'s own idiom for the identical top-level
/// case. `code` itself is NOT 5 here: calling a function is calling a
/// COMMAND, and `execute_pipeline`'s `apply_spill_contract` call (the "one
/// seam every execution surface... funnels through", GH #212) unconditionally
/// remaps ANY `did_spill: true` command result to exit 3 — the same thing
/// that happens to a plain `seq 1 100000` alone. What the bug lost was NOT
/// that remap (expected, unrelated to this fix) but the `Some(0)` standing in
/// for the real `5` once `execute_user_tool` returned with a stale
/// `original_code`: `apply_spill_contract` only backfills `original_code`
/// when it is still `None`, so a stale `Some(0)` from the earlier `seq`
/// statement silently WON the race and `real_exit` read `0`, never
/// `5`.
#[tokio::test]
async fn a_spill_before_an_exit_does_not_leave_a_stale_original_code() {
    let kernel = setup().await;
    kernel.execute("set -o output-limit=64").await.unwrap();
    kernel.execute("function spills_then_exits { seq 1 5000; exit 5 }").await.unwrap();

    let result = kernel.execute("spills_then_exits").await.unwrap();
    assert!(result.did_spill, "statement 1's truncation still happened: {result:?}");
    assert_eq!(result.code, 3, "a did_spill result is remapped to 3 at the command boundary: {result:?}");
    let real_exit = result.original_code.unwrap_or(result.code);
    assert_eq!(
        real_exit, 5,
        "`exit 5` set the real status; a stale Some(0) from the earlier spill must not survive: {result:?}"
    );
}

/// The `exec_error` early return (a genuine Rust-level fault inside the
/// function body — an assignment's `$((1/0))`, not a `ControlFlow` signal)
/// builds its own `prior` `ExecResult` and must carry the same two fields.
///
/// A function call whose body faults is itself a COMMAND that failed —
/// `execute_command`/`execute_pipeline` propagate the `Err` with `?` with no
/// catch of their own, but by the time it reaches the top of this script
/// (a single statement that is just the function call), it surfaces as an
/// ordinary failed `ExecResult` (`function_body_output_is_kept_in_its_failed_result`
/// in `error_keeps_prior_output_tests.rs` pins this for output/err; this
/// pins it for `did_spill`/`original_code` too). Without the fix, the
/// generic "assignment fault → exit 1" code (not `seq`'s own 0) got
/// captured as `original_code` by the outer output-limit's own idempotent
/// backfill, once it independently re-detected the still-oversize text.
#[tokio::test]
async fn a_spill_before_a_fault_survives_on_the_failed_result() {
    let kernel = setup().await;
    kernel.execute("set -o output-limit=64").await.unwrap();
    kernel
        .execute("function spills_then_faults { seq 1 5000; x=$((1/0)) }")
        .await
        .unwrap();

    let result = kernel
        .execute("spills_then_faults")
        .await
        .expect("a fault inside a function body is a failed result, not a Rust-level error");
    assert!(
        result.err.contains("divides by zero"),
        "the fault's own message must still reach the caller: {result:?}"
    );
    assert!(result.did_spill, "the spill before the fault must survive: {result:?}");
    assert_eq!(
        result.original_code,
        Some(0),
        "seq exited 0 before the remap replaced it — not the generic fault's own exit 1: {result:?}"
    );
}
