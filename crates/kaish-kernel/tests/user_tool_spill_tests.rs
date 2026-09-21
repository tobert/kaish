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
