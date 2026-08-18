//! A background job's stderr is the same stderr the foreground would see.
//!
//! `execute_background` runs the pipeline through `runner.run`, never through
//! `Kernel::execute` — so the statement-boundary drains that collect the
//! fork's stderr channel (#360's mechanism) never ran, and a job's stderr
//! never reached its result: `echo $(cat /nope) &` reported nothing and
//! `/v/jobs/{id}/stderr` stayed empty, while the same statement in the
//! foreground names `/nope`. bash gives a background job the same fd 2 as the
//! foreground; kaish's equivalent is the job's stderr stream.
//!
//! The job announcement (`[1]`) is a shell message, not command output: bash
//! writes it to stderr, and a substitution capturing it (`x=$(cmd &)`) must
//! not see it as data.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;
use std::time::{Duration, Instant};

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated())
        .expect("failed to create kernel")
        .into_arc()
}

/// Poll until the job leaves `running`, like background_execution_tests.
async fn wait_done(kernel: &Kernel, job_id: u64) -> String {
    let deadline = Instant::now() + Duration::from_secs(10);
    loop {
        let status = kernel
            .execute(&format!("cat /v/jobs/{job_id}/status"))
            .await
            .expect("status check")
            .text_out()
            .trim()
            .to_string();
        if !status.starts_with("running") {
            return status;
        }
        assert!(Instant::now() < deadline, "job {job_id} never finished");
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}

async fn job_stderr(kernel: &Kernel, job_id: u64) -> String {
    kernel
        .execute(&format!("cat /v/jobs/{job_id}/stderr"))
        .await
        .expect("read job stderr")
        .text_out()
        .into_owned()
}

/// The repro from the defect: a substitution's stderr reaches the caller in
/// the foreground (#360); the background form must agree.
#[tokio::test]
async fn substitution_stderr_reaches_the_job_stream() {
    let k = setup().await;
    let r = k.execute("echo $(cat /nope) &").await.expect("execute");
    let job_id = parse_job_id(&r);
    let status = wait_done(&k, job_id).await;
    assert!(status.starts_with("done"), "job status: {status}");
    let stderr = job_stderr(&k, job_id).await;
    assert!(
        stderr.contains("/nope"),
        "the substitution's stderr must reach the job's stderr stream, got {stderr:?}"
    );
}

/// An intermediate pipeline stage flushes its stderr to the kernel's stderr
/// channel; a background job must drain it exactly like the foreground.
#[tokio::test]
async fn intermediate_stage_stderr_reaches_the_job_stream() {
    let k = setup().await;
    let r = k.execute("cat /nope | head &").await.expect("execute");
    let job_id = parse_job_id(&r);
    wait_done(&k, job_id).await;
    let stderr = job_stderr(&k, job_id).await;
    assert!(
        stderr.contains("/nope"),
        "the first stage's stderr must reach the job's stderr stream, got {stderr:?}"
    );
}

/// The announcement is a shell message: stderr, line-terminated, and stdout
/// stays clean so `$(cmd &)` captures no shell metadata.
#[tokio::test]
async fn announcement_rides_stderr_and_ends_its_line() {
    let k = setup().await;
    let r = k.execute("echo hi &").await.expect("execute");
    assert!(
        r.text_out().is_empty(),
        "the announcement is not command output, got stdout {:?}",
        r.text_out()
    );
    assert!(
        r.err.starts_with('[') && r.err.ends_with('\n') && !r.err.ends_with("\n\n"),
        "the announcement must ride stderr and end exactly one line, got {:?}",
        r.err
    );
}

/// The job id the announcement names.
fn parse_job_id(result: &kaish_kernel::interpreter::ExecResult) -> u64 {
    let text = result.err.trim();
    assert!(
        text.starts_with('[') && text.ends_with(']'),
        "expected a `[N]` announcement on stderr, got {text:?}"
    );
    text[1..text.len() - 1].parse().expect("job id is a number")
}
