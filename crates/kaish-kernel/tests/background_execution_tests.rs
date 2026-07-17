//! TDD tests for background execution (`&` operator).
//!
//! These tests document expected behavior. Implementation in kernel.rs
//! will make them pass.
//!
//! The `&` operator should:
//! - Spawn the command as a background job
//! - Register it with JobManager and create /v/jobs/{id}/ VFS entries
//! - Return immediately with a job ID like "[1]"
//! - Capture stdout/stderr via BoundedStream
//! - Allow polling status via /v/jobs/{id}/status

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::time::Duration;

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

/// Create a test kernel with an isolated (no local filesystem) configuration.
async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated()).expect("failed to create kernel").into_arc()
}

/// Wait for a job to complete by polling status.
///
/// This avoids flaky sleeps by checking actual job state.
async fn wait_for_job(kernel: &Kernel, job_id: u64, timeout: Duration) -> String {
    let start = std::time::Instant::now();
    let status_cmd = format!("cat /v/jobs/{}/status", job_id);

    loop {
        let result = kernel.execute(&status_cmd).await.expect("status check failed");
        let text = result.text_out();
        let status = text.trim();

        if status.starts_with("done:") || status.starts_with("failed:") {
            return status.to_string();
        }

        if start.elapsed() > timeout {
            panic!("Job {} did not complete within {:?}", job_id, timeout);
        }

        tokio::time::sleep(Duration::from_millis(10)).await;
    }
}

// ============================================================================
// Host filesystem isolation (no spill/job-output leaks)
// ============================================================================

/// A kernel that owns no host filesystem must not persist job output to a host
/// temp file — that write goes through `std::fs`, bypassing the VFS and any
/// read-only mount. Holds for `NoLocal` (mounts nothing) and `with_backend`
/// (the embedder owns the VFS). A host-owning kernel still persists.
#[tokio::test]
async fn test_job_output_persistence_disabled_for_hostless_kernels() {
    use kaish_kernel::vfs::{MemoryFs, VfsRouter};
    use kaish_kernel::{KernelBackend, LocalBackend};

    // NoLocal: no host filesystem mounted.
    let isolated = Kernel::new(KernelConfig::isolated()).expect("isolated kernel");
    assert!(
        !isolated.jobs().persist_output_files(),
        "NoLocal kernel must not write job output to host disk"
    );

    // Custom backend: embedder owns the entire VFS, kernel owns no host mounts.
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let embedded = Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |_| {})
        .expect("with_backend kernel");
    assert!(
        !embedded.jobs().persist_output_files(),
        "with_backend kernel must not write job output to host disk"
    );

    // A host-owning kernel (Sandboxed) keeps the default persistence.
    let host_owning = Kernel::new(KernelConfig::transient()).expect("transient kernel");
    assert!(
        host_owning.jobs().persist_output_files(),
        "a host-owning kernel should still persist job output files"
    );
}

/// A `with_backend` kernel must truncate over-limit output in memory, never
/// spill it to a host file via `std::fs` — even when the config requests
/// `SpillMode::Disk`. Otherwise project bytes leak to host `/tmp`, bypassing the
/// read-only VFS. Uses a `Sandboxed`-mode config (whose `vfs_mode` alone would
/// NOT force memory) so this proves the `with_backend` override specifically.
#[tokio::test]
async fn test_with_backend_forces_in_memory_spill_not_host_disk() {
    use kaish_kernel::vfs::{MemoryFs, VfsRouter};
    use kaish_kernel::{KernelBackend, LocalBackend, OutputLimitConfig};

    let mut limit = OutputLimitConfig::agent(); // SpillMode::Disk (the leaky default)
    limit.set_limit(Some(16));
    let config = KernelConfig::transient().with_output_limit(limit); // Sandboxed mode

    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let kernel = Kernel::with_backend(backend, config, |_| {}, |_| {})
        .expect("with_backend kernel");

    let result = kernel.execute("seq 1 50").await.expect("execute failed");
    assert!(result.did_spill, "output over the 16-byte limit should be truncated");
    assert!(
        result.text_out().contains("no spill file"),
        "with_backend kernel must truncate in memory, not spill to host disk; got: {}",
        result.text_out()
    );
}

// ============================================================================
// Basic Background Execution
// ============================================================================

#[tokio::test]
async fn test_background_job_returns_job_id() {
    let kernel = setup().await;
    let result = kernel.execute("echo hello &").await.unwrap();

    assert!(result.ok(), "background command should succeed, got: {}", result.err);
    // Should return job ID like "[1]"
    assert!(
        result.text_out().contains("[1]") || result.text_out().contains("1"),
        "expected job ID in output, got: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn test_background_job_creates_vfs_entry() {
    let kernel = setup().await;
    kernel.execute("echo hello &").await.unwrap();

    // Give job a moment to register
    tokio::time::sleep(Duration::from_millis(10)).await;

    // Job should appear in /v/jobs
    let result = kernel.execute("ls /v/jobs").await.unwrap();
    assert!(result.ok(), "ls /v/jobs failed: {}", result.err);
    assert!(
        result.text_out().contains("1"),
        "expected job 1 in /v/jobs, got: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn test_background_job_captures_stdout() {
    let kernel = setup().await;
    kernel.execute("echo 'hello from background' &").await.unwrap();

    wait_for_job(&kernel, 1, Duration::from_secs(1)).await;

    let result = kernel.execute("cat /v/jobs/1/stdout").await.unwrap();
    assert!(result.ok(), "cat stdout failed: {}", result.err);
    assert!(
        result.text_out().contains("hello from background"),
        "expected stdout content, got: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn test_background_job_status_transitions() {
    let kernel = setup().await;
    // The background command must run long enough that the "running" check
    // below reliably observes it mid-flight, while the completion wait below
    // has generous margin over it — a prior version used `sleep 1` against a
    // 1s wait_for_job deadline with zero margin, so host load could push the
    // job's actual completion past the deadline and fail the test (#148).
    // `sleep 0.5` keeps both margins strictly better than the original on
    // both axes: ~490ms of headroom for the "running" check below (10ms
    // registration sleep + an execute() round-trip through the kernel lock —
    // #148 was seen at load average 50-130, exactly the conditions that can
    // stretch that round-trip) and 10x headroom (5s wait) on completion,
    // versus the original's ~990ms/1x.
    kernel.execute("sleep 0.5 &").await.unwrap();

    // Give job a moment to register
    tokio::time::sleep(Duration::from_millis(10)).await;

    // Check status while running (immediate check)
    let result = kernel.execute("cat /v/jobs/1/status").await.unwrap();
    assert!(result.ok(), "status check failed: {}", result.err);
    assert_eq!(result.text_out().trim(), "running", "expected running status");

    // Wait for completion, with 10x margin over the 0.5s sleep above.
    let status = wait_for_job(&kernel, 1, Duration::from_secs(5)).await;
    assert_eq!(status, "done:0", "expected done:0 status");
}

#[tokio::test]
async fn test_background_job_command_file() {
    let kernel = setup().await;
    kernel.execute("echo test123 &").await.unwrap();

    wait_for_job(&kernel, 1, Duration::from_secs(1)).await;

    let result = kernel.execute("cat /v/jobs/1/command").await.unwrap();
    assert!(result.ok(), "cat command failed: {}", result.err);
    assert!(
        result.text_out().contains("echo") && result.text_out().contains("test123"),
        "expected command in output, got: {}",
        result.text_out()
    );
}

// ============================================================================
// Multiple Jobs
// ============================================================================

#[tokio::test]
async fn test_multiple_background_jobs() {
    let kernel = setup().await;

    kernel.execute("echo job1 &").await.unwrap();
    kernel.execute("echo job2 &").await.unwrap();
    kernel.execute("echo job3 &").await.unwrap();

    // Wait for all jobs
    wait_for_job(&kernel, 1, Duration::from_secs(1)).await;
    wait_for_job(&kernel, 2, Duration::from_secs(1)).await;
    wait_for_job(&kernel, 3, Duration::from_secs(1)).await;

    // All jobs should exist
    let result = kernel.execute("ls /v/jobs").await.unwrap();
    assert!(result.ok());
    assert!(result.text_out().contains("1"), "missing job 1");
    assert!(result.text_out().contains("2"), "missing job 2");
    assert!(result.text_out().contains("3"), "missing job 3");
}

#[tokio::test]
async fn test_each_job_has_correct_output() {
    let kernel = setup().await;

    kernel.execute("echo 'output-one' &").await.unwrap();
    kernel.execute("echo 'output-two' &").await.unwrap();

    wait_for_job(&kernel, 1, Duration::from_secs(1)).await;
    wait_for_job(&kernel, 2, Duration::from_secs(1)).await;

    let r1 = kernel.execute("cat /v/jobs/1/stdout").await.unwrap();
    let r2 = kernel.execute("cat /v/jobs/2/stdout").await.unwrap();

    assert!(r1.text_out().contains("output-one"), "job 1 wrong output: {}", r1.text_out());
    assert!(r2.text_out().contains("output-two"), "job 2 wrong output: {}", r2.text_out());
}

// ============================================================================
// Context Inheritance
// ============================================================================

#[tokio::test]
async fn test_background_job_inherits_env() {
    let kernel = setup().await;

    // Set environment variable
    kernel.execute("export MY_VAR=test_value").await.unwrap();
    kernel.execute("echo $MY_VAR &").await.unwrap();

    wait_for_job(&kernel, 1, Duration::from_secs(1)).await;

    let result = kernel.execute("cat /v/jobs/1/stdout").await.unwrap();
    assert!(
        result.text_out().contains("test_value"),
        "expected env var in output, got: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn test_background_job_inherits_cwd() {
    let kernel = setup().await;

    // Create a unique test directory in the in-memory VFS and cd into it
    let dir = format!("/tmp/test_cwd_{}", std::process::id());
    kernel.execute(&format!("mkdir -p {dir}")).await.unwrap();
    kernel.execute(&format!("cd {dir}")).await.unwrap();
    kernel.execute("pwd &").await.unwrap();

    wait_for_job(&kernel, 1, Duration::from_secs(1)).await;

    let result = kernel.execute("cat /v/jobs/1/stdout").await.unwrap();
    assert!(
        result.text_out().contains(&dir),
        "expected cwd in output, got: {}",
        result.text_out()
    );
}

// ============================================================================
// Error Handling & Exit Codes
// ============================================================================

#[tokio::test]
async fn test_failed_background_job_status() {
    let kernel = setup().await;

    // Command that will fail (false returns exit code 1)
    kernel.execute("false &").await.unwrap();

    let status = wait_for_job(&kernel, 1, Duration::from_secs(1)).await;
    assert!(
        status.starts_with("failed:") || status == "done:1",
        "expected failed status, got: {}",
        status
    );
}

// ============================================================================
// Pipelines in Background
// ============================================================================

#[tokio::test]
async fn test_pipeline_in_background() {
    let kernel = setup().await;

    kernel.execute("echo 'line1\nline2\nline3' | wc -l &").await.unwrap();

    wait_for_job(&kernel, 1, Duration::from_secs(1)).await;

    let result = kernel.execute("cat /v/jobs/1/stdout").await.unwrap();
    assert!(result.ok(), "cat failed: {}", result.err);
    // wc -l should output "3"
    assert!(
        result.text_out().trim() == "3" || result.text_out().contains("3"),
        "expected 3 lines, got: {}",
        result.text_out()
    );
}

// ============================================================================
// Jobs Builtin Integration
// ============================================================================

#[tokio::test]
async fn test_jobs_builtin_shows_background_job() {
    let kernel = setup().await;

    kernel.execute("echo background-test &").await.unwrap();

    // Give job a moment to register
    tokio::time::sleep(Duration::from_millis(10)).await;

    let result = kernel.execute("jobs").await.unwrap();
    assert!(result.ok(), "jobs command failed: {}", result.err);
    assert!(
        result.text_out().contains("1") && result.text_out().contains("/v/jobs/1/"),
        "expected job info, got: {}",
        result.text_out()
    );
}

/// `wait %N` — the bash jobspec form — must parse (it was a lexer error) and
/// wait for the named background job end-to-end.
#[tokio::test]
async fn wait_jobspec_percent_form_end_to_end() {
    let kernel = setup().await;
    let result = kernel
        .execute("sleep 0.05 & wait %1")
        .await
        .expect("wait %1 should parse and run");
    assert!(result.ok(), "wait %1 failed: {}", result.err);
    assert!(
        result.text_out().contains("[1]"),
        "expected job 1 status, got: {}",
        result.text_out()
    );
}

/// `wait %N` propagates a background job's failure: exit 1 + "Failed" in the
/// status line. Pins the exit-code contract (any waited job failed → 1)
/// through the full kernel, not just the inline unit tests.
#[tokio::test]
async fn wait_propagates_background_job_failure() {
    let kernel = setup().await;
    let result = kernel
        .execute("bgfail() { sleep 0.01; return 7; }; bgfail & wait %1")
        .await
        .expect("execute");
    assert_eq!(
        result.code, 1,
        "wait on a failed job must exit 1: out={} err={}",
        result.text_out(),
        result.err
    );
    assert!(
        result.text_out().contains("[1] Failed"),
        "expected failed status line, got: {}",
        result.text_out()
    );
}

/// `kill %N` removes the job from the table, so a subsequent `wait %N` reports
/// it gone (exit 1, "not found") rather than hanging or succeeding silently.
/// Pure kaish job control — works in hermetic builds (no `subprocess`).
#[tokio::test]
async fn wait_after_kill_reports_job_gone() {
    let kernel = setup().await;
    let killed = kernel.execute("sleep 30 & kill %1").await.expect("execute");
    assert_eq!(killed.code, 0, "kill %1 should succeed: {}", killed.err);

    let waited = kernel.execute("wait %1").await.expect("execute");
    assert!(!waited.ok(), "wait on a killed job must fail");
    assert!(
        waited.err.contains("not found"),
        "expected job-not-found, got: {}",
        waited.err
    );
}

/// Phase 1: `kill %N` stops a pure-builtin background job (no OS process
/// group) via its cancellation token, and removes it from the table. This is
/// kernel-level job control: it works in **every** build, hermetic included.
#[tokio::test]
async fn kill_terminates_builtin_background_job() {
    let kernel = setup().await;
    // Background a long builtin sleep, then kill it. The kill must succeed and
    // drop the job — a follow-up kill reports it gone.
    let r = kernel.execute("sleep 30 & kill %1").await.expect("execute");
    assert_eq!(r.code, 0, "kill %1 of a builtin job should succeed: {}", r.err);

    let again = kernel.execute("kill %1").await.expect("execute");
    assert!(!again.ok(), "job should be gone after kill");
    assert!(again.err.contains("not found"), "got: {}", again.err);
}

/// A non-terminating signal to a pure-builtin job is refused loudly (there is
/// no process group to deliver SIGUSR1 to), rather than silently terminating.
/// Holds in every build — the refusal is the same with or without `subprocess`.
#[tokio::test]
async fn kill_nonterminating_signal_on_builtin_job_errors() {
    let kernel = setup().await;
    let r = kernel
        .execute("sleep 30 & kill --signal USR1 %1")
        .await
        .expect("execute");
    assert!(!r.ok());
    assert!(
        r.err.contains("in-process task") && r.err.contains("USR1"),
        "expected in-process-task error, got: {}",
        r.err
    );
    // Clean up the still-running job.
    let _ = kernel.execute("kill %1").await;
}
