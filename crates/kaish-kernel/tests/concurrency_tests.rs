//! Regression tests for the `execute_lock` and `Kernel::fork` concurrency fix
//! for the kernel's serialized-execute + per-worker-fork design.
//!
//! Two categories:
//!
//! 1. **Lock safety** — multiple `execute()` callers on a shared `Arc<Kernel>`
//!    must not clobber each other's scope / cwd / aliases / stderr. Before the
//!    fix, `dispatch_command` and `execute_pipeline` snapshot per-session state
//!    out of RwLocks, ran, then wrote it back, so concurrent callers would
//!    lose updates. The execute lock serialises them into a fair FIFO queue.
//!
//! 2. **Fork semantics** — background jobs, scatter parallel workers, and
//!    concurrent pipeline stages all run against a forked kernel. Forks
//!    snapshot per-session state (so mutations stay isolated) while sharing
//!    the job manager / VFS / tool registry (so `jobs`, files, and tools all
//!    still work). The fork unlocks the full dispatch chain inside background
//!    contexts — user tools, `.kai` scripts, and `$(...)` in args now work
//!    where they didn't before.
//!
//! All tests use a multi-threaded tokio runtime so contention is real.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;
use std::time::Duration;

use kaish_kernel::{Kernel, KernelConfig};

/// Construct a fresh in-memory kernel with validation skipped so tests can
/// use dynamically-generated scripts without tripping the validator.
async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

// ============================================================================
// 1. Lock safety — concurrent execute() on a shared Kernel
// ============================================================================

/// Eight tasks each cd into their own directory and pwd, 50 times.
/// Before the lock, concurrent callers would see each other's cwd values
/// because `execute_pipeline` snapshots cwd out of the RwLock and writes it
/// back after running.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_cwd_no_clobber() {
    let kernel = setup().await;
    // Create the target directories up front.
    for i in 0..8 {
        kernel
            .execute(&format!("mkdir -p /task-{i}"))
            .await
            .expect("mkdir");
    }

    let mut handles = Vec::new();
    for i in 0..8 {
        let k = Arc::clone(&kernel);
        handles.push(tokio::spawn(async move {
            for _ in 0..50 {
                let script = format!("cd /task-{i}; pwd");
                let r = k.execute(&script).await.expect("execute");
                let pwd = r.text_out().trim().to_string();
                assert_eq!(
                    pwd, format!("/task-{i}"),
                    "task {i} saw another task's cwd: {pwd}"
                );
            }
        }));
    }
    for h in handles {
        h.await.expect("task panicked");
    }
}

/// Eight tasks each set `MY_VAR` to a unique value and echo it 50 times in
/// the same script. If the lock is missing, one task's assignment can bleed
/// into another's echo because scope is snapshotted out of the RwLock.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_var_no_clobber() {
    let kernel = setup().await;
    let mut handles = Vec::new();
    for i in 0..8 {
        let k = Arc::clone(&kernel);
        handles.push(tokio::spawn(async move {
            let label = format!("task-{i}");
            // Set + echo in a single script so the lock is held across both.
            let script = format!(
                "MY_VAR={label}\nfor n in $(seq 1 50); do echo $MY_VAR; done",
            );
            let r = k.execute(&script).await.expect("execute");
            for line in r.text_out().lines() {
                assert_eq!(line, label, "task {i} saw wrong value: {line}");
            }
        }));
    }
    for h in handles {
        h.await.expect("task panicked");
    }
}

/// Same shape with aliases. Aliases are part of the per-session state that
/// `execute_pipeline` syncs through the exec_ctx RwLock.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_alias_no_clobber() {
    let kernel = setup().await;
    let mut handles = Vec::new();
    for i in 0..4 {
        let k = Arc::clone(&kernel);
        handles.push(tokio::spawn(async move {
            let label = format!("task-{i}");
            let script = format!("alias greet='echo {label}'\ngreet\ngreet\ngreet");
            let r = k.execute(&script).await.expect("execute");
            for line in r.text_out().lines() {
                assert_eq!(line, label, "task {i} got {line}");
            }
        }));
    }
    for h in handles {
        h.await.expect("task panicked");
    }
}

/// Four tasks each write a unique stderr tag and assert the result only
/// contains their own tag. Without the lock, `drain_lossy()` on the shared
/// stderr receiver picks up bytes from any concurrent task.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn concurrent_stderr_isolation() {
    let kernel = setup().await;
    let mut handles = Vec::new();
    for i in 0..4 {
        let k = Arc::clone(&kernel);
        handles.push(tokio::spawn(async move {
            let tag = format!("tag-{i}");
            // Run ~20 stderr writes in a row while holding the execute lock.
            let script = format!(
                "for n in $(seq 1 20); do echo {tag} 1>&2; done",
            );
            let r = k.execute(&script).await.expect("execute");
            for line in r.err.lines().filter(|l| !l.is_empty()) {
                assert!(
                    line.contains(&tag),
                    "task {i} got stderr from another task: {line}"
                );
            }
        }));
    }
    for h in handles {
        h.await.expect("task panicked");
    }
}

// ============================================================================
// 2. Fork semantics — background jobs run the full dispatch chain
// ============================================================================

/// Background jobs must not hold the execute lock — otherwise a long-running
/// `&` command blocks all foreground commands until it finishes. This test
/// spawns a 400ms sleep in the background and verifies that a foreground
/// `echo` completes well before the sleep would have.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn background_job_does_not_block_foreground() {
    let kernel = setup().await;
    // Spawn the sleep — returns immediately with "[1]".
    kernel.execute("sleep 0.4 &").await.expect("spawn");
    // Measure how long a foreground echo takes. If the lock were held
    // across the background job, this would wait for the full 400ms.
    let start = std::time::Instant::now();
    let r = kernel.execute("echo ready").await.expect("echo");
    let elapsed = start.elapsed();
    assert_eq!(r.text_out().trim(), "ready");
    assert!(
        elapsed < Duration::from_millis(300),
        "foreground echo took {elapsed:?} — background job is blocking the lock"
    );
}

/// User-defined tools must work inside background jobs. Before the fork
/// refactor, background jobs used `BackendDispatcher`, which only resolves
/// builtins + backend tools — user functions silently failed.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn background_job_runs_user_function() {
    let kernel = setup().await;
    // Define a user tool (POSIX function syntax) and call it in background.
    kernel
        .execute("greet() { echo hello-from-user-fn; }")
        .await
        .expect("define tool");
    kernel.execute("greet &").await.expect("run in background");
    // Wait for job 1 and read its stdout.
    wait_for_job(&kernel, 1, Duration::from_secs(2)).await;
    let r = kernel
        .execute("cat /v/jobs/1/stdout")
        .await
        .expect("read stdout");
    assert!(
        r.text_out().contains("hello-from-user-fn"),
        "background user function didn't run: {}",
        r.text_out()
    );
}

/// Background jobs snapshot the parent's scope at spawn time. Subsequent
/// parent mutations must NOT be visible to the already-running job.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn background_job_snapshot_isolation() {
    let kernel = setup().await;
    kernel.execute("VAR=before").await.expect("set");
    // Start a background job that pauses briefly then echoes VAR — the
    // pause lets the parent mutate VAR before the job reads it. The delay
    // must live INSIDE the backgrounded unit: a bare `sleep 0.2; echo $VAR &`
    // runs the sleep in the foreground and backgrounds only the echo, so the
    // test would pass even under a shared-scope regression.
    kernel
        .execute("snap() { sleep 0.2; echo $VAR; }")
        .await
        .expect("define");
    kernel.execute("snap &").await.expect("spawn");
    kernel.execute("VAR=after").await.expect("mutate");
    wait_for_job(&kernel, 1, Duration::from_secs(2)).await;
    let r = kernel
        .execute("cat /v/jobs/1/stdout")
        .await
        .expect("read stdout");
    assert_eq!(
        r.text_out().trim(),
        "before",
        "background job saw parent's post-spawn mutation"
    );
}

/// The inverse: mutations inside a background job must NOT leak back into
/// the parent's scope. Background jobs are subshells — `&` backgrounds the
/// whole function-call so the mutation happens only inside the fork.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn parent_does_not_see_background_mutation() {
    let kernel = setup().await;
    kernel.execute("VAR=parent-value").await.expect("set");
    // Wrap the mutation in a function so `&` applies to the whole body.
    // A bare `VAR=set-in-bg; echo done &` would run the assignment in the
    // foreground and only background the echo.
    kernel
        .execute("mutator() { VAR=set-in-bg; echo done; }")
        .await
        .expect("define");
    kernel.execute("mutator &").await.expect("spawn");
    wait_for_job(&kernel, 1, Duration::from_secs(2)).await;
    // Parent scope should still show "parent-value".
    let r = kernel.execute("echo $VAR").await.expect("echo");
    assert_eq!(
        r.text_out().trim(),
        "parent-value",
        "background mutation leaked into parent scope"
    );
}

/// Scatter parallel workers must also run the full dispatch chain. Before
/// the fork refactor, scatter workers used `BackendDispatcher` and user
/// functions would silently fall through to "command not found".
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn scatter_parallel_runs_user_function() {
    let kernel = setup().await;
    kernel
        .execute(r#"shout() { echo "loud-${ITEM}"; }"#)
        .await
        .expect("define tool");
    // Feed scatter a JSON array via split.
    let r = kernel
        .execute(r#"split "a b c" | scatter | shout | gather"#)
        .await
        .expect("scatter");
    assert!(r.ok(), "scatter failed: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("loud-a"), "missing loud-a: {out}");
    assert!(out.contains("loud-b"), "missing loud-b: {out}");
    assert!(out.contains("loud-c"), "missing loud-c: {out}");
}

/// The docs' canonical plain-text example (`cat file | scatter --as ITEM ...`)
/// must fan out one worker per line, not bind the whole stdin to one worker.
/// Regression test for the scatter newline-split fix (docs/issues.md #3).
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn scatter_plain_text_stdin_fans_out_per_line() {
    let kernel = setup().await;
    kernel
        .execute(r#"shout() { echo "loud-${ITEM}"; }"#)
        .await
        .expect("define tool");
    // Plain-text, newline-separated stdin — no split/structured data.
    let r = kernel
        .execute(r#"printf "a\nb\nc\n" | scatter --as ITEM | shout | gather"#)
        .await
        .expect("scatter");
    assert!(r.ok(), "scatter failed: {}", r.err);
    let out = r.text_out();
    assert!(out.contains("loud-a"), "missing loud-a (no fan-out?): {out}");
    assert!(out.contains("loud-b"), "missing loud-b: {out}");
    assert!(out.contains("loud-c"), "missing loud-c: {out}");
    // The whole-stdin-as-one-item bug would have produced a single worker
    // bound to "a\nb\nc"; assert that degenerate item never appears.
    assert!(
        !out.contains("loud-a\nb"),
        "stdin bound as one item instead of fanning out: {out}"
    );
}

// ============================================================================
// Helpers
// ============================================================================

/// Wait for a specific job to finish by polling `/v/jobs/{id}/status`.
async fn wait_for_job(kernel: &Kernel, job_id: u64, timeout: Duration) {
    let start = std::time::Instant::now();
    let status_cmd = format!("cat /v/jobs/{job_id}/status");
    loop {
        let r = kernel.execute(&status_cmd).await.expect("status");
        let text = r.text_out();
        let s = text.trim();
        if s.starts_with("done:") || s.starts_with("failed:") {
            return;
        }
        if start.elapsed() > timeout {
            panic!("job {job_id} did not complete within {timeout:?}");
        }
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}
