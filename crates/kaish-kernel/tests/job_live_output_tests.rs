//! A background job's output is readable **while the job is still running**.
//!
//! GH #240 removed `/v/jobs/{id}/stdout` and `/stderr` because they filled
//! once, at completion, while four docs promised a live stream. The nodes are
//! back, this time fed per 8 KiB chunk from the external-command drain tasks
//! as the child emits — which is what an embedder polling a running
//! `cargo build` needed all along.
//!
//! **Every test here must fail if the stream stops being live.** The shape
//! that does that: a child emits, sleeps, emits again, and the assertion is
//! that the first token is readable while `status` still says `running` and
//! the second token has not arrived yet. A test that only checks the final
//! aggregate would pass against the pre-#240 behavior this replaces.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(feature = "localfs", feature = "subprocess"))]

use std::collections::HashMap;
use std::time::{Duration, Instant};

use kaish_kernel::ast::Value;
use kaish_kernel::scheduler::JobId;
use kaish_kernel::{Kernel, KernelConfig};

/// The execution core is hermetic — it never reads OS env — so PATH comes in
/// through `initial_vars`, exactly as the REPL frontend supplies it.
fn kernel() -> Kernel {
    let mut vars = HashMap::new();
    vars.insert(
        "PATH".to_string(),
        Value::String(std::env::var("PATH").unwrap_or_default()),
    );
    Kernel::new(KernelConfig::repl().with_initial_vars(vars)).expect("failed to create kernel")
}

/// How long a liveness poll may run before the test calls it a hang. Generous
/// against a loaded CI box; the assertions, not this bound, are what fail when
/// liveness regresses.
const LIVE_TIMEOUT: Duration = Duration::from_secs(10);

async fn stdout_of(kernel: &Kernel, id: JobId) -> String {
    let bytes = kernel.jobs().read_stdout(id).await.expect("job must exist");
    String::from_utf8_lossy(&bytes).into_owned()
}

async fn stderr_of(kernel: &Kernel, id: JobId) -> String {
    let bytes = kernel.jobs().read_stderr(id).await.expect("job must exist");
    String::from_utf8_lossy(&bytes).into_owned()
}

async fn status_of(kernel: &Kernel, id: JobId) -> String {
    kernel.jobs().get_status_string(id).await.expect("job must exist")
}

/// Poll until the job's status leaves `running`, then return it.
async fn wait_done(kernel: &Kernel, id: JobId) -> String {
    let deadline = Instant::now() + LIVE_TIMEOUT;
    loop {
        let status = status_of(kernel, id).await;
        if status != "running" {
            return status;
        }
        assert!(Instant::now() < deadline, "job never finished within {LIVE_TIMEOUT:?}");
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
}

/// The core liveness proof, read through the `JobManager` API.
///
/// The three assertions together are airtight: `status` is sampled **before**
/// the stream, so a `running` status plus a stream holding `first` but not
/// `second` cannot be explained by a completed job dumping its whole buffer —
/// a completed job would have written `second` too.
#[tokio::test]
async fn stdout_is_readable_while_the_job_is_still_running() {
    let kernel = kernel();
    kernel
        .execute("sh -c 'echo first; sleep 2; echo second' &")
        .await
        .expect("spawn failed");
    let id = JobId(1);

    let deadline = Instant::now() + LIVE_TIMEOUT;
    loop {
        let status = status_of(&kernel, id).await;
        let out = stdout_of(&kernel, id).await;

        if out.contains("first") {
            assert_eq!(
                status, "running",
                "the job was already finished the first time any output appeared — \
                 the stream is filling at completion, not live"
            );
            assert!(
                !out.contains("second"),
                "the whole buffer arrived at once ({out:?}) — that is the completion-time \
                 write GH #240 removed, not a live stream"
            );
            break;
        }

        assert_eq!(
            status, "running",
            "the job finished before a single byte was readable — the stream is not live"
        );
        assert!(
            Instant::now() < deadline,
            "no output appeared within {LIVE_TIMEOUT:?} while the job ran"
        );
        tokio::time::sleep(Duration::from_millis(20)).await;
    }

    assert_eq!(wait_done(&kernel, id).await, "done:0");
    let out = stdout_of(&kernel, id).await;
    assert!(out.contains("first") && out.contains("second"), "final stream: {out:?}");
    assert_eq!(
        out.matches("first").count(),
        1,
        "the completion path must not re-write bytes the live tee already delivered: {out:?}"
    );
}

/// The same liveness, read the way a model reads it: `cat /v/jobs/1/stdout`.
#[tokio::test]
async fn the_vfs_node_grows_while_the_job_runs() {
    let kernel = kernel();
    kernel
        .execute("sh -c 'echo first; sleep 2; echo second' &")
        .await
        .expect("spawn failed");

    let deadline = Instant::now() + LIVE_TIMEOUT;
    loop {
        let status = status_of(&kernel, JobId(1)).await;
        let res = kernel.execute("cat /v/jobs/1/stdout").await.unwrap();
        let out = res.text_out().into_owned();

        if out.contains("first") {
            assert_eq!(status, "running", "the node only filled once the job was over");
            assert!(!out.contains("second"), "whole buffer at once: {out:?}");
            break;
        }
        assert_eq!(status, "running", "job finished before the node had anything in it");
        assert!(Instant::now() < deadline, "node stayed empty for the whole run");
        tokio::time::sleep(Duration::from_millis(20)).await;
    }

    wait_done(&kernel, JobId(1)).await;

    // The directory listing has to agree with what is readable.
    let res = kernel.execute("ls /v/jobs/1").await.unwrap();
    let listing = res.text_out().into_owned();
    assert!(listing.contains("stdout"), "ls must show the node: {listing:?}");
    assert!(listing.contains("stderr"), "ls must show the node: {listing:?}");
}

/// stderr is live on the same terms as stdout.
#[tokio::test]
async fn stderr_is_readable_while_the_job_is_still_running() {
    let kernel = kernel();
    kernel
        .execute("sh -c 'echo boom >&2; sleep 2; echo later >&2' &")
        .await
        .expect("spawn failed");
    let id = JobId(1);

    let deadline = Instant::now() + LIVE_TIMEOUT;
    loop {
        let status = status_of(&kernel, id).await;
        let err = stderr_of(&kernel, id).await;
        if err.contains("boom") {
            assert_eq!(status, "running", "stderr only filled at completion");
            assert!(!err.contains("later"), "whole buffer at once: {err:?}");
            break;
        }
        assert_eq!(status, "running", "job finished before stderr had anything in it");
        assert!(Instant::now() < deadline, "stderr stayed empty for the whole run");
        tokio::time::sleep(Duration::from_millis(20)).await;
    }

    wait_done(&kernel, id).await;
    let err = stderr_of(&kernel, id).await;
    assert_eq!(err.matches("boom").count(), 1, "no double write on stderr either: {err:?}");
}

/// A pipeline whose **last** stage is an external streams that stage live —
/// and streams only that stage. An upstream stage's bytes are the next
/// stage's stdin, not the job's stdout, so they must never reach the node.
///
/// The slow work is deliberately in the last stage: kaish forwards a stage's
/// output to the next stage when the stage *returns*, never chunk by chunk, so
/// an upstream external produces no observable live window no matter what it
/// does. Only the last stage's own emission is watchable.
#[tokio::test]
async fn a_pipeline_streams_its_last_stage_and_only_that_stage() {
    let kernel = kernel();
    kernel
        .execute("echo upstream-marker | sh -c 'echo first; sleep 2; echo second' &")
        .await
        .expect("spawn failed");
    let id = JobId(1);

    let deadline = Instant::now() + LIVE_TIMEOUT;
    loop {
        let status = status_of(&kernel, id).await;
        let out = stdout_of(&kernel, id).await;
        if out.contains("first") {
            assert_eq!(status, "running", "the last stage only filled the node at completion");
            assert!(!out.contains("second"), "whole buffer at once: {out:?}");
            break;
        }
        assert_eq!(status, "running", "pipeline finished before anything was readable");
        assert!(Instant::now() < deadline, "the last stage produced no live output");
        tokio::time::sleep(Duration::from_millis(20)).await;
    }

    wait_done(&kernel, id).await;
    let out = stdout_of(&kernel, id).await;
    assert!(
        !out.contains("upstream-marker"),
        "an upstream stage's output is the next stage's stdin, not the job's stdout: {out:?}"
    );
    assert_eq!(out.matches("first").count(), 1, "written once: {out:?}");
}

/// A pipeline ending in a **builtin** is not live, and the docs say so.
///
/// kaish's `tee`, `cat`, `head`, and friends are builtins: a builtin stage
/// produces its whole output as a value when it returns, so
/// `cargo build 2>&1 | tee build.log &` has nothing to stream chunk by chunk.
/// The output still lands in the node when the job finishes. An embedder that
/// wants a live build log drops the `| tee` and reads `/v/jobs/{id}/stdout`
/// itself — the job's stream is the log.
#[tokio::test]
async fn a_pipeline_ending_in_a_builtin_lands_its_output_at_completion() {
    let kernel = kernel();
    kernel
        .execute("sh -c 'echo one; echo two' | cat &")
        .await
        .expect("spawn failed");
    let id = JobId(1);

    assert_eq!(wait_done(&kernel, id).await, "done:0");
    let out = stdout_of(&kernel, id).await;
    assert!(out.contains("one") && out.contains("two"), "output must still arrive: {out:?}");
    assert_eq!(out.matches("one").count(), 1, "written once: {out:?}");
}

/// A builtin-only job has no byte stream to tee — a builtin produces its
/// output as a value when it returns. Its captured output still has to reach
/// the node, or `echo hi &` would read as an empty job.
#[tokio::test]
async fn a_builtin_only_job_still_lands_its_output_at_completion() {
    let kernel = kernel();
    kernel.execute("echo hello-builtin &").await.expect("spawn failed");
    let id = JobId(1);

    assert_eq!(wait_done(&kernel, id).await, "done:0");
    let out = stdout_of(&kernel, id).await;
    assert!(out.contains("hello-builtin"), "builtin output must reach the node: {out:?}");
    assert_eq!(out.matches("hello-builtin").count(), 1, "written once: {out:?}");
}

/// A killed job keeps whatever it had already streamed — the node is not
/// wiped or left empty by the cancellation path.
#[tokio::test]
async fn a_killed_job_keeps_the_bytes_it_already_streamed() {
    let kernel = kernel();
    kernel
        .execute("sh -c 'echo before-kill; sleep 30' &")
        .await
        .expect("spawn failed");
    let id = JobId(1);

    let deadline = Instant::now() + LIVE_TIMEOUT;
    while !stdout_of(&kernel, id).await.contains("before-kill") {
        assert!(Instant::now() < deadline, "nothing streamed before the kill");
        tokio::time::sleep(Duration::from_millis(20)).await;
    }

    kernel.execute("kill %1").await.expect("kill failed");
    wait_done(&kernel, id).await;

    let out = stdout_of(&kernel, id).await;
    assert!(
        out.contains("before-kill"),
        "a kill must not discard already-streamed output: {out:?}"
    );
}

/// Reading a job that does not exist is `None`, not an empty stream — the
/// caller can tell "no such job" from "nothing written yet".
#[tokio::test]
async fn reading_an_unknown_job_is_none() {
    let kernel = kernel();
    assert!(kernel.jobs().read_stdout(JobId(99)).await.is_none());
    assert!(kernel.jobs().read_stderr(JobId(99)).await.is_none());
}
