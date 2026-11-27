//! Captured `1>&2` and `2>&1` interleave stdout and stderr in recorded write
//! order when `StreamOrder` spans are present, falling back to stdout then
//! stderr otherwise. Already-published stderr on a background job stays first.
//! See `docs/LANGUAGE.md`, "Pipes & Redirects".

#![cfg(all(feature = "localfs", feature = "subprocess"))]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;

const BOTH: &str = "both() { echo out; echo err >&2; echo out2; echo err2 >&2; }\n";

/// The text a script merged into stderr (`1>&2`) and into stdout (`2>&1`).
async fn merged(command: &str) -> (String, String) {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("good"), "good\n").unwrap();
    let kernel = kernel_at(dir.path());
    let to_stderr = kernel.execute(&format!("{BOTH}{command} 1>&2")).await.unwrap();
    let to_stdout = kernel.execute(&format!("{BOTH}{command} 2>&1")).await.unwrap();
    assert_eq!(to_stderr.text_out(), "", "`1>&2` leaves nothing on stdout");
    assert_eq!(to_stdout.err, "", "`2>&1` leaves nothing on stderr");
    (to_stderr.err, to_stdout.text_out().into_owned())
}

#[tokio::test]
async fn builtin_stdout_precedes_its_stderr_under_1_to_2() {
    let (err, out) = merged("ls good nosuch").await;
    assert!(err.starts_with("good"), "stdout block first: {err:?}");
    assert!(err.contains("nosuch"), "{err:?}");
    assert_eq!(err, out, "`1>&2` and `2>&1` join the blocks alike");
}

#[tokio::test]
async fn function_merge_preserves_write_order_under_1_to_2_and_2_to_1() {
    let (err, out) = merged("both").await;
    assert_eq!(err, "out\nerr\nout2\nerr2\n");
    assert_eq!(out, err, "`1>&2` and `2>&1` preserve write order alike");
}

#[tokio::test]
async fn job_stream_holds_stdout_before_stderr_under_1_to_2() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("good"), "good\n").unwrap();
    let kernel = kernel_at(dir.path());
    kernel.execute("ls good nosuch 1>&2 &").await.unwrap();
    let id = kaish_kernel::scheduler::JobId(1);
    kernel.jobs().wait(id).await.unwrap();
    let stream = String::from_utf8_lossy(&kernel.jobs().read_stderr(id).await.unwrap()).into_owned();
    assert!(stream.starts_with("good"), "stdout block first: {stream:?}");
    assert_eq!(stream.matches("nosuch").count(), 1, "{stream:?}");
}

#[tokio::test]
async fn external_job_captured_merge_reaches_stderr_once() {
    let dir = tempfile::tempdir().unwrap();
    let variables = std::collections::HashMap::from([
        ("PATH".to_string(), kaish_kernel::ast::Value::String(std::env::var("PATH").expect("PATH"))),
    ]);
    let kernel = kaish_kernel::Kernel::new(
        kaish_kernel::KernelConfig::repl().with_cwd(dir.path().to_path_buf()).with_initial_vars(variables),
    ).unwrap();
    kernel.execute("sh -c 'echo out; echo err >&2' 1>&2 &").await.unwrap();
    let id = kaish_kernel::scheduler::JobId(1);
    let result = kernel.jobs().wait(id).await.unwrap();
    assert_eq!(result.code, 0);
    // Two OS pipes can deliver back-to-back writes in either order; the
    // contract here is that both lines land once and result.err matches the
    // job's stderr stream verbatim.
    assert!(
        result.err == "out\nerr\n" || result.err == "err\nout\n",
        "expected both lines once, got {:?}",
        result.err
    );
    let stderr = String::from_utf8(kernel.jobs().read_stderr(id).await.unwrap()).unwrap();
    assert_eq!(stderr, result.err);
    assert!(kernel.jobs().read_stdout(id).await.unwrap().is_empty());
}

#[tokio::test]
async fn merge_into_stderr_leaves_pipeline_stdout_empty() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(&format!("{BOTH}both 1>&2 | cat")).await.unwrap();
    assert_eq!(result.code, 0);
    assert_eq!(result.text_out(), "");
    assert_eq!(result.err, "out\nerr\nout2\nerr2\n");
}

#[tokio::test]
async fn copying_stderr_before_redirecting_it_keeps_distinct_targets() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(&format!("{BOTH}both 1>&2 2> errors")).await.unwrap();
    assert_eq!(result.code, 0);
    assert_eq!(result.text_out(), "");
    assert_eq!(result.err, "out\nout2\n");
    assert_eq!(std::fs::read_to_string(dir.path().join("errors")).unwrap(), "err\nerr2\n");
}
