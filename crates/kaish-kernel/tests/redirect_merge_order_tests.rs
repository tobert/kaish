//! Captured `1>&2` joins stdout and stderr as two blocks, stdout first,
//! the order `2>&1` already uses. Already-published stderr stays first. Neither follows write order inside one
//! command; `docs/LANGUAGE.md`, "Pipes & Redirects" states the limit.

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
async fn function_stdout_precedes_its_stderr_under_1_to_2() {
    let (err, out) = merged("both").await;
    // bash writes `out err out2 err2` (write order). kaish joins blocks.
    assert_eq!(err, "out\nout2\nerr\nerr2\n");
    assert_eq!(out, err, "`1>&2` and `2>&1` join the blocks alike");
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
    assert_eq!(result.err, "out\nerr\n");
    let stderr = String::from_utf8(kernel.jobs().read_stderr(id).await.unwrap()).unwrap();
    assert_eq!(stderr, "out\nerr\n");
    assert!(kernel.jobs().read_stdout(id).await.unwrap().is_empty());
}

#[tokio::test]
async fn merge_into_stderr_leaves_pipeline_stdout_empty() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(&format!("{BOTH}both 1>&2 | cat")).await.unwrap();
    assert_eq!(result.code, 0);
    assert_eq!(result.text_out(), "");
    assert_eq!(result.err, "out\nout2\nerr\nerr2\n");
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
