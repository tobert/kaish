//! `1>&2` joins a command's stdout and stderr as two blocks, stdout first,
//! the order `2>&1` already uses. Neither follows write order inside one
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
