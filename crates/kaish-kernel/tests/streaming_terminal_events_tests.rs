//! A streaming caller sees everything the returned result reports.
//!
//! `execute_with_options_streaming` calls `on_output` once per top-level
//! statement, and the REPL `-c` frontend prints only what `on_output`
//! delivers. Output that reaches the returned `ExecResult` but never the
//! callback is output that caller never shows. Each test compares the two.
//!
//! Only builtins run here, so these need no feature gate.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::time::Duration;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{ExecuteOptions, Kernel};

/// Run `program`, returning the stderr `on_output` delivered and the result.
async fn run_streaming(program: &str, opts: ExecuteOptions) -> (String, ExecResult) {
    let kernel = Kernel::transient().expect("kernel");
    let mut streamed_err = String::new();
    let mut on_output = |output: &ExecResult| streamed_err.push_str(&output.err);
    let result = kernel
        .execute_with_options_streaming(program, opts, &mut on_output)
        .await
        .expect("program runs");
    (streamed_err, result)
}

#[tokio::test]
async fn substitution_stderr_streams_from_an_ordinary_statement() {
    // Control: an ordinary statement already streamed its drained stderr.
    let (streamed, result) = run_streaming("echo $(ls /no-such-dir; echo 3)", ExecuteOptions::new()).await;
    assert!(result.err.contains("no-such-dir"), "{result:?}");
    assert_eq!(streamed, result.err);
}

#[tokio::test]
async fn substitution_stderr_streams_from_an_exit_statement() {
    let (streamed, result) = run_streaming("exit $(ls /no-such-dir; echo 3)", ExecuteOptions::new()).await;
    assert_eq!(result.code, 3);
    assert!(result.err.contains("no-such-dir"), "{result:?}");
    assert_eq!(streamed, result.err, "the exit statement's stderr must reach the stream");
}

#[tokio::test]
async fn timeout_is_named_after_earlier_stderr() {
    let (streamed, result) = run_streaming(
        "echo early >&2; sleep 30",
        ExecuteOptions::new().with_timeout(Duration::from_millis(50)),
    )
    .await;
    assert_eq!(result.code, 124);
    let early = result.err.find("early").expect("statement stderr kept");
    let timed_out = result
        .err
        .find("timeout: timed out after")
        .unwrap_or_else(|| panic!("timeout diagnostic missing: {:?}", result.err));
    assert!(early < timed_out, "{:?}", result.err);
    assert_eq!(streamed, result.err, "the timeout diagnostic must reach the stream");
}
