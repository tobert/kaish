//! A builtin pipeline ends when it should, on the current-thread runtime an
//! embedder like kaibo drives the kernel on.
//!
//! Two hazards, both found through kaibo, where a model ran
//! `xxd FILE | grep -m 6 -B2 -A2 PATTERN` and the call never returned:
//!
//! - A stage that exits before reading all of its input must break the pipe
//!   for the stage writing to it, the way `head -1` does. Otherwise the writer
//!   waits forever on a full pipe and the pipeline never finishes.
//! - `request_timeout` must stop the work, not only label the result 124 after
//!   the work finishes on its own.
//!
//! Every case runs under an outer `tokio::time::timeout`, so a regression fails
//! in seconds instead of hanging the test binary.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::time::{Duration, Instant};

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};

/// Well past what any case below needs when kaish behaves.
const HANG: Duration = Duration::from_secs(10);

async fn run(config: KernelConfig, script: &str) -> (ExecResult, Duration) {
    let kernel = Kernel::new(config).expect("kernel");
    let started = Instant::now();
    let result = tokio::time::timeout(HANG, kernel.execute(script))
        .await
        .unwrap_or_else(|_| panic!("`{script}` did not return within {HANG:?}"))
        .expect("execute");
    (result, started.elapsed())
}

// More than one pipe buffer (64 KiB) of `seq` output, so the writer must block
// if its reader stops reading.
const MANY_LINES: &str = "seq 1 100000";

#[tokio::test]
async fn a_reader_that_exits_early_ends_the_pipeline() {
    let (r, _) = run(KernelConfig::isolated(), &format!("{MANY_LINES} | head -1")).await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    assert_eq!(r.text_out().trim(), "1");
}

#[tokio::test]
async fn a_reader_that_fails_before_reading_ends_the_pipeline() {
    // `grep` refuses the unknown flag before it reads a byte. The pipeline takes
    // grep's status, as bash does: `seq 1 100000 | grep --bogus x; echo $?` → 2.
    let (r, _) = run(
        KernelConfig::isolated(),
        &format!("{MANY_LINES} | grep --no-such-flag x"),
    )
    .await;
    assert_eq!(r.code, 2, "grep's usage error is the pipeline's status; err: {}", r.err);
}

#[tokio::test]
async fn grep_max_count_stops_reading_and_ends_the_pipeline() {
    let (r, _) = run(KernelConfig::isolated(), &format!("{MANY_LINES} | grep --max-count 1 5")).await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    assert_eq!(r.text_out().trim(), "5");
}

#[tokio::test]
async fn request_timeout_stops_a_busy_pipeline() {
    // Tens of seconds of work in a debug build; the deadline is 200ms.
    let config = KernelConfig::isolated().with_request_timeout(Duration::from_millis(200));
    let (r, took) = run(config, "seq 1 50000000 | wc -l").await;
    assert_eq!(r.code, 124, "err: {}", r.err);
    assert!(
        took < Duration::from_secs(3),
        "the timeout stops the work; it does not wait for it to finish (took {took:?})"
    );
}

#[tokio::test]
async fn request_timeout_stops_a_busy_loop() {
    let config = KernelConfig::isolated().with_request_timeout(Duration::from_millis(200));
    let (r, took) = run(config, "while true; do :; done").await;
    assert_eq!(r.code, 124, "err: {}", r.err);
    assert!(took < Duration::from_secs(3), "took {took:?}");
}

#[tokio::test]
async fn request_timeout_stops_a_busy_builtin_without_a_pipe() {
    let config = KernelConfig::isolated().with_request_timeout(Duration::from_millis(200));
    let (r, took) = run(config, "seq 1 50000000").await;
    assert_eq!(r.code, 124, "err: {}", r.err);
    assert!(took < Duration::from_secs(3), "took {took:?}");
}
