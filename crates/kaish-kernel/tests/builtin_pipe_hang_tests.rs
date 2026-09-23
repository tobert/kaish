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
//! instead of hanging the test binary. A builtin that never yields holds the
//! thread past that timeout too, so its failure arrives when the builtin
//! finishes: about a minute for the `seq` cases in a debug build.
//!
//! `request_timeout_stops_a_busy_builtin` and `request_timeout_stops_a_busy_diff`
//! extend the second hazard past `seq`: a builtin with a loop whose length
//! depends on input size calls `ToolCtx::checkpoint` (kaish-tool-api) once per
//! pass, so `request_timeout` stops it mid-scan. With the kernel's `checkpoint`
//! made inert, every one of those cases fails, as do the two `seq` timeout
//! cases above. `cat` and `cmp` have no case; the comment above `diff` says why.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::time::{Duration, Instant};

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};
use rstest::rstest;

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
    // Passes without the pipeline.rs fix (item 2): `head` drops its own
    // reader once it has its line, so the writer sees a broken pipe
    // regardless of when `pipe_stdin` is cleared.
    let (r, _) = run(KernelConfig::isolated(), &format!("{MANY_LINES} | head -1")).await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    assert_eq!(r.text_out().trim(), "1");
}

#[tokio::test]
async fn a_reader_that_fails_before_reading_ends_the_pipeline() {
    // `grep` refuses the unknown flag before it reads a byte. The pipeline takes
    // grep's status, as bash does: `seq 1 100000 | grep --bogus x; echo $?` → 2.
    // This is the case that pins the pipeline.rs fix (item 2): `grep` never
    // reads its pipe at all, so the writer only unblocks once THIS stage's
    // reader is dropped after it finishes (redirects included).
    let (r, _) = run(
        KernelConfig::isolated(),
        &format!("{MANY_LINES} | grep --no-such-flag x"),
    )
    .await;
    assert_eq!(r.code, 2, "grep's usage error is the pipeline's status; err: {}", r.err);
}

#[tokio::test]
async fn grep_max_count_stops_reading_and_ends_the_pipeline() {
    // Passes without the pipeline.rs fix (item 2): streaming grep drops its
    // own reader once `--max-count` is reached, the same as `head` above.
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

// ============================================================================
// ToolCtx::checkpoint — a busy builtin (not just a busy loop or a busy pipe)
// stops at the timeout
// ============================================================================
//
// The `seq` case above is one instance of the bug `ToolCtx::checkpoint`
// (kaish-tool-api) exists to fix: a builtin loop over input-sized data that
// never awaits holds its thread past `request_timeout`, since the watchdog
// task that would set the cancel token cannot run until the loop ends. This
// batch is every other builtin checkpointed for the same reason.
//
// The fixture is capped at 2,000,000 lines (~15MB) and stays there. Growing
// it to force every case past a loose bound was tried and reverted: kaish
// holds the file in an in-memory VFS, several of these builtins (`sort`,
// `tac`, `uniq`, `base64`, `xxd`) collect the whole input into memory too,
// and a bigger file multiplies across whatever runs in parallel — a run
// against a much larger file was killed for memory on the machine this
// suite runs on. The fix instead: keep the input fixed and assert a bound
// close to the deadline, not a loose flat one, so a case that finishes
// under a loose bound without ever being interrupted still fails a tight
// one. A case whose natural (uninterrupted) runtime at 2,000,000 lines
// does not clear `RELABEL_BOUND` cannot prove anything at this input size
// either way, and is dropped rather than kept on a bound loose enough to
// pass on a natural finish — see the comment above the case list below for
// which builtins that is and their measured natural runtimes.
const HANG_DEADLINE: Duration = Duration::from_millis(50);

/// The bound every case below asserts `took` against — `HANG_DEADLINE` plus
/// a second of headroom for dispatch/scheduling overhead on a loaded
/// machine, not the flat 3s bound this suite used to share everywhere.
const RELABEL_BOUND: Duration = Duration::from_millis(1050);

/// Run `script` against `kernel` under `HANG_DEADLINE`, wrapped in the
/// module's outer `HANG` timeout so a regression fails instead of hanging.
async fn run_with_deadline(kernel: &Kernel, script: &str) -> (ExecResult, Duration) {
    let started = Instant::now();
    let result = tokio::time::timeout(
        HANG,
        kernel.execute_with_options(
            script,
            ExecuteOptions {
                timeout: Some(HANG_DEADLINE),
                ..Default::default()
            },
        ),
    )
    .await
    .unwrap_or_else(|_| panic!("`{script}` did not return within {HANG:?}"))
    .expect("execute");
    (result, started.elapsed())
}

/// A fresh isolated kernel with `/tmp/big.txt` populated with 2,000,000 lines
/// (`seq 1 2000000`) — built *without* a timeout, since the setup itself must
/// complete; only the case under test runs against `HANG_DEADLINE`.
async fn kernel_with_big_file() -> Kernel {
    let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let setup = kernel
        .execute("seq 1 2000000 > /tmp/big.txt")
        .await
        .expect("execute");
    assert_eq!(setup.code, 0, "setup must succeed: {}", setup.err);
    kernel
}

// Every builtin whose whole-file or whole-stdin scan, measured *without* a
// timeout at 2,000,000 lines, finishes fast enough that `RELABEL_BOUND`
// cannot distinguish a genuine interrupt from a natural finish. Dropped
// rather than kept on a looser bound that would pass without the checkpoint
// ever firing (proving relabeling, per this file's own module doc): `grep
// FILE` 335ms, `sort` 493ms, `wc -l FILE` 154ms, `uniq` 796ms, `tac` 362ms,
// `grep -c`/`-B2 -A2`/`< stdin` (the checkpointed whole-buffer path added
// alongside this comment) 47-59ms, `wc -l < stdin` 150ms, `checksum <
// stdin` 214ms. `read_file_chunked` and stdin-chunking (both added this
// same round) still checkpoint these; this file just cannot prove it at a
// 2,000,000-line, memory-safe input size. A bigger, dedicated fixture could
// prove it, at the memory cost this file's history already found
// unacceptable — an open gap, not a claim.
#[rstest]
#[case::xxd("xxd /tmp/big.txt")]
#[case::tr("tr a-z A-Z < /tmp/big.txt")]
#[case::cut("cut -c 1-3 /tmp/big.txt")]
#[case::sed("sed 's/1/9/' /tmp/big.txt")]
#[case::awk("awk '{print $1}' /tmp/big.txt")]
#[case::base64("base64 /tmp/big.txt")]
#[tokio::test]
async fn request_timeout_stops_a_busy_builtin(#[case] script: &str) {
    let kernel = kernel_with_big_file().await;
    let (result, took) = run_with_deadline(&kernel, script).await;
    assert_eq!(result.code, 124, "`{script}`: err: {}", result.err);
    assert!(
        took < RELABEL_BOUND,
        "`{script}` took {took:?}; the timeout stops the work, it does not wait for it to finish"
    );
}

// `cat` and `cmp` have no case here. `cat` checkpoints only its piped paths:
// its single-file path keeps the unranged `backend.read`, because a chunked
// read never ends on an endless device like `/dev/zero`, and the unranged read
// is what refuses one. `cmp` checkpoints per chunk pair, but it reached 124
// here even with the checkpoint made inert, so a case could not fail for the
// reason this file tests.

/// `diff` needs two files different enough that every line disagrees — two
/// identical files return before the diff computation even starts (the
/// execute()-level fast path), which would prove nothing here. 2,000,000
/// lines each, matching [`kernel_with_big_file`]'s cap.
#[tokio::test]
async fn request_timeout_stops_a_busy_diff() {
    let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let setup = kernel
        .execute("seq 1 2000000 > /tmp/big.txt; seq 2 2000001 > /tmp/big2.txt")
        .await
        .expect("execute");
    assert_eq!(setup.code, 0, "setup must succeed: {}", setup.err);

    let script = "diff /tmp/big.txt /tmp/big2.txt";
    let (result, took) = run_with_deadline(&kernel, script).await;
    assert_eq!(result.code, 124, "err: {}", result.err);
    assert!(took < RELABEL_BOUND, "took {took:?}");
}
