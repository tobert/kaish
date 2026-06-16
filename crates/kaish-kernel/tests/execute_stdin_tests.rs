//! Kernel-routed coverage for `ExecuteOptions::with_stdin` — the seam that lets
//! a frontend feed a top-level command's stdin (e.g. `printf … | kaish -c sort`).
//!
//! Before this existed, a bare top-level builtin reading "stdin" (`sort`, `cut`,
//! `wc`) had no input source from a non-interactive frontend and silently
//! produced nothing — exactly the SILENT-WRONG data hazard the builtin sweep
//! hunts. These tests drive real command strings through `kernel.execute_with_
//! options(...)` so the full lex → parse → dispatch → builtin path runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_latch(false).with_trash(false))
        .expect("failed to create kernel")
}

async fn run_with_stdin(prog: &str, stdin: &str) -> (String, i64) {
    let kernel = kernel();
    let result = kernel
        .execute_with_options(prog, ExecuteOptions::new().with_stdin(stdin))
        .await
        .expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

#[tokio::test]
async fn stdin_feeds_a_bare_top_level_builtin() {
    // `sort` with no file argument must read the supplied stdin.
    let (out, code) = run_with_stdin("sort", "10\n2\n1\n20\n").await;
    assert_eq!(code, 0, "sort should succeed: {out:?}");
    assert_eq!(out, "1\n10\n2\n20\n", "sort must consume options stdin");
}

#[tokio::test]
async fn stdin_feeds_first_stage_of_a_pipeline() {
    // The first stage of a top-level pipeline reads the supplied stdin.
    let (out, code) = run_with_stdin("cut -d: -f1", "a:b:c\n").await;
    assert_eq!(code, 0, "cut should succeed: {out:?}");
    assert_eq!(out, "a\n");
}

#[tokio::test]
async fn stdin_is_consumed_by_the_first_command_only() {
    // Shell semantics: stdin is drained by the first reader; a later command
    // that also reads stdin sees nothing (no input source remains).
    let (out, code) = run_with_stdin("cat ; cat", "payload\n").await;
    assert_eq!(code, 0, "both cats should succeed: {out:?}");
    assert_eq!(out, "payload\n", "stdin reaches the first cat only, not a duplicate");
}

#[tokio::test]
async fn stdin_does_not_leak_between_calls() {
    // A second call without stdin must not see the prior call's input.
    let kernel = kernel();
    let first = kernel
        .execute_with_options("cat", ExecuteOptions::new().with_stdin("first\n"))
        .await
        .expect("first execute");
    assert_eq!(first.text_out(), "first\n");

    let second = kernel.execute("echo done").await.expect("second execute");
    assert_eq!(second.text_out(), "done\n", "prior stdin must not bleed into a later call");
}
