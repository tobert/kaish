//! Kernel-routed tests for runtime output-limit control.
//!
//! These pin the bug cluster from docs/issues.md (2026-06-09): every
//! documented way to adjust the output limit at runtime used to fail —
//! `set -o output-limit=SIZE` was a parse error, `kaish-output-limit set N`
//! collided with the `set` keyword token, and even the paths that parsed
//! didn't persist past the current statement. They drive real command
//! strings through `kernel.execute()` so the whole lex → parse → dispatch →
//! builtin pipeline runs, not just the builtin's arg struct.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig, OutputLimitConfig};

/// A kernel that starts with no limit but truncates in memory once one is set
/// (no disk spill, so the test leaves nothing on the host filesystem).
fn mem_kernel() -> Kernel {
    let config = KernelConfig::repl().with_output_limit(OutputLimitConfig::none().in_memory());
    Kernel::new(config).expect("failed to create kernel")
}

async fn run(kernel: &Kernel, script: &str) -> (String, i64) {
    let result = kernel.execute(script).await.expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

// ── `set` usable as a bareword argument (subcommand-collision fix) ──

#[tokio::test]
async fn echo_set_prints_the_word_set() {
    // `set` is a keyword, but in argument position it is the literal word.
    // Before the fix this parsed as two statements (`echo` then `set`) and
    // printed an empty line.
    let kernel = mem_kernel();
    let (out, code) = run(&kernel, "echo set").await;
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "set");
}

#[tokio::test]
async fn echo_word_set_word_keeps_set_inline() {
    let kernel = mem_kernel();
    let (out, code) = run(&kernel, "echo before set after").await;
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "before set after");
}

#[tokio::test]
async fn kaish_output_limit_set_subcommand_parses_and_applies() {
    // `kaish-output-limit set 1K` — the `set` keyword used to hijack the
    // subcommand position and make this a parse error.
    let kernel = mem_kernel();
    let (out, code) = run(&kernel, "kaish-output-limit set 1K").await;
    assert_eq!(code, 0, "should parse and run; got: {out}");
    assert!(out.contains("1K"), "limit should read back as 1K: {out}");
}

// ── `set -o output-limit=SIZE` parses (the documented form) ──

#[tokio::test]
async fn set_o_output_limit_with_size_parses() {
    let kernel = mem_kernel();
    let (out, code) = run(&kernel, "set -o output-limit=4K").await;
    assert_eq!(code, 0, "documented form must parse; got: {out}");
}

#[tokio::test]
async fn set_o_output_limit_with_raw_bytes_parses() {
    let kernel = mem_kernel();
    let (out, code) = run(&kernel, "set -o output-limit=65536").await;
    assert_eq!(code, 0, "raw-byte form must parse; got: {out}");
}

#[tokio::test]
async fn set_o_output_limit_quoted_parses() {
    let kernel = mem_kernel();
    let (out, code) = run(&kernel, "set -o \"output-limit=4K\"").await;
    assert_eq!(code, 0, "quoted form must parse; got: {out}");
}

// ── Persistence: a limit set at runtime survives into later execute() calls ──

#[tokio::test]
async fn kaish_output_limit_set_persists_across_execute_calls() {
    let kernel = mem_kernel();

    // First call: install a limit (1K — large enough that the readback table
    // below isn't itself truncated, small enough that `seq` blows past it).
    let (_, code) = run(&kernel, "kaish-output-limit set 1K").await;
    assert_eq!(code, 0);

    // Second call: the show subcommand must still report the limit.
    let (out, code) = run(&kernel, "kaish-output-limit").await;
    assert_eq!(code, 0);
    assert!(out.contains("1K"), "limit must persist across calls: {out}");

    // Third call: a large output is actually truncated (exit 3).
    let (out, code) = run(&kernel, "seq 1 5000").await;
    assert_eq!(code, 3, "persisted limit must truncate later output; got: {out}");
    assert!(out.contains("truncated"), "expected truncation marker: {out}");
}

#[tokio::test]
async fn set_o_output_limit_persists_and_truncates() {
    let kernel = mem_kernel();

    let (_, code) = run(&kernel, "set -o output-limit=64").await;
    assert_eq!(code, 0);

    let (out, code) = run(&kernel, "seq 1 5000").await;
    assert_eq!(code, 3, "set -o output-limit must persist and truncate; got: {out}");
    assert!(out.contains("truncated"), "expected truncation marker: {out}");
}

#[tokio::test]
async fn set_plus_o_output_limit_disables_and_persists() {
    let kernel = mem_kernel();

    run(&kernel, "set -o output-limit=64").await;
    let (_, code) = run(&kernel, "set +o output-limit").await;
    assert_eq!(code, 0);

    // With the limit cleared, large output flows through untruncated.
    let (out, code) = run(&kernel, "seq 1 5000").await;
    assert_eq!(code, 0, "limit was disabled, should not truncate; got code {code}");
    assert!(!out.contains("truncated"), "should not be truncated: {out}");
    assert!(out.contains("5000"), "full output expected: tail missing");
}
