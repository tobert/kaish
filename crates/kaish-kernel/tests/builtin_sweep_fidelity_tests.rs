//! Kernel-routed fidelity fixes from the builtin sweep (docs/builtin-sweep-
//! overhaul.md, P2/P3). Each asserts kaish output == the panel's banked
//! consensus, driven through `kernel.execute_with_options(...)` so the full
//! lex → parse → dispatch → builtin path runs (not a builtin's `.execute()`).
//!
//! Oracle = model-consensus, NOT GNU (`[[model-memory-over-gnu-oracle]]`).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_latch(false).with_trash(false))
        .expect("failed to create kernel")
}

async fn run(prog: &str, stdin: &str) -> (String, i64) {
    let kernel = kernel();
    let result = kernel
        .execute_with_options(prog, ExecuteOptions::new().with_stdin(stdin))
        .await
        .expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

// ─────────────────────────── tail -n +N (P2.2) ───────────────────────────
// `tail -n +N` starts *at* line N (1-based), not "last N lines". The leading
// `+` was lost (clap/`parse` strips it), so `+2` was silently treated as `2`.

#[tokio::test]
async fn tail_plus_n_starts_from_line_n() {
    let (out, code) = run("tail -n +2", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "beta\ncherry\ndate\n", "tail -n +2 = from line 2 to end");
}

#[tokio::test]
async fn tail_plus_one_is_whole_input() {
    let (out, code) = run("tail -n +1", "alpha\nbeta\ncherry\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "alpha\nbeta\ncherry\n", "tail -n +1 = entire input");
}

#[tokio::test]
async fn tail_plus_n_past_end_is_empty() {
    let (out, code) = run("tail -n +9", "alpha\nbeta\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "", "start past EOF yields nothing");
}

#[tokio::test]
async fn tail_negative_n_still_means_last_n() {
    // Regression guard: the plain "last N" form must be unaffected.
    let (out, code) = run("tail -n 2", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "cherry\ndate\n", "tail -n 2 = last 2 lines");
}

// ─────────────────────────── head -n -N (P2.3) ───────────────────────────
// `head -n -N` prints all lines BUT the last N. The negative count used to
// wrap (`-1 as usize`) and emit everything.

#[tokio::test]
async fn head_negative_n_drops_last_n() {
    let (out, code) = run("head -n -1", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "alpha\nbeta\ncherry\n", "head -n -1 = all but last line");
}

#[tokio::test]
async fn head_negative_n_dropping_all_is_empty() {
    let (out, code) = run("head -n -9", "alpha\nbeta\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "", "dropping more than present yields nothing");
}

#[tokio::test]
async fn head_positive_n_still_means_first_n() {
    // Regression guards: explicit `-n N` and POSIX shorthand `-N` unaffected.
    let (out, code) = run("head -n 2", "alpha\nbeta\ncherry\ndate\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "alpha\nbeta\n", "head -n 2 = first 2 lines");

    let (out2, code2) = run("head -2", "alpha\nbeta\ncherry\n").await;
    assert_eq!(code2, 0, "out={out2:?}");
    assert_eq!(out2, "alpha\nbeta\n", "head -2 shorthand = first 2 lines");
}

// ─────────────────── cut: line with no delimiter (P2.4) ───────────────────
// `cut -f` (no `-s`) passes a line that lacks the delimiter through unchanged;
// it used to emit an empty line. `-s` suppresses non-delimited lines.

#[tokio::test]
async fn cut_passes_through_line_without_delimiter() {
    let (out, code) = run("cut -d, -f2", "a,b,c\nnodelim\nx,y,z\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "b\nnodelim\ny\n", "non-delimited line passes through whole");
}

#[tokio::test]
async fn cut_s_suppresses_line_without_delimiter() {
    let (out, code) = run("cut -s -d, -f2", "a,b,c\nnodelim\nx,y,z\n").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "b\ny\n", "-s drops the non-delimited line entirely");
}

// ─────────────────────── split --limit=N (P2.6) ──────────────────────────
// `--limit=N` caps the result at N fields (N-1 splits); the remainder stays
// intact. It used to produce N+1 fields (`splitn(limit + 1)`).

async fn run_no_stdin(prog: &str) -> (String, i64) {
    let kernel = kernel();
    let result = kernel.execute(prog).await.expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

#[tokio::test]
async fn split_limit_caps_field_count() {
    // 2 fields: first split, remainder intact.
    let (out, code) = run_no_stdin(r#"split "a:b:c:d" ":" --limit=2"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "a\nb:c:d\n", "--limit=2 = at most 2 fields");
}

#[tokio::test]
async fn split_limit_one_is_whole_input() {
    let (out, code) = run_no_stdin(r#"split "a:b:c" ":" --limit=1"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "a:b:c\n", "--limit=1 = the whole string, unsplit");
}

#[tokio::test]
async fn split_no_limit_splits_all() {
    // Regression guard: unlimited split unchanged.
    let (out, code) = run_no_stdin(r#"split "a:b:c:d" ":""#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "a\nb\nc\nd\n", "no limit = split every delimiter");
}

#[tokio::test]
async fn split_limit_whitespace() {
    let (out, code) = run_no_stdin(r#"split "a b c d" --limit=2"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "a\nb c d\n", "whitespace --limit=2 = at most 2 fields");
}
