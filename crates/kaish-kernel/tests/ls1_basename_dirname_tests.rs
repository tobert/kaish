//! Kernel-routed POSIX-fidelity tests for three small bugs:
//!   1. `ls -1` treats `-1` as a path operand instead of a flag.
//!   2. `basename //` returns `//` instead of the POSIX-required `/`.
//!   3. `dirname //` returns `.` instead of the POSIX-required `/`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::{kernel_at, run};
use tempfile::tempdir;

fn touch(dir: &std::path::Path, name: &str) {
    fs::write(dir.join(name), b"").expect("write file");
}

// ── ls -1: one entry per line ────────────────────────────────────────────────

/// `ls -1` must be accepted as a flag (one-per-line), not treated as a path.
/// Bug: the lexer emits `-1` as `Int(-1)` (not `ShortFlag("1")`), so it lands
/// in positionals and ls tries to open a file literally named `-1`.
#[tokio::test]
async fn ls_dash_one_is_a_flag_not_a_path() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "alpha.txt");
    touch(dir.path(), "beta.txt");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "ls -1").await;
    assert_eq!(code, 0, "ls -1 should succeed, not error with 'not found': {out:?}");
}

/// With `-1`, every filename must appear on its own line (canonical text output).
/// When the flag is silently treated as a path the output is an error message,
/// not a file list.
#[tokio::test]
async fn ls_dash_one_lists_one_per_line() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "alpha.txt");
    touch(dir.path(), "beta.txt");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "ls -1").await;
    assert_eq!(code, 0, "ls -1 should succeed: {out:?}");
    assert!(out.contains("alpha.txt"), "alpha.txt missing from ls -1 output: {out:?}");
    assert!(out.contains("beta.txt"), "beta.txt missing from ls -1 output: {out:?}");

    // Each name must be on its own line — split by newline, filter empty, all
    // entries must be names we placed there.
    let lines: Vec<&str> = out.lines().filter(|l| !l.is_empty()).collect();
    assert!(lines.len() >= 2, "expected at least 2 lines from ls -1, got: {out:?}");
    for line in &lines {
        // Every non-empty line is exactly one filename (no spaces from multi-column).
        assert!(
            !line.contains("  "),
            "ls -1 output line contains multiple columns (spaces): {line:?}"
        );
    }
}

// ── basename //: POSIX requires `/` ──────────────────────────────────────────

/// POSIX: `basename //` → `/`.
/// The double-slash path consists entirely of separators; `file_name()` returns
/// `None`, and the bug is that the code falls back to the raw string `//`.
#[tokio::test]
async fn basename_double_slash_returns_single_slash() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "basename //").await;
    assert_eq!(code, 0, "basename // should succeed: {out:?}");
    assert_eq!(
        out.trim(),
        "/",
        "basename // must return '/' per POSIX, got {out:?}"
    );
}

/// Sanity-check: `basename /` also returns `/` (already worked before this fix).
#[tokio::test]
async fn basename_single_slash_returns_slash() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "basename /").await;
    assert_eq!(code, 0, "basename / should succeed: {out:?}");
    assert_eq!(out.trim(), "/", "basename / must return '/': {out:?}");
}

// ── dirname //: POSIX requires `/` ───────────────────────────────────────────

/// POSIX: `dirname //` → `/`.
/// `Path::new("//").parent()` returns `None`, so the current code falls through
/// to the empty-parent branch and returns `"."`.
#[tokio::test]
async fn dirname_double_slash_returns_single_slash() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "dirname //").await;
    assert_eq!(code, 0, "dirname // should succeed: {out:?}");
    assert_eq!(
        out.trim(),
        "/",
        "dirname // must return '/' per POSIX, got {out:?}"
    );
}

/// Sanity-check: `dirname /` still returns `/` (already worked before).
#[tokio::test]
async fn dirname_single_slash_returns_slash() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "dirname /").await;
    assert_eq!(code, 0, "dirname / should succeed: {out:?}");
    assert_eq!(out.trim(), "/", "dirname / must return '/': {out:?}");
}

// ── empty-string operand: the all-slash check must not be vacuously true ──────

#[tokio::test]
async fn basename_empty_string_is_empty() {
    // `chars().all('/')` is vacuously true on "" — must not return "/".
    // GNU `basename ""` prints an empty line.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, r#"basename """#).await;
    assert_eq!(code, 0, "{out:?}");
    assert_eq!(out, "", "basename \"\" should be empty, not /: {out:?}");
}

#[tokio::test]
async fn dirname_empty_string_is_dot() {
    // GNU `dirname ""` prints ".".
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, r#"dirname """#).await;
    assert_eq!(code, 0, "{out:?}");
    assert_eq!(out, ".", "dirname \"\" should be ., not /: {out:?}");
}
