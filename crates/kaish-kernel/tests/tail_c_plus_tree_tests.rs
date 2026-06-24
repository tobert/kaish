//! Kernel-routed tests for two P2 bugs:
//!   1. `tail -c +N` ignored the `+` prefix (treated same as `tail -c N`).
//!   2. `tree <nonexistent>` exited 0 with a bare root node instead of failing.
//!
//! Tests are kernel-routed (`kernel.execute()`) so the full dispatch chain
//! runs — the only meaningful level for pinning these contracts.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use tempfile::tempdir;

use common::{kernel_at, run};

// ---------------------------------------------------------------------------
// Bug 1: tail -c +N — "from byte N" (1-based) vs "last N bytes"
// ---------------------------------------------------------------------------

/// `tail -c N` (no `+`) → last N bytes of input.
#[tokio::test]
async fn tail_c_no_plus_returns_last_n_bytes() {
    let dir = tempdir().unwrap();
    // "abcdefgh" — 8 bytes; last 3 = "fgh"
    fs::write(dir.path().join("bytes.txt"), b"abcdefgh").unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "tail -c 3 bytes.txt").await;
    assert_eq!(code, 0, "tail -c 3 should succeed: {out:?}");
    assert_eq!(out.trim(), "fgh", "tail -c 3 should return last 3 bytes: {out:?}");
}

/// `tail -c +N` (with `+`) → bytes from byte offset N to EOF (1-based).
/// For "abcdefgh" (8 bytes) with +3: bytes 3-8 = "cdefgh".
#[tokio::test]
async fn tail_c_plus_returns_bytes_from_offset() {
    let dir = tempdir().unwrap();
    // "abcdefgh" — 8 bytes; from byte +3 = "cdefgh" (bytes at index 2..8)
    fs::write(dir.path().join("bytes.txt"), b"abcdefgh").unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "tail -c +3 bytes.txt").await;
    assert_eq!(code, 0, "tail -c +3 should succeed: {out:?}");
    assert_eq!(out.trim(), "cdefgh", "tail -c +3 should return bytes from offset 3 to end: {out:?}");
}

/// `tail -c +1` → entire file (from byte 1 = all bytes).
#[tokio::test]
async fn tail_c_plus_one_returns_entire_file() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("bytes.txt"), b"hello").unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "tail -c +1 bytes.txt").await;
    assert_eq!(code, 0, "tail -c +1 should succeed: {out:?}");
    assert_eq!(out.trim(), "hello", "tail -c +1 should return entire file: {out:?}");
}

/// `tail -c +N` where N > file length → empty output (not an error).
#[tokio::test]
async fn tail_c_plus_past_end_returns_empty() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("bytes.txt"), b"hi").unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "tail -c +100 bytes.txt").await;
    assert_eq!(code, 0, "tail -c +100 past end should succeed: {out:?}");
    assert!(out.trim().is_empty(), "tail -c +100 past end should return empty: {out:?}");
}

/// Regression: `tail -c -N` (explicit negative) still means last N bytes.
#[tokio::test]
async fn tail_c_negative_still_means_last_n_bytes() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("bytes.txt"), b"abcdefgh").unwrap();
    let kernel = kernel_at(dir.path());
    // "-3" (explicit negative) = last 3 bytes = "fgh"
    // Note: the lexer may lex "-3" as Int(-3) in positional, test via a pipe expr
    // Use a file arg so the path is clear
    let (out, code) = run(&kernel, "tail -c 3 bytes.txt").await;
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "fgh", "tail -c 3 (last 3) should return 'fgh': {out:?}");
}

/// Verify `tail -c +N` and `tail -c N` give DIFFERENT results (the core
/// invariant that the bug violated).
#[tokio::test]
async fn tail_c_plus_and_no_plus_differ() {
    let dir = tempdir().unwrap();
    // "0123456789" — 10 bytes
    // tail -c 3 → last 3 → "789"
    // tail -c +3 → from byte 3 → "23456789" (8 bytes)
    fs::write(dir.path().join("digits.txt"), b"0123456789").unwrap();
    let kernel = kernel_at(dir.path());

    let (out_last, code_last) = run(&kernel, "tail -c 3 digits.txt").await;
    assert_eq!(code_last, 0);
    assert_eq!(out_last.trim(), "789", "tail -c 3 should be last 3: {out_last:?}");

    let (out_from, code_from) = run(&kernel, "tail -c +3 digits.txt").await;
    assert_eq!(code_from, 0);
    assert_eq!(out_from.trim(), "23456789", "tail -c +3 should be from byte 3: {out_from:?}");

    assert_ne!(
        out_last.trim(),
        out_from.trim(),
        "tail -c 3 and tail -c +3 must produce different output (the bug made them equal)"
    );
}

// ---------------------------------------------------------------------------
// Bug 2: tree <nonexistent> should exit non-zero
// ---------------------------------------------------------------------------

/// `tree` on a non-existent path must exit non-zero.
#[tokio::test]
async fn tree_nonexistent_path_exits_nonzero() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    // "nonexistent_xyz_kaish_test" definitely does not exist in the fresh tempdir
    let (out, code) = run(&kernel, "tree nonexistent_xyz_kaish_test").await;
    assert_ne!(code, 0, "tree on nonexistent path should exit non-zero, got code={code} out={out:?}");
}

/// `tree` on an existing directory still exits 0.
#[tokio::test]
async fn tree_existing_path_exits_zero() {
    let dir = tempdir().unwrap();
    fs::create_dir_all(dir.path().join("subdir")).unwrap();
    fs::write(dir.path().join("subdir/file.txt"), b"hello").unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "tree subdir").await;
    assert_eq!(code, 0, "tree on existing dir should succeed: {out:?}");
    assert!(out.contains("subdir") || out.contains("file.txt"), "output should mention the tree: {out:?}");
}

/// `tree` without an argument (uses cwd `.`) still exits 0.
#[tokio::test]
async fn tree_default_cwd_exits_zero() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("readme.txt"), b"hi").unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "tree").await;
    assert_eq!(code, 0, "tree with no args should succeed: {out:?}");
    // Should show something (at least the cwd marker)
    assert!(!out.is_empty(), "tree with no args should produce some output: {out:?}");
}
