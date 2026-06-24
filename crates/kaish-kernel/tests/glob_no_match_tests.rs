//! Kernel-routed tests for the glob builtin's strict-no-match guarantee.
//!
//! The bug: `glob '*.none'` returned exit 0 with empty output instead of
//! a non-zero exit with an error message. kaish's strict-glob contract says
//! zero matches is an error (consistent with how the kernel handles a bare
//! glob in argv position).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use tempfile::tempdir;

use common::{kernel_at, run};

fn touch(dir: &std::path::Path, name: &str, contents: &str) {
    let path = dir.join(name);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent dirs");
    }
    fs::write(path, contents).expect("write file");
}

/// Zero matches must produce a non-zero exit code and an error message.
#[tokio::test]
async fn glob_no_match_exits_nonzero() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "hello\n");
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("glob '*.none'").await.expect("execute");
    assert_ne!(
        result.code, 0,
        "glob with zero matches must exit non-zero, got exit 0 with output: {:?}",
        result.text_out()
    );
    assert!(
        !result.err.is_empty() || result.text_out().contains("no match") || result.code != 0,
        "glob with zero matches must produce a non-zero exit code"
    );
}

/// The error message should mention the pattern so users know what failed.
#[tokio::test]
async fn glob_no_match_error_contains_pattern() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "hello\n");
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("glob '*.none'").await.expect("execute");
    assert_ne!(result.code, 0, "glob with zero matches must exit non-zero");
    // The error output should mention the pattern
    let combined = format!("{} {}", result.err, result.text_out());
    assert!(
        combined.contains("*.none") || combined.contains("no match") || combined.contains("no matches"),
        "error output should mention the pattern or 'no match'; got err={:?} out={:?}",
        result.err,
        result.text_out()
    );
}

/// When there are matches, glob must still succeed (exit 0, paths in output).
#[tokio::test]
async fn glob_with_matches_exits_zero() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "foo.txt", "content\n");
    touch(dir.path(), "bar.txt", "content\n");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "glob '*.txt'").await;
    assert_eq!(code, 0, "glob with matches must exit 0; output: {out:?}");
    assert!(out.contains("foo.txt"), "output should contain foo.txt: {out:?}");
    assert!(out.contains("bar.txt"), "output should contain bar.txt: {out:?}");
}

/// A no-match on a recursive pattern also errors.
#[tokio::test]
async fn glob_recursive_no_match_exits_nonzero() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "main.rs", "fn main() {}\n");
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("glob '**/*.go'").await.expect("execute");
    assert_ne!(
        result.code, 0,
        "recursive glob with zero matches must exit non-zero"
    );
}

/// Null-separated mode (-0) with no matches must also exit non-zero.
#[tokio::test]
async fn glob_null_sep_no_match_exits_nonzero() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "x\n");
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("glob -0 '*.go'").await.expect("execute");
    assert_ne!(
        result.code, 0,
        "glob -0 with zero matches must exit non-zero"
    );
}
