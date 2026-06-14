//! Explicit operand errors are reported, not silently skipped (POSIX fail-loud).
//!
//! `grep p real.txt typo.txt` and `ls real.txt gone.txt` used to drop the
//! unreadable/missing operand without a word, hiding typos. They now report
//! the bad operand on stderr and exit nonzero, while the readable operands
//! still produce their normal output.

#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use std::fs;

#[tokio::test]
async fn grep_reports_missing_explicit_operand() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("real.txt"), "hello\nworld\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("grep hello real.txt typo.txt")
        .await
        .expect("execute");
    // Match still found in the readable file...
    assert!(result.text_out().contains("hello"), "got: {}", result.text_out());
    // ...but the missing operand is loud: stderr names it, exit is nonzero.
    assert!(
        result.err.contains("typo.txt"),
        "stderr should name the missing operand: {:?}",
        result.err
    );
    assert_eq!(result.code, 2, "file error → exit 2");
}

#[tokio::test]
async fn grep_recursive_does_not_error_on_race() {
    // The recursive walk must keep its benign-race tolerance: a normal `grep -r`
    // over a real tree exits on match/no-match, never 2 for "missing".
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), "needle\n").unwrap();
    fs::write(tmp.path().join("b.txt"), "haystack\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel.execute("grep -r needle .").await.expect("execute");
    assert_eq!(result.code, 0, "match found, no spurious error: {:?}", result.err);
    assert!(result.text_out().contains("needle"));
}

#[tokio::test]
async fn ls_reports_inaccessible_explicit_operand() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("real.txt"), "x").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel.execute("ls real.txt gone.txt").await.expect("execute");
    // Readable operand still listed...
    assert!(
        result.text_out().contains("real.txt"),
        "got: {}",
        result.text_out()
    );
    // ...missing one reported loudly.
    assert!(
        result.err.contains("gone.txt") && result.err.contains("cannot access"),
        "stderr should name the inaccessible operand: {:?}",
        result.err
    );
    assert_ne!(result.code, 0, "inaccessible operand → nonzero exit");
}

#[tokio::test]
async fn ls_glob_tolerates_no_match_race() {
    // A glob that expands to real files lists them with exit 0 — the glob path
    // keeps skip-on-race semantics and does not synthesize operand errors.
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.log"), "x").unwrap();
    fs::write(tmp.path().join("b.log"), "y").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel.execute("ls *.log").await.expect("execute");
    assert_eq!(result.code, 0, "glob listing should succeed: {:?}", result.err);
    assert!(result.text_out().contains("a.log"));
    assert!(result.text_out().contains("b.log"));
}
