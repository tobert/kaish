//! `stat` accepts multiple file operands (the 2026-05-28 multi-positional
//! sweep), so a one-shot metadata rip works: `stat a b c` / `stat --json a b c`.
//! Regression guard against a relapse to the old single-positional behavior
//! (which joined the list into one path and errored "not found").

#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use std::fs;

#[tokio::test]
async fn stat_lists_multiple_files() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), b"aa").unwrap();
    fs::write(tmp.path().join("b.txt"), b"bbbb").unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "stat a.txt b.txt").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("a.txt"), "got: {out}");
    assert!(out.contains("b.txt"), "got: {out}");
}

#[tokio::test]
async fn stat_json_emits_one_object_per_file() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), b"aa").unwrap();
    fs::write(tmp.path().join("b.txt"), b"bbbb").unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "stat a.txt b.txt --json").await;
    assert_eq!(code, 0, "got: {out}");
    let parsed: serde_json::Value = serde_json::from_str(&out).expect("stat --json is JSON");
    let arr = parsed.as_array().expect("stat --json is an array for multiple files");
    assert_eq!(arr.len(), 2, "one object per file: {out}");
    let files: Vec<&str> = arr.iter().filter_map(|o| o["FILE"].as_str()).collect();
    assert!(files.contains(&"a.txt") && files.contains(&"b.txt"), "got: {files:?}");
}

#[tokio::test]
async fn stat_reports_missing_operand_loudly() {
    // A missing operand among several is reported on stderr with a nonzero exit,
    // while the readable ones still stat — same fail-loud contract as grep/ls.
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), b"aa").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel.execute("stat a.txt nope.txt --json").await.expect("execute");
    assert_eq!(result.code, 1, "missing operand → exit 1: {:?}", result.err);
    assert!(result.err.contains("nope.txt"), "stderr should name it: {:?}", result.err);
    // The readable operand is still stat'd (proves stat continued past the
    // error rather than aborting on the whole batch).
    assert!(
        result.text_out().contains("a.txt"),
        "readable operand should still be listed: {}",
        result.text_out()
    );
}
