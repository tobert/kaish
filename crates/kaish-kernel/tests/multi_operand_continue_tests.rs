//! A failing operand does not stop a multi-operand reader: the other
//! operands still produce output in order, the failure is named on stderr,
//! and the exit code is nonzero. Reference: GNU coreutils (`good nosuch good`).

#![cfg(feature = "localfs")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;
use std::fs;

/// Returns (stdout, stderr, exit code) for `script` in a directory holding
/// `good` ("one\ntwo\n").
async fn run_with_good(script: &str) -> (String, String, i64) {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("good"), b"one\ntwo\n").unwrap();
    let kernel = kernel_at(tmp.path());
    let r = kernel.execute(script).await.expect("execute");
    (r.text_out().into_owned(), r.err.clone(), r.code)
}

#[tokio::test]
async fn cat_continues_past_a_missing_operand() {
    let (out, err, code) = run_with_good("cat good nosuch good").await;
    assert_eq!(out, "one\ntwo\none\ntwo\n");
    assert!(err.contains("cat: nosuch"), "stderr names the path: {err}");
    assert!(!err.contains("good"), "only the failure is reported: {err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn cat_missing_operand_first_and_last() {
    let (out, err, code) = run_with_good("cat nosuch good nosuch2").await;
    assert_eq!(out, "one\ntwo\n");
    assert!(err.contains("cat: nosuch:") && err.contains("cat: nosuch2:"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn cat_numbered_lines_continue_past_a_missing_operand() {
    let (out, err, code) = run_with_good("cat -n good nosuch good").await;
    assert_eq!(out, "     1\tone\n     2\ttwo\n     3\tone\n     4\ttwo\n");
    assert!(err.contains("cat: nosuch"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn cat_all_operands_missing_prints_nothing_and_fails() {
    let (out, err, code) = run_with_good("cat nosuch nosuch2").await;
    assert_eq!(out, "");
    assert!(err.contains("nosuch:") && err.contains("nosuch2:"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn head_continues_past_a_missing_operand() {
    let (out, err, code) = run_with_good("head -n 1 good nosuch good").await;
    assert_eq!(out, "==> good <==\none\n\n==> good <==\none");
    assert!(err.contains("head: nosuch"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn head_missing_operand_first_leaves_no_leading_blank_line() {
    let (out, err, code) = run_with_good("head -n 1 nosuch good good").await;
    assert_eq!(out, "==> good <==\none\n\n==> good <==\none");
    assert!(err.contains("head: nosuch"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn tail_continues_past_a_missing_operand() {
    let (out, err, code) = run_with_good("tail -n 1 good nosuch good").await;
    assert_eq!(out, "==> good <==\ntwo\n\n==> good <==\ntwo");
    assert!(err.contains("tail: nosuch"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn tail_missing_operand_first_leaves_no_leading_blank_line() {
    let (out, err, code) = run_with_good("tail -n 1 nosuch good good").await;
    assert_eq!(out, "==> good <==\ntwo\n\n==> good <==\ntwo");
    assert!(err.contains("tail: nosuch"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn tac_continues_past_a_missing_operand() {
    let (out, err, code) = run_with_good("tac good nosuch good").await;
    assert_eq!(out, "two\none\ntwo\none\n");
    assert!(err.contains("tac: nosuch"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn cut_continues_past_a_missing_operand() {
    let (out, err, code) = run_with_good("cut -c1 good nosuch good").await;
    assert_eq!(out, "o\nt\no\nt\n");
    assert!(err.contains("cut: nosuch"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn file_continues_past_a_missing_operand() {
    let (out, err, code) = run_with_good("file good nosuch good").await;
    assert_eq!(out.lines().count(), 2, "two good lines: {out}");
    assert!(out.lines().all(|l| l.starts_with("good:")), "{out}");
    assert!(err.contains("file: nosuch"), "{err}");
    assert_eq!(code, 1);
}
