//! Kernel-routed tests for `grep -x` (match whole lines only).

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::kernel_at;
use tempfile::tempdir;

async fn grep(args: &str, content: &str) -> (String, String, i64) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f"), content).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(&format!("grep {args} f")).await.unwrap();
    (result.text_out().to_string(), result.err.clone(), result.code)
}

const LINES: &str = "foo\nfoo bar\nbarfoo\nfoo\nFOO\n";

#[tokio::test]
async fn grep_x_matches_whole_lines_only() {
    let (out, err, code) = grep("-x foo", LINES).await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out, "foo\nfoo\n");
}

#[tokio::test]
async fn grep_x_long_spelling() {
    let (out, err, code) = grep("--line-regexp foo", LINES).await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out, "foo\nfoo\n");
}

#[tokio::test]
async fn grep_x_with_line_numbers() {
    let (out, _, code) = grep("-nx foo", LINES).await;
    assert_eq!(code, 0);
    assert_eq!(out, "1:foo\n4:foo\n");
}

#[tokio::test]
async fn grep_x_ignore_case() {
    let (out, _, code) = grep("-xi foo", LINES).await;
    assert_eq!(code, 0);
    assert_eq!(out, "foo\nfoo\nFOO\n");
}

#[tokio::test]
async fn grep_x_inverted() {
    let (out, _, code) = grep("-xv foo", LINES).await;
    assert_eq!(code, 0);
    assert_eq!(out, "foo bar\nbarfoo\nFOO\n");
}

#[tokio::test]
async fn grep_x_counts() {
    let (out, _, code) = grep("-xc foo", LINES).await;
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "2");
}

#[tokio::test]
async fn grep_x_no_match_exits_one() {
    let (out, _, code) = grep("-x fo", LINES).await;
    assert_eq!(code, 1);
    assert_eq!(out, "");
}

#[tokio::test]
async fn grep_x_keeps_alternation_inside_the_line_anchors() {
    let (out, _, code) = grep("-Ex 'foo|bar'", "foo\nbar\nfoobar\n").await;
    assert_eq!(code, 0);
    assert_eq!(out, "foo\nbar\n");
}

#[tokio::test]
async fn grep_x_fixed_string() {
    let (out, _, code) = grep("-xF 'a.b'", "a.b\naxb\na.bc\n").await;
    assert_eq!(code, 0);
    assert_eq!(out, "a.b\n");
}

#[tokio::test]
async fn grep_x_wins_over_w() {
    let (out, _, code) = grep("-xw foo", LINES).await;
    assert_eq!(code, 0);
    assert_eq!(out, "foo\nfoo\n");
}

#[tokio::test]
async fn grep_x_on_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f"), LINES).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("cat f | grep -x foo").await.unwrap();
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out(), "foo\nfoo\n");
}
