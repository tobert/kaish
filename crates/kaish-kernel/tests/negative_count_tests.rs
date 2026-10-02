//! A negative count flag is refused with a named message (exit 2), not wrapped
//! into a huge unsigned count. Each case runs source text through the lexer,
//! parser, argument binder, and clap via `kernel.execute()`.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use rstest::rstest;
use std::fs;
use tempfile::tempdir;

const PATCH: &str = "--- a.txt\n+++ a.txt\n@@ -1 +1 @@\n-one\n+two\n";

#[rstest]
#[case::xxd_length("xxd -l -1 f.txt", "xxd: -l -1: length must be 0 or more")]
#[case::xxd_length_equals("xxd -l=-1 f.txt", "xxd: -l -1: length must be 0 or more")]
#[case::xxd_length_long("xxd --length -1 f.txt", "xxd: -l -1: length must be 0 or more")]
#[case::xxd_seek("xxd -s -1 f.txt", "xxd: -s -1: seeking from the end is not supported")]
#[case::base64_wrap("base64 -w -1 f.txt", "base64: -w -1: wrap width must be 0 or more")]
#[case::base64_wrap_long("base64 --wrap -1 f.txt", "base64: -w -1: wrap width must be 0 or more")]
#[case::diff_context("diff -C -1 f.txt g.txt", "diff: -C -1: context lines must be 0 or more")]
#[case::patch_strip("patch -p -1 < p.diff", "patch: -p -1: strip count must be 0 or more")]
#[tokio::test]
async fn negative_count_is_refused_with_the_fix(#[case] script: &str, #[case] message: &str) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f.txt"), "one\n").unwrap();
    fs::write(dir.path().join("g.txt"), "two\n").unwrap();
    fs::write(dir.path().join("a.txt"), "one\n").unwrap();
    fs::write(dir.path().join("p.diff"), PATCH).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(script).await.expect("kernel execute");
    assert_eq!(result.code, 2, "{script}: out={:?} err={:?}", result.text_out(), result.err);
    assert!(result.err.contains(message), "{script}: err={:?}", result.err);
    assert!(result.text_out().is_empty(), "{script}: wrote output {:?}", result.text_out());
}

#[rstest]
#[case::xxd_length_zero("xxd -l 0 f.txt", "")]
#[case::xxd_length("xxd -p -l 2 f.txt", "6f6e")]
#[case::xxd_seek("xxd -p -s 2 f.txt", "650a")]
#[case::base64_wrap_zero("base64 -w 0 f.txt", "b25lCg==")]
#[case::diff_context_zero("diff -C 0 f.txt g.txt", "")]
#[tokio::test]
async fn valid_count_still_runs(#[case] script: &str, #[case] expected: &str) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f.txt"), "one\n").unwrap();
    fs::write(dir.path().join("g.txt"), "one\n").unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(script).await.expect("kernel execute");
    assert_eq!(result.text_out().trim(), expected, "{script}: err={:?}", result.err);
    assert_eq!(result.code, 0, "{script}: err={:?}", result.err);
}
