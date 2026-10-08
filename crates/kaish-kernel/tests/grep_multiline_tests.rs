//! `grep -U` lets a pattern match across line boundaries on every input path:
//! a single file, several files, a pipe, and input larger than one 256 KiB
//! (`ExecContext::STREAM_CHUNK_SIZE`) search window.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use std::path::Path;

use common::kernel_at;
use rstest::rstest;
use tempfile::tempdir;

/// `ExecContext::STREAM_CHUNK_SIZE`.
const CHUNK: usize = 256 * 1024;

const SMALL: &str = "alpha\nfoo start\nmiddle\nbar end\nomega\n";

/// A file whose `foo start` line ends just before the first 256 KiB boundary,
/// followed by a long line that crosses it, then `bar end`. A search cut at
/// the last newline before the boundary puts `foo start` and `bar end` in
/// different windows.
fn straddle_fixture() -> String {
    let filler = "0123456789abcdef0123456789abcdef\n"; // 33 bytes
    let head = "foo start\n";
    let mut body = String::new();
    while body.len() + filler.len() + head.len() <= CHUNK - 20 {
        body.push_str(filler);
    }
    body.push_str(head);
    let start_of_middle = body.len();
    assert!(start_of_middle < CHUNK, "the head line ends inside the first window");
    let middle = format!("middle{}\n", "x".repeat(200));
    body.push_str(&middle);
    assert!(
        body.len() > CHUNK,
        "the middle line crosses the window boundary; len={}",
        body.len()
    );
    body.push_str("bar end\n");
    for _ in 0..16 {
        body.push_str(filler);
    }
    body
}

/// The 1-based line number of `foo start` in [`straddle_fixture`].
fn head_line_number() -> usize {
    let body = straddle_fixture();
    let at = body.find("foo start").unwrap();
    body[..at].matches('\n').count() + 1
}

fn straddle_match() -> String {
    format!("foo start\nmiddle{}\nbar end", "x".repeat(200))
}

async fn run_in(dir: &Path, script: &str) -> (String, String, i64) {
    let kernel = kernel_at(dir);
    let result = kernel.execute(script).await.unwrap();
    (result.text_out().to_string(), result.err.clone(), result.code)
}

async fn run_with_files(files: &[(&str, &str)], script: &str) -> (String, String, i64) {
    let dir = tempdir().unwrap();
    for (name, content) in files {
        fs::write(dir.path().join(name), content).unwrap();
    }
    run_in(dir.path(), script).await
}

const SPAN: &str = "'(?s)foo.*?bar'";

#[rstest]
#[case::plain(format!("grep -U -E {SPAN} small.txt"), "foo start\nmiddle\nbar end\n")]
#[case::line_numbers(format!("grep -nU -E {SPAN} small.txt"), "2:foo start\nmiddle\nbar end\n")]
#[case::only_matching(format!("grep -oU -E {SPAN} small.txt"), "foo start\nmiddle\nbar\n")]
// -c counts the lines a match covers, as it does without -U.
#[case::count(format!("grep -cU -E {SPAN} small.txt"), "3\n")]
#[case::count_two_regions(format!("grep -cU -E {SPAN} two.txt"), "4\n")]
#[case::count_stdin(format!("cat two.txt | grep -cU -E {SPAN}"), "4\n")]
#[case::count_two_files(format!("grep -cU -E {SPAN} two.txt other.txt"), "two.txt:4\nother.txt:0\n")]
#[case::count_invert(format!("grep -cvU -E {SPAN} small.txt"), "2\n")]
#[case::count_two_matches_one_line("grep -cU -E 'foo' dup.txt", "1\n")]
#[case::count_max_count(format!("grep -cU -m1 -E {SPAN} two.txt"), "2\n")]
#[case::files_with_matches(format!("grep -lU -E {SPAN} small.txt"), "small.txt\n")]
#[case::pipe_into_pipe(format!("cat small.txt | grep -U -E {SPAN} | cat"), "foo start\nmiddle\nbar end\n")]
#[case::stdin(format!("cat small.txt | grep -U -E {SPAN}"), "foo start\nmiddle\nbar end\n")]
#[case::two_files(
    format!("grep -U -E {SPAN} small.txt other.txt"),
    "small.txt:foo start\nmiddle\nbar end\n"
)]
#[case::quiet(format!("grep -qU -E {SPAN} small.txt"), "")]
#[case::invert(format!("grep -vU -E {SPAN} small.txt"), "alpha\nomega\n")]
#[case::after_context(format!("grep -A1 -U -E {SPAN} small.txt"), "foo start\nmiddle\nbar end\nomega\n")]
#[case::max_count(format!("grep -m1 -U -E {SPAN} two.txt"), "foo 1\nbar 1\n")]
#[case::max_count_unbounded(format!("grep -U -E {SPAN} two.txt"), "foo 1\nbar 1\nfoo 2\nbar 2\n")]
#[case::recursive(format!("grep -rlU -E {SPAN} ."), "./small.txt\n./two.txt\n")]
#[tokio::test]
async fn multiline_match_crosses_lines(#[case] script: String, #[case] expected: &str) {
    let files = [
        ("small.txt", SMALL),
        ("other.txt", "nothing here\n"),
        ("two.txt", "foo 1\nbar 1\nbetween\nfoo 2\nbar 2\n"),
        ("dup.txt", "foo foo\nqux\n"),
    ];
    let (out, err, code) = run_with_files(&files, &script).await;
    assert_eq!(code, 0, "script={script:?} err={err:?} out={out:?}");
    assert_eq!(out, expected, "script={script:?}");
}

// A pipeline exits with its last stage, so `| cat` exits 0.
#[rstest]
#[case::plain(format!("grep -E {SPAN} small.txt"), 1, "")]
#[case::count(format!("grep -c -E {SPAN} small.txt"), 1, "0\n")]
#[case::pipe_into_pipe(format!("cat small.txt | grep -E {SPAN} | cat"), 0, "")]
#[case::stdin(format!("cat small.txt | grep -E {SPAN}"), 1, "")]
#[tokio::test]
async fn without_multiline_a_match_stays_on_one_line(
    #[case] script: String,
    #[case] expected_code: i64,
    #[case] expected: &str,
) {
    let (out, err, code) = run_with_files(&[("small.txt", SMALL)], &script).await;
    assert_eq!(code, expected_code, "script={script:?} err={err:?} out={out:?}");
    assert_eq!(out, expected, "script={script:?}");
}

#[rstest]
#[case::plain("grep -U -E '(?s)foo start.*?bar end' big.txt", format!("{}\n", straddle_match()))]
#[case::line_numbers(
    "grep -nU -E '(?s)foo start.*?bar end' big.txt",
    format!("{}:{}\n", head_line_number(), straddle_match())
)]
#[case::only_matching("grep -oU -E '(?s)foo start.*?bar end' big.txt", format!("{}\n", straddle_match()))]
#[case::count("grep -cU -E '(?s)foo start.*?bar end' big.txt", "3\n".to_string())]
#[case::files_with_matches("grep -lU -E '(?s)foo start.*?bar end' big.txt", "big.txt\n".to_string())]
#[case::stdin("cat big.txt | grep -cU -E '(?s)foo start.*?bar end'", "3\n".to_string())]
#[case::pipe_into_pipe(
    "cat big.txt | grep -U -E '(?s)foo start.*?bar end' | cat",
    format!("{}\n", straddle_match())
)]
#[case::two_files(
    "grep -cU -E '(?s)foo start.*?bar end' big.txt other.txt",
    "big.txt:3\nother.txt:0\n".to_string()
)]
#[tokio::test]
async fn multiline_match_straddles_a_chunk_boundary(#[case] script: &str, #[case] expected: String) {
    let big = straddle_fixture();
    let (out, err, code) =
        run_with_files(&[("big.txt", big.as_str()), ("other.txt", "nothing here\n")], script).await;
    assert_eq!(code, 0, "script={script:?} err={err:?} out_len={}", out.len());
    assert_eq!(out, expected, "script={script:?}");
}

#[tokio::test]
async fn the_straddle_fixture_line_number_is_the_head_line() {
    let big = straddle_fixture();
    let (out, err, code) =
        run_with_files(&[("big.txt", big.as_str())], "grep -n 'foo start' big.txt").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out, format!("{}:foo start\n", head_line_number()));
}

#[tokio::test]
async fn without_multiline_the_straddle_fixture_matches_per_line() {
    let big = straddle_fixture();
    let (out, err, code) =
        run_with_files(&[("big.txt", big.as_str())], "grep -c -E 'foo start|bar end' big.txt").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out, "2\n");
}

// `\s` can match a newline, so one match region covers the whole file. The
// count is still the lines it covers: every one of the 34.
#[rstest]
#[case::file("grep -cU '\\s' lines.txt", "34\n")]
#[case::stdin("cat lines.txt | grep -cU '\\s'", "34\n")]
#[case::two_files("grep -cU '\\s' lines.txt other.txt", "lines.txt:34\nother.txt:1\n")]
#[tokio::test]
async fn count_of_a_match_that_spans_every_line_is_the_line_count(
    #[case] script: &str,
    #[case] expected: &str,
) {
    let lines: String = (1..=34).map(|n| format!("line {n}\n")).collect();
    let (out, err, code) =
        run_with_files(&[("lines.txt", lines.as_str()), ("other.txt", "one line\n")], script).await;
    assert_eq!(code, 0, "script={script:?} err={err:?}");
    assert_eq!(out, expected, "script={script:?}");
}
