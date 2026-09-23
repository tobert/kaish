//! `grep -A/-B/-C` prints the context it was asked for, wherever the match
//! sits in a large file.
//!
//! grep's whole-buffer path checkpoints so a script timeout can stop it
//! (`ToolCtx::checkpoint`), which it does by searching in
//! `ExecContext::STREAM_CHUNK_SIZE` (256 KiB) windows. `Searcher::search_slice`
//! resets its context tracking on every call, so a match within NUM lines of a
//! window boundary would print short context. A context grep therefore searches
//! the whole buffer in one call: a wrong answer is worse than a slow one.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use tempfile::tempdir;

use common::{kernel_at, run};

/// Lines of a known width, with `needle` placed just past the first 256 KiB
/// boundary — close enough to it that a chunked search would cut the match's
/// before-context away.
fn file_with_needle_past_the_first_chunk(path: &std::path::Path) {
    let line = "0123456789abcdef0123456789abcdef"; // 32 chars + newline
    let per_chunk = (256 * 1024) / 33 + 1;
    let mut body = String::new();
    for _ in 0..per_chunk {
        body.push_str(line);
        body.push('\n');
    }
    body.push_str("needle\n");
    for _ in 0..8 {
        body.push_str(line);
        body.push('\n');
    }
    fs::write(path, body).expect("write the fixture");
}

#[tokio::test]
async fn context_is_whole_even_when_the_match_sits_past_a_chunk_boundary() {
    let dir = tempdir().unwrap();
    let path = dir.path().join("big.txt");
    file_with_needle_past_the_first_chunk(&path);
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -B2 -A2 needle big.txt").await;
    assert_eq!(code, 0, "the needle is there; out={out:?}");
    let lines: Vec<&str> = out.lines().filter(|l| !l.is_empty()).collect();
    assert_eq!(
        lines.len(),
        5,
        "2 before + the match + 2 after, the same as for a match in the first \
         chunk; out={out:?}"
    );
    assert!(lines[2].contains("needle"), "the match sits in the middle; out={out:?}");
}

#[tokio::test]
async fn a_match_inside_the_first_chunk_is_unchanged() {
    let dir = tempdir().unwrap();
    let path = dir.path().join("small.txt");
    fs::write(&path, "a\nb\nneedle\nc\nd\n").expect("write");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -B2 -A2 needle small.txt").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.lines().filter(|l| !l.is_empty()).count(), 5, "out={out:?}");
}
