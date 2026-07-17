//! Kernel-routed regression tests for GH #176 item 2: `wc -m`/`-w`/default must
//! refuse invalid UTF-8 loudly instead of silently lossy-decoding it
//! (`String::from_utf8_lossy` expands every invalid byte run into `U+FFFD`,
//! over-counting chars/words on binary input). `wc -c`/`wc -l` are pure
//! byte-level operations (exact length, raw `\n` scan) and must keep working on
//! binary — only the counts that require a text view refuse.
//!
//! This mirrors the rest of the fleet: `grep`/`sed`/`awk`/`cut`/`sort` already
//! error loud on non-UTF-8 stdin/file input (`read_stdin_to_text`,
//! `String::from_utf8`) rather than mangling it — `wc`'s char/word counts are
//! brought in line with that stance. See `docs/binary-data.md`.
//!
//! All tests route through `kernel.execute()` so the full dispatch chain runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use std::fs;
use tempfile::tempdir;

/// Invalid-UTF-8 octets — same fixture idiom as `binary_text_sink_tests.rs`.
const BIN: &[u8] = b"\xff\x00\xfe\x80\x01\xc0kaish\xf5";

// ── stdin (redirect) path ──

#[tokio::test]
async fn wc_dash_c_still_works_on_binary_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("wc -c < src.bin").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(result.text_out().trim(), BIN.len().to_string());
}

#[tokio::test]
async fn wc_dash_l_still_works_on_binary_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("wc -l < src.bin").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    // BIN has no `\n` byte in it.
    assert_eq!(result.text_out().trim(), "0");
}

#[tokio::test]
async fn wc_dash_m_errors_loud_on_binary_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("wc -m < src.bin").await.unwrap();
    assert!(!result.ok(), "wc -m on binary must fail loud, not lossy-decode it");
    assert!(
        result.err.contains("invalid UTF-8"),
        "error should name the binary problem, got err={:?}",
        result.err
    );
    assert!(
        result.text_out().trim().is_empty(),
        "no lossy char count should be emitted: {:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn wc_dash_w_errors_loud_on_binary_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("wc -w < src.bin").await.unwrap();
    assert!(!result.ok(), "wc -w on binary must fail loud");
    assert!(result.err.contains("invalid UTF-8"), "err={:?}", result.err);
}

#[tokio::test]
async fn wc_default_all_counts_errors_loud_on_binary_stdin() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("wc < src.bin").await.unwrap();
    assert!(!result.ok(), "plain wc (all counts) on binary must fail loud");
    assert!(result.err.contains("invalid UTF-8"), "err={:?}", result.err);
}

// ── file path (chunked read) ──

#[tokio::test]
async fn wc_dash_c_still_works_on_binary_file() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("wc -c src.bin").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert!(
        result.text_out().contains(&BIN.len().to_string()),
        "out={:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn wc_dash_m_errors_loud_on_binary_file() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("wc -m src.bin").await.unwrap();
    assert!(!result.ok(), "wc -m on a binary file must fail loud");
    assert!(
        result.err.contains("invalid UTF-8"),
        "error should name the file and the binary problem, got err={:?}",
        result.err
    );
}

#[tokio::test]
async fn wc_dash_m_multi_file_reports_the_binary_one_but_still_counts_the_text_one() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    fs::write(dir.path().join("plain.txt"), "hi\n").unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("wc -m plain.txt src.bin").await.unwrap();
    assert!(!result.ok(), "one binary file among many must still fail loud overall");
    assert!(result.err.contains("src.bin"), "err={:?}", result.err);
    assert!(
        result.text_out().contains("plain.txt"),
        "the valid file's row should still be reported: {:?}",
        result.text_out()
    );
}

// ── control: valid UTF-8 is unaffected ──

#[tokio::test]
async fn wc_dash_m_counts_multibyte_utf8_as_one_char_each() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    // "héllo" is 6 bytes (é is 2 bytes) but 5 chars.
    let result = kernel.execute(r#"echo -n "héllo" | wc -m"#).await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(result.text_out().trim(), "5");
}
