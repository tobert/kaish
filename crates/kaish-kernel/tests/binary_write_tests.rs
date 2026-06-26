//! Kernel-routed regression tests for the binary-`$()` write corruption bug.
//!
//! `write FILE $(producer)` and `producer | write FILE` must persist the raw
//! bytes of a `Value::Bytes` verbatim, not stringify it to the `[binary: N
//! bytes]` placeholder — that placeholder reaching a file is silent corruption.
//! `tee` already reads stdin as raw bytes; pinned here so it stays that way.
//!
//! All tests route through `kernel.execute()` so the full dispatch chain runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use std::fs;
use tempfile::tempdir;

/// Bytes with several invalid-UTF-8 octets (`\xff`, `\x80`, `\xc0`, `\xf5`) so a
/// producer marks them as binary (`Value::Bytes`) rather than capturing a
/// `String` — that's the path that used to corrupt to the placeholder.
const BIN: &[u8] = b"\xff\x00\xfe\x80\x01\xc0kaish\xf5";

#[tokio::test]
async fn write_positional_cmdsubst_preserves_bytes() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();

    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "write dst.bin $(cat src.bin)").await;
    assert_eq!(code, 0, "write should succeed");

    let written = fs::read(dir.path().join("dst.bin")).unwrap();
    assert_eq!(
        written, BIN,
        "write must persist raw bytes, not the [binary: N bytes] placeholder"
    );
}

#[tokio::test]
async fn write_pipe_stdin_preserves_bytes() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();

    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "cat src.bin | write dst.bin").await;
    assert_eq!(code, 0, "write should succeed");

    let written = fs::read(dir.path().join("dst.bin")).unwrap();
    assert_eq!(written, BIN);
}

#[tokio::test]
async fn tee_preserves_bytes_through_to_file() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();

    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "cat src.bin | tee out.bin").await;
    assert_eq!(code, 0, "tee should succeed");

    let written = fs::read(dir.path().join("out.bin")).unwrap();
    assert_eq!(written, BIN, "tee must keep binary intact to the file");
}

// ── gemini-pro review (#30) follow-ups: named-flag, empty stream, multi-sink ──

#[tokio::test]
async fn write_named_content_flag_preserves_bytes() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();

    let kernel = kernel_at(dir.path());
    // The `--content` named branch must not stringify a `Value::Bytes` either.
    let (_out, code) = run(&kernel, "write dst.bin --content $(cat src.bin)").await;
    assert_eq!(code, 0, "write --content should succeed");

    let written = fs::read(dir.path().join("dst.bin")).unwrap();
    assert_eq!(written, BIN, "named --content must preserve raw bytes");
}

#[tokio::test]
async fn write_empty_pipe_creates_empty_file() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    // An empty pipe is `Some(vec![])`, not `None` — write an empty file rather
    // than erroring with "missing content argument".
    let (_out, code) = run(&kernel, "printf '' | write dst.bin").await;
    assert_eq!(code, 0, "empty pipe should write an empty file, not error");

    let written = fs::read(dir.path().join("dst.bin")).unwrap();
    assert!(written.is_empty(), "empty pipe yields an empty file: {written:?}");
}

#[tokio::test]
async fn tee_preserves_bytes_to_multiple_sinks() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();

    let kernel = kernel_at(dir.path());
    // The per-file loop must not drain/mangle the buffer after the first sink.
    let (_out, code) = run(&kernel, "cat src.bin | tee a.bin b.bin").await;
    assert_eq!(code, 0, "multi-sink tee should succeed");

    assert_eq!(fs::read(dir.path().join("a.bin")).unwrap(), BIN);
    assert_eq!(fs::read(dir.path().join("b.bin")).unwrap(), BIN, "second sink intact");
}
