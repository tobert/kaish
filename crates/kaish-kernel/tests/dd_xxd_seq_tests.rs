//! Kernel-routed regression tests for three P2 bugs:
//!
//! 1. `dd skip=N` ignored unless `count=` is also present.
//! 2. `xxd -r -p` silently dropping a trailing odd hex nibble — verify kaish
//!    matches real xxd (which also silently drops), then add an explicit
//!    even-nibble round-trip test to pin the current contract.
//! 3. `seq -w` not padding negative numbers to equal width.
//!
//! All tests route through `kernel.execute()` so the full dispatch chain runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use std::fs;
use tempfile::tempdir;

// ---------------------------------------------------------------------------
// Bug 1: dd skip=N without count=
// ---------------------------------------------------------------------------

/// dd skip=2 bs=1 (no count) must skip the first 2 bytes and yield the rest.
/// Verified against GNU dd:
///   printf '\x00\x01\x02\x03\x04\x05\x06\x07' > test.bin
///   dd if=test.bin skip=2 bs=1 2>/dev/null | xxd -p  -> 020304050607
#[tokio::test]
async fn dd_skip_without_count_skips_bytes() {
    let dir = tempdir().unwrap();
    // Write 8 bytes: 0x00..0x07
    let test_file = dir.path().join("input.bin");
    fs::write(&test_file, b"\x00\x01\x02\x03\x04\x05\x06\x07").unwrap();

    let kernel = kernel_at(dir.path());
    // skip=2 bs=1 means: skip 2 * 1 = 2 bytes, then read the rest
    let (out, code) = run(&kernel, "dd if=input.bin skip=2 bs=1 | xxd -p").await;
    assert_eq!(code, 0, "dd skip=2 bs=1 should succeed, got: {out:?}");
    assert_eq!(
        out, "020304050607",
        "dd skip=2 bs=1 should skip first 2 bytes; got: {out:?}"
    );
}

/// dd skip=2 bs=2 (no count) must skip 2*2=4 bytes and yield bytes 4..7.
/// Verified against GNU dd:
///   dd if=test.bin skip=2 bs=2 2>/dev/null | xxd -p  -> 04050607
#[tokio::test]
async fn dd_skip_without_count_respects_block_size() {
    let dir = tempdir().unwrap();
    let test_file = dir.path().join("input.bin");
    fs::write(&test_file, b"\x00\x01\x02\x03\x04\x05\x06\x07").unwrap();

    let kernel = kernel_at(dir.path());
    // skip=2 bs=2 means: skip 2 * 2 = 4 bytes
    let (out, code) = run(&kernel, "dd if=input.bin skip=2 bs=2 | xxd -p").await;
    assert_eq!(code, 0, "dd skip=2 bs=2 should succeed: {out:?}");
    assert_eq!(
        out, "04050607",
        "dd skip=2 bs=2 should skip first 4 bytes; got: {out:?}"
    );
}

/// dd with no skip and no count must still read the whole file (regression guard).
#[tokio::test]
async fn dd_no_skip_no_count_reads_all() {
    let dir = tempdir().unwrap();
    let test_file = dir.path().join("input.bin");
    fs::write(&test_file, b"\x00\x01\x02\x03").unwrap();

    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "dd if=input.bin bs=1 | xxd -p").await;
    assert_eq!(code, 0);
    assert_eq!(out, "00010203", "dd without skip/count must read all; got: {out:?}");
}

/// dd skip=0 bs=1 (explicit zero skip) must also read all (no-op skip).
#[tokio::test]
async fn dd_skip_zero_reads_all() {
    let dir = tempdir().unwrap();
    let test_file = dir.path().join("input.bin");
    fs::write(&test_file, b"\x00\x01\x02\x03").unwrap();

    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "dd if=input.bin skip=0 bs=1 | xxd -p").await;
    assert_eq!(code, 0);
    assert_eq!(out, "00010203", "dd skip=0 must read all bytes; got: {out:?}");
}

/// dd skip=N with count=M should still skip and then read only count blocks.
/// This tests that the fix doesn't break the existing count+skip path.
/// Verified against GNU dd:
///   dd if=test.bin skip=2 bs=1 count=3 2>/dev/null | xxd -p  -> 020304
#[tokio::test]
async fn dd_skip_with_count_still_works() {
    let dir = tempdir().unwrap();
    let test_file = dir.path().join("input.bin");
    fs::write(&test_file, b"\x00\x01\x02\x03\x04\x05\x06\x07").unwrap();

    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "dd if=input.bin skip=2 bs=1 count=3 | xxd -p").await;
    assert_eq!(code, 0, "dd skip=2 count=3 should succeed: {out:?}");
    assert_eq!(
        out, "020304",
        "dd skip=2 bs=1 count=3 should yield 3 bytes starting at offset 2; got: {out:?}"
    );
}

// ---------------------------------------------------------------------------
// Bug 2: xxd -r -p trailing odd nibble handling
// ---------------------------------------------------------------------------

/// An even number of nibbles round-trips correctly (baseline).
#[tokio::test]
async fn xxd_reverse_plain_even_nibbles_roundtrip() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    // "hello" -> 68656c6c6f (10 nibbles = 5 bytes) -> "hello"
    let (out, code) = run(&kernel, "echo 68656c6c6f | xxd -r -p").await;
    assert_eq!(code, 0);
    assert_eq!(out, "hello", "xxd -r -p of even hex should round-trip; got: {out:?}");
}

/// A trailing odd nibble is silently dropped — this matches real GNU xxd behavior.
/// Verified: `printf 'abcde' | xxd -r -p | wc -c` -> 2 (not 3), trailing 'e' dropped.
/// Verified: `printf 'c' | xxd -r -p | wc -c` -> 0, single nibble dropped.
/// kaish must match this contract: silently drop, exit 0.
#[tokio::test]
async fn xxd_reverse_plain_odd_nibble_dropped_silently() {
    let dir = tempdir().unwrap();
    // "68656c6c6f1" = "hello" hex + trailing nibble '1': should produce "hello" (5 bytes)
    // The trailing '1' nibble is dropped silently, matching GNU xxd.
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("echo 68656c6c6f1 | xxd -r -p")
        .await
        .unwrap();
    assert_eq!(result.code, 0, "xxd -r -p with trailing odd nibble must exit 0");
    // The text output should be "hello" (5 bytes); the extra nibble is silently dropped.
    assert_eq!(
        result.text_out().as_ref(),
        "hello",
        "xxd -r -p must drop trailing odd nibble silently (matches GNU xxd); got: {:?}",
        result.text_out()
    );
}

/// A single lone nibble produces empty output (matches GNU xxd).
/// Verified: `printf 'c' | xxd -r -p | wc -c` -> 0
#[tokio::test]
async fn xxd_reverse_plain_single_nibble_produces_empty() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute("echo c | xxd -r -p | xxd -p")
        .await
        .unwrap();
    assert_eq!(result.code, 0, "xxd with single nibble must exit 0");
    // A single nibble decodes to 0 bytes, so xxd -p of that produces empty output.
    assert_eq!(
        result.text_out().trim(),
        "",
        "single nibble should yield empty output; got: {:?}",
        result.text_out()
    );
}

/// Whitespace in plain hex input is correctly stripped (matches GNU xxd).
/// Verified: `printf 'ab cd ef' | xxd -r -p | xxd -p` -> abcdef
#[tokio::test]
async fn xxd_reverse_plain_strips_whitespace() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "echo 'ab cd ef' | xxd -r -p | xxd -p").await;
    assert_eq!(code, 0);
    assert_eq!(out, "abcdef", "xxd -r -p must strip whitespace; got: {out:?}");
}

// ---------------------------------------------------------------------------
// Bug 3: seq -w negative number padding
// ---------------------------------------------------------------------------

/// seq -w -3 3 must equal-width pad ALL numbers (including negatives).
/// Verified against GNU seq:
///   seq -w -3 3  ->  -3, -2, -1, 00, 01, 02, 03
/// The width is determined by the widest item including the minus sign: "-3" = 2 chars.
/// Positives are then zero-padded to the same total width: "00", "01", etc.
#[tokio::test]
async fn seq_w_negative_numbers_padded() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "seq -w -3 3").await;
    assert_eq!(code, 0, "seq -w -3 3 should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(
        lines,
        vec!["-3", "-2", "-1", "00", "01", "02", "03"],
        "seq -w -3 3 must pad positives to width of widest item (including sign); got: {lines:?}"
    );
}

/// seq -w -10 2 must pad to width 3 ("-10" is widest).
/// Verified against GNU seq:
///   seq -w -10 2  ->  -10, -09, -08, ..., -01, 000, 001, 002
#[tokio::test]
async fn seq_w_negative_numbers_wider_padding() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "seq -w -10 2").await;
    assert_eq!(code, 0, "seq -w -10 2 should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    // Width is 3 ("-10" has 3 chars)
    // Negatives: -10, -09, -08, -07, -06, -05, -04, -03, -02, -01
    // Positives: 000, 001, 002
    assert_eq!(lines[0], "-10", "first line should be -10; got: {:?}", lines[0]);
    assert_eq!(lines[1], "-09", "second line should be -09; got: {:?}", lines[1]);
    assert_eq!(
        lines[10], "000",
        "first non-negative should be 000 (width 3); got: {:?}",
        lines[10]
    );
    assert_eq!(lines[12], "002", "last line should be 002; got: {:?}", lines[12]);
}

/// seq -w 1 10 (all positive) still pads correctly (regression guard).
/// Verified: seq -w 1 10 -> 01, 02, ..., 10
#[tokio::test]
async fn seq_w_positive_only_unaffected() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "seq -w 1 10").await;
    assert_eq!(code, 0);
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(lines[0], "01", "first should be 01; got: {:?}", lines[0]);
    assert_eq!(lines[9], "10", "last should be 10; got: {:?}", lines[9]);
}

/// seq -w with all same width negatives (no padding needed for negatives, positives still pad).
/// seq -w -2 2 -> -2, -1, 00, 01, 02 (width is 2, "-2" and "-1" are already width 2)
#[tokio::test]
async fn seq_w_negatives_same_width_as_padded_positives() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "seq -w -2 2").await;
    assert_eq!(code, 0, "seq -w -2 2 should succeed: {out:?}");
    let lines: Vec<&str> = out.lines().collect();
    assert_eq!(
        lines,
        vec!["-2", "-1", "00", "01", "02"],
        "seq -w -2 2 must pad positives to match negative width; got: {lines:?}"
    );
}
