//! Kernel-routed regression tests for GH #176 item 1: buffered stdin must
//! carry raw bytes, not stringify (or refuse) binary content.
//!
//! Before this fix, `ExecContext::stdin` / `ExecuteOptions::stdin` were
//! `Option<String>`, so a `< file` redirect over non-UTF-8 content failed at
//! *redirect setup* (`String::from_utf8` error) before any builtin ever ran,
//! and an embedder feeding pre-read binary via `ExecuteOptions::with_stdin` had
//! no way to hand over anything but valid UTF-8 text. Byte-aware builtins
//! (`wc -c`, `cat`, `cmp`, `checksum`, …) should consume that content exactly
//! as they already do over a streamed pipe; text-only builtins (`grep`,
//! `sed`, …) must still refuse it loudly — just at the point of
//! *consumption* (`read_stdin_to_text`), not at redirect setup.
//!
//! All tests route through `kernel.execute()` / `kernel.execute_with_options()`
//! so the full dispatch chain runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use kaish_kernel::ExecuteOptions;
use std::fs;
use tempfile::tempdir;

/// Invalid-UTF-8 octets — same fixture idiom as `binary_text_sink_tests.rs` —
/// so a redirect/`with_stdin` source is genuinely binary, not just "not ASCII".
const BIN: &[u8] = b"\xff\x00\xfe\x80\x01\xc0kaish\xf5";

#[tokio::test]
async fn redirect_stdin_binary_file_feeds_exact_byte_count() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("wc -c < src.bin").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(
        result.text_out().trim(),
        BIN.len().to_string(),
        "wc -c over a `< file` redirect must see the exact byte count instead \
         of erroring on invalid UTF-8 at redirect setup: out={:?}",
        result.text_out()
    );
}

#[tokio::test]
async fn redirect_stdin_binary_file_cat_round_trips_bytes() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("cat < src.bin").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(
        result.out_bytes(),
        Some(BIN),
        "cat over a `< file` redirect must round-trip the raw bytes byte-for-byte"
    );
}

#[tokio::test]
async fn redirect_stdin_binary_file_text_tool_still_errors_loud() {
    // grep is a text-only consumer: it must still refuse binary — just at the
    // point it calls `read_stdin_to_text`, not at redirect setup (which no
    // longer requires UTF-8 up front).
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("grep x < src.bin").await.unwrap();
    assert!(!result.ok(), "grep over binary stdin must fail, not match garbage");
    assert!(
        result.err.contains("not valid UTF-8"),
        "error should name the binary problem, got err={:?}",
        result.err
    );
}

#[tokio::test]
async fn redirect_stdin_text_file_is_unaffected() {
    // Control: an ordinary text `< file` redirect keeps working exactly as
    // before (this is the overwhelmingly common case).
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("plain.txt"), "b\na\nc\n").unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("sort < plain.txt").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(result.text_out(), "a\nb\nc\n");
}

#[tokio::test]
async fn redirect_stdin_missing_file_is_still_a_loud_error() {
    // Control: a missing redirect target must still error (not silently empty
    // stdin) — the read failure path is untouched by the UTF-8-gate removal.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("cat < does-not-exist.txt").await.unwrap();
    assert!(!result.ok(), "a missing redirect target must be a loud error");
}

#[tokio::test]
async fn execute_options_with_stdin_bytes_feeds_byte_aware_builtin() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute_with_options("wc -c", ExecuteOptions::new().with_stdin(BIN.to_vec()))
        .await
        .unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(
        result.text_out().trim(),
        BIN.len().to_string(),
        "ExecuteOptions::with_stdin(Vec<u8>) must feed raw bytes to a byte-aware builtin"
    );
}

#[tokio::test]
async fn execute_options_with_stdin_bytes_text_tool_errors_loud() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute_with_options("grep x", ExecuteOptions::new().with_stdin(BIN.to_vec()))
        .await
        .unwrap();
    assert!(!result.ok());
    assert!(
        result.err.contains("not valid UTF-8"),
        "err={:?}",
        result.err
    );
}

#[tokio::test]
async fn execute_options_with_stdin_text_str_is_unaffected() {
    // Control: the common `&str` embedder call site must keep compiling and
    // working now that `with_stdin` takes `impl Into<Vec<u8>>` instead of
    // `impl Into<String>`.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute_with_options("cat", ExecuteOptions::new().with_stdin("hello\n"))
        .await
        .unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(result.text_out(), "hello\n");
}

// ── `xxd -r` regression: a `< file` redirect newly reaches binary content
// (see above), and `xxd -r` must refuse it loudly rather than lossy-decoding
// it — found via a second review pass over this PR. ──

#[tokio::test]
async fn redirect_stdin_binary_file_xxd_reverse_errors_loud_not_lossy() {
    // `xxd -r` consumes hex TEXT. Before this PR's stdin fix, a `< file`
    // redirect over binary content couldn't even reach `xxd` (redirect setup
    // itself rejected non-UTF-8, so this exact bug was unreachable). Now that
    // redirects carry bytes through intact, `xxd -r` must refuse loud on its
    // own instead of `String::from_utf8_lossy`-ing BIN into `U+FFFD` and
    // silently mis-decoding the (wrong) "hex".
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("xxd -r -p < src.bin").await.unwrap();
    assert!(!result.ok(), "xxd -r on binary input must fail loud, not lossy-decode it");
    assert!(
        result.err.contains("not valid UTF-8"),
        "error should name the binary problem, got err={:?}",
        result.err
    );
}

#[tokio::test]
async fn xxd_reverse_file_path_binary_input_errors_loud() {
    // Same guard, exercised via a binary *file path* operand rather than
    // stdin — both sources funnel through the same `data: Vec<u8>` read in
    // `xxd.rs` before the reverse-mode decode, so this was an equally live
    // (pre-existing, not GH #176-introduced) lossy-decode hazard.
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("xxd -r -p src.bin").await.unwrap();
    assert!(!result.ok(), "xxd -r on a binary file must fail loud, not lossy-decode it");
    assert!(result.err.contains("not valid UTF-8"), "err={:?}", result.err);
}

#[tokio::test]
async fn xxd_forward_reverse_round_trip_is_unaffected() {
    // Happy-path pin: an ordinary forward-then-reverse round trip through a
    // pipeline (valid hex text, not a binary source) must keep working
    // exactly as before the strict-decode guard was added.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("echo -n hello | xxd -p | xxd -r -p").await.unwrap();
    assert!(result.ok(), "err={}", result.err);
    assert_eq!(result.text_out(), "hello");
}
