//! Kernel-routed regression tests for binary (`Value::Bytes`) at TEXT SINKS.
//!
//! A binary value captured via `$(...)` (e.g. `b=$(cat blob)` — `cat` of a
//! non-UTF-8 file yields `Value::Bytes`) must go LOUD when it reaches a text
//! sink — string interpolation (`"x=$b"`), a bare word into an external-command
//! argv (`prog $b`), or the `echo` text-output builtin — never render the
//! `[binary: N bytes]` placeholder. The placeholder where the user's real bytes
//! should be is silent data corruption (crash beats corrupt).
//!
//! All tests route through `kernel.execute()` so the full dispatch chain runs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use std::fs;
use tempfile::tempdir;

/// Invalid-UTF-8 octets so `cat` marks the capture binary (`Value::Bytes`)
/// rather than a `String` — that's the path that used to reach the placeholder.
const BIN: &[u8] = b"\xff\x00\xfe\x80\x01\xc0kaish\xf5";

/// Assert a script ran loud: either `execute` returned `Err`, or it returned a
/// nonzero `ExecResult` whose error names the binary problem — and in NO case
/// did the `[binary: N bytes]` placeholder leak into stdout.
async fn assert_loud_binary(script: &str) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("src.bin"), BIN).unwrap();
    let kernel = kernel_at(dir.path());
    match kernel.execute(script).await {
        Ok(r) => {
            assert_ne!(r.code, 0, "binary at a text sink must be a nonzero exit: {script:?}");
            assert!(
                r.err.contains("binary"),
                "error should name the binary problem, got err={:?} for {script:?}",
                r.err
            );
            assert!(
                !r.text_out().contains("[binary"),
                "the placeholder must NOT leak to stdout: {:?}",
                r.text_out()
            );
        }
        Err(e) => {
            let msg = e.to_string();
            assert!(
                msg.contains("binary"),
                "execute error should name the binary problem, got {msg:?} for {script:?}"
            );
        }
    }
}

#[tokio::test]
async fn interpolating_a_binary_capture_is_loud_not_placeholder() {
    // The primary live bug: capture bytes, then splice into a string.
    assert_loud_binary(r#"b=$(cat src.bin); echo "x=$b""#).await;
}

#[tokio::test]
async fn bare_word_binary_into_echo_is_loud() {
    // `echo` is a pure text-output sink; a bare `$b` binary word goes loud.
    assert_loud_binary("b=$(cat src.bin); echo $b").await;
}

#[tokio::test]
async fn binary_arg_into_printf_is_loud() {
    // `printf` is a pure text-output sink like `echo`; a binary operand goes
    // loud instead of the `[binary: N bytes]` placeholder (kaibo C1).
    assert_loud_binary(r#"b=$(cat src.bin); printf "val=%s\n" "$b""#).await;
}

#[tokio::test]
async fn binary_in_default_expansion_is_loud() {
    // `${b:-fallback}` where b is present-and-binary must also go loud, not
    // render the placeholder (the value is present, so the default never fires).
    assert_loud_binary(r#"b=$(cat src.bin); echo "v=${b:-none}""#).await;
}

#[tokio::test]
async fn text_capture_interpolation_is_unaffected() {
    // Control: a normal text var still interpolates fine.
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(r#"t=$(echo hi); echo "x=$t""#).await.unwrap();
    assert_eq!(result.code, 0, "text var must still work: {}", result.err);
    assert_eq!(result.text_out().trim(), "x=hi");
}

#[tokio::test]
async fn text_capture_bare_word_is_unaffected() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("t=$(echo hi); echo $t").await.unwrap();
    assert_eq!(result.code, 0, "text var must still work: {}", result.err);
    assert_eq!(result.text_out().trim(), "hi");
}

/// External-command argv (`build_args_flat`): a bare `$b` binary word must go
/// loud crossing the process boundary. Gated on Linux + subprocess because it
/// spawns a real `/bin/echo`.
#[cfg(all(target_os = "linux", feature = "subprocess"))]
#[tokio::test]
async fn bare_word_binary_into_external_argv_is_loud() {
    assert_loud_binary("b=$(cat src.bin); /bin/echo $b").await;
}
