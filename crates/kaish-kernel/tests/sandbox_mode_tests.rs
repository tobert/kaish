//! Integration tests for sandbox mode (no-default-features / WASI-compatible).
//!
//! These tests exercise the kernel with `KernelConfig::isolated()` — the same
//! configuration that kaish-wasi uses. They verify that:
//!
//! - Core builtins work without OS dependencies
//! - Pipelines work (multi-stage text transforms)
//! - The VFS is memory-only (no local filesystem leaks)
//! - External commands are correctly blocked
//! - Help system lists only available (ungated) builtins

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

/// Create an isolated kernel (same config as kaish-wasi).
async fn sandbox_kernel() -> Arc<Kernel> {
    let config = KernelConfig::isolated();
    Kernel::new(config)
        .expect("failed to create isolated kernel")
        .into_arc()
}

#[tokio::test]
async fn sandbox_echo() {
    let k = sandbox_kernel().await;
    let r = k.execute("echo hello").await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "hello");
}

#[tokio::test]
async fn sandbox_pipeline() {
    let k = sandbox_kernel().await;
    let r = k.execute("seq 1 5 | sort -rn | head -3").await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "5\n4\n3");
}

#[tokio::test]
async fn sandbox_awk() {
    let k = sandbox_kernel().await;
    let r = k.execute(r#"echo "one two three" | awk "{print NF}""#).await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "3");
}

#[tokio::test]
async fn sandbox_jq() {
    let k = sandbox_kernel().await;
    let r = k.execute(r#"echo '{"x":42}' | jq ".x""#).await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "42");
}

#[tokio::test]
async fn sandbox_vfs_is_memory_only() {
    let k = sandbox_kernel().await;
    let r = k.execute("ls /").await.expect("execute failed");
    assert!(r.ok());
    let out = r.text_out().into_owned();
    let entries: Vec<&str> = out.trim().lines().collect();
    // NoLocal mode mounts: / → MemoryFs, /tmp → MemoryFs, /v → MemoryFs
    // ls / should show tmp and v (mounted under /)
    assert!(entries.contains(&"tmp"), "expected 'tmp' in ls /: {:?}", entries);
    assert!(entries.contains(&"v"), "expected 'v' in ls /: {:?}", entries);
    // Should NOT contain real filesystem entries
    assert!(!entries.contains(&"home"), "real filesystem leaking: {:?}", entries);
    assert!(!entries.contains(&"usr"), "real filesystem leaking: {:?}", entries);
    assert!(!entries.contains(&"etc"), "real filesystem leaking: {:?}", entries);
}

#[tokio::test]
async fn sandbox_write_and_read_via_vfs() {
    let k = sandbox_kernel().await;
    let r = k.execute("echo foo > /tmp/bar && cat /tmp/bar").await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "foo");
}

#[tokio::test]
async fn sandbox_append_redirect_via_vfs() {
    let k = sandbox_kernel().await;
    let r = k.execute("echo first > /tmp/app && echo second >> /tmp/app && cat /tmp/app")
        .await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "first\nsecond");
}

#[tokio::test]
async fn sandbox_relative_redirect_honors_cwd() {
    // Regression: redirect targets must resolve against $PWD like every other
    // path operand, so a relative `>` write and a later relative read agree.
    // Previously the redirect resolved against `/` while `cat` resolved against
    // cwd, so the file written by `>` was invisible to `cat`.
    let k = sandbox_kernel().await;
    let r = k
        .execute("mkdir -p /tmp/sub && cd /tmp/sub && echo x > f && cat f")
        .await
        .expect("execute failed");
    assert!(r.ok(), "expected success, got code={} err={:?}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "x");

    // The file must land at $PWD/f, not /f.
    let at_cwd = k.execute("cat /tmp/sub/f").await.expect("execute failed");
    assert!(at_cwd.ok(), "file should exist at /tmp/sub/f");
    assert_eq!(at_cwd.text_out().trim(), "x");

    let at_root = k.execute("cat /f").await.expect("execute failed");
    assert!(!at_root.ok(), "relative redirect must not write to /f");
}

#[tokio::test]
async fn sandbox_relative_append_honors_cwd() {
    let k = sandbox_kernel().await;
    let r = k
        .execute("mkdir -p /tmp/sub && cd /tmp/sub && echo a > log && echo b >> log && cat log")
        .await
        .expect("execute failed");
    assert!(r.ok(), "expected success, got code={} err={:?}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "a\nb");
}

#[tokio::test]
async fn sandbox_stdin_redirect_reads_via_vfs_and_cwd() {
    // `< file` must read through the VFS (not the host fs) with the target
    // resolved against $PWD, so it round-trips with a relative `>` write.
    let k = sandbox_kernel().await;
    let r = k
        .execute("mkdir -p /tmp/sub && cd /tmp/sub && echo payload > in && cat < in")
        .await
        .expect("execute failed");
    assert!(r.ok(), "expected success, got code={} err={:?}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "payload");
}

#[tokio::test]
async fn sandbox_stdin_redirect_missing_file_fails_loud() {
    // A missing redirect source must error, not silently feed empty stdin.
    let k = sandbox_kernel().await;
    let r = k.execute("cat < /tmp/does-not-exist").await.expect("execute failed");
    assert!(!r.ok(), "missing stdin redirect source should fail");
    assert!(
        r.err.contains("redirect"),
        "error should mention redirect, got: {:?}",
        r.err
    );
}

#[tokio::test]
async fn sandbox_redirect_target_command_substitution() {
    // Command substitution must run in redirect targets: both the output
    // target of `>` and the source of `<`. The subst yields the whole path.
    let k = sandbox_kernel().await;
    let r = k
        .execute("echo hi > $(echo /tmp/gen.txt) && cat /tmp/gen.txt")
        .await
        .expect("execute failed");
    assert!(r.ok(), "expected success, got code={} err={:?}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "hi");

    let r = k
        .execute("echo via-subst > /tmp/src && cat < $(echo /tmp/src)")
        .await
        .expect("execute failed");
    assert!(r.ok(), "expected success, got code={} err={:?}", r.code, r.err);
    assert_eq!(r.text_out().trim(), "via-subst");
}

#[tokio::test]
async fn sandbox_file_test_cannot_probe_host() {
    let k = sandbox_kernel().await;
    // `[[ -r ]]` should route through VFS, not the real filesystem.
    // /etc/passwd exists on the host but not in MemoryFs.
    let r = k.execute("[[ -r /etc/passwd ]]").await.expect("execute failed");
    assert!(!r.ok(), "[[ -r /etc/passwd ]] should be false in sandbox (file doesn't exist in VFS)");

    let r = k.execute("[[ -w /etc/shadow ]]").await.expect("execute failed");
    assert!(!r.ok(), "[[ -w /etc/shadow ]] should be false in sandbox");

    // But files written to VFS should be readable/writable
    let r = k.execute("echo data > /tmp/probe && [[ -r /tmp/probe ]]").await.expect("execute failed");
    assert!(r.ok(), "[[ -r ]] should work for VFS files");
}

#[tokio::test]
async fn sandbox_external_commands_blocked() {
    let k = sandbox_kernel().await;
    // Use a command name that is definitely not a builtin.
    let r = k.execute("/usr/bin/curl https://example.com").await.expect("execute failed");
    // Pin the policy block exactly: exit 127 + "command not found". A looser
    // !ok() would also pass if curl actually RAN and failed (network error,
    // exit 6) — i.e., a sandbox escape would go unnoticed.
    assert_eq!(
        r.code, 127,
        "external command should be policy-blocked (127), not merely fail: out={:?} err={:?}",
        r.text_out(),
        r.err
    );
    assert!(
        r.err.contains("command not found"),
        "policy block should surface as command-not-found: err={:?}",
        r.err
    );
}

#[tokio::test]
async fn sandbox_help_builtins_lists_available() {
    let k = sandbox_kernel().await;
    let r = k.execute("help builtins").await.expect("execute failed");
    assert!(r.ok());
    let out = r.text_out();
    // Core builtins should be listed
    assert!(out.contains("echo"), "echo should be listed");
    assert!(out.contains("cat"), "cat should be listed");
    assert!(out.contains("sort"), "sort should be listed");
    assert!(out.contains("jq"), "jq should be listed");
    // Native-only builtins should NOT be listed (when compiled without native)
    // But when running with default features, they WILL be listed — so we only
    // assert the positive case here. The no-default-features test below checks
    // the negative case.
}

/// When the capability axes are off, their builtins must not appear.
#[cfg(not(any(feature = "subprocess", feature = "tokens")))]
#[tokio::test]
async fn sandbox_no_native_builtins() {
    let k = sandbox_kernel().await;
    let r = k.execute("help builtins").await.expect("execute failed");
    assert!(r.ok());
    let out = r.text_out();
    assert!(!out.contains("tokens"), "tokens should not be listed without native");
    assert!(!out.contains(" bg "), "bg should not be listed without native");
    assert!(!out.contains(" fg "), "fg should not be listed without native");
    assert!(!out.contains("which"), "which should not be listed without native");
}

#[tokio::test]
async fn sandbox_wc_pipeline() {
    let k = sandbox_kernel().await;
    let r = k.execute("echo hello | wc -c").await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "6"); // "hello\n" = 6 bytes
}

#[tokio::test]
async fn sandbox_sed() {
    let k = sandbox_kernel().await;
    let r = k.execute(r#"echo "hello world" | sed "s/world/kaish/""#).await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "hello kaish");
}

#[tokio::test]
async fn sandbox_grep() {
    let k = sandbox_kernel().await;
    let r = k.execute(r#"printf "apple\nbanana\ncherry" | grep "an""#).await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "banana");
}

#[tokio::test]
async fn sandbox_diff() {
    let k = sandbox_kernel().await;
    // Write two files, diff them
    let r = k.execute(r#"echo "a" > /tmp/a.txt; echo "b" > /tmp/b.txt; diff /tmp/a.txt /tmp/b.txt"#)
        .await.expect("execute failed");
    // diff returns exit code 1 when files differ
    assert_eq!(r.code, 1);
    assert!(r.text_out().contains("a") || r.text_out().contains("b"));
}

#[tokio::test]
async fn sandbox_base64_roundtrip() {
    let k = sandbox_kernel().await;
    let r = k.execute(r#"echo "secret" | base64 | base64 -d"#).await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "secret");
}

#[tokio::test]
async fn sandbox_checksum() {
    let k = sandbox_kernel().await;
    let r = k.execute(r#"echo -n "hello" | checksum --sha256"#).await.expect("execute failed");
    assert!(r.ok());
    // SHA-256 of "hello"
    assert!(r.text_out().trim().starts_with("2cf24dba"));
}

// ── Synthetic /dev (DevFs) ──────────────────────────────────────────────────
// In NoLocal mode the host's real /dev is unreachable, so /dev/null and
// /dev/zero are software-backed. See crates/kaish-vfs/src/dev.rs.

#[tokio::test]
async fn sandbox_dev_null_is_a_sink() {
    let k = sandbox_kernel().await;
    // Writing succeeds and discards; reading back is empty — proving it is a
    // real sink, not the in-memory file the old "/" MemoryFs would have made.
    let r = k.execute("echo hello > /dev/null; cat /dev/null").await.expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert_eq!(r.text_out().trim(), "");
}

#[tokio::test]
async fn sandbox_dev_zero_counted_read() {
    let k = sandbox_kernel().await;
    // head -c N pushes the byte count down to DevFs, which yields exactly N
    // zero bytes. Counting via wc -c keeps NULs out of the assertion.
    let r = k.execute("head -c 8 /dev/zero | wc -c").await.expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert_eq!(r.text_out().trim(), "8");
}

#[tokio::test]
async fn sandbox_dev_zero_whole_read_is_loud_error() {
    let k = sandbox_kernel().await;
    // Reading the whole endless device is unbounded — it must fail loudly,
    // not hang or silently truncate.
    let r = k.execute("cat /dev/zero").await.expect("execute failed");
    assert!(!r.ok(), "expected cat /dev/zero to fail; out={:?}", r.text_out());
    assert!(
        r.err.contains("head -c") || r.err.contains("endless"),
        "error should name the fix: {}",
        r.err
    );
}

#[tokio::test]
async fn sandbox_dev_lists_devices() {
    let k = sandbox_kernel().await;
    let r = k.execute("ls /dev").await.expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    let out = r.text_out().into_owned();
    let entries: Vec<&str> = out.trim().lines().collect();
    assert!(entries.contains(&"null"), "expected null in ls /dev: {:?}", entries);
    assert!(entries.contains(&"zero"), "expected zero in ls /dev: {:?}", entries);
}

#[tokio::test]
async fn sandbox_dev_unknown_device_not_found() {
    let k = sandbox_kernel().await;
    // A genuinely unknown device is not found.
    let r = k.execute("cat /dev/sda").await.expect("execute failed");
    assert!(!r.ok(), "expected /dev/sda to be absent");
}

// ── dd + /dev/urandom — the binary-data north-star ──────────────────────────

#[tokio::test]
async fn sandbox_dd_urandom_to_dev_null() {
    let k = sandbox_kernel().await;
    // Counted read of an endless device terminates at exactly bs*count.
    let r = k
        .execute("dd if=/dev/urandom of=/dev/null bs=1024 count=10")
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert!(r.err.contains("10240 bytes copied"), "status: {}", r.err);
}

#[tokio::test]
async fn sandbox_dd_roundtrip_file_size_exact() {
    let k = sandbox_kernel().await;
    // Random → file (binary survives the write); read it back with dd (no
    // count) → exact byte count, no UTF-8 mangling anywhere.
    let r = k
        .execute(
            "dd if=/dev/urandom of=/tmp/rand.bin bs=1024 count=10; \
             dd if=/tmp/rand.bin of=/dev/null",
        )
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert!(r.err.contains("10240 bytes copied"), "readback size: {}", r.err);
}

#[tokio::test]
async fn sandbox_dd_entropy_differs() {
    let k = sandbox_kernel().await;
    // Two independent draws must differ — the source is actually random.
    let r = k
        .execute(
            "dd if=/dev/urandom of=/tmp/a bs=32 count=1 && \
             dd if=/dev/urandom of=/tmp/b bs=32 count=1 && \
             [[ \"$(checksum --sha256 /tmp/a | cut -d ' ' -f 1)\" != \
                \"$(checksum --sha256 /tmp/b | cut -d ' ' -f 1)\" ]]",
        )
        .await
        .expect("execute failed");
    assert!(r.ok(), "two random draws should differ; stderr: {}", r.err);
}

#[tokio::test]
async fn sandbox_binary_survives_pipe_roundtrip() {
    let k = sandbox_kernel().await;
    // 16 random bytes → base64 (text) → base64 -d (binary through the pipe) →
    // wc -c. If the pipe mangled the bytes, the count wouldn't be 16.
    let r = k
        .execute("dd if=/dev/urandom bs=16 count=1 | base64 | base64 -d | wc -c")
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert_eq!(r.text_out().trim(), "16");
}

#[tokio::test]
async fn sandbox_text_tool_loud_errors_on_binary_stdin() {
    let k = sandbox_kernel().await;
    // 0xFF is always invalid UTF-8. A text tool handed it must fail loud, not
    // silently lossy-decode (corruption). `xxd -r -p` produces the raw byte.
    for cmd in [
        "echo ff | xxd -r -p | grep x",
        "echo ff | xxd -r -p | sed 's/a/b/'",
        "echo ff | xxd -r -p | sort",
        "echo ff | xxd -r -p | jq .",
    ] {
        let r = k.execute(cmd).await.expect("execute failed");
        assert!(!r.ok(), "`{cmd}` should fail loud on binary, got out={:?}", r.text_out());
    }
}

#[tokio::test]
async fn sandbox_binary_aware_tools_accept_binary_stdin() {
    let k = sandbox_kernel().await;
    // The byte-aware tools still consume the same 0xFF byte fine.
    let r = k.execute("echo ff | xxd -r -p | base64").await.expect("execute failed");
    assert!(r.ok(), "base64 should accept binary: {}", r.err);
    assert_eq!(r.text_out().trim(), "/w==", "base64 of 0xFF");

    let r = k.execute("echo ff | xxd -r -p | wc -c").await.expect("execute failed");
    assert!(r.ok());
    assert_eq!(r.text_out().trim(), "1");
}

#[tokio::test]
async fn sandbox_cat_last_stage_keeps_binary() {
    let k = sandbox_kernel().await;
    // `cat` as the last pipeline stage must not lossy-decode piped binary.
    let r = k
        .execute("echo ff | xxd -r -p | cat | wc -c")
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert_eq!(r.text_out().trim(), "1");
}

#[tokio::test]
async fn sandbox_kaish_last_keeps_binary() {
    let k = sandbox_kernel().await;
    // kaish-last must preserve a binary previous result, not lossy-decode it.
    // base64 of the raw 0xFF byte is "/w=="; a lossy U+FFFD would be "77+9".
    let r = k
        .execute("echo ff | xxd -r -p; kaish-last | base64")
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert!(r.text_out().contains("/w=="), "kaish-last kept the byte: {:?}", r.text_out());
    assert!(!r.text_out().contains("77+9"), "must not be lossy-decoded: {:?}", r.text_out());
}

#[tokio::test]
async fn sandbox_subst_capture_preserves_binary_byte_count() {
    let k = sandbox_kernel().await;
    // $() of a binary command yields a Bytes value (1 byte), not a 3-byte
    // U+FFFD mangle. base64 it back out to observe exactly one byte.
    let r = k
        .execute("x=$(echo ff | xxd -r -p); kaish-vars --json")
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    // The var renders via the bytes placeholder, proving it stayed binary.
    assert!(r.text_out().contains("binary"), "x should be binary: {}", r.text_out());
}

#[tokio::test]
async fn sandbox_for_over_binary_is_loud_error() {
    let k = sandbox_kernel().await;
    // A loud error surfaces as an Err from execute (or a non-zero result).
    let r = k
        .execute("for x in $(echo ff | xxd -r -p); do echo $x; done")
        .await;
    assert!(
        r.is_err() || !r.unwrap().ok(),
        "iterating binary should fail loud"
    );
}

#[tokio::test]
async fn sandbox_binary_in_string_interpolation_is_loud_error() {
    let k = sandbox_kernel().await;
    // Command substitution producing binary directly inside a string must error.
    let r = k.execute("echo \"val=$(echo ff | xxd -r -p)\"").await;
    assert!(
        r.is_err() || !r.unwrap().ok(),
        "binary in a string should fail loud, not splice U+FFFD"
    );
}

#[tokio::test]
async fn sandbox_head_c_on_piped_binary() {
    let k = sandbox_kernel().await;
    // `head -c 4` on a piped binary stream must keep exactly 4 raw bytes — not
    // lossy-decode them (which would inflate the count via U+FFFD).
    let r = k
        .execute("dd if=/dev/urandom bs=16 count=1 | head -c 4 | wc -c")
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert_eq!(r.text_out().trim(), "4");
}

#[tokio::test]
async fn sandbox_binary_merge_stdout_to_stderr_is_loud_error() {
    let k = sandbox_kernel().await;
    // Folding binary stdout into text stderr (1>&2) must fail loud, not mangle.
    let r = k
        .execute("dd if=/dev/urandom bs=16 count=1 1>&2")
        .await
        .expect("execute failed");
    assert!(!r.ok(), "expected a loud error for binary 1>&2");
}

#[tokio::test]
async fn sandbox_binary_pipe_to_xxd() {
    let k = sandbox_kernel().await;
    // 4 zero bytes through the pipe into xxd -p → eight hex zeros.
    let r = k
        .execute("dd if=/dev/zero bs=4 count=1 | xxd -p")
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert_eq!(r.text_out().trim(), "00000000");
}

#[tokio::test]
async fn sandbox_binary_redirect_then_cmp() {
    let k = sandbox_kernel().await;
    // Redirect a binary result to a file (raw bytes, no lossy decode), then
    // cmp it against itself — identical, exit 0.
    let r = k
        .execute("dd if=/dev/urandom bs=16 count=1 > /tmp/r.bin && cmp /tmp/r.bin /tmp/r.bin")
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    // And the file is exactly 16 bytes (binary-safe size check via dd).
    let size = k.execute("dd if=/tmp/r.bin of=/dev/null").await.expect("execute failed");
    assert!(size.err.contains("16 bytes copied"), "size: {}", size.err);
}

#[tokio::test]
async fn sandbox_dd_zero_counted() {
    let k = sandbox_kernel().await;
    let r = k
        .execute("dd if=/dev/zero of=/dev/null bs=512 count=2")
        .await
        .expect("execute failed");
    assert!(r.ok(), "stderr: {}", r.err);
    assert!(r.err.contains("1024 bytes copied"), "status: {}", r.err);
}
