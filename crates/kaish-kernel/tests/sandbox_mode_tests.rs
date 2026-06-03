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
async fn sandbox_test_builtin_cannot_probe_host() {
    let k = sandbox_kernel().await;
    // test -r should route through VFS, not the real filesystem.
    // /etc/passwd exists on the host but not in MemoryFs.
    let r = k.execute("test -r /etc/passwd").await.expect("execute failed");
    assert!(!r.ok(), "test -r /etc/passwd should be false in sandbox (file doesn't exist in VFS)");

    let r = k.execute("test -w /etc/shadow").await.expect("execute failed");
    assert!(!r.ok(), "test -w /etc/shadow should be false in sandbox");

    // But files written to VFS should be readable/writable
    let r = k.execute("echo data > /tmp/probe && test -r /tmp/probe").await.expect("execute failed");
    assert!(r.ok(), "test -r should work for VFS files");
}

#[tokio::test]
async fn sandbox_external_commands_blocked() {
    let k = sandbox_kernel().await;
    // Use a command name that is definitely not a builtin
    let r = k.execute("/usr/bin/curl https://example.com").await.expect("execute failed");
    assert!(!r.ok());
    // Isolated mode blocks external commands — either "not found" or path resolution fails
    assert!(
        r.code == 127 || !r.ok(),
        "external command should fail in sandbox: code={} out={:?} err={:?}", r.code, r.text_out(), r.err
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
#[cfg(not(any(feature = "subprocess", feature = "git", feature = "tokens")))]
#[tokio::test]
async fn sandbox_no_native_builtins() {
    let k = sandbox_kernel().await;
    let r = k.execute("help builtins").await.expect("execute failed");
    assert!(r.ok());
    let out = r.text_out();
    assert!(!out.contains(" git "), "git should not be listed without native");
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
