//! Kernel-routed tests for `readlink` and `realpath` — symlink resolution.
//!
//! Covers:
//!  - `readlink -f link` resolves through symlinks, not just dot/dotdot
//!  - `realpath link` resolves through symlinks
//!  - bare `readlink non_symlink` reports "not a symbolic link" (P3)
//!  - `realpath nonexistent` exits non-zero (P2)
//!  - `readlink -f nonexistent` exits non-zero when parent doesn't exist either
//!  - `readlink -f parent/missing` succeeds (missing final component is allowed per GNU)
//!  - chained symlinks are fully resolved

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
// Symlinks are unix-only; real FS via localfs feature.
#![cfg(all(feature = "localfs", unix))]

use std::os::unix::fs::symlink;
use std::path::Path;

use kaish_kernel::{Kernel, KernelConfig};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("readlink-realpath-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

fn kernel_at(dir: &Path) -> Kernel {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_latch(false)
        .with_trash(false);
    Kernel::new(config).expect("kernel")
}

// ============================================================================
// readlink -f — must canonicalize through symlinks
// ============================================================================

#[tokio::test]
async fn readlink_f_resolves_symlink_to_absolute_target() {
    let dir = tempdir();
    let root = dir.path();
    // target.txt at root
    std::fs::write(root.join("target.txt"), "hello").unwrap();
    // link.txt -> target.txt (relative)
    symlink("target.txt", root.join("link.txt")).unwrap();

    let k = kernel_at(root);
    let r = k.execute("readlink -f link.txt").await.unwrap();

    assert_eq!(r.code, 0, "readlink -f failed: {}", r.err);
    let out = r.text_out().trim().to_string();
    // must resolve to the real absolute path of target.txt
    let expected = root.join("target.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected, "readlink -f must resolve through symlink");
}

#[tokio::test]
async fn readlink_f_resolves_chained_symlinks() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("real.txt"), "content").unwrap();
    // a -> b -> real.txt
    symlink("real.txt", root.join("b.txt")).unwrap();
    symlink("b.txt", root.join("a.txt")).unwrap();

    let k = kernel_at(root);
    let r = k.execute("readlink -f a.txt").await.unwrap();

    assert_eq!(r.code, 0, "readlink -f chained failed: {}", r.err);
    let out = r.text_out().trim().to_string();
    let expected = root.join("real.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected, "readlink -f must follow chained symlinks");
}

#[tokio::test]
async fn readlink_f_resolves_symlink_in_subdir() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("sub")).unwrap();
    std::fs::write(root.join("sub").join("real.txt"), "data").unwrap();
    // link at root points to sub/real.txt
    symlink("sub/real.txt", root.join("link.txt")).unwrap();

    let k = kernel_at(root);
    let r = k.execute("readlink -f link.txt").await.unwrap();

    assert_eq!(r.code, 0, "readlink -f subdir failed: {}", r.err);
    let out = r.text_out().trim().to_string();
    let expected = root.join("sub").join("real.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

#[tokio::test]
async fn readlink_f_resolves_absolute_symlink() {
    let dir = tempdir();
    let root = dir.path();
    let target_path = root.join("target.txt");
    std::fs::write(&target_path, "hello").unwrap();
    // absolute symlink
    symlink(&target_path, root.join("abslink.txt")).unwrap();

    let k = kernel_at(root);
    let r = k.execute("readlink -f abslink.txt").await.unwrap();

    assert_eq!(r.code, 0, "readlink -f absolute link failed: {}", r.err);
    let out = r.text_out().trim().to_string();
    let expected = target_path.to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

#[tokio::test]
async fn readlink_f_allows_missing_final_component() {
    // GNU readlink -f resolves as far as possible; if the final component
    // doesn't exist but the parent does, it still returns a canonical path.
    let dir = tempdir();
    let root = dir.path();

    let k = kernel_at(root);
    // "nofile" doesn't exist but root does — should succeed and return root/nofile
    let r = k.execute("readlink -f nofile").await.unwrap();

    assert_eq!(r.code, 0, "readlink -f missing final component should succeed: {}", r.err);
    let out = r.text_out().trim().to_string();
    let expected = root.join("nofile").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

#[tokio::test]
async fn readlink_f_nonexistent_parent_fails() {
    // If the parent directory doesn't exist, readlink -f should fail.
    let dir = tempdir();
    let root = dir.path();

    let k = kernel_at(root);
    let r = k.execute("readlink -f nosuchdir/nofile").await.unwrap();

    assert_ne!(r.code, 0, "readlink -f with nonexistent parent should fail");
}

// ============================================================================
// realpath — must canonicalize through symlinks and error on nonexistent paths
// ============================================================================

#[tokio::test]
async fn realpath_resolves_symlink() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "hello").unwrap();
    symlink("target.txt", root.join("link.txt")).unwrap();

    let k = kernel_at(root);
    let r = k.execute("realpath link.txt").await.unwrap();

    assert_eq!(r.code, 0, "realpath failed: {}", r.err);
    let out = r.text_out().trim().to_string();
    let expected = root.join("target.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected, "realpath must resolve through symlink");
}

#[tokio::test]
async fn realpath_errors_on_nonexistent_path() {
    // GNU realpath (without -m) exits 1 when the path doesn't exist.
    let dir = tempdir();
    let root = dir.path();

    let k = kernel_at(root);
    let r = k.execute("realpath nosuchfile.txt").await.unwrap();

    assert_ne!(r.code, 0, "realpath on nonexistent path should fail (exit {})", r.code);
    assert!(
        r.err.contains("No such file") || r.err.contains("not found") || r.err.contains("no such"),
        "realpath error message should mention missing file, got: {}",
        r.err
    );
}

#[tokio::test]
async fn realpath_resolves_chained_symlinks() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("real.txt"), "content").unwrap();
    symlink("real.txt", root.join("b.txt")).unwrap();
    symlink("b.txt", root.join("a.txt")).unwrap();

    let k = kernel_at(root);
    let r = k.execute("realpath a.txt").await.unwrap();

    assert_eq!(r.code, 0, "realpath chained failed: {}", r.err);
    let out = r.text_out().trim().to_string();
    let expected = root.join("real.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

#[tokio::test]
async fn realpath_normalizes_dotdot() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("sub")).unwrap();
    std::fs::write(root.join("sub").join("file.txt"), "x").unwrap();

    let k = kernel_at(root);
    let r = k.execute("realpath sub/../sub/file.txt").await.unwrap();

    assert_eq!(r.code, 0, "realpath dotdot failed: {}", r.err);
    let out = r.text_out().trim().to_string();
    let expected = root.join("sub").join("file.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

// ============================================================================
// bare readlink — non-symlink must error "not a symbolic link"  (P3)
// ============================================================================

#[tokio::test]
async fn readlink_on_regular_file_says_not_a_symlink() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("regular.txt"), "data").unwrap();

    let k = kernel_at(root);
    let r = k.execute("readlink regular.txt").await.unwrap();

    assert_ne!(r.code, 0, "readlink on regular file should fail");
    assert!(
        r.err.contains("not a symbolic link"),
        "error must say 'not a symbolic link', got: {}",
        r.err
    );
}

#[tokio::test]
async fn readlink_on_directory_says_not_a_symlink() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("mydir")).unwrap();

    let k = kernel_at(root);
    let r = k.execute("readlink mydir").await.unwrap();

    assert_ne!(r.code, 0, "readlink on directory should fail");
    assert!(
        r.err.contains("not a symbolic link"),
        "error must say 'not a symbolic link', got: {}",
        r.err
    );
}

// ============================================================================
// bare readlink — symlink still works correctly
// ============================================================================

#[tokio::test]
async fn readlink_on_symlink_returns_raw_target() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "content").unwrap();
    symlink("target.txt", root.join("link.txt")).unwrap();

    let k = kernel_at(root);
    let r = k.execute("readlink link.txt").await.unwrap();

    assert_eq!(r.code, 0, "readlink on symlink failed: {}", r.err);
    assert_eq!(r.text_out().trim(), "target.txt");
}
