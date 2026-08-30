//! Kernel-routed tests for the `-L` / `-h` file test (is a symbolic link).
//!
//! Both spellings mean: `lstat` succeeds AND the entry is a symlink. A
//! dangling link is TRUE (the link itself exists even though its target
//! doesn't) — that is the whole point of `-L` vs `-e`. A regular file, a
//! directory, and a missing path are FALSE.
//!
//! Every case drives real command strings through `kernel.execute()` so the
//! whole stack runs: lex -> parse -> validate -> interpreter / `test`
//! builtin -> `file_test`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
// symlink setup is unix-only.
#![cfg(all(feature = "localfs", unix))]

use std::os::unix::fs::symlink;
use std::path::Path;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("file-test-symlink-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

fn kernel_at(dir: &Path) -> Kernel {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_trash(false);
    Kernel::new(config).expect("kernel")
}

async fn run(kernel: &Kernel, script: &str) -> ExecResult {
    kernel.execute(script).await.expect("kernel execute")
}

#[tokio::test]
async fn bracket_l_true_for_link_to_file() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), b"hi").expect("write target");
    symlink(root.join("target.txt"), root.join("link")).expect("symlink");

    let kernel = kernel_at(root);
    let r = run(&kernel, "[[ -L link ]]").await;
    assert_eq!(r.code, 0, "[[ -L link ]] should be true: {}", r.err);
}

#[tokio::test]
async fn bracket_h_true_for_link_to_file() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), b"hi").expect("write target");
    symlink(root.join("target.txt"), root.join("link")).expect("symlink");

    let kernel = kernel_at(root);
    let r = run(&kernel, "[[ -h link ]]").await;
    assert_eq!(r.code, 0, "[[ -h link ]] should be true: {}", r.err);
}

#[tokio::test]
async fn dangling_link_is_l_true_but_e_false() {
    let dir = tempdir();
    let root = dir.path();
    symlink(root.join("does-not-exist"), root.join("dangling")).expect("symlink");

    let kernel = kernel_at(root);
    let l = run(&kernel, "[[ -L dangling ]]").await;
    assert_eq!(l.code, 0, "[[ -L dangling ]] should be true: {}", l.err);
    let e = run(&kernel, "[[ -e dangling ]]").await;
    assert_eq!(e.code, 1, "[[ -e dangling ]] should be false");
}

#[tokio::test]
async fn bracket_l_false_for_regular_file() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("file.txt"), b"hi").expect("write file");

    let kernel = kernel_at(root);
    let r = run(&kernel, "[[ -L file.txt ]]").await;
    assert_eq!(r.code, 1, "[[ -L file.txt ]] should be false");
}

#[tokio::test]
async fn bracket_l_false_for_directory() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("adir")).expect("mkdir");

    let kernel = kernel_at(root);
    let r = run(&kernel, "[[ -L adir ]]").await;
    assert_eq!(r.code, 1, "[[ -L adir ]] should be false");
}

#[tokio::test]
async fn bracket_l_false_for_missing_path() {
    let dir = tempdir();
    let root = dir.path();

    let kernel = kernel_at(root);
    let r = run(&kernel, "[[ -L nope ]]").await;
    assert_eq!(r.code, 1, "[[ -L nope ]] should be false");
}

#[tokio::test]
async fn bracket_not_l_true_for_regular_file() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("file.txt"), b"hi").expect("write file");

    let kernel = kernel_at(root);
    let r = run(&kernel, "[[ ! -L file.txt ]]").await;
    assert_eq!(r.code, 0, "[[ ! -L file.txt ]] should be true: {}", r.err);
}

#[tokio::test]
async fn test_builtin_l_true_for_link_false_for_file() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), b"hi").expect("write target");
    symlink(root.join("target.txt"), root.join("link")).expect("symlink");

    let kernel = kernel_at(root);
    let link = run(&kernel, "test -L link").await;
    assert_eq!(link.code, 0, "test -L link should be true: {}", link.err);
    let file = run(&kernel, "test -L target.txt").await;
    assert_eq!(file.code, 1, "test -L target.txt should be false");
}

#[tokio::test]
async fn link_to_directory_is_l_true_and_d_true() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("realdir")).expect("mkdir");
    symlink(root.join("realdir"), root.join("dirlink")).expect("symlink");

    let kernel = kernel_at(root);
    let l = run(&kernel, "[[ -L dirlink ]]").await;
    assert_eq!(l.code, 0, "[[ -L dirlink ]] should be true (lstat sees the link): {}", l.err);
    let d = run(&kernel, "[[ -d dirlink ]]").await;
    assert_eq!(d.code, 0, "[[ -d dirlink ]] should be true (stat follows the link): {}", d.err);
}

#[tokio::test]
async fn bracket_l_does_not_trip_unknown_flag_error() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), b"hi").expect("write target");
    symlink(root.join("target.txt"), root.join("link")).expect("symlink");

    let kernel = kernel_at(root);
    let r = run(&kernel, "[[ -L link ]]").await;
    assert!(
        !r.err.to_lowercase().contains("unknown") && !r.err.to_lowercase().contains("unrecognized"),
        "unexpected error for -L: {}",
        r.err
    );
}
