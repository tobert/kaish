//! Kernel-routed tests: the START OPERAND of `find`, `mv`, and `ls` must be
//! classified with `lstat`, matching `crates/kaish-vfs/src/traits.rs` and
//! `crates/kaish-tool-api/src/backend.rs`.
//!
//! A cross-model review found three builtins that still classify their
//! operand with `stat` (follows the final symlink) even though the same
//! builtin already uses `lstat` for entries it discovers while walking:
//!
//! - `find`: the start path was `stat`'d, so a symlink operand reported the
//!   target's kind, a dangling link errored "No such file or directory"
//!   instead of listing as a link, and a link to a directory was descended
//!   into instead of reported as a leaf (`find.rs`).
//! - `mv`: the cross-mount directory fallback (`move_dir_recursive`) read
//!   each non-directory child through `read`, which follows a symlink child
//!   and copies the *target's* bytes instead of recreating the link
//!   (`mv.rs`).
//! - `ls`: the operand was `stat`'d, so `ls -l link` always showed `-` and
//!   never a target, and a dangling link operand errored instead of
//!   rendering as a link (`ls.rs`).
//!
//! Every case drives real command strings through `kernel.execute()` so the
//! whole stack runs: lex -> parse -> validate -> clap binding -> builtin ->
//! backend -> LocalFs. Fixture idiom copied from
//! crates/kaish-kernel/tests/rm_mv_symlink_safety_tests.rs.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
// repl() mounts the real filesystem; symlink setup is unix-only.
#![cfg(all(feature = "localfs", unix))]

use std::os::unix::fs::symlink;
use std::path::Path;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("operand-symlink-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// Trash forced OFF unless a test opts in via `set -o trash`.
fn kernel_at(dir: &Path) -> Kernel {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_trash(false);
    Kernel::new(config).expect("kernel")
}

async fn run(kernel: &Kernel, script: &str) -> ExecResult {
    kernel.execute(script).await.expect("kernel execute")
}

// ============================================================================
// A — find: the start operand is lstat'd, not stat'd
// ============================================================================

#[tokio::test]
async fn find_type_l_on_a_link_operand_prints_it() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("real.txt"), "data").unwrap();
    symlink("real.txt", root.join("link")).unwrap();

    let r = run(&kernel_at(root), "find link -type l").await;
    assert_eq!(r.code, 0, "find failed: {}", r.err);
    assert_eq!(r.text_out().trim(), "link");
}

#[tokio::test]
async fn find_type_f_on_a_link_to_a_file_operand_prints_nothing() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("real.txt"), "data").unwrap();
    symlink("real.txt", root.join("link")).unwrap();

    let r = run(&kernel_at(root), "find link -type f").await;
    assert_eq!(r.code, 0, "find failed: {}", r.err);
    assert_eq!(r.text_out().trim(), "", "a link to a file must not match -type f");
}

#[tokio::test]
async fn find_on_a_dirlink_operand_prints_only_the_link_and_does_not_recurse() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("realdir")).unwrap();
    std::fs::write(root.join("realdir/child.txt"), "keepme").unwrap();
    symlink("realdir", root.join("dirlink")).unwrap();

    let r = run(&kernel_at(root), "find dirlink").await;
    assert_eq!(r.code, 0, "find failed: {}", r.err);
    assert_eq!(
        r.text_out().trim(),
        "dirlink",
        "a dir-symlink operand must be reported as a leaf, never descended into"
    );
}

#[tokio::test]
async fn find_on_a_dangling_link_operand_prints_it_not_an_error() {
    let dir = tempdir();
    let root = dir.path();
    symlink("nowhere", root.join("dangling")).unwrap();

    let r = run(&kernel_at(root), "find dangling").await;
    assert_eq!(r.code, 0, "find on a dangling-link operand must succeed: {}", r.err);
    assert_eq!(r.text_out().trim(), "dangling");
}

#[tokio::test]
async fn find_on_a_missing_path_still_errors() {
    let dir = tempdir();
    let root = dir.path();

    let r = run(&kernel_at(root), "find nope").await;
    assert_ne!(r.code, 0, "a genuinely missing path must still error");
    assert!(r.err.contains("No such file or directory"), "{}", r.err);
}
