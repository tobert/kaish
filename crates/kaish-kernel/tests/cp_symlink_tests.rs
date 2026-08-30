//! Kernel-routed symlink-semantics tests for `cp`.
//!
//! The headline bug: `cp` only ever called `backend.stat` (which follows),
//! so every symlink — top-level source or one found while walking `-r` —
//! was dereferenced. A link inside a copied tree came out as a plain file
//! holding the target's bytes, and there was no `-P`/`-L` to ask for
//! anything else. This file pins the bash/coreutils contract: `-r` never
//! follows a symlink it finds inside the tree (recreating it as a link,
//! target string and all — even dangling, even a link-to-dir, even a
//! self-referential loop), `-P` never follows at all, `-L` always follows,
//! and the two together are a loud usage error.
//!
//! Every case drives a real command string through `kernel.execute()` and
//! asserts the on-disk reality with `std::fs`, so the whole stack runs:
//! lex → parse → validate → clap binding → builtin → backend → LocalFs.

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
        .prefix("cp-symlink-")
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

/// `symlink_metadata` (lstat) — exists without following the link.
fn lexists(p: &Path) -> bool {
    std::fs::symlink_metadata(p).is_ok()
}

fn is_symlink(p: &Path) -> bool {
    std::fs::symlink_metadata(p)
        .map(|m| m.file_type().is_symlink())
        .unwrap_or(false)
}

// ============================================================================
// Non-recursive `cp` follows a symlink source (bash/coreutils default)
// ============================================================================

#[tokio::test]
async fn cp_file_follows_symlink_source_without_r() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "precious").unwrap();
    symlink("target.txt", root.join("link.txt")).unwrap();

    let r = run(&kernel_at(root), "cp link.txt dst.txt").await;
    assert_eq!(r.code, 0, "cp link.txt dst.txt failed: {}", r.err);

    assert!(!is_symlink(&root.join("dst.txt")), "dst must be a regular file, not a link");
    assert_eq!(
        std::fs::read_to_string(root.join("dst.txt")).unwrap(),
        "precious",
        "dst must hold the target's bytes"
    );
}

// ============================================================================
// `cp -r`: never follow a symlink found inside the tree
// ============================================================================

#[tokio::test]
async fn cp_r_recreates_symlink_in_tree_same_target() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("src")).unwrap();
    std::fs::write(root.join("src/real.txt"), "data").unwrap();
    symlink("real.txt", root.join("src/alias.txt")).unwrap();

    let r = run(&kernel_at(root), "cp -r src dstdir").await;
    assert_eq!(r.code, 0, "cp -r failed: {}", r.err);

    assert!(
        is_symlink(&root.join("dstdir/alias.txt")),
        "the copied entry must still be a symlink, not a materialized file"
    );
    assert_eq!(
        std::fs::read_link(root.join("dstdir/alias.txt")).unwrap(),
        Path::new("real.txt"),
        "the recreated link must carry the same target string"
    );
    assert_eq!(
        std::fs::read_to_string(root.join("dstdir/real.txt")).unwrap(),
        "data"
    );
}

#[tokio::test]
async fn cp_r_recreates_dangling_symlink_in_tree() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("src")).unwrap();
    symlink("nowhere", root.join("src/orphan")).unwrap();

    let r = run(&kernel_at(root), "cp -r src dstdir").await;
    assert_eq!(r.code, 0, "cp -r with a dangling link must not error: {}", r.err);

    assert!(
        is_symlink(&root.join("dstdir/orphan")),
        "a dangling link inside the tree must be recreated, not skipped"
    );
    assert_eq!(
        std::fs::read_link(root.join("dstdir/orphan")).unwrap(),
        Path::new("nowhere")
    );
}

#[tokio::test]
async fn cp_r_recreates_symlink_to_dir_as_link_not_descended() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("src")).unwrap();
    std::fs::create_dir(root.join("elsewhere")).unwrap();
    std::fs::write(root.join("elsewhere/child.txt"), "outside").unwrap();
    symlink("../elsewhere", root.join("src/linkdir")).unwrap();

    let r = run(&kernel_at(root), "cp -r src dstdir").await;
    assert_eq!(r.code, 0, "cp -r failed: {}", r.err);

    assert!(
        is_symlink(&root.join("dstdir/linkdir")),
        "a link-to-dir inside the tree must be recreated as a link, never descended into"
    );
    assert!(
        !root.join("dstdir/linkdir/child.txt").exists(),
        "the copy must not have walked through the link into elsewhere/"
    );
}

#[tokio::test]
async fn cp_r_symlink_cycle_does_not_loop() {
    // A self-referential symlink inside the tree must not send the recursive
    // walk spinning (never followed, so it can't).
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("src")).unwrap();
    symlink("loop", root.join("src/loop")).unwrap();

    let r = run(&kernel_at(root), "cp -r src dstdir").await;
    assert_eq!(r.code, 0, "cp -r over a symlink cycle failed: {}", r.err);
    assert!(is_symlink(&root.join("dstdir/loop")));
    assert_eq!(
        std::fs::read_link(root.join("dstdir/loop")).unwrap(),
        Path::new("loop")
    );
}

// ============================================================================
// `-P`/`--no-dereference`: never follow, including the top-level source
// ============================================================================

#[tokio::test]
async fn cp_p_top_level_never_follows() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "precious").unwrap();
    symlink("target.txt", root.join("link.txt")).unwrap();

    let r = run(&kernel_at(root), "cp -P link.txt dst.txt").await;
    assert_eq!(r.code, 0, "cp -P link.txt dst.txt failed: {}", r.err);

    assert!(is_symlink(&root.join("dst.txt")), "cp -P must make dst a link, not a file");
    assert_eq!(
        std::fs::read_link(root.join("dst.txt")).unwrap(),
        Path::new("target.txt")
    );
}

#[tokio::test]
async fn cp_p_no_dereference_long_flag_top_level_never_follows() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "precious").unwrap();
    symlink("target.txt", root.join("link.txt")).unwrap();

    let r = run(&kernel_at(root), "cp --no-dereference link.txt dst.txt").await;
    assert_eq!(r.code, 0, "cp --no-dereference failed: {}", r.err);
    assert!(is_symlink(&root.join("dst.txt")));
}

// ============================================================================
// `-L`/`--dereference`: always follow, including inside `-r`
// ============================================================================

#[tokio::test]
async fn cp_l_dereferences_inside_r() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("src")).unwrap();
    std::fs::write(root.join("src/real.txt"), "data").unwrap();
    symlink("real.txt", root.join("src/alias.txt")).unwrap();

    let r = run(&kernel_at(root), "cp -rL src dstdir").await;
    assert_eq!(r.code, 0, "cp -rL failed: {}", r.err);

    assert!(
        !is_symlink(&root.join("dstdir/alias.txt")),
        "cp -L must materialize the link as a real file inside the tree"
    );
    assert_eq!(
        std::fs::read_to_string(root.join("dstdir/alias.txt")).unwrap(),
        "data"
    );
}

#[tokio::test]
async fn cp_l_dangling_symlink_errors() {
    let dir = tempdir();
    let root = dir.path();
    symlink("nowhere", root.join("orphan")).unwrap();

    let r = run(&kernel_at(root), "cp -L orphan dst.txt").await;
    assert_ne!(r.code, 0, "cp -L on a dangling link must error, not write an empty file");
    assert!(
        r.err.contains("orphan"),
        "the error must name the dangling link: {}",
        r.err
    );
    assert!(!lexists(&root.join("dst.txt")), "no dst must be created on error");
}

// ============================================================================
// A symlink at the DESTINATION for a plain byte-write: written through it
// ============================================================================

#[tokio::test]
async fn cp_writes_through_symlink_to_file_destination() {
    // This is the OS `open(2)` rule (bytes land at the link's target, not
    // replacing the link) and it stays: pin it so it isn't "fixed" by
    // mistake while making the -P/-L/-r paths symlink-safe.
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("src.txt"), "NEW").unwrap();
    std::fs::write(root.join("target.txt"), "OLD").unwrap();
    symlink("target.txt", root.join("link.txt")).unwrap();

    let r = run(&kernel_at(root), "cp src.txt link.txt").await;
    assert_eq!(r.code, 0, "cp src.txt link.txt failed: {}", r.err);

    assert!(
        is_symlink(&root.join("link.txt")),
        "the destination link itself must survive the write"
    );
    assert_eq!(
        std::fs::read_to_string(root.join("target.txt")).unwrap(),
        "NEW",
        "bytes must land at the link's target"
    );
}

// ============================================================================
// `-P` and `-L` together: a loud usage error
// ============================================================================

#[tokio::test]
async fn cp_p_and_l_together_errors() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("src.txt"), "x").unwrap();

    let r = run(&kernel_at(root), "cp -P -L src.txt dst.txt").await;
    assert_eq!(r.code, 2, "cp -P -L must be a usage error (exit 2): {}", r.err);
    assert!(
        r.err.contains("-P") || r.err.contains("no-dereference"),
        "error must name -P/--no-dereference: {}",
        r.err
    );
    assert!(
        r.err.contains("-L") || r.err.contains("dereference"),
        "error must name -L/--dereference: {}",
        r.err
    );
    assert!(!lexists(&root.join("dst.txt")), "no dst on a usage error");
}
