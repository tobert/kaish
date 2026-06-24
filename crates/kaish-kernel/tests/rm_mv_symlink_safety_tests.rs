//! Kernel-routed safety tests for `rm`/`mv` on symlinks.
//!
//! The headline bug: `rm -r <symlink-to-dir>` followed the link and deleted the
//! *target's* contents (path resolution canonicalized through the link, and the
//! recursive walk stat()'d through it). The fix routes both `rm` and `mv` through
//! the single, symlink-safe `backend.remove(path, recursive)` and resolves
//! removes/renames with `resolve_for_unlink` (parent canonicalized, final
//! component left literal) so a symlink is unlinked/renamed, never followed.
//!
//! Every case drives real command strings through `kernel.execute()` and then
//! asserts the on-disk reality with `std::fs`, so the whole stack runs: lex →
//! parse → validate → clap binding → builtin → backend → LocalFs.
//!
//! Scenario inventory adapted from a kaibo/deepseek adversarial brainstorm
//! (groups A/B/C/E/F/H). Real-FS, single-mount cases live here; cross-mount
//! mv-fallback cases need a multi-mount router (tracked separately).

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
        .prefix("rm-mv-symlink-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// Latch/trash forced OFF unless a test opts in via `set -o`.
fn kernel_at(dir: &Path) -> Kernel {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_latch(false)
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

// ============================================================================
// GROUP A — rm on symlinks: the link goes, the target stays
// ============================================================================

#[tokio::test]
async fn rm_symlink_to_file_unlinks_link_keeps_target() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "precious").unwrap();
    symlink("target.txt", root.join("link.txt")).unwrap();

    let r = run(&kernel_at(root), "rm link.txt").await;
    assert_eq!(r.code, 0, "rm link.txt failed: {}", r.err);

    assert!(!lexists(&root.join("link.txt")), "symlink should be unlinked");
    assert_eq!(
        std::fs::read_to_string(root.join("target.txt")).unwrap(),
        "precious",
        "target file must survive untouched"
    );
}

#[tokio::test]
async fn rm_r_symlink_to_dir_unlinks_link_keeps_target_tree() {
    // THE regression: `rm -r linkdir` must not descend into the target.
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("targetdir")).unwrap();
    std::fs::write(root.join("targetdir/child.txt"), "keepme").unwrap();
    std::fs::create_dir(root.join("targetdir/sub")).unwrap();
    std::fs::write(root.join("targetdir/sub/deep.txt"), "deep").unwrap();
    symlink("targetdir", root.join("linkdir")).unwrap();

    let r = run(&kernel_at(root), "rm -r linkdir").await;
    assert_eq!(r.code, 0, "rm -r linkdir failed: {}", r.err);

    assert!(!lexists(&root.join("linkdir")), "symlink should be unlinked");
    assert!(
        root.join("targetdir").is_dir(),
        "target dir must survive"
    );
    assert_eq!(
        std::fs::read_to_string(root.join("targetdir/child.txt")).unwrap(),
        "keepme",
        "target contents must survive"
    );
    assert_eq!(
        std::fs::read_to_string(root.join("targetdir/sub/deep.txt")).unwrap(),
        "deep"
    );
}

#[tokio::test]
async fn rm_symlink_to_dir_without_r_unlinks_link() {
    // A symlink lstats as a non-dir, so plain `rm` unlinks it (no -r needed).
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("targetdir")).unwrap();
    std::fs::write(root.join("targetdir/child.txt"), "keepme").unwrap();
    symlink("targetdir", root.join("linkdir")).unwrap();

    let r = run(&kernel_at(root), "rm linkdir").await;
    assert_eq!(r.code, 0, "rm linkdir failed: {}", r.err);

    assert!(!lexists(&root.join("linkdir")));
    assert_eq!(
        std::fs::read_to_string(root.join("targetdir/child.txt")).unwrap(),
        "keepme"
    );
}

#[tokio::test]
async fn rm_dangling_symlink_succeeds() {
    let dir = tempdir();
    let root = dir.path();
    symlink("nowhere", root.join("orphan")).unwrap();

    let r = run(&kernel_at(root), "rm orphan").await;
    assert_eq!(r.code, 0, "rm of dangling symlink failed: {}", r.err);
    assert!(!lexists(&root.join("orphan")));
}

#[tokio::test]
async fn rm_r_dir_containing_symlink_child_keeps_external_target() {
    // The recursive walk must lstat each child: a symlink child inside the
    // removed dir is unlinked, never followed to an external target.
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("outside")).unwrap();
    std::fs::write(root.join("outside/safe.txt"), "external").unwrap();
    std::fs::create_dir(root.join("victim")).unwrap();
    std::fs::write(root.join("victim/own.txt"), "own").unwrap();
    symlink("../outside", root.join("victim/link")).unwrap();

    let r = run(&kernel_at(root), "rm -r victim").await;
    assert_eq!(r.code, 0, "rm -r victim failed: {}", r.err);

    assert!(!lexists(&root.join("victim")), "victim dir removed");
    assert!(root.join("outside").is_dir(), "external dir survives");
    assert_eq!(
        std::fs::read_to_string(root.join("outside/safe.txt")).unwrap(),
        "external",
        "external target contents survive"
    );
}

#[tokio::test]
async fn rm_symlink_loop_to_self_unlinks() {
    // A self-referential symlink must not send the recursion (or lstat) spinning.
    let dir = tempdir();
    let root = dir.path();
    symlink("loop", root.join("loop")).unwrap();

    let r = run(&kernel_at(root), "rm -r loop").await;
    assert_eq!(r.code, 0, "rm -r loop failed: {}", r.err);
    assert!(!lexists(&root.join("loop")));
}

// ============================================================================
// GROUP B/E — normal recursion still works; confusable names
// ============================================================================

#[tokio::test]
async fn rm_r_real_dir_removes_whole_tree() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir_all(root.join("a/b/c")).unwrap();
    std::fs::write(root.join("a/x.txt"), "1").unwrap();
    std::fs::write(root.join("a/b/y.txt"), "2").unwrap();
    std::fs::write(root.join("a/b/c/z.txt"), "3").unwrap();

    let r = run(&kernel_at(root), "rm -r a").await;
    assert_eq!(r.code, 0, "rm -r a failed: {}", r.err);
    assert!(!lexists(&root.join("a")), "tree fully removed");
}

#[tokio::test]
async fn rm_r_dir_and_its_symlink_alias_both_resolved() {
    // `dir` is a real directory; `dir-link` points at it. Removing the link
    // leaves the dir; removing the dir leaves the (now-dangling) link.
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("d")).unwrap();
    std::fs::write(root.join("d/file.txt"), "data").unwrap();
    symlink("d", root.join("d-link")).unwrap();

    let r = run(&kernel_at(root), "rm -r d-link").await;
    assert_eq!(r.code, 0, "rm -r d-link failed: {}", r.err);
    assert!(!lexists(&root.join("d-link")), "alias removed");
    assert!(root.join("d").is_dir(), "real dir survives");
    assert_eq!(
        std::fs::read_to_string(root.join("d/file.txt")).unwrap(),
        "data"
    );
}

// ============================================================================
// GROUP C — multiple paths, mixed symlinks
// ============================================================================

#[tokio::test]
async fn rm_multiple_symlinks_keeps_all_targets() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("t1.txt"), "one").unwrap();
    std::fs::write(root.join("t2.txt"), "two").unwrap();
    symlink("t1.txt", root.join("l1")).unwrap();
    symlink("t2.txt", root.join("l2")).unwrap();

    let r = run(&kernel_at(root), "rm l1 l2").await;
    assert_eq!(r.code, 0, "rm l1 l2 failed: {}", r.err);
    assert!(!lexists(&root.join("l1")));
    assert!(!lexists(&root.join("l2")));
    assert_eq!(std::fs::read_to_string(root.join("t1.txt")).unwrap(), "one");
    assert_eq!(std::fs::read_to_string(root.join("t2.txt")).unwrap(), "two");
}

// ============================================================================
// GROUP F — mv on symlinks (same-mount rename path)
// ============================================================================

#[tokio::test]
async fn mv_symlink_to_file_renames_link_not_target() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "precious").unwrap();
    symlink("target.txt", root.join("link.txt")).unwrap();

    let r = run(&kernel_at(root), "mv link.txt renamed").await;
    assert_eq!(r.code, 0, "mv failed: {}", r.err);

    assert!(!lexists(&root.join("link.txt")), "old link name gone");
    // The renamed entry is still a symlink (not a copy of the target).
    let meta = std::fs::symlink_metadata(root.join("renamed")).unwrap();
    assert!(meta.file_type().is_symlink(), "renamed entry stays a symlink");
    assert_eq!(
        std::fs::read_to_string(root.join("target.txt")).unwrap(),
        "precious",
        "target file untouched"
    );
}

#[tokio::test]
async fn mv_symlink_to_dir_renames_link_not_target_tree() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("targetdir")).unwrap();
    std::fs::write(root.join("targetdir/child.txt"), "keepme").unwrap();
    symlink("targetdir", root.join("linkdir")).unwrap();

    let r = run(&kernel_at(root), "mv linkdir renamed").await;
    assert_eq!(r.code, 0, "mv failed: {}", r.err);

    assert!(!lexists(&root.join("linkdir")));
    let meta = std::fs::symlink_metadata(root.join("renamed")).unwrap();
    assert!(meta.file_type().is_symlink(), "renamed entry stays a symlink");
    assert_eq!(
        std::fs::read_to_string(root.join("targetdir/child.txt")).unwrap(),
        "keepme",
        "target tree untouched"
    );
}

#[tokio::test]
async fn mv_dangling_symlink_renames_without_error() {
    let dir = tempdir();
    let root = dir.path();
    symlink("nowhere", root.join("orphan")).unwrap();

    let r = run(&kernel_at(root), "mv orphan moved").await;
    assert_eq!(r.code, 0, "mv of dangling symlink failed: {}", r.err);
    assert!(!lexists(&root.join("orphan")));
    let meta = std::fs::symlink_metadata(root.join("moved")).unwrap();
    assert!(meta.file_type().is_symlink());
}

// ============================================================================
// GROUP H — trash interaction: a symlink never trashes its target
// ============================================================================

#[tokio::test]
async fn rm_r_symlink_to_dir_under_trash_keeps_target() {
    // With trash ON, removing a symlink must NOT trash (which would resolve the
    // real path through the link and move the *target*). It unlinks directly.
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("targetdir")).unwrap();
    std::fs::write(root.join("targetdir/child.txt"), "keepme").unwrap();
    symlink("targetdir", root.join("linkdir")).unwrap();

    let kernel = kernel_at(root);
    let enable = run(&kernel, "set -o trash").await;
    assert_eq!(enable.code, 0, "set -o trash failed: {}", enable.err);

    let r = run(&kernel, "rm -r linkdir").await;
    assert_eq!(r.code, 0, "rm -r linkdir (trash) failed: {}", r.err);

    assert!(!lexists(&root.join("linkdir")), "symlink unlinked");
    assert_eq!(
        std::fs::read_to_string(root.join("targetdir/child.txt")).unwrap(),
        "keepme",
        "trash mode must not move the link target"
    );
}
