//! Kernel-routed tests: `find -type l` and `stat` on a symlink.
//!
//! The headline bug (both builtins): a symlink was classified by `stat`
//! (follows), so it was reported as the *target's* kind — `find` had no
//! `-type l` at all, and `stat` on a link described the target with no way
//! to say "this is a link". The fix routes classification through `lstat`
//! (find: crates/kaish-kernel/src/tools/builtin/find.rs; stat:
//! crates/kaish-kernel/src/tools/builtin/stat.rs), matching GNU `find`
//! without `-L` and GNU `stat` without `-L`/`--dereference`.
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
        .prefix("find-stat-symlink-")
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
// find -type l / -type f / -type d
// ============================================================================

#[tokio::test]
async fn find_type_l_finds_a_link_and_a_dangling_link() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("real.txt"), "data").unwrap();
    symlink("real.txt", root.join("link_to_file")).unwrap();
    symlink("nowhere", root.join("dangling")).unwrap();

    let r = run(&kernel_at(root), "find . -type l").await;
    assert_eq!(r.code, 0, "find -type l failed: {}", r.err);

    let out = r.text_out();
    assert!(out.contains("link_to_file"), "link_to_file should match -type l: {out}");
    assert!(out.contains("dangling"), "a dangling link should still match -type l: {out}");
    assert!(!out.contains("real.txt"), "a real file must not match -type l: {out}");
}

#[tokio::test]
async fn find_type_f_excludes_a_link_to_a_file() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("real.txt"), "data").unwrap();
    symlink("real.txt", root.join("link_to_file")).unwrap();

    let r = run(&kernel_at(root), "find . -type f").await;
    assert_eq!(r.code, 0, "find -type f failed: {}", r.err);

    let out = r.text_out();
    assert!(out.contains("real.txt"), "a real file must match -type f: {out}");
    assert!(!out.contains("link_to_file"), "a link to a file must not match -type f: {out}");
}

#[tokio::test]
async fn find_type_d_excludes_a_link_to_a_dir() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("realdir")).unwrap();
    symlink("realdir", root.join("linkdir")).unwrap();

    let r = run(&kernel_at(root), "find . -type d").await;
    assert_eq!(r.code, 0, "find -type d failed: {}", r.err);

    let out = r.text_out();
    assert!(out.contains("realdir"), "a real dir must match -type d: {out}");
    assert!(!out.contains("linkdir"), "a link to a dir must not match -type d: {out}");
}

#[tokio::test]
async fn find_does_not_descend_a_link_to_a_dir() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::create_dir(root.join("realdir")).unwrap();
    std::fs::write(root.join("realdir/child.txt"), "keepme").unwrap();
    symlink("realdir", root.join("linkdir")).unwrap();

    let r = run(&kernel_at(root), "find .").await;
    assert_eq!(r.code, 0, "find failed: {}", r.err);

    let out = r.text_out();
    assert!(out.contains("realdir/child.txt"), "child reachable through the real dir: {out}");
    assert!(
        !out.contains("linkdir/child.txt"),
        "child must not be reported through the symlinked path: {out}"
    );
}

#[tokio::test]
async fn find_type_unknown_letter_names_l_in_the_error() {
    let dir = tempdir();
    let root = dir.path();

    let r = run(&kernel_at(root), "find . -type x").await;
    assert_ne!(r.code, 0, "an unknown -type letter must fail");
    assert!(r.err.contains("'l'"), "error should list 'l' as a valid type: {}", r.err);
}

// ============================================================================
// stat default (lstat) vs -L (dereference)
// ============================================================================

#[tokio::test]
async fn stat_default_on_a_link_reports_symlink_kind_and_target() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "hello").unwrap();
    symlink("target.txt", root.join("link")).unwrap();

    let r = run(&kernel_at(root), "stat link").await;
    assert_eq!(r.code, 0, "stat link failed: {}", r.err);

    let out = r.text_out();
    assert!(out.contains("symbolic link"), "default stat must report the link's own kind: {out}");
    assert!(out.contains("target.txt"), "default stat must include the link's target: {out}");
}

#[tokio::test]
async fn stat_dereference_reports_the_targets_kind() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "hello").unwrap();
    symlink("target.txt", root.join("link")).unwrap();

    let r = run(&kernel_at(root), "stat -L link").await;
    assert_eq!(r.code, 0, "stat -L link failed: {}", r.err);

    let out = r.text_out();
    assert!(out.contains("regular file"), "-L must describe the target's kind: {out}");
    assert!(!out.contains("symbolic link"), "-L must not report the link's own kind: {out}");
}

#[tokio::test]
async fn stat_dangling_link_default_ok_dereference_errors() {
    let dir = tempdir();
    let root = dir.path();
    symlink("nowhere", root.join("dangling")).unwrap();

    let default_r = run(&kernel_at(root), "stat dangling").await;
    assert_eq!(default_r.code, 0, "a dangling link IS a link; default stat must succeed: {}", default_r.err);
    assert!(default_r.text_out().contains("symbolic link"));

    let deref_r = run(&kernel_at(root), "stat -L dangling").await;
    assert_ne!(deref_r.code, 0, "-L on a dangling link must fail (no target to describe)");
    assert!(deref_r.err.contains("dangling"), "the error must name the path: {}", deref_r.err);
}

#[tokio::test]
async fn stat_json_includes_target_for_a_link_and_omits_it_for_a_file() {
    let dir = tempdir();
    let root = dir.path();
    std::fs::write(root.join("target.txt"), "hello").unwrap();
    symlink("target.txt", root.join("link")).unwrap();

    let link_r = run(&kernel_at(root), "stat link --json").await;
    assert_eq!(link_r.code, 0, "stat link --json failed: {}", link_r.err);
    let link_json: serde_json::Value = serde_json::from_str(&link_r.text_out()).expect("valid json");
    let link_row = &link_json[0];
    assert_eq!(
        link_row.get("TARGET").and_then(|v| v.as_str()),
        Some("target.txt"),
        "a symlink row must carry its TARGET: {link_json}"
    );

    let file_r = run(&kernel_at(root), "stat target.txt --json").await;
    assert_eq!(file_r.code, 0, "stat target.txt --json failed: {}", file_r.err);
    let file_json: serde_json::Value = serde_json::from_str(&file_r.text_out()).expect("valid json");
    let file_row = &file_json[0];
    assert!(
        file_row.get("TARGET").is_none(),
        "a plain file row must not carry a TARGET key: {file_json}"
    );
}
