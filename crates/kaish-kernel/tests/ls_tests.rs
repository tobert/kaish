//! Kernel-routed `ls` tests.
//!
//! `ls` carries a lot of responsibility (globs, multiple paths, sort flags,
//! recursion, dotfile hiding) and historically its unit tests called
//! `Ls.execute()` directly with hand-built `ToolArgs` — bypassing the
//! kernel's glob pre-expansion and flag canonicalization. That bypass let a
//! real bug ship green: `ls crates/*/Cargo.toml` lists only the first match
//! because the kernel expands the glob into N positionals but `ls` reads only
//! `positional[0]` (docs/issues.md, P2).
//!
//! These tests drive real command strings through `kernel.execute()` over a
//! `tempfile::tempdir()` root, so they exercise the same path a REPL/MCP user
//! hits. See `common::kernel_at` / `common::run`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use std::fs;

use common::{kernel_at, run};
use tempfile::tempdir;

/// Write `name` (relative to `dir`) with the given contents, creating parents.
fn touch(dir: &std::path::Path, name: &str, contents: &str) {
    let path = dir.join(name);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent dirs");
    }
    fs::write(path, contents).expect("write file");
}

// ---------------------------------------------------------------------------
// Baseline behavior (these should pass today — regression guards)
// ---------------------------------------------------------------------------

#[tokio::test]
async fn ls_empty_dir_is_empty() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls").await;
    assert_eq!(code, 0, "ls on empty dir should succeed");
    assert!(out.is_empty(), "empty dir should list nothing, got: {out:?}");
}

#[tokio::test]
async fn ls_single_file_shows_name() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "solo.txt", "hi");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls solo.txt").await;
    assert_eq!(code, 0);
    assert!(out.contains("solo.txt"), "expected filename, got: {out:?}");
}

#[tokio::test]
async fn ls_directory_lists_all_entries() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "");
    touch(dir.path(), "b.txt", "");
    touch(dir.path(), "c.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls").await;
    assert_eq!(code, 0);
    for name in ["a.txt", "b.txt", "c.txt"] {
        assert!(out.contains(name), "missing {name} in: {out:?}");
    }
}

#[tokio::test]
async fn ls_hides_dotfiles_by_default() {
    let dir = tempdir().unwrap();
    touch(dir.path(), ".hidden", "");
    touch(dir.path(), "visible.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls").await;
    assert_eq!(code, 0);
    assert!(out.contains("visible.txt"), "got: {out:?}");
    assert!(!out.contains(".hidden"), "dotfile should be hidden: {out:?}");
}

#[tokio::test]
async fn ls_all_flag_shows_dotfiles() {
    let dir = tempdir().unwrap();
    touch(dir.path(), ".hidden", "");
    touch(dir.path(), "visible.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -a").await;
    assert_eq!(code, 0);
    assert!(out.contains(".hidden"), "-a should reveal dotfile: {out:?}");
    assert!(out.contains("visible.txt"), "got: {out:?}");
}

#[tokio::test]
async fn ls_nonexistent_path_fails() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "ls does_not_exist").await;
    assert_ne!(code, 0, "ls of a missing path should fail");
}

#[tokio::test]
async fn ls_long_format_includes_file() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "data.txt", "0123456789");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -l data.txt").await;
    assert_eq!(code, 0);
    assert!(out.contains("data.txt"), "long format missing name: {out:?}");
    assert!(out.contains("10"), "long format should show size 10: {out:?}");
}

// ---------------------------------------------------------------------------
// Multiple paths / glob expansion — the kernel-contract cases the old
// direct-`.execute()` unit tests could not see. EXPECTED TO FAIL until `ls`
// iterates all positionals (docs/issues.md P2: "ls <glob> only lists the
// first match under kernel pre-expansion").
// ---------------------------------------------------------------------------

#[tokio::test]
async fn ls_multiple_explicit_files_lists_all() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "first.txt", "");
    touch(dir.path(), "second.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls first.txt second.txt").await;
    assert_eq!(code, 0, "ls of two files should succeed: {out:?}");
    assert!(out.contains("first.txt"), "missing first.txt: {out:?}");
    assert!(out.contains("second.txt"), "missing second.txt: {out:?}");
}

#[tokio::test]
async fn ls_glob_lists_all_matches() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "one.rs", "");
    touch(dir.path(), "two.rs", "");
    touch(dir.path(), "three.rs", "");
    touch(dir.path(), "ignore.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls *.rs").await;
    assert_eq!(code, 0, "glob ls should succeed: {out:?}");
    for name in ["one.rs", "two.rs", "three.rs"] {
        assert!(out.contains(name), "glob dropped {name}: {out:?}");
    }
    assert!(!out.contains("ignore.txt"), "glob matched non-.rs: {out:?}");
}

#[tokio::test]
async fn ls_glob_in_subdir_lists_all_matches() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "sub/alpha.txt", "");
    touch(dir.path(), "sub/beta.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls sub/*.txt").await;
    assert_eq!(code, 0, "subdir glob ls should succeed: {out:?}");
    assert!(out.contains("alpha.txt"), "missing alpha.txt: {out:?}");
    assert!(out.contains("beta.txt"), "missing beta.txt: {out:?}");
}

#[tokio::test]
async fn ls_single_glob_match_works() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "only.rs", "");
    touch(dir.path(), "skip.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls *.rs").await;
    assert_eq!(code, 0);
    assert!(out.contains("only.rs"), "got: {out:?}");
    assert!(!out.contains("skip.txt"), "got: {out:?}");
}

// ---------------------------------------------------------------------------
// Sort / recursion flags through the real flag-canonicalization path.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn ls_recursive_includes_nested_entries() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "top.txt", "");
    touch(dir.path(), "nested/inner.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -R").await;
    assert_eq!(code, 0);
    assert!(out.contains("top.txt"), "missing top-level entry: {out:?}");
    assert!(out.contains("inner.txt"), "recursive missed nested: {out:?}");
}

#[tokio::test]
async fn ls_reverse_sort_orders_descending() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "aaa.txt", "");
    touch(dir.path(), "zzz.txt", "");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -r").await;
    assert_eq!(code, 0);
    let a = out.find("aaa.txt").expect("aaa present");
    let z = out.find("zzz.txt").expect("zzz present");
    assert!(z < a, "reverse sort should put zzz before aaa: {out:?}");
}
