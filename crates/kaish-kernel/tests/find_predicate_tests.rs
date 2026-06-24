//! Kernel-routed tests for `find` predicate correctness.
//!
//! Covers the bug cluster identified in docs/issues.md (P1):
//!  (a) `find <regular-file>` should print the file path and exit 0.
//!  (b) `find -maxdepth 0` should print only the start path, not children.
//!  (c) `-mindepth N` should exclude results shallower than N.
//!  (d) `-path GLOB` / `-ipath GLOB` should match against the whole path.
//!
//! All tests use real FS under `tempfile::tempdir()` so the kernel dispatch
//! chain runs: lex → parse → validate → glob pre-expansion → dispatch → find.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use std::fs;
use tempfile::tempdir;

fn touch(dir: &std::path::Path, rel: &str, contents: &str) {
    let path = dir.join(rel);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent dirs");
    }
    fs::write(&path, contents).expect("write file");
}

/// Build a standard test tree:
/// ```
/// <tmpdir>/
///   top.txt
///   sub/
///     mid.txt
///     deep/
///       bottom.txt
/// ```
fn build_tree(dir: &std::path::Path) {
    touch(dir, "top.txt", "top\n");
    fs::create_dir_all(dir.join("sub/deep")).expect("create sub/deep");
    touch(dir, "sub/mid.txt", "mid\n");
    touch(dir, "sub/deep/bottom.txt", "bottom\n");
}

// --- (a) find on a regular file --------------------------------------------

#[tokio::test]
async fn find_regular_file_prints_itself() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "hello.txt", "hello\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "find hello.txt").await;
    assert_eq!(code, 0, "find on a regular file should exit 0, got: {out:?}");
    assert!(
        out.contains("hello.txt"),
        "find on a regular file should print the file path, got: {out:?}"
    );
}

#[tokio::test]
async fn find_regular_file_no_extra_output() {
    // A regular-file operand should print exactly that path — nothing more.
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "find top.txt").await;
    assert_eq!(code, 0, "exit code: {out:?}");
    // The output should mention top.txt but NOT sub/ children.
    assert!(out.contains("top.txt"), "file path missing: {out:?}");
    assert!(
        !out.contains("mid.txt"),
        "find on a file should not recurse: {out:?}"
    );
}

// --- (b) -maxdepth 0 -------------------------------------------------------

#[tokio::test]
async fn find_maxdepth_0_prints_start_dir_only() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    // With -maxdepth 0 the start dir itself should be printed but no children.
    let (out, code) = run(&kernel, "find . -maxdepth 0").await;
    assert_eq!(code, 0, "exit 0 expected: {out:?}");
    // "." or the dir itself should appear
    assert!(!out.is_empty(), "output should not be empty: {out:?}");
    // No children should appear
    assert!(
        !out.contains("top.txt"),
        "-maxdepth 0 must not list children, got: {out:?}"
    );
    assert!(
        !out.contains("sub"),
        "-maxdepth 0 must not list children, got: {out:?}"
    );
}

#[tokio::test]
async fn find_maxdepth_1_prints_dir_and_direct_children() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "find . -maxdepth 1").await;
    assert_eq!(code, 0, "exit code: {out:?}");
    // top.txt is a direct child (depth 1)
    assert!(out.contains("top.txt"), "depth-1 child missing: {out:?}");
    // sub/ is a direct child
    assert!(out.contains("sub"), "sub dir missing: {out:?}");
    // bottom.txt is at depth 3 — should not appear
    assert!(
        !out.contains("bottom.txt"),
        "-maxdepth 1 should not reach bottom.txt: {out:?}"
    );
}

// --- (c) -mindepth ----------------------------------------------------------

#[tokio::test]
async fn find_mindepth_1_skips_start_dir() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    // -mindepth 1 skips the start directory but includes children.
    let (out, code) = run(&kernel, "find . -mindepth 1").await;
    assert_eq!(code, 0, "exit code: {out:?}");
    // Children should appear
    assert!(out.contains("top.txt"), "top.txt should appear: {out:?}");
    // The start dir itself ("." alone) should not appear as an entry
    // (it's suppressed by mindepth=1); the output lines contain filenames, so
    // checking that "." does not appear as a standalone line is sufficient.
    let dot_line = out
        .lines()
        .any(|l| l.trim() == "." || l.trim() == "./");
    assert!(
        !dot_line,
        "-mindepth 1 should exclude the start dir itself: {out:?}"
    );
}

#[tokio::test]
async fn find_mindepth_2_skips_shallow_files() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "find . -mindepth 2").await;
    assert_eq!(code, 0, "exit code: {out:?}");
    // top.txt is at depth 1 — should be excluded
    assert!(
        !out.contains("top.txt"),
        "-mindepth 2 should exclude depth-1 files: {out:?}"
    );
    // sub/mid.txt is at depth 2 — should appear
    assert!(out.contains("mid.txt"), "mid.txt should appear: {out:?}");
    // bottom.txt is at depth 3 — should also appear (mindepth doesn't cap)
    assert!(
        out.contains("bottom.txt"),
        "bottom.txt should appear: {out:?}"
    );
}

// --- (d) -path / -ipath -----------------------------------------------------

#[tokio::test]
async fn find_path_predicate_matches_full_path() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    // -path with a glob matching the "sub" component
    let (out, code) = run(&kernel, "find . -path '*/sub/*'").await;
    assert_eq!(code, 0, "exit code: {out:?}");
    // sub/mid.txt and sub/deep/bottom.txt should match
    assert!(out.contains("mid.txt"), "mid.txt should match -path: {out:?}");
    assert!(
        out.contains("bottom.txt"),
        "bottom.txt should match -path: {out:?}"
    );
    // top.txt does NOT have /sub/ in its path
    assert!(
        !out.contains("top.txt"),
        "top.txt should not match -path '*/sub/*': {out:?}"
    );
}

#[tokio::test]
async fn find_path_predicate_no_match_returns_empty() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "find . -path '*/nonexistent/*'").await;
    assert_eq!(code, 0, "exit code: {out:?}");
    assert!(
        out.trim().is_empty(),
        "-path with no matches should produce no output, got: {out:?}"
    );
}

#[tokio::test]
async fn find_ipath_is_case_insensitive() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    // top.txt has lowercase name; -ipath with uppercase GLOB should still match.
    let (out, code) = run(&kernel, "find . -ipath '*/TOP.TXT'").await;
    assert_eq!(code, 0, "exit code: {out:?}");
    assert!(
        out.contains("top.txt"),
        "-ipath should match case-insensitively, got: {out:?}"
    );
}

#[tokio::test]
async fn find_ipath_does_not_match_wrong_case_sensitive_path() {
    // Confirm -path IS case-sensitive (unlike -ipath).
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "find . -path '*/TOP.TXT'").await;
    assert_eq!(code, 0, "exit code: {out:?}");
    assert!(
        out.trim().is_empty(),
        "-path should be case-sensitive, got: {out:?}"
    );
}
