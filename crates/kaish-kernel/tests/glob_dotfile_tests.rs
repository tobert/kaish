//! Dotfile handling in glob expansion and the `glob` builtin.
//!
//! Bash semantics without `dotglob`: a leading `.` in a filename is matched
//! only when the governing pattern segment explicitly begins with a literal
//! `.`. So `*` skips dotfiles, while `.*`, `.github`, and `.github/*` reach
//! them. `glob -a` / a hidden-inclusive walk acts like `dotglob`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use std::fs;

/// Lay down a small tree with both visible and dot-prefixed entries.
fn seed(dir: &std::path::Path) {
    fs::write(dir.join("visible.txt"), b"v").unwrap();
    fs::write(dir.join(".env"), b"e").unwrap();
    fs::write(dir.join(".npmrc"), b"n").unwrap();
    fs::create_dir(dir.join(".github")).unwrap();
    fs::write(dir.join(".github/config.yml"), b"c").unwrap();
    fs::write(dir.join(".github/.secret"), b"s").unwrap();
}

#[tokio::test]
async fn star_skips_dotfiles_in_shell_expansion() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "for f in *; do echo $f; done").await;
    assert_eq!(code, 0);
    assert!(out.contains("visible.txt"), "got: {out}");
    assert!(!out.contains(".env"), "`*` must not match dotfiles: {out}");
    assert!(!out.contains(".github"), "`*` must not match dot dirs: {out}");
}

#[tokio::test]
async fn dot_star_matches_dotfiles_in_shell_expansion() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    // The regression: `.*` previously errored with "no matches" because the
    // walker filtered dotfiles before the pattern was applied.
    let (out, code) = run(&kernel, "for f in .*; do echo $f; done").await;
    assert_eq!(code, 0, "`.*` should expand, not error: {out}");
    assert!(out.contains(".env"), "got: {out}");
    assert!(out.contains(".npmrc"), "got: {out}");
    assert!(!out.contains("visible.txt"), "`.*` must not match non-dot: {out}");
}

#[tokio::test]
async fn explicit_dotdir_glob_reaches_children() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "for f in .github/*; do echo $f; done").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("config.yml"), "got: {out}");
    // `*` inside the named dot dir still skips dot-prefixed children.
    assert!(!out.contains(".secret"), "nested dotfile must stay hidden: {out}");
}

#[tokio::test]
async fn glob_builtin_dot_star_matches_without_flag() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    // glob is files-only by default; `.env`/`.npmrc` are files.
    let (out, code) = run(&kernel, "glob '.*'").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains(".env"), "got: {out}");
    assert!(out.contains(".npmrc"), "got: {out}");
}

#[tokio::test]
async fn globstar_reaches_explicit_dotfile() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    fs::write(tmp.path().join(".github/.env"), b"hidden").unwrap();
    fs::create_dir(tmp.path().join("sub")).unwrap();
    fs::write(tmp.path().join("sub/.env"), b"s").unwrap();
    let kernel = kernel_at(tmp.path());

    // `**/.env` must reach `.env` at root and inside visible `sub/`, but not the
    // one buried in the hidden `.github/` dir (`**` can't traverse it).
    let (out, code) = run(&kernel, "glob '**/.env'").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("sub/.env"), "got: {out}");
    assert!(!out.contains(".github/.env"), "`**` must not enter hidden dir: {out}");
}

#[tokio::test]
async fn glob_builtin_star_hides_dotfiles() {
    let tmp = tempfile::tempdir().unwrap();
    seed(tmp.path());
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "glob '*'").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("visible.txt"), "got: {out}");
    assert!(!out.contains(".env"), "got: {out}");
}
