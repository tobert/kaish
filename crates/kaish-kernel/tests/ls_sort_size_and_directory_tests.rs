//! Kernel-routed tests for `ls -S` (largest first) and `ls -d` (list the
//! directory itself, not its contents).

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::{kernel_at, run};
use tempfile::tempdir;

/// Names sort a_small, b_big, c_mid; sizes sort b_big, c_mid, a_small.
fn build_sized(dir: &std::path::Path) {
    fs::write(dir.join("a_small"), "1").unwrap();
    fs::write(dir.join("b_big"), "x".repeat(100)).unwrap();
    fs::write(dir.join("c_mid"), "x".repeat(10)).unwrap();
}

fn build_tree(dir: &std::path::Path) {
    fs::create_dir_all(dir.join("sub")).unwrap();
    fs::create_dir_all(dir.join("other/inner")).unwrap();
    fs::write(dir.join("sub/mid.txt"), "m").unwrap();
    fs::write(dir.join("top.txt"), "t").unwrap();
}

#[tokio::test]
async fn ls_s_sorts_largest_first() {
    let dir = tempdir().unwrap();
    build_sized(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -S").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out.lines().collect::<Vec<_>>(), ["b_big", "c_mid", "a_small"]);
}

#[tokio::test]
async fn ls_s_reverse_sorts_smallest_first() {
    let dir = tempdir().unwrap();
    build_sized(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -Sr").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out.lines().collect::<Vec<_>>(), ["a_small", "c_mid", "b_big"]);
}

#[tokio::test]
async fn ls_d_lists_the_directory_not_its_contents() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -d sub").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out, "sub");
}

#[tokio::test]
async fn ls_d_without_operand_lists_dot() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -d").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out, ".");
}

#[tokio::test]
async fn ls_d_lists_each_operand_by_name() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -d sub other top.txt").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out.lines().collect::<Vec<_>>(), ["other", "sub", "top.txt"]);
}

#[tokio::test]
async fn ls_d_with_glob_lists_matches_themselves() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -d [so]*").await;
    assert_eq!(code, 0, "{out}");
    assert!(!out.contains("mid.txt") && !out.contains("inner"), "{out}");
    assert!(out.contains("sub") && out.contains("other"), "{out}");
}

#[tokio::test]
async fn ls_d_beats_recursive() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -dR other").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out, "other");
}

#[tokio::test]
async fn ls_ld_shows_one_long_row_for_the_directory() {
    let dir = tempdir().unwrap();
    build_tree(dir.path());
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "ls -ld sub").await;
    assert_eq!(code, 0, "{out}");
    assert_eq!(out.lines().count(), 1, "{out}");
    assert!(out.contains("sub") && !out.contains("mid.txt"), "{out}");
}

#[tokio::test]
async fn ls_d_missing_operand_still_fails() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("ls -d nope").await.unwrap();
    assert_ne!(result.code, 0);
    assert!(result.err.contains("nope"), "{}", result.err);
}
