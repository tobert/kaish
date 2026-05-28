//! Kernel-routed coverage for builtins that take multiple positional paths,
//! globs, or stdin — the contract surface the old direct-`.execute()` unit
//! tests bypass (see `ls_tests.rs` for the bug that bypass hid).
//!
//! The 2026-05-28 multi-positional sweep fixed cp/mv/cut/sort/stat/dirname/
//! realpath/readlink/cat/wc to read every positional, but those fixes had no
//! kernel-routed test locking them in. These do: real command strings through
//! `kernel.execute()` over a `tempfile::tempdir()` root.

mod common;

use std::fs;

use common::{kernel_at, run};
use tempfile::tempdir;

fn touch(dir: &std::path::Path, name: &str, contents: &str) {
    let path = dir.join(name);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent dirs");
    }
    fs::write(path, contents).expect("write file");
}

// --- cat: multiple files and globs ----------------------------------------

#[tokio::test]
async fn cat_concatenates_multiple_files_in_order() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "alpha\n");
    touch(dir.path(), "b.txt", "beta\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cat a.txt b.txt").await;
    assert_eq!(code, 0, "cat of two files should succeed: {out:?}");
    let a = out.find("alpha").expect("alpha present");
    let b = out.find("beta").expect("beta present");
    assert!(a < b, "cat should preserve argument order: {out:?}");
}

#[tokio::test]
async fn cat_glob_concatenates_all_matches() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "one.log", "first\n");
    touch(dir.path(), "two.log", "second\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cat *.log").await;
    assert_eq!(code, 0);
    assert!(out.contains("first"), "missing first: {out:?}");
    assert!(out.contains("second"), "missing second: {out:?}");
}

// --- wc: multiple files yield per-file rows + a total ----------------------

#[tokio::test]
async fn wc_multi_file_reports_each_and_total() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "f1.txt", "a\nb\n");
    touch(dir.path(), "f2.txt", "c\nd\ne\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "wc -l f1.txt f2.txt").await;
    assert_eq!(code, 0, "wc of two files should succeed: {out:?}");
    assert!(out.contains("f1.txt"), "missing f1 row: {out:?}");
    assert!(out.contains("f2.txt"), "missing f2 row: {out:?}");
    // 2 + 3 = 5 total lines; the total row must be present (the bug the
    // realworld suite found was multi-file wc dropping all but one row).
    assert!(out.contains('5'), "missing total of 5 lines: {out:?}");
}

// --- stat: single file produces a populated table --------------------------

#[tokio::test]
async fn stat_reports_size_for_file() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "data.bin", "0123456789"); // 10 bytes
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "stat data.bin").await;
    assert_eq!(code, 0, "stat should succeed: {out:?}");
    assert!(out.contains("data.bin"), "missing file name: {out:?}");
    assert!(out.contains("10"), "missing size 10: {out:?}");
}

// --- cp / mv: copy, multi-source-to-dir, rename ----------------------------

#[tokio::test]
async fn cp_copies_file_contents() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "src.txt", "payload");
    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "cp src.txt dst.txt").await;
    assert_eq!(code, 0);
    let copied = fs::read_to_string(dir.path().join("dst.txt")).expect("dst exists");
    assert_eq!(copied, "payload", "copy should preserve contents");
}

#[tokio::test]
async fn cp_multiple_sources_into_directory() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "A");
    touch(dir.path(), "b.txt", "B");
    fs::create_dir(dir.path().join("dest")).unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cp a.txt b.txt dest").await;
    assert_eq!(code, 0, "multi-source cp should succeed: {out:?}");
    assert!(dir.path().join("dest/a.txt").exists(), "a.txt not copied");
    assert!(dir.path().join("dest/b.txt").exists(), "b.txt not copied");
}

#[tokio::test]
async fn cp_multiple_sources_into_directory_trailing_slash() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "a.txt", "A");
    touch(dir.path(), "b.txt", "B");
    fs::create_dir(dir.path().join("dest")).unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cp a.txt b.txt dest/").await;
    assert_eq!(code, 0, "multi-source cp to DST/ should succeed: {out:?}");
    assert!(dir.path().join("dest/a.txt").exists(), "a.txt not copied");
    assert!(dir.path().join("dest/b.txt").exists(), "b.txt not copied");
}

#[tokio::test]
async fn mv_renames_file() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "old.txt", "stuff");
    let kernel = kernel_at(dir.path());
    let (_out, code) = run(&kernel, "mv old.txt new.txt").await;
    assert_eq!(code, 0);
    assert!(!dir.path().join("old.txt").exists(), "old name should be gone");
    assert!(dir.path().join("new.txt").exists(), "new name should exist");
}

// --- path-arithmetic builtins ----------------------------------------------

#[tokio::test]
async fn dirname_and_basename_split_path() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (dn, dc) = run(&kernel, "dirname /x/y/z.txt").await;
    assert_eq!(dc, 0);
    assert_eq!(dn, "/x/y", "dirname mismatch: {dn:?}");
    let (bn, bc) = run(&kernel, "basename /x/y/z.txt").await;
    assert_eq!(bc, 0);
    assert_eq!(bn, "z.txt", "basename mismatch: {bn:?}");
}

// --- text filters over real files ------------------------------------------

#[tokio::test]
async fn sort_orders_lines() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "lines.txt", "charlie\nalpha\nbravo\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "sort lines.txt").await;
    assert_eq!(code, 0);
    let a = out.find("alpha").expect("alpha");
    let b = out.find("bravo").expect("bravo");
    let c = out.find("charlie").expect("charlie");
    assert!(a < b && b < c, "sort order wrong: {out:?}");
}

#[tokio::test]
async fn cut_selects_field() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "csv.txt", "one,two,three\n");
    let kernel = kernel_at(dir.path());
    // The delimiter must be quoted: a bare `,` (`cut -d ,` or `cut -d,`) is a
    // parse error in kaish — see docs/issues.md (bash-porting ergonomics gap).
    let (out, code) = run(&kernel, "cut -d ',' -f 2 csv.txt").await;
    assert_eq!(code, 0, "cut should succeed: {out:?}");
    assert_eq!(out, "two", "cut should select the second field: {out:?}");
}

#[tokio::test]
async fn head_and_tail_bound_lines() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "five.txt", "l1\nl2\nl3\nl4\nl5\n");
    let kernel = kernel_at(dir.path());
    let (head, hc) = run(&kernel, "head -n 2 five.txt").await;
    assert_eq!(hc, 0);
    assert!(head.contains("l1") && head.contains("l2"), "head: {head:?}");
    assert!(!head.contains("l5"), "head leaked last line: {head:?}");
    let (tail, tc) = run(&kernel, "tail -n 2 five.txt").await;
    assert_eq!(tc, 0);
    assert!(tail.contains("l4") && tail.contains("l5"), "tail: {tail:?}");
    assert!(!tail.contains("l1"), "tail leaked first line: {tail:?}");
}

#[tokio::test]
async fn tr_translates_from_pipeline() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "echo hello | tr l L").await;
    assert_eq!(code, 0, "tr should succeed: {out:?}");
    assert_eq!(out, "heLLo", "tr translation wrong: {out:?}");
}
