//! Kernel-routed tests for grep's rg-style search features:
//! `--ftype` / `--ftype-not` (file-type filtering), `--ftype-list`, and
//! `--hidden`. Driven through `kernel.execute()` so flag binding, repeatable
//! `Json(Array)` accumulation, and dispatch all run the real path.
//!
//! The walker engine (`WalkOptions.types`) is already unit-tested in
//! kaish-glob; these pin the builtin *surface* — that the flags reach the
//! walker, that an unknown type is loud, and that repeatable `--ftype` works.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use tempfile::tempdir;

use common::{kernel_at, run};

/// A small mixed-language tree: every file contains the same needle so the
/// only thing that varies a match is the file-type filter.
fn mixed_tree(dir: &std::path::Path) {
    fs::write(dir.join("main.rs"), "let needle = 1;\n").expect("write rs");
    fs::write(dir.join("util.py"), "needle = 1\n").expect("write py");
    fs::write(dir.join("README.md"), "the needle doc\n").expect("write md");
}

#[tokio::test]
async fn ftype_rust_only_searches_rust_files() {
    let dir = tempdir().unwrap();
    mixed_tree(dir.path());
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -r needle . --ftype rust").await;
    assert_eq!(code, 0, "match expected; out={out:?}");
    assert!(out.contains("main.rs"), "rust file must match: {out:?}");
    assert!(!out.contains("util.py"), "python file must be filtered out: {out:?}");
    assert!(!out.contains("README.md"), "markdown file must be filtered out: {out:?}");
}

#[tokio::test]
async fn ftype_not_excludes_the_type() {
    let dir = tempdir().unwrap();
    mixed_tree(dir.path());
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -r needle . --ftype-not rust").await;
    assert_eq!(code, 0, "match expected; out={out:?}");
    assert!(!out.contains("main.rs"), "rust file must be excluded: {out:?}");
    assert!(out.contains("util.py"), "python file must remain: {out:?}");
    assert!(out.contains("README.md"), "markdown file must remain: {out:?}");
}

/// Repeatable `--ftype` accumulates (the `Json(Array)` binder path), unioning
/// the selected types.
#[tokio::test]
async fn ftype_repeatable_unions_types() {
    let dir = tempdir().unwrap();
    mixed_tree(dir.path());
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -r needle . --ftype rust --ftype py").await;
    assert_eq!(code, 0, "match expected; out={out:?}");
    assert!(out.contains("main.rs"), "rust selected: {out:?}");
    assert!(out.contains("util.py"), "python selected: {out:?}");
    assert!(!out.contains("README.md"), "markdown not selected: {out:?}");
}

/// An unknown type name is a loud usage error (exit 2), never a silent empty
/// match.
#[tokio::test]
async fn ftype_unknown_is_loud() {
    let dir = tempdir().unwrap();
    mixed_tree(dir.path());
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute("grep -r needle . --ftype definitely-not-a-type")
        .await
        .expect("execute");
    assert_eq!(result.code, 2, "unknown type must exit 2; out={:?}", result.text_out());
    assert!(
        result.err.contains("definitely-not-a-type") || result.err.contains("unknown file type"),
        "error must name the bad type: {:?}",
        result.err
    );
}

/// `--ftype-list` emits the TYPE→globs table and exits 0 with no pattern.
#[tokio::test]
async fn ftype_list_emits_table() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep --ftype-list").await;
    assert_eq!(code, 0, "ftype-list must exit 0; out={out:?}");
    assert!(out.contains("rust"), "list must include rust: {out:?}");
    assert!(out.contains("*.rs"), "list must show rust's globs: {out:?}");
}

/// `--ftype-list` is valid JSON-shaped too (kernel `--json` path).
#[tokio::test]
async fn ftype_list_json_shape() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep --ftype-list --json").await;
    assert_eq!(code, 0, "out={out:?}");
    let v: serde_json::Value = serde_json::from_str(&out).expect("valid json");
    assert!(v.is_array(), "table --json is an array of rows: {out:?}");
    let has_rust = v
        .as_array()
        .unwrap()
        .iter()
        .any(|row| row.get("TYPE").and_then(|t| t.as_str()) == Some("rust"));
    assert!(has_rust, "rust row present: {out:?}");
}

/// `--hidden` reaches dotfiles in the recursive walk; without it they're
/// skipped (bash no-dotglob default).
#[tokio::test]
async fn hidden_flag_reaches_dotfiles() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join(".env"), "needle=secret\n").expect("write dotfile");
    fs::write(dir.path().join("visible.txt"), "needle plain\n").expect("write visible");
    let kernel = kernel_at(dir.path());

    // Default: dotfiles skipped.
    let (out, code) = run(&kernel, "grep -r needle .").await;
    assert_eq!(code, 0, "visible match expected; out={out:?}");
    assert!(out.contains("visible.txt"), "visible file matches: {out:?}");
    assert!(!out.contains(".env"), "dotfile skipped by default: {out:?}");

    // --hidden: dotfiles included.
    let (out_h, code_h) = run(&kernel, "grep -r needle . --hidden").await;
    assert_eq!(code_h, 0, "out={out_h:?}");
    assert!(out_h.contains(".env"), "--hidden must reach the dotfile: {out_h:?}");
    assert!(out_h.contains("visible.txt"), "visible still matches: {out_h:?}");
}

/// `--max-count N` caps a single-file search at N matching lines (the
/// streaming-scanner path).
#[tokio::test]
async fn max_count_caps_single_file() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f.txt"), "x\nx\nx\nx\nx\n").expect("write");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep --max-count 2 x f.txt").await;
    assert_eq!(code, 0, "match expected; out={out:?}");
    assert_eq!(out.lines().count(), 2, "must cap at 2 lines: {out:?}");
}

/// `--max-count 0` matches nothing and exits 1.
#[tokio::test]
async fn max_count_zero_matches_nothing() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f.txt"), "x\nx\nx\n").expect("write");
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("grep --max-count 0 x f.txt").await.expect("execute");
    assert_eq!(result.code, 1, "max-count 0 matches nothing → exit 1");
    assert!(result.text_out().trim().is_empty(), "no output: {:?}", result.text_out());
}

/// `--max-count` combined with `-c` reports the capped count (whole-buffer
/// path).
#[tokio::test]
async fn max_count_with_count_reports_capped() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f.txt"), "x\nx\nx\nx\nx\n").expect("write");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -c --max-count 2 x f.txt").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "2", "count must be capped to max-count: {out:?}");
}

/// `--max-count` is per-file under a recursive walk: two files of 5 matches
/// each, capped at 2, yields 4 matching lines total.
#[tokio::test]
async fn max_count_is_per_file_when_recursive() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("a.txt"), "x\nx\nx\nx\nx\n").expect("write a");
    fs::write(dir.path().join("b.txt"), "x\nx\nx\nx\nx\n").expect("write b");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -r --max-count 2 x .").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.lines().count(), 4, "2 per file across 2 files: {out:?}");
}

/// `--max-count` caps a piped (streaming-stdin) search.
#[tokio::test]
async fn max_count_caps_streaming_stdin() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    // seq 1..5 → grep '.' matches every line; cap at 2 keeps the first two.
    let (out, code) = run(&kernel, "seq 5 | grep --max-count 2 .").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.lines().count(), 2, "stdin stream capped at 2: {out:?}");
}

// ─── #105: `grep -r PATTERN FILE` (a file operand, not a dir) ────────────────
// `-r` used to unconditionally treat the operand as a walk root; a file has
// nothing "under" it, so the walk collected zero entries → 0 matches, exit 1,
// silently. A file operand under `-r` must be searched directly instead.

/// The headline reflex: `grep -r PATTERN <file>` searches the file.
#[tokio::test]
async fn recursive_flag_searches_a_file_operand() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("notes.txt"), "needle a\nhay\nneedle b\n").expect("write");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -r needle notes.txt").await;
    assert_eq!(code, 0, "a file operand under -r must match, not silently miss: {out:?}");
    assert!(out.contains("needle a"), "first match: {out:?}");
    assert!(out.contains("needle b"), "second match: {out:?}");
}

/// `grep -rc PATTERN <file>` reports the real count (was 0), like plain `-c`
/// on a single file — no filename prefix.
#[tokio::test]
async fn recursive_count_on_a_file_operand() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("notes.txt"), "needle\nneedle\nhay\nneedle\n").expect("write");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -rc needle notes.txt").await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out.trim(), "3", "count of a single file operand: {out:?}");
}

/// The uppercase `-R` alias behaves identically on a file operand.
#[tokio::test]
async fn recursive_upper_flag_searches_a_file_operand() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("notes.txt"), "needle a\nhay\n").expect("write");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -R needle notes.txt").await;
    assert_eq!(code, 0, "-R on a file operand must match: {out:?}");
    assert!(out.contains("needle a"), "match: {out:?}");
}

/// A mixed operand list — a file *and* a directory — searches the file
/// directly and walks the directory, prefixing both (multi-source display).
#[tokio::test]
async fn recursive_mixed_file_and_dir_operands() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("top.txt"), "needle top\n").expect("write top");
    fs::create_dir(dir.path().join("sub")).expect("mkdir sub");
    fs::write(dir.path().join("sub/inner.txt"), "needle inner\n").expect("write inner");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "grep -r needle top.txt sub").await;
    assert_eq!(code, 0, "out={out:?}");
    assert!(out.contains("top.txt"), "the file operand is searched: {out:?}");
    assert!(out.contains("inner.txt"), "the dir operand is walked: {out:?}");
    assert!(out.contains("needle top") && out.contains("needle inner"), "both matches: {out:?}");
}
