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
