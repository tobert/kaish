//! Kernel-routed tests for glob's file-type features: `--ftype` /
//! `--ftype-not` / `--ftype-list`, plus a regression that glob's existing
//! `-t` (entry-kind, fd-style) still works alongside the new file-type axis.
//!
//! Shares the `kaish-glob::build_file_types` engine with grep; these pin the
//! glob *surface*.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use tempfile::tempdir;

use common::{kernel_at, run};

fn mixed_tree(dir: &std::path::Path) {
    fs::write(dir.join("main.rs"), "fn main() {}\n").expect("write rs");
    fs::write(dir.join("util.py"), "x = 1\n").expect("write py");
    fs::write(dir.join("README.md"), "# doc\n").expect("write md");
}

#[tokio::test]
async fn ftype_selects_only_that_type() {
    let dir = tempdir().unwrap();
    mixed_tree(dir.path());
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "glob '**/*' --ftype rust").await;
    assert_eq!(code, 0, "out={out:?}");
    assert!(out.contains("main.rs"), "rust file listed: {out:?}");
    assert!(!out.contains("util.py"), "python filtered: {out:?}");
    assert!(!out.contains("README.md"), "markdown filtered: {out:?}");
}

#[tokio::test]
async fn ftype_not_excludes_that_type() {
    let dir = tempdir().unwrap();
    mixed_tree(dir.path());
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "glob '**/*' --ftype-not rust").await;
    assert_eq!(code, 0, "out={out:?}");
    assert!(!out.contains("main.rs"), "rust excluded: {out:?}");
    assert!(out.contains("util.py"), "python remains: {out:?}");
    assert!(out.contains("README.md"), "markdown remains: {out:?}");
}

#[tokio::test]
async fn ftype_unknown_is_loud() {
    let dir = tempdir().unwrap();
    mixed_tree(dir.path());
    let kernel = kernel_at(dir.path());

    let result = kernel
        .execute("glob '**/*' --ftype definitely-not-a-type")
        .await
        .expect("execute");
    assert_eq!(result.code, 2, "unknown type → exit 2; out={:?}", result.text_out());
    assert!(
        result.err.contains("definitely-not-a-type") || result.err.contains("unknown file type"),
        "error names the bad type: {:?}",
        result.err
    );
}

#[tokio::test]
async fn ftype_list_emits_table() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "glob --ftype-list").await;
    assert_eq!(code, 0, "out={out:?}");
    assert!(out.contains("rust"), "list includes rust: {out:?}");
    assert!(out.contains("*.rs"), "list shows rust's globs: {out:?}");
}

/// Regression: glob's `-t` (entry-kind, fd-style) is untouched by the new
/// file-type axis. `-t d` lists directories only.
#[tokio::test]
async fn entry_kind_dash_t_still_works() {
    let dir = tempdir().unwrap();
    fs::create_dir(dir.path().join("sub")).expect("mkdir");
    fs::write(dir.path().join("sub/main.rs"), "fn main() {}\n").expect("write");
    fs::write(dir.path().join("top.rs"), "fn x() {}\n").expect("write");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "glob '**/*' -t d").await;
    assert_eq!(code, 0, "out={out:?}");
    assert!(out.contains("sub"), "directory listed: {out:?}");
    assert!(!out.contains("main.rs"), "files excluded by -t d: {out:?}");
    assert!(!out.contains("top.rs"), "files excluded by -t d: {out:?}");
}

/// `-t` (entry-kind) and `--ftype` (file-type) are independent flags, not
/// aliases. A file-type filter narrows *files*; directories pass it untouched
/// (the property that keeps recursive type-filtered walks traversable). So:
/// - default entry-kind (files) + `--ftype rust` → only the rust file
/// - `-t d` + `--ftype rust` → the directory still lists (ftype no-ops on dirs)
#[tokio::test]
async fn entry_kind_and_ftype_compose_independently() {
    let dir = tempdir().unwrap();
    fs::create_dir(dir.path().join("sub")).expect("mkdir");
    fs::write(dir.path().join("main.rs"), "fn main() {}\n").expect("write");
    let kernel = kernel_at(dir.path());

    // Files of type rust: finds main.rs, not the directory.
    let (files, fcode) = run(&kernel, "glob '**/*' --ftype rust").await;
    assert_eq!(fcode, 0);
    assert!(files.contains("main.rs"), "rust file found: {files:?}");
    assert!(!files.contains("sub"), "dir not listed in files mode: {files:?}");

    // Dirs-only + a file-type: the directory passes (ftype doesn't gate dirs).
    let (dirs, dcode) = run(&kernel, "glob '**/*' -t d --ftype rust").await;
    assert_eq!(dcode, 0, "dirs still list under a file-type filter: {dirs:?}");
    assert!(dirs.contains("sub"), "directory passes the file-type filter: {dirs:?}");
    assert!(!dirs.contains("main.rs"), "files excluded by -t d: {dirs:?}");
}
