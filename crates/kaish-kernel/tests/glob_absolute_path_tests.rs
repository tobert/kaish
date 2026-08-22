//! Kernel-routed tests: the `glob` builtin must preserve the leading `/` of
//! an absolute (anchored) pattern in its output.
//!
//! The bug: `glob '/tmp/x/*.txt'` reported `tmp/x/z.txt` — the leading `/`
//! silently dropped — while a bare glob in argv position (`echo /tmp/x/*.txt`)
//! reported the correct absolute path. Because the wrong value is never
//! flagged as an error, it flows downstream: `cat $(glob '/tmp/x/*.txt')`
//! reports "not found" for a file that exists. `--json` output carried the
//! same wrong value.
//!
//! Root cause: `glob.rs` computed `report_root = ctx.resolve_path("/")` for
//! an anchored pattern (i.e. `/`), then did
//! `p.strip_prefix(&report_root)` unconditionally — stripping the leading
//! `/` off every matched absolute path.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use tempfile::tempdir;

use common::{kernel_at, run};

fn touch(dir: &std::path::Path, name: &str) {
    let path = dir.join(name);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent dirs");
    }
    fs::write(path, b"x").expect("write file");
}

/// An absolute pattern must yield absolute results, leading `/` intact.
#[tokio::test]
async fn absolute_pattern_preserves_leading_slash() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "z.txt");
    let kernel = kernel_at(dir.path());

    let abs_pattern = format!("{}/*.txt", dir.path().display());
    let expected = format!("{}/z.txt", dir.path().display());

    let (out, code) = run(&kernel, &format!("glob '{abs_pattern}'")).await;
    assert_eq!(code, 0, "glob should succeed: {out:?}");
    assert!(
        out.contains(&expected),
        "expected absolute path {expected:?} in output, got {out:?}"
    );
    // The specific defect: the leading slash silently dropped.
    let stripped = expected.trim_start_matches('/');
    assert!(
        !out.split_whitespace().any(|line| line == stripped),
        "output must not contain the path with its leading slash stripped: {out:?}"
    );
}

/// Control: a relative pattern must still yield relative results, unchanged
/// by the absolute-path fix.
#[tokio::test]
async fn relative_pattern_stays_relative() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "z.txt");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "glob '*.txt'").await;
    assert_eq!(code, 0, "glob should succeed: {out:?}");
    assert_eq!(out, "z.txt", "relative pattern must report a bare relative name: {out:?}");
}

/// `--json` must carry the same corrected absolute value as text output.
#[tokio::test]
async fn absolute_pattern_json_preserves_leading_slash() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "z.txt");
    let kernel = kernel_at(dir.path());

    let abs_pattern = format!("{}/*.txt", dir.path().display());
    let expected_json_string = format!("\"{}/z.txt\"", dir.path().display());

    let (out, code) = run(&kernel, &format!("glob --json '{abs_pattern}'")).await;
    assert_eq!(code, 0, "glob --json should succeed: {out:?}");
    assert!(
        out.contains(&expected_json_string),
        "expected {expected_json_string:?} in --json output, got {out:?}"
    );
}

/// An absolute pattern is unaffected by the caller's cwd: after `cd`
/// elsewhere, the same absolute pattern must still report the same absolute
/// path (the per-context cwd only governs *relative* resolution).
#[tokio::test]
async fn absolute_pattern_unaffected_by_cd() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "z.txt");
    fs::create_dir_all(dir.path().join("elsewhere")).unwrap();
    let kernel = kernel_at(dir.path());

    let abs_pattern = format!("{}/*.txt", dir.path().display());
    let expected = format!("{}/z.txt", dir.path().display());

    let (out, code) = run(
        &kernel,
        &format!("cd elsewhere; glob '{abs_pattern}'"),
    )
    .await;
    assert_eq!(code, 0, "glob should succeed after cd: {out:?}");
    assert!(
        out.contains(&expected),
        "expected absolute path {expected:?} in output after cd, got {out:?}"
    );
}

/// A relative pattern after `cd` into a subdirectory reports names relative
/// to the NEW cwd, with no leading slash and no path prefix at all.
#[tokio::test]
async fn relative_pattern_after_cd_reports_subdir_relative_names() {
    let dir = tempdir().unwrap();
    fs::create_dir_all(dir.path().join("sub")).unwrap();
    touch(dir.path(), "sub/inner.txt");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "cd sub; glob '*.txt'").await;
    assert_eq!(code, 0, "glob should succeed after cd: {out:?}");
    assert_eq!(
        out, "inner.txt",
        "relative pattern after cd must report a bare name relative to the new cwd: {out:?}"
    );
}
