//! Kernel-routed tests verifying `diff --json` shape consistency across all
//! three code paths: identical files, differing files (full), and quiet mode.
//!
//! Bug: the three paths emitted inconsistent JSON shapes:
//!   - identical: `{old_file, new_file, differ:false, hunks:[]}` (correct)
//!   - full diff: `{old_file, new_file, hunks:[...]}` (missing `differ`)
//!   - quiet: `{old_file, new_file, differ:true}` (missing `hunks` — by design)
//!
//! The fix makes the full-diff path also emit `differ:true`, so `old_file`,
//! `new_file`, and `differ` are always present. `hunks` is present in all
//! non-`-q` paths (identical → empty array, full diff → populated array).
//! `-q` intentionally omits `hunks` (per CHANGELOG contract).
//!
//! All tests route through `kernel.execute("diff --json …")` so the kernel
//! strips `--json` and applies `apply_output_format`.

#![cfg(feature = "localfs")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;
use common::kernel_at;
use std::fs;

// ── helpers ──────────────────────────────────────────────────────────────────

/// Parse the JSON text out of an ExecResult, asserting it is valid JSON.
fn parse_json(text: &str) -> serde_json::Value {
    serde_json::from_str(text).unwrap_or_else(|e| panic!("expected valid JSON, got error {e}; text was: {text:?}"))
}

// ── identical-files path ──────────────────────────────────────────────────────

/// Identical files: `differ:false`, `hunks:[]`, and `old_file`/`new_file`
/// always present.
#[tokio::test]
async fn diff_json_identical_shape_has_differ_and_hunks() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("a.txt"), "same content\n").unwrap();
    fs::write(tmp.path().join("b.txt"), "same content\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("diff --json a.txt b.txt")
        .await
        .expect("diff should succeed");
    assert_eq!(result.code, 0, "identical files → exit 0");

    let json = parse_json(&result.text_out());

    assert_eq!(json["old_file"], "a.txt", "old_file must always be present: {json}");
    assert_eq!(json["new_file"], "b.txt", "new_file must always be present: {json}");
    assert_eq!(json["differ"], false, "identical files: differ must be false: {json}");
    assert!(
        json["hunks"].is_array(),
        "hunks must be present as an array in non-quiet mode: {json}"
    );
    assert_eq!(
        json["hunks"].as_array().unwrap().len(),
        0,
        "identical files: hunks must be empty: {json}"
    );
}

// ── full-diff path ────────────────────────────────────────────────────────────

/// Differing files (full mode): `differ:true`, `hunks:[...]` (non-empty), and
/// `old_file`/`new_file` always present.
#[tokio::test]
async fn diff_json_full_diff_shape_has_differ_and_hunks() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("old.txt"), "line1\nline2\nline3\n").unwrap();
    fs::write(tmp.path().join("new.txt"), "line1\nchanged\nline3\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("diff --json old.txt new.txt")
        .await
        .expect("diff should execute (exit 1 is not an Err)");
    assert_eq!(result.code, 1, "differing files → exit 1");

    let json = parse_json(&result.text_out());

    assert_eq!(json["old_file"], "old.txt", "old_file must always be present: {json}");
    assert_eq!(json["new_file"], "new.txt", "new_file must always be present: {json}");
    assert_eq!(
        json["differ"], true,
        "full diff: differ must be true — this is the shape-consistency bug: {json}"
    );
    assert!(
        json["hunks"].is_array(),
        "hunks must be present as an array in non-quiet mode: {json}"
    );
    assert!(
        !json["hunks"].as_array().unwrap().is_empty(),
        "differing files: hunks must be non-empty: {json}"
    );
}

// ── quiet-mode path ───────────────────────────────────────────────────────────

/// Quiet mode: `differ:true` present, `old_file`/`new_file` present.
/// Per CHANGELOG, `-q --json` intentionally omits `hunks`.
#[tokio::test]
async fn diff_json_quiet_shape_has_differ_no_hunks() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("x.txt"), "alpha\n").unwrap();
    fs::write(tmp.path().join("y.txt"), "beta\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("diff -q --json x.txt y.txt")
        .await
        .expect("diff -q should execute");
    assert_eq!(result.code, 1, "differing files → exit 1");

    let json = parse_json(&result.text_out());

    assert_eq!(json["old_file"], "x.txt", "old_file must always be present: {json}");
    assert_eq!(json["new_file"], "y.txt", "new_file must always be present: {json}");
    assert_eq!(json["differ"], true, "quiet differ: differ must be true: {json}");
    // `hunks` is intentionally absent in -q mode (per CHANGELOG contract).
    assert!(
        json.get("hunks").is_none(),
        "quiet mode must NOT include hunks (per CHANGELOG contract): {json}"
    );
}

// ── key-set consistency check ─────────────────────────────────────────────────

/// Cross-path shape contract: `old_file`, `new_file`, and `differ` are always
/// present in all three paths. This is the core contract this test suite guards.
#[tokio::test]
async fn diff_json_all_paths_have_core_keys() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("same1.txt"), "abc\n").unwrap();
    fs::write(tmp.path().join("same2.txt"), "abc\n").unwrap();
    fs::write(tmp.path().join("diff1.txt"), "abc\n").unwrap();
    fs::write(tmp.path().join("diff2.txt"), "xyz\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let scenarios: &[(&str, i64)] = &[
        ("diff --json same1.txt same2.txt", 0),      // identical
        ("diff --json diff1.txt diff2.txt", 1),      // full diff
        ("diff -q --json diff1.txt diff2.txt", 1),   // quiet
    ];

    for (cmd, expected_code) in scenarios {
        let result = kernel
            .execute(cmd)
            .await
            .expect("diff should execute");
        assert_eq!(result.code, *expected_code, "exit code for `{cmd}`");

        let json = parse_json(&result.text_out());
        for key in &["old_file", "new_file", "differ"] {
            assert!(
                json.get(*key).is_some(),
                "`{key}` must be present in all `diff --json` shapes; cmd=`{cmd}`; json={json}"
            );
        }
    }
}
