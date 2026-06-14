//! Pre-execution validation tests for `jq` (E007 InvalidJqFilter) and
//! `sed` (E006 InvalidSedExpr).
//!
//! All tests are kernel-routed: they drive a command string through
//! `kernel.execute()` so the full lex → parse → validate → dispatch pipeline
//! runs, matching the real agent experience.

#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use std::fs;

// ============================================================================
// jq — E007 InvalidJqFilter
// ============================================================================

/// A clearly-malformed filter (`.[`) is rejected with E007 before execution.
#[tokio::test]
async fn jq_invalid_filter_blocks_with_e007() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let err = kernel
        .execute(r#"echo '{}' | jq '.['  "#)
        .await
        .expect_err("invalid jq filter should fail validation");
    let msg = err.to_string();
    assert!(msg.contains("E007"), "should name the code: {msg}");
}

/// A syntactically valid filter over real JSON passes validation and runs.
#[tokio::test]
async fn jq_valid_filter_passes_validation_and_runs() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute(r#"echo '{"x":1}' | jq '.x'"#)
        .await
        .expect("valid jq filter should pass validation");
    assert!(result.ok(), "jq .x should succeed: {:?}", result.err);
    assert!(result.text_out().contains('1'), "output should contain 1");
}

/// A filter that uses `$x` with a matching `--arg x 1` binding must NOT
/// produce E007 — the validator must compile with the declared var names.
#[tokio::test]
async fn jq_bound_var_filter_does_not_false_error() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute(r#"echo '{"foo":1}' | jq --arg x hello '.foo | tostring + $x'"#)
        .await
        .expect("filter with --arg binding should pass validation");
    assert!(result.ok(), "should succeed at runtime: {:?}", result.err);
}

/// A filter stored in a variable expands to `<dynamic>` at validation time —
/// the validator must skip and let runtime handle it.
#[tokio::test]
async fn jq_dynamic_filter_skips_validation() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // F contains a valid filter, so the dynamic path should run successfully.
    let result = kernel
        .execute(r#"F='.x'; echo '{"x":42}' | jq $F"#)
        .await
        .expect("dynamic filter should pass validation and run");
    assert!(result.ok(), "dynamic filter should succeed: {:?}", result.err);
    assert!(
        result.text_out().trim() == "42",
        "should output 42: {}",
        result.text_out()
    );
}

// ============================================================================
// sed — E006 InvalidSedExpr
// ============================================================================

/// An unknown sed command letter is rejected with E006 before execution.
#[tokio::test]
async fn sed_unknown_command_blocks_with_e006() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let err = kernel
        .execute(r#"echo hello | sed 'zzz'"#)
        .await
        .expect_err("unknown sed command should fail validation");
    let msg = err.to_string();
    assert!(msg.contains("E006"), "should name the code: {msg}");
}

/// A valid substitution expression passes validation and runs.
#[tokio::test]
async fn sed_valid_expression_passes_validation_and_runs() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute(r#"echo hello | sed 's/hello/world/'"#)
        .await
        .expect("valid sed expression should pass validation");
    assert!(result.ok(), "sed s/hello/world/ should succeed: {:?}", result.err);
    assert!(
        result.text_out().trim() == "world",
        "should output 'world': {}",
        result.text_out()
    );
}

/// A sed expression in a variable is `<dynamic>` at validation time — skip.
#[tokio::test]
async fn sed_dynamic_expression_skips_validation() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // Use a valid expression so the dynamic path runs successfully.
    let result = kernel
        .execute(r#"F='s/a/b/'; echo 'abc' | sed $F"#)
        .await
        .expect("dynamic sed expression should pass validation and run");
    assert!(result.ok(), "dynamic sed should succeed: {:?}", result.err);
    assert!(
        result.text_out().trim() == "bbc",
        "should output 'bbc': {}",
        result.text_out()
    );
}

/// A file-based test to confirm the kernel is properly rooted (no system-path
/// coupling). Creates a real file and exercises sed on it end-to-end.
#[tokio::test]
async fn sed_valid_on_real_file_passes() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("data.txt"), "hello world\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let result = kernel
        .execute("sed 's/world/kaish/' data.txt")
        .await
        .expect("sed on real file should pass validation");
    assert!(result.ok(), "sed on file should succeed: {:?}", result.err);
    assert!(
        result.text_out().trim() == "hello kaish",
        "should output 'hello kaish': {}",
        result.text_out()
    );
}
