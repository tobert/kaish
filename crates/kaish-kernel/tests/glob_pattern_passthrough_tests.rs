//! Kernel-routed tests: the `glob` builtin receives its pattern *as written*.
//!
//! The bug: an unquoted pattern (`glob **/*.rs`) was pre-expanded by the
//! kernel's argv glob expansion before the builtin ran. The builtin then read
//! the first matching path as its "pattern", silently ignored the rest, and
//! printed exactly one file — after walking the tree twice (once at bind
//! time, once inside the builtin). The builtin's own schema examples teach
//! the unquoted spelling, so agents hit this constantly.
//!
//! The fix: `ToolSchema::glob_passthrough` tells the argv binder to hand
//! bare glob patterns through as literal text for tools whose input IS the
//! pattern. Alongside it, `glob` accepts multiple patterns (its schema
//! always said "pattern(s)") instead of silently dropping extras.

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

/// The headline bug: unquoted `glob **/*.rs` must match every .rs file,
/// not bind the first pre-expanded path as the pattern and return one file.
#[tokio::test]
async fn unquoted_recursive_pattern_matches_all_files() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "src/alpha.rs");
    touch(dir.path(), "src/deep/beta.rs");
    touch(dir.path(), "README.md");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "glob **/*.rs").await;
    assert_eq!(code, 0, "glob should succeed, got: {out:?}");
    assert!(out.contains("alpha.rs"), "missing alpha.rs in: {out:?}");
    assert!(out.contains("beta.rs"), "missing nested beta.rs in: {out:?}");
    assert!(!out.contains("README.md"), "README.md must not match: {out:?}");
    assert!(
        out.lines().count() >= 2,
        "must match all .rs files, not just the first pre-expanded path: {out:?}"
    );
}

/// Unquoted and quoted spellings must be equivalent.
#[tokio::test]
async fn unquoted_and_quoted_patterns_agree() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "one.txt");
    touch(dir.path(), "two.txt");
    touch(dir.path(), "three.md");
    let kernel = kernel_at(dir.path());

    let (unquoted, code_a) = run(&kernel, "glob *.txt").await;
    let (quoted, code_b) = run(&kernel, "glob '*.txt'").await;
    assert_eq!(code_a, 0);
    assert_eq!(code_b, 0);
    assert_eq!(unquoted, quoted, "unquoted pattern must behave like quoted");
    assert!(unquoted.contains("one.txt") && unquoted.contains("two.txt"));
}

/// Multiple patterns union their matches (the schema always said
/// "pattern(s)"); previously every positional past the first was
/// silently dropped.
#[tokio::test]
async fn multiple_patterns_union_matches() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "main.rs");
    touch(dir.path(), "notes.md");
    touch(dir.path(), "data.json");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "glob *.rs *.md").await;
    assert_eq!(code, 0, "multi-pattern glob should succeed: {out:?}");
    assert!(out.contains("main.rs"), "missing *.rs match in: {out:?}");
    assert!(out.contains("notes.md"), "missing *.md match in: {out:?}");
    assert!(!out.contains("data.json"), "unrequested match in: {out:?}");
}

/// Overlapping patterns must not emit duplicate paths.
#[tokio::test]
async fn overlapping_patterns_dedupe() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "main.rs");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "glob '*.rs' 'main*'").await;
    assert_eq!(code, 0);
    let hits = out.lines().filter(|l| l.contains("main.rs")).count();
    assert_eq!(hits, 1, "main.rs must appear exactly once: {out:?}");
}

/// Strict-glob applies per pattern: one pattern with zero matches fails
/// the whole command and names the offending pattern.
#[tokio::test]
async fn zero_match_pattern_among_multiple_errors() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "main.rs");
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("glob '*.rs' '*.none'").await.expect("execute");
    assert_ne!(result.code, 0, "a no-match pattern must fail the command");
    assert!(
        result.err.contains("*.none"),
        "error must name the pattern that matched nothing: {:?}",
        result.err
    );
}

/// `set +o glob` already handed the builtin its literal pattern; that
/// spelling must keep working identically with passthrough.
#[tokio::test]
async fn noglob_mode_still_reaches_builtin() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "one.txt");
    touch(dir.path(), "two.txt");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "set +o glob\nglob *.txt").await;
    assert_eq!(code, 0, "glob must work under set +o glob: {out:?}");
    assert!(out.contains("one.txt") && out.contains("two.txt"), "got: {out:?}");
}

/// Regression guard: passthrough is scoped to tools that opt in — every
/// other command still gets shell glob expansion in argv position.
#[tokio::test]
async fn other_builtins_still_expand_argv_globs() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "one.txt");
    touch(dir.path(), "two.txt");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, "echo *.txt").await;
    assert_eq!(code, 0);
    assert!(
        out.contains("one.txt") && out.contains("two.txt"),
        "echo must still receive expanded paths, got: {out:?}"
    );
    assert!(!out.contains('*'), "pattern must not leak literally: {out:?}");
}

/// Zero matches for other commands stays a bind-time error (strict glob).
#[tokio::test]
async fn other_builtins_keep_strict_no_match_error() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "one.txt");
    let kernel = kernel_at(dir.path());

    let result = kernel.execute("echo *.none").await.expect("execute");
    assert_ne!(result.code, 0, "strict glob: zero matches must fail");
}

/// Structured output survives the change: `glob` still sets `.data` to a
/// JSON array of paths so for-loops iterate per element.
#[tokio::test]
async fn multi_pattern_result_data_is_json_array() {
    let dir = tempdir().unwrap();
    touch(dir.path(), "main.rs");
    touch(dir.path(), "notes.md");
    let kernel = kernel_at(dir.path());

    let (out, code) = run(
        &kernel,
        "for f in $(glob *.rs *.md); do echo \"got:$f\"; done",
    )
    .await;
    assert_eq!(code, 0, "for over glob output should succeed: {out:?}");
    assert!(out.contains("got:main.rs"), "missing element iteration: {out:?}");
    assert!(out.contains("got:notes.md"), "missing element iteration: {out:?}");
}
