//! `ls`/`find`/`glob` refuse to report a newline-bearing filename as TEXT.
//!
//! `ls` has always returned plain text from `$(ls dir)`; `find`/`glob` used
//! to carry an internal typed `.data` JSON array instead. Per POSIX/GNU/BSD
//! familiarity (a builtin without `--json` should look like the tool it's
//! named after), all three now agree on text — but text is lossy for a path
//! containing `\n`: the newline-split every consumer of that text already
//! relies on (`for f in $(cmd)`, a pipe, canonical-string rendering) turns
//! one such name into two lines naming files that do not exist.
//!
//! Measured before this guard existed, in a 2-file dir with one file named
//! `we\nird.txt`:
//!   for f in $(find dir -name '*.txt'); do n=$((n+1)); done   -> n=2 (typed, correct)
//!   for f in $(ls dir);                  do n=$((n+1)); done   -> n=3 (text, WRONG)
//!
//! These tests cover the fix: `find`/`glob` join `ls` as text, and all three
//! refuse (exit 2) rather than silently miscounting when a name they would
//! report contains a newline. `--json` is the documented, lossless way past
//! the refusal, and must keep working on the exact same directory.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use std::path::Path;

use common::kernel_at;
use tempfile::tempdir;

/// Build a directory with one plain file and one file whose name contains a
/// literal newline byte — created from Rust (`std::fs::write`), not a shell,
/// per the project's "don't rely on a shell to make the fixture" rule. Any
/// byte except `/` and NUL is legal in a Unix filename, so this is a real,
/// on-disk newline-named file, not a synthetic string.
fn newline_fixture() -> (tempfile::TempDir, String) {
    let dir = tempdir().expect("tempdir");
    fs::write(dir.path().join("normal.txt"), b"a").expect("write normal.txt");
    fs::write(dir.path().join("we\nird.txt"), b"b").expect("write newline-named file");
    let path = dir.path().display().to_string();
    (dir, path)
}

// ---------------------------------------------------------------------------
// Refusal: ls / find / glob, no --json.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn ls_refuses_newline_named_file_as_text() {
    let (_dir, path) = newline_fixture();
    let kernel = kernel_at(Path::new(&path));
    let result = kernel.execute("ls .").await.expect("execute");
    assert_eq!(result.code, 2, "ls should refuse loudly: out={:?} err={:?}", result.text_out(), result.err);
    assert!(result.err.contains("we\\nird.txt"), "error must name the escaped path: {:?}", result.err);
    assert!(result.err.contains("ls --json"), "error must point at --json: {:?}", result.err);
    assert!(result.err.starts_with("ls:"), "error must be ls-prefixed: {:?}", result.err);
}

#[tokio::test]
async fn find_refuses_newline_named_file_as_text() {
    let (_dir, path) = newline_fixture();
    let kernel = kernel_at(Path::new(&path));
    let result = kernel.execute("find . -name '*.txt'").await.expect("execute");
    assert_eq!(result.code, 2, "find should refuse loudly: out={:?} err={:?}", result.text_out(), result.err);
    assert!(result.err.contains("we\\nird.txt"), "error must name the escaped path: {:?}", result.err);
    assert!(result.err.contains("find --json"), "error must point at --json: {:?}", result.err);
    assert!(result.err.starts_with("find:"), "error must be find-prefixed: {:?}", result.err);
}

#[tokio::test]
async fn glob_refuses_newline_named_file_as_text() {
    let (_dir, path) = newline_fixture();
    let kernel = kernel_at(Path::new(&path));
    let result = kernel.execute("glob '*.txt'").await.expect("execute");
    assert_eq!(result.code, 2, "glob should refuse loudly: out={:?} err={:?}", result.text_out(), result.err);
    assert!(result.err.contains("we\\nird.txt"), "error must name the escaped path: {:?}", result.err);
    assert!(result.err.contains("glob --json"), "error must point at --json: {:?}", result.err);
    assert!(result.err.starts_with("glob:"), "error must be glob-prefixed: {:?}", result.err);
}

// ---------------------------------------------------------------------------
// --json is the documented, lossless override: unaffected by the guard.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn ls_json_succeeds_on_newline_named_file() {
    let (_dir, path) = newline_fixture();
    let kernel = kernel_at(Path::new(&path));
    let result = kernel.execute("ls . --json").await.expect("execute");
    assert!(result.ok(), "ls --json must still succeed: err={:?}", result.err);
    let parsed: serde_json::Value =
        serde_json::from_str(&result.text_out()).expect("ls --json must be valid JSON");
    let names: Vec<&str> = parsed.as_array().expect("array").iter()
        .map(|v| v.as_str().expect("string entry"))
        .collect();
    assert!(names.contains(&"we\nird.txt"), "the real newline-bearing name must survive intact: {names:?}");
    assert!(names.contains(&"normal.txt"), "the ordinary name must still be present: {names:?}");
}

#[tokio::test]
async fn find_json_succeeds_on_newline_named_file() {
    let (_dir, path) = newline_fixture();
    let kernel = kernel_at(Path::new(&path));
    let result = kernel.execute("find . -name '*.txt' --json").await.expect("execute");
    assert!(result.ok(), "find --json must still succeed: err={:?}", result.err);
    let parsed: serde_json::Value =
        serde_json::from_str(&result.text_out()).expect("find --json must be valid JSON");
    let names: Vec<&str> = parsed.as_array().expect("array").iter()
        .map(|v| v.as_str().expect("string entry"))
        .collect();
    assert!(names.iter().any(|n| n.contains("we\nird.txt")), "newline-bearing name must survive: {names:?}");
}

#[tokio::test]
async fn glob_json_succeeds_on_newline_named_file() {
    let (_dir, path) = newline_fixture();
    let kernel = kernel_at(Path::new(&path));
    let result = kernel.execute("glob '*.txt' --json").await.expect("execute");
    assert!(result.ok(), "glob --json must still succeed: err={:?}", result.err);
    let parsed: serde_json::Value =
        serde_json::from_str(&result.text_out()).expect("glob --json must be valid JSON");
    let names: Vec<&str> = parsed.as_array().expect("array").iter()
        .map(|v| v.as_str().expect("string entry"))
        .collect();
    assert!(names.iter().any(|n| n.contains("we\nird.txt")), "newline-bearing name must survive: {names:?}");
}

// ---------------------------------------------------------------------------
// find/glob now return TEXT (join ls): a clean directory (no newlines)
// still iterates once per path via the for-head's newline split.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn for_over_find_iterates_once_per_clean_path() {
    let dir = tempdir().expect("tempdir");
    fs::write(dir.path().join("a.txt"), b"a").expect("write a.txt");
    fs::write(dir.path().join("b.txt"), b"b").expect("write b.txt");
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("n=0\nfor f in $(find . -name '*.txt'); do n=$((n+1)); done\necho $n")
        .await
        .expect("execute");
    assert!(result.ok(), "find for-loop should succeed: {}", result.err);
    assert_eq!(result.text_out().trim(), "2", "should iterate exactly 2 paths, got: {}", result.text_out());
}

#[tokio::test]
async fn for_over_glob_iterates_once_per_clean_path() {
    let dir = tempdir().expect("tempdir");
    fs::write(dir.path().join("a.txt"), b"a").expect("write a.txt");
    fs::write(dir.path().join("b.txt"), b"b").expect("write b.txt");
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("n=0\nfor f in $(glob '*.txt'); do n=$((n+1)); done\necho $n")
        .await
        .expect("execute");
    assert!(result.ok(), "glob for-loop should succeed: {}", result.err);
    assert_eq!(result.text_out().trim(), "2", "should iterate exactly 2 paths, got: {}", result.text_out());
}

/// The exact miscount from the bug report, now fixed by refusal rather than
/// by silent correctness: `ls` on a newline-named dir no longer produces 3
/// items from 2 files. A substitution's failure doesn't stop the enclosing
/// script by default (matching `sh`; see docs/LANGUAGE.md "A substitution's
/// stderr belongs to the enclosing statement") — `$(ls .)` becomes an empty
/// value, so the loop runs zero times rather than three: undercounting to
/// zero, never the silent 3-from-2 corruption, and the refusal's stderr
/// still reaches the caller.
#[tokio::test]
async fn for_over_ls_on_newline_dir_refuses_instead_of_miscounting() {
    let (_dir, path) = newline_fixture();
    let kernel = kernel_at(Path::new(&path));
    let result = kernel
        .execute("n=0\nfor f in $(ls .); do n=$((n+1)); done\necho $n")
        .await
        .expect("execute");
    assert_eq!(result.text_out().trim(), "0", "must never reach the miscounted 3: {}", result.text_out());
    assert!(result.err.contains("we\\nird.txt"), "the refusal must still reach stderr: {:?}", result.err);
}

// ---------------------------------------------------------------------------
// Typed builtins are untouched: fromjson/keys/values stay typed.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn fromjson_command_subst_index_stays_typed() {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    let result = kernel
        .execute("xs=$(fromjson \"[10,20]\")\necho ${xs[0]}")
        .await
        .expect("execute");
    assert!(result.ok(), "fromjson round-trip should succeed: {}", result.err);
    assert_eq!(result.text_out().trim(), "10", "fromjson must stay typed through $(...): {}", result.text_out());
}

#[tokio::test]
async fn keys_command_subst_iterates_typed() {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    let result = kernel
        .execute(
            "r=$(fromjson '{\"a\":1,\"b\":2}')\nn=0\nfor k in $(keys $r); do n=$((n+1)); done\necho $n",
        )
        .await
        .expect("execute");
    assert!(result.ok(), "keys iteration should succeed: {}", result.err);
    assert_eq!(result.text_out().trim(), "2", "keys must stay typed: {}", result.text_out());
}
