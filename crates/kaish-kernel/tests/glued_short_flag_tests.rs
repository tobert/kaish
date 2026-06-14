//! Glued short-flag values on clap-based builtins (`cut -f1`, `head -c5`).
//!
//! POSIX/coreutils muscle memory glues a value onto a single-char short flag:
//! `cut -f1`, `head -c5`, `tail -c20`, `cut -f1-3`, `grep -A1`. The separated
//! form (`-f 1`) always worked; the glued form used to be split into bogus
//! flags (`-f` + `-1`) and errored `unexpected argument '-1'`. The kernel's
//! arg binder now recognizes a value-taking first char and treats the rest of
//! the token as its value, while a run of bare bool flags (`ls -la`) still
//! splits into individual flags.

mod common;

use common::{kernel_at, run};
use std::fs;

#[tokio::test]
async fn cut_glued_field_selects_column() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "echo 'a,b,c' | cut -d ',' -f1").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "a");
}

#[tokio::test]
async fn cut_glued_field_range() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // `-f1-3` lexes as one short-flag token (`-` is in the flag char class).
    let (out, code) = run(&kernel, "echo 'a,b,c,d' | cut -d ',' -f1-3").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "a,b,c");
}

#[tokio::test]
async fn head_glued_byte_count() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "echo 'hello world' | head -c5").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "hello");
}

#[tokio::test]
async fn tail_glued_byte_count() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // "hello\n" → last 3 bytes "lo\n", trimmed to "lo".
    let (out, code) = run(&kernel, "echo 'hello' | tail -c3").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "lo");
}

#[tokio::test]
async fn grep_glued_after_context() {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("f.txt"), "match\nafter\nother\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "grep -A1 match f.txt").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("match"), "got: {out}");
    assert!(out.contains("after"), "context line missing: {out}");
}

#[tokio::test]
async fn combined_bool_flags_still_split() {
    // `ls -la` is a run of bare bool flags, not a glued value — must keep
    // splitting into individual flags, not bind "a" as a value of "-l".
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("visible.txt"), "x").unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "ls -la").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("visible.txt"), "got: {out}");
}
