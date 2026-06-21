//! Glued short-flag values on clap-based builtins (`cut -f1`, `head -c5`).
//!
//! POSIX/coreutils muscle memory glues a value onto a single-char short flag:
//! `cut -f1`, `head -c5`, `tail -c20`, `cut -f1-3`, `grep -A1`. The separated
//! form (`-f 1`) always worked; the glued form used to be split into bogus
//! flags (`-f` + `-1`) and errored `unexpected argument '-1'`. The kernel's
//! arg binder now recognizes a value-taking first char and treats the rest of
//! the token as its value, while a run of bare bool flags (`ls -la`) still
//! splits into individual flags.

#![cfg(feature = "localfs")]

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
async fn combined_bools_then_glued_value_flag() {
    // `-inA1`: bools `i`,`n` stack, then value-taking `A` glues its value `1`.
    // Before the fix, `A` was treated as a bool and `1`... had nowhere to go.
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("f.txt"), "alpha\nMATCH\nbeta\n").unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "grep -inA1 match f.txt").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("MATCH"), "case-insensitive match missing: {out}");
    assert!(out.contains("beta"), "after-context line missing: {out}");
    assert!(out.contains("2:"), "line numbers (-n) missing: {out}");
}

#[tokio::test]
async fn combined_bools_then_value_flag_takes_next_positional() {
    // `grep -ivC 3` — the headline case from issues.md. `i`,`v` are bools and
    // `C` is value-taking as the LAST char, so it must consume the next
    // positional `3` (context). Before the fix `C` was a bool and `3` was a
    // stray positional → arity error.
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("f.txt"), "alpha\nMATCH\nbeta\n").unwrap();
    let kernel = kernel_at(tmp.path());

    // Use -n (not -v) so the assertion is deterministic; the binding path for
    // `C`-as-last-char is identical.
    let (out, code) = run(&kernel, "grep -inC 1 match f.txt").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("MATCH"), "match missing: {out}");
    assert!(out.contains("alpha"), "before-context missing: {out}");
    assert!(out.contains("beta"), "after-context missing: {out}");
}

#[tokio::test]
async fn glued_first_char_repeatable_accumulates() {
    // `sed -e1d -e2d`: the GLUED first-char form of a repeatable flag (`-e`)
    // must accumulate both expressions (delete lines 1 and 2 of `1\n2\n3` → "3"),
    // not keep only the last (which left "1\n3"). Matches the separated
    // `-e 1d -e 2d`. (code-review finding: the first-char glued arm did a plain
    // `named.insert` and ignored `repeatable`, clobbering earlier values.)
    // NB: piped (no file operand) on purpose — the file-positional form
    // (`sed -e1d f`) is separately blocked by the schema-less validation
    // arg-builder; tracked in issues.md.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "seq 1 3 | sed -e1d -e2d").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out.trim(), "3", "both -e expressions must apply: {out}");
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
