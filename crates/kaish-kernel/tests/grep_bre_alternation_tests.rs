//! Kernel-routed tests for GNU BRE backslash-meta support in grep (issue #60).
//!
//! kaish grep is ERE-always, but commercial LLMs reflexively write the GNU BRE
//! spellings — `foo\|bar`, `a\+`, `\(...\)`, `x\{2,5\}`. These used to silently
//! match nothing (a literal `|`/`+`/…); grep now rewrites them to their ERE
//! form so the agent-idiomatic pattern works. Driven through `kernel.execute()`
//! so the real dispatch, flag-binding, and streaming paths run.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use tempfile::tempdir;

use common::{kernel_at, run};

/// The headline case: `\|` alternation matches either branch (issue #60).
#[tokio::test]
async fn bre_alternation_matches_either_branch() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "foo line\nbar line\nbaz line\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep 'foo\|bar' t.txt"#).await;
    assert_eq!(code, 0, "alternation should match; out={out:?}");
    assert!(out.contains("foo line"), "foo branch: {out:?}");
    assert!(out.contains("bar line"), "bar branch: {out:?}");
    assert!(!out.contains("baz line"), "non-branch must not match: {out:?}");
}

/// Multi-term batch — the efficiency win the issue calls out: one call, three
/// terms, instead of three single-term retries.
#[tokio::test]
async fn bre_alternation_three_terms() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "alpha\nbeta\ngamma\ndelta\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -c 'alpha\|gamma\|delta' t.txt"#).await;
    assert_eq!(code, 0, "should match; out={out:?}");
    assert_eq!(out.trim(), "3", "three of four lines match: {out:?}");
}

/// `\(...\)\+` — BRE group with a BRE quantifier.
#[tokio::test]
async fn bre_group_and_quantifier() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "abcabc\nxyz\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep '\(abc\)\+' t.txt"#).await;
    assert_eq!(code, 0, "group+quant should match; out={out:?}");
    assert!(out.contains("abcabc"), "{out:?}");
    assert!(!out.contains("xyz"), "{out:?}");
}

/// `\{N\}` — BRE interval.
#[tokio::test]
async fn bre_interval() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "aa\na\naaa\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -c 'a\{2\}' t.txt"#).await;
    assert_eq!(code, 0, "interval should match; out={out:?}");
    assert_eq!(out.trim(), "2", "'aa' and 'aaa' contain 2 consecutive a's: {out:?}");
}

/// Bare ERE alternation still works — the superset keeps both spellings.
#[tokio::test]
async fn ere_bare_pipe_still_works() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "foo\nbar\nbaz\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep 'foo|bar' t.txt"#).await;
    assert_eq!(code, 0, "ERE alternation should match; out={out:?}");
    assert!(out.contains("foo") && out.contains("bar"), "{out:?}");
}

/// `-E` (strict ERE) turns the rewrite off: `\|` is a literal pipe, so
/// `foo\|bar` matches the literal 7-char string `foo|bar`, not `foo` OR `bar`.
/// This is the escape hatch for matching a literal pipe.
#[tokio::test]
async fn extended_mode_backslash_pipe_is_literal() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "foo|bar\nfoo\nbar\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -E 'foo\|bar' t.txt"#).await;
    assert_eq!(code, 0, "literal pipe should match; out={out:?}");
    assert_eq!(out.trim(), "foo|bar", "only the literal-pipe line, no alternation: {out:?}");
}

/// Non-meta escapes keep their meaning — `\.` is still a literal dot, not the
/// any-char metachar, and is untouched by the BRE rewrite.
#[tokio::test]
async fn non_meta_escape_preserved() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "a.b\naxb\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep 'a\.b' t.txt"#).await;
    assert_eq!(code, 0, "literal-dot should match; out={out:?}");
    assert!(out.contains("a.b"), "{out:?}");
    assert!(!out.contains("axb"), "escaped dot must not match any-char: {out:?}");
}

/// `-F` is unaffected: fixed-strings escapes every metachar, so a `|` is a
/// literal pipe, not alternation.
#[tokio::test]
async fn fixed_strings_pipe_stays_literal() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "a|b\nab\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -F 'a|b' t.txt"#).await;
    assert_eq!(code, 0, "literal pipe should match; out={out:?}");
    assert!(out.contains("a|b"), "{out:?}");
    assert!(!out.contains("ab\n") || out.trim() == "a|b", "ab must not match: {out:?}");
}

/// `-F` wins over the BRE rewrite: `a\|b` fixed-string is the literal 4-char
/// text `a\|b` (backslash included), never alternation.
#[tokio::test]
async fn fixed_strings_backslash_pipe_is_verbatim_text() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "a\\|b\na|b\na\nb\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -F 'a\|b' t.txt"#).await;
    assert_eq!(code, 0, "verbatim text should match; out={out:?}");
    assert_eq!(out.trim(), "a\\|b", "only the backslash-pipe line: {out:?}");
}

/// A formerly-literal escape that the rewrite breaks (`:\)` → unmatched `)`)
/// fails loudly WITH a dialect hint — the engine error alone describes a
/// pattern the author never wrote.
#[tokio::test]
async fn rewritten_meta_compile_error_carries_dialect_hint() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "smiley :)\n").unwrap();
    let kernel = kernel_at(dir.path());

    let err = kernel
        .execute(r#"grep ':\)' t.txt"#)
        .await
        .expect_err("unmatched group should fail validation");
    let msg = err.to_string();
    assert!(msg.contains("GNU BRE"), "should explain the dialect: {msg}");
    assert!(msg.contains("[)]"), "should offer the bracket-class spelling: {msg}");
    assert!(msg.contains("-E"), "should offer the strict-ERE escape hatch: {msg}");
}

/// `-E` with bare ERE is unchanged — strict mode only alters the escapes.
#[tokio::test]
async fn extended_mode_bare_ere_alternation_works() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "foo\nbar\nbaz\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -E 'foo|bar' t.txt"#).await;
    assert_eq!(code, 0, "ERE alternation under -E should match; out={out:?}");
    assert!(out.contains("foo") && out.contains("bar"), "{out:?}");
    assert!(!out.contains("baz"), "{out:?}");
}
