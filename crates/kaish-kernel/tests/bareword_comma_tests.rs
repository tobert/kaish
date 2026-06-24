//! Bare comma as an argument bareword, and the no-token-pasting guard for
//! adjacent words / numeric ranges.
//!
//! `cut -d,` / `tr -d ,` reach for a lone comma as a delimiter/set argument —
//! a comma now lexes-and-parses as the literal `","` in argument position, so
//! the common `cut -d, -f2` idiom works without quoting. The no-pasting rule
//! still guards the genuinely ambiguous cases: `echo 1,2,3` (a run of
//! comma-touching positional words) is a loud parse error with a "quote the
//! whole word" hint.
//!
//! A digit range like `0-9` / `1-3` is a *single contiguous word* the user
//! typed, so it now lexes as one bareword (`DashNumWord`) and reaches the tool
//! verbatim — `tr -d 0-9` deletes digits, matching bash/GNU. This supersedes
//! the earlier loud-error decision, which was only the best available while
//! `0-9` could only fragment into `Int(0)`+`Int(-9)`. See `lexer_idiom_tests`.

#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use kaish_kernel::{Kernel, KernelConfig};

#[tokio::test]
async fn cut_bare_comma_delimiter_glued() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "echo 'a,b,c' | cut -d, -f2").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "b");
}

#[tokio::test]
async fn cut_bare_comma_delimiter_spaced() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "echo 'a,b,c' | cut -d , -f2").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "b");
}

#[tokio::test]
async fn tr_deletes_bare_comma() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "echo 'a,b,c' | tr -d ,").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "abc");
}

#[tokio::test]
async fn adjacent_commas_are_loud_not_pasted() {
    // `echo 1,2,3` is a run of touching positional words; the no-pasting guard
    // rejects it loudly rather than silently joining or dropping pieces.
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let result = kernel.execute("echo 1,2,3").await;
    assert!(result.is_err(), "adjacent words must be a loud error");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(msg.contains("quote"), "should hint to quote: {msg}");
}

#[tokio::test]
async fn unquoted_numeric_range_deletes_digits() {
    // `tr -d 0-9` used to lex as Int(0) + Int(-9) and was made a loud error as
    // a stopgap. `0-9` is one contiguous word, so it now reaches tr verbatim
    // and the range applies — matching bash/GNU.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo 'abc123def' | tr -d 0-9").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "abcdef");
}

#[tokio::test]
async fn quoted_numeric_range_works() {
    // Quoting the range is equivalent (and still the safe habit).
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo 'abc123def' | tr -d '0-9'").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "abcdef");
}
