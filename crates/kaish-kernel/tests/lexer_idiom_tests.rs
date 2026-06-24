//! Common-idiom lexer gaps: bare `@`, digit-leading hyphenated words (ISO
//! dates, `N-M` ranges), and minus-led numeric words (`find -size -1k`).
//!
//! These are everyday agent inputs that used to fragment into adjacent tokens
//! and trip the no-token-pasting guard (a loud-but-wrong parse error). A
//! contiguous word the user typed is one word — kaish now lexes these as a
//! single bareword, matching bash/GNU. The no-pasting guard still fires on
//! genuinely separate adjacent tokens (`echo 1,2,3`).

#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};

#[tokio::test]
async fn at_sign_midword_userhost() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo user@host").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "user@host");
}

#[tokio::test]
async fn at_sign_email() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo a@b.com").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "a@b.com");
}

#[tokio::test]
async fn at_sign_leading_scope() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo @scope/pkg").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "@scope/pkg");
}

#[tokio::test]
async fn at_sign_leading_epoch() {
    // `date -d @0` shape: a leading `@` followed by digits.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo @0").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "@0");
}

#[tokio::test]
async fn at_sign_userhost_with_port() {
    // `@` merges into an Ident, so a colon-port still colon-merges into one word.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo user@host:8080").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "user@host:8080");
}

#[tokio::test]
async fn iso_date_is_one_word() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo 2024-01-02").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "2024-01-02", "leading zeros must be preserved");
}

#[tokio::test]
async fn integer_range_is_one_word() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo 10-20").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "10-20");
}

#[tokio::test]
async fn float_dash_int_is_one_word() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo 1.5-2").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "1.5-2");
}

#[tokio::test]
async fn bare_integer_still_lexes_as_int() {
    // Regression guard: the digit-hyphen word must NOT swallow a plain integer.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo 2024").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "2024");
}

#[tokio::test]
async fn cut_field_range_unquoted() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo 'a:b:c:d' | cut -d: -f 1-3").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "a:b:c");
}

#[tokio::test]
async fn tr_digit_range_now_deletes_digits() {
    // Previously a loud "quote the whole word" error (0-9 fragmented into
    // Int(0)+Int(-9)). Now `0-9` is one word and tr applies the range, matching
    // bash/GNU. Supersedes the old `numeric_range_is_loud_not_silent` decision.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "echo 'abc123def' | tr -d 0-9").await;
    assert_eq!(code, 0, "got: {out}");
    assert_eq!(out, "abcdef");
}

#[tokio::test]
async fn find_size_negative_suffix_parses_and_runs() {
    // `find -size -1k` ("smaller than 1k"): the `-1k` arg used to fail to lex
    // (Int(-1) + Ident(k), adjacent). A tiny file matches "smaller than 1k".
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("small.txt"), b"hi").unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "find . -size -1k -type f").await;
    assert_eq!(code, 0, "got: {out}");
    assert!(out.contains("small.txt"), "got: {out}");
}

#[tokio::test]
async fn adjacent_commas_still_loud() {
    // Unchanged: commas are not part of the digit-hyphen word; a run of
    // touching positional words is still a loud no-pasting error.
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let result = kernel.execute("echo 1,2,3").await;
    assert!(result.is_err(), "adjacent comma words must stay a loud error");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(msg.contains("quote"), "should hint to quote: {msg}");
}
