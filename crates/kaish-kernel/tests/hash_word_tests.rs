//! `#` opens a comment only at the start of a word.
//!
//! kaish used to start a comment at any `#`, so `echo abc#3` printed `abc` and
//! the rest of the line — `;` separators included — disappeared at exit 0. Two
//! commands could vanish with no error, which is indistinguishable from two
//! commands that ran and printed nothing.
//!
//! POSIX opens a comment only at the start of a word, and bash and `/bin/sh`
//! agree on every row here (see `shell_compat_tests.rs`, which runs the same
//! scripts through bash when `KAISH_BASH_COMPAT=1`). The word classes carry
//! `#` as an ordinary character; a `#` that lands mid-word after something
//! that cannot hold it (`$x#3`, `"abc"#3`) is a loud error, never a comment.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::lexer::{tokenize, Token};

mod common;

/// Lex a script, dropping newlines, and panic on any lexer error.
fn lex(input: &str) -> Vec<Token> {
    tokenize(input)
        .expect("lexing should succeed")
        .into_iter()
        .map(|s| s.token)
        .filter(|t| !matches!(t, Token::Newline))
        .collect()
}

// ── The reported defect: a swallowed line, at exit 0 ────────────────────────

/// The whole reason this class matters: the second command must still run.
#[tokio::test]
async fn hash_in_word_does_not_swallow_the_rest_of_the_line() {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    let (out, code) = common::run(&kernel, r#"echo "bare:" abc#3; echo "two:" x"#).await;
    assert_eq!(out, "bare: abc#3\ntwo: x");
    assert_eq!(code, 0);
}

/// kaijutsu prints short block ids as `<principal8>#<seq>` and accepts that
/// form back as input. The id is digit-leading, so it lexes as a NumberIdent
/// rather than an Ident — a separate word class, and it was truncated too.
#[tokio::test]
async fn short_id_shaped_argument_survives_intact() {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    let (out, code) = common::run(&kernel, "echo 2d25fb02#3").await;
    assert_eq!(out, "2d25fb02#3");
    assert_eq!(code, 0);
}

// ── `#` is an ordinary character inside a word ─────────────────────────────

#[test]
fn hash_mid_word_lexes_as_one_word() {
    assert_eq!(lex("abc#3"), vec![Token::Ident("abc#3".to_string())]);
}

#[test]
fn trailing_hash_stays_in_the_word() {
    // bash: `echo abc# 3` prints `abc# 3` — the `#` belongs to the word.
    assert_eq!(
        lex("abc# 3"),
        vec![Token::Ident("abc#".to_string()), Token::Int(3)]
    );
}

#[test]
fn repeated_hashes_stay_in_the_word() {
    assert_eq!(lex("a#b#c"), vec![Token::Ident("a#b#c".to_string())]);
}

#[test]
fn hash_in_a_path_stays_in_the_path() {
    assert_eq!(lex("/foo/bar#1"), vec![Token::Path("/foo/bar#1".to_string())]);
}

#[test]
fn hash_in_a_digit_leading_word_stays_in_the_word() {
    assert_eq!(
        lex("2d25fb02#3"),
        vec![Token::NumberIdent("2d25fb02#3".to_string())]
    );
}

// ── `#` at the start of a word still opens a comment ───────────────────────

#[test]
fn hash_after_whitespace_is_a_comment() {
    assert_eq!(lex("abc #3"), vec![Token::Ident("abc".to_string())]);
}

#[test]
fn hash_at_start_of_input_is_a_comment() {
    assert_eq!(lex("# comment"), vec![]);
}

#[test]
fn hash_after_a_separator_is_a_comment() {
    assert_eq!(
        lex("a;#c"),
        vec![Token::Ident("a".to_string()), Token::Semi]
    );
}

#[tokio::test]
async fn spaced_hash_still_comments_the_rest_of_the_line() {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    let (out, code) = common::run(&kernel, "echo one #two\necho three").await;
    assert_eq!(out, "one\nthree");
    assert_eq!(code, 0);
}

// ── A `#` that cannot join a word is loud, never a silent comment ──────────

/// kaish keeps `$VAR` a separate word, so `$x#3` cannot become one token the
/// way bash's `q#3` does. What matters is that it does not silently comment
/// the line away: the script is rejected before anything runs, and the message
/// names the fix.
///
/// The failure arrives as `Err` from `execute`, not as a nonzero `ExecResult` —
/// a lexer error is caught before execution, so no statement runs at all.
#[tokio::test]
async fn hash_after_a_variable_reference_is_a_loud_error() {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    let error = kernel
        .execute("x=q; echo $x#3; echo second")
        .await
        .expect_err("mid-word `#` after a variable reference must be rejected");
    let text = error.to_string();
    assert!(
        text.contains("start of a word"),
        "error must teach the rule: {text}"
    );
    assert!(
        text.contains(r#""$x#3""#),
        "error must show the quote fix: {text}"
    );
}

#[tokio::test]
async fn hash_after_a_quoted_string_is_a_loud_error() {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    let error = kernel
        .execute(r##"echo "abc"#3"##)
        .await
        .expect_err("mid-word `#` after a quoted string must be rejected");
    assert!(error.to_string().contains("start of a word"));
}

/// `$(f)#3` is one word in bash. kaish cannot tell this `)` from a subshell's,
/// so it refuses rather than commenting the rest of the line away.
#[tokio::test]
async fn hash_after_a_closing_paren_is_a_loud_error() {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    let error = kernel
        .execute("echo $(echo a)#3")
        .await
        .expect_err("mid-word `#` after `)` must be rejected");
    assert!(error.to_string().contains("start of a word"));
}

// ── Quoting is the override, and it was never broken ───────────────────────

#[tokio::test]
async fn quoting_keeps_a_leading_hash_literal() {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    let (out, code) = common::run(&kernel, r##"echo "#3" '#4'"##).await;
    assert_eq!(out, "#3 #4");
    assert_eq!(code, 0);
}
