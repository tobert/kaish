//! `%` is an ordinary character inside an unquoted word.
//!
//! `git log --format=%cd`, `echo a%b` and `echo 100%` used to die with
//! `lexer error: unexpected character`; POSIX shells treat `%` as a plain word
//! character. The meanings that must survive: job specs (`%1`), the `${x%y}`
//! and `${x%%y}` trim operators, and `%` as the modulo operator inside
//! `$(( ))`.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::lexer::{tokenize, Token};

mod common;

fn lex(input: &str) -> Vec<Token> {
    tokenize(input)
        .expect("lexing should succeed")
        .into_iter()
        .map(|s| s.token)
        .filter(|t| !matches!(t, Token::Newline))
        .collect()
}

#[test]
fn percent_inside_and_leading_words_lex_as_one_word() {
    assert_eq!(lex("a%b"), vec![Token::Ident("a%b".into())]);
    assert_eq!(lex("%s"), vec![Token::Ident("%s".into())]);
    assert_eq!(lex("%cd"), vec![Token::Ident("%cd".into())]);
    assert_eq!(lex("100%"), vec![Token::NumberIdent("100%".into())]);
    assert_eq!(lex("x/y%z"), vec![Token::RelativePath("x/y%z".into())]);
    assert_eq!(lex("/tmp/a%b"), vec![Token::Path("/tmp/a%b".into())]);
}

#[test]
fn long_flag_value_may_contain_percent() {
    assert_eq!(
        lex("--format=%cd"),
        vec![
            Token::LongFlag("format".into()),
            Token::Eq,
            Token::Ident("%cd".into())
        ]
    );
}

#[test]
fn job_specs_still_lex_as_job_specs() {
    assert_eq!(lex("%1"), vec![Token::JobSpec("%1".into())]);
    assert_eq!(
        lex("wait %1 %2"),
        vec![
            Token::Ident("wait".into()),
            Token::JobSpec("%1".into()),
            Token::JobSpec("%2".into())
        ]
    );
}

#[test]
fn plus_and_minus_percent_words_unchanged() {
    assert_eq!(lex("+%s"), vec![Token::PlusBare("+%s".into())]);
    assert_eq!(lex("+%Y-%m-%d"), vec![Token::PlusBare("+%Y-%m-%d".into())]);
    assert_eq!(lex("-%"), vec![Token::MinusBare("-%".into())]);
}

#[test]
fn trim_operators_and_modulo_unchanged() {
    assert_eq!(lex("${x%y}"), vec![Token::VarRef("${x%y}".into())]);
    assert_eq!(lex("${x%%y}"), vec![Token::VarRef("${x%%y}".into())]);
    assert_eq!(lex("$((7 % 3))"), vec![Token::Arithmetic("7 % 3".into())]);
}

async fn run(script: &str) -> (String, i64) {
    let kernel = kaish_kernel::Kernel::transient().expect("kernel");
    common::run(&kernel, script).await
}

#[tokio::test]
async fn echo_percent_words() {
    assert_eq!(run("echo a%b").await, ("a%b".to_string(), 0));
    assert_eq!(run("echo 100%").await, ("100%".to_string(), 0));
    assert_eq!(run("echo %s").await, ("%s".to_string(), 0));
}

#[tokio::test]
async fn echo_long_flag_with_percent_value() {
    // `echo` rejects unknown flags, so use `printf`, whose `--` ends option parsing.
    assert_eq!(
        run("printf '%s\\n' -- --format=%cd").await,
        ("--format=%cd".to_string(), 0)
    );
}

#[tokio::test]
async fn printf_percent_s_as_bare_argument() {
    assert_eq!(run("printf %s x").await, ("x".to_string(), 0));
}

#[tokio::test]
async fn modulo_still_evaluates() {
    assert_eq!(run("echo $((7 % 3))").await, ("1".to_string(), 0));
}
