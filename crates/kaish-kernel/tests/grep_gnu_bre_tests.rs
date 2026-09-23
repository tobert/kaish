//! Kernel-routed tests: `grep` without `-E` or `-F` follows GNU BRE.
//!
//! GNU grep is the specification for the default mode. Models write grep from
//! GNU habit (`grep -rn "fn consult("`, `grep 'a\|b'`), so kaish matches what
//! GNU grep does, byte for byte, including where GNU surprises:
//!
//! - bare `( ) { } | + ?` are literal characters;
//! - `\( \)` group, `\{n,m\}` is an interval, `\|` `\+` `\?` are operators;
//! - `*` at the start of a pattern, group, or alternative is literal;
//! - `^` and `$` in the middle of a pattern are literal;
//! - inside `[...]` every character is literal, backslash included;
//! - a backslash before an ordinary character (`\d`, `\n`, `\/`) is that
//!   character, with GNU's "stray \" warning on stderr.
//!
//! The expected values are what `/usr/bin/grep` (GNU grep 3.12) printed for the
//! same pattern over [`FIXTURE`], recorded as literals. CI need not have GNU
//! grep; these tests never run it. To regenerate a row, write `FIXTURE` to a
//! file and run `grep -e PATTERN file`.
//!
//! Each row runs through every code path that compiles the pattern: a single
//! file (chunked line scanner), piped stdin (streaming), `-c` (whole-buffer
//! searcher), and `-U` (multiline searcher).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use rstest::rstest;
use tempfile::tempdir;

use common::{kernel_at, run};
use kaish_kernel::Kernel;

/// One line per case the table needs to tell apart. Keep it in sync with the
/// recorded GNU output below: every expected list is a subset of these lines.
const FIXTURE: &[&str] = &[
    "fn consult(q)", "KjCaller { x }", "(foo|bar)", "foo", "bar", "a+b", "aab", "a?b",
    "b", "x{2}", "xx", "a|b", "*foo", "a^b", "a$b", "a.b", "axb", "ab", r"a\b", "x[y",
    "a&b", "a~b", "w-x", "foo bar", "foobar", "a]b", "d1", "123", "nt", "a}b", "+a",
    "aaa", r#"a"b"#, "FOO",
];

fn fixture_kernel() -> (tempfile::TempDir, Kernel) {
    let dir = tempdir().unwrap();
    let mut text = FIXTURE.join("\n");
    text.push('\n');
    fs::write(dir.path().join("fx.txt"), text).unwrap();
    let kernel = kernel_at(dir.path());
    (dir, kernel)
}

fn lines(out: &str) -> Vec<&str> {
    out.lines().collect()
}

/// The differential-test corpus (Latin diacritics, CJK) that found the POSIX
/// bracket-class gap: the regex engine's own `[[:alpha:]]` is ASCII-only,
/// GNU grep in a UTF-8 locale is not.
const UNICODE_ALPHA_FIXTURE: &[&str] = &["héllo wörld", "日本語テキスト"];

/// Non-ASCII coverage for every bracket class: a non-ASCII letter, an ASCII
/// and a non-ASCII digit script, non-ASCII punctuation, a non-breaking space
/// (glibc classifies it `[:punct:]`, not `[:space:]`), an ideographic space
/// (which IS `[:space:]`), and a combining mark (also `[:punct:]`).
const UNICODE_CLASS_FIXTURE: &[&str] = &[
    "héllo wörld",
    "日本語テキスト",
    "abc123 ABC",
    "\u{0663} \u{FF13}",             // Arabic-Indic 3, fullwidth 3
    "「line」\u{2014} \u{00BF}",     // corner brackets, em dash, inverted question
    "a\u{00A0}b",                    // U+00A0 NO-BREAK SPACE
    "a\u{3000}b",                    // U+3000 IDEOGRAPHIC SPACE
    "cafe\u{0301} bar",              // "e" + U+0301 COMBINING ACUTE ACCENT
];

fn unicode_fixture_kernel(corpus: &[&str]) -> (tempfile::TempDir, Kernel) {
    let dir = tempdir().unwrap();
    let mut text = corpus.join("\n");
    text.push('\n');
    fs::write(dir.path().join("fx.txt"), text).unwrap();
    let kernel = kernel_at(dir.path());
    (dir, kernel)
}

/// GNU grep's output for `grep FLAGS -e PATTERN fx.txt`, row by row.
#[rstest]
#[case(r#""#, r#"fn consult("#, 0, &[r#"fn consult(q)"#])]
#[case(r#""#, r#"KjCaller {"#, 0, &[r#"KjCaller { x }"#])]
#[case(r#""#, r#"(foo|bar)"#, 0, &[r#"(foo|bar)"#])]
#[case(r#""#, r#"\(foo\|bar\)"#, 0, &[r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"*foo"#, r#"foo bar"#, r#"foobar"#])]
#[case(r#""#, r#"a+"#, 0, &[r#"a+b"#])]
#[case(r#""#, r#"a\+"#, 0, &[r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"a|b"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"a&b"#, r#"a~b"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"a}b"#, r#"+a"#, r#"aaa"#, r#"a"b"#])]
#[case(r#""#, r#"a?"#, 0, &[r#"a?b"#])]
#[case(r#""#, r#"a\?"#, 0, &[r#"fn consult(q)"#, r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"x{2}"#, r#"xx"#, r#"a|b"#, r#"*foo"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"x[y"#, r#"a&b"#, r#"a~b"#, r#"w-x"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"d1"#, r#"123"#, r#"nt"#, r#"a}b"#, r#"+a"#, r#"aaa"#, r#"a"b"#, r#"FOO"#])]
#[case(r#""#, r#"x{2}"#, 0, &[r#"x{2}"#])]
#[case(r#""#, r#"x\{2\}"#, 0, &[r#"xx"#])]
#[case(r#""#, r#"a|b"#, 0, &[r#"a|b"#])]
#[case(r#""#, r#"a\|b"#, 0, &[r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"a|b"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"a&b"#, r#"a~b"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"a}b"#, r#"+a"#, r#"aaa"#, r#"a"b"#])]
#[case(r#""#, r#"[(]"#, 0, &[r#"fn consult(q)"#, r#"(foo|bar)"#])]
#[case(r#""#, r#"*foo"#, 0, &[r#"*foo"#])]
#[case(r#""#, r#"a^b"#, 0, &[r#"a^b"#])]
#[case(r#""#, r#"a$b"#, 0, &[r#"a$b"#])]
#[case(r#""#, r#"\."#, 0, &[r#"a.b"#])]
#[case(r#""#, r#"^*foo"#, 0, &[r#"*foo"#])]
#[case(r#""#, r#"\(*foo\)"#, 0, &[r#"*foo"#])]
#[case(r#""#, r#"foo\|*foo"#, 0, &[r#"(foo|bar)"#, r#"foo"#, r#"*foo"#, r#"foo bar"#, r#"foobar"#])]
#[case(r#""#, r#"\(^a\)"#, 0, &[r#"a+b"#, r#"aab"#, r#"a?b"#, r#"a|b"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"a&b"#, r#"a~b"#, r#"a]b"#, r#"a}b"#, r#"aaa"#, r#"a"b"#])]
#[case(r#""#, r#"b$\|^x"#, 0, &[r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"x{2}"#, r#"xx"#, r#"a|b"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"x[y"#, r#"a&b"#, r#"a~b"#, r#"a]b"#, r#"a}b"#, r#"a"b"#])]
#[case(r#""#, r#"\(a$\)"#, 0, &[r#"+a"#, r#"aaa"#])]
#[case(r#""#, r#"a**"#, 0, &[r#"fn consult(q)"#, r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"x{2}"#, r#"xx"#, r#"a|b"#, r#"*foo"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"x[y"#, r#"a&b"#, r#"a~b"#, r#"w-x"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"d1"#, r#"123"#, r#"nt"#, r#"a}b"#, r#"+a"#, r#"aaa"#, r#"a"b"#, r#"FOO"#])]
#[case(r#""#, r#"a*\+"#, 0, &[r#"fn consult(q)"#, r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"x{2}"#, r#"xx"#, r#"a|b"#, r#"*foo"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"x[y"#, r#"a&b"#, r#"a~b"#, r#"w-x"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"d1"#, r#"123"#, r#"nt"#, r#"a}b"#, r#"+a"#, r#"aaa"#, r#"a"b"#, r#"FOO"#])]
#[case(r#""#, r#"a*\?b"#, 0, &[r#"(foo|bar)"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"a|b"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"a&b"#, r#"a~b"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"a}b"#, r#"a"b"#])]
#[case(r#""#, r#"\+a"#, 0, &[r#"+a"#])] // GNU: grep: warning: stray \ before +
#[case(r#""#, r#"\(\+a\)"#, 0, &[r#"+a"#])] // GNU: grep: warning: stray \ before +
#[case(r#""#, r#"x\|\+a"#, 0, &[r#"KjCaller { x }"#, r#"x{2}"#, r#"xx"#, r#"axb"#, r#"x[y"#, r#"w-x"#, r#"+a"#])] // GNU: grep: warning: stray \ before +
#[case(r#""#, r#"\{2\}"#, 0, &[r#"x{2}"#])] // GNU: grep: warning: stray \ before {
#[case(r#""#, r#"a\}b"#, 0, &[r#"a}b"#])]
#[case(r#""#, r#"a\{2,\}"#, 0, &[r#"aab"#, r#"aaa"#])]
#[case(r#""#, r#"a\{,2\}b"#, 0, &[r#"(foo|bar)"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"a|b"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"a&b"#, r#"a~b"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"a}b"#, r#"a"b"#])]
#[case(r#""#, r#"\d"#, 0, &[r#"d1"#])] // GNU: grep: warning: stray \ before d
#[case(r#""#, r#"[0-9]\+"#, 0, &[r#"x{2}"#, r#"d1"#, r#"123"#])]
#[case(r#""#, r#"\n"#, 0, &[r#"fn consult(q)"#, r#"nt"#])] // GNU: grep: warning: stray \ before n
#[case(r#""#, r#"\w\+"#, 0, &[r#"fn consult(q)"#, r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"x{2}"#, r#"xx"#, r#"a|b"#, r#"*foo"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"x[y"#, r#"a&b"#, r#"a~b"#, r#"w-x"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"d1"#, r#"123"#, r#"nt"#, r#"a}b"#, r#"+a"#, r#"aaa"#, r#"a"b"#, r#"FOO"#])]
#[case(r#""#, r#"\s"#, 0, &[r#"fn consult(q)"#, r#"KjCaller { x }"#, r#"foo bar"#])]
#[case(r#""#, r#"\W"#, 0, &[r#"fn consult(q)"#, r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"a+b"#, r#"a?b"#, r#"x{2}"#, r#"a|b"#, r#"*foo"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"a\b"#, r#"x[y"#, r#"a&b"#, r#"a~b"#, r#"w-x"#, r#"foo bar"#, r#"a]b"#, r#"a}b"#, r#"+a"#, r#"a"b"#])]
#[case(r#""#, r#"\S"#, 0, &[r#"fn consult(q)"#, r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"x{2}"#, r#"xx"#, r#"a|b"#, r#"*foo"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"x[y"#, r#"a&b"#, r#"a~b"#, r#"w-x"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"d1"#, r#"123"#, r#"nt"#, r#"a}b"#, r#"+a"#, r#"aaa"#, r#"a"b"#, r#"FOO"#])]
#[case(r#""#, r#"\<bar"#, 0, &[r#"(foo|bar)"#, r#"bar"#, r#"foo bar"#])]
#[case(r#""#, r#"bar\>"#, 0, &[r#"(foo|bar)"#, r#"bar"#, r#"foo bar"#, r#"foobar"#])]
#[case(r#""#, r#"\bbar"#, 0, &[r#"(foo|bar)"#, r#"bar"#, r#"foo bar"#])]
#[case(r#""#, r#"\Bbar"#, 0, &[r#"foobar"#])]
#[case(r#""#, r#"[\]"#, 0, &[r#"a\b"#])]
#[case(r#""#, r#"[\d]"#, 0, &[r#"a\b"#, r#"d1"#])]
#[case(r#""#, r#"[[]"#, 0, &[r#"x[y"#])]
#[case(r#""#, r#"[&]"#, 0, &[r#"a&b"#])]
#[case(r#""#, r#"[~]"#, 0, &[r#"a~b"#])]
#[case(r#""#, r#"[]a]b"#, 0, &[r#"aab"#, r#"ab"#, r#"a]b"#])]
#[case(r#""#, r#"a]b"#, 0, &[r#"a]b"#])]
#[case(r#""#, r#"[^]a]"#, 0, &[r#"fn consult(q)"#, r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"x{2}"#, r#"xx"#, r#"a|b"#, r#"*foo"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"x[y"#, r#"a&b"#, r#"a~b"#, r#"w-x"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"d1"#, r#"123"#, r#"nt"#, r#"a}b"#, r#"+a"#, r#"a"b"#, r#"FOO"#])]
#[case(r#""#, r#"[[:digit:]]\+"#, 0, &[r#"x{2}"#, r#"d1"#, r#"123"#])]
#[case(r#""#, r#"[a-]"#, 0, &[r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"a|b"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"a&b"#, r#"a~b"#, r#"w-x"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"a}b"#, r#"+a"#, r#"aaa"#, r#"a"b"#])]
#[case(r#""#, r#"[w-x]"#, 0, &[r#"KjCaller { x }"#, r#"x{2}"#, r#"xx"#, r#"axb"#, r#"x[y"#, r#"w-x"#])]
#[case(r#""#, r#"[!--]"#, 0, &[r#"fn consult(q)"#, r#"(foo|bar)"#, r#"a+b"#, r#"*foo"#, r#"a$b"#, r#"a&b"#, r#"w-x"#, r#"+a"#, r#"a"b"#])]
#[case(r#""#, r#"[[.-.]]"#, 0, &[r#"w-x"#])]
#[case(r#""#, r#"[[=a=]]b"#, 0, &[r#"aab"#, r#"ab"#])]
#[case(r#""#, r#"\-"#, 0, &[r#"w-x"#])] // GNU: grep: warning: stray \ before -
#[case(r#""#, r#"\/"#, 1, &[])] // GNU: grep: warning: stray \ before /
#[case(r#""#, r#"\]"#, 0, &[r#"a]b"#])]
#[case(r#""#, r#"\*"#, 0, &[r#"*foo"#])]
#[case(r#""#, r#"\["#, 0, &[r#"x[y"#])]
#[case(r#""#, r#"\^"#, 0, &[r#"a^b"#])]
#[case(r#""#, r#"\$"#, 0, &[r#"a$b"#])]
#[case(r#""#, r#"\\"#, 0, &[r#"a\b"#])]
#[case(r#""#, r#"a\"b"#, 0, &[r#"a"b"#])] // GNU: grep: warning: stray \ before "
#[case(r#""#, r#"a\{2\}"#, 0, &[r#"aab"#, r#"aaa"#])]
#[case(r#"-w"#, r#"KjCaller {"#, 0, &[r#"KjCaller { x }"#])]
#[case(r#"-w"#, r#"foo\|bar"#, 0, &[r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"*foo"#, r#"foo bar"#])]
#[case(r#"-w"#, r#"(foo|bar)"#, 0, &[r#"(foo|bar)"#])]
#[case(r#"-w"#, r#"\(foo\|bar\)"#, 0, &[r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"*foo"#, r#"foo bar"#])]
#[case(r#"-w"#, r#"x\{2\}"#, 0, &[r#"xx"#])]
#[case(r#"-i"#, r#"KJCALLER {"#, 0, &[r#"KjCaller { x }"#])]
#[case(r#"-i"#, r#"FOO\|BAR"#, 0, &[r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"*foo"#, r#"foo bar"#, r#"foobar"#, r#"FOO"#])]
#[case(r#"-i"#, r#"(FOO|BAR)"#, 0, &[r#"(foo|bar)"#])]
#[case(r#"-iw"#, r#"FOO\|BAR"#, 0, &[r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"*foo"#, r#"foo bar"#, r#"FOO"#])]
#[case(r#"-v"#, r#"a\|b\|x"#, 0, &[r#"fn consult(q)"#, r#"foo"#, r#"*foo"#, r#"d1"#, r#"123"#, r#"nt"#, r#"FOO"#])]
#[tokio::test]
async fn default_mode_matches_gnu_grep(
    #[case] flags: &str,
    #[case] pattern: &str,
    #[case] gnu_code: i64,
    #[case] gnu_lines: &[&str],
) {
    let (_dir, kernel) = fixture_kernel();

    // Single file: the chunked line scanner.
    let (out, code) = run(&kernel, &format!("grep {flags} '{pattern}' fx.txt")).await;
    assert_eq!(lines(&out), gnu_lines, "file path, pattern {pattern:?}");
    assert_eq!(code, gnu_code, "file path exit code, pattern {pattern:?}");

    // Piped stdin with a downstream pipe: the streaming path.
    let (out, code) = run(
        &kernel,
        &format!("set -o pipefail; cat fx.txt | grep {flags} '{pattern}' | cat"),
    )
    .await;
    assert_eq!(lines(&out), gnu_lines, "streaming stdin, pattern {pattern:?}");
    assert_eq!(code, gnu_code, "streaming stdin exit code, pattern {pattern:?}");

    // `-c`: the whole-buffer searcher.
    let (out, _) = run(&kernel, &format!("grep -c {flags} '{pattern}' fx.txt")).await;
    assert_eq!(out, gnu_lines.len().to_string(), "-c count, pattern {pattern:?}");

    // `-U`: the multiline searcher. No row spans a newline, so it selects the
    // same lines.
    let (out, code) = run(&kernel, &format!("grep -U {flags} '{pattern}' fx.txt")).await;
    assert_eq!(lines(&out), gnu_lines, "-U, pattern {pattern:?}");
    assert_eq!(code, gnu_code, "-U exit code, pattern {pattern:?}");
}

/// GNU grep's output for `-o` and `-c` rows: the whole-buffer path, where
/// the match span matters, not just the line.
#[rstest]
#[case(r#"-o"#, r#"a\+"#, 0, &[r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"aa"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"aaa"#, r#"a"#])]
#[case(r#"-o"#, r#"(foo|bar)"#, 0, &[r#"(foo|bar)"#])]
#[case(r#"-o"#, r#"\(foo\|bar\)"#, 0, &[r#"foo"#, r#"bar"#, r#"foo"#, r#"bar"#, r#"foo"#, r#"foo"#, r#"bar"#, r#"foo"#, r#"bar"#])]
#[case(r#"-o"#, r#"x\{2\}"#, 0, &[r#"xx"#])]
#[case(r#"-o"#, r#"a\+*"#, 0, &[r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"aa"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"aaa"#, r#"a"#])]
#[case(r#"-o"#, r#"a*\?b"#, 0, &[r#"b"#, r#"b"#, r#"b"#, r#"aab"#, r#"b"#, r#"b"#, r#"b"#, r#"b"#, r#"b"#, r#"b"#, r#"b"#, r#"ab"#, r#"b"#, r#"b"#, r#"b"#, r#"b"#, r#"b"#, r#"b"#, r#"b"#, r#"b"#])]
#[case(r#"-o"#, r#"[0-9]\+"#, 0, &[r#"2"#, r#"1"#, r#"123"#])]
#[case(r#"-o"#, r#"fn consult("#, 0, &[r#"fn consult("#])]
#[case(r#"-o"#, r#"a^b\|a$b"#, 0, &[r#"a^b"#, r#"a$b"#])]
#[case(r#"-io"#, r#"FOO\|BAR"#, 0, &[r#"foo"#, r#"bar"#, r#"foo"#, r#"bar"#, r#"foo"#, r#"foo"#, r#"bar"#, r#"foo"#, r#"bar"#, r#"FOO"#])]
#[case(r#"-wo"#, r#"foo\|bar"#, 0, &[r#"foo"#, r#"bar"#, r#"foo"#, r#"bar"#, r#"foo"#, r#"foo"#, r#"bar"#])]
#[case(r#"-c"#, r#"KjCaller {"#, 0, &[r#"1"#])]
#[case(r#"-c"#, r#"a\|b"#, 0, &[r#"23"#])]
#[case(r#"-c"#, r#"(foo|bar)"#, 0, &[r#"1"#])]
#[tokio::test]
async fn match_output_matches_gnu_grep(
    #[case] flags: &str,
    #[case] pattern: &str,
    #[case] gnu_code: i64,
    #[case] gnu_lines: &[&str],
) {
    let (_dir, kernel) = fixture_kernel();
    let (out, code) = run(&kernel, &format!("grep {flags} '{pattern}' fx.txt")).await;
    assert_eq!(lines(&out), gnu_lines, "pattern {pattern:?}");
    assert_eq!(code, gnu_code, "exit code, pattern {pattern:?}");
}

/// A backslash before an ordinary character is that character, and GNU grep
/// warns on stderr. The match still runs and the exit code follows the match.
#[rstest]
#[case(r"\d", r"grep: warning: stray \ before d", &["d1"])]
#[case(r"\n", r"grep: warning: stray \ before n", &["fn consult(q)", "nt"])]
#[case(r"\+a", r"grep: warning: stray \ before +", &["+a"])]
#[case(r"\{2\}", r"grep: warning: stray \ before {", &["x{2}"])]
#[case(r"\-", r"grep: warning: stray \ before -", &["w-x"])]
#[case(r#"a\"b"#, r#"grep: warning: stray \ before ""#, &[r#"a"b"#])]
#[case(r"a\ b", r"grep: warning: stray \ before white space", &[])]
#[tokio::test]
async fn stray_backslash_warns_like_gnu_grep(
    #[case] pattern: &str,
    #[case] gnu_stderr: &str,
    #[case] gnu_lines: &[&str],
) {
    let (_dir, kernel) = fixture_kernel();
    let result = kernel
        .execute(&format!("grep '{pattern}' fx.txt"))
        .await
        .expect("a stray backslash is a warning, not an error");
    assert_eq!(lines(result.text_out().trim()), gnu_lines, "pattern {pattern:?}");
    assert_eq!(result.err.trim(), gnu_stderr, "pattern {pattern:?}");
}

/// Patterns GNU grep refuses with exit 2. kaish refuses them too, before the
/// search runs, and the message names the construct and the fix.
#[rstest]
#[case(r"\(foo", r"\(")] // GNU: Unmatched ( or \(
#[case(r"foo\)", r"\)")] // GNU: Unmatched ) or \)
#[case(r":\)", r"\)")] // GNU: Unmatched ) or \)
#[case(r"a\{2", r"\{")] // GNU: Unmatched \{
#[case(r"x\{a\}", r"\{")] // GNU: Invalid content of \{\}
#[case(r"a\{2,1\}", r"\{")] // GNU: Invalid content of \{\}
#[case(r"foo\", "backslash")] // GNU: Trailing backslash
#[case("[[:alpha]", "[")] // GNU: Unmatched [, [^, [:, [., or [=
#[case("[abc", "[")] // GNU: Unmatched [, [^, [:, [., or [=
#[case("[[:foo:]]", "foo")] // GNU: Invalid character class name
#[case(r"\1", r"\1")] // GNU: Invalid back reference
#[tokio::test]
async fn invalid_patterns_fail_like_gnu_grep(#[case] pattern: &str, #[case] named: &str) {
    let (_dir, kernel) = fixture_kernel();
    let message = match kernel.execute(&format!("grep '{pattern}' fx.txt")).await {
        Err(e) => e.to_string(),
        Ok(result) => {
            assert_eq!(result.code, 2, "pattern {pattern:?} must fail with exit 2");
            result.err.clone()
        }
    };
    assert!(message.contains(named), "pattern {pattern:?}: {message}");
    assert!(message.contains("-E"), "names the ERE override, pattern {pattern:?}: {message}");
}

// ─── Known gaps: GNU behavior the regex engine cannot reach ──────────────────

/// GNU grep supports back-references (`\1` to `\9`) in a BRE. Rust's `regex`
/// crate has none, so kaish refuses the pattern with exit 2 instead.
#[rstest]
#[case(r#""#, r#"\(o\)\1"#, 0, &[r#"(foo|bar)"#, r#"foo"#, r#"*foo"#, r#"foo bar"#, r#"foobar"#])]
#[tokio::test]
#[ignore = "gap: GNU BRE back-references; the regex crate has no back-references"]
async fn gap_back_references_match_like_gnu_grep(
    #[case] flags: &str,
    #[case] pattern: &str,
    #[case] gnu_code: i64,
    #[case] gnu_lines: &[&str],
) {
    let (_dir, kernel) = fixture_kernel();
    let (out, code) = run(&kernel, &format!("grep {flags} '{pattern}' fx.txt")).await;
    assert_eq!(lines(&out), gnu_lines);
    assert_eq!(code, gnu_code);
}

/// GNU grep picks the longest match among alternatives (POSIX leftmost-
/// longest): `-o 'a\|ab'` on `ab` prints `ab`. The regex crate is
/// leftmost-first and prints `a`. Line selection is the same; only `-o` spans
/// differ.
#[rstest]
#[case(r#"-o"#, r#"a\|ab"#, 0, &[r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"ab"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"ab"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#, r#"a"#])]
#[tokio::test]
#[ignore = "gap: GNU leftmost-longest alternation; the regex crate is leftmost-first"]
async fn gap_only_matching_prefers_longest_alternative(
    #[case] flags: &str,
    #[case] pattern: &str,
    #[case] gnu_code: i64,
    #[case] gnu_lines: &[&str],
) {
    let (_dir, kernel) = fixture_kernel();
    let (out, code) = run(&kernel, &format!("grep {flags} '{pattern}' fx.txt")).await;
    assert_eq!(lines(&out), gnu_lines);
    assert_eq!(code, gnu_code);
}

// ─── POSIX bracket classes are Unicode-aware ──────────────────────────────────
//
// The regex engine's own `[[:alpha:]]` support is ASCII-only; GNU grep in a
// UTF-8 locale is not. `alpha`/`alnum` also match a non-ASCII decimal digit
// (glibc classifies every Unicode `Nd` character but the ASCII range as
// alphabetic — a real quirk, confirmed against GNU grep 3.12, that this
// mirrors); `digit` itself stays ASCII-only either way. `space`/`blank`
// exclude U+00A0/U+2007/U+202F, which glibc classifies `[:punct:]` instead.

/// `grep 'a|b'` on the differential-test corpus (issue: `[[:alpha:]]` matched
/// `héllo wörld` but not `日本語テキスト`).
#[rstest]
#[case(r#""#, r#"[[:alpha:]]"#, 0, UNICODE_ALPHA_FIXTURE)]
#[case(r#"-E"#, r#"[[:alpha:]]"#, 0, UNICODE_ALPHA_FIXTURE)]
#[tokio::test]
async fn alpha_class_matches_gnu_grep_on_non_ascii_letters(
    #[case] flags: &str,
    #[case] pattern: &str,
    #[case] gnu_code: i64,
    #[case] gnu_lines: &[&str],
) {
    let (_dir, kernel) = unicode_fixture_kernel(UNICODE_ALPHA_FIXTURE);
    let (out, code) = run(&kernel, &format!("grep {flags} '{pattern}' fx.txt")).await;
    assert_eq!(lines(&out), gnu_lines, "pattern {pattern:?}");
    assert_eq!(code, gnu_code, "pattern {pattern:?}");
}

/// `grep -o '[[:alpha:]]'`: GNU emits each non-ASCII letter on its own line,
/// skipping the space and every digit.
#[tokio::test]
async fn alpha_class_extracts_non_ascii_letters_like_gnu_grep() {
    let (_dir, kernel) = unicode_fixture_kernel(UNICODE_ALPHA_FIXTURE);
    let (out, code) = run(&kernel, r#"grep -o '[[:alpha:]]' fx.txt"#).await;
    assert_eq!(code, 0);
    assert_eq!(
        lines(&out),
        &[
            "h", "é", "l", "l", "o", "w", "ö", "r", "l", "d", "日", "本", "語", "テ", "キ", "ス",
            "ト",
        ],
    );
}

/// `grep -o '[[:alpha:]]\{3\}'`: an interval over a Unicode-aware class,
/// matching GNU byte for byte (`hél`/`wör`, `日本語`/`テキス`).
#[tokio::test]
async fn alpha_class_interval_matches_gnu_grep() {
    let (_dir, kernel) = unicode_fixture_kernel(UNICODE_ALPHA_FIXTURE);
    let (out, code) = run(&kernel, r#"grep -o '[[:alpha:]]\{3\}' fx.txt"#).await;
    assert_eq!(code, 0);
    assert_eq!(lines(&out), &["hél", "wör", "日本語", "テキス"]);
}

/// `-i '[[:upper:]]'` case-folds a non-ASCII letter, as GNU does: `héllo
/// wörld` matches because `é`/`ö` fold to letters `[:upper:]` recognizes.
///
/// The all-CJK line is GNU's too; see the gap test below.
#[tokio::test]
async fn upper_class_case_folds_non_ascii_letters_like_gnu_grep() {
    let (_dir, kernel) = unicode_fixture_kernel(UNICODE_ALPHA_FIXTURE);
    let (out, code) = run(&kernel, r#"grep -i '[[:upper:]]' fx.txt"#).await;
    assert_eq!(code, 0);
    assert!(lines(&out).contains(&UNICODE_ALPHA_FIXTURE[0]));
}

/// glibc's `-i` widens `[:upper:]`/`[:lower:]` to `[:alpha:]`, so GNU also
/// matches the case-less all-CJK line. kaish's class table does not see `-i`.
#[tokio::test]
#[ignore = "gap: GNU -i widens [:upper:]/[:lower:] to [:alpha:]"]
async fn gap_ignore_case_widens_upper_class_to_alpha_like_gnu_grep() {
    let (_dir, kernel) = unicode_fixture_kernel(UNICODE_ALPHA_FIXTURE);
    let (out, code) = run(&kernel, r#"grep -i '[[:upper:]]' fx.txt"#).await;
    assert_eq!(code, 0);
    assert_eq!(lines(&out), UNICODE_ALPHA_FIXTURE);
}

/// GNU grep's output for the wider bracket-class table, row by row.
#[rstest]
// `[[:alpha:]]` matches every line, including the digit-script line — GNU
// classifies a non-ASCII decimal digit as alphabetic too (glibc quirk).
#[case(r#""#, r#"[[:alpha:]]"#, 0, UNICODE_CLASS_FIXTURE)]
#[case(r#"-E"#, r#"[[:alpha:]]"#, 0, UNICODE_CLASS_FIXTURE)]
// `[:digit:]` stays ASCII-only: neither the Arabic-Indic nor the fullwidth
// digit matches, only the ASCII `123`.
#[case(r#""#, r#"[[:digit:]]"#, 0, &[r#"abc123 ABC"#])]
// Negation: every line has a non-letter somewhere except the all-CJK line.
#[case(r#""#, r#"[^[:alpha:]]"#, 0, &[r#"héllo wörld"#, r#"abc123 ABC"#, "\u{0663} \u{FF13}", "「line」\u{2014} \u{00BF}", "a\u{00A0}b", "a\u{3000}b", "cafe\u{0301} bar"])]
// `[:space:]`/`[:blank:]`: the ideographic space counts, the no-break space
// does not (both match the same lines here; they differ on vertical
// whitespace GNU treats as `[:space:]` only, not covered by this fixture).
#[case(r#""#, r#"[[:space:]]"#, 0, &[r#"héllo wörld"#, r#"abc123 ABC"#, "\u{0663} \u{FF13}", "「line」\u{2014} \u{00BF}", "a\u{3000}b", "cafe\u{0301} bar"])]
#[case(r#""#, r#"[[:blank:]]"#, 0, &[r#"héllo wörld"#, r#"abc123 ABC"#, "\u{0663} \u{FF13}", "「line」\u{2014} \u{00BF}", "a\u{3000}b", "cafe\u{0301} bar"])]
// `[:punct:]`: corner brackets/dash/question, the no-break space (glibc
// quirk), and the combining mark.
#[case(r#""#, r#"[[:punct:]]"#, 0, &["「line」\u{2014} \u{00BF}", "a\u{00A0}b", "cafe\u{0301} bar"])]
#[tokio::test]
async fn bracket_classes_match_gnu_grep_on_non_ascii_corpus(
    #[case] flags: &str,
    #[case] pattern: &str,
    #[case] gnu_code: i64,
    #[case] gnu_lines: &[&str],
) {
    let (_dir, kernel) = unicode_fixture_kernel(UNICODE_CLASS_FIXTURE);
    let (out, code) = run(&kernel, &format!("grep {flags} '{pattern}' fx.txt")).await;
    assert_eq!(lines(&out), gnu_lines, "pattern {pattern:?}");
    assert_eq!(code, gnu_code, "pattern {pattern:?}");
}

/// A class mixed with an explicit range and `_`: `[[:alpha:]0-9_]\+` joins
/// letters and ASCII digits into one run, unlike `[[:alpha:]]` alone, which
/// would split `abc123` at the digits.
#[tokio::test]
async fn alpha_class_mixed_with_range_and_underscore_matches_gnu_grep() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("fx.txt"), "abc123 ABC\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -o '[[:alpha:]0-9_]\+' fx.txt"#).await;
    assert_eq!(code, 0);
    assert_eq!(lines(&out), &["abc123", "ABC"]);
}

/// Known gap: a combining mark (`e` + U+0301) is a word character for the
/// regex engine's `\w`/`\b` (Unicode `\p{Mark}` continues a word) but not for
/// glibc's (not alphanumeric, so it ends one). GNU's `-w cafe` matches
/// `cafe` before the mark; kaish's does not, since the engine sees no
/// boundary there.
#[tokio::test]
#[ignore = "gap: a combining mark is a \\w character for the regex engine, not for glibc"]
async fn gap_word_boundary_before_combining_mark_matches_gnu_grep() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("fx.txt"), "cafe\u{0301} bar\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -wo 'cafe' fx.txt"#).await;
    assert_eq!(code, 0);
    assert_eq!(lines(&out), &["cafe"]);
}

// ─── Scenarios ───────────────────────────────────────────────────────────────

/// Recursive search (the directory walk) uses the same translation: the
/// idiom from kaibo telemetry, a function name with its open paren.
#[tokio::test]
async fn recursive_search_takes_a_literal_open_paren() {
    let dir = tempdir().unwrap();
    fs::create_dir(dir.path().join("src")).unwrap();
    fs::write(dir.path().join("src/lib.rs"), "pub fn consult(q: &str) {}\nfn other() {}\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -rn "fn consult(" src"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "src/lib.rs:1:pub fn consult(q: &str) {}");
}

/// Multi-term batch: one call, three terms.
#[tokio::test]
async fn bre_alternation_three_terms() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "alpha\nbeta\ngamma\ndelta\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -c 'alpha\|gamma\|delta' t.txt"#).await;
    assert_eq!(code, 0, "should match; out={out:?}");
    assert_eq!(out.trim(), "3", "three of four lines match: {out:?}");
}

/// `\(...\)\+`: a BRE group with a BRE quantifier.
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

/// Bare `|` is a literal pipe, as in GNU grep: `foo|bar` matches only the
/// line that contains that text. `grep -E` is the alternation spelling.
#[tokio::test]
async fn bare_pipe_is_literal_without_extended() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "foo\nbar\nfoo|bar\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep 'foo|bar' t.txt"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "foo|bar", "only the literal line: {out:?}");

    let (out, code) = run(&kernel, r#"grep -E 'foo|bar' t.txt"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "foo\nbar\nfoo|bar", "-E alternates: {out:?}");
}

/// `-E` (strict ERE) is unchanged: `\|` is a literal pipe, so `foo\|bar`
/// matches the literal 7-char string `foo|bar`, not `foo` OR `bar`.
#[tokio::test]
async fn extended_mode_backslash_pipe_is_literal() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "foo|bar\nfoo\nbar\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -E 'foo\|bar' t.txt"#).await;
    assert_eq!(code, 0, "literal pipe should match; out={out:?}");
    assert_eq!(out.trim(), "foo|bar", "only the literal-pipe line, no alternation: {out:?}");
}

/// `-E` with bare ERE operators is unchanged.
#[tokio::test]
async fn extended_mode_bare_ere_operators_work() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "foo\nbar\nbaz\nxx\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -E '(foo|bar)' t.txt"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "foo\nbar", "{out:?}");

    let (out, _) = run(&kernel, r#"grep -cE 'x{2}' t.txt"#).await;
    assert_eq!(out, "1", "ERE interval: {out:?}");
}

/// `-E -w` with alternation anchors every alternative, not just the ends.
#[tokio::test]
async fn extended_word_regexp_wraps_the_whole_alternation() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "foo\nfoox\nxbar\nbar\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -Ew 'foo|bar' t.txt"#).await;
    assert_eq!(code, 0, "out={out:?}");
    assert_eq!(out, "foo\nbar", "{out:?}");
}

/// `-F` is unaffected: every character is literal.
#[tokio::test]
async fn fixed_strings_pipe_stays_literal() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "a|b\nab\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -F 'a|b' t.txt"#).await;
    assert_eq!(code, 0, "literal pipe should match; out={out:?}");
    assert_eq!(out, "a|b", "{out:?}");
}

/// `-F` keeps a backslash as text: `a\|b` is the 4-char string, never
/// alternation.
#[tokio::test]
async fn fixed_strings_backslash_pipe_is_verbatim_text() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("t.txt"), "a\\|b\na|b\na\nb\n").unwrap();
    let kernel = kernel_at(dir.path());

    let (out, code) = run(&kernel, r#"grep -F 'a\|b' t.txt"#).await;
    assert_eq!(code, 0, "verbatim text should match; out={out:?}");
    assert_eq!(out.trim(), "a\\|b", "only the backslash-pipe line: {out:?}");
}

// ─── `-E` reads the same GNU escape table as the default mode ────────────────
//
// `grep -E '\d'` matches the letter `d`, not the regex engine's own
// Perl-style digit class — GNU's stray-backslash rule applies in ERE too.
// `/usr/bin/grep` (GNU grep 3.12, `LC_ALL=C.UTF-8`) over [`FIXTURE`].

#[rstest]
#[case(r#"\d"#, 0, &[r#"d1"#])] // GNU: grep: warning: stray \ before d
#[case(r#"\n"#, 0, &[r#"fn consult(q)"#, r#"nt"#])] // GNU: grep: warning: stray \ before n
#[case(r#"\w+"#, 0, &[r#"fn consult(q)"#, r#"KjCaller { x }"#, r#"(foo|bar)"#, r#"foo"#, r#"bar"#, r#"a+b"#, r#"aab"#, r#"a?b"#, r#"b"#, r#"x{2}"#, r#"xx"#, r#"a|b"#, r#"*foo"#, r#"a^b"#, r#"a$b"#, r#"a.b"#, r#"axb"#, r#"ab"#, r#"a\b"#, r#"x[y"#, r#"a&b"#, r#"a~b"#, r#"w-x"#, r#"foo bar"#, r#"foobar"#, r#"a]b"#, r#"d1"#, r#"123"#, r#"nt"#, r#"a}b"#, r#"+a"#, r#"aaa"#, r#"a"b"#, r#"FOO"#])]
#[case(r#"\<bar"#, 0, &[r#"(foo|bar)"#, r#"bar"#, r#"foo bar"#])]
#[case(r#"bar\>"#, 0, &[r#"(foo|bar)"#, r#"bar"#, r#"foo bar"#, r#"foobar"#])]
#[case(r#"a\+b"#, 0, &[r#"a+b"#])] // escaped ERE meta is already literal, no warning
#[tokio::test]
async fn extended_mode_reads_the_same_gnu_escapes(
    #[case] pattern: &str,
    #[case] gnu_code: i64,
    #[case] gnu_lines: &[&str],
) {
    let (_dir, kernel) = fixture_kernel();
    let (out, code) = run(&kernel, &format!("grep -E '{pattern}' fx.txt")).await;
    assert_eq!(lines(&out), gnu_lines, "pattern {pattern:?}");
    assert_eq!(code, gnu_code, "pattern {pattern:?}");
}

/// `-E`'s stray-backslash warning: same text as the default mode.
#[rstest]
#[case(r"\d", r"grep: warning: stray \ before d", &["d1"])]
#[case(r"\-", r"grep: warning: stray \ before -", &["w-x"])]
#[tokio::test]
async fn extended_mode_stray_backslash_warns_like_gnu_grep(
    #[case] pattern: &str,
    #[case] gnu_stderr: &str,
    #[case] gnu_lines: &[&str],
) {
    let (_dir, kernel) = fixture_kernel();
    let result = kernel
        .execute(&format!("grep -E '{pattern}' fx.txt"))
        .await
        .expect("a stray backslash is a warning, not an error");
    assert_eq!(lines(result.text_out().trim()), gnu_lines, "pattern {pattern:?}");
    assert_eq!(result.err.trim(), gnu_stderr, "pattern {pattern:?}");
}

/// `-E` back-references (`\1`): refused, the same gap the default mode
/// documents — the regex engine has no back-references in any dialect.
#[tokio::test]
async fn extended_mode_refuses_back_references() {
    let (_dir, kernel) = fixture_kernel();
    let message = match kernel.execute(r#"grep -E '(o)\1' fx.txt"#).await {
        Err(e) => e.to_string(),
        Ok(result) => {
            assert_eq!(result.code, 2, "must fail with exit 2");
            result.err.clone()
        }
    };
    assert!(message.contains("back-reference"), "{message}");
}

/// GNU grep's ERE back-references are a GNU extension: `(o)\1` matches
/// "foo|bar" the way "oo" is a repeated group. The regex engine has none.
#[tokio::test]
#[ignore = "gap: GNU ERE back-references; the regex crate has no back-references"]
async fn gap_extended_mode_back_references_match_like_gnu_grep() {
    let (_dir, kernel) = fixture_kernel();
    let (out, code) = run(&kernel, r#"grep -E '(o)\1' fx.txt"#).await;
    assert_eq!(lines(&out), &["(foo|bar)", "foo", "*foo", "foo bar", "foobar"]);
    assert_eq!(code, 0);
}
