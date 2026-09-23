//! Kernel-routed tests: `sed` without `-E`/`-r` follows GNU BRE, exactly like
//! `grep` without `-E` (`tests/grep_gnu_bre_tests.rs`). `-E`/`-r` is strict
//! ERE.
//!
//! GNU sed is the specification for the default mode:
//!
//! - bare `( ) { } | + ?` are literal characters — `sed 's/fn consult(/x/'`
//!   matches the literal text `fn consult(`, the audit's headline case;
//! - `\( \)` group, `\{n,m\}` is an interval, `\|` `\+` `\?` are operators;
//! - `-E`/`-r` (strict ERE) reverses this: bare forms are operators, a
//!   backslash before one is the literal character;
//! - GNU sed reads `\n \t \r \a \f \v` as their real control character in
//!   BOTH dialects — unlike GNU grep, where a backslash before an ordinary
//!   letter is that letter with a "stray \" warning (GNU sed prints no such
//!   warning at all, in any dialect);
//! - a delimiter other than `/` works the same way — an escaped delimiter is
//!   always literal, even when that character also has a regex meaning
//!   (`s|a\|b|X|`'s `\|` is a literal `|`, not alternation, because `|` is
//!   the chosen delimiter here).
//!
//! The expected values are what `/usr/bin/sed` (GNU sed 4.10) produced for
//! the same program over a small input, recorded as literals. CI need not
//! have GNU sed; these tests never run it.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use rstest::rstest;
use tempfile::tempdir;

use common::{kernel_at, run};

/// Run `sed FLAGS 'PROGRAM' in.txt` over `input` and return `(stdout, exit)`.
async fn run_sed(flags: &str, program: &str, input: &str) -> (String, i64) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("in.txt"), input).unwrap();
    let kernel = kernel_at(dir.path());
    run(&kernel, &format!("sed {flags} '{program}' in.txt")).await
}

/// GNU sed's output for `sed PROGRAM in.txt`, default (GNU BRE) mode: a bare
/// ERE meta is literal, so it never matches an intentional operator's text.
#[rstest]
#[case("s/fn consult(/X/", "fn consult(q)", "Xq)")]
#[case("s/a{2}/X/", "a{2}", "X")]
#[case("s/a{2}/X/", "aa", "aa")]
#[case("s/a|b/X/", "a|b", "X")]
#[case("s/a+/X/", "a+", "X")]
#[case("s/a?/X/", "a?", "X")]
#[tokio::test]
async fn bare_ere_metas_are_literal_by_default(
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_sed("", program, input).await;
    assert_eq!(out, expected, "program {program:?}, input {input:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// The GNU BRE spellings of the same operators, default mode.
#[rstest]
#[case(r"s/a\{2\}/X/", "aa", "X")]
#[case(r"s/cat\|dog/X/g", "cat dog", "X X")]
#[case(r"s/\(a\)\(b\)/\2\1/", "ab", "ba")]
#[case(r"s/a\+/X/", "aaa", "X")]
#[case(r"s/a\?b/X/", "ab", "X")]
#[case(r"s/a\?b/X/", "b", "X")]
#[tokio::test]
async fn escaped_bre_metas_are_operators_by_default(
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_sed("", program, input).await;
    assert_eq!(out, expected, "program {program:?}, input {input:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// `-E`/`-r` reverses the reading: bare is the operator, escaped is literal.
#[rstest]
#[case("-E", r"s/(a)(b)/\2\1/", "ab", "ba")]
#[case("-E", "s/a{2}/X/", "aa", "X")]
#[case("-E", "s/cat|dog/X/g", "cat dog", "X X")]
#[case("-E", r"s/\(x\)/Y/", "(x)", "Y")]
#[case("-E", r"s/a\|b/X/", "a|b", "X")]
#[case("-r", r"s/a\|b/X/", "a|b", "X")]
#[tokio::test]
async fn extended_mode_is_strict_ere(
    #[case] flags: &str,
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_sed(flags, program, input).await;
    assert_eq!(out, expected, "program {program:?}, input {input:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// GNU sed reads `\n \t \r` as real control characters, not "stray \n"
/// literal letters — the opposite of GNU grep. `\n` matters most: it is
/// what lets a pattern match across the two lines `N` joins.
#[rstest]
#[case("", r"s/a\tb/X/", "a\tb", "X")]
#[case("-E", r"s/a\tb/X/", "a\tb", "X")]
#[tokio::test]
async fn control_escapes_are_real_characters(
    #[case] flags: &str,
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_sed(flags, program, input).await;
    assert_eq!(out, expected, "program {program:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// A custom delimiter's escaped form is always literal, even when that
/// character also has a BRE meaning — the delimiter rule wins.
#[rstest]
#[case(r"s|a/b|X|", "a/b", "X")]
#[case(r"s|a\|b|X|", "a|b", "X")]
#[case(r"s|a\|b|X|", "a", "a")]
#[tokio::test]
async fn custom_delimiter_escape_is_always_literal(
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_sed("", program, input).await;
    assert_eq!(out, expected, "program {program:?}, input {input:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// `s///` flags are unaffected by the regex-dialect fix: `I` case-folds,
/// digit+`g` replaces from the Nth match onward.
#[rstest]
#[case("s/foo/x/I", "FOO", "x")]
#[case("s/a/X/2g", "a a a a", "a X X X")]
#[tokio::test]
async fn s_flags_still_work(#[case] program: &str, #[case] input: &str, #[case] expected: &str) {
    let (out, code) = run_sed("", program, input).await;
    assert_eq!(out, expected, "program {program:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// `&` and `\N` in the *replacement* are untouched by the pattern-dialect
/// fix (they're not regex syntax at all).
#[tokio::test]
async fn ampersand_and_backreference_replacement_still_work() {
    let (out, code) = run_sed("", "s/foo/[&]/", "hello foo world").await;
    assert_eq!(out, "hello [foo] world");
    assert_eq!(code, 0);

    let (out, code) = run_sed("-E", r"s/(foo)(bar)/\2\1/", "foobar").await;
    assert_eq!(out, "barfoo");
    assert_eq!(code, 0);
}

// ─── Addresses: /re/, ranges, and a real multi-line file ─────────────────────

const FIXTURE: &[&str] =
    &["fn consult(q)", "(foo|bar)", "foo", "bar", "a+b", "a?b", "START", "mid", "END"];

fn fixture_kernel() -> (tempfile::TempDir, kaish_kernel::Kernel) {
    let dir = tempdir().unwrap();
    let mut text = FIXTURE.join("\n");
    text.push('\n');
    fs::write(dir.path().join("fx.txt"), text).unwrap();
    let kernel = kernel_at(dir.path());
    (dir, kernel)
}

#[tokio::test]
async fn pattern_address_reads_a_literal_paren() {
    let (_dir, kernel) = fixture_kernel();
    let (out, code) = run(&kernel, r#"sed -n '/fn consult(/p' fx.txt"#).await;
    assert_eq!(out, "fn consult(q)");
    assert_eq!(code, 0);
}

#[tokio::test]
async fn pattern_address_alternates_on_escaped_pipe() {
    let (_dir, kernel) = fixture_kernel();
    let (out, code) = run(&kernel, r#"sed -n '/foo\|bar/p' fx.txt"#).await;
    assert_eq!(out, "(foo|bar)\nfoo\nbar");
    assert_eq!(code, 0);
}

#[tokio::test]
async fn pattern_range_address_selects_the_span() {
    let (_dir, kernel) = fixture_kernel();
    let (out, code) = run(&kernel, r#"sed -n '/START/,/END/p' fx.txt"#).await;
    assert_eq!(out, "START\nmid\nEND");
    assert_eq!(code, 0);
}

/// The recursive-search idiom from `grep_gnu_bre_tests.rs`, applied to `sed`'s
/// delete command: a function signature's open paren, matched literally.
#[tokio::test]
async fn delete_by_literal_paren_address() {
    let (_dir, kernel) = fixture_kernel();
    let (out, code) = run(&kernel, r#"sed '/fn consult(/d' fx.txt"#).await;
    assert_eq!(
        out,
        "(foo|bar)\nfoo\nbar\na+b\na?b\nSTART\nmid\nEND",
        "the literal-paren line alone is deleted",
    );
    assert_eq!(code, 0);
}

// ─── Refusals: what GNU sed rejects, sed rejects too ──────────────────────────

/// Patterns GNU sed refuses. kaish refuses them too, before the edit runs,
/// naming the construct and sed's strict-ERE flags.
#[rstest]
#[case(r"s/a\)/X/", "unmatched")] // GNU: Unmatched ) or \)
#[case(r"s/\(a/X/", "unmatched")] // GNU: Unmatched ( or \(
#[case(r"s/a\{2/X/", "unmatched")] // GNU: Unmatched \{
#[case(r"s/a\{2,1\}/X/", "invalid")] // GNU: Invalid content of \{\}
#[tokio::test]
async fn invalid_patterns_fail_like_gnu_sed(#[case] program: &str, #[case] named: &str) {
    let (_dir, kernel) = fixture_kernel();
    let message = match kernel.execute(&format!("sed '{program}' fx.txt")).await {
        Err(e) => e.to_string(),
        Ok(result) => {
            assert_ne!(result.code, 0, "program {program:?} must fail");
            result.err.clone()
        }
    };
    assert!(
        message.to_lowercase().contains(named),
        "program {program:?}: {message}",
    );
    assert!(message.contains("GNU BRE"), "names the dialect: {message}");
    assert!(message.contains("-E/-r"), "names the ERE override: {message}");
}

// ─── Known gaps: GNU sed behavior the regex engine cannot reach ──────────────

/// GNU sed supports back-references (`\1` to `\9`) in the pattern itself
/// (not just the replacement) — `/usr/bin/sed` matches `\(a\)\1` against
/// "aa". Rust's `regex` crate has none, so kaish refuses the pattern.
#[tokio::test]
#[ignore = "gap: GNU BRE back-references in the pattern; the regex crate has no back-references"]
async fn gap_backreference_in_pattern_matches_like_gnu_sed() {
    let (out, code) = run_sed("", r"s/\(a\)\1/X/", "aa").await;
    assert_eq!(out, "X");
    assert_eq!(code, 0);
}

/// `\n` matches a real newline in the pattern space, which only exists after
/// the `N` command joins two lines — `/usr/bin/sed`: `N;s/foo\nbar/JOINED/`
/// on "foo\nbar" prints "JOINED". kaish's sed has no `N` command (or the
/// other multi-line commands: `n D P h H g G x`), so this translation is
/// currently unreachable in practice; the `\t`/`\r` cases above exercise the
/// same code path within one line, where it is reachable today.
#[tokio::test]
#[ignore = "gap: kaish's sed has no N command, so a pattern space is always one line and \\n can never match"]
async fn gap_newline_control_escape_matches_lines_n_joined() {
    let (out, code) = run_sed("", r"N;s/foo\nbar/JOINED/", "foo\nbar").await;
    assert_eq!(out, "JOINED");
    assert_eq!(code, 0);
}

/// GNU sed's empty regex `//` reuses the last regex that actually ran — a
/// second `s//+/` after `s/X/-/` still targets `X`, not "the start of the
/// line" (confirmed against `/usr/bin/sed`: `aXbXc` → `a-b+c`). kaish has no
/// last-regex state, so an empty pattern instead matches the empty string at
/// every position.
#[tokio::test]
#[ignore = "gap: GNU sed's // reuses the last regex; kaish tracks no such state"]
async fn gap_empty_regex_reuses_last_pattern_like_gnu_sed() {
    let (out, code) = run_sed("", r"s/X/-/; s//+/", "aXbXc").await;
    assert_eq!(out, "a-b+c");
    assert_eq!(code, 0);
}
