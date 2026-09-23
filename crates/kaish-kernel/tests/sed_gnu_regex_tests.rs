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

/// `-E`'s POSIX bracket classes are Unicode-aware, matching GNU sed in a
/// UTF-8 locale — the engine's own `[[:alpha:]]` support is ASCII-only.
/// `/usr/bin/sed` 4.10, `LC_ALL=C.UTF-8`, over
/// `héllo日本語٣ 0␠x　y３「line」` (␠ = U+00A0 NBSP, between the two
/// `[:space:]`-vs-`[:punct:]` cases): `alpha` also takes the Arabic-Indic
/// digit `٣` (a glibc quirk shared with `grep`'s table) but not ASCII `0`;
/// `digit` stays ASCII-only either dialect; `space` takes the ideographic
/// space U+3000 but not the NBSP, which glibc classifies `[:punct:]` instead.
#[rstest]
#[case(
    "-E",
    "s/[[:alpha:]]/X/g",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}",
    "XXXXXXXXX 0\u{a0}X\u{3000}XX\u{300c}XXXX\u{300d}"
)]
#[case(
    "-E",
    "s/[[:digit:]]/D/g",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} D\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}"
)]
#[case(
    "-E",
    "s/[[:space:]]/_/g",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663}_0\u{a0}x_y\u{ff13}\u{300c}line\u{300d}"
)]
#[case(
    "-E",
    "s/[[:punct:]]/P/g",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0Px\u{3000}y\u{ff13}PlineP"
)]
#[case(
    "-r",
    "s/[[:alpha:]]/X/g",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}",
    "XXXXXXXXX 0\u{a0}X\u{3000}XX\u{300c}XXXX\u{300d}"
)]
#[tokio::test]
async fn extended_mode_posix_classes_are_unicode_aware(
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

// ─── `-E`/`-r` reads the same GNU escape table as the default mode ───────────
//
// `sed -E 's/\d/X/'` matches the letter `d`, not the regex engine's own
// Perl-style digit class. Confirmed against `/usr/bin/sed` 4.10,
// `LC_ALL=C.UTF-8`. Unlike `grep -E`, GNU sed prints no stray-backslash
// warning in either dialect.

#[rstest]
#[case("-E", r"s/\d/X/", "adb", "aXb")]
#[case("-E", r"s/\w/X/g", "a3b", "XXX")]
#[case("-E", r"s/\<foo\>/X/", "a foo b", "a X b")]
#[case("-E", r"s/a\+b/X/", "a+b", "X")] // an escaped ERE meta is already literal
#[case("-E", r"s/a\-b/X/", "a-b", "X")]
#[tokio::test]
async fn extended_mode_reads_the_same_gnu_escapes(
    #[case] flags: &str,
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_sed(flags, program, input).await;
    assert_eq!(out, expected, "program {program:?}, input {input:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// `-E` back-references: GNU sed runs `(a)\1` as a GNU ERE extension when a
/// group precedes it, and refuses `\1` outright when none does.
#[rstest]
#[case(r"s/(a)\1/X/")]
#[case(r"s/\1/X/")]
#[tokio::test]
async fn extended_mode_back_reference_is_refused(#[case] program: &str) {
    let (_dir, kernel) = fixture_kernel();
    let message = match kernel.execute(&format!("sed -E '{program}' fx.txt")).await {
        Err(e) => e.to_string(),
        Ok(result) => {
            assert_ne!(result.code, 0, "program {program:?} must fail");
            result.err.clone()
        }
    };
    assert!(message.contains("back-reference"), "program {program:?}: {message}");
}

/// GNU sed's ERE back-references are a GNU extension: `(a)\1` matches "aa".
/// The regex engine has none in any dialect.
#[tokio::test]
#[ignore = "gap: GNU ERE back-references in the pattern; the regex crate has no back-references"]
async fn gap_extended_mode_back_reference_matches_like_gnu_sed() {
    let (out, code) = run_sed("-E", r"s/(a)\1/X/", "aa").await;
    assert_eq!(out, "X");
    assert_eq!(code, 0);
}

// ─── Bracket expressions: GNU sed's control escapes reach inside `[...]` ──────
//
// `sed 's/[\t]/X/'` matches a TAB, not a class of `\` or `t` — GNU sed reads
// `\n \t \r \a \f \v` the same way inside a bracket expression as outside
// one, in both dialects. kaish already gets this right: `\t` and its
// siblings are expanded to the real control byte by
// `expand_sed_control_escapes` *before* the bracket is even parsed, so the
// bracket reader — grep's, reused as-is — never sees the backslash. Locked
// in here since it was previously exercised only outside brackets.

#[rstest]
#[case("", r"s/[\t]/X/", "a\tb", "aXb")]
#[case("-E", r"s/[\t]/X/", "a\tb", "aXb")]
#[tokio::test]
async fn bracket_control_escapes_are_real_characters(
    #[case] flags: &str,
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_sed(flags, program, input).await;
    assert_eq!(out, expected, "program {program:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// `[\-] [\\] [\]]` inside a bracket read exactly like `grep`'s: no
/// escaping happens there at all, so each backslash is its own literal
/// class member alongside the character after it. Confirmed against
/// `/usr/bin/sed`: `[\-]` matches a bare backslash *and* a dash.
#[rstest]
#[case(r"s/[\-]/X/", "a\\b", "aXb")]
#[case(r"s/[\-]/X/", "a-b", "aXb")]
#[case(r"s/[\\]/X/", "a\\b", "aXb")]
#[tokio::test]
async fn bracket_non_control_escapes_match_gnu_sed_literally(
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_sed("", program, input).await;
    assert_eq!(out, expected, "program {program:?}, input {input:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// GNU sed's `\dNNN`/`\oNNN`/`\xHH`/`\cX` decimal/octal/hex/control escapes
/// (documented gap: `expand_sed_control_escapes` covers `\n \t \r \a \f \v`
/// only) leave a visible trace even with no digits: bare `[\d]` drops the
/// backslash and matches only the letter `d`, not `\` or `d`. Confirmed
/// against `/usr/bin/sed`: `[\d]` against a lone backslash does not match.
#[tokio::test]
#[ignore = "gap: GNU sed's \\dNNN/\\oNNN/\\xHH/\\cX escapes are not implemented; \
            bare \\d degenerates to dropping the backslash"]
async fn gap_bracket_unrecognized_decimal_escape_drops_backslash_like_gnu_sed() {
    let (out, code) = run_sed("", r"s/[\d]/X/", "adb").await;
    assert_eq!(out, "aXb");
    assert_eq!(code, 0);

    // The backslash itself must NOT be part of the class.
    let (out, code) = run_sed("", r"s/[\d]/X/", "a\\b").await;
    assert_eq!(out, "a\\b", "backslash must not match once GNU's \\d wins");
    assert_eq!(code, 0);
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

// ─── `-E` does NOT share `grep -E`'s leniency for a bare operator ────────────
//
// `grep -E`'s strict-ERE translator now reads a `{`/`*`/`+`/`?` with nothing
// to repeat as a literal character (`grep_gnu_bre_tests.rs`). `sed -E`
// shares the same translator, but NOT this leniency: confirmed against
// `/usr/bin/sed -E` 4.10 on the same patterns, sed's own regcomp refuses
// every one of them, unchanged from before the leniency existed. Both
// dialects DO share GNU's `{,m}` shorthand for `{0,m}` — that is a genuine
// interval GNU sed also accepts, not part of the leniency split.

#[rstest]
#[case("s/{/X/")]
#[case("s/a{/X/")]
#[case("s/a{x}/X/")]
#[case("s/a{1/X/")]
#[case("s/{2}/X/")]
#[case("s/^{/X/")]
#[case("s/*a/X/")]
#[case("s/(*a)/X/")]
#[case("s/+a/X/")]
#[case("s/?a/X/")]
#[case("s/(+)/X/")]
#[tokio::test]
async fn extended_mode_still_refuses_a_bare_operator(#[case] program: &str) {
    let (_dir, kernel) = fixture_kernel();
    let result = kernel.execute(&format!("sed -E '{program}' fx.txt")).await;
    let code = match result {
        Err(_) => return, // validation caught it before the edit ran
        Ok(result) => result.code,
    };
    assert_ne!(code, 0, "program {program:?} must still fail, like /usr/bin/sed -E");
}

/// GNU's `{,m}` shorthand for `{0,m}` works in `-E` mode too — the engine
/// has no syntax for an omitted low bound, so it needs the same rewrite
/// `grep -E` gets. Confirmed against `/usr/bin/sed -E`: `a{,2}` matches
/// zero to two `a`s, same as `a{0,2}`.
#[rstest]
#[case("-E", "s/a{,2}/X/", "aa", "X")]
#[case("-E", "s/a{,2}/X/", "b", "Xb")]
#[tokio::test]
async fn extended_mode_omitted_low_bound_is_gnu_shorthand(
    #[case] flags: &str,
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_sed(flags, program, input).await;
    assert_eq!(out, expected, "program {program:?}, input {input:?}");
    assert_eq!(code, 0, "program {program:?}");
}

// ─── Unknown class names always refuse, in both dialects ─────────────────────
//
// `sed -E 's/[[:foo:]]/X/'` used to match — the strict-ERE translator left
// an unrecognized class name exactly as written, letting the engine read it
// as a plain bracket set instead of refusing the way GNU sed does.
// `[:alpha:]` (no outer brackets) is a second, distinct GNU refusal;
// confirmed against `/usr/bin/sed`/`/usr/bin/sed -E`, `LC_ALL=C.UTF-8`.

#[rstest]
#[case("", r#"s/[[:foo:]]/X/"#, "foo")] // GNU: Invalid character class name
#[case("-E", r#"s/[[:foo:]]/X/"#, "foo")] // GNU: Invalid character class name
#[case("", r#"s/[:alpha:]/X/"#, "space")] // GNU: character class syntax is [[:space:]], not [:space:]
#[case("-E", r#"s/[:alpha:]/X/"#, "space")] // GNU: character class syntax is [[:space:]], not [:space:]
#[tokio::test]
async fn unrecognized_class_name_refuses(#[case] flags: &str, #[case] program: &str, #[case] named: &str) {
    let (_dir, kernel) = fixture_kernel();
    let message = match kernel.execute(&format!("sed {flags} '{program}' fx.txt")).await {
        Err(e) => e.to_string(),
        Ok(result) => {
            assert_ne!(result.code, 0, "flags {flags:?}, program {program:?} must fail");
            result.err.clone()
        }
    };
    assert!(message.contains(named), "flags {flags:?}, program {program:?}: {message}");
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
