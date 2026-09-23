//! Kernel-routed tests: `awk` follows gawk's ERE exactly.
//!
//! awk has no BRE mode. gawk's own ERE already reads the way the regex
//! engine reads ERE — bare `( ) { } | + ?` are operators, `\( \) \{n,m\} \|
//! \+ \?` are literal — so this file mostly confirms there is nothing left
//! to translate for the audit's headline case: `awk '/fn consult\(/'` is a
//! literal match, and a bare unescaped `(` is a real, unclosed group (an
//! error in both gawk and kaish, not a silent pass).
//!
//! Where gawk's own ERE reads differently from the engine's raw syntax:
//!
//! - a bare `{` commits to interval parsing only when a digit follows it
//!   immediately — `a{x}` is literal, `a{2}` is the interval `{2}`;
//! - `\<` `\>` `` \` `` `\'` `\y` are GNU word/buffer-boundary escapes;
//! - `\b` is gawk's backspace character, not a word boundary (`\y` is);
//! - `\d` `\D` are not gawk regexp operators — the literal letter, with a
//!   warning, not the engine's own Perl-style digit class;
//! - `-F`/`-v`/a command-line `var=value` run through the same string-escape
//!   rules as a program string literal before the value is used — `-F '\|'`
//!   sees the CLI text unescape to a literal `|` before FS is even compiled.
//!
//! The expected values are what `gawk` (GNU Awk 5.4.1) produced for the same
//! program, recorded as literals. CI need not have gawk; these tests never
//! run it.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use rstest::rstest;
use tempfile::tempdir;

use common::{kernel_at, run};

/// Run an awk program over a single line of stdin and return `(stdout, exit)`.
async fn run_awk(program: &str, input: &str) -> (String, i64) {
    let kernel = kernel_at(tempdir().unwrap().path());
    run(&kernel, &format!("printf '%s' '{input}' | awk '{program}'")).await
}

// ─── The audit's headline case: a literal paren ───────────────────────────────

#[tokio::test]
async fn escaped_paren_is_a_literal_match() {
    let (out, code) = run_awk(r#"$0 ~ /fn consult\(/ {print}"#, "fn consult(q)").await;
    assert_eq!(out, "fn consult(q)");
    assert_eq!(code, 0);
}

/// A bare, unescaped `(` is a real group in gawk ERE, unclosed here — a
/// compile error in both gawk and kaish, not a silent literal fallback.
#[tokio::test]
async fn bare_unmatched_paren_is_a_loud_error() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let result = kernel
        .execute(r#"echo x | awk '/fn consult(/{print}'"#)
        .await
        .expect("a bad regex is a runtime error, not a validation error");
    assert_ne!(result.code, 0, "unbalanced ( must fail: {result:?}");
}

/// `[(]` — a bracket expression — is unaffected: every character inside is
/// already literal in gawk's ERE, same as the engine's own bracket syntax.
#[tokio::test]
async fn bracket_class_around_paren_is_literal() {
    let (out, code) = run_awk(r#"$0 ~ /[(]/ {print}"#, "fn consult(q)").await;
    assert_eq!(out, "fn consult(q)");
    assert_eq!(code, 0);
}

// ─── Bare operators vs. escaped-literal forms ────────────────────────────────

#[rstest]
#[case("aa", r#"$0 ~ /a{2}/ {print "match"}"#, "match")]
#[case("a{x}", r#"$0 ~ /a{x}/ {print "match"}"#, "match")]
#[case("a{2}", r#"$0 ~ /a\{2\}/ {print "match"}"#, "match")]
#[case("dog", r#"$0 ~ /cat|dog/ {print "match"}"#, "match")]
#[case("cat|dog", r#"$0 ~ /cat\|dog/ {print "match"}"#, "match")]
#[case("cat", r#"$0 ~ /cat\|dog/ {print "match"; next} {print "no"}"#, "no")]
#[case("aaa", r#"$0 ~ /a+/ {print "match"}"#, "match")]
#[case("a+", r#"$0 ~ /a\+/ {print "match"}"#, "match")]
#[case("b", r#"$0 ~ /a?b/ {print "match"}"#, "match")]
#[case("a?b", r#"$0 ~ /a\?b/ {print "match"}"#, "match")]
#[tokio::test]
async fn bare_vs_escaped_ere_metas(#[case] input: &str, #[case] program: &str, #[case] expected: &str) {
    let (out, code) = run_awk(program, input).await;
    assert_eq!(out, expected, "program {program:?}, input {input:?}");
    assert_eq!(code, 0, "program {program:?}");
}

/// `gsub`/`match` read the same dialect as `/re/` and `~`.
#[tokio::test]
async fn gsub_reads_escaped_metas_as_literal() {
    let (out, code) = run_awk(r#"{gsub(/fn consult\(/, "X"); print}"#, "fn consult(q)").await;
    assert_eq!(out, "Xq)");
    assert_eq!(code, 0);

    let (out, code) = run_awk(r#"{gsub(/cat\|dog/, "X"); print}"#, "cat|dog").await;
    assert_eq!(out, "X");
    assert_eq!(code, 0);
}

#[tokio::test]
async fn match_function_reads_escaped_metas_as_literal() {
    // match($0, /a\(b\)c/) on "xa(b)cx": the literal "a(b)c" starts at 1-based
    // position 2.
    let (out, code) = run_awk(r#"{print match($0, /a\(b\)c/)}"#, "xa(b)cx").await;
    assert_eq!(out, "2");
    assert_eq!(code, 0);
}

// ─── GNU word/buffer escapes and the \b / \y split ───────────────────────────

#[tokio::test]
async fn backslash_y_is_a_word_boundary() {
    let (out, code) = run_awk(r#"$0 ~ /\ybar/ {print "match"}"#, "foo bar").await;
    assert_eq!(out, "match");
    assert_eq!(code, 0);
}

#[tokio::test]
async fn backslash_word_start_end_anchors() {
    let (out, code) = run_awk(r#"$0 ~ /\<bar\>/ {print "match"}"#, "foo bar").await;
    assert_eq!(out, "match");
    assert_eq!(code, 0);
}

/// gawk's `\b` is a literal backspace character, not a word boundary —
/// `\y` (above) is gawk's spelling for that. Exercised directly against the
/// kernel (a real backspace byte is awkward to carry through a shell word).
#[tokio::test]
async fn backslash_b_is_backspace_not_a_word_boundary() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) = run(
        &kernel,
        r#"awk 'BEGIN{s = "a" sprintf("%c", 8) "b"; if (s ~ /a\bb/) print "match"; else print "no"}'"#,
    )
    .await;
    assert_eq!(out, "match", "\\b matches a real backspace byte, like gawk");
    assert_eq!(code, 0);
}

/// `\d`/`\D` are not gawk regexp operators — the literal letter, unlike the
/// engine's own Perl-style digit class.
#[rstest]
#[case("d", "match")]
#[case("5", "no")]
#[tokio::test]
async fn backslash_d_is_the_literal_letter_not_a_digit_class(
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_awk(r#"$0 ~ /\d/ {print "match"; next} {print "no"}"#, input).await;
    assert_eq!(out, expected, "input {input:?}");
    assert_eq!(code, 0);
}

// ─── POSIX bracket classes are Unicode-aware ──────────────────────────────────
//
// The regex engine's own `[[:alpha:]]` support is ASCII-only; gawk in a
// UTF-8 locale is not, and reads the same glibc class table grep and sed do.
// `gawk` (GNU Awk 5.4.1), `LC_ALL=C.UTF-8`, over
// `héllo日本語٣ 0␠x　y３「line」` (␠ = U+00A0 NBSP): `alpha` also takes the
// Arabic-Indic digit `٣` and the fullwidth `３` (the glibc quirk shared with
// `grep`'s and `sed`'s tables) but not ASCII `0`; `digit` stays ASCII-only;
// `space` takes the ideographic space U+3000 but not the NBSP, which glibc
// classifies `[:punct:]` instead.
#[rstest]
#[case(
    "alpha",
    "X",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}",
    "XXXXXXXXX 0\u{a0}X\u{3000}XX\u{300c}XXXX\u{300d}"
)]
#[case(
    "digit",
    "D",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} D\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}"
)]
#[case(
    "space",
    "_",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663}_0\u{a0}x_y\u{ff13}\u{300c}line\u{300d}"
)]
#[case(
    "punct",
    "P",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0\u{a0}x\u{3000}y\u{ff13}\u{300c}line\u{300d}",
    "h\u{e9}llo\u{65e5}\u{672c}\u{8a9e}\u{663} 0Px\u{3000}y\u{ff13}PlineP"
)]
#[tokio::test]
async fn posix_classes_are_unicode_aware_like_gawk(
    #[case] class: &str,
    #[case] replacement: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let program = format!(r#"{{gsub(/[[:{class}:]]/,"{replacement}"); print}}"#);
    let (out, code) = run_awk(&program, input).await;
    assert_eq!(out, expected, "class {class:?}, input {input:?}");
    assert_eq!(code, 0);
}

// ─── An unrecognized class name always refuses ────────────────────────────────
//
// `awk '$0 ~ /[[:foo:]]/'` used to match — the bracket translator copied an
// unrecognized class name through unchanged, and the engine read
// `[[:foo:]]` as a plain six-character set instead of refusing. Confirmed
// against gawk 5.4.1: `gawk '$0 ~ /[[:foo:]]/'` is a fatal "invalid
// character class name". `[:alpha:]` (no outer brackets around the class)
// stays lenient, matching gawk exactly — real gawk only warns
// ("should probably be `[[:alpha:]]`") and still reads it as a plain
// bracket set, unlike `grep`'s and `sed`'s shared translator, which refuses
// that shape outright (`grep_gnu_bre_tests.rs`).

#[tokio::test]
async fn unrecognized_class_name_refuses() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let result = kernel
        .execute(r#"printf x | awk '$0 ~ /[[:foo:]]/ {print "match"}'"#)
        .await
        .expect("a bad regex is a runtime error, not a validation error");
    assert_ne!(result.code, 0, "unrecognized class name must fail: {result:?}");
    assert!(result.err.contains("foo"), "{result:?}");
}

#[tokio::test]
async fn missing_outer_brackets_stays_lenient_like_gawk() {
    let (out, code) = run_awk(r#"$0 ~ /[:alpha:]/ {print "match"}"#, ":").await;
    assert_eq!(out, "match", "`:` is one of the six literal characters `[:alpha:]` reads as a set");
    assert_eq!(code, 0);
}

// ─── FS / split(): a raw one-char separator is literal, longer is an ERE ─────

#[tokio::test]
async fn single_char_separator_is_always_literal_even_a_meta() {
    // `"("` is exactly one character, so the POSIX single-char-FS rule
    // applies before the separator is ever read as a regex — it splits on
    // the literal `(`, the same as gawk, rather than failing to compile.
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) = run(
        &kernel,
        r#"echo x | awk 'BEGIN{n = split("a(b(c", arr, "("); print n, arr[2]}'"#,
    )
    .await;
    assert_eq!(out, "3 b");
    assert_eq!(code, 0);
}

#[tokio::test]
async fn multichar_fs_with_a_bare_paren_is_a_loud_error() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let result = kernel
        .execute(r#"echo x | awk -F 'xx(' '{print NF}'"#)
        .await
        .expect("invalid FS is a runtime error, not a validation error");
    assert_ne!(result.code, 0, "unbalanced ( in FS must fail: {result:?}");
}

/// `-F`'s CLI value is unescaped the way a program string literal is before
/// FS is compiled: `-F '\|'` becomes the single-character literal `|`.
#[tokio::test]
async fn field_separator_flag_unescapes_before_compiling() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) = run(&kernel, r#"echo 'a|b|c' | awk -F '\|' '{print NF, $2}'"#).await;
    assert_eq!(out, "3 b");
    assert_eq!(code, 0);
}

// ─── `{` with nothing to repeat, and an invalid numeric range, are errors ────

#[rstest]
#[case(r#"$0 ~ /{2}/ {print "match"}"#)] // GNU: not preceded by valid subpattern
#[case(r#"$0 ~ /a{2,1}/ {print "match"}"#)] // GNU: invalid contents of {}
#[tokio::test]
async fn invalid_interval_forms_are_loud_errors(#[case] program: &str) {
    let kernel = kernel_at(tempdir().unwrap().path());
    let result = kernel
        .execute(&format!("printf x | awk '{program}'"))
        .await
        .expect("a bad regex is a runtime error, not a validation error");
    assert_ne!(result.code, 0, "program {program:?} must fail: {result:?}");
}

// ─── A dynamic regex (a string used as `~`'s right side) reads the same way ──

#[tokio::test]
async fn dynamic_regex_from_a_string_literal_reads_the_same_dialect() {
    // `"cat\\|dog"` is the awk string literal `\\` (one backslash) + bare
    // `|`, so the runtime string is the two characters `\|` — a literal
    // pipe once compiled as a regex, same as a `/cat\|dog/` constant.
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) = run(
        &kernel,
        r#"echo x | awk 'BEGIN{p = "cat\\|dog"; if ("cat|dog" ~ p) print "match"; else print "no"}'"#,
    )
    .await;
    assert_eq!(out, "match");
    assert_eq!(code, 0);
}

// ─── Known gaps: gawk behavior the translator does not reach ────────────────

/// gawk applies its own regexp-escape rules *inside* a bracket expression
/// too (a GNU extension over POSIX): `[\d]` is a class containing the
/// literal letter `d`, with the same "not a known regexp operator" warning
/// as a bare `\d`. kaish copies a bracket expression through unchanged, so
/// `\d` inside `[...]` keeps the engine's own Perl-style meaning (a digit
/// class) instead.
#[tokio::test]
#[ignore = "gap: gawk reads regexp escapes inside [...] too; kaish copies bracket interiors through unchanged"]
async fn gap_backslash_d_inside_brackets_is_literal_like_gawk() {
    let (out, code) = run_awk(r#"$0 ~ /[\d]/ {print "match"; next} {print "no"}"#, "d").await;
    assert_eq!(out, "match", "gawk: [\\d] matches the literal letter d");
    assert_eq!(code, 0);
}

/// `\1`-`\9` are octal escapes in real gawk (`\1` is control character
/// `\001`), not digits and not back-references. kaish refuses every
/// backslash-digit instead — a deliberate, narrow gap (no octal escapes, no
/// back-references), matching the same refusal `sed`/`grep` give a BRE
/// back-reference.
#[tokio::test]
#[ignore = "gap: gawk reads \\1-\\9 as octal escapes; kaish's regex engine has no octal escapes"]
async fn gap_backslash_digit_is_an_octal_escape_like_gawk() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let (out, code) = run(
        &kernel,
        r#"echo x | awk 'BEGIN{s = "a" sprintf("%c", 1) "b"; if (s ~ /a\1b/) print "match"; else print "no"}'"#,
    )
    .await;
    assert_eq!(out, "match", "gawk: \\1 is octal 1 (SOH), matching sprintf(\"%c\", 1)");
    assert_eq!(code, 0);
}

// ─── `-v` with no `=` is a loud error, not a silent no-op ────────────────────
//
// `awk -v foo '{print}'` used to silently drop the assignment. Confirmed
// against gawk 5.4.1: `` `foo' argument to `-v' not in `var=value' form ``,
// fatal, exit 1 — kaish refuses too, naming the fix.

#[tokio::test]
async fn dash_v_with_no_equals_sign_is_a_loud_error() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let message = match kernel.execute("printf x | awk -v foo '{print}'").await {
        Err(e) => e.to_string(),
        Ok(result) => {
            assert_ne!(result.code, 0, "must fail: {result:?}");
            result.err.clone()
        }
    };
    assert!(message.contains("-v"), "names the flag: {message}");
    assert!(message.contains("name=value"), "names the fix: {message}");
}

// ─── A dynamic regex from a string reads gawk's own escape rules ────────────
//
// `p = "cat\|dog"; $0 ~ p` used to read `p` as the literal 8-character text
// `cat\|dog` (the backslash survived string-literal scanning), never
// matching `"cat"` alone. Confirmed against gawk 5.4.1: an unrecognized
// string escape drops the backslash at STRING PARSE TIME, before the value
// is ever used as a regex (gawk: "escape sequence `\|' treated as plain
// `|'"), so the runtime string is the two characters `cat|dog`, and used as
// a dynamic regex that's alternation, matching "cat" alone as well as
// "cat|dog" itself (which contains the substring "cat"). `\d` (not a regex
// metacharacter) is a cleaner two-way discriminator: it drops to the bare
// letter `d`, matching text with a literal `d` and nothing else.

#[rstest]
#[case(r#"BEGIN{p="cat\|dog"} $0 ~ p {print "match"}"#, "cat", "match")]
#[case(r#"BEGIN{p="cat\|dog"} $0 ~ p {print "match"}"#, "dog", "match")]
#[case(r#"BEGIN{p="cat\|dog"} $0 ~ p {print "match"}"#, "cat|dog", "match")]
#[case(r#"BEGIN{p="cat\|dog"} $0 ~ p {print "match"}"#, "xyz", "")]
#[case(r#"BEGIN{p="\d"} $0 ~ p {print "match"}"#, "ddd", "match")]
#[case(r#"BEGIN{p="\d"} $0 ~ p {print "match"}"#, "555", "")]
#[tokio::test]
async fn dynamic_regex_string_drops_backslash_on_unrecognized_escape(
    #[case] program: &str,
    #[case] input: &str,
    #[case] expected: &str,
) {
    let (out, code) = run_awk(program, input).await;
    assert_eq!(out, expected, "program {program:?}, input {input:?}");
    assert_eq!(code, 0, "program {program:?}, input {input:?}");
}

/// `p = "a\(b"; $0 ~ p`: `\(` was never a valid STRING escape either, so the
/// backslash drops at string-parse time, leaving the bare, unmatched `(` —
/// a regex compile error, not a literal-paren match. Confirmed against
/// gawk 5.4.1: `fatal: invalid regexp: unbalanced (`.
#[tokio::test]
async fn dynamic_regex_from_dropped_escape_can_be_an_unbalanced_group() {
    let kernel = kernel_at(tempdir().unwrap().path());
    let message = match kernel
        .execute(r#"echo 'a(b' | awk 'BEGIN{p="a\(b"} $0 ~ p {print "match"}'"#)
        .await
    {
        Err(e) => e.to_string(),
        Ok(result) => {
            assert_ne!(result.code, 0, "must fail: {result:?}");
            result.err.clone()
        }
    };
    assert!(message.contains("invalid regex"), "{message}");
}
