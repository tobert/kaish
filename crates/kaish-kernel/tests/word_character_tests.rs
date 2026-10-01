//! Ordinary bash words that kaish's lexer used to refuse.
//!
//! Each refusal costs an agent a full turn, so a word bash reads as plain
//! text is plain text here too. Every row asserts the effect: the bytes
//! `printf '<%s>\n'` prints (one line per argument, so a split or a lost
//! word shows), or the exit code of a test that depends on the word.
//!
//! The places where a character keeps a meaning have rows of their own, so a
//! widened word class cannot swallow them silently.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

/// Run `source` in a fresh transient kernel: `(stdout, stderr, exit code)`.
/// A parse or validation refusal comes back as exit code `-1` with the
/// error text in the stderr slot.
async fn run(source: &str) -> (String, String, i64) {
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    match kernel.execute(source).await {
        Ok(result) => (result.text_out().into_owned(), result.err.clone(), result.code),
        Err(error) => (String::new(), format!("{error:#}"), -1),
    }
}

/// The lines `printf '<%s>\n' WORDS` prints for `words`.
async fn printf_words(words: &str) -> String {
    let source = format!("printf '<%s>\\n' {words}");
    let (out, err, code) = run(&source).await;
    assert_eq!(code, 0, "{source:?} failed: {err}");
    out
}

// ── `^` is an ordinary word character ──────────────────────────────────

#[rstest]
#[case::alone("^", "<^>\n")]
#[case::infix("a^b", "<a^b>\n")]
#[case::suffix("x^", "<x^>\n")]
#[case::git_parent("HEAD^", "<HEAD^>\n")]
#[case::git_grandparent("HEAD^^", "<HEAD^^>\n")]
#[case::git_second_parent("HEAD^2", "<HEAD^2>\n")]
#[case::git_range("master^ master", "<master^>\n<master>\n")]
#[case::leading("^foo", "<^foo>\n")]
#[case::digit_leading("1^2", "<1^2>\n")]
#[case::absolute_path("/tmp/a^b", "</tmp/a^b>\n")]
#[case::relative_path("a^/b", "<a^/b>\n")]
#[case::dotted(".a^b", "<.a^b>\n")]
#[case::at_word("@a^b", "<@a^b>\n")]
#[case::git_path_spec("HEAD^:src/main.rs", "<HEAD^:src/main.rs>\n")]
#[tokio::test]
async fn caret_is_a_word_character(#[case] words: &str, #[case] expected: &str) {
    assert_eq!(printf_words(words).await, expected);
}

/// `$(( ))` and `(( ))` read `^` as XOR; the arithmetic text never reaches
/// the word classes.
#[tokio::test]
async fn caret_is_still_xor_in_arithmetic() {
    let (out, err, code) = run("echo $(( 6 ^ 3 )); (( (6 ^ 3) == 5 )) && echo xor").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out, "5\nxor\n");
}

/// `${x^}` and `${x^^}` are bash case operators kaish does not have; the
/// whole `${…}` is one token, so the name check still refuses them.
#[rstest]
#[case::upper_first("echo ${x^}")]
#[case::upper_all("echo ${x^^}")]
#[tokio::test]
async fn caret_case_operator_is_still_refused(#[case] source: &str) {
    let (_, err, code) = run(source).await;
    assert_ne!(code, 0, "{source:?} must still be refused");
    assert!(err.contains("variable name contains `^`"), "{source:?}: {err}");
}

/// `[^a]` negates a glob bracket expression.
#[rstest]
#[case::negated_class_matches("case b in [^a]) echo neg;; *) echo other;; esac", "neg\n")]
#[case::negated_class_rejects("case a in [^a]) echo neg;; *) echo other;; esac", "other\n")]
#[tokio::test]
async fn caret_negates_a_glob_bracket(#[case] source: &str, #[case] expected: &str) {
    let (out, err, code) = run(source).await;
    assert_eq!(code, 0, "{source:?}: {err}");
    assert_eq!(out, expected);
}

/// An unquoted `^` anchor in a `[[ =~ ]]` operand reaches the regex.
#[rstest]
#[case::anchored_match("[[ abc =~ ^ab ]]", 0)]
#[case::anchored_miss("[[ xabc =~ ^ab ]]", 1)]
#[tokio::test]
async fn caret_anchors_an_unquoted_regex(#[case] source: &str, #[case] expected: i64) {
    let (_, err, code) = run(source).await;
    assert_eq!(code, expected, "{source:?}: {err}");
}

// ── `~` that does not start a word is an ordinary character ────────────
//
// Tilde expansion keeps its two places: a word that starts with `~`, and a
// `~` right after an assignment's `=`. Every other `~` is part of the word.

#[rstest]
#[case::infix("a~b", "<a~b>\n")]
#[case::suffix("x~", "<x~>\n")]
#[case::git_ancestor("HEAD~1", "<HEAD~1>\n")]
#[case::git_ancestor_bare("HEAD~", "<HEAD~>\n")]
#[case::git_ancestor_then_parent("HEAD~1^2", "<HEAD~1^2>\n")]
#[case::git_range("HEAD~3..HEAD", "<HEAD~3..HEAD>\n")]
#[case::digit_leading("1~2", "<1~2>\n")]
#[case::backup_file("f.txt~", "<f.txt~>\n")]
#[case::absolute_path("/tmp/f~", "</tmp/f~>\n")]
#[case::relative_path("a/b~c", "<a/b~c>\n")]
#[case::slash_after_tilde("a~/b", "<a~/b>\n")]
#[case::dot_slash("./a~b", "<./a~b>\n")]
#[case::dotted(".a~b", "<.a~b>\n")]
#[case::at_word("@a~b", "<@a~b>\n")]
#[case::colon_then_tilde("a:~/b", "<a:~/b>\n")]
#[case::quoted("\"HEAD~1\"", "<HEAD~1>\n")]
#[tokio::test]
async fn tilde_inside_a_word_is_a_character(#[case] words: &str, #[case] expected: &str) {
    assert_eq!(printf_words(words).await, expected);
}

/// A long flag's value keeps its `~`: past `--` the pair is one operand.
#[tokio::test]
async fn tilde_inside_a_flag_value_is_a_character() {
    assert_eq!(printf_words("-- --from=HEAD~1").await, "<--from=HEAD~1>\n");
}

/// A word that starts with `~` still expands, and so does a `~` after an
/// assignment's `=`.
#[rstest]
#[case::bare_tilde("HOME=/home/t; printf '<%s>\\n' ~", "</home/t>\n")]
#[case::tilde_path("HOME=/home/t; printf '<%s>\\n' ~/x", "</home/t/x>\n")]
#[case::tilde_path_with_inner_tilde("HOME=/home/t; printf '<%s>\\n' ~/a~b", "</home/t/a~b>\n")]
#[case::assignment("HOME=/home/t; p=~/x; printf '<%s>\\n' \"$p\"", "</home/t/x>\n")]
#[tokio::test]
async fn tilde_at_a_word_start_still_expands(#[case] source: &str, #[case] expected: &str) {
    let (out, err, code) = run(source).await;
    assert_eq!(code, 0, "{source:?}: {err}");
    assert_eq!(out, expected);
}
