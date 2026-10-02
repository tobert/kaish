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

// ── A digit-leading word that is not a number is one string word ───────
//
// `cut -c 9-`, `cut -f 1-3,5-`, and `cut -d, -f 2,4-` are field lists, not
// arithmetic. Numbers, redirects, and `$(( ))` keep their meaning.

#[rstest]
#[case::open_range("9-", "<9->\n")]
#[case::open_range_from_zero("0-", "<0->\n")]
#[case::open_range_leading_zero("007-", "<007->\n")]
#[case::range_then_dash("1-3-", "<1-3->\n")]
#[case::field_list("1-3,5-", "<1-3,5->\n")]
#[case::field_list_short("2,4-", "<2,4->\n")]
#[case::field_list_pair("1,3-", "<1,3->\n")]
#[case::comma_led(",5-", "<,5->\n")]
#[case::float_then_dash("1.5-", "<1.5->\n")]
#[case::date_prefix("2024-01-", "<2024-01->\n")]
#[case::colon_then_range("1:2-", "<1:2->\n")]
#[case::range_then_colon("1-:", "<1-:>\n")]
#[case::minus_led_open("-5-", "<-5->\n")]
#[case::minus_led_range("-1-3", "<-1-3>\n")]
#[case::segment_with_at("1-a@b", "<1-a@b>\n")]
#[case::segment_with_plus("1-a+b", "<1-a+b>\n")]
#[case::segment_with_tilde("2024-01-02~1", "<2024-01-02~1>\n")]
#[case::segment_with_caret("9-^", "<9-^>\n")]
#[tokio::test]
async fn digit_leading_text_is_one_word(#[case] words: &str, #[case] expected: &str) {
    assert_eq!(printf_words(words).await, expected);
}

/// `1--` used to split into `1` and the `--` end-of-options marker, so the
/// command silently lost a word.
#[tokio::test]
async fn digit_then_double_dash_is_one_word() {
    assert_eq!(printf_words("x 1--").await, "<x>\n<1-->\n");
}

#[rstest]
#[case::cut_open_range("printf 'abcdefghijkl\\n' | cut -c 9-", "ijkl\n")]
#[case::cut_field_list("printf 'a\\tb\\tc\\td\\te\\tf\\n' | cut -f 1-3,5-", "a\tb\tc\te\tf\n")]
#[case::cut_delimited_list("printf 'a,b,c,d,e\\n' | cut -d, -f 2,4-", "b,d,e\n")]
#[case::glob_class_trailing_dash("case 7 in [0-9-]) echo digit;; *) echo other;; esac", "digit\n")]
#[tokio::test]
async fn digit_leading_field_lists_reach_the_tool(#[case] source: &str, #[case] expected: &str) {
    let (out, err, code) = run(source).await;
    assert_eq!(code, 0, "{source:?}: {err}");
    assert_eq!(out, expected);
}

/// Numbers, ranges that already worked, redirects, and arithmetic are
/// unchanged.
#[rstest]
#[case::numbers("printf '<%s>\\n' 5 -5 1.5 -1.5", "<5>\n<-5>\n<1.5>\n<-1.5>\n")]
#[case::closed_ranges("printf '<%s>\\n' 1-3 2024-01-02 -1k", "<1-3>\n<2024-01-02>\n<-1k>\n")]
#[case::stderr_merge("printf '<%s>\\n' a 2>&1", "<a>\n")]
#[case::arithmetic("echo $(( 9 - 3 )); (( 9-3 == 6 )) && echo six", "6\nsix\n")]
#[case::assignment_value("x=9-; printf '<%s>\\n' \"$x\"", "<9->\n")]
#[tokio::test]
async fn numbers_redirects_and_arithmetic_are_unchanged(#[case] source: &str, #[case] expected: &str) {
    let (out, err, code) = run(source).await;
    assert_eq!(code, 0, "{source:?}: {err}");
    assert_eq!(out, expected);
}

/// A trailing `.` after digits is the float rule's refusal, not a field
/// list; these stay errors on purpose.
#[rstest]
#[case::trailing_dot("printf '<%s>\\n' 1.")]
#[case::double_dot("printf '<%s>\\n' 1..5")]
#[case::version_trailing_dot("printf '<%s>\\n' 1.2.")]
#[tokio::test]
async fn trailing_dot_numerals_stay_refused(#[case] source: &str) {
    let (_, _, code) = run(source).await;
    assert_ne!(code, 0, "{source:?} must stay refused");
}

// ── A backslash outside quotes makes the next character literal ────────
//
// Every ASCII punctuation character, a space, and three ordinary characters
// after a backslash, alone and inside a word. Each row is bash's output for
// `printf '<%s>\n' WORD`. `\~` alone has its own test below.

#[rstest]
#[case::space(r"\ ", "< >\n")]
#[case::bang(r"\!", "<!>\n")]
#[case::double_quote(r#"\""#, "<\">\n")]
#[case::hash(r"\#", "<#>\n")]
#[case::dollar(r"\$", "<$>\n")]
#[case::percent(r"\%", "<%>\n")]
#[case::ampersand(r"\&", "<&>\n")]
#[case::single_quote(r"\'", "<'>\n")]
#[case::open_paren(r"\(", "<(>\n")]
#[case::close_paren(r"\)", "<)>\n")]
#[case::star(r"\*", "<*>\n")]
#[case::plus(r"\+", "<+>\n")]
#[case::comma(r"\,", "<,>\n")]
#[case::minus(r"\-", "<->\n")]
#[case::dot(r"\.", "<.>\n")]
#[case::slash(r"\/", "</>\n")]
#[case::colon(r"\:", "<:>\n")]
#[case::semicolon(r"\;", "<;>\n")]
#[case::less_than(r"\<", "<<>\n")]
#[case::equals(r"\=", "<=>\n")]
#[case::greater_than(r"\>", "<>>\n")]
#[case::question(r"\?", "<?>\n")]
#[case::at(r"\@", "<@>\n")]
#[case::open_bracket(r"\[", "<[>\n")]
#[case::backslash(r"\\", "<\\>\n")]
#[case::close_bracket(r"\]", "<]>\n")]
#[case::caret(r"\^", "<^>\n")]
#[case::underscore(r"\_", "<_>\n")]
#[case::backtick(r"\`", "<`>\n")]
#[case::open_brace(r"\{", "<{>\n")]
#[case::pipe(r"\|", "<|>\n")]
#[case::close_brace(r"\}", "<}>\n")]
#[case::letter_n(r"\n", "<n>\n")]
#[case::letter_t(r"\t", "<t>\n")]
#[case::digit(r"\0", "<0>\n")]
#[tokio::test]
async fn backslash_punctuation_alone(#[case] word: &str, #[case] expected: &str) {
    assert_eq!(printf_words(word).await, expected);
}

#[rstest]
#[case::space(r"a\ b", "<a b>\n")]
#[case::bang(r"a\!b", "<a!b>\n")]
#[case::double_quote(r#"a\"b"#, "<a\"b>\n")]
#[case::hash(r"a\#b", "<a#b>\n")]
#[case::dollar(r"a\$b", "<a$b>\n")]
#[case::percent(r"a\%b", "<a%b>\n")]
#[case::ampersand(r"a\&b", "<a&b>\n")]
#[case::single_quote(r"a\'b", "<a'b>\n")]
#[case::open_paren(r"a\(b", "<a(b>\n")]
#[case::close_paren(r"a\)b", "<a)b>\n")]
#[case::star(r"a\*b", "<a*b>\n")]
#[case::plus(r"a\+b", "<a+b>\n")]
#[case::comma(r"a\,b", "<a,b>\n")]
#[case::minus(r"a\-b", "<a-b>\n")]
#[case::dot(r"a\.b", "<a.b>\n")]
#[case::slash(r"a\/b", "<a/b>\n")]
#[case::colon(r"a\:b", "<a:b>\n")]
#[case::semicolon(r"a\;b", "<a;b>\n")]
#[case::less_than(r"a\<b", "<a<b>\n")]
#[case::equals(r"a\=b", "<a=b>\n")]
#[case::greater_than(r"a\>b", "<a>b>\n")]
#[case::question(r"a\?b", "<a?b>\n")]
#[case::at(r"a\@b", "<a@b>\n")]
#[case::open_bracket(r"a\[b", "<a[b>\n")]
#[case::backslash(r"a\\b", "<a\\b>\n")]
#[case::close_bracket(r"a\]b", "<a]b>\n")]
#[case::caret(r"a\^b", "<a^b>\n")]
#[case::underscore(r"a\_b", "<a_b>\n")]
#[case::backtick(r"a\`b", "<a`b>\n")]
#[case::open_brace(r"a\{b", "<a{b>\n")]
#[case::pipe(r"a\|b", "<a|b>\n")]
#[case::close_brace(r"a\}b", "<a}b>\n")]
#[case::tilde(r"a\~b", "<a~b>\n")]
#[case::letter_n(r"a\nb", "<anb>\n")]
#[case::letter_t(r"a\tb", "<atb>\n")]
#[case::digit(r"a\0b", "<a0b>\n")]
#[tokio::test]
async fn backslash_punctuation_inside_a_word(#[case] word: &str, #[case] expected: &str) {
    assert_eq!(printf_words(word).await, expected);
}

/// An escaped leading tilde stays literal, like a single-quoted word.
#[tokio::test]
async fn escaped_leading_tilde_stays_literal() {
    let (out, err, code) = run(r"HOME=/home/t; printf '<%s>\n' \~ '~'").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out, "<~>\n<~>\n");
}

/// `find`'s grouping and terminator words reach the command as separate
/// arguments.
#[rstest]
#[case::group(r"\( a \)", "<(>\n<a>\n<)>\n")]
#[case::find_shape(r"\( -name a -o -name b \)", "<(>\n<-name>\n<a>\n<-o>\n<-name>\n<b>\n<)>\n")]
#[case::terminator(r"x \;", "<x>\n<;>\n")]
#[case::negation(r"\! -name a", "<!>\n<-name>\n<a>\n")]
#[tokio::test]
async fn escaped_operators_are_separate_words(#[case] words: &str, #[case] expected: &str) {
    // `--` keeps printf's own flag parser off `-name` and `-o`.
    assert_eq!(printf_words(&format!("-- {words}")).await, expected);
}

/// An escape-bearing word is literal: no expansion, no glob.
#[rstest]
#[case::no_expansion(r"HOME=/h; printf '<%s>\n' \$HOME", "<$HOME>\n")]
#[case::no_glob(r"printf '<%s>\n' \*.txt", "<*.txt>\n")]
#[case::quoted_flag_is_text(r"printf '<%s>\n' \-n", "<-n>\n")]
#[case::comment_after_escaped_space(r"printf '<%s>\n' a\ #b", "<a #b>\n")]
#[case::hash_run_after_escape(r"printf '<%s>\n' \#x \## y", "<#x>\n<##>\n<y>\n")]
#[case::hash_after_escaped_operator(r"printf '<%s>\n' a\;#b", "<a;#b>\n")]
#[case::unicode(r"printf '<%s>\n' 会\ sh", "<会 sh>\n")]
#[case::command(r"ec\ho hit", "hit\n")]
#[case::marker_collision(r"printf '<%s>\n' __KAISH_WORD_0__ a\ b", "<__KAISH_WORD_0__>\n<a b>\n")]
#[case::comment_after_a_space_still_comments(r"printf '<%s>\n' a\  #b", "<a >\n")]
#[tokio::test]
async fn escaped_words_are_literal(#[case] source: &str, #[case] expected: &str) {
    let (out, err, code) = run(source).await;
    assert_eq!(code, 0, "{source:?}: {err}");
    assert_eq!(out, expected);
}

/// The escape joins only the value; flags, `=`, and assignment keys keep
/// their structure.
#[rstest]
#[case::assignment(r"x=a\ b; printf '<%s>\n' $x", "<a b>\n")]
#[case::assignment_leading_escape(r"x=\$y; printf '<%s>\n' $x", "<$y>\n")]
#[case::escaped_key_is_not_an_assignment(r"printf '<%s>\n' a\=b", "<a=b>\n")]
#[case::long_flag_value(r"printf '<%s>\n' -- --opt=a\ b", "<--opt=a b>\n")]
#[case::glued_short_value_space(r"printf 'a b\n' | cut -d\  -f2", "b\n")]
#[case::glued_short_value_semicolon(r"printf 'a;b\n' | awk -F\; '{print $2}'", "b\n")]
#[case::tilde_path(r"HOME=/home/t; printf '<%s>\n' ~/a\ b", "</home/t/a b>\n")]
#[case::list_literal(r"x=[a\ b c]; printf '<%s>\n' ${x[0]} ${x[1]}", "<a b>\n<c>\n")]
#[case::record_literal(r"r={k: a\ b}; printf '<%s>\n' ${r[k]}", "<a b>\n")]
#[case::case_pattern(r"case 'a b' in a\ b) echo hit;; *) echo miss;; esac", "hit\n")]
#[case::case_escaped_star_is_literal(r"case x in \*) echo star;; *) echo other;; esac", "other\n")]
#[case::case_escaped_star_matches_a_star(r"case '*' in \*) echo star;; *) echo other;; esac", "star\n")]
#[case::case_quoted_star_is_literal(r"case x in '*') echo star;; *) echo other;; esac", "other\n")]
#[case::case_quoted_braces(r"case '{a,b}' in '{a,b}') echo literal;; *) echo other;; esac", "literal\n")]
#[case::case_quoted_braces_do_not_expand(r"case a in '{a,b}') echo wrong;; *) echo literal;; esac", "literal\n")]
#[case::test_compare(r"[[ 'a b' == a\ b ]] && echo same", "same\n")]
#[case::heredoc_quoted_delimiter("HOME=/h; cat <<\\EOF\n$HOME\nEOF\n", "$HOME\n")]
#[tokio::test]
async fn escapes_keep_the_surrounding_structure(#[case] source: &str, #[case] expected: &str) {
    let (out, err, code) = run(source).await;
    assert_eq!(code, 0, "{source:?}: {err}");
    assert_eq!(out, expected);
}

/// In a `[[ =~ ]]` operand an escaped character matches itself, as in bash:
/// `\.` is a literal dot, and `\<` is `<`, not a word boundary.
#[rstest]
#[case::escaped_dot_matches_dot("[[ a.b =~ ^a\\.b ]]", 0)]
#[case::escaped_dot_is_not_any_char("[[ axb =~ ^a\\.b ]]", 1)]
#[case::escaped_less_than("[[ 'a<b' =~ a\\<b ]]", 0)]
#[case::escaped_space("[[ 'a b' =~ a\\ b ]]", 0)]
#[case::unescaped_dot_is_any_char("[[ axb =~ ^a.b ]]", 0)]
#[case::escaped_d_is_a_letter("[[ d =~ \\d ]]", 0)]
#[case::escaped_d_is_not_a_digit("[[ '5' =~ \\d ]]", 1)]
#[tokio::test]
async fn escapes_in_a_regex_operand_match_literally(#[case] source: &str, #[case] expected: i64) {
    let (_, err, code) = run(source).await;
    assert_eq!(code, expected, "{source:?}: {err}");
}

/// A word with an escape is literal, so an unescaped glob character in it
/// is refused rather than silently matched or silently made literal.
#[rstest]
#[case::star(r"printf '<%s>\n' a\ *.txt", r"a\ *.txt", "'a *.txt'")]
#[case::question(r"printf '<%s>\n' a\ ?", r"a\ ?", "'a ?'")]
#[case::bracket(r"printf '<%s>\n' a\ [bc]", r"a\ [bc]", "'a [bc]'")]
#[tokio::test]
async fn escape_with_an_unescaped_glob_is_refused(
    #[case] source: &str,
    #[case] word: &str,
    #[case] fix: &str,
) {
    let (_, err, code) = run(source).await;
    assert_ne!(code, 0, "{source:?} must be refused");
    assert!(err.contains(word), "{source:?} must name the word: {err}");
    assert!(err.contains(fix), "{source:?} must name the quoted fix: {err}");
}

#[rstest]
#[case::regex(r"[[ 'a b' =~ a\ .* ]]; echo )")]
#[case::case_pattern(r"case 'a b' in a\ *) echo hit;; esac; echo )")]
#[tokio::test]
async fn a_valid_escaped_pattern_does_not_steal_a_later_error(#[case] source: &str) {
    let (_, error, code) = run(source).await;
    assert_ne!(code, 0);
    assert!(!error.contains("quote the whole word"), "{error}");
}

#[rstest]
#[case::variable(r"longname=prefix; echo $longname\ suffix")]
#[case::substitution(r"echo $(echo prefix)\ suffix")]
#[tokio::test]
async fn escaped_suffixes_do_not_paste_an_expansion(#[case] source: &str) {
    let (_, error, code) = run(source).await;
    assert_ne!(code, 0);
    assert!(error.contains("quote"), "{error}");
}

#[rstest]
#[case::space(r"case 'a b' in 'a b') echo hit;; *) echo miss;; esac")]
#[case::star(r"case '*' in '*') echo hit;; *) echo miss;; esac")]
#[case::braces(r"case '{a,b}' in '{a,b}') echo hit;; *) echo miss;; esac")]
#[case::operator(r"case 'a|b' in 'a|b') echo hit;; *) echo miss;; esac")]
#[tokio::test]
async fn case_pattern_quoting_survives_plan_rendering(#[case] source: &str) {
    let plans = kaish_kernel::plan_program(source).expect("plan");
    let (output, error, code) = run(&plans[0].plan.rendered).await;
    assert_eq!(code, 0, "{error}");
    assert_eq!(output, "hit\n", "{}", plans[0].plan.rendered);
}

#[rstest]
#[case::trailing("echo hidden; echo tail\\", "backslash at the end of a word")]
#[case::combined_flag(r"echo hidden; awk -Ffoo\ bar '{print NF}' f", "write the flag and its quoted value separately")]
#[tokio::test]
async fn invalid_escape_words_refuse_before_execution(#[case] source: &str, #[case] hint: &str) {
    let (output, error, code) = run(source).await;
    assert_ne!(code, 0);
    assert!(output.is_empty(), "{output}");
    assert!(error.contains(hint), "{error}");
}

#[cfg(feature = "localfs")]
mod escaped_globs_on_disk {
    use kaish_kernel::{Kernel, KernelConfig};

    fn kernel_at(dir: &std::path::Path) -> Kernel {
        Kernel::new(KernelConfig::repl().with_cwd(dir.to_path_buf()).with_trash(false))
            .expect("kernel")
    }

    fn tree() -> tempfile::TempDir {
        let dir = tempfile::tempdir().expect("tempdir");
        std::fs::write(dir.path().join("a.txt"), "needle\n").expect("write a.txt");
        std::fs::write(dir.path().join("b.md"), "needle\n").expect("write b.md");
        dir
    }

    /// `find . -name \*.txt` hands `find` the pattern, not the shell's glob.
    #[tokio::test]
    async fn find_receives_the_escaped_pattern() {
        let dir = tree();
        let result = kernel_at(dir.path()).execute(r"find . -name \*.txt").await.expect("execute");
        assert_eq!(result.code, 0, "{}", result.err);
        assert_eq!(result.text_out().trim(), "./a.txt");
    }

    /// `--include=\*.txt` stays grep's flag with the value `*.txt`.
    #[tokio::test]
    async fn grep_include_keeps_its_flag() {
        let dir = tree();
        let result =
            kernel_at(dir.path()).execute(r"grep -r --include=\*.txt needle .").await.expect("execute");
        assert_eq!(result.code, 0, "{}", result.err);
        let out = result.text_out();
        assert!(out.contains("a.txt"), "{out}");
        assert!(!out.contains("b.md"), "{out}");
    }

    /// An escaped `*` never globs, even with matching files present.
    #[tokio::test]
    async fn escaped_star_does_not_glob() {
        let dir = tree();
        let result = kernel_at(dir.path()).execute(r"printf '<%s>\n' \*").await.expect("execute");
        assert_eq!(result.code, 0, "{}", result.err);
        assert_eq!(result.text_out(), "<*>\n");
    }
}
