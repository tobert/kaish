//! Tilde expansion applies only to an UNQUOTED tilde-prefix written in the
//! source — bash's rule. A quoted `'~'`/`"~"`, a variable's value, and a
//! command substitution's output must never expand, no matter what they
//! contain: `rm -r '~/x'` must target a directory literally named `~`, never
//! the real home directory.
//!
//! On main this was broken: `apply_tilde_expansion`/`value_to_string_with_tilde`
//! expanded any evaluated `Value::String` starting with `~`, after quoting was
//! already lost. The fix moves the decision to the parser: only the lexer's
//! own `Tilde`/`TildePath` tokens (which the lexer emits solely for an
//! unquoted source word) become `Expr::TildePath`, and only that AST node
//! expands — never a `Value` after the fact.
//!
//! Every expected value here was captured from real `bash` with a known
//! `HOME` (`HOME=/tmp/tildetest/h bash -c '...'`), not guessed.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::HashMap;

use kaish_kernel::ast::Value;
use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

const HOME: &str = "/home/fixture";

fn kernel() -> Kernel {
    let mut vars = HashMap::new();
    vars.insert("HOME".to_string(), Value::String(HOME.into()));
    Kernel::new(KernelConfig::isolated().with_initial_vars(vars)).expect("kernel")
}

fn hermetic_kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated().with_initial_vars(HashMap::new())).expect("kernel")
}

// --- Core table: quoted never expands, unquoted always does -----------------
//
// bash reference (HOME=/home/fixture bash -c '<source minus the trailing echo
// wiring>'):
//   echo '~'            -> ~
//   echo "~"             -> ~
//   x='~'; echo "$x"     -> ~
//   echo '~/a'           -> ~/a
//   echo ~               -> /home/fixture
//   echo ~/a             -> /home/fixture/a
//   x=~/a; echo $x       -> /home/fixture/a
//   export X=~/a; echo $X -> /home/fixture/a
#[rstest]
// Quoted: literal, never expanded — the bug this suite pins closed.
#[case("echo '~'", "~\n")]
#[case("echo \"~\"", "~\n")]
#[case("x='~'; echo \"$x\"", "~\n")]
#[case("x='~'; echo $x", "~\n")]
#[case("echo '~/a'", "~/a\n")]
#[case("echo \"~/a\"", "~/a\n")]
// A variable's VALUE never re-expands even when it equals a bare `~`,
// because the assignment RHS itself was quoted.
#[case("x='~/a'; y=$x; echo $y", "~/a\n")]
// A command substitution's OUTPUT is a computed string, never a source
// word: nothing inside this `$(...)` is a bare `~` (the inner `echo`'s
// argument was itself quoted), so nothing expands anywhere.
#[case("x='~'; echo \"$(echo $x)\"", "~\n")]
// A bare `~` INSIDE the substitution's own source still expands there —
// this is not "substitutions never expand", it is "the output is inert".
#[case("echo \"$(echo ~)\"", "/home/fixture\n")]
// Unquoted: expands against the session HOME.
#[case("echo ~", "/home/fixture\n")]
#[case("echo ~/a", "/home/fixture/a\n")]
#[case("echo ~/a/b", "/home/fixture/a/b\n")]
// Assignment value: unquoted `~` expands (bash: `x=~/a; echo $x`).
#[case("x=~/a; echo $x", "/home/fixture/a\n")]
#[case("x=~; echo $x", "/home/fixture\n")]
#[case("export X=~/a; echo $X", "/home/fixture/a\n")]
#[case("local x=~/a; echo $x", "/home/fixture/a\n")]
#[tokio::test]
async fn tilde_expansion_matches_bash(#[case] source: &str, #[case] expected: &str) {
    let kernel = kernel();
    let result = kernel.execute(source).await.unwrap();
    assert!(result.ok(), "{source}: {result:?}");
    assert_eq!(result.text_out(), expected, "source: {source}");
}

// --- grep argument: a bare `~` pattern is a literal glyph, not a path -------
//
// bash: `echo 'tilde~home' | grep '~'` -> `tilde~home` (matches; `~` has no
// regex meaning). Kaish's `grep` positional goes through the same evaluator
// as every other builtin argument, so this is also a differential for the
// argv-binding fix, not just `echo`.
#[tokio::test]
async fn quoted_tilde_is_a_literal_grep_pattern_not_a_path() {
    let kernel = kernel();
    let result = kernel.execute("echo 'tilde~home' | grep '~'").await.unwrap();
    assert!(result.ok(), "{result:?}");
    assert_eq!(result.text_out(), "tilde~home\n");
}

// A bare (unquoted) `~` pattern DOES expand under kaish's argument evaluator
// (it is a source word like any other), so it becomes the session HOME and
// matches nothing in this input — proving the two spellings genuinely
// disagree, not that neither expands.
#[tokio::test]
async fn bare_tilde_grep_pattern_expands_and_therefore_matches_nothing() {
    let kernel = kernel();
    let result = kernel.execute("echo 'tilde~home' | grep ~").await.unwrap();
    // grep with no match exits 1; this is the point of the test.
    assert_eq!(result.code, 1, "{result:?}");
    assert_eq!(result.text_out(), "");
}

// --- cd -----------------------------------------------------------------

#[tokio::test]
async fn cd_bare_tilde_goes_home() {
    let kernel = kernel();
    let result = kernel.execute("mkdir -p /home/fixture; cd ~; pwd").await.unwrap();
    assert!(result.ok(), "{result:?}");
    assert_eq!(result.text_out().trim(), HOME);
}

#[tokio::test]
async fn cd_quoted_tilde_is_a_literal_directory_name() {
    let kernel = kernel();
    let result = kernel.execute("cd '~'").await.unwrap();
    // bash: `cd: ~: No such file or directory` — there is no directory
    // literally named `~` in a fresh session. The hazard this whole fix
    // closes: `cd '~'`/`rm -r '~/x'` must never land on the real home dir.
    assert!(!result.ok(), "{result:?}");
    assert!(
        result.err.contains('~') && !result.err.contains(HOME),
        "quoted ~ must stay literal, not resolve to {HOME}: {result:?}"
    );
}

// --- [[ -f ]] / test -f ---------------------------------------------------

#[tokio::test]
async fn double_bracket_file_test_expands_bare_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute("mkdir -p /home/fixture; touch /home/fixture/x; [[ -f ~/x ]] && echo yes || echo no")
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "yes", "{out:?}");
}

#[tokio::test]
async fn double_bracket_file_test_does_not_expand_quoted_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute("mkdir -p /home/fixture; touch /home/fixture/x; [[ -f '~/x' ]] && echo yes || echo no")
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "no", "{out:?}");
}

#[tokio::test]
async fn test_builtin_expands_bare_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute("mkdir -p /home/fixture; touch /home/fixture/x; test -f ~/x && echo yes || echo no")
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "yes", "{out:?}");
}

#[tokio::test]
async fn test_builtin_does_not_expand_quoted_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute("mkdir -p /home/fixture; touch /home/fixture/x; test -f '~/x' && echo yes || echo no")
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "no", "{out:?}");
}

// --- for f in ~/a ~/b ------------------------------------------------------

#[tokio::test]
async fn for_loop_items_expand_bare_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute("for f in ~/a ~/b; do echo $f; done")
        .await
        .unwrap();
    assert_eq!(out.text_out(), "/home/fixture/a\n/home/fixture/b\n");
}

#[tokio::test]
async fn for_loop_items_do_not_expand_quoted_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute("for f in '~/a' '~/b'; do echo $f; done")
        .await
        .unwrap();
    assert_eq!(out.text_out(), "~/a\n~/b\n");
}

// --- case ~ in --------------------------------------------------------------
//
// bash: `case ~ in "$HOME") echo matched;; esac` matches, and quoting the
// subject (`case '~' in ...`) does not.

#[tokio::test]
async fn case_subject_expands_bare_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute(&format!("case ~ in {HOME}) echo matched;; *) echo no;; esac"))
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "matched", "{out:?}");
}

#[tokio::test]
async fn case_subject_does_not_expand_quoted_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute(&format!("case '~' in {HOME}) echo matched;; *) echo no;; esac"))
        .await
        .unwrap();
    assert_eq!(out.text_out().trim(), "no", "{out:?}");
}

// --- list literal element ----------------------------------------------------

#[tokio::test]
async fn list_literal_element_bare_tilde_expands_quoted_does_not() {
    let kernel = kernel();
    let out = kernel
        .execute("p = [~ '~']; echo ${p[0]} ${p[1]}")
        .await
        .unwrap();
    assert_eq!(out.text_out(), format!("{HOME} ~\n"));
}

// --- heredoc body: bash does NOT expand ~ in a heredoc body -----------------
//
// bash: `cat <<EOF` / `~` / `EOF` prints a literal `~`, even though the
// heredoc body is otherwise interpolated (an unquoted delimiter still
// expands `$VAR`/`$(...)`). Tilde expansion is a WORD-splitting-time
// operation on source words; a heredoc body is never split into words.

#[tokio::test]
async fn heredoc_body_never_expands_tilde() {
    let kernel = kernel();
    let out = kernel.execute("cat <<EOF\n~\nEOF").await.unwrap();
    assert_eq!(out.text_out(), "~\n", "{out:?}");
}

#[tokio::test]
async fn heredoc_body_never_expands_tilde_even_with_interpolation() {
    let kernel = kernel();
    let out = kernel.execute("x=hi; cat <<EOF\n~/$x\nEOF").await.unwrap();
    assert_eq!(out.text_out(), "~/hi\n", "{out:?}");
}

// --- redirect targets ---------------------------------------------------

#[tokio::test]
async fn redirect_target_expands_bare_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute("mkdir -p /home/fixture; echo hi > ~/f; cat /home/fixture/f")
        .await
        .unwrap();
    assert!(out.ok(), "{out:?}");
    assert_eq!(out.text_out(), "hi\n");
}

#[tokio::test]
async fn redirect_target_does_not_expand_quoted_tilde() {
    let kernel = kernel();
    // Writing to a literal `~f` path (no directory named `~` exists, so a
    // path *under* it would fail to create — write directly to `~f` at the
    // session cwd to isolate the quoting question from directory creation).
    // Read back with a quoted target too, so this doesn't also depend on
    // `~f`'s bare-word `~user` lookup (gated behind the `host` feature).
    let out = kernel.execute("echo hi > '~f'; cat '~f'").await.unwrap();
    assert!(out.ok(), "{out:?}");
    assert_eq!(out.text_out(), "hi\n");
}

// --- execute_argv: tokens are literal, like a quoted word -------------------
//
// See `Kernel::execute_argv`'s doc comment: an argv token carries no
// quoting, so it is treated the same as a quoted source word — never
// tilde-expanded. This is a deliberate behavior change from the pre-fix
// kernel, which expanded a leading `~` in argv "for consistency with the
// string door" — that carve-out was itself downstream of the same bug this
// suite closes, and is gone now.
#[tokio::test]
async fn execute_argv_tokens_do_not_expand_tilde() {
    let kernel = kernel();
    let out = kernel
        .execute_argv("echo", &[Value::String("~/a".into())])
        .await
        .unwrap();
    assert!(out.ok(), "{out:?}");
    assert_eq!(out.text_out(), "~/a\n");
}

// --- hermetic: no HOME in scope, bare ~ stays literal, never the host env ---

#[tokio::test]
async fn hermetic_kernel_leaves_bare_tilde_unexpanded() {
    let kernel = hermetic_kernel();
    let out = kernel.execute("echo ~").await.unwrap();
    assert_eq!(out.text_out().trim(), "~", "~ must not leak the host HOME");
    let out = kernel.execute("echo ~/a").await.unwrap();
    assert_eq!(out.text_out().trim(), "~/a");
}

// --- `~` not at the start of a word: kaish refuses the ambiguous paste -----
//
// bash concatenates `foo~bar` into one literal word (no expansion, since the
// tilde-prefix must start the word). kaish's lexer has no bareword+tilde
// pasting rule at all — `foo~bar` is two adjacent tokens with no space
// between them, which kaish's own "no token pasting" grammar refuses as a
// parse error rather than silently gluing them (see
// `keyword_literal_words_tests.rs` for the general rule). Either way, the
// bug this suite is about — over-expansion — cannot happen here: there is
// no path from `foo~bar`/`a/~` to `Expr::TildePath`.
#[test]
fn tilde_not_at_word_start_is_never_a_tilde_expansion() {
    use kaish_kernel::parser::parse;
    assert!(parse("echo foo~bar").is_err(), "adjacent unquoted words must not paste into one arg");
    assert!(parse("echo a/~").is_err(), "adjacent unquoted words must not paste into one arg");
    // Quoting sidesteps the pasting question entirely and is never expanded.
    parse("echo \"foo~bar\"").unwrap();
}

// --- known, pre-existing gap: kaish never supported the `x=a:~/b` form -----
//
// bash expands `~` after an unquoted `:` inside an assignment value
// (`x=a:~/b` -> `a:/h/b`, verified with `HOME=/h bash -c 'x=a:~/b; echo $x'`).
// kaish's lexer fuses any colon-adjacent run of tilde/word tokens into one
// plain `Ident` *before* the parser ever sees a `TildePath` token (see
// `lexer::merge_colon_adjacent`), so a tilde following a colon has never
// reached tilde expansion in kaish, in this source's tilde-first case
// (`~/a:b`) or the colon-first case here. This test pins the current,
// unsupported behavior (literal, unexpanded) so a future change to add the
// `:`-form is a deliberate decision, not an accidental regression pinned by
// nothing.
#[tokio::test]
async fn colon_adjacent_tilde_in_assignment_value_is_unsupported() {
    let kernel = kernel();
    let out = kernel.execute("x=a:~/b; echo $x").await.unwrap();
    assert!(out.ok(), "{out:?}");
    assert_eq!(out.text_out().trim(), "a:~/b", "kaish does not expand ~ after a colon (unlike bash)");
}
