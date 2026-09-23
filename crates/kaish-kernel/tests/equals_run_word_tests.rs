//! A bareword built only from `=` characters, or an ordinary bareword with
//! `=`/`==`/`!=` glued into it, is one word the lexer split into pieces —
//! not two words the caller pasted together.
//!
//! `=`, `==`, and `!=` stay their own tokens even with no space around them
//! (unlike comma or colon, which the lexer folds into the surrounding
//! bareword — see `bareword_comma_tests.rs`), because a lone `=` can open a
//! `NAME=value` assignment and has to reach the parser able to say so. But
//! `==`/`!=` can never be part of a real assignment (that needs exactly one
//! bare `=`), so a run of `=`-family tokens and plain barewords with no
//! other operator involved — `===`, `==x`, `a==b` — is unambiguous: it
//! fuses back into one literal word instead of erroring. Telemetry showed
//! 112 of 112 unquoted `echo ===`/`echo ====SEPARATOR====` calls (models
//! using `=` runs as section separators) failing on the old "no token
//! pasting" rejection before this fix.
//!
//! Kaish still refuses a run built from anything else glued to a bare `=` —
//! a quoted string, a substitution, or a typed non-string value — because
//! fusing those would hide a real value boundary or silently coerce a typed
//! value to text. `glued_arg_span_tests.rs` covers those and the pinned
//! `x==1`/`./bin=1`-style cases that must keep erroring.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

async fn run(source: &str) -> String {
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let result = kernel.execute(source).await.expect("should succeed");
    result.text_out().to_string()
}

#[tokio::test]
async fn bare_equals_run_prints_literally() {
    assert_eq!(run("echo ===").await, "===\n");
}

#[tokio::test]
async fn longer_equals_run_used_as_separator_prints_literally() {
    // The exact shape models reach for as a section separator.
    assert_eq!(run("echo ====SEPARATOR====").await, "====SEPARATOR====\n");
}

#[tokio::test]
async fn equals_run_glued_to_a_trailing_word_prints_literally() {
    assert_eq!(run("echo ==x").await, "==x\n");
}

#[tokio::test]
async fn equals_run_glued_between_two_words_prints_literally() {
    assert_eq!(run("echo a==b").await, "a==b\n");
}

#[tokio::test]
async fn single_bare_equals_keeps_working() {
    // Already worked (a single token, no adjacent fragment to glue to) —
    // pinned so a regression here is caught the same way as the others.
    assert_eq!(run("echo =").await, "=\n");
}

#[tokio::test]
async fn double_bare_equals_keeps_working() {
    assert_eq!(run("echo ==").await, "==\n");
}

#[tokio::test]
async fn not_equal_run_glued_between_two_words_prints_literally() {
    // `!=` is `test_operator_arg_parser`'s other operator token; the same
    // fusion rule applies to it as to `==`.
    assert_eq!(run("echo a!=b").await, "a!=b\n");
}

#[tokio::test]
async fn equals_run_is_not_echo_specific() {
    // The fusion happens in argv parsing, not an `echo` special case — any
    // command's argument is affected the same way (matches bash: no
    // command name is special-cased for word splitting).
    assert_eq!(run("printf '%s\\n' ===").await, "===\n");
}

#[tokio::test]
async fn multiple_spaced_equals_runs_are_separate_args() {
    // Each `===` fuses on its own; the spaces between them still separate
    // three distinct positional arguments.
    assert_eq!(run("echo === step 3 ===").await, "=== step 3 ===\n");
}

#[tokio::test]
async fn multibyte_word_with_equals_run_prints_literally() {
    assert_eq!(run("echo ===é").await, "===é\n");
}

// --- Regression pins: constructs that must keep their current meaning ---

#[tokio::test]
async fn double_bracket_equality_is_unaffected() {
    // `[[ ]]` has its own comparison grammar, never argv parsing, so this
    // was never affected by the bug and must not be affected by the fix.
    assert_eq!(run("[[ a == b ]] && echo yes || echo no").await, "no\n");
}

#[tokio::test]
async fn double_bracket_inequality_is_unaffected() {
    assert_eq!(run("[[ a != b ]] && echo yes || echo no").await, "yes\n");
}

#[tokio::test]
async fn assignment_is_unaffected() {
    assert_eq!(run("x=1; echo $x").await, "1\n");
}
