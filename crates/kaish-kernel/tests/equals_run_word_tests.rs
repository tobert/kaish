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
//! The fuse also covers a numeral piece (`Int`/`Float`/`Bool`/
//! `NumericLiteral`) glued into the same run, taking each piece's SOURCE
//! TEXT rather than its formatted value — `echo ===1.50===` prints
//! `===1.50===`, not `===1.5===`. `echo ===007` and `echo ===2024-01-01===`
//! already worked before this widening: a leading-zero numeral and a
//! hyphenated date both lex as plain `Value::String` text already, never a
//! typed `Int`/`Float`.
//!
//! Kaish still refuses a run built from anything else glued to a bare `=` —
//! a quoted string, a substitution, or a run with a single bare `=` and no
//! `==`/`!=` marker anywhere in it — because fusing those would hide a real
//! value boundary or turn a botched assignment into silent text.
//! `glued_arg_span_tests.rs` covers those and the pinned `./bin=1`-style
//! cases that must keep erroring.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig, KernelError};

async fn run(source: &str) -> String {
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let result = kernel.execute(source).await.expect("should succeed");
    result.text_out().to_string()
}

/// The number of args `plan` reports for one command's argv — a count the
/// printed text of `run()` cannot give: `echo === step 3 ===` prints the
/// same text whether it ran as four args or one. `plan` renders the
/// statement unexpanded and never executes it (`plan_builtin_tests.rs`).
async fn arg_count(command_source: &str) -> usize {
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let plan_source = format!("plan {command_source:?} --json");
    let result = kernel
        .execute(&plan_source)
        .await
        .unwrap_or_else(|e| panic!("plan should parse {command_source:?}: {e}"));
    assert!(result.ok(), "plan should succeed for {command_source:?}: {}", result.err);
    let doc: serde_json::Value = serde_json::from_str(&result.text_out())
        .unwrap_or_else(|e| panic!("plan --json must emit JSON: {e}: {}", result.text_out()));
    doc["statements"][0]["plan"]["commands"][0]["args"]
        .as_array()
        .unwrap_or_else(|| panic!("no commands[0].args in plan for {command_source:?}: {doc}"))
        .len()
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
    // Each `===` fuses on its own; printed text alone cannot tell four args
    // ("=== step 3 ===") from one ("===_step_3_===" joined by a builtin
    // that reprints its argv with spaces) — assert the argv shape `plan`
    // reports instead: "===", "step", "3", "===".
    assert_eq!(run("echo === step 3 ===").await, "=== step 3 ===\n");
    assert_eq!(arg_count("echo === step 3 ===").await, 4);
}

#[tokio::test]
async fn multibyte_word_with_equals_run_prints_literally() {
    assert_eq!(run("echo ===é").await, "===é\n");
}

// --- Numeral pieces: source text, not formatted value ---

#[tokio::test]
async fn equals_run_glued_to_a_trailing_int_prints_literally() {
    assert_eq!(run("echo ==5").await, "==5\n");
    assert_eq!(arg_count("echo ==5").await, 1);
}

#[tokio::test]
async fn equals_run_glued_to_a_leading_int_prints_literally() {
    assert_eq!(run("echo ===1").await, "===1\n");
    assert_eq!(arg_count("echo ===1").await, 1);
}

#[tokio::test]
async fn equals_run_glued_on_both_sides_of_an_int_prints_literally() {
    assert_eq!(run("echo ===2024===").await, "===2024===\n");
    assert_eq!(arg_count("echo ===2024===").await, 1);
}

#[tokio::test]
async fn equals_run_glued_to_a_non_canonical_float_keeps_its_source_spelling() {
    // `1.50`'s own `Display` would print `1.5` — the fuse must take the
    // SOURCE SLICE, never the formatted value, or this silently loses the
    // trailing zero.
    assert_eq!(run("echo ===1.50===").await, "===1.50===\n");
    assert_eq!(arg_count("echo ===1.50===").await, 1);
}

#[tokio::test]
async fn equals_run_glued_to_a_bool_prints_literally() {
    assert_eq!(run("echo ===true===").await, "===true===\n");
    assert_eq!(arg_count("echo ===true===").await, 1);
}

#[tokio::test]
async fn leading_zero_numeral_already_worked_before_the_widening() {
    // `007` is text (leading zero), a `Value::String` already accepted
    // before Int/Float/Bool/NumericLiteral joined the fuse — pinned so a
    // regression here reads as a regression, not new ground.
    assert_eq!(run("echo ===007").await, "===007\n");
}

#[tokio::test]
async fn hyphenated_date_already_worked_before_the_widening() {
    assert_eq!(run("echo ===2024-01-01===").await, "===2024-01-01===\n");
}

// --- export: the fused word still splits on the first `=` ---

#[tokio::test]
async fn export_with_double_equals_and_a_word_value() {
    // Already worked before the widening (`y` is a plain String literal) —
    // pinned alongside `export X==1` below so the two keep matching bash's
    // "only the first `=` separates name from value" rule the same way.
    assert_eq!(run("export X==y; echo $X").await, "=y\n");
}

#[tokio::test]
async fn export_with_double_equals_and_an_int_value() {
    assert_eq!(run("export X==1; echo $X").await, "=1\n");
}

// --- Boundaries the fuse still refuses ---

#[tokio::test]
async fn flag_glued_to_an_equals_run_still_fuses_after_double_dash() {
    // Past `--`, `--flag` is a plain positional string like any other —
    // already worked before the widening (no numeral piece involved).
    assert_eq!(run("echo -- --flag==x").await, "--flag==x\n");
}

#[tokio::test]
async fn not_equal_glued_to_a_trailing_word_prints_literally() {
    // Already worked before the widening (`x` is a plain String literal).
    assert_eq!(run("echo !=x").await, "!=x\n");
}

#[tokio::test]
async fn bang_glue_keeps_refusing() {
    // `!x` is a separate, still-open decision (`Bang` glued to a bareword,
    // no `==`/`!=` marker involved) — untouched by this fix.
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let err = kernel
        .execute("echo !x")
        .await
        .expect_err("echo !x must keep failing to parse");
    assert!(
        err.to_string().contains("adjacent words with no space between them"),
        "echo !x must keep refusing with the glued-args message: {err}"
    );
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
async fn double_bracket_unspaced_equality_is_unaffected() {
    // `[[ $X==1 ]]` — no space around `==` inside `[[ ]]` — never goes
    // through argv parsing either, so it is unaffected the same way.
    assert_eq!(
        run("X=1; [[ $X==1 ]] && echo yes || echo no").await,
        "yes\n"
    );
}

#[tokio::test]
async fn assignment_is_unaffected() {
    assert_eq!(run("x=1; echo $x").await, "1\n");
}

#[tokio::test]
async fn spaced_assignment_is_unaffected() {
    assert_eq!(run("x = 1; echo $x").await, "1\n");
}

#[tokio::test]
async fn indexed_assignment_is_unaffected() {
    assert_eq!(run("x = [1 2 3]; x[0] = 9; echo ${x[0]}").await, "9\n");
}

// --- x==1 / x==y at statement start: same shape, same message ---

#[tokio::test]
async fn bareword_glued_to_double_equals_and_an_int_reports_the_same_as_a_word() {
    // Before this fix, `x==1` (typed `Int`) errored with the glued-args
    // "quote the whole word" message while `x==y` (plain `String`) already
    // fused and reached command-name validation instead — two different
    // diagnoses for the same shape. Both now fuse `==1`/`==y` into one arg
    // and both report the parser's "command name and first argument need a
    // space between them" — a message that names a fix which actually
    // parses: `x == 1` runs (and fails at runtime with "command not found:
    // x", same as any other undefined command, not a parse error).
    fn parse_error_message(err: KernelError) -> String {
        let KernelError::Parse { errors, .. } = err else {
            panic!("expected a parse error, got {err:?}");
        };
        assert_eq!(errors.len(), 1, "expected exactly one error: {errors:?}");
        errors[0].message.clone()
    }

    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let int_message = parse_error_message(
        kernel.execute("x==1").await.expect_err("x==1 must still fail to parse"),
    );
    let word_message = parse_error_message(
        kernel.execute("x==y").await.expect_err("x==y must still fail to parse"),
    );
    assert!(
        int_message.contains("command name and first argument need a space"),
        "x==1: {int_message}"
    );
    assert_eq!(
        int_message, word_message,
        "x==1 and x==y must report the identical message"
    );

    // The fix the message names actually parses (it just names a command
    // that does not exist, which is a runtime error, not a parse error).
    assert_eq!(run("x == 1").await, "");
}
