//! A glued `!` in ARGUMENT position fuses into one literal word instead of
//! erroring — the same decision `equals_run_word_tests.rs` already
//! documents for a glued `==`/`!=` run, extended to `!` (Amy's call): bash
//! reads `!x`, `a!b`, `!!` glued in argument position as ordinary text (a
//! non-interactive script does no history expansion), never as negation,
//! so a plain-literal run carrying a bare `!` now proves the lexer split
//! one word instead of the caller pasting two, exactly like `==`/`!=`
//! already did. `fuse_plain_operator_run` (`parser.rs`) is the one fuse
//! function for all three markers.
//!
//! This is argument position ONLY. A glued `!` at statement position
//! (`!true`), condition position (`if !true`), or inside `[[ ]]`
//! (`[[ !-f x ]]`) is `bang_prefixed`'s guard — a completely different
//! parser path that consumes `!` before argv parsing ever runs — and keeps
//! refusing exactly as before (`parser_tests.rs`,
//! `glued_bang_is_refused`). `! cmd &` (E022, a background-negation
//! semantic check, not a parse-time glue check) is unaffected the same
//! way.
//!
//! A `!` glued to a substitution, a variable, or a quoted string also
//! keeps refusing: `fuse_plain_operator_run` only fuses a run whose every
//! member is a plain literal or numeral piece (`plain_literal_source_text`)
//! — `VarRef`, `CommandSubst`, and a quoted string (its source carries
//! quote marks the value lacks) are never such a piece, so a run touching
//! any of them still hits the pre-existing "adjacent words" refusal. A `!`
//! glued to a GLOB is a different story: the lexer's own glob-run scanner
//! folds a leading `!` straight into the glob token itself
//! (`tokenize("!*.rs")` is one `GlobWord`, never `Bang` + `GlobWord`), so
//! it never reaches `reject_glued_args` as a multi-piece run at all —
//! see `bang_glued_to_a_glob_was_never_a_glue_case` below.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

async fn run(source: &str) -> String {
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let result = kernel.execute(source).await.expect("should succeed");
    result.text_out().to_string()
}

/// The number of args `plan` reports for one command's argv — see
/// `equals_run_word_tests.rs::arg_count` for why printed text alone can't
/// tell a one-arg fuse from several.
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

// --- Argument position: a glued `!` fuses into one literal word, one arg ---

#[rstest]
#[case::bang_prefix("echo !x", "!x\n")]
#[case::bang_suffix("echo x!", "x!\n")]
#[case::bang_infix("echo a!b", "a!b\n")]
#[case::bang_run("echo !!", "!!\n")]
#[case::bang_sandwiched_twice("echo !x!y", "!x!y\n")]
#[case::bang_glued_to_an_int("echo !1", "!1\n")]
#[case::bang_glued_to_a_negative_int("echo !-1", "!-1\n")]
#[tokio::test]
async fn glued_bang_argument_fuses_and_prints_literally(#[case] source: &str, #[case] expected: &str) {
    assert_eq!(run(source).await, expected);
    assert_eq!(arg_count(source).await, 1, "must be a single fused arg for {source:?}");
}

#[tokio::test]
async fn bang_run_is_not_echo_specific() {
    // The fusion happens in argv parsing, not an `echo` special case —
    // matches `equals_run_word_tests.rs::equals_run_is_not_echo_specific`.
    // Two args to `printf`: the format string, then the fused `!x`.
    assert_eq!(run("printf '%s\\n' !x").await, "!x\n");
    assert_eq!(arg_count("printf '%s\\n' !x").await, 2);
}

#[tokio::test]
async fn single_bare_bang_already_worked() {
    // Already worked (a single token, no adjacent fragment to glue to) —
    // pinned the same way the `=`/`==` singles are in
    // `equals_run_word_tests.rs`.
    assert_eq!(run("echo !").await, "!\n");
    assert_eq!(arg_count("echo !").await, 1);
}

#[tokio::test]
async fn spaced_bang_mid_argv_stays_its_own_word() {
    assert_eq!(run("echo a ! b").await, "a ! b\n");
    assert_eq!(arg_count("echo a ! b").await, 3);
}

#[tokio::test]
async fn bang_run_across_a_line_continuation_fuses_like_bash() {
    let source = "echo !\\\nx";
    assert_eq!(run(source).await, "!x\n");
    assert_eq!(arg_count(source).await, 1);
}

#[tokio::test]
async fn multiple_spaced_bang_runs_are_separate_args() {
    // Each glued run fuses on its own — assert the argv shape, not just
    // the printed text, the same way
    // `equals_run_word_tests.rs::multiple_spaced_equals_runs_are_separate_args`
    // does.
    assert_eq!(run("echo !x step 3 !y").await, "!x step 3 !y\n");
    assert_eq!(arg_count("echo !x step 3 !y").await, 4);
}

#[tokio::test]
async fn bang_after_double_dash_still_fuses() {
    // Past `--`, a glued `!` run fuses the same way it does before `--` —
    // matches `equals_run_word_tests.rs::flag_glued_to_an_equals_run_still_fuses_after_double_dash`.
    // `--` itself is not part of the run (a real space separates it from
    // `!x`); `echo`'s own clap-based flag parsing consumes the `--` marker
    // rather than printing it (same pre-existing behavior
    // `equals_run_word_tests.rs::flag_glued_to_an_equals_run_still_fuses_after_double_dash`
    // pins for `--flag==x`), so only the fused `!x` shows in the output.
    assert_eq!(run("echo -- !x").await, "!x\n");
    assert_eq!(arg_count("echo -- !x").await, 2, "DoubleDash + the fused !x");
}

#[tokio::test]
async fn bang_glued_to_a_long_flag_before_double_dash_still_refused() {
    // Pre-`--`, `--flag` parses as `Arg::LongFlag`, not a plain literal
    // piece `plain_literal_source_text` can hand back source text for —
    // the same pre-existing limit `==` already has (see
    // `equals_run_word_tests.rs`'s `-- --flag==x` case, which only fuses
    // AFTER `--`, where a flag is an ordinary positional string). Not a
    // regression: `--flag!x` never fused before this change either.
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let err = kernel.execute("echo --flag!x").await.expect_err("must still be refused");
    assert!(
        err.to_string().contains("adjacent words with no space between them"),
        "echo --flag!x must keep the glued-args message: {err}"
    );
}

// --- Boundaries the fuse still refuses: substitution, quoted ---

#[rstest]
#[case::variable("echo !$x")]
#[case::command_subst("echo !$(true)")]
#[case::quoted_string(r#"echo !"x""#)]
fn glued_bang_to_a_non_plain_piece_still_refused(#[case] source: &str) {
    // Not `run()`/async: only the parse result matters here, matching the
    // style of `glued_arg_span_tests.rs`'s sync `parse()`-based checks.
    let errors =
        kaish_kernel::parser::parse(source).expect_err("must still be a parse error");
    assert!(
        errors[0].message.contains("adjacent words with no space between them"),
        "{source:?} must keep the glued-args message: {errors:?}"
    );
}

#[tokio::test]
async fn bang_glued_to_a_glob_was_never_a_glue_case() {
    // `!*.rs` is not a glued RUN at all: the lexer's own glob-run scanner
    // (`flush_glob_run`, the same one that already folds a bare comma into
    // a bareword) folds a leading `!` straight into the glob token itself —
    // `tokenize("!*.rs")` is one `GlobWord("!*.rs")`, never `Bang` +
    // `GlobWord("*.rs")`. So this never reaches `reject_glued_args` as a
    // multi-piece run, before or after this fix; it parses (and fails at
    // RUNTIME with "no matches", the same as any other non-matching glob),
    // never at parse time. Recorded here so the boundary this fix does NOT
    // touch is pinned by an actual assertion, not just the doc comment
    // above.
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let result = kernel.execute("echo !*.rs").await.expect("must parse and execute");
    assert!(!result.ok(), "no file matches the literal pattern \"!*.rs\": {result:?}");
    assert!(
        result.err.contains("no matches"),
        "expected a runtime glob-miss, not a parse error: {result:?}"
    );
}

// --- Statement/condition position: unaffected, `bang_prefixed`'s own guard ---

#[rstest]
#[case::statement_start("!true")]
#[case::if_condition("if !true; then echo yes; fi")]
#[case::while_condition("while !cmd; do :; done")]
#[case::double_bracket_file_test("[[ !-f x ]]")]
#[case::double_bracket_var("[[ !$x == y ]]")]
fn glued_bang_at_statement_or_condition_position_still_refused(#[case] source: &str) {
    let errors =
        kaish_kernel::parser::parse(source).expect_err("must still be a parse error");
    assert!(
        errors[0].message.contains("needs a space"),
        "{source:?} must keep `bang_prefixed`'s own message, not the argv-glue one: {errors:?}"
    );
}

#[tokio::test]
async fn negated_background_pipeline_is_still_a_separate_semantic_refusal() {
    // `! cmd &` is E022 (a validator check on `Stmt::Not` over a background
    // pipeline), not a parse-time glue check — this argument-position fuse
    // has no bearing on it. `kernel_error_tests.rs` covers E022 in full;
    // this just pins that the statement still fails, unaffected.
    let kernel = Kernel::new(KernelConfig::transient()).expect("kernel");
    let err = kernel.execute("! true &").await.expect_err("must still be refused");
    let kaish_kernel::KernelError::Validation { issues, .. } = err else {
        panic!("`! true &` must still be KernelError::Validation");
    };
    assert!(
        issues.iter().any(|i| i.code == kaish_kernel::validator::IssueCode::NegatedBackgroundPipeline),
        "expected NegatedBackgroundPipeline (E022): {issues:?}"
    );
}
