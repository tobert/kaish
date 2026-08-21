//! `test` refuses `-a`, `-o`, `(` and `)` — and now refuses them *before the
//! statement runs*.
//!
//! The operators were already rejected at runtime with a message naming
//! `&&`/`||`. What made that worthless is where `test` lives: an `if` reads
//! only the exit code, so exit 2 chose the `else` branch and the message went
//! nowhere. That is the bug that was reported as "`test -o` silently returns
//! false rather than OR-ing".
//!
//! Two changes fix it from both ends. A condition's stderr now reaches the
//! author (see `condition_output_tests`), so a runtime refusal is audible at
//! all; and this validator rule stops the statement before anything executes,
//! which is the stronger promise kaish already makes for the rest of the
//! language.
//!
//! **Implementing the operators was tried and rejected.** A 984-expression
//! differential sweep against `bash -c` matched bash exactly, and that is the
//! argument against shipping it: bash overloads `-a`/`-o` (a unary `-a FILE`
//! synonym for `-e`, a unary `-o NAME` option query) which is what makes the
//! binary form ambiguous to parse, and three of its operand-count rules
//! outrank `!` in ways a careful reader gets wrong — `test ! = x` compares two
//! strings, `test ! -a ""` is an AND, `test ! x -o x` negates the whole
//! expression. coreutils' own man page points at `&&`/`||` instead. kaish
//! agrees and says so.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

/// Rejected before execution: the kernel refuses the whole statement, so the
/// error is an `Err`, not an exit code a condition can quietly consume.
#[rstest]
#[case("test a = a -o b = c")]
#[case("test a = a -a b = b")]
#[case("test -f a -a -f b")]
#[case("test '(' a = a ')'")]
#[case("test -z \"\" -o -z x")]
#[tokio::test]
async fn compound_operators_are_refused_before_running(#[case] script: &str) {
    let err = kernel()
        .execute(script)
        .await
        .expect_err("the statement must not run")
        .to_string();
    assert!(err.contains("E020"), "`{script}` should carry the code: {err}");
    assert!(
        err.contains("&&") && err.contains("||"),
        "`{script}` should name the fix: {err}"
    );
}

/// The point of doing it in the validator: inside a condition, a runtime exit
/// 2 picks a branch. A validation error stops the statement, so no branch is
/// taken at all and the author cannot miss it.
#[tokio::test]
async fn a_compound_in_a_condition_stops_the_statement() {
    // Distinctive markers: the first draft asserted on "yes"/"no" and the
    // error text contains "not supported", so the "no" check passed on the
    // wrong substring.
    let err = kernel()
        .execute("if test a = a -o b = c; then echo BRANCH_THEN; else echo BRANCH_ELSE; fi")
        .await
        .expect_err("must not choose a branch")
        .to_string();
    assert!(err.contains("E020"), "{err}");
    assert!(
        !err.contains("BRANCH_THEN") && !err.contains("BRANCH_ELSE"),
        "no branch ran: {err}"
    );
}

/// The validation binder hands a `raw_argv` tool the same words execution
/// does, in source order.
///
/// It did not before: it split by token shape, so `-a`/`-o` landed in `flags`
/// and `(`/`)` in `positional`, and the ORDER — the only thing separating an
/// operator from a literal — was gone. `test "-a" = "-a"` and
/// `test a = a -a b = b` decomposed identically, so no `Tool::validate` could
/// tell them apart, and the first version of this rule refused both.
///
/// The verbatim arm exists for exactly this reason; raw_argv never got one.
#[tokio::test]
async fn validation_binds_raw_argv_in_source_order() {
    use kaish_kernel::tools::{register_builtins, ToolRegistry};

    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);
    let tool = registry.get("test").expect("test builtin");

    // The operator slot: caught.
    let refused = kernel().execute("test a = a -o b = c").await;
    assert!(refused.is_err(), "an operator in the operator slot is refused");

    // The same word one slot over: not caught, because it is data.
    let allowed = kernel()
        .execute("test \"-o\" = \"-o\"")
        .await
        .expect("an operator word as an operand must run");
    assert_eq!(allowed.code, 0);

    // And directly: a decomposed `ToolArgs` with the words out of order can
    // no longer occur, so `validate` reads `positional` alone.
    let mut flags_only = kaish_kernel::tools::ToolArgs::new();
    flags_only.flags.insert("o".to_string());
    assert!(
        tool.validate(&flags_only).is_empty(),
        "nothing routes `-o` into `flags` any more; reading it there would \
         resurrect the position-blind check"
    );
}

/// Everything `test` does support is untouched.
#[rstest]
#[case("test a = a", 0)]
#[case("test a = b", 1)]
#[case("test ! a = a", 1)]
#[case("test ! a = b", 0)]
#[case("test ! ! x", 0)]
#[case("test -n x", 0)]
#[case("test -z \"\"", 0)]
#[case("test = = =", 0)]
#[case("test -n = -n", 0)]
#[tokio::test]
async fn supported_forms_are_unchanged(#[case] script: &str, #[case] expected: i64) {
    let r = kernel().execute(script).await.expect("kernel execute");
    assert_eq!(r.code, expected, "`{script}`");
}

/// Two conformance fixes kept from the attempt at implementing the operators,
/// both independent of them and both verified against `bash -c`.
#[rstest]
// The empty path names no file. It resolved to the working directory, so
// every file operator answered TRUE for it.
#[case("test -e \"\"", 1)]
#[case("test -f \"\"", 1)]
#[case("test -d \"\"", 1)]
// No operands is false, as in bash, rather than a usage error — an absent
// expression cannot be a typo'd one.
#[case("test", 1)]
#[tokio::test]
async fn conformance_fixes_kept(#[case] script: &str, #[case] expected: i64) {
    let r = kernel().execute(script).await.expect("kernel execute");
    assert_eq!(r.code, expected, "`{script}`");
}

/// `[[ ]]` answers the empty path the same way, because `file_test` says it
/// "mirrors `[[`'s `FileTest` arm so the two stay consistent" and a fix that
/// lands in one arm makes that comment false. The two spellings of a file
/// test must never disagree about the same path.
#[rstest]
#[case("[[ -e \"\" ]]", 1)]
#[case("[[ -f \"\" ]]", 1)]
#[case("[[ -d \"\" ]]", 1)]
#[case("[[ -r \"\" ]]", 1)]
#[case("[[ -w \"\" ]]", 1)]
#[case("[[ -x \"\" ]]", 1)]
#[tokio::test]
async fn double_bracket_answers_the_empty_path_like_test(
    #[case] script: &str,
    #[case] expected: i64,
) {
    let r = kernel().execute(script).await.expect("kernel execute");
    assert_eq!(r.code, expected, "`{script}`");
}

/// The same path through both spellings, to catch a future fix landing in one
/// arm again. A non-empty path that exists must still be true on both sides.
#[tokio::test]
async fn both_spellings_agree_on_a_real_path() {
    let k = kernel();
    let r = k.execute("[[ -d \"/\" ]]").await.expect("kernel execute");
    assert_eq!(r.code, 0, "`[[ -d / ]]`");
    let r = k.execute("test -d \"/\"").await.expect("kernel execute");
    assert_eq!(r.code, 0, "`test -d /`");
}

/// A lone operator stays loud, which is the divergence from bash that this
/// whole area exists to defend: bash reads `test -f` as the non-empty string
/// `"-f"` and returns TRUE, turning a forgotten operand into a passing
/// condition.
///
/// A compound operator is no exception, and E020 must not claim it. One
/// operand has no operator slot for `-a` to sit in, so the accurate report is
/// the runtime's "needs an operand" — not "chain with `&&`/`||`", which names
/// a compound the author did not write. E020's first version answered these
/// because its own comment argued the runtime message would be swallowed by an
/// `if`; #385 made that false.
#[rstest]
#[case("test -f")]
#[case("test -z")]
#[case("test !")]
#[case("test \"-a\"")]
#[case("test -a")]
#[case("test \"-o\"")]
#[case("test \"(\"")]
#[case("test \")\"")]
#[tokio::test]
async fn a_lone_operator_is_loud_not_a_string(#[case] script: &str) {
    let r = kernel().execute(script).await.expect("kernel execute");
    assert_eq!(r.code, 2, "`{script}` should be a usage error, not bash's true");
    assert!(r.err.contains("operand"), "should name the problem: {}", r.err);
}

// --- `set -o` reports option state, which is where that question belongs ----

/// `test -o NAME` is deliberately absent (it is one of bash's overloads of
/// `-o`, and the reason the binary form is ambiguous). The question it would
/// have answered — "is this shell option on?" — belongs to `set`, which could
/// not answer it either: bare `set` prints only what differs from the default,
/// so an option AT its default was indistinguishable from an unknown one.
#[tokio::test]
async fn set_dash_o_reports_every_option() {
    let k = kernel();
    let out = k.execute("set -o").await.expect("kernel execute").text_out().into_owned();
    for name in ["glob", "output-limit", "trash"] {
        assert!(out.contains(name), "`set -o` should list {name}: {out:?}");
    }
    assert!(out.contains("on") && out.contains("off"), "states missing: {out:?}");
}

#[tokio::test]
async fn set_dash_o_reflects_a_change() {
    let k = kernel();
    let before = k.execute("set -o").await.expect("exec").text_out().into_owned();
    assert!(before.contains("trash\toff"), "{before:?}");

    let after = k
        .execute("set -o trash; set -o")
        .await
        .expect("exec")
        .text_out()
        .into_owned();
    assert!(after.contains("trash\ton"), "{after:?}");
}

/// It is a table, so `--json` gives an embedder the same answer as structured
/// data rather than text to parse.
#[tokio::test]
async fn set_dash_o_is_structured() {
    let k = kernel();
    let out = k.execute("set -o --json").await.expect("exec").text_out().into_owned();
    let rows: serde_json::Value = serde_json::from_str(&out).expect("parses as JSON");
    let rows = rows.as_array().expect("array");
    // Grew to 4 when pipefail became a real option.
    assert_eq!(rows.len(), 4, "{rows:?}");
    assert_eq!(rows[0]["OPTION"], "glob");
    assert!(
        rows.iter().any(|r| r["OPTION"] == "pipefail"),
        "pipefail must be reportable: {rows:?}"
    );
}

/// Setting an option still works and still rejects an unknown name — the
/// report path must not have swallowed the apply path.
#[tokio::test]
async fn set_dash_o_with_a_name_still_applies() {
    let k = kernel();
    assert_eq!(k.execute("set -o trash").await.expect("exec").code, 0);
    assert_eq!(k.execute("set -o bogus").await.expect("exec").code, 1);
}

// --- an operator word in OPERAND position is data, not an operator ---------

/// E020 must fire on the operator slot only. A file named `-a`, or the string
/// `-a` compared against itself, is an ordinary `test` — bash answers it and
/// so must kaish.
///
/// The first version of this rule scanned every word regardless of position
/// and refused all of these. It could not do better: for a `raw_argv` tool the
/// validation binder had no twin, so it split the words by token shape and the
/// operand ORDER — the only thing that distinguishes an operator from a
/// literal — was gone before `validate` ever ran.
#[rstest]
#[case("test -f \"-a\"", 1)]
#[case("test \"-a\" = \"-a\"", 0)]
#[case("test \"-o\" = \"-o\"", 0)]
#[case("test \"(\" = \"(\"", 0)]
#[case("test \"-a\" != \"-o\"", 0)]
#[case("test -n \"-o\"", 0)]
#[tokio::test]
async fn an_operator_word_in_operand_position_is_data(
    #[case] script: &str,
    #[case] expected: i64,
) {
    let r = kernel()
        .execute(script)
        .await
        .unwrap_or_else(|e| panic!("`{script}` must not be refused: {e}"));
    assert_eq!(r.code, expected, "`{script}`");
}

// --- a non-string operand must not shift the operator slots ---------------

/// `validate` rebuilt its word list from `positional` and dropped every
/// non-`String` value. Runtime keeps operands typed, so an `Int` operand
/// shifted the slots by one and the two disagreed again — the same class the
/// source-order fix closed, in a narrower form.
///
/// `test "-a" = 1` is a string compared against a number: bash exits 1 and
/// kaish refused it outright.
#[rstest]
#[case("test \"-a\" = 1", 1)]
#[case("test \"-o\" = 1", 1)]
#[case("test 1 = \"-a\"", 1)]
#[case("test \"(\" = 1", 1)]
#[tokio::test]
async fn a_numeric_operand_does_not_shift_the_slots(#[case] script: &str, #[case] expected: i64) {
    let r = kernel()
        .execute(script)
        .await
        .unwrap_or_else(|e| panic!("`{script}` must not be refused: {e}"));
    assert_eq!(r.code, expected, "`{script}`");
}

/// The same shift in the other direction: a compound operator IS in the
/// operator slot, but a dropped numeric operand hid it from the validator, so
/// the statement ran and failed at runtime instead of being stopped.
#[rstest]
#[case("test -a 1")]
#[case("test -o 1")]
#[case("test 1 -a 2")]
// `test -a` alone is NOT here: one operand has no operator slot, so it is a
// lone operator, not a compound — see `a_lone_operator_is_loud_not_a_string`.
#[tokio::test]
async fn a_compound_operator_is_caught_even_beside_a_number(#[case] script: &str) {
    let err = kernel()
        .execute(script)
        .await
        .expect_err("must be refused before running")
        .to_string();
    assert!(err.contains("E020"), "`{script}`: {err}");
}
