//! A `[[ ]]` type error is a result, not an escape — in statement position.
//!
//! `[[ x -eq 1 ]]` where `x` holds `abc` is a refusal kaish makes on purpose:
//! bash coerces `abc` to 0 and reads the test as false, kaish never answers a
//! number it was not given. What was wrong is the SHAPE of that refusal.
//!
//! A standalone `[[ ]]` shares its position with `(( ))` (`Stmt::Arith`) and
//! with the `test` builtin, and both already report a type error as exit 2
//! with the message on stderr. `Stmt::Test` alone let the error escape
//! `Kernel::execute` as `Err`, which cost two things: the exit code collapsed
//! to 1, making a type error indistinguishable from a false condition, and an
//! embedder had to handle one condition in two control-flow shapes.
//!
//! CONDITION position is deliberately NOT changed. `if`/`while` conditions
//! evaluate through `Expr::Test`, never through this statement arm, and a
//! condition has no exit-code channel separate from true/false — so a fault
//! there aborts rather than silently reading false. `condition_position_*`
//! below pins that, so the decision cannot be reversed by accident.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::kernel_at;
use tempfile::tempdir;

/// Run `script` and return `(exit code, stderr)`. Panics if the kernel
/// returned `Err`, which is itself the thing under test.
async fn code_and_err(script: &str) -> (i64, String) {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute(script)
        .await
        .expect("kernel returned Err where a result was expected");
    (result.code, result.err)
}

// --- statement position: a type error is exit 2, like `test` and `(( ))` ----

#[tokio::test]
async fn standalone_bracket_type_error_is_code_2() {
    let (code, _) = code_and_err(r#"x=abc; [[ "$x" -eq 1 ]]"#).await;
    assert_eq!(code, 2, "a non-numeric operand is a type error, not `false`");
}

/// The discriminator: without it, exit 2 could be read as "any nonzero".
/// A genuinely false comparison must stay 1, or the error is still
/// indistinguishable from a false reading.
#[tokio::test]
async fn standalone_bracket_false_is_still_code_1() {
    let (code, _) = code_and_err(r#"x=2; [[ "$x" -eq 1 ]]"#).await;
    assert_eq!(code, 1, "a false comparison is 1, never the type-error 2");
}

#[tokio::test]
async fn standalone_bracket_true_is_still_code_0() {
    let (code, _) = code_and_err(r#"x=1; [[ "$x" -eq 1 ]]"#).await;
    assert_eq!(code, 0);
}

/// The refusal has to name the value, or it teaches nothing.
#[tokio::test]
async fn standalone_bracket_type_error_names_the_value() {
    let (_, err) = code_and_err(r#"x=abc; [[ "$x" -eq 1 ]]"#).await;
    assert!(
        err.contains("abc"),
        "the error must name the offending value, got: {err:?}"
    );
    assert!(
        err.contains("numeric"),
        "the error must name the rule, got: {err:?}"
    );
}

/// All six numeric comparators share `numeric_compare`, so all six must
/// share the shape. One of them behaving differently is the defect class.
#[tokio::test]
async fn all_six_numeric_comparators_report_code_2() {
    for op in ["-eq", "-ne", "-lt", "-le", "-gt", "-ge"] {
        let (code, err) = code_and_err(&format!(r#"x=abc; [[ "$x" {op} 1 ]]"#)).await;
        assert_eq!(code, 2, "`{op}` should report a type error as 2");
        assert!(err.contains("abc"), "`{op}` should name the value");
    }
}

/// `test` is the sibling this shape is matched to; pinning it here means a
/// later change to either one cannot silently split them apart again.
#[tokio::test]
async fn test_builtin_agrees_with_bracket_on_all_three_outcomes() {
    for (script_bracket, script_test) in [
        (r#"x=1; [[ "$x" -eq 1 ]]"#, r#"x=1; test "$x" -eq 1"#),
        (r#"x=2; [[ "$x" -eq 1 ]]"#, r#"x=2; test "$x" -eq 1"#),
        (r#"x=abc; [[ "$x" -eq 1 ]]"#, r#"x=abc; test "$x" -eq 1"#),
    ] {
        let (bracket, _) = code_and_err(script_bracket).await;
        let (builtin, _) = code_and_err(script_test).await;
        assert_eq!(
            bracket, builtin,
            "`{script_bracket}` and `{script_test}` must agree"
        );
    }
}

// --- condition position: unchanged, and pinned so it stays that way --------

/// A fault in a CONDITION aborts the enclosing statement. It must not become
/// a silent `false` that runs the `else` branch — that is the silent fallback
/// kaish refuses. This test fails if someone extends the statement-position
/// fix into condition position.
#[tokio::test]
async fn condition_position_aborts_rather_than_reading_false() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let outcome = kernel
        .execute(r#"x=abc; if [[ "$x" -eq 1 ]]; then echo T; else echo F; fi"#)
        .await;
    assert!(
        outcome.is_err(),
        "a bad operand in a condition must abort, not select `else`"
    );
}

/// The same rule for `(( ))`, the other condition form, so the two stay
/// aligned with each other as well as with their statement forms.
#[tokio::test]
async fn arith_condition_position_also_aborts() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    let outcome = kernel
        .execute(r#"x=abc; if (( x )); then echo T; else echo F; fi"#)
        .await;
    assert!(outcome.is_err(), "`(( ))` conditions abort the same way");
}
