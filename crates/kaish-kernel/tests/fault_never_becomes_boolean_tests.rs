//! A fault is never converted to a boolean.
//!
//! `[[ ]]`, `test`, and `(( ))` all refuse a non-numeric operand. The refusal
//! is not in question; what these tests pin is what happens to it next.
//!
//! Where something CONSUMES the result as a boolean — an `if`/`while`
//! condition, a `!`, or the left operand of `&&`/`||` — there is no true or
//! false to hand it, so the statement aborts. Answering `false` there is a
//! silent coercion: `[[ $x -eq 1 ]] || echo "not one"` would print a
//! conclusion drawn from a comparison that never happened.
//!
//! Where NOTHING consumes it as a boolean — a standalone statement, or the
//! right operand of a chain, whose value simply becomes the chain's value —
//! the fault is reported as exit 2 with its message and execution continues.
//! The exit code is the report there, and 2 is distinguishable from a false
//! comparison's 1.
//!
//! All three constructs must agree in every position. Before this rule they
//! did not, and `[[ ]]` did not even agree with itself: it aborted inside an
//! `if` and read as false inside a `||`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::Kernel;

/// The three spellings of the same broken comparison. `x` holds `abc`
/// throughout, so each is a type fault, not a false reading.
const FAULTS: [(&str, &str); 3] = [
    ("[[ ]]", r#"[[ "$x" -eq 1 ]]"#),
    ("test", r#"test "$x" -eq 1"#),
    ("(( ))", r#"(( x == 1 ))"#),
];

/// The same three spellings with an operand that is a real number, so they
/// genuinely evaluate. Used as controls: every abort test needs proof that
/// the position works at all when nothing faults.
const SOUND: [(&str, &str); 3] = [
    ("[[ ]]", r#"[[ "$x" -eq 1 ]]"#),
    ("test", r#"test "$x" -eq 1"#),
    ("(( ))", r#"(( x == 1 ))"#),
];

async fn aborts(script: &str) -> bool {
    let kernel = Kernel::transient().unwrap();
    kernel.execute(script).await.is_err()
}

async fn code_of(script: &str) -> i64 {
    let kernel = Kernel::transient().unwrap();
    kernel
        .execute(script)
        .await
        .expect("expected a result, not an abort")
        .code
}

// --- boolean-consuming positions: every construct aborts -------------------

#[tokio::test]
async fn fault_in_an_if_condition_aborts() {
    for (name, expr) in FAULTS {
        let script = format!("x=abc; if {expr}; then echo T; else echo F; fi");
        assert!(aborts(&script).await, "{name} must abort in an if condition");
    }
}

#[tokio::test]
async fn fault_in_a_while_condition_aborts() {
    for (name, expr) in FAULTS {
        let script = format!("x=abc; while {expr}; do break; done");
        assert!(
            aborts(&script).await,
            "{name} must abort in a while condition"
        );
    }
}

#[tokio::test]
async fn fault_under_negation_aborts() {
    for (name, expr) in FAULTS {
        let script = format!("x=abc; if ! {expr}; then echo T; fi");
        assert!(aborts(&script).await, "{name} must abort under `!`");
    }
}

#[tokio::test]
async fn fault_as_the_left_operand_of_and_aborts() {
    for (name, expr) in FAULTS {
        let script = format!("x=abc; {expr} && echo RIGHT_RAN");
        assert!(
            aborts(&script).await,
            "{name} must abort as the left operand of `&&`"
        );
    }
}

/// The headline case. Reading a fault as false here does not merely lose the
/// error, it prints a wrong conclusion.
#[tokio::test]
async fn fault_as_the_left_operand_of_or_aborts() {
    for (name, expr) in FAULTS {
        let script = format!(r#"x=abc; {expr} || echo "concluded: not one""#);
        assert!(
            aborts(&script).await,
            "{name} must abort as the left operand of `||`, never conclude"
        );
    }
}

#[tokio::test]
async fn fault_in_a_compound_condition_aborts() {
    for (name, expr) in FAULTS {
        let script = format!("x=abc; if {expr} && [[ 1 -eq 1 ]]; then echo T; else echo F; fi");
        assert!(
            aborts(&script).await,
            "{name} must abort inside a compound condition"
        );
    }
}

// --- non-consuming positions: exit 2, execution continues ------------------

#[tokio::test]
async fn fault_as_a_standalone_statement_is_code_2() {
    for (name, expr) in FAULTS {
        let script = format!("x=abc; {expr}");
        assert_eq!(code_of(&script).await, 2, "{name} standalone is exit 2");
    }
}

/// A standalone fault does not stop the statement list — the exit code is the
/// report, and nothing consumed it as a boolean.
#[tokio::test]
async fn a_standalone_fault_does_not_stop_the_script() {
    for (name, expr) in FAULTS {
        let kernel = Kernel::transient().unwrap();
        let script = format!("x=abc; {expr}; echo AFTER");
        let result = kernel
            .execute(&script)
            .await
            .expect("a standalone fault is a result");
        assert!(
            result.text_out().contains("AFTER"),
            "{name} must not stop the statement list"
        );
    }
}

/// The right operand of a chain is not consumed as a boolean — its value
/// becomes the chain's value — so it reports rather than aborting.
#[tokio::test]
async fn fault_as_the_right_operand_of_and_is_code_2() {
    for (name, expr) in FAULTS {
        let script = format!("x=abc; true && {expr}");
        assert_eq!(
            code_of(&script).await,
            2,
            "{name} as a right operand reports, it does not abort"
        );
    }
}

// --- controls: the same positions work when nothing faults ----------------

#[tokio::test]
async fn control_sound_comparisons_still_decide_every_position() {
    for (name, expr) in SOUND {
        assert_eq!(code_of(&format!("x=1; {expr}")).await, 0, "{name} true");
        assert_eq!(code_of(&format!("x=2; {expr}")).await, 1, "{name} false");

        let taken = format!("x=1; if {expr}; then echo T; else echo F; fi");
        let kernel = Kernel::transient().unwrap();
        let out = kernel.execute(&taken).await.expect("sound condition runs");
        assert!(out.text_out().contains('T'), "{name} true takes `then`");

        let not_taken = format!("x=2; if {expr}; then echo T; else echo F; fi");
        let kernel = Kernel::transient().unwrap();
        let out = kernel
            .execute(&not_taken)
            .await
            .expect("sound condition runs");
        assert!(out.text_out().contains('F'), "{name} false takes `else`");
    }
}

/// A false comparison must still drive `||`. The abort is for faults only —
/// if this regressed, the rule would have eaten ordinary shell control flow.
#[tokio::test]
async fn control_a_false_comparison_still_runs_the_or_branch() {
    for (name, expr) in SOUND {
        let kernel = Kernel::transient().unwrap();
        let script = format!(r#"x=2; {expr} || echo "not one""#);
        let out = kernel
            .execute(&script)
            .await
            .expect("a false comparison is not a fault");
        assert!(
            out.text_out().contains("not one"),
            "{name}: a FALSE comparison must still run the `||` branch"
        );
    }
}

/// And a true comparison must still drive `&&`.
#[tokio::test]
async fn control_a_true_comparison_still_runs_the_and_branch() {
    for (name, expr) in SOUND {
        let kernel = Kernel::transient().unwrap();
        let script = format!(r#"x=1; {expr} && echo "is one""#);
        let out = kernel
            .execute(&script)
            .await
            .expect("a true comparison is not a fault");
        assert!(
            out.text_out().contains("is one"),
            "{name}: a TRUE comparison must still run the `&&` branch"
        );
    }
}

/// A command that merely FAILS is not a fault: `grep` finding nothing, or a
/// missing file, must keep selecting `else` and driving `||` as it always
/// has. The rule is about operands that cannot be compared, not about
/// commands that ran and said no.
#[tokio::test]
async fn control_an_ordinary_command_failure_is_not_a_fault() {
    let kernel = Kernel::transient().unwrap();
    let out = kernel
        .execute("if false; then echo T; else echo F; fi")
        .await
        .expect("an ordinary failure is not a fault");
    assert!(out.text_out().contains('F'));

    let kernel = Kernel::transient().unwrap();
    let out = kernel
        .execute(r#"false || echo "fell back""#)
        .await
        .expect("an ordinary failure still drives ||");
    assert!(out.text_out().contains("fell back"));
}
