//! [`kaish_kernel::KernelError`] — the typed error out of the execute surface.
//!
//! An embedder (kaijutsu) reported that every `execute` failure arrived as
//! one untyped `anyhow::Error`, indistinguishable from a genuine execution
//! fault, so it routed a validator rejection to a model the same way it
//! routed a crash. These tests pin the fix from the caller's side: a
//! rejection is matchable as `is_rejected()` without reading `Display`
//! text, and `Display` on every variant is byte-identical to what
//! `Kernel::execute` returned before this type existed.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig, KernelError};

/// Transient kernel, matching the other error-surface test files.
fn make_kernel() -> Kernel {
    Kernel::new(KernelConfig::transient()).expect("failed to create kernel")
}

// ── 1: a validation rejection is matchable structurally, and its Display
//       text is unchanged from what `Kernel::execute` returned before this
//       type existed ─────────────────────────────────────────────────────

#[tokio::test]
async fn validation_rejection_is_matchable_as_rejected_before_running() {
    let kernel = make_kernel();
    // `break` outside a loop is IssueCode::BreakOutsideLoop (E008), an
    // Error-severity issue the validator raises before anything runs.
    let err = kernel.execute("break").await.expect_err("break outside a loop must be rejected");

    assert!(err.is_rejected(), "a validator rejection must be classified as rejected: {err:?}");
    assert!(!err.is_execution_failure());

    let KernelError::Validation { issues, message } = err else {
        panic!("break outside a loop must be KernelError::Validation, not {err:?}");
    };

    // Structural: a caller routes on the issue code, not on `message`.
    assert_eq!(issues.len(), 1, "expected exactly one validation issue: {issues:?}");
    assert_eq!(issues[0].code, kaish_kernel::validator::IssueCode::BreakOutsideLoop);
    assert_eq!(issues[0].severity, kaish_kernel::validator::Severity::Error);

    // Display is pinned to the text `Kernel::execute` returned before
    // `KernelError` existed: `anyhow::anyhow!("validation failed:\n{}",
    // error_msg)`, where `error_msg` is each issue's `.format(source)`
    // joined with "\n". `ValidationIssue::format` is exercised directly so
    // this test does not itself hardcode the wording twice.
    let expected = format!("validation failed:\n{}", issues[0].format("break"));
    assert_eq!(message, expected);
    assert_eq!(message.to_string(), expected);
}

// ── 2: a genuine execution failure is matchable as "failed while running"
//       ────────────────────────────────────────────────────────────────

#[tokio::test]
async fn arithmetic_division_by_zero_is_matchable_as_failed_while_running() {
    let kernel = make_kernel();
    // An assignment's value expression is evaluated with `?`, so a fault
    // there propagates as a real `Err` (unlike a command argument's
    // expansion, which the dispatcher catches and folds into a nonzero
    // `ExecResult` so the rest of the script can still run). `x=$((1/0))`
    // passes the validator — nothing about it is a syntax or known-bad-
    // argument problem — and faults inside arithmetic evaluation: a
    // statement genuinely started running.
    let err = kernel
        .execute("x=$((1/0))")
        .await
        .expect_err("division by zero must fail, not silently continue");

    assert!(err.is_execution_failure(), "a runtime fault must be classified as an execution failure: {err:?}");
    assert!(!err.is_rejected());

    let KernelError::Execution(inner) = err else {
        panic!("division by zero must be KernelError::Execution");
    };
    // `Display` shows only the outermost `.context(...)` — unchanged from
    // today's `anyhow::Error` behavior, since this is the same object,
    // untouched. The original fault is still there, one level down: the
    // debug chain (`{:?}`) and `source()` both still reach it, so nothing
    // was actually lost — only `Display`'s single line is terse.
    assert_eq!(inner.to_string(), "failed to evaluate assignment");
    let chain = format!("{inner:?}");
    assert!(chain.contains("division by zero"), "the chain must still carry the original fault: {chain}");
}

// ── 3: a parse/lexer error is classified deliberately ──────────────────
//
// `Kernel::execute` unifies lexer and parser failures into one
// `Vec<ParseError>` before this type existed (`parser::parse` folds a
// `LexerError` into a `ParseError` whose message is prefixed "lexer error:
// …" — see `crates/kaish-kernel/src/parser.rs`). Both are "rejected before
// running": nothing lexed cleanly enough to reach the validator, let alone
// execute_stmt_flow. `KernelError::Parse` carries both under one variant
// rather than splitting lexer/parser apart — the pre-existing unification
// is a design decision worth keeping, not an accident to work around, and
// splitting it would require carrying a distinction the kernel had already
// deliberately erased.

#[tokio::test]
async fn unterminated_string_is_a_parse_rejection() {
    let kernel = make_kernel();
    // An unterminated double-quoted string is a lexer failure.
    let err = kernel
        .execute(r#"echo "unterminated"#)
        .await
        .expect_err("an unterminated string must not lex");

    assert!(err.is_rejected(), "a lex failure must be classified as rejected: {err:?}");
    assert!(!err.is_execution_failure());

    let KernelError::Parse { errors, message } = err else {
        panic!("an unterminated string must be KernelError::Parse, not {err:?}");
    };
    assert_eq!(errors.len(), 1, "expected exactly one parse error: {errors:?}");
    assert!(
        message.starts_with("parse error:\n"),
        "Display text must keep the pre-existing \"parse error:\\n\" prefix: {message:?}"
    );
    assert!(message.contains("unterminated string"), "{message:?}");
}

#[tokio::test]
async fn genuine_syntax_error_is_also_a_parse_rejection() {
    let kernel = make_kernel();
    // Unbalanced `if` with no `fi` — a grammar failure, not a lexer one;
    // both land in the same variant (see the module doc above).
    let err = kernel.execute("if true; then echo hi").await.expect_err("missing `fi` must not parse");

    assert!(err.is_rejected());
    assert!(matches!(err, KernelError::Parse { .. }), "expected KernelError::Parse, got {err:?}");
}

// ── 4: Display output is pinned per variant, so a refactor cannot
//       silently change what an embedder reads ────────────────────────

#[tokio::test]
async fn validation_display_is_pinned() {
    let kernel = make_kernel();
    let err = kernel.execute("continue").await.expect_err("continue outside a loop must be rejected");
    assert_eq!(err.to_string(), "validation failed:\nerror [E008]: continue used outside of a loop");
}

#[tokio::test]
async fn parse_display_is_pinned() {
    let kernel = make_kernel();
    let err = kernel.execute("echo \"unterminated").await.expect_err("must not lex");
    assert_eq!(
        err.to_string(),
        "parse error:\n1:6 [parse]: lexer error: unterminated string\n  | echo \"unterminated"
    );
}

#[tokio::test]
async fn execution_display_is_pinned() {
    let kernel = make_kernel();
    let err = kernel.execute("x=$((1/0))").await.expect_err("must fault at runtime");
    // Identical to what `.to_string()` on the pre-existing `anyhow::Error`
    // produced: `Display` shows the outermost `.context(...)` only.
    assert_eq!(err.to_string(), "failed to evaluate assignment");
}
