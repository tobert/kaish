//! An `anyhow::Error` chain built with `.context(...)` shows only its
//! outermost frame under `{}`/`.to_string()`; `{:#}` walks the whole chain.
//! Several sites folded a chained fault into `ExecResult.err` via
//! `.to_string()`, permanently baking in the terse form and discarding the
//! real cause forever (unlike `Kernel::execute`'s own `KernelError`, which
//! keeps the `anyhow::Error` alive so a caller can still choose `{:#}` later
//! — see `kernel_error_tests.rs`).
//!
//! Each test below reaches a fault two `.context()`/`anyhow!()` frames deep
//! — a nested `$(...)` around `x=$((1/0))` — and pins that the innermost
//! cause (`divides by zero`) survives to `ExecResult.err`.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true)).expect("failed to create kernel")
}

/// `eval_redirect_target` (`scheduler/pipeline.rs`) ran a redirect target's
/// `$(...)` through the full async evaluator and folded a fault via
/// `.map_err(|e| e.to_string())`. A redirect target that is itself a command
/// substitution containing a failing assignment carries a two-frame chain
/// ("failed to evaluate assignment" wrapping "arithmetic error: ... divides
/// by zero"); `to_string()` showed only the outer frame.
#[tokio::test]
async fn redirect_target_fault_keeps_its_cause_chain() {
    let kernel = kernel();
    let result = kernel
        .execute("echo hi > $(x=$((1/0)))")
        .await
        .expect("redirect evaluation fails into an ExecResult, not a KernelError");

    assert!(!result.ok(), "a failing redirect target must not succeed: {result:?}");
    assert!(
        result.err.contains("divides by zero"),
        "the redirect target's real cause must survive: {:?}",
        result.err
    );
}

/// `timeout`'s dispatch-error arm (`tools/builtin/timeout.rs`) formatted the
/// re-dispatched command's `anyhow::Error` with `{}` instead of `{:#}`. A
/// user tool body that faults through a chain of `.context()` calls (calling
/// it re-dispatches through the full statement executor) lost every frame
/// but the outermost.
#[tokio::test]
async fn timeout_dispatch_fault_keeps_its_cause_chain() {
    let kernel = kernel().into_arc();
    let script = "function boom { x=$((1/0)) }\ntimeout 5 boom";
    let result = kernel.execute(script).await.expect("timeout's fault path returns an ExecResult");

    assert!(!result.ok(), "a faulting tool body under timeout must not succeed: {result:?}");
    assert!(
        result.err.contains("divides by zero"),
        "the tool body's real cause must survive through timeout: {:?}",
        result.err
    );
}

/// `Stmt::Arith`'s fault arm (`kernel.rs`) is the sibling of `Stmt::Test`'s
/// (already fixed to use `{:#}`) but still folded its `anyhow::Error` with
/// `.to_string()`. A bare `(( $(...) ))` whose command substitution contains
/// a failing assignment carries the same two-frame chain as the redirect
/// case above.
#[tokio::test]
async fn bare_arith_statement_fault_keeps_its_cause_chain() {
    let kernel = kernel();
    let result = kernel
        .execute("(( $(x=$((1/0))) ))")
        .await
        .expect("a bare (( )) fault returns an ExecResult, not a KernelError");

    assert!(!result.ok(), "a faulting (( )) command substitution must not succeed: {result:?}");
    assert!(
        result.err.contains("divides by zero"),
        "the arithmetic statement's real cause must survive: {:?}",
        result.err
    );
}

/// The plain, single-frame case must be unaffected by the `{:#}` fix: a bare
/// `(( 1/0 ))` has no outer `.context()` to add a second frame, so its
/// message is unchanged (this pins that the fix does not start showing
/// duplicate or unexpected text on the common case).
#[tokio::test]
async fn bare_arith_statement_simple_fault_is_unchanged() {
    let kernel = kernel();
    let result = kernel.execute("(( 1/0 ))").await.expect("a bare (( )) fault returns an ExecResult");

    assert!(!result.ok());
    assert_eq!(result.err.trim_end(), "arithmetic error: `1 / 0` divides by zero");
}
