//! A runtime fault stops a program; it does not unprint what already ran.
//!
//! `break`, `continue`, `return`, and `exit` already carry the output a block
//! produced before they left it. A runtime fault (`x=$((1/0))`) propagated as
//! a bare error and dropped that output, so `echo left && x=$((1/0))` printed
//! nothing but the error. `KernelError::Execution { output, .. }` now holds
//! the output that ran, and a streaming caller receives it before the `Err`.
//!
//! Only builtins run here, so these need no feature gate.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{ExecuteOptions, Kernel, KernelError};

/// Run `program`, which must fault while running; return the error's output
/// and its full cause text.
async fn fault(program: &str) -> (ExecResult, String) {
    let kernel = Kernel::transient().expect("kernel");
    let err = kernel.execute(program).await.expect_err("program must fault while running");
    let cause = format!("{err:#}");
    let KernelError::Execution { output, .. } = err else {
        panic!("expected an execution fault, got {err:?}");
    };
    (*output, cause)
}

#[tokio::test]
async fn statement_before_the_fault_is_kept() {
    let (output, cause) = fault("echo first; x=$((1/0))").await;
    assert_eq!(output.text_out(), "first\n");
    assert!(cause.contains("divides by zero"), "{cause}");
}

#[tokio::test]
async fn left_side_of_an_and_chain_is_kept() {
    let (output, _) = fault("echo left && x=$((1/0))").await;
    assert_eq!(output.text_out(), "left\n");
}

#[tokio::test]
async fn earlier_statements_and_the_faulting_statement_are_both_kept() {
    let (output, _) = fault("echo first; echo left && x=$((1/0)); echo never").await;
    assert_eq!(output.text_out(), "first\nleft\n");
}

#[tokio::test]
async fn if_body_output_is_kept() {
    let (output, _) = fault("if true; then echo in-if; x=$((1/0)); fi").await;
    assert_eq!(output.text_out(), "in-if\n");
}

#[tokio::test]
async fn loop_iterations_are_kept() {
    let (output, _) = fault("for i in 1 2 3; do echo $i; if [[ $i == 2 ]]; then x=$((1/0)); fi; done").await;
    assert_eq!(output.text_out(), "1\n2\n");
}

#[tokio::test]
async fn function_body_output_is_kept_in_its_failed_result() {
    // A command that faults becomes a failed result (exit 1), and the script
    // goes on; the result keeps what the function printed and names the cause.
    let kernel = Kernel::transient().expect("kernel");
    let result = kernel
        .execute("f() { echo in-f; x=$((1/0)); }; f")
        .await
        .expect("a faulting command is a failed result, not an error");
    assert_eq!(result.code, 1, "{result:?}");
    assert_eq!(result.text_out(), "in-f\n");
    assert!(result.err.contains("divides by zero"), "{:?}", result.err);
}

#[tokio::test]
async fn stderr_before_the_fault_is_kept() {
    let (output, _) = fault("echo early >&2 && x=$((1/0))").await;
    assert_eq!(output.err, "early\n");
}

#[tokio::test]
async fn a_fault_before_any_output_carries_empty_output() {
    // Control: nothing ran before the fault, so there is nothing to keep.
    let (output, cause) = fault("x=$((1/0))").await;
    assert_eq!(output.text_out(), "");
    assert!(cause.contains("divides by zero"), "{cause}");
}

#[tokio::test]
async fn streaming_caller_receives_the_faulting_statement_output() {
    let kernel = Kernel::transient().expect("kernel");
    let mut streamed = String::new();
    let mut on_output = |r: &ExecResult| streamed.push_str(&r.text_out());
    let err = kernel
        .execute_with_options_streaming("echo first; echo left && x=$((1/0))", ExecuteOptions::new(), &mut on_output)
        .await
        .expect_err("program must fault");
    assert!(err.is_execution_failure());
    assert_eq!(streamed, "first\nleft\n", "the stream must show what ran before the fault");
}

#[tokio::test]
async fn display_is_unchanged_by_the_carried_output() {
    let kernel = Kernel::transient().expect("kernel");
    let err = kernel.execute("echo left && x=$((1/0))").await.expect_err("fault");
    assert_eq!(err.to_string(), "failed to evaluate assignment");
    let alternate = format!("{err:#}");
    assert!(alternate.starts_with("failed to evaluate assignment: "), "{alternate}");
    assert!(alternate.contains("divides by zero"), "{alternate}");
    assert!(!alternate.contains("left"), "output must not leak into the error text: {alternate}");
}
