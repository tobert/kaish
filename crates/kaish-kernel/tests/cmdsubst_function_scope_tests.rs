//! A function defined, redefined, or sourced inside `$(...)` must not
//! reach the session: `x=$(f() { echo hi; }); f` is "command not found"
//! (exit 127), as in bash. `$(...)` already restores variables, cwd, aliases,
//! ignore config, and output limit; the function table is the same kind of
//! session state. Each test mutates inside `$(...)` and reads the table back
//! from a LATER `kernel.execute()` call.

#![cfg(feature = "localfs")]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;
use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

async fn run(kernel: &Kernel, script: &str) -> (String, i64) {
    let result = kernel.execute(script).await.expect("kernel execute");
    (result.text_out().to_string(), result.code)
}

async fn assert_not_defined(kernel: &Kernel, name: &str) {
    assert!(
        !kernel.has_function(name).await,
        "function `{name}` leaked out of $(...)"
    );
    let (out, code) = run(kernel, name).await;
    assert_eq!(code, 127, "calling leaked `{name}` must be command-not-found, got {out:?}");
}

#[tokio::test]
async fn bare_cmd_subst_function_definition_does_not_leak() {
    let k = kernel();
    let (_, code) = run(&k, r#"x=$(leaked() { echo hi; }; echo done)"#).await;
    assert_eq!(code, 0);
    assert_not_defined(&k, "leaked").await;
}

#[tokio::test]
async fn string_interpolated_cmd_subst_function_definition_does_not_leak() {
    let k = kernel();
    run(&k, r#"echo "a $(leaked() { echo hi; }; echo b) c""#).await;
    assert_not_defined(&k, "leaked").await;
}

#[tokio::test]
async fn arithmetic_cmd_subst_function_definition_does_not_leak() {
    let k = kernel();
    let (out, code) = run(&k, r#"echo $(( $(leaked() { echo hi; }; echo 4) + 1 ))"#).await;
    assert_eq!((out.trim(), code), ("5", 0));
    assert_not_defined(&k, "leaked").await;
}

#[tokio::test]
async fn function_body_cmd_subst_definition_does_not_leak() {
    let k = kernel();
    run(&k, "outer() { x=$(inner() { echo hi; }; echo done); }; outer").await;
    assert_not_defined(&k, "inner").await;
    assert!(k.has_function("outer").await);
}

#[tokio::test]
async fn cmd_subst_redefinition_keeps_the_original() {
    let k = kernel();
    run(&k, "f() { echo old; }").await;
    let (_, code) = run(&k, r#"x=$(f() { echo new; }; f)"#).await;
    assert_eq!(code, 0);
    let (out, code) = run(&k, "f").await;
    assert_eq!((out.trim(), code), ("old", 0), "redefinition inside $(...) must not stick");
}

#[tokio::test]
async fn cmd_subst_sees_its_own_definition_while_running() {
    let k = kernel();
    let (out, code) = run(&k, r#"x=$(g() { echo inside; }; g); echo "$x""#).await;
    assert_eq!((out.trim(), code), ("inside", 0));
}

#[tokio::test]
async fn cmd_subst_in_pipeline_stage_function_definition_does_not_leak() {
    let k = kernel();
    run(&k, r#"echo "$(leaked() { echo hi; }; echo x)" | cat"#).await;
    assert_not_defined(&k, "leaked").await;
    run(&k, r#"echo a | cat | echo "$(leaked2() { echo hi; }; echo x)""#).await;
    assert_not_defined(&k, "leaked2").await;
}

#[tokio::test]
async fn cmd_subst_in_redirect_target_function_definition_does_not_leak() {
    let dir = tempfile::tempdir().unwrap();
    let k = kernel_at(dir.path());
    run(&k, r#"echo hi > $(leaked() { echo hi; }; echo out.txt)"#).await;
    assert_not_defined(&k, "leaked").await;
    let (out, _) = run(&k, "cat out.txt").await;
    assert_eq!(out.trim(), "hi");
}

#[tokio::test]
async fn cmd_subst_exit_still_restores_functions() {
    let k = kernel();
    let (_, code) = run(&k, r#"x=$(leaked() { echo hi; }; exit 3); echo "rc=$?""#).await;
    assert_eq!(code, 0);
    assert_not_defined(&k, "leaked").await;
}

#[tokio::test]
async fn cmd_subst_error_still_restores_functions() {
    let k = kernel();
    let result = k.execute(r#"x=$(leaked() { echo hi; }; echo $((1/0)))"#).await;
    assert!(result.is_err() || result.is_ok_and(|r| r.code != 0), "division by zero must fail");
    assert_not_defined(&k, "leaked").await;
}

#[tokio::test]
async fn cmd_subst_source_does_not_leak_sourced_functions() {
    let dir = tempfile::tempdir().unwrap();
    let k = kernel_at(dir.path());
    run(&k, "echo 'q() { echo Q; }' > s.sh").await;
    run(&k, "x=$(source ./s.sh; q)").await;
    assert_not_defined(&k, "q").await;
}

#[tokio::test]
async fn function_defined_outside_survives_cmd_subst() {
    let k = kernel();
    run(&k, "keep() { echo kept; }; x=$(keep)").await;
    let (out, code) = run(&k, "keep").await;
    assert_eq!((out.trim(), code), ("kept", 0));
}
