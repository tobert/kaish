//! State a function call leaves behind when `exit` ends it.
//!
//! The call must still pop its frame and restore the caller's positional
//! parameters, or a later `execute` on the same kernel sees the function's
//! locals.

#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use kaish_kernel::Kernel;

#[tokio::test]
async fn exit_in_function_pops_locals_and_restores_positionals() {
    let kernel = Kernel::transient().expect("kernel");
    let (out, code) = common::run(
        &kernel,
        "f(){ local v=inner; echo \"in:$1\"; exit 3; }; f arg; echo after",
    )
    .await;
    assert_eq!((out.as_str(), code), ("in:arg", 3));

    let (out, code) = common::run(&kernel, "echo \"[${v:-}][${1:-}]\"").await;
    assert_eq!((out.as_str(), code), ("[][]", 0), "locals and $1 must not outlive the call");
}

// `timeout` re-dispatches its command through the kernel. It is not a
// subshell, so an `exit` in a function it runs ends the script. Kaish-only:
// bash's `timeout` runs a program, never a function.
async fn run_arc(script: &str) -> (String, i64) {
    let kernel = Kernel::new(kaish_kernel::KernelConfig::isolated().with_skip_validation(true))
        .expect("kernel")
        .into_arc();
    common::run(&kernel, script).await
}

#[tokio::test]
async fn exit_in_function_run_by_timeout_ends_script() {
    let (out, code) = run_arc("f(){ echo hi; exit 3; }; timeout 5 f; echo after").await;
    assert_eq!((out.as_str(), code), ("hi", 3));
}

#[tokio::test]
async fn exit_in_function_run_by_timeout_in_a_stage_ends_only_the_stage() {
    let (out, code) = run_arc("f(){ exit 3; }; timeout 5 f | cat; echo after").await;
    assert_eq!((out.as_str(), code), ("after", 0));
}

#[tokio::test]
async fn function_exit_under_errexit_ends_script_with_its_code() {
    let (out, code) = run_arc("set -e; f(){ echo hi; exit 3; }; f; echo after").await;
    assert_eq!((out.as_str(), code), ("hi", 3));
}
