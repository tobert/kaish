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
