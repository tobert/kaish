//! `&&` / `||` have EQUAL precedence and associate left-to-right (POSIX), not
//! `&&`-binds-tighter. Regression for the statement-list path (`parser.rs`); the
//! `[[ ]]` compound path was already correct.

#![cfg(feature = "localfs")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

async fn out(script: &str) -> String {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel.execute(script).await.expect("execute");
    r.text_out().trim().to_string()
}

// `true || echo A && echo B`  ==  ((true || echo A) && echo B)  -> prints B
#[tokio::test]
async fn or_then_and_left_assoc() {
    assert_eq!(out("true || echo A && echo B").await, "B");
}

// `false && echo A || echo B` == ((false && echo A) || echo B) -> prints B
#[tokio::test]
async fn and_then_or_short_circuits_to_or() {
    assert_eq!(out("false && echo A || echo B").await, "B");
}

// `true && echo A || echo B`  == ((true && echo A) || echo B)  -> prints A
#[tokio::test]
async fn and_success_skips_or_branch() {
    assert_eq!(out("true && echo A || echo B").await, "A");
}

// `false || echo A && echo B` == ((false || echo A) && echo B) -> prints A then B
#[tokio::test]
async fn or_recovers_then_and_runs() {
    assert_eq!(out("false || echo A && echo B").await, "A\nB");
}

// Unquoted `$(...)` goes through cmd_subst_parser, which had its own &&-tighter
// fold — it must use the same equal/left-assoc precedence as the top level.
#[tokio::test]
async fn cmdsubst_or_then_and_left_assoc() {
    // ((true || echo A) && echo B) -> the cmdsubst prints B
    assert_eq!(out("echo $(true || echo A && echo B)").await, "B");
}
