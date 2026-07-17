//! Kernel-routed (GH #189): a glued redirect target now fails with a
//! "quote the redirect target" hint instead of a generic parse error, and
//! the no-token-pasting guard now also covers post-`--` positionals.
//!
//! `parser_tests.rs` pins the exact parser-level message/AST shape for these
//! and the companion "flag glued to a fragment" / "short-flag glued value
//! still works" cases; this file only checks the behavior is reachable
//! through the real `kernel.execute()` path. `KernelConfig::isolated()`
//! (memory-only VFS) is used throughout so the executing test never touches
//! a real host path (per CLAUDE.md's no-hardcoded-`/tmp` rule) — the parse-
//! error tests never reach execution at all, so the FS backing is moot for
//! them, but using the same isolated kernel everywhere keeps the file simple.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

#[tokio::test]
async fn glued_redirect_target_errors_with_quote_hint() {
    let kernel = kernel();
    let result = kernel.execute("echo hi > /tmp/$(echo x).txt").await;
    assert!(result.is_err(), "glued redirect target must be a loud error");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(msg.contains("redirect target"), "should name the redirect target: {msg}");
    assert!(msg.to_lowercase().contains("quote"), "should hint to quote: {msg}");
}

#[tokio::test]
async fn quoted_redirect_target_still_works() {
    let kernel = kernel();
    let result = kernel
        .execute(r#"echo hi > "/out-$(echo x).txt"; cat "/out-$(echo x).txt""#)
        .await
        .expect("kernel execute");
    assert_eq!(result.code, 0, "got: {result:?}");
    assert_eq!(result.text_out().trim(), "hi");
}

#[tokio::test]
async fn glued_positional_after_double_dash_errors_loud() {
    let kernel = kernel();
    let result = kernel.execute("echo hi -- /tmp/$(echo x).txt").await;
    assert!(result.is_err(), "glued post-`--` positional must be a loud error");
    let msg = format!("{:#}", result.unwrap_err());
    assert!(msg.to_lowercase().contains("quote"), "should hint to quote: {msg}");
}
