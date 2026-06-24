//! Kernel-routed tests for `env -u` and `env -i` on the listing (no-command) path.
//!
//! GNU env contract:
//!   `env -u NAME`  — remove NAME from the printed environment listing
//!   `env -i`       — print nothing (empty environment listing, ignoring inherited env)
//!
//! Both flags must apply when there is NO positional command, not only when a
//! command is specified.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::HashMap;

use kaish_kernel::ast::Value;
use kaish_kernel::{Kernel, KernelConfig};

/// Build a hermetic kernel with SECRET and KEEP seeded as exported variables.
///
/// The kernel never reads OS env; initial_vars is the only source of variables,
/// which maps exactly to what the real REPL frontend does via `os_env_vars()`.
fn kernel_with_vars() -> Kernel {
    let mut vars = HashMap::new();
    vars.insert("SECRET".to_string(), Value::String("hunter2".to_string()));
    vars.insert("KEEP".to_string(), Value::String("visible".to_string()));
    let config = KernelConfig::repl().with_initial_vars(vars);
    Kernel::new(config).expect("failed to create kernel")
}

/// `env -u SECRET` must print KEEP but must NOT print SECRET.
#[tokio::test]
async fn env_u_removes_var_from_listing() {
    let kernel = kernel_with_vars();
    let result = kernel.execute("env -u SECRET").await.expect("execute");
    let out = result.text_out().into_owned();
    assert!(result.ok(), "env -u SECRET should exit 0, got code {}: {}", result.code, result.err);
    assert!(
        out.contains("KEEP=visible"),
        "expected KEEP=visible in output:\n{out}"
    );
    assert!(
        !out.contains("SECRET"),
        "SECRET must be absent from `env -u SECRET` output:\n{out}"
    );
}

/// `env -i` must print nothing (empty environment — all inherited vars suppressed).
#[tokio::test]
async fn env_i_prints_nothing() {
    let kernel = kernel_with_vars();
    let result = kernel.execute("env -i").await.expect("execute");
    let out = result.text_out().into_owned();
    assert!(result.ok(), "env -i should exit 0, got code {}: {}", result.code, result.err);
    assert!(
        out.trim().is_empty(),
        "expected empty output from `env -i`, got:\n{out}"
    );
}

/// `env -i NAME=val` must print ONLY the explicitly supplied NAME=val pairs.
#[tokio::test]
async fn env_i_with_assignment_prints_only_that_var() {
    let kernel = kernel_with_vars();
    let result = kernel
        .execute("env -i ONLY=this")
        .await
        .expect("execute");
    let out = result.text_out().into_owned();
    assert!(result.ok(), "env -i ONLY=this should exit 0: {}", result.err);
    assert!(
        out.contains("ONLY=this"),
        "expected ONLY=this in output:\n{out}"
    );
    assert!(
        !out.contains("SECRET"),
        "SECRET must be absent after env -i:\n{out}"
    );
    assert!(
        !out.contains("KEEP"),
        "KEEP must be absent after env -i:\n{out}"
    );
}

/// Multiple `-u` flags must each remove the named variable.
#[tokio::test]
async fn env_u_multiple_removes_all_named() {
    let mut vars = HashMap::new();
    vars.insert("A".to_string(), Value::String("1".to_string()));
    vars.insert("B".to_string(), Value::String("2".to_string()));
    vars.insert("C".to_string(), Value::String("3".to_string()));
    let config = KernelConfig::repl().with_initial_vars(vars);
    let kernel = Kernel::new(config).expect("kernel");

    let result = kernel.execute("env -u A -u B").await.expect("execute");
    let out = result.text_out().into_owned();
    assert!(result.ok(), "env -u A -u B should exit 0: {}", result.err);
    assert!(
        out.contains("C=3"),
        "expected C=3 in output:\n{out}"
    );
    assert!(!out.contains("A="), "A must be absent:\n{out}");
    assert!(!out.contains("B="), "B must be absent:\n{out}");
}
