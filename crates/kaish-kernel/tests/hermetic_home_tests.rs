//! The kernel is hermetic: it never reads the host `HOME` env var. `HOME` is
//! owned by the frontend via `initial_vars`. These tests pin that contract so
//! a regression (re-introducing `std::env::var("HOME")` in construction, tilde
//! expansion, or `cd`) fails loudly.
//!
//! See docs/issues.md "Kernel reads host HOME unconditionally".

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::HashMap;

use kaish_kernel::ast::Value;
use kaish_kernel::{Kernel, KernelConfig};

/// A kernel with `HOME` seeded into `initial_vars` (what the REPL/MCP do).
fn kernel_with_home(home: &str) -> Kernel {
    let mut vars = HashMap::new();
    vars.insert("HOME".to_string(), Value::String(home.into()));
    // `isolated()` (memory VFS) rather than `repl()` so this runs in the
    // minimal `--no-default-features` build too — tilde/HOME logic is
    // VFS-independent (echo is a builtin; no real FS needed).
    Kernel::new(KernelConfig::isolated().with_initial_vars(vars)).expect("kernel")
}

/// A hermetic kernel with empty `initial_vars` (what a sandboxed embedder does).
fn hermetic_kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated().with_initial_vars(HashMap::new())).expect("kernel")
}

#[tokio::test]
async fn tilde_expands_to_session_home_not_host() {
    // The session HOME drives `~`, regardless of the developer's real $HOME.
    let kernel = kernel_with_home("/seeded/home");

    let out = kernel.execute("echo ~").await.unwrap();
    assert_eq!(out.text_out().trim(), "/seeded/home");

    let out = kernel.execute("echo ~/projects").await.unwrap();
    assert_eq!(out.text_out().trim(), "/seeded/home/projects");
}

#[tokio::test]
async fn hermetic_kernel_does_not_leak_host_home_via_tilde() {
    // With no HOME in scope, `~` must stay literal — never the host home dir.
    let kernel = hermetic_kernel();

    let out = kernel.execute("echo ~").await.unwrap();
    assert_eq!(out.text_out().trim(), "~", "~ leaked a home directory");

    let out = kernel.execute("echo ~/projects").await.unwrap();
    assert_eq!(out.text_out().trim(), "~/projects");
}

#[tokio::test]
async fn hermetic_kernel_has_no_home_var() {
    // $HOME is unset in a hermetic kernel — construction must not seed it from
    // the host env.
    let kernel = hermetic_kernel();
    let out = kernel.execute("echo $HOME").await.unwrap();
    assert_eq!(out.text_out().trim(), "", "$HOME leaked from the host env");
}
