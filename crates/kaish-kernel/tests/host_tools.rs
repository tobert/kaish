//! Kernel-routed integration tests for the `ps` builtin.
//!
//! `ps` now lives in the `kaish-tools-host` crate (keeps procfs out of the
//! kernel) and runs against the portable `ToolCtx` surface. It reads `/proc`,
//! so these are Linux + `host` only. Driving through `kernel.execute` validates
//! registration and the full dispatch path.

#![cfg(all(target_os = "linux", feature = "host"))]

use kaish_kernel::{Kernel, KernelConfig};

fn host_kernel() -> Kernel {
    Kernel::new(KernelConfig::repl()).expect("failed to create kernel")
}

#[tokio::test]
async fn ps_lists_processes() {
    let kernel = host_kernel();
    let result = kernel.execute("ps -a").await.unwrap();
    assert!(result.ok(), "ps -a should succeed: {:?}", result);
    // The test process itself must show up somewhere in the listing.
    assert!(
        !result.text_out().trim().is_empty(),
        "ps -a should list at least one process"
    );
}

#[tokio::test]
async fn ps_json_is_an_array() {
    let kernel = host_kernel();
    let result = kernel.execute("ps -a --json").await.unwrap();
    assert!(result.ok(), "ps -a --json should succeed: {:?}", result);

    let parsed: serde_json::Value =
        serde_json::from_str(result.text_out().trim()).expect("ps --json must emit valid JSON");
    assert!(parsed.is_array(), "ps --json should be a JSON array: {parsed}");
    assert!(
        !parsed.as_array().unwrap().is_empty(),
        "ps --json array should be non-empty"
    );
}
