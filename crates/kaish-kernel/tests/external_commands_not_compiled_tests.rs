//! Tests for the `subprocess` capability being compiled out entirely.
//!
//! `sandbox_mode_tests.rs` and `external_command_tests.rs` cover the runtime
//! `allow_external_commands: false` case — subprocess is compiled in, but
//! refused by configuration. This file is the other half: no `subprocess`
//! capability in this binary at all, a build-time fact rather than a config
//! value, and it gets its own distinct message (see
//! `ExternalCommandsUnavailable` in `kaish-kernel/src/tools/context.rs`).
//! Only compiles under a build that leaves `subprocess` off — CLAUDE.md's
//! `cargo test -p kaish-kernel --no-default-features` CI leg is the one that
//! runs it; `cargo test --all` unifies `subprocess` in via kaish-repl's
//! `full` feature and skips this file entirely.

#![cfg(not(feature = "subprocess"))]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

#[tokio::test]
async fn not_compiled_reports_a_build_fact_distinct_from_configured_off() {
    // Ask explicitly for external commands to be allowed at runtime — this
    // states the test's own precondition rather than relying on
    // `KernelConfig::isolated()`'s default (`false`) to silently do the
    // right thing. Without the `subprocess` capability, no runtime config
    // can produce a command execution: the refusal must still fire, and
    // with build-specific wording, not the runtime "disabled" message.
    let config = KernelConfig::isolated().with_allow_external_commands(true);
    let kernel = Kernel::new(config).expect("kernel");

    let result = kernel.execute("/bin/sh -c true").await.expect("execute");

    assert_eq!(result.code, 127, "still exit 127: {result:?}");
    let msg = format!("{}{}", result.text_out(), result.err);
    assert!(
        !msg.contains("command not found"),
        "must not be misreported as a missing command: {msg}"
    );
    assert!(
        msg.contains("external commands are not available in this build of the shell"),
        "the refusal should name the build fact, distinct from the runtime-disabled \
         wording: {msg}"
    );
    assert!(
        !msg.contains("disabled on this shell"),
        "must not use the runtime-configured-off wording when the real cause is a \
         build-time capability: {msg}"
    );
}
