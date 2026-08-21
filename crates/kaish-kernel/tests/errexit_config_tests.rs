//! Kernel-wide and per-call errexit knobs.
//!
//! kaish's default is standard shell behavior: a failing statement does not
//! abort, and the script's exit status is the LAST statement's status. For
//! an embedder whose exit status is a security or gating decision (kaijutsu's
//! motivating case), that default makes an early failure invisible in
//! exactly the case that matters. `KernelConfig::errexit_enabled` sets a
//! kernel-wide default; `ExecuteOptions::errexit` overrides it per call.
//! Both feed the same `Scope::error_exit` state that `set -e`/`set +e`
//! mutate at runtime, so there is one notion of "errexit is on" and `set -o`
//! always reports the true answer regardless of which one set it.
//!
//! `#![allow(clippy::unwrap_used, clippy::expect_used)]` per CLAUDE.md — test
//! code, not production.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::ast::Value;
use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};

// ---------------------------------------------------------------------------
// Default: off. A failing statement does not abort; the script's status is
// the LAST statement's status.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn errexit_off_by_default_does_not_abort() {
    let kernel = Kernel::transient().expect("kernel");
    let result = kernel
        .execute(r#"false; AFTER="yes""#)
        .await
        .expect("execute");
    assert!(result.ok(), "last statement (AFTER=\"yes\") should succeed: {result:?}");
    assert_eq!(kernel.get_var("AFTER").await, Some(Value::String("yes".into())));
}

#[tokio::test]
async fn errexit_off_by_default_status_is_last_statements() {
    let kernel = Kernel::transient().expect("kernel");
    let result = kernel.execute("true; false").await.expect("execute");
    assert!(!result.ok(), "last statement (false) should fail: {result:?}");
    let result = kernel.execute("false; true").await.expect("execute");
    assert!(result.ok(), "last statement (true) should succeed: {result:?}");
}

// ---------------------------------------------------------------------------
// On via `ExecuteOptions::errexit`: an early failure aborts, and the
// script's status is the failure's, not the last statement's.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn errexit_on_via_execute_options_aborts() {
    let kernel = Kernel::transient().expect("kernel");
    let result = kernel
        .execute_with_options(
            r#"false; AFTER="yes""#,
            ExecuteOptions::new().with_errexit(true),
        )
        .await
        .expect("execute");
    assert!(!result.ok(), "should abort on the early failure: {result:?}");
    assert_eq!(
        kernel.get_var("AFTER").await,
        None,
        "AFTER must not be set — errexit should abort before it runs"
    );
}

// ---------------------------------------------------------------------------
// On via `KernelConfig::errexit_enabled`: same behavior, set at construction.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn errexit_on_via_kernel_config_aborts() {
    let kernel =
        Kernel::new(KernelConfig::isolated().with_errexit(true)).expect("kernel");
    let result = kernel
        .execute(r#"false; AFTER="yes""#)
        .await
        .expect("execute");
    assert!(!result.ok(), "should abort on the early failure: {result:?}");
    assert_eq!(kernel.get_var("AFTER").await, None);
}

#[tokio::test]
async fn errexit_off_via_kernel_config_is_the_default() {
    // isolated() with no .with_errexit() call — must not change behavior for
    // an existing embedder that doesn't know the knob exists.
    let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let result = kernel
        .execute(r#"false; AFTER="yes""#)
        .await
        .expect("execute");
    assert!(result.ok());
    assert_eq!(kernel.get_var("AFTER").await, Some(Value::String("yes".into())));
}

// ---------------------------------------------------------------------------
// Per-call override wins over the kernel-config default, both directions.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn per_call_off_overrides_config_on() {
    let kernel =
        Kernel::new(KernelConfig::isolated().with_errexit(true)).expect("kernel");
    let result = kernel
        .execute_with_options(
            r#"false; AFTER="yes""#,
            ExecuteOptions::new().with_errexit(false),
        )
        .await
        .expect("execute");
    assert!(result.ok(), "per-call false should override config's on: {result:?}");
    assert_eq!(kernel.get_var("AFTER").await, Some(Value::String("yes".into())));
}

#[tokio::test]
async fn per_call_on_overrides_config_off() {
    let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let result = kernel
        .execute_with_options(
            r#"false; AFTER="yes""#,
            ExecuteOptions::new().with_errexit(true),
        )
        .await
        .expect("execute");
    assert!(!result.ok(), "per-call true should override config's off: {result:?}");
    assert_eq!(kernel.get_var("AFTER").await, None);
}

#[tokio::test]
async fn per_call_override_does_not_leak_into_the_next_call() {
    // The override is scoped to one execute_with_options call; a later plain
    // execute() must see the kernel's own state again (config default here:
    // off), not a stuck override from the previous call.
    let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    kernel
        .execute_with_options(
            r#"false; AFTER="yes""#,
            ExecuteOptions::new().with_errexit(true),
        )
        .await
        .expect("execute");
    let result = kernel
        .execute(r#"false; AFTER="yes""#)
        .await
        .expect("execute");
    assert!(result.ok(), "override must not leak: {result:?}");
    assert_eq!(kernel.get_var("AFTER").await, Some(Value::String("yes".into())));
}

// ---------------------------------------------------------------------------
// `set -e` inside the script still works when config says off — one piece
// of state, mutated by set -e/set +e regardless of where it started.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn set_e_in_script_still_works_when_config_is_off() {
    let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let result = kernel
        .execute(r#"set -e; false; AFTER="yes""#)
        .await
        .expect("execute");
    assert!(!result.ok(), "set -e in the script should still abort: {result:?}");
    assert_eq!(kernel.get_var("AFTER").await, None);
}

#[tokio::test]
async fn set_plus_e_in_script_still_works_when_config_is_on() {
    let kernel =
        Kernel::new(KernelConfig::isolated().with_errexit(true)).expect("kernel");
    let result = kernel
        .execute(r#"set +e; false; AFTER="yes""#)
        .await
        .expect("execute");
    assert!(result.ok(), "set +e in the script should still disable it: {result:?}");
    assert_eq!(kernel.get_var("AFTER").await, Some(Value::String("yes".into())));
}

// ---------------------------------------------------------------------------
// `set -o` reports errexit's state — the whole point: a config-set default
// is otherwise invisible from inside the script.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn set_dash_o_reports_errexit_off_by_default() {
    let kernel = Kernel::transient().expect("kernel");
    let out = kernel.execute("set -o").await.expect("execute").text_out().into_owned();
    assert!(out.contains("errexit\toff"), "{out:?}");
}

#[tokio::test]
async fn set_dash_o_reports_errexit_on_from_config() {
    let kernel =
        Kernel::new(KernelConfig::isolated().with_errexit(true)).expect("kernel");
    let out = kernel.execute("set -o").await.expect("execute").text_out().into_owned();
    assert!(out.contains("errexit\ton"), "{out:?}");
}

#[tokio::test]
async fn set_dash_o_reports_errexit_on_from_set_dash_e() {
    let kernel = Kernel::transient().expect("kernel");
    let out = kernel
        .execute("set -e; set -o")
        .await
        .expect("execute")
        .text_out()
        .into_owned();
    assert!(out.contains("errexit\ton"), "{out:?}");
}

#[tokio::test]
async fn set_dash_o_reports_errexit_from_a_per_call_override() {
    let kernel = Kernel::transient().expect("kernel");
    let out = kernel
        .execute_with_options("set -o", ExecuteOptions::new().with_errexit(true))
        .await
        .expect("execute")
        .text_out()
        .into_owned();
    assert!(out.contains("errexit\ton"), "{out:?}");
}
