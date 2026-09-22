//! `spawn --timeout` must actually kill the child process, not just report a
//! 124 timeout while leaving the OS process running.
//!
//! Before this fix, the timeout arm in `Spawn::execute` wrapped
//! `child.wait_with_output()` in `tokio::time::timeout` and, on expiry, simply
//! dropped that future (and the `Child` it owned) without `kill_on_drop`
//! having been set on the underlying `Command`. Tokio only kills a child on
//! `Drop` if `kill_on_drop(true)` was set at construction, so the dropped
//! future left the real OS process running past the timeout — a leak that
//! accumulates indefinitely in a long-lived agent that repeatedly times out
//! spawned commands.
//!
//! This proves the process itself is gone, not just that `spawn` returned
//! exit code 124: it spawns a shell that sleeps longer than the timeout and
//! then touches a marker file, and asserts the marker is never created even
//! after waiting past the sleep duration.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(target_os = "linux", feature = "subprocess"))]

use std::collections::HashMap;
use std::path::Path;

use kaish_kernel::ast::Value;
use kaish_kernel::{Kernel, KernelConfig};

/// The kernel is hermetic — it never reads the OS env — so the `sh -c
/// "sleep ..."` scripts below need PATH exported to find `sleep` themselves,
/// the same way `external_command_tests.rs::repl_kernel` seeds it.
fn kernel_at(dir: &Path) -> Kernel {
    let mut vars = HashMap::new();
    vars.insert("PATH".to_string(), Value::String(std::env::var("PATH").unwrap_or_default()));
    let config = KernelConfig::repl().with_cwd(dir.to_path_buf()).with_initial_vars(vars);
    Kernel::new(config).expect("kernel")
}

#[tokio::test]
async fn spawn_timeout_kills_child_process_does_not_leak() {
    let tmp = tempfile::tempdir().unwrap();
    let marker = tmp.path().join("marker");
    let kernel = kernel_at(tmp.path());

    // Sleep (0.3s) comfortably outlasts the timeout (50ms); if the child is
    // genuinely killed, it never reaches the `touch`.
    let script = format!(
        r#"spawn --command sh --argv '["-c", "sleep 0.3; touch {}"]' --timeout 50"#,
        marker.display()
    );
    let result = kernel.execute(&script).await.expect("kernel execute");
    assert_eq!(result.code, 124, "expected timeout exit code: {:?}", result.err);

    // Give the leaked-process window (the 0.3s sleep) time to elapse, then
    // confirm the marker was never created — proof the child was actually
    // killed, not left running in the background past the timeout.
    tokio::time::sleep(std::time::Duration::from_millis(700)).await;
    assert!(
        !marker.exists(),
        "child process kept running past the timeout — leaked"
    );
}

/// A timeout keeps what the child already wrote.
///
/// `wait_with_output()` owns the buffers it fills, so dropping that future on
/// the timeout dropped the bytes with it: a child that printed a diagnostic
/// and then hung reported 124 and nothing else, and the one line that said
/// why it hung was gone.
#[tokio::test]
async fn spawn_timeout_keeps_the_childs_partial_output() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    // `sh` runs `sleep 5` as its own child, which survives a kill aimed at
    // `sh` and keeps the pipe's write end open. Waiting for EOF would make
    // this "300ms" call return in five seconds, so the elapsed-time assertion
    // below is as much the point as the output one.
    let script = r#"spawn --command sh --argv '["-c", "echo partial-out; echo partial-err >&2; sleep 5"]' --timeout 300"#;
    let started = std::time::Instant::now();
    let result = kernel.execute(script).await.expect("kernel execute");
    let elapsed = started.elapsed();

    assert_eq!(result.code, 124, "expected timeout exit code: {:?}", result.err);
    assert!(
        elapsed < std::time::Duration::from_secs(2),
        "a 300ms timeout must return promptly, not when a surviving grandchild exits: {elapsed:?}"
    );
    assert!(
        result.text_out().contains("partial-out"),
        "stdout written before the timeout must survive it: {:?}",
        result.text_out()
    );
    assert!(
        result.err.contains("partial-err"),
        "stderr written before the timeout must survive it: {:?}",
        result.err
    );
    assert!(
        result.err.contains("timed out after 300ms"),
        "the timeout diagnostic rides alongside the child's stderr: {:?}",
        result.err
    );
}

/// A child killed by a signal is not a timeout.
///
/// `ExitStatus::code()` is `None` both when the timer fires and when the
/// child dies by signal. Reading that `None` as expiry reported 124 and a
/// fabricated "timed out after 10000ms" line for a child that died in 10ms.
#[tokio::test]
async fn a_signal_death_inside_the_timeout_is_not_reported_as_one() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let script = r#"spawn --command sh --argv '["-c", "kill -9 $$"]' --timeout 10000"#;
    let result = kernel.execute(script).await.expect("kernel execute");

    // kaibo round-3 finding: `assert_ne!(_, 124)` alone can pass on a
    // DIFFERENT spawn failure (e.g. `sh` not resolving) as easily as on the
    // real fix — pinning the exact signal-death code (128 + SIGKILL) proves
    // the child actually ran and killed itself, not that spawn failed some
    // other way that also happens not to be 124.
    assert_eq!(
        result.code, 137,
        "the child died by SIGKILL (128+9), well inside the timeout: {result:?}"
    );
    assert!(
        !result.err.contains("timed out"),
        "no timeout diagnostic belongs on a signal death: {:?}",
        result.err
    );
}
