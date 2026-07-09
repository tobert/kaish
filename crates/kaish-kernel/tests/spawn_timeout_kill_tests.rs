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

use std::path::Path;

use kaish_kernel::{Kernel, KernelConfig};

fn kernel_at(dir: &Path) -> Kernel {
    let config = KernelConfig::repl().with_cwd(dir.to_path_buf());
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
