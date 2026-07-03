//! Cancellation, request-timeout, and child-process kill tests.
//!
//! These tests verify the SIGTERM-grace-SIGKILL discipline by spawning real
//! external processes (sleep, bash) and asserting the OS PID is gone after
//! the kernel returns. Each test uses a small real time budget (50–500ms);
//! `tokio::time::pause` is intentionally avoided because `nix::kill` and OS
//! signal delivery do not honor the tokio test clock.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(unix, feature = "subprocess"))]

use std::path::Path;
use std::sync::Arc;
use std::time::{Duration, Instant};

use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};
use kaish_kernel::ast::Value;

mod common_cancel {
    use super::*;
    use std::collections::HashMap;
    use std::fs;

    /// PATH (from OS env) as `initial_vars`, mirroring what the real REPL
    /// frontend does with `os_env_vars()`. The kernel never reads OS PATH, so
    /// these tests must seed it to resolve external commands (`bash`, `sleep`).
    pub fn path_vars() -> HashMap<String, Value> {
        let mut vars = HashMap::new();
        vars.insert(
            "PATH".to_string(),
            Value::String(std::env::var("PATH").unwrap_or_default()),
        );
        vars
    }

    /// Returns true while the OS sees `pid` as a live process. Uses `kill(pid, 0)`,
    /// which signals nothing but errors with ESRCH when the PID is gone.
    pub fn child_alive(pid: u32) -> bool {
        nix::sys::signal::kill(nix::unistd::Pid::from_raw(pid as i32), None).is_ok()
    }

    /// Polls `child_alive` every 10ms up to `max`. Returns true if the PID went
    /// away within the deadline.
    pub async fn wait_for_dead(pid: u32, max: Duration) -> bool {
        let deadline = Instant::now() + max;
        while Instant::now() < deadline {
            if !child_alive(pid) {
                return true;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        !child_alive(pid)
    }

    /// Builds a tempfile shell script that writes its own PID to `pid_file`,
    /// then `exec`s the inner shell snippet. Returns the script path.
    ///
    /// The kaish call invokes `bash <script>` so we know the spawned process
    /// is the one whose PID lands in `pid_file`. Reading the file after the
    /// kaish call returns gives us the OS PID for liveness checks.
    ///
    /// The script filename is derived from `pid_file`'s stem so multiple
    /// scripts in the same tempdir don't clobber each other.
    pub fn pid_writer(tmp_dir: &Path, pid_file: &Path, inner: &str) -> std::path::PathBuf {
        let stem = pid_file
            .file_stem()
            .map(|s| s.to_string_lossy().into_owned())
            .unwrap_or_else(|| "pid".to_string());
        let script_path = tmp_dir.join(format!("pid_writer_{}.sh", stem));
        let script = format!(
            "#!/bin/bash\necho $$ > {pf}\nexec {inner}\n",
            pf = pid_file.display(),
            inner = inner,
        );
        fs::write(&script_path, script).expect("write pid_writer script");
        // Make executable
        use std::os::unix::fs::PermissionsExt;
        let mut perms = fs::metadata(&script_path).expect("stat script").permissions();
        perms.set_mode(0o755);
        fs::set_permissions(&script_path, perms).expect("chmod script");
        script_path
    }

    /// Reads the child PID from the *first* line of `pid_file`. First-line (not
    /// whole-file) so scripts that append more lines after their pid (e.g. the
    /// vars-combo test writes `WHO=…` on line 2) still parse.
    pub fn read_pid(pid_file: &Path) -> Option<u32> {
        let s = fs::read_to_string(pid_file).ok()?;
        s.lines().next()?.trim().parse().ok()
    }

    /// Polls for the child's recorded PID up to `max`. The `pid_writer` script
    /// writes its PID as its very first action, but the write can land a beat
    /// after `execute()` returns (fs flush) — and the *test's* job is to act
    /// once the child is actually running. Polling here (instead of a bare
    /// `read_pid().expect()`) is what keeps these tests from flaking under load:
    /// a kill/cancel window that fires during bash startup must not race the
    /// pid write into a panic. Returns `None` only if the pid never appears.
    pub async fn wait_for_pid(pid_file: &Path, max: Duration) -> Option<u32> {
        let deadline = Instant::now() + max;
        loop {
            if let Some(pid) = read_pid(pid_file) {
                return Some(pid);
            }
            if Instant::now() >= deadline {
                return None;
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
    }

    pub fn kernel_for_test() -> Arc<Kernel> {
        Kernel::new(
            KernelConfig::repl()
                .with_skip_validation(true)
                .with_kill_grace(Duration::from_millis(500))
                .with_initial_vars(path_vars()),
        )
        .expect("kernel")
        .into_arc()
    }
}

use common_cancel::{child_alive, kernel_for_test, path_vars, pid_writer, wait_for_dead, wait_for_pid};

// ════════════════════════════════════════════════════════════════════════════
// 1. request_timeout kills a foreground external
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn request_timeout_kills_external_child() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    let script = pid_writer(tmp.path(), &pid_file, "sleep 60");

    let kernel = kernel_for_test();
    // 500ms (not 150ms): the timeout must outlast worst-case bash fork/exec +
    // pid-write under load, or the child is killed before recording its pid and
    // the read below has nothing to check. Still vastly under `sleep 60`, so the
    // timeout path is exactly what fires.
    let result = kernel
        .execute_with_options(
            &format!("bash {}", script.display()),
            ExecuteOptions::new().with_timeout(Duration::from_millis(500)))
        .await
        .expect("execute");

    assert_eq!(result.code, 124, "expected timeout exit 124, got {} err={}", result.code, result.err);

    let pid = wait_for_pid(&pid_file, Duration::from_secs(2)).await.expect("pid_file");
    assert!(
        wait_for_dead(pid, Duration::from_secs(2)).await,
        "child pid {} still alive after timeout + grace",
        pid,
    );
}

// ════════════════════════════════════════════════════════════════════════════
// 2. Per-call ExecuteOptions::timeout overrides KernelConfig
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn per_call_timeout_overrides_config_default() {
    // Config sets a long default timeout; per-call passes a short one.
    let kernel = Kernel::new(
        KernelConfig::repl()
            .with_skip_validation(true)
            .with_request_timeout(Duration::from_secs(60))
            .with_kill_grace(Duration::from_millis(300))
            .with_initial_vars(path_vars()),
    )
    .expect("kernel")
    .into_arc();

    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    let script = pid_writer(tmp.path(), &pid_file, "sleep 60");

    // 500ms per-call timeout (see test 1): outlast bash startup so the child
    // records its pid before the kill, while still overriding the 60s config.
    let result = kernel
        .execute_with_options(
            &format!("bash {}", script.display()),
            ExecuteOptions::new().with_timeout(Duration::from_millis(500)))
        .await
        .expect("execute");

    assert_eq!(result.code, 124);
    let pid = wait_for_pid(&pid_file, Duration::from_secs(2)).await.expect("pid_file");
    assert!(
        wait_for_dead(pid, Duration::from_secs(2)).await,
        "per-call timeout did not kill child {}",
        pid,
    );
}

// ════════════════════════════════════════════════════════════════════════════
// 3. ZERO-duration timeout returns 124 immediately without spawning
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn zero_duration_timeout_returns_124_without_spawn() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    let script = pid_writer(tmp.path(), &pid_file, "sleep 60");

    let kernel = kernel_for_test();
    let result = kernel
        .execute_with_options(
            &format!("bash {}", script.display()),
            ExecuteOptions::new().with_timeout(Duration::ZERO))
        .await
        .expect("execute");

    assert_eq!(result.code, 124);
    // PID file should NOT exist — bash never ran.
    assert!(!pid_file.exists(), "pid_file exists; bash was spawned despite ZERO timeout");
}

// ════════════════════════════════════════════════════════════════════════════
// 4. Kernel::cancel() from an embedder kills the foreground external
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn kernel_cancel_kills_running_external() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    let script = pid_writer(tmp.path(), &pid_file, "sleep 60");

    let kernel = kernel_for_test();
    let kernel_clone = kernel.clone();
    let pid_file_clone = pid_file.clone();

    // Cancel once the child is actually running (its pid is recorded), not after
    // a fixed sleep — otherwise the cancel can fire during bash startup and race
    // the pid write, the flake this suite suffered under load.
    tokio::spawn(async move {
        let _ = wait_for_pid(&pid_file_clone, Duration::from_secs(2)).await;
        kernel_clone.cancel();
    });

    let result = kernel
        .execute(&format!("bash {}", script.display()))
        .await
        .expect("execute");

    // Cancel returns control with the kernel's "interrupted" path; exit code
    // is 130 (SIGINT-style) on the cancellation checkpoint.
    assert!(
        result.code == 130 || result.code == 143,
        "expected 130 or 143, got {}",
        result.code,
    );

    let pid = wait_for_pid(&pid_file, Duration::from_secs(2)).await.expect("pid_file");
    assert!(
        wait_for_dead(pid, Duration::from_secs(2)).await,
        "Kernel::cancel did not kill external pid {}",
        pid,
    );
}

// ════════════════════════════════════════════════════════════════════════════
// 5. timeout builtin kills the inner external
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn timeout_builtin_kills_inner_external() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    let script = pid_writer(tmp.path(), &pid_file, "sleep 60");

    let kernel = kernel_for_test();
    let result = kernel
        .execute(&format!("timeout 1 bash {}", script.display()))
        .await
        .expect("execute");

    assert_eq!(result.code, 124, "expected 124, got code={} err={}", result.code, result.err);
    let pid = wait_for_pid(&pid_file, Duration::from_secs(2)).await.expect("pid_file");
    assert!(
        wait_for_dead(pid, Duration::from_secs(3)).await,
        "timeout builtin left pid {} alive",
        pid,
    );
}

// ════════════════════════════════════════════════════════════════════════════
// 6. Pipeline cascade: cancel kills both stages of a `sleep | cat`
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn pipeline_cascade_kills_both_stages() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let p1 = tmp.path().join("pid1");
    let p2 = tmp.path().join("pid2");
    let s1 = pid_writer(tmp.path(), &p1, "sleep 60");
    let s2 = pid_writer(tmp.path(), &p2, "cat");

    let kernel = kernel_for_test();
    // 500ms (not 200ms): both pipeline stages are separate bash forks that must
    // each record their pid before the timeout kills the group (see test 1).
    let result = kernel
        .execute_with_options(
            &format!("bash {} | bash {}", s1.display(), s2.display()),
            ExecuteOptions::new().with_timeout(Duration::from_millis(500)))
        .await
        .expect("execute");

    assert_eq!(result.code, 124);
    let pid1 = wait_for_pid(&p1, Duration::from_secs(2)).await.expect("pid1");
    let pid2 = wait_for_pid(&p2, Duration::from_secs(2)).await.expect("pid2");
    assert!(wait_for_dead(pid1, Duration::from_secs(2)).await, "pid1 {} alive", pid1);
    assert!(wait_for_dead(pid2, Duration::from_secs(2)).await, "pid2 {} alive", pid2);
}

// ════════════════════════════════════════════════════════════════════════════
// 7. SIGTERM-trap grace escalation: child ignores TERM, gets SIGKILL after grace
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn grace_escalation_sigkills_term_trapping_child() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    // Trap SIGTERM and ignore; SIGKILL is uncatchable so the child dies after grace.
    let script = pid_writer(tmp.path(), &pid_file, "bash -c 'trap \"\" TERM; sleep 60'");

    let kernel = Kernel::new(
        KernelConfig::repl()
            .with_skip_validation(true)
            .with_kill_grace(Duration::from_millis(200))
            .with_initial_vars(path_vars()),
    )
    .expect("kernel")
    .into_arc();

    let started = Instant::now();
    // 500ms timeout so the (nested) bash records its pid before the kill; the
    // 200ms grace above is what this test really exercises — SIGKILL after the
    // TERM-trapping child ignores SIGTERM.
    let result = kernel
        .execute_with_options(
            &format!("bash {}", script.display()),
            ExecuteOptions::new().with_timeout(Duration::from_millis(500)))
        .await
        .expect("execute");
    let elapsed = started.elapsed();

    assert_eq!(result.code, 124);
    let pid = wait_for_pid(&pid_file, Duration::from_secs(2)).await.expect("pid");
    assert!(
        wait_for_dead(pid, Duration::from_secs(3)).await,
        "TERM-trapping pid {} survived SIGKILL escalation",
        pid,
    );
    // Sanity: total time ≈ timeout + grace, not ~60s.
    assert!(
        elapsed < Duration::from_secs(5),
        "took too long ({:?}) — escalation may not have fired",
        elapsed,
    );
}

// ════════════════════════════════════════════════════════════════════════════
// 8. Background `&` jobs survive parent cancellation
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn background_amp_job_survives_parent_cancel() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    let script = pid_writer(tmp.path(), &pid_file, "sleep 5");

    let kernel = kernel_for_test();
    // Launch in background and immediately exit the foreground script.
    let _ = kernel
        .execute(&format!("bash {} &", script.display()))
        .await
        .expect("execute");

    // Wait for the bg child to record its PID (2s budget for load headroom).
    let pid = wait_for_pid(&pid_file, Duration::from_secs(2)).await.expect("pid");

    // Cancel the parent kernel — bg job should NOT be killed.
    kernel.cancel();
    tokio::time::sleep(Duration::from_millis(300)).await;
    assert!(
        child_alive(pid),
        "background job pid {} died after parent cancel (should survive)",
        pid,
    );

    // Cleanup: kill it so we don't leak.
    let _ = nix::sys::signal::kill(
        nix::unistd::Pid::from_raw(pid as i32),
        nix::sys::signal::Signal::SIGKILL,
    );
}

// ════════════════════════════════════════════════════════════════════════════
// 9. Embedder cancel_token fires the request without leaking into the kernel
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn embedder_cancel_token_does_not_leak_into_kernel_state() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    let script = pid_writer(tmp.path(), &pid_file, "sleep 60");

    let kernel = kernel_for_test();
    let embedder_token = tokio_util::sync::CancellationToken::new();

    // Fire the embedder token once the child is running, not after a fixed
    // sleep — avoids cancelling during bash startup (the load-flake).
    let token_clone = embedder_token.clone();
    let pid_file_clone = pid_file.clone();
    tokio::spawn(async move {
        let _ = wait_for_pid(&pid_file_clone, Duration::from_secs(2)).await;
        token_clone.cancel();
    });

    let result = kernel
        .execute_with_options(
            &format!("bash {}", script.display()),
            ExecuteOptions::new().with_cancel_token(embedder_token.clone()))
        .await
        .expect("execute");

    // The cancellation cascaded; child is dead.
    let pid = wait_for_pid(&pid_file, Duration::from_secs(2)).await.expect("pid");
    assert!(
        wait_for_dead(pid, Duration::from_secs(2)).await,
        "embedder cancel did not kill pid {}",
        pid,
    );

    // Embedder still holds the token (proof: it's still cancelled).
    assert!(embedder_token.is_cancelled());

    // Run a second call without the embedder token. The kernel must NOT have
    // retained the embedder's token (which is already cancelled) — otherwise
    // the second call would short-circuit.
    let echo_result = kernel.execute("echo ok").await.expect("second execute");
    assert!(echo_result.ok(), "kernel leaked embedder token: second call failed code={} err={}",
        echo_result.code, echo_result.err);
    drop(result);
}

// ════════════════════════════════════════════════════════════════════════════
// 10. Vars overlay applies during the call and cleans up after
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn vars_overlay_visible_during_call_and_cleaned_up_after() {
    let kernel = kernel_for_test();

    let mut vars = std::collections::HashMap::new();
    vars.insert("OVERLAY_X".to_string(), Value::String("hello".to_string()));

    let result = kernel
        .execute_with_options(r#"echo "${OVERLAY_X}""#, ExecuteOptions::new().with_vars(vars))
        .await
        .expect("execute");

    assert!(result.ok(), "echo failed: {}", result.err);
    assert_eq!(result.text_out().trim(), "hello");

    // Second call without overlay: var should be unset.
    let after = kernel
        .execute_with_options(r#"echo "${OVERLAY_X:-unset}""#, ExecuteOptions::default())
        .await
        .expect("execute");
    assert_eq!(after.text_out().trim(), "unset", "vars overlay leaked between calls");
}

// ════════════════════════════════════════════════════════════════════════════
// 11. Combo: vars overlay + per-call timeout (both apply, child still killed)
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn vars_plus_timeout_combo_kills_child_with_vars_visible() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    // Script reads $WHO, writes its pid, then sleeps. We assert it sees the var.
    let inner_path = tmp.path().join("inner.sh");
    std::fs::write(
        &inner_path,
        format!(
            "#!/bin/bash\necho $$ > {pf}\necho \"WHO=$WHO\" >> {pf}\nsleep 60\n",
            pf = pid_file.display(),
        ),
    )
    .expect("write inner");
    use std::os::unix::fs::PermissionsExt;
    let mut perms = std::fs::metadata(&inner_path).expect("stat").permissions();
    perms.set_mode(0o755);
    std::fs::set_permissions(&inner_path, perms).expect("chmod");

    let kernel = kernel_for_test();
    let mut vars = std::collections::HashMap::new();
    vars.insert("WHO".to_string(), Value::String("amy".to_string()));

    let result = kernel
        .execute_with_options(
            &format!("bash {}", inner_path.display()),
            ExecuteOptions::new()
                .with_vars(vars)
                // 500ms (see test 1): outlast bash startup so the child records
                // both its pid and the WHO line before the timeout kills it.
                .with_timeout(Duration::from_millis(500)))
        .await
        .expect("execute");

    assert_eq!(result.code, 124, "expected 124, got {}", result.code);

    // Child wrote its PID and the WHO line before sleeping. Poll: the second
    // line (WHO) may flush a beat after execute() returns under load.
    let _ = wait_for_pid(&pid_file, Duration::from_secs(2)).await.expect("pid_file");
    let contents = std::fs::read_to_string(&pid_file).expect("read pid_file");
    let mut lines = contents.lines();
    let pid: u32 = lines.next().expect("pid line").parse().expect("parse pid");
    let who_line = lines.next().expect("who line");
    assert_eq!(who_line, "WHO=amy", "vars overlay did not reach child env");

    assert!(
        wait_for_dead(pid, Duration::from_secs(2)).await,
        "combo (vars+timeout) left pid {} alive",
        pid,
    );
}

// ════════════════════════════════════════════════════════════════════════════
// 12. Combo: cancel_token + vars (cancel still kills child; vars still applied)
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn cancel_token_plus_vars_combo() {
    let tmp = tempfile::tempdir().expect("tempdir");
    let pid_file = tmp.path().join("pid");
    let script = pid_writer(tmp.path(), &pid_file, "sleep 60");

    let kernel = kernel_for_test();
    let token = tokio_util::sync::CancellationToken::new();

    let token_clone = token.clone();
    let pid_file_clone = pid_file.clone();
    tokio::spawn(async move {
        // Cancel once the child is running, not after a fixed sleep (load-flake).
        let _ = wait_for_pid(&pid_file_clone, Duration::from_secs(2)).await;
        token_clone.cancel();
    });

    let mut vars = std::collections::HashMap::new();
    vars.insert("FROM_OPTS".to_string(), Value::String("yes".to_string()));

    let _ = kernel
        .execute_with_options(
            &format!("bash {}", script.display()),
            ExecuteOptions::new()
                .with_vars(vars)
                .with_cancel_token(token))
        .await
        .expect("execute");

    let pid = wait_for_pid(&pid_file, Duration::from_secs(2)).await.expect("pid");
    assert!(wait_for_dead(pid, Duration::from_secs(2)).await);
}

// ════════════════════════════════════════════════════════════════════════════
// 13. Scatter --timeout cancels stuck workers and tags timed_out
// ════════════════════════════════════════════════════════════════════════════
//
// We can't easily inspect ScatterResult.timed_out from the language (the
// flag surfaces in JSON output). Verifying via the JSON gather output is
// sufficient: each entry should have "timed_out": true.

#[tokio::test]
async fn scatter_timeout_kills_stuck_workers() {
    // Use external `bash -c "sleep 60"` so the worker's cancel actually kills
    // the OS process. The builtin `sleep` is a plain tokio::time::sleep that
    // doesn't honor cancellation, so a builtin sleep would run to completion
    // even with the cancel token cancelled.
    let kernel = kernel_for_test();
    let result = kernel
        .execute(
            r#"echo "1\n2\n3" | split "\n" | scatter --limit 3 --timeout 300ms | bash -c "sleep 60" | gather"#,
        )
        .await
        .expect("execute");

    // GH #73: timeouts are failure rows (code 124, timed_out:true) and gather
    // exits 123.
    assert_eq!(result.code, 123, "timeouts count as failures: {:?}", result.err);
    let output = result.text_out();
    let trimmed = output.trim();
    assert!(
        trimmed.contains("\"timed_out\":true"),
        "scatter --timeout did not surface timed_out rows: {}",
        trimmed,
    );
    assert!(
        trimmed.contains("\"code\":124"),
        "timed-out rows report 124: {}",
        trimmed,
    );
}

// ════════════════════════════════════════════════════════════════════════════
// 14. on_output callback fires for each statement
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn on_output_callback_fires_per_statement() {
    let kernel = kernel_for_test();
    let mut count = 0usize;
    let mut cb = |_: &kaish_kernel::interpreter::ExecResult| {
        count += 1;
    };
    let _ = kernel
        .execute_with_options_streaming("echo a\necho b\necho c", ExecuteOptions::default(), &mut cb)
        .await
        .expect("execute");
    assert!(count >= 3, "callback fired {} times for 3 statements", count);
}

// ════════════════════════════════════════════════════════════════════════════
// 15. Per-call cwd override pushes for the call and restores on return
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn per_call_cwd_overrides_and_restores() {
    let kernel = kernel_for_test();
    let original_cwd = kernel.cwd().await;

    let tmp = tempfile::tempdir().expect("tempdir");
    let scratch = tmp.path().to_path_buf();

    let result = kernel
        .execute_with_options("pwd", ExecuteOptions::new().with_cwd(scratch.clone()))
        .await
        .expect("execute");
    assert!(result.ok(), "pwd failed: {}", result.err);
    let printed = result.text_out().trim().to_string();
    let canonical_scratch = scratch.canonicalize().unwrap_or(scratch.clone());
    let canonical_printed = std::path::PathBuf::from(&printed)
        .canonicalize()
        .unwrap_or_else(|_| std::path::PathBuf::from(&printed));
    assert_eq!(
        canonical_printed, canonical_scratch,
        "expected pwd inside the per-call cwd, got {}",
        printed,
    );

    // After the call, the kernel's persistent cwd is restored.
    let restored = kernel.cwd().await;
    assert_eq!(
        restored, original_cwd,
        "kernel cwd should be restored after per-call override; was {:?}",
        restored,
    );
}

// ════════════════════════════════════════════════════════════════════════════
// 16. Successful command under a timeout that doesn't fire
// ════════════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn timeout_does_not_fire_when_command_finishes_first() {
    let kernel = kernel_for_test();
    let result = kernel
        .execute_with_options("echo done", ExecuteOptions::new().with_timeout(Duration::from_secs(30)))
        .await
        .expect("execute");
    assert!(result.ok(), "expected ok, got {}", result.code);
    assert_eq!(result.text_out().trim(), "done");
}
