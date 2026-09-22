//! An external that exits while a background grandchild still holds its
//! pipes leaves the kernel waiting for EOF, as bash's `$(...)` waits. A
//! cancel during that wait must end it: the kernel returns promptly, and the
//! grandchild in the command's process group is signalled.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(target_os = "linux", feature = "subprocess"))]

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;
use std::time::{Duration, Instant};

use kaish_kernel::ast::Value;
use kaish_kernel::scheduler::JobId;
use kaish_kernel::{Kernel, KernelConfig};

/// Well past `kill %N`'s default kill grace (2s), far short of the 60s sleep.
const PROMPT: Duration = Duration::from_secs(8);

fn kernel_at(dir: &Path) -> Arc<Kernel> {
    let mut vars = HashMap::new();
    vars.insert("PATH".to_string(), Value::String(std::env::var("PATH").unwrap_or_default()));
    let config = KernelConfig::repl().with_cwd(dir.to_path_buf()).with_initial_vars(vars);
    Kernel::new(config).expect("kernel").into_arc()
}

fn script(pidfile: &Path) -> String {
    format!("sh -c 'sleep 60 & echo $! > {}; echo hi'", pidfile.display())
}

/// Wait for the grandchild's pid to be written, then for it to be gone.
async fn assert_grandchild_dies(pidfile: &Path) {
    let pid: u32 = std::fs::read_to_string(pidfile).expect("pid file").trim().parse().expect("pid");
    let proc_dir = format!("/proc/{pid}");
    let deadline = Instant::now() + PROMPT;
    while Path::new(&proc_dir).exists() {
        assert!(Instant::now() < deadline, "grandchild {pid} still alive after the cancel");
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
}

#[tokio::test]
async fn a_cancel_ends_the_wait_on_a_pipe_a_grandchild_holds() {
    let tmp = tempfile::tempdir().unwrap();
    let pidfile = tmp.path().join("grandchild.pid");
    let kernel = kernel_at(tmp.path());
    let running = {
        let kernel = kernel.clone();
        let script = script(&pidfile);
        tokio::spawn(async move { kernel.execute(&script).await })
    };
    // `sh` has exited by now; the kernel waits on the pipe the sleep holds.
    tokio::time::sleep(Duration::from_millis(500)).await;
    assert!(!running.is_finished(), "the wait for EOF must hold while nothing is cancelled");
    let cancelled_at = Instant::now();
    kernel.cancel();
    let result = tokio::time::timeout(PROMPT, running)
        .await
        .expect("a cancel must end the wait promptly")
        .expect("task")
        .expect("execute");
    assert!(cancelled_at.elapsed() < PROMPT);
    assert_eq!(result.code, 130, "{result:?}");
    assert_grandchild_dies(&pidfile).await;
}

#[tokio::test]
async fn kill_ends_a_job_waiting_on_a_pipe_a_grandchild_holds() {
    let tmp = tempfile::tempdir().unwrap();
    let pidfile = tmp.path().join("grandchild.pid");
    let kernel = kernel_at(tmp.path());
    kernel.execute(&format!("{} &", script(&pidfile))).await.expect("spawn");
    tokio::time::sleep(Duration::from_millis(500)).await;
    let id = JobId(1);
    assert_eq!(kernel.jobs().get_status_string(id).await.expect("job"), "running");
    kernel.execute("kill %1").await.expect("kill");
    let result = tokio::time::timeout(PROMPT, kernel.jobs().wait(id))
        .await
        .expect("kill must end the job promptly")
        .expect("job result");
    // `kill %N` also signals the job's process groups, so the grandchild may
    // die and close the pipe before the cancel reaches the wait: 0 or 130.
    assert!(matches!(result.code, 0 | 130), "{result:?}");
    assert_grandchild_dies(&pidfile).await;
}

/// A grandchild that left the process group (`setsid`) outlives every group
/// signal, so only the cancel itself can end the wait. It keeps running: the
/// kernel has no handle on it. The job still ends promptly.
#[tokio::test]
async fn kill_ends_a_job_even_when_the_grandchild_left_the_group() {
    let tmp = tempfile::tempdir().unwrap();
    let pidfile = tmp.path().join("grandchild.pid");
    let kernel = kernel_at(tmp.path());
    kernel
        .execute(&format!("sh -c 'setsid sleep 60 & echo $! > {}; echo hi' &", pidfile.display()))
        .await
        .expect("spawn");
    tokio::time::sleep(Duration::from_millis(500)).await;
    let id = JobId(1);
    assert_eq!(kernel.jobs().get_status_string(id).await.expect("job"), "running");
    kernel.execute("kill %1").await.expect("kill");
    let result = tokio::time::timeout(PROMPT, kernel.jobs().wait(id))
        .await
        .expect("kill must end the job promptly")
        .expect("job result");
    assert_eq!(result.code, 130, "{result:?}");
    let pid: i32 = std::fs::read_to_string(&pidfile).expect("pid").trim().parse().expect("pid");
    // Not ours to leave behind.
    let _ = std::process::Command::new("kill").arg("-KILL").arg(pid.to_string()).status();
}
