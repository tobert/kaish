//! A hard-killed kaish process must not leave its external children running.
//!
//! `setpgid` + pidfd kills + `kill_on_drop` all need *this* process to still
//! be alive and running code. None of them fire on `kill -9`, a segfault, or
//! an OOM kill, so a `cargo build` started under an embedder that dies that
//! way keeps building forever, invisible. Linux's `PR_SET_PDEATHSIG` is the
//! only mechanism that does not depend on the parent getting to run anything:
//! the OS delivers the signal.
//!
//! **The only honest test is a real one.** This file re-executes its own test
//! binary as a helper, waits for the helper to report the pid of an external
//! command it started, `SIGKILL`s the helper outright — no unwinding, no Drop,
//! no shutdown path — and then asks whether that pid is still alive. The
//! negative case (flag off, child survives) is what proves the positive case
//! is measuring anything at all.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(target_os = "linux", feature = "localfs", feature = "subprocess"))]

use std::collections::HashMap;
use std::io::{BufRead, BufReader};
use std::process::{Child, Command, Stdio};
use std::time::{Duration, Instant};

use kaish_kernel::ast::Value;
use kaish_kernel::scheduler::JobId;
use kaish_kernel::{Kernel, KernelConfig};

/// Env var the helper reads to decide whether to arm the parent-death signal.
const ARM_VAR: &str = "KAISH_TEST_PDEATHSIG_ARMED";

/// Marker the helper prints so the parent test knows which pid to watch.
const PID_MARKER: &str = "KAISH_TEST_EXTERNAL_PID=";

fn helper_kernel(armed: bool) -> Kernel {
    let mut vars = HashMap::new();
    vars.insert(
        "PATH".to_string(),
        Value::String(std::env::var("PATH").unwrap_or_default()),
    );
    let config = KernelConfig::repl()
        .with_initial_vars(vars)
        .with_kill_children_on_parent_death(armed);
    Kernel::new(config).expect("failed to create kernel")
}

/// Whether `pid` still names a live process. `kill(pid, 0)` is the standard
/// probe: `ESRCH` means gone. A zombie still answers, so the caller polls
/// rather than sampling once — the parent's death, the SIGKILL, and the
/// reparenting reaper's `wait` are three separate events.
fn is_alive(pid: u32) -> bool {
    nix::sys::signal::kill(nix::unistd::Pid::from_raw(pid as i32), None).is_ok()
}

/// Owns the helper process and guarantees it is killed and reaped, even if an
/// assertion between spawn and kill panics. Without this a failing test leaks
/// a parked helper *and* the external command it started.
struct Helper(Child);

impl Helper {
    /// `kill -9` the helper and reap it. Idempotent — `Drop` runs it again.
    fn hard_kill(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

impl Drop for Helper {
    fn drop(&mut self) {
        self.hard_kill();
    }
}

/// Run this test binary again as a helper, and read back the pid of the
/// external command it started. Returns the helper process and that pid.
fn spawn_helper(armed: bool) -> (Helper, u32) {
    let exe = std::env::current_exe().expect("test binary path");
    let helper = Command::new(exe)
        .arg("pdeathsig_helper_process")
        .arg("--exact")
        .arg("--ignored")
        .arg("--nocapture")
        .env(ARM_VAR, if armed { "1" } else { "0" })
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .expect("failed to re-exec the test binary as a helper");

    let mut helper = Helper(helper);
    let stdout = helper.0.stdout.take().expect("piped");
    let mut reader = BufReader::new(stdout);
    let deadline = Instant::now() + Duration::from_secs(30);
    let mut line = String::new();
    loop {
        line.clear();
        let n = reader.read_line(&mut line).expect("reading helper stdout");
        assert!(n > 0, "helper exited before reporting a pid");
        if let Some(rest) = line.trim().strip_prefix(PID_MARKER) {
            let pid: u32 = rest.parse().expect("helper printed a non-numeric pid");
            return (helper, pid);
        }
        assert!(Instant::now() < deadline, "helper never reported a pid");
    }
}

/// The positive case: armed, the external command dies with the process that
/// started it — with no code of ours running in between.
#[test]
fn a_hard_killed_parent_takes_its_external_child_with_it() {
    let (mut helper, pid) = spawn_helper(true);
    assert!(is_alive(pid), "the external command should be running before the kill");

    helper.hard_kill();

    let deadline = Instant::now() + Duration::from_secs(10);
    while is_alive(pid) {
        assert!(
            Instant::now() < deadline,
            "pid {pid} outlived the SIGKILLed parent — PR_SET_PDEATHSIG did not fire"
        );
        std::thread::sleep(Duration::from_millis(50));
    }
}

/// The negative case, which is what makes the positive one mean something:
/// with the flag off, the child is orphaned and keeps running. This is the
/// behavior every kaish release before this change had, and still the default.
#[test]
fn without_the_flag_a_hard_killed_parent_orphans_its_external_child() {
    let (mut helper, pid) = spawn_helper(false);
    assert!(is_alive(pid), "the external command should be running before the kill");

    helper.hard_kill();

    // Give the OS the same window the positive case gets. Anything that would
    // have killed the child has had its chance by now.
    std::thread::sleep(Duration::from_millis(500));
    let orphaned = is_alive(pid);

    // Clean up before asserting — a failed assertion must not leak a `sleep`.
    let _ = nix::sys::signal::kill(
        nix::unistd::Pid::from_raw(pid as i32),
        nix::sys::signal::Signal::SIGKILL,
    );

    assert!(
        orphaned,
        "pid {pid} died without the flag — then the positive test proves nothing, \
         because something other than PR_SET_PDEATHSIG is killing the child"
    );
}

/// Not a test: the helper process the two tests above drive. `#[ignore]` keeps
/// it out of a normal run; the parent passes `--ignored --exact` to reach it.
///
/// Starts a real external command in the background, reports its pid, and then
/// parks forever waiting to be killed.
#[tokio::test]
#[ignore = "helper process, re-executed by the pdeathsig tests"]
async fn pdeathsig_helper_process() {
    use std::io::Write;

    let armed = std::env::var(ARM_VAR).as_deref() == Ok("1");
    let kernel = helper_kernel(armed);

    // `/bin/sleep`, not `sleep`: kaish's `sleep` is a builtin and would never
    // spawn an OS process to orphan in the first place.
    kernel.execute("/bin/sleep 300 &").await.expect("spawn failed");

    // The pgid is recorded once the child is spawned, and equals its pid
    // (every external does `setpgid(0, 0)` in pre_exec).
    let deadline = Instant::now() + Duration::from_secs(20);
    let pid = loop {
        if let Some(pid) = kernel.jobs().job_pgids(JobId(1)).await.first().copied() {
            break pid;
        }
        assert!(Instant::now() < deadline, "the external command never registered a pgid");
        tokio::time::sleep(Duration::from_millis(20)).await;
    };

    println!("{PID_MARKER}{pid}");
    std::io::stdout().flush().expect("flush");

    // Park. The parent SIGKILLs this process; nothing here gets to run again,
    // which is the whole point — no Drop, no shutdown, no cancellation.
    loop {
        tokio::time::sleep(Duration::from_secs(60)).await;
    }
}
