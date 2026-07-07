//! PTY-based integration tests for interactive job control.
//!
//! These tests spawn kaish in a real pseudo-terminal and exercise
//! Ctrl-Z (SIGTSTP), fg, bg, kill, and jobs.
//!
//! Only runs on Unix. Each test gets its own PTY + child process.

#![cfg(unix)]
#![allow(unsafe_code)]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::{Read, Write};
use std::os::fd::{AsRawFd, FromRawFd, IntoRawFd};
use std::process::Command;
use std::time::{Duration, Instant};

use nix::libc;
use nix::pty::openpty;
use nix::sys::termios;

/// Kaish binary path (set by cargo).
fn kaish_bin() -> String {
    env!("CARGO_BIN_EXE_kaish").to_string()
}

/// A PTY-backed kaish session for testing.
struct PtySession {
    master: std::fs::File,
    child: std::process::Child,
}

impl PtySession {
    /// Spawn kaish in a PTY.
    fn new() -> Self {
        let pty = openpty(None, None).expect("openpty failed");

        // Configure PTY: disable echo but keep signal generation (ISIG)
        let mut attrs = termios::tcgetattr(&pty.slave).expect("tcgetattr");
        attrs.local_flags.remove(termios::LocalFlags::ECHO);
        // Ensure ISIG is on so Ctrl-Z/Ctrl-C generate signals
        attrs.local_flags.insert(termios::LocalFlags::ISIG);
        termios::tcsetattr(&pty.slave, termios::SetArg::TCSANOW, &attrs)
            .expect("tcsetattr");

        let slave_fd = pty.slave.as_raw_fd();

        // SAFETY: We're duplicating file descriptors for the child process
        // and setting up a new session with the PTY as controlling terminal.
        let child = unsafe {
            use std::os::unix::process::CommandExt;
            let mut cmd = Command::new(kaish_bin());
            cmd.stdin(std::process::Stdio::from_raw_fd(libc::dup(slave_fd)))
                .stdout(std::process::Stdio::from_raw_fd(libc::dup(slave_fd)))
                .stderr(std::process::Stdio::from_raw_fd(libc::dup(slave_fd)));

            // Create a new session and make the slave PTY the controlling terminal
            cmd.pre_exec(move || {
                // Create new session (detach from parent's terminal)
                libc::setsid();
                // Make this PTY the controlling terminal (TIOCSCTTY)
                libc::ioctl(0, libc::TIOCSCTTY, 0);
                Ok(())
            });

            cmd.spawn().expect("failed to spawn kaish")
        };

        // Close slave fd in parent — child owns its copies
        drop(pty.slave);

        // Make master non-blocking for reads
        // SAFETY: fcntl with F_GETFL/F_SETFL on a valid fd is safe
        let master_fd = pty.master.as_raw_fd();
        unsafe {
            let flags = libc::fcntl(master_fd, libc::F_GETFL);
            libc::fcntl(master_fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }

        // SAFETY: we're converting the OwnedFd to a raw fd and wrapping in File
        let raw_fd = pty.master.into_raw_fd();
        let master = unsafe { std::fs::File::from_raw_fd(raw_fd) };

        let mut session = PtySession { master, child };

        // Wait for initial prompt
        session.wait_for("会sh> ", Duration::from_secs(5))
            .expect("never got initial prompt");

        session
    }

    /// Send a line of input (appends newline).
    fn send_line(&mut self, line: &str) {
        writeln!(self.master, "{}", line).expect("write to pty failed");
    }

    /// Send raw bytes (for control characters).
    fn send_bytes(&mut self, bytes: &[u8]) {
        self.master.write_all(bytes).expect("write to pty failed");
    }

    /// Send Ctrl-Z (SIGTSTP).
    fn send_ctrl_z(&mut self) {
        self.send_bytes(&[0x1a]); // ASCII SUB = Ctrl-Z
    }

    /// Send Ctrl-C (SIGINT).
    fn send_ctrl_c(&mut self) {
        self.send_bytes(&[0x03]); // ASCII ETX = Ctrl-C
    }

    /// Read all available output from the PTY.
    fn read_available(&mut self) -> String {
        let mut buf = [0u8; 4096];
        let mut output = String::new();
        loop {
            match self.master.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => output.push_str(&String::from_utf8_lossy(&buf[..n])),
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => break,
                Err(e) => panic!("read error: {}", e),
            }
        }
        output
    }

    /// Wait until output contains the expected string, or timeout.
    fn wait_for(&mut self, expected: &str, timeout: Duration) -> Result<String, String> {
        let start = Instant::now();
        let mut accumulated = String::new();

        while start.elapsed() < timeout {
            let chunk = self.read_available();
            if !chunk.is_empty() {
                accumulated.push_str(&chunk);
                if accumulated.contains(expected) {
                    return Ok(accumulated);
                }
            }
            std::thread::sleep(Duration::from_millis(50));
        }

        Err(format!(
            "timeout waiting for {:?} in output:\n---\n{}\n---",
            expected, accumulated
        ))
    }

    /// Send a command, wait for the next prompt, return everything in between.
    fn run_command(&mut self, cmd: &str) -> String {
        self.send_line(cmd);
        // Wait for the next prompt
        match self.wait_for("会sh> ", Duration::from_secs(5)) {
            Ok(output) => output,
            Err(e) => panic!("command {:?} failed: {}", cmd, e),
        }
    }
}

impl Drop for PtySession {
    fn drop(&mut self) {
        // Try graceful exit first
        let _ = writeln!(self.master, "exit");
        std::thread::sleep(Duration::from_millis(100));

        // Force kill if still running
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

// ── Tests ──────────────────────────────────────────────────────────────

#[test]
fn test_ctrl_z_stops_foreground_process() {
    let mut session = PtySession::new();

    // Start a long-running process
    session.send_line("sh -c 'sleep 60'");
    std::thread::sleep(Duration::from_millis(300));

    // Send Ctrl-Z
    session.send_ctrl_z();

    // Should see "Stopped" and get the prompt back
    let output = session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("should get prompt back after Ctrl-Z");

    assert!(
        output.contains("Stopped"),
        "expected 'Stopped' in output, got:\n{}",
        output
    );
}

#[test]
fn test_fg_resumes_stopped_job() {
    let mut session = PtySession::new();

    // Start and stop a process
    session.send_line("sh -c 'sleep 60'");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_z();
    session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after Ctrl-Z");

    // Jobs should show it as stopped
    let output = session.run_command("jobs");
    assert!(
        output.contains("Stopped"),
        "jobs should show Stopped, got:\n{}",
        output
    );

    // Resume with fg, then Ctrl-C to kill it
    session.send_line("fg");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_c();

    session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after fg + Ctrl-C");

    // Job should be gone now
    let output = session.run_command("jobs");
    assert!(
        output.contains("no jobs"),
        "jobs should be empty after fg + Ctrl-C, got:\n{}",
        output
    );
}

#[test]
fn test_fg_with_percent_jobspec() {
    // GH #126 part A: `fg %1` (the POSIX jobspec form) must be accepted, not
    // rejected with "invalid job id: %1".
    let mut session = PtySession::new();

    session.send_line("sh -c 'sleep 60'");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_z();
    session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after Ctrl-Z");

    // Resume with fg "%1", then Ctrl-C to kill it. The output captured by the
    // next wait_for includes whatever `fg "%1"` printed immediately (an error,
    // pre-fix) as well as anything printed after Ctrl-C.
    session.send_line("fg \"%1\"");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_c();

    let output = session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after fg \"%1\" + Ctrl-C");
    assert!(
        !output.contains("invalid job id"),
        "fg \"%1\" should be accepted as a jobspec, got:\n{}",
        output
    );

    // Job should be gone now — this also fails pre-fix, since a rejected
    // `fg "%1"` never touches the stopped job at all.
    let output = session.run_command("jobs");
    assert!(
        output.contains("no jobs"),
        "jobs should be empty after fg \"%1\" + Ctrl-C, got:\n{}",
        output
    );
}

#[test]
fn test_bg_resumes_in_background() {
    let mut session = PtySession::new();

    // Start and stop a process
    session.send_line("sh -c 'sleep 60'");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_z();
    session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after Ctrl-Z");

    // Resume in background
    let output = session.run_command("bg");
    assert!(
        output.contains("&"),
        "bg should print job with &, got:\n{}",
        output
    );

    // Kill it to clean up
    session.run_command("kill \"%1\"");
}

#[test]
fn test_bg_with_percent_jobspec() {
    // GH #126 part A: `bg %1` (the POSIX jobspec form, not just a bare job
    // number) must be accepted — `kill`/`wait` already strip the leading `%`.
    let mut session = PtySession::new();

    session.send_line("sh -c 'sleep 60'");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_z();
    session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after Ctrl-Z");

    let output = session.run_command("bg \"%1\"");
    assert!(
        !output.contains("invalid job id"),
        "bg \"%1\" should be accepted as a jobspec, got:\n{}",
        output
    );
    assert!(
        output.contains("&"),
        "bg \"%1\" should print job with &, got:\n{}",
        output
    );

    // Kill it to clean up
    session.run_command("kill \"%1\"");
}

#[test]
fn test_kill_terminates_stopped_job() {
    let mut session = PtySession::new();

    // Start and stop a process
    session.send_line("sh -c 'sleep 60'");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_z();
    session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after Ctrl-Z");

    // Kill the stopped job
    session.run_command("kill \"%1\"");

    // Jobs should be empty
    let output = session.run_command("jobs");
    assert!(
        output.contains("no jobs"),
        "jobs should be empty after kill, got:\n{}",
        output
    );
}

#[test]
fn test_multiple_stopped_jobs() {
    let mut session = PtySession::new();

    // Stop two processes
    session.send_line("sh -c 'sleep 61'");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_z();
    session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after first Ctrl-Z");

    session.send_line("sh -c 'sleep 62'");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_z();
    session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after second Ctrl-Z");

    // Jobs should show both
    let output = session.run_command("jobs");
    assert!(
        output.contains("61") && output.contains("62"),
        "jobs should show both stopped processes, got:\n{}",
        output
    );

    // fg without arg should resume most recent (sleep 62)
    session.send_line("fg");
    std::thread::sleep(Duration::from_millis(300));
    session.send_ctrl_c();
    session
        .wait_for("会sh> ", Duration::from_secs(3))
        .expect("prompt after fg + Ctrl-C");

    // Should still have sleep 61
    let output = session.run_command("jobs");
    assert!(
        output.contains("61"),
        "should still have sleep 61, got:\n{}",
        output
    );

    // Clean up
    session.run_command("kill \"%1\"");
}
