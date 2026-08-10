//! The approval prompt in a real terminal (`docs/approval-ledger.md` §C.3).
//!
//! `approval_prompt_tests.rs` covers everything below the keystroke with a
//! scripted answer. These cover the keystroke: kaish in a pseudo-terminal,
//! with the answer typed at the prompt, because two of the properties §C.3
//! promises exist only there — that the question reaches the terminal at all,
//! and that **Ctrl-C at the prompt is input rather than a signal**, which is
//! only true because the line editor holds the terminal in raw mode while it
//! reads the answer.
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

/// A PTY-backed kaish session, spawned with `--gate echo` so every statement
/// naming `echo` asks first.
struct GatedPty {
    master: std::fs::File,
    child: std::process::Child,
    #[allow(dead_code)]
    dir: tempfile::TempDir,
}

impl GatedPty {
    fn new() -> Self {
        let dir = tempfile::Builder::new()
            .prefix("kaish-pty-approval")
            .tempdir()
            .expect("tempdir");
        let pty = openpty(None, None).expect("openpty failed");

        // Echo off (the child's line editor draws its own), signals on — the
        // point of the Ctrl-C case is that raw mode inside `readline` beats
        // ISIG here, so ISIG must genuinely be on.
        let mut attrs = termios::tcgetattr(&pty.slave).expect("tcgetattr");
        attrs.local_flags.remove(termios::LocalFlags::ECHO);
        attrs.local_flags.insert(termios::LocalFlags::ISIG);
        termios::tcsetattr(&pty.slave, termios::SetArg::TCSANOW, &attrs).expect("tcsetattr");

        let slave_fd = pty.slave.as_raw_fd();

        // SAFETY: duplicating fds for the child and giving it the PTY as its
        // controlling terminal — the pattern `pty_job_control.rs` uses.
        let child = unsafe {
            use std::os::unix::process::CommandExt;
            let mut cmd = Command::new(env!("CARGO_BIN_EXE_kaish"));
            cmd.arg("--gate")
                .arg("echo")
                .current_dir(dir.path())
                // No rc file: a developer's own init.kai must not decide
                // whether this test passes.
                .env("KAISH_INIT", dir.path().join("no-such-init.kai"))
                .stdin(std::process::Stdio::from_raw_fd(libc::dup(slave_fd)))
                .stdout(std::process::Stdio::from_raw_fd(libc::dup(slave_fd)))
                .stderr(std::process::Stdio::from_raw_fd(libc::dup(slave_fd)));
            cmd.pre_exec(move || {
                libc::setsid();
                libc::ioctl(0, libc::TIOCSCTTY, 0);
                Ok(())
            });
            cmd.spawn().expect("failed to spawn kaish")
        };

        drop(pty.slave);

        let master_fd = pty.master.as_raw_fd();
        // SAFETY: fcntl F_GETFL/F_SETFL on a valid fd.
        unsafe {
            let flags = libc::fcntl(master_fd, libc::F_GETFL);
            libc::fcntl(master_fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }
        // SAFETY: taking ownership of the master fd as a File.
        let master = unsafe { std::fs::File::from_raw_fd(pty.master.into_raw_fd()) };

        let mut session = GatedPty { master, child, dir };
        session
            .wait_for("会sh> ", Duration::from_secs(10))
            .expect("never got the initial prompt");
        session
    }

    fn send_line(&mut self, line: &str) {
        writeln!(self.master, "{line}").expect("write to pty failed");
    }

    fn send_ctrl_c(&mut self) {
        self.master.write_all(&[0x03]).expect("write to pty failed");
    }

    fn read_available(&mut self) -> String {
        let mut buf = [0u8; 4096];
        let mut output = String::new();
        loop {
            match self.master.read(&mut buf) {
                Ok(0) => break,
                Ok(n) => output.push_str(&String::from_utf8_lossy(&buf[..n])),
                Err(e) if e.kind() == std::io::ErrorKind::WouldBlock => break,
                Err(e) => panic!("read error: {e}"),
            }
        }
        output
    }

    fn wait_for(&mut self, expected: &str, timeout: Duration) -> Result<String, String> {
        let start = Instant::now();
        let mut accumulated = String::new();
        while start.elapsed() < timeout {
            accumulated.push_str(&self.read_available());
            if accumulated.contains(expected) {
                return Ok(accumulated);
            }
            std::thread::sleep(Duration::from_millis(50));
        }
        Err(format!(
            "timeout waiting for {expected:?} in output:\n---\n{accumulated}\n---"
        ))
    }
}

impl Drop for GatedPty {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

/// §H acceptance test 1, in a terminal: the prompt appears, `y` grants, and
/// the held statement completes.
#[test]
fn a_tty_session_prompts_grants_and_the_held_statement_completes() {
    let mut session = GatedPty::new();

    session.send_line("echo held-then-run");
    let prompt = session
        .wait_for("grant?", Duration::from_secs(10))
        .expect("the gate must reach the terminal");
    assert!(
        prompt.contains("approval required"),
        "the request renders before the question: {prompt}"
    );
    assert!(
        prompt.contains("cmd.execute"),
        "the operation is named: {prompt}"
    );

    session.send_line("y");
    let after = session
        .wait_for("held-then-run", Duration::from_secs(10))
        .expect("the approved statement must run");
    assert!(
        !after.contains("pending approval"),
        "a granted statement must not report itself pending: {after}"
    );
}

/// §H acceptance test 3, in a terminal: Ctrl-C at the prompt denies, and the
/// session survives to answer the next line — the keystroke is input, not a
/// signal that tore something down.
#[test]
fn ctrl_c_at_the_prompt_denies_and_the_session_lives() {
    let mut session = GatedPty::new();

    session.send_line("echo never-runs");
    session
        .wait_for("grant?", Duration::from_secs(10))
        .expect("the gate must reach the terminal");

    session.send_ctrl_c();
    let denied = session
        .wait_for("denied", Duration::from_secs(10))
        .expect("Ctrl-C at the prompt must deny");
    assert!(
        !denied.contains("never-runs\r\n"),
        "a denied statement must not run: {denied}"
    );

    // The request was closed, not left live, and the REPL is still reading.
    session.send_line("approvals list");
    let listed = session
        .wait_for("no pending approvals", Duration::from_secs(10))
        .expect("the denied request must leave nothing live");
    assert!(listed.contains("no pending approvals"), "{listed}");
}
