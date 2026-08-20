//! Terminal control and job control for interactive mode.
//!
//! Handles process group management, terminal ownership, and
//! foreground wait with WUNTRACED support for Ctrl-Z (SIGTSTP).
//!
//! All functionality is `#[cfg(unix)]` — non-Unix platforms get stubs.
//!
//! Signal disposition (`sigaction`) requires unsafe per POSIX. This is the only
//! module in kaish that uses unsafe code, and it's limited to well-understood
//! signal-handling patterns that every shell must perform.

#[cfg(unix)]
#[allow(unsafe_code)]
mod unix {
    use std::os::unix::io::BorrowedFd;

    use nix::sys::signal::{self, SigHandler, Signal};
    use nix::sys::wait::{WaitPidFlag, WaitStatus, waitpid};
    use nix::unistd::{self, Pid, tcsetpgrp};

    /// Result of waiting for a foreground process.
    #[derive(Debug)]
    #[non_exhaustive]
    pub enum WaitResult {
        /// Process exited with a status code.
        Exited(i32),
        /// Process was killed by a signal.
        Signaled(i32),
        /// Process was stopped (e.g., SIGTSTP from Ctrl-Z).
        Stopped(Signal),
    }

    /// Terminal state for interactive job control.
    ///
    /// Created once at REPL startup. Manages signal disposition and
    /// terminal foreground process group.
    pub struct TerminalState {
        /// The shell's own process group ID.
        shell_pgid: Pid,
    }

    /// Get a borrowed fd for stdin.
    fn stdin_fd() -> BorrowedFd<'static> {
        // SAFETY: stdin (fd 0) is valid for the lifetime of the process
        // in an interactive shell context where we've verified isatty.
        unsafe { BorrowedFd::borrow_raw(0) }
    }

    /// Set a signal to SIG_IGN.
    fn ignore_signal(sig: Signal) -> nix::Result<()> {
        // SAFETY: SIG_IGN is a well-defined, safe signal disposition.
        // No custom handler code is executed.
        unsafe {
            signal::sigaction(
                sig,
                &signal::SigAction::new(
                    SigHandler::SigIgn,
                    signal::SaFlags::empty(),
                    signal::SigSet::empty(),
                ),
            )?;
        }
        Ok(())
    }

    impl TerminalState {
        /// Initialize terminal state for interactive job control.
        ///
        /// - Puts the shell in its own process group
        /// - Ignores SIGTSTP, SIGTTOU, SIGTTIN (so the shell isn't stopped)
        /// - Takes the terminal foreground
        pub fn init() -> nix::Result<Self> {
            let shell_pid = unistd::getpid();

            // Put the shell in its own process group.
            // This may fail with EPERM if we're already a session leader
            // (e.g., spawned via setsid), which is fine — we're already
            // in our own process group in that case.
            match unistd::setpgid(shell_pid, shell_pid) {
                Ok(()) => {}
                Err(nix::errno::Errno::EPERM) => {
                    // Already session leader or in our own pgid — acceptable
                }
                Err(e) => return Err(e),
            }

            // Ignore SIGTTOU first so tcsetpgrp doesn't stop us
            ignore_signal(Signal::SIGTTOU)?;

            tcsetpgrp(stdin_fd(), shell_pid)?;

            // Ignore the other job-control signals
            ignore_signal(Signal::SIGTSTP)?;
            ignore_signal(Signal::SIGTTIN)?;

            Ok(Self {
                shell_pgid: shell_pid,
            })
        }

        /// Give the terminal foreground to a process group.
        pub fn give_terminal_to(&self, pgid: Pid) -> nix::Result<()> {
            tcsetpgrp(stdin_fd(), pgid)
        }

        /// Reclaim the terminal foreground for the shell.
        pub fn reclaim_terminal(&self) -> nix::Result<()> {
            tcsetpgrp(stdin_fd(), self.shell_pgid)
        }

        /// Wait for a foreground process, handling stop signals (WUNTRACED).
        ///
        /// This blocks the current thread. Call from `block_in_place`.
        pub fn wait_for_foreground(&self, pid: Pid) -> WaitResult {
            loop {
                match waitpid(pid, Some(WaitPidFlag::WUNTRACED)) {
                    Ok(WaitStatus::Exited(_, code)) => {
                        return WaitResult::Exited(code);
                    }
                    Ok(WaitStatus::Signaled(_, sig, _)) => {
                        return WaitResult::Signaled(sig as i32);
                    }
                    Ok(WaitStatus::Stopped(_, sig)) => {
                        return WaitResult::Stopped(sig);
                    }
                    Ok(WaitStatus::Continued(_)) => continue,
                    Ok(_) => continue,
                    Err(nix::errno::Errno::EINTR) => continue,
                    Err(nix::errno::Errno::ECHILD) => {
                        return WaitResult::Exited(0);
                    }
                    Err(e) => {
                        tracing::error!("waitpid failed: {}", e);
                        return WaitResult::Exited(1);
                    }
                }
            }
        }
    }
}

#[cfg(unix)]
pub use unix::{TerminalState, WaitResult};
