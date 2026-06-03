//! Process file descriptors (Linux pidfd) for race-free child kill.
//!
//! `kill(pid, sig)` sends a signal to whatever process currently has that PID.
//! After the kernel reaps the original child, the PID may be reused by an
//! unrelated process — so a delayed `kill()` from our cancel/timeout path can
//! signal the wrong target. Linux 5.3+ provides `pidfd_open` to obtain a
//! generation-bound handle and `pidfd_send_signal` to deliver signals to it,
//! eliminating the race for the direct child.
//!
//! Process-group kills (`killpg`) have no pidfd-equivalent; grandchildren
//! still go through PID-based delivery and retain the (small) reuse window.
//!
//! On non-Linux unix targets, [`KillTarget`] degrades gracefully to PID-based
//! `kill()` so the call sites stay portable.

#![cfg(all(unix, feature = "subprocess"))]

#[cfg(target_os = "linux")]
use std::os::fd::{AsRawFd, FromRawFd, OwnedFd};

use nix::sys::signal::Signal;
use nix::unistd::Pid;

/// A `pidfd` handle (Linux ≥ 5.3). Internally opaque; use [`KillTarget`].
#[cfg(target_os = "linux")]
struct Pidfd(OwnedFd);

#[cfg(target_os = "linux")]
impl Pidfd {
    /// Open a pidfd for `pid`. Returns `Err` on pre-5.3 kernels (ENOSYS),
    /// for permission errors (EPERM), or if the process is already gone
    /// (ESRCH).
    fn open(pid: u32) -> std::io::Result<Self> {
        // SAFETY: pidfd_open(pid, flags) is a well-defined syscall that
        // returns a new fd or -1/errno. We pass flags = 0 (no PIDFD_NONBLOCK)
        // and a positive PID. The returned fd is exclusively ours.
        // libc::syscall is variadic — pass each arg as c_long so the C
        // variadic ABI promotes consistently across architectures.
        #[allow(unsafe_code)]
        let res = unsafe {
            nix::libc::syscall(
                nix::libc::SYS_pidfd_open,
                pid as nix::libc::c_long,
                0 as nix::libc::c_long,
            )
        };
        if res < 0 {
            return Err(std::io::Error::last_os_error());
        }
        // FDs fit in c_int (~10^6 max on Linux), so the i32 cast is safe.
        // SAFETY: a non-negative syscall return is a fresh fd this process
        // owns. From_raw_fd takes ownership; OwnedFd will close on drop.
        #[allow(unsafe_code)]
        let fd = unsafe { OwnedFd::from_raw_fd(res as i32) };
        Ok(Self(fd))
    }

    fn send_signal(&self, sig: Signal) -> std::io::Result<()> {
        // SAFETY: pidfd_send_signal(fd, sig, info=NULL, flags=0) is a
        // well-defined syscall; passing NULL info synthesises a default
        // siginfo_t. The fd outlives the call (held in &self). All
        // variadic args passed as c_long for portable ABI.
        #[allow(unsafe_code)]
        let res = unsafe {
            nix::libc::syscall(
                nix::libc::SYS_pidfd_send_signal,
                self.0.as_raw_fd() as nix::libc::c_long,
                sig as nix::libc::c_int as nix::libc::c_long,
                std::ptr::null::<nix::libc::siginfo_t>(),
                0 as nix::libc::c_long,
            )
        };
        if res < 0 {
            Err(std::io::Error::last_os_error())
        } else {
            Ok(())
        }
    }
}

/// A bundle of identifiers for killing a child process.
///
/// On Linux, holds a pidfd for race-free direct-child signalling. On other
/// unix targets, just the raw PID — kills go through `kill(pid, sig)`.
/// Process-group signals always use the PID as PGID (we set `pgid = pid` in
/// the child via `setpgid` at spawn) and accept the killpg PID-reuse window.
pub struct KillTarget {
    pid: Pid,
    #[cfg(target_os = "linux")]
    pidfd: Option<Pidfd>,
}

impl KillTarget {
    /// Build a kill target from a freshly spawned `tokio::process::Child`.
    ///
    /// Returns `None` if the child has no PID (already reaped). On Linux,
    /// attempts to open a pidfd; if that fails (old kernel, permission
    /// denied), the target falls back to PID-based kill — the caller still
    /// gets a usable target, just without the reuse-race protection.
    pub fn from_child(child: &tokio::process::Child) -> Option<Self> {
        let pid_raw = child.id()?;
        Some(Self {
            pid: Pid::from_raw(pid_raw as i32),
            #[cfg(target_os = "linux")]
            pidfd: Pidfd::open(pid_raw).ok(),
        })
    }

    /// Build a target from a raw PID (the JC watcher uses this — it can't
    /// borrow the original target across the spawn boundary, so it re-opens).
    /// On Linux, a fresh pidfd is opened; if it fails (e.g. the child was
    /// reaped between spawn and reopen, leaving the PID free for reuse),
    /// returns a target that falls back to PID-based kill — best-effort.
    pub fn from_pid(pid: Pid) -> Self {
        Self {
            pid,
            #[cfg(target_os = "linux")]
            pidfd: {
                let raw = pid.as_raw();
                if raw > 0 {
                    Pidfd::open(raw as u32).ok()
                } else {
                    None
                }
            },
        }
    }

    /// Send `sig` to the **direct child**. On Linux uses pidfd if available
    /// (immune to PID reuse), otherwise falls back to `kill(pid, sig)`.
    pub fn signal(&self, sig: Signal) {
        #[cfg(target_os = "linux")]
        if let Some(pfd) = &self.pidfd {
            let _ = pfd.send_signal(sig);
            return;
        }
        let _ = nix::sys::signal::kill(self.pid, sig);
    }

    /// Send `sig` to the child's **process group** (PGID == child PID since
    /// we `setpgid(0, 0)` in the child's `pre_exec`). No pidfd-equivalent
    /// for PGIDs; this retains the small reuse window for grandchildren.
    pub fn signal_pg(&self, sig: Signal) {
        let _ = nix::sys::signal::killpg(self.pid, sig);
    }

    /// Raw PID, for paths that still need it (logging, JC waitpid, etc.).
    pub fn pid(&self) -> Pid {
        self.pid
    }
}
