//! What the kernel can do with one path.

/// Whether the kernel can read, write, or execute a path.
///
/// A file test needs two facts, and neither one answers alone: the read-only
/// state of the mount that owns the path, and the mode bits that mount
/// reports for the path itself.
///
/// The mode is not enough. A `LocalFs::read_only` wrapper over an
/// OS-writable directory reports real mode bits with the write bit set,
/// because `LocalFs::stat` asks the OS and the OS does not know about the
/// wrapper. Every write to such a path fails; a mode-only check says it
/// would succeed.
///
/// The mount is not enough either. `DevFs::read_only()` is deliberately
/// `false` — refusing writes would break `> /dev/null` — while
/// `mkdir /dev/x` is refused for every caller. Only the mode separates the
/// writable device from the unwritable directory above it.
///
/// [`PathAccess::resolve`] takes both and is the only constructor, so no
/// caller can answer from one of them by accident. The struct is
/// `#[non_exhaustive]`: read the fields, do not construct it by literal.
///
/// # What an absent mode means
///
/// `DirEntry.permissions` is `None` only for a backend that does not model
/// permissions at all. Every backend in this workspace that can be written
/// reports a mode — `LocalFs` on every platform, `MemoryFs`, `DevFs`, and
/// `OverlayFs` through whichever layer holds the path — so the backends
/// still reporting `None` (`BuiltinFs`, `JobFs`) are read-only ones. An
/// absent mode therefore reads as: readable, not writable, not executable.
///
/// That premise is load-bearing. **A backend that is writable and reports
/// `None` will be told its paths are unwritable.** An embedder adding one
/// should report a mode rather than rely on a default here.
/// What the operating system says *this process* may do with a path.
///
/// The answer to `faccessat(..., AT_EACCESS)` — an access check against the
/// effective uid and gid, which is the same primitive `bash`'s `test -w` uses.
///
/// This is not the same question as the mode bits. `0o222` means "some
/// principal may write"; a root-owned `0o644` file has it set for a process
/// that cannot write a byte. Only the OS knows the process's identity, so
/// only the OS can answer.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EffectiveAccess {
    /// The process may read the path.
    pub read: bool,
    /// The process may write the path.
    pub write: bool,
    /// The process may execute the path, or search it if it is a directory.
    pub execute: bool,
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PathAccess {
    /// The path's contents can be read. A read-only mount is readable.
    pub readable: bool,
    /// The path can be written. Needs both facts to agree: false whenever the
    /// owning mount is read-only, whatever the mode says, and false whenever
    /// the mode clears `0o222`, whatever the mount says.
    pub writable: bool,
    /// The path can be executed, or — on a directory — searched. False when
    /// the mount reports no mode.
    pub executable: bool,
}

impl PathAccess {
    /// Combine a mount's read-only state with the mode bits it reports for
    /// one path.
    ///
    /// `mode` is `DirEntry.permissions` — `None` when the mount does not
    /// model Unix modes. `mount_read_only` is the `read_only()` of the mount
    /// that actually owns the path, not of the whole router.
    ///
    /// The three answers treat an absent mode differently, because the three
    /// questions are different:
    ///
    /// - `readable` — a backend that does not model permissions does not
    ///   restrict reads, and read-only mounts read. So: readable.
    /// - `writable` — every writable backend reports a mode, so an absent one
    ///   means read-only. Both facts must agree for a yes: a read-only mount
    ///   is never writable, and a writable mount still honors a mode that
    ///   clears `0o222`.
    /// - `executable` — an absent mode means there is nothing here to run.
    ///   Read-only-ness contributes nothing; it is about writes.
    ///
    /// On a directory, `0o111` means searchable, which is the POSIX meaning
    /// and what `test -x DIR` should answer.
    pub fn resolve(mode: Option<u32>, mount_read_only: bool) -> Self {
        Self {
            readable: mode.is_none_or(|p| p & 0o444 != 0),
            writable: !mount_read_only && mode.is_some_and(|p| p & 0o222 != 0),
            executable: mode.is_some_and(|p| p & 0o111 != 0),
        }
    }

    /// Combine the OS's effective-access answer with a mount's read-only
    /// state.
    ///
    /// For a backend whose paths are real OS paths, this is the accurate
    /// constructor and [`PathAccess::resolve`] is not: `resolve` reads mode
    /// bits, and mode bits answer "may some principal do this", while a file
    /// test has to answer "may this process do this". Only `LocalFs` can use
    /// this, because only its paths have an OS identity to check against.
    ///
    /// `mount_read_only` is still ANDed into `writable`, and still has to be:
    /// a `LocalFs::read_only` wrapper is a kaish-level restriction that the
    /// OS cannot see, so the kernel granting write does not settle it. Both
    /// facts, one funnel — the same contract `resolve` keeps.
    pub fn from_effective_access(access: EffectiveAccess, mount_read_only: bool) -> Self {
        Self {
            readable: access.read,
            writable: !mount_read_only && access.write,
            executable: access.execute,
        }
    }

}

#[cfg(test)]
mod tests {
    use super::PathAccess;

    /// A backend that models no permissions reads, and nothing else — the
    /// mount being writable does not make up for an absent mode, because
    /// every writable backend in the workspace reports one.
    #[test]
    fn absent_mode_reads_and_nothing_else() {
        for mount_read_only in [false, true] {
            let access = PathAccess::resolve(None, mount_read_only);
            assert!(access.readable, "read-only is about writes");
            assert!(!access.writable, "absent mode means read-only backend");
            assert!(!access.executable, "nothing here to hand exec(2)");
        }
    }

    /// A directory mode of `0o777` (MemoryFs) is writable and searchable;
    /// `0o555` (the `/dev` directory) is searchable and not writable. The
    /// pair is the whole reason DevFs needed a mode of its own.
    #[test]
    fn directory_modes_separate_searchable_from_writable() {
        let memory_dir = PathAccess::resolve(Some(0o777), false);
        assert!(memory_dir.writable);
        assert!(memory_dir.executable, "0o111 on a directory is searchable");

        let dev_dir = PathAccess::resolve(Some(0o555), false);
        assert!(!dev_dir.writable, "/dev accepts no mkdir");
        assert!(dev_dir.executable);
        assert!(dev_dir.readable);
    }

    /// A writable mount still honours the mode bits it reports.
    #[test]
    fn mode_bits_decide_on_a_writable_mount() {
        assert!(PathAccess::resolve(Some(0o644), false).writable);
        assert!(!PathAccess::resolve(Some(0o444), false).writable);
        assert!(PathAccess::resolve(Some(0o755), false).executable);
        assert!(!PathAccess::resolve(Some(0o644), false).executable);
        assert!(!PathAccess::resolve(Some(0o000), false).readable);
    }

    /// The hazard: a read-only wrapper over an OS-writable file. The mode
    /// says yes and the mount says no; both must agree for a yes.
    #[test]
    fn read_only_mount_overrides_a_writable_mode() {
        let access = PathAccess::resolve(Some(0o755), true);
        assert!(!access.writable, "the mount's read-only state must win");
        assert!(access.readable, "read-only is about writes");
        assert!(access.executable, "read-only says nothing about exec");
    }


}
