//! What the kernel can do with one path.

/// What the operating system says *this process* may do with a path.
///
/// The answer to `faccessat(..., AT_EACCESS)` — an access check against the
/// effective uid and gid, the same primitive `bash`'s `test -w` uses. Mode
/// bits answer a different question: `0o222` means "some principal may
/// write", and a root-owned `0o644` file has it set for a process that
/// cannot write a byte. Only the OS knows the process's identity.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct EffectiveAccess {
    /// The process may read the path.
    pub read: bool,
    /// The process may write the path.
    pub write: bool,
    /// The process may execute the path, or search it if it is a directory.
    pub execute: bool,
}

/// Whether the kernel can read, write, or execute a path.
///
/// A file test needs two facts and neither answers alone. A
/// `LocalFs::read_only` wrapper over an OS-writable directory reports mode
/// bits with the write bit set, because `LocalFs::stat` asks the OS and the
/// OS does not know about the wrapper. `DevFs::read_only()` is `false` so
/// that `> /dev/null` works, while `mkdir /dev/x` is refused for every
/// caller — only the mode separates those two.
///
/// [`PathAccess::resolve`] is the only constructor, so no caller answers
/// from one fact by accident. Read the fields; the struct is
/// `#[non_exhaustive]`.
///
/// An absent `DirEntry.permissions` reads as readable, not writable, not
/// executable. Every writable backend here reports a mode, so `None` means a
/// backend that does not model permissions, and all of those are read-only.
/// **A writable backend reporting `None` will be told its paths are
/// unwritable** — see `docs/EMBEDDING.md`, "Reporting file permissions".
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
    /// `mode` is `DirEntry.permissions`; `mount_read_only` is the
    /// `read_only()` of the mount that owns the path, not of the whole
    /// router.
    ///
    /// An absent mode still reads: a backend that does not model permissions
    /// does not restrict reads. Writes need both facts to agree. Exec needs a
    /// mode — absent means there is nothing here to run. On a directory
    /// `0o111` is searchable, which is what `test -x DIR` answers.
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
    /// The accurate constructor for a backend whose paths are real OS paths,
    /// which is only `LocalFs`. A file test asks "may this process do this",
    /// and [`PathAccess::resolve`]'s mode bits cannot answer that.
    ///
    /// `mount_read_only` is still ANDed into `writable`: a `LocalFs::read_only`
    /// wrapper is a kaish-level restriction the OS cannot see.
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
