//! What the kernel can do with one path.

/// Whether the kernel can read, write, or execute a path.
///
/// A file test needs two facts that neither one answers alone: the read-only
/// state of the mount that owns the path, and the mode bits that mount
/// reports for the path itself.
///
/// Neither fact is sufficient. `DirEntry.permissions` is `None` on MemoryFs
/// (writable), on DevFs (writable — writes discard), on BuiltinFs
/// (read-only) and on JobFs (read-only), so an absent mode carries no
/// information about writability at all. In the other direction a
/// `LocalFs::read_only` wrapper over an OS-writable directory reports real
/// mode bits with the write bit set, because `LocalFs::stat` asks the OS and
/// the OS does not know about the wrapper.
///
/// [`PathAccess::resolve`] is the only way to build a `PathAccess`, and it
/// takes both facts, so a caller cannot answer from one of them by accident.
/// The struct is `#[non_exhaustive]`: read the fields, do not construct it
/// by literal.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct PathAccess {
    /// The path's contents can be read. A read-only mount is readable.
    pub readable: bool,
    /// The path can be written. False whenever the owning mount is read-only,
    /// whatever the mode bits say.
    pub writable: bool,
    /// The path can be executed. False when the mount reports no mode bits —
    /// a mount with nothing to hand exec(2) has no executable path on it.
    pub executable: bool,
}

impl PathAccess {
    /// Combine a mount's read-only state with the mode bits it reports for
    /// one path.
    ///
    /// `mode` is `DirEntry.permissions` — `None` when the mount does not
    /// model Unix modes. `mount_read_only` is that mount's
    /// `Filesystem::read_only()`, for the mount that actually owns the path,
    /// not the whole router.
    ///
    /// The three answers do not treat an absent mode the same way, because
    /// the three questions are not the same question:
    ///
    /// - `readable` — an absent mode means the mount does not restrict reads,
    ///   so it reads. Read-only mounts read.
    /// - `writable` — an absent mode says nothing, so the mount decides. Both
    ///   must agree: a read-only mount is never writable, and a writable
    ///   mount still honours a mode that clears `0o222`.
    /// - `executable` — an absent mode means there is no executable here.
    ///   Read-only-ness is about writes and contributes nothing; a mount that
    ///   reports no modes (MemoryFs, JobFs, BuiltinFs, DevFs) also has no real
    ///   path for exec(2) to open.
    pub fn resolve(mode: Option<u32>, mount_read_only: bool) -> Self {
        Self {
            readable: mode.is_none_or(|p| p & 0o444 != 0),
            writable: !mount_read_only && mode.is_none_or(|p| p & 0o222 != 0),
            executable: mode.is_some_and(|p| p & 0o111 != 0),
        }
    }

    /// Re-answer `writable` from a different layer than the one that answered
    /// `readable` and `executable`.
    ///
    /// Copy-on-write overlays need this: reads resolve against whichever
    /// layer holds the path, but every write lands in the upper layer, so the
    /// upper layer decides writability. A lower file whose mode clears `0o222`
    /// is still writable through copy-up, because `OverlayFs::write` copies
    /// the content up and writes the upper — it never consults the lower's
    /// mode.
    ///
    /// Takes the same pair as [`PathAccess::resolve`], for the write layer.
    pub fn with_write_layer(self, mode: Option<u32>, mount_read_only: bool) -> Self {
        Self {
            writable: Self::resolve(mode, mount_read_only).writable,
            ..self
        }
    }
}

#[cfg(test)]
mod tests {
    use super::PathAccess;

    /// MemoryFs and DevFs: no modes, writable mount.
    #[test]
    fn absent_mode_on_a_writable_mount_is_writable_not_executable() {
        let access = PathAccess::resolve(None, false);
        assert!(access.readable);
        assert!(access.writable);
        assert!(!access.executable);
    }

    /// BuiltinFs and JobFs: no modes, read-only mount. The shipped 0.16 bug
    /// answered `writable` here.
    #[test]
    fn absent_mode_on_a_read_only_mount_is_readable_only() {
        let access = PathAccess::resolve(None, true);
        assert!(access.readable);
        assert!(!access.writable);
        assert!(!access.executable);
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

    /// Copy-up: the lower's mode answers read and exec, the upper answers
    /// write.
    #[test]
    fn write_layer_replaces_only_the_write_answer() {
        let lower = PathAccess::resolve(Some(0o444), false);
        assert!(!lower.writable);
        let overlaid = lower.with_write_layer(None, false);
        assert!(overlaid.writable, "copy-up makes a mode-444 lower writable");
        assert!(overlaid.readable);
        assert_eq!(overlaid.executable, lower.executable);
    }

    /// A read-only upper makes the whole overlay unwritable, whatever the
    /// lower reports.
    #[test]
    fn a_read_only_write_layer_wins() {
        let overlaid = PathAccess::resolve(Some(0o755), false).with_write_layer(Some(0o755), true);
        assert!(!overlaid.writable);
    }
}
