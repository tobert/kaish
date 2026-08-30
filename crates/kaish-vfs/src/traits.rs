//! Core VFS traits and types.

use async_trait::async_trait;
use std::io;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

// DirEntry and DirEntryKind live in kaish-types.
pub use kaish_types::{DirEntry, DirEntryKind, EffectiveAccess, PathAccess, ReadRange};

/// Abstract filesystem interface.
///
/// All operations use paths relative to the filesystem root.
/// For example, if a `LocalFs` is rooted at `/home/amy/project`,
/// then `read("src/main.rs")` reads `/home/amy/project/src/main.rs`.
#[async_trait]
pub trait Filesystem: Send + Sync {
    /// Read the entire contents of a file.
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>>;

    /// Read a (possibly partial) slice of a file.
    ///
    /// The default reads the whole file and slices in memory, which is correct
    /// for any finite backend. Backends that cannot answer a whole-file read —
    /// notably synthetic infinite devices like `/dev/zero`, where reading
    /// "everything" is unbounded — override this to honour the requested byte
    /// count directly and to reject a `None` range loudly rather than hang.
    async fn read_range(&self, path: &Path, range: Option<ReadRange>) -> io::Result<Vec<u8>> {
        let content = self.read(path).await?;
        Ok(match range {
            Some(r) => r.apply(&content),
            None => content,
        })
    }

    /// Write data to a file, creating it if it doesn't exist.
    ///
    /// Returns `Err` if the filesystem is read-only.
    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()>;

    /// Append data to a file, creating it if it doesn't exist.
    ///
    /// The default composes `read` (treating a missing file as empty) with
    /// `write` of the concatenation, which is correct for any backend but
    /// costs a read permission the caller may not have and is not atomic —
    /// a writer landing between the read and the write is silently
    /// overwritten. Backends that can answer a true `O_APPEND`-style append
    /// — no read, one atomic write — override this to grant it. Backends
    /// that must materialize state on first write (a copy-on-write overlay
    /// snapshotting its base) should keep the default: it routes through
    /// `write`, so materialization still happens correctly.
    ///
    /// Returns `Err` if the filesystem is read-only.
    async fn append(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        let mut existing = match self.read(path).await {
            Ok(content) => content,
            Err(e) if e.kind() == io::ErrorKind::NotFound => Vec::new(),
            Err(e) => return Err(e),
        };
        existing.extend_from_slice(data);
        self.write(path, &existing).await
    }

    /// List entries in a directory.
    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>>;

    /// Get metadata for a file or directory, following symlinks.
    ///
    /// A dangling link is `NotFound`. Use `lstat` to see the link itself.
    async fn stat(&self, path: &Path) -> io::Result<DirEntry>;

    /// Create a directory (and parent directories if needed).
    ///
    /// Returns `Err` if the filesystem is read-only.
    async fn mkdir(&self, path: &Path) -> io::Result<()>;

    /// Remove a file, empty directory, or symlink.
    ///
    /// The final component is never followed: removing a symlink unlinks the
    /// link and leaves its target untouched, even when the target is a
    /// directory.
    ///
    /// Returns `Err` if the filesystem is read-only.
    async fn remove(&self, path: &Path) -> io::Result<()>;

    /// Set the modification time of an existing path.
    ///
    /// The default errors with `Unsupported`. Writable filesystems that track
    /// timestamps override this; read-only mounts reject. There is deliberately
    /// **no silent no-op** — a `touch` that cannot record the time must say so
    /// rather than report success it didn't deliver.
    async fn set_mtime(&self, path: &Path, mtime: SystemTime) -> io::Result<()> {
        let _ = mtime;
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            format!("set_mtime not supported for {}", path.display()),
        ))
    }

    /// Returns true if this filesystem is read-only.
    fn read_only(&self) -> bool;

    /// What the kernel can do with one path on this filesystem.
    ///
    /// The query behind `test -r`, `test -w`, and `test -x`. Neither
    /// [`Filesystem::read_only`] nor `DirEntry.permissions` answers on its
    /// own — `MemoryFs` (writable) and `JobFs` (read-only) both report
    /// `permissions: None`, and a `LocalFs::read_only` wrapper over an
    /// OS-writable directory reports the write bit set. [`PathAccess::resolve`]
    /// is where the two combine.
    ///
    /// The default is right for a filesystem that is uniformly read-only or
    /// uniformly writable. `VfsRouter` overrides it to ask the mount that owns
    /// the path.
    ///
    /// A backend that can be written must report a mode; an absent one is read
    /// as read-only, and nothing checks that for you. See `docs/EMBEDDING.md`,
    /// "Reporting file permissions".
    ///
    /// `OverlayFs` keeps the default and inherits its one inaccuracy: writes
    /// always land in the upper, so a lower file whose mode clears `0o222`
    /// reports unwritable while copy-up would write it.
    ///
    /// Errors exactly as `stat` does: a path that does not exist is an error,
    /// not a `PathAccess` of all-false.
    async fn path_access(&self, path: &Path) -> io::Result<PathAccess> {
        let entry = self.stat(path).await?;
        Ok(PathAccess::resolve(entry.permissions, self.read_only()))
    }

    /// Memory-resident content bytes this filesystem is holding, if it
    /// tracks them.
    ///
    /// Memory-backed filesystems (`MemoryFs`, `OverlayFs` and its base
    /// snapshots) keep an exact net counter — an overwrite charges the
    /// delta, a remove credits — and return `Some`. Disk-backed filesystems
    /// keep the default `None`: disk residency is the host's concern (page
    /// cache, `df`); this counter is about RAM. Counts file content only,
    /// not directory/symlink metadata. Feeds per-mount introspection and
    /// eviction decisions.
    fn resident_bytes(&self) -> Option<u64> {
        None
    }

    /// Check if a path exists, following symlinks.
    ///
    /// A dangling link does not exist, and any error reads as `false`. Ask
    /// `lstat` when the question is whether a link is present.
    async fn exists(&self, path: &Path) -> bool {
        self.stat(path).await.is_ok()
    }

    /// Rename (move) a file, directory, or symlink.
    ///
    /// Neither path follows its final component: a symlink source is moved as
    /// a link, and a symlink at the destination is replaced, never written
    /// through to its target.
    ///
    /// This is an atomic operation when source and destination are on the same
    /// filesystem. The default implementation is remove-destination, copy,
    /// delete, which is not atomic and does not move directories.
    ///
    /// Returns `Err` if the filesystem is read-only.
    async fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        let entry = self.lstat(from).await?;
        if entry.is_dir() {
            return Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "rename directories not supported by this filesystem",
            ));
        }
        // Clear the destination first: `write` would follow a link left there.
        match self.remove(to).await {
            Ok(()) => {}
            Err(error) if error.kind() == io::ErrorKind::NotFound => {}
            Err(error) => return Err(error),
        }
        if entry.is_symlink() {
            let target = self.read_link(from).await?;
            self.symlink(&target, to).await?;
        } else {
            let data = self.read(from).await?;
            self.write(to, &data).await?;
        }
        self.remove(from).await?;
        Ok(())
    }

    /// Get the real filesystem path for a VFS path.
    ///
    /// Returns `Some(path)` for backends backed by the real filesystem (like LocalFs),
    /// or `None` for virtual backends (like MemoryFs).
    ///
    /// This is needed for tools like `git` that must use real paths with external libraries.
    fn real_path(&self, path: &Path) -> Option<PathBuf> {
        let _ = path;
        None
    }

    /// Read the target of a symbolic link without following it.
    ///
    /// Returns the path the symlink points to. Use `stat` to follow symlinks.
    async fn read_link(&self, path: &Path) -> io::Result<PathBuf> {
        let _ = path;
        Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "symlinks not supported by this filesystem",
        ))
    }

    /// Create a symbolic link.
    ///
    /// Creates a symlink at `link` pointing to `target`. The target is stored
    /// verbatim; a relative target resolves from the link's directory, as in
    /// `readlink`. `link` itself is never followed: an existing path there is
    /// `AlreadyExists`.
    async fn symlink(&self, target: &Path, link: &Path) -> io::Result<()> {
        let _ = (target, link);
        Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "symlinks not supported by this filesystem",
        ))
    }

    /// Get metadata for a path without following its final symlink.
    ///
    /// Unlike `stat`, this returns metadata about the symlink itself,
    /// not the target it points to. A backend that supports symlinks must
    /// override this: the default aliases `stat`, and the conformance suite
    /// fails a backend whose `lstat` follows.
    async fn lstat(&self, path: &Path) -> io::Result<DirEntry> {
        // Default: same as stat (for backends that don't support symlinks)
        self.stat(path).await
    }
}
