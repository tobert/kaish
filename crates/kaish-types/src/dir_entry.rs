//! Directory entry types for VFS and backend operations.

use std::path::PathBuf;
use std::time::SystemTime;

/// Kind of directory entry.
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DirEntryKind {
    /// A regular file.
    File,
    Directory,
    Symlink,
    /// A character device, such as `/dev/null` or `/dev/zero`.
    CharDevice,
    BlockDevice,
    /// A named pipe. Opening one for reading blocks until a writer appears.
    Fifo,
    Socket,
}

/// A directory entry — the unified file metadata type.
///
/// Used everywhere: VFS `list()`, `stat()`, `lstat()`, and `KernelBackend` methods.
#[derive(Debug, Clone)]
pub struct DirEntry {
    /// Name of the entry (not full path).
    pub name: String,
    /// Kind of entry.
    pub kind: DirEntryKind,
    /// Size in bytes (0 for directories).
    pub size: u64,
    /// Last modification time, if available.
    pub modified: Option<SystemTime>,
    /// Unix permissions (e.g., 0o644), if available.
    pub permissions: Option<u32>,
    /// For symlinks, the target path.
    pub symlink_target: Option<PathBuf>,
}

impl DirEntry {
    /// Create a new directory entry.
    pub fn directory(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            kind: DirEntryKind::Directory,
            size: 0,
            modified: None,
            permissions: None,
            symlink_target: None,
        }
    }

    /// Create a new file entry.
    pub fn file(name: impl Into<String>, size: u64) -> Self {
        Self {
            name: name.into(),
            kind: DirEntryKind::File,
            size,
            modified: None,
            permissions: None,
            symlink_target: None,
        }
    }

    /// Create a new symlink entry.
    pub fn symlink(name: impl Into<String>, target: impl Into<PathBuf>) -> Self {
        Self {
            name: name.into(),
            kind: DirEntryKind::Symlink,
            size: 0,
            modified: None,
            permissions: None,
            symlink_target: Some(target.into()),
        }
    }

    /// Returns true if this entry is a directory.
    pub fn is_dir(&self) -> bool {
        self.kind == DirEntryKind::Directory
    }

    /// Returns true if this entry is a regular file.
    pub fn is_file(&self) -> bool {
        self.kind == DirEntryKind::File
    }

    /// Returns true if this entry is a symbolic link.
    pub fn is_symlink(&self) -> bool {
        self.kind == DirEntryKind::Symlink
    }

    /// Returns true for a device, FIFO, or socket: an entry whose read may
    /// block or never end.
    pub fn is_special(&self) -> bool {
        matches!(
            self.kind,
            DirEntryKind::CharDevice
                | DirEntryKind::BlockDevice
                | DirEntryKind::Fifo
                | DirEntryKind::Socket
        )
    }
}
