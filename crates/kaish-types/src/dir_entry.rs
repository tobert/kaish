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
    /// Hard link count, if the backend tracks one.
    pub links: Option<u64>,
    /// Allocated size in 512-byte blocks, if available. `ls -l` sums these
    /// for its `total` line.
    pub blocks: Option<u64>,
    /// The owning user, if the backend tracks one.
    pub owner: Option<AccountId>,
    /// The owning group, if the backend tracks one.
    pub group: Option<AccountId>,
}

/// A numeric user or group ID, with its name when the system can resolve it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AccountId {
    /// The numeric ID (uid or gid).
    pub id: u32,
    /// The account name, or `None` when the ID has no entry on this system.
    pub name: Option<String>,
}

impl AccountId {
    /// The name when known, else the numeric ID, as `ls -l` prints it.
    pub fn display_name(&self) -> String {
        match &self.name {
            Some(name) => name.clone(),
            None => self.id.to_string(),
        }
    }
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
            links: None,
            blocks: None,
            owner: None,
            group: None,
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
            links: None,
            blocks: None,
            owner: None,
            group: None,
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
            links: None,
            blocks: None,
            owner: None,
            group: None,
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

#[cfg(test)]
mod tests {
    use super::AccountId;

    #[test]
    fn display_name_is_the_name_else_the_numeric_id() {
        assert_eq!(AccountId { id: 1000, name: Some("amy".into()) }.display_name(), "amy");
        assert_eq!(AccountId { id: 4242, name: None }.display_name(), "4242");
    }
}
