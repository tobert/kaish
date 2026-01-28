//! Core VFS traits and types.

use async_trait::async_trait;
use std::io;
use std::path::Path;
use std::time::SystemTime;

/// Metadata about a file or directory.
#[derive(Debug, Clone)]
pub struct Metadata {
    /// True if this is a directory.
    pub is_dir: bool,
    /// True if this is a file.
    pub is_file: bool,
    /// Size in bytes (0 for directories).
    pub size: u64,
    /// Last modification time, if available.
    pub modified: Option<SystemTime>,
}

/// Type of directory entry.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntryType {
    File,
    Directory,
}

/// A directory entry returned by `list`.
#[derive(Debug, Clone)]
pub struct DirEntry {
    /// Name of the entry (not full path).
    pub name: String,
    /// Type of entry.
    pub entry_type: EntryType,
}

/// Abstract filesystem interface.
///
/// All operations use paths relative to the filesystem root.
/// For example, if a `LocalFs` is rooted at `/home/amy/project`,
/// then `read("src/main.rs")` reads `/home/amy/project/src/main.rs`.
#[async_trait]
pub trait Filesystem: Send + Sync {
    /// Read the entire contents of a file.
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>>;

    /// Write data to a file, creating it if it doesn't exist.
    ///
    /// Returns `Err` if the filesystem is read-only.
    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()>;

    /// List entries in a directory.
    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>>;

    /// Get metadata for a file or directory.
    async fn stat(&self, path: &Path) -> io::Result<Metadata>;

    /// Create a directory (and parent directories if needed).
    ///
    /// Returns `Err` if the filesystem is read-only.
    async fn mkdir(&self, path: &Path) -> io::Result<()>;

    /// Remove a file or empty directory.
    ///
    /// Returns `Err` if the filesystem is read-only.
    async fn remove(&self, path: &Path) -> io::Result<()>;

    /// Returns true if this filesystem is read-only.
    fn read_only(&self) -> bool;

    /// Check if a path exists.
    async fn exists(&self, path: &Path) -> bool {
        self.stat(path).await.is_ok()
    }

    /// Get the real filesystem path for a VFS path.
    ///
    /// Returns `Some(path)` for backends backed by the real filesystem (like LocalFs),
    /// or `None` for virtual backends (like MemoryFs).
    ///
    /// This is needed for tools like `git` that must use real paths with external libraries.
    fn real_path(&self, path: &Path) -> Option<std::path::PathBuf> {
        let _ = path;
        None
    }
}
