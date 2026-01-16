//! Local filesystem backend.
//!
//! Provides access to real filesystem paths, with optional read-only mode.

use super::traits::{DirEntry, EntryType, Filesystem, Metadata};
use async_trait::async_trait;
use std::io;
use std::path::{Path, PathBuf};
use tokio::fs;

/// Local filesystem backend.
///
/// All operations are relative to `root`. For example, if `root` is
/// `/home/amy/project`, then `read("src/main.rs")` reads
/// `/home/amy/project/src/main.rs`.
#[derive(Debug, Clone)]
pub struct LocalFs {
    root: PathBuf,
    read_only: bool,
}

impl LocalFs {
    /// Create a new local filesystem rooted at the given path.
    ///
    /// The path must exist and be a directory.
    pub fn new(root: impl Into<PathBuf>) -> Self {
        Self {
            root: root.into(),
            read_only: false,
        }
    }

    /// Create a read-only local filesystem.
    pub fn read_only(root: impl Into<PathBuf>) -> Self {
        Self {
            root: root.into(),
            read_only: true,
        }
    }

    /// Set whether this filesystem is read-only.
    pub fn set_read_only(&mut self, read_only: bool) {
        self.read_only = read_only;
    }

    /// Get the root path.
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// Resolve a relative path to an absolute path within the root.
    ///
    /// Returns an error if the path escapes the root (via `..`).
    fn resolve(&self, path: &Path) -> io::Result<PathBuf> {
        // Strip leading slash if present
        let path = path.strip_prefix("/").unwrap_or(path);

        // Join with root
        let full = self.root.join(path);

        // Canonicalize to resolve symlinks and ..
        // For non-existent paths, we need to check parent
        let canonical = if full.exists() {
            full.canonicalize()?
        } else {
            // For new files, canonicalize parent and append filename
            let parent = full.parent().ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "invalid path")
            })?;
            let filename = full.file_name().ok_or_else(|| {
                io::Error::new(io::ErrorKind::InvalidInput, "invalid path")
            })?;

            if parent.exists() {
                parent.canonicalize()?.join(filename)
            } else {
                // Parent doesn't exist, just use the path as-is
                // (will fail on actual operation)
                full
            }
        };

        // Verify we haven't escaped the root
        let canonical_root = self.root.canonicalize().unwrap_or_else(|_| self.root.clone());
        if !canonical.starts_with(&canonical_root) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                format!(
                    "path escapes root: {} is not under {}",
                    canonical.display(),
                    canonical_root.display()
                ),
            ));
        }

        Ok(canonical)
    }

    /// Check if write operations are allowed.
    fn check_writable(&self) -> io::Result<()> {
        if self.read_only {
            Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "filesystem is read-only",
            ))
        } else {
            Ok(())
        }
    }
}

#[async_trait]
impl Filesystem for LocalFs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        let full_path = self.resolve(path)?;
        fs::read(&full_path).await
    }

    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        self.check_writable()?;
        let full_path = self.resolve(path)?;

        // Ensure parent directory exists
        if let Some(parent) = full_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        fs::write(&full_path, data).await
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let full_path = self.resolve(path)?;
        let mut entries = Vec::new();
        let mut dir = fs::read_dir(&full_path).await?;

        while let Some(entry) = dir.next_entry().await? {
            let file_type = entry.file_type().await?;
            let entry_type = if file_type.is_dir() {
                EntryType::Directory
            } else {
                EntryType::File
            };

            entries.push(DirEntry {
                name: entry.file_name().to_string_lossy().into_owned(),
                entry_type,
            });
        }

        entries.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(entries)
    }

    async fn stat(&self, path: &Path) -> io::Result<Metadata> {
        let full_path = self.resolve(path)?;
        let meta = fs::metadata(&full_path).await?;

        Ok(Metadata {
            is_dir: meta.is_dir(),
            is_file: meta.is_file(),
            size: meta.len(),
            modified: meta.modified().ok(),
        })
    }

    async fn mkdir(&self, path: &Path) -> io::Result<()> {
        self.check_writable()?;
        let full_path = self.resolve(path)?;
        fs::create_dir_all(&full_path).await
    }

    async fn remove(&self, path: &Path) -> io::Result<()> {
        self.check_writable()?;
        let full_path = self.resolve(path)?;
        let meta = fs::metadata(&full_path).await?;

        if meta.is_dir() {
            fs::remove_dir(&full_path).await
        } else {
            fs::remove_file(&full_path).await
        }
    }

    fn read_only(&self) -> bool {
        self.read_only
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;
    use std::sync::atomic::{AtomicU64, Ordering};

    static TEST_COUNTER: AtomicU64 = AtomicU64::new(0);

    fn temp_dir() -> PathBuf {
        let id = TEST_COUNTER.fetch_add(1, Ordering::SeqCst);
        env::temp_dir().join(format!("kaish-test-{}-{}", std::process::id(), id))
    }

    async fn setup() -> (LocalFs, PathBuf) {
        let dir = temp_dir();
        let _ = fs::remove_dir_all(&dir).await;
        fs::create_dir_all(&dir).await.unwrap();
        (LocalFs::new(&dir), dir)
    }

    async fn cleanup(dir: &Path) {
        let _ = fs::remove_dir_all(dir).await;
    }

    #[tokio::test]
    async fn test_write_and_read() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("test.txt"), b"hello").await.unwrap();
        let data = fs.read(Path::new("test.txt")).await.unwrap();
        assert_eq!(data, b"hello");

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_nested_write() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("a/b/c.txt"), b"nested").await.unwrap();
        let data = fs.read(Path::new("a/b/c.txt")).await.unwrap();
        assert_eq!(data, b"nested");

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_read_only() {
        let (_, dir) = setup().await;
        let fs = LocalFs::read_only(&dir);

        let result = fs.write(Path::new("test.txt"), b"data").await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::PermissionDenied);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_list() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("a.txt"), b"a").await.unwrap();
        fs.write(Path::new("b.txt"), b"b").await.unwrap();
        fs.mkdir(Path::new("subdir")).await.unwrap();

        let entries = fs.list(Path::new("")).await.unwrap();
        assert_eq!(entries.len(), 3);

        let names: Vec<_> = entries.iter().map(|e| &e.name).collect();
        assert!(names.contains(&&"a.txt".to_string()));
        assert!(names.contains(&&"b.txt".to_string()));
        assert!(names.contains(&&"subdir".to_string()));

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_stat() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("file.txt"), b"content").await.unwrap();
        fs.mkdir(Path::new("dir")).await.unwrap();

        let file_meta = fs.stat(Path::new("file.txt")).await.unwrap();
        assert!(file_meta.is_file);
        assert!(!file_meta.is_dir);
        assert_eq!(file_meta.size, 7);

        let dir_meta = fs.stat(Path::new("dir")).await.unwrap();
        assert!(dir_meta.is_dir);
        assert!(!dir_meta.is_file);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_remove() {
        let (fs, dir) = setup().await;

        fs.write(Path::new("file.txt"), b"data").await.unwrap();
        assert!(fs.exists(Path::new("file.txt")).await);

        fs.remove(Path::new("file.txt")).await.unwrap();
        assert!(!fs.exists(Path::new("file.txt")).await);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_path_escape_blocked() {
        let (fs, dir) = setup().await;

        // Trying to escape via .. should fail
        let result = fs.read(Path::new("../../../etc/passwd")).await;
        assert!(result.is_err());

        cleanup(&dir).await;
    }
}
