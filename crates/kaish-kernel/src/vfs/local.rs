//! Local filesystem backend.
//!
//! Provides access to real filesystem paths, with optional read-only mode.

use super::traits::{DirEntry, DirEntryKind, Filesystem};
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

    /// Resolve a path within the root WITHOUT following symlinks.
    ///
    /// Used by `lstat()` and `read_link()` which must not follow symlinks.
    /// Validates that the path stays within the sandbox by normalizing
    /// path components (resolving `.` and `..`) without canonicalization.
    fn resolve_no_follow(&self, path: &Path) -> io::Result<PathBuf> {
        let path = path.strip_prefix("/").unwrap_or(path);

        let mut normalized = self.root.clone();
        for component in path.components() {
            match component {
                std::path::Component::ParentDir => {
                    if normalized == self.root {
                        return Err(io::Error::new(
                            io::ErrorKind::PermissionDenied,
                            "path escapes root",
                        ));
                    }
                    normalized.pop();
                    if !normalized.starts_with(&self.root) {
                        return Err(io::Error::new(
                            io::ErrorKind::PermissionDenied,
                            "path escapes root",
                        ));
                    }
                }
                std::path::Component::Normal(c) => normalized.push(c),
                std::path::Component::CurDir => {} // skip
                _ => {}
            }
        }

        // Final containment check
        if !normalized.starts_with(&self.root) {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "path escapes root",
            ));
        }
        Ok(normalized)
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

    /// Extract permissions from std::fs::Metadata (unix only).
    #[cfg(unix)]
    fn extract_permissions(meta: &std::fs::Metadata) -> Option<u32> {
        use std::os::unix::fs::PermissionsExt;
        Some(meta.permissions().mode())
    }

    #[cfg(not(unix))]
    fn extract_permissions(_meta: &std::fs::Metadata) -> Option<u32> {
        None
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
            // Use symlink_metadata to detect symlinks without following them
            let metadata = fs::symlink_metadata(entry.path()).await?;
            let file_type = metadata.file_type();

            let (kind, symlink_target) = if file_type.is_symlink() {
                // Read the symlink target
                let target = fs::read_link(entry.path()).await.ok();
                (DirEntryKind::Symlink, target)
            } else if file_type.is_dir() {
                (DirEntryKind::Directory, None)
            } else {
                // Special files (sockets, pipes, devices) → File. See stat() comment.
                (DirEntryKind::File, None)
            };

            entries.push(DirEntry {
                name: entry.file_name().to_string_lossy().into_owned(),
                kind,
                size: metadata.len(),
                modified: metadata.modified().ok(),
                permissions: Self::extract_permissions(&metadata),
                symlink_target,
            });
        }

        entries.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(entries)
    }

    async fn stat(&self, path: &Path) -> io::Result<DirEntry> {
        let full_path = self.resolve(path)?;
        // stat follows symlinks
        let meta = fs::metadata(&full_path).await?;

        let kind = if meta.is_dir() {
            DirEntryKind::Directory
        } else {
            // Unix special files (sockets, pipes, block/char devices) are classified
            // as File. kaish doesn't operate on special files, and adding a variant
            // would force match-arm changes everywhere for no practical benefit.
            DirEntryKind::File
        };

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "/".to_string());

        Ok(DirEntry {
            name,
            kind,
            size: meta.len(),
            modified: meta.modified().ok(),
            permissions: Self::extract_permissions(&meta),
            symlink_target: None, // stat follows symlinks
        })
    }

    async fn lstat(&self, path: &Path) -> io::Result<DirEntry> {
        // lstat doesn't follow symlinks - validate containment without canonicalization
        let full_path = self.resolve_no_follow(path)?;

        // Use symlink_metadata which doesn't follow symlinks
        let meta = fs::symlink_metadata(&full_path).await?;

        let file_type = meta.file_type();
        let kind = if file_type.is_symlink() {
            DirEntryKind::Symlink
        } else if meta.is_dir() {
            DirEntryKind::Directory
        } else {
            // Special files (sockets, pipes, devices) → File. See stat() comment.
            DirEntryKind::File
        };

        let symlink_target = if file_type.is_symlink() {
            fs::read_link(&full_path).await.ok()
        } else {
            None
        };

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "/".to_string());

        Ok(DirEntry {
            name,
            kind,
            size: meta.len(),
            modified: meta.modified().ok(),
            permissions: Self::extract_permissions(&meta),
            symlink_target,
        })
    }

    async fn read_link(&self, path: &Path) -> io::Result<PathBuf> {
        let full_path = self.resolve_no_follow(path)?;
        fs::read_link(&full_path).await
    }

    async fn symlink(&self, target: &Path, link: &Path) -> io::Result<()> {
        self.check_writable()?;

        // Validate absolute symlink targets stay within sandbox.
        // `resolve` would strip the leading slash and treat `/etc/passwd` as
        // root-relative, so `<root>/etc/passwd` always "contains". For symlink
        // targets the OS follows the literal absolute path, so compare the
        // canonical target (or the literal path if it doesn't exist yet) to
        // the canonical root.
        if target.is_absolute() {
            let canonical_root = self.root.canonicalize().unwrap_or_else(|_| self.root.clone());
            let canonical_target = target.canonicalize().unwrap_or_else(|_| target.to_path_buf());
            if !canonical_target.starts_with(&canonical_root) {
                return Err(io::Error::new(
                    io::ErrorKind::PermissionDenied,
                    format!("symlink target escapes root: {}", target.display()),
                ));
            }
        }

        let link_path = self.resolve_no_follow(link)?;

        // Ensure parent directory exists
        if let Some(parent) = link_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        #[cfg(unix)]
        {
            tokio::fs::symlink(target, &link_path).await
        }
        #[cfg(windows)]
        {
            // Windows needs to know if target is a file or directory
            // Default to file symlink; for directories use symlink_dir
            tokio::fs::symlink_file(target, &link_path).await
        }
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

    async fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        self.check_writable()?;
        let from_path = self.resolve(from)?;
        let to_path = self.resolve(to)?;

        // Ensure parent directory exists for destination
        if let Some(parent) = to_path.parent() {
            fs::create_dir_all(parent).await?;
        }

        fs::rename(&from_path, &to_path).await
    }

    fn read_only(&self) -> bool {
        self.read_only
    }

    fn real_path(&self, path: &Path) -> Option<PathBuf> {
        self.resolve(path).ok()
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

        let file_entry = fs.stat(Path::new("file.txt")).await.unwrap();
        assert!(file_entry.is_file());
        assert_eq!(file_entry.size, 7);

        let dir_entry = fs.stat(Path::new("dir")).await.unwrap();
        assert!(dir_entry.is_dir());

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

    #[tokio::test]
    async fn test_lstat_path_escape_blocked() {
        // Bug H: lstat must validate path containment
        let (fs, dir) = setup().await;

        let result = fs.lstat(Path::new("../../etc/passwd")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::PermissionDenied);

        cleanup(&dir).await;
    }

    #[tokio::test]
    async fn test_read_link_path_escape_blocked() {
        // Bug H: read_link must validate path containment
        let (fs, dir) = setup().await;

        let result = fs.read_link(Path::new("../../etc/passwd")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::PermissionDenied);

        cleanup(&dir).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_lstat_on_valid_symlink() {
        // Regression: lstat should still work for valid symlinks
        let (fs, dir) = setup().await;

        fs.write(Path::new("target.txt"), b"content").await.unwrap();
        fs.symlink(Path::new("target.txt"), Path::new("link.txt"))
            .await
            .unwrap();

        let entry = fs.lstat(Path::new("link.txt")).await.unwrap();
        assert!(entry.is_symlink(), "lstat should report symlink kind");

        cleanup(&dir).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_symlink_absolute_target_escape_blocked() {
        // Bug I: absolute symlink targets must stay within sandbox
        let (fs, dir) = setup().await;

        let result = fs
            .symlink(Path::new("/etc/passwd"), Path::new("escape_link"))
            .await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::PermissionDenied);

        cleanup(&dir).await;
    }

    #[cfg(unix)]
    #[tokio::test]
    async fn test_symlink_relative_target_allowed() {
        // Regression: relative symlink targets should still be allowed
        let (fs, dir) = setup().await;

        fs.write(Path::new("target.txt"), b"content").await.unwrap();
        let result = fs
            .symlink(Path::new("target.txt"), Path::new("rel_link"))
            .await;
        assert!(result.is_ok());

        cleanup(&dir).await;
    }
}
