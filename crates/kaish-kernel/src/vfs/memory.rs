//! In-memory filesystem implementation.
//!
//! Used for `/scratch` and testing. All data is ephemeral.

use super::traits::{DirEntry, EntryType, Filesystem, Metadata};
use async_trait::async_trait;
use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use tokio::sync::RwLock;

/// Entry in the memory filesystem.
#[derive(Debug, Clone)]
enum Entry {
    File { data: Vec<u8>, modified: SystemTime },
    Directory { modified: SystemTime },
    Symlink { target: PathBuf, modified: SystemTime },
}

/// In-memory filesystem.
///
/// Thread-safe via internal `RwLock`. All data is lost when dropped.
#[derive(Debug)]
pub struct MemoryFs {
    entries: RwLock<HashMap<PathBuf, Entry>>,
}

impl Default for MemoryFs {
    fn default() -> Self {
        Self::new()
    }
}

impl MemoryFs {
    /// Create a new empty in-memory filesystem.
    pub fn new() -> Self {
        let mut entries = HashMap::new();
        // Root directory always exists
        entries.insert(
            PathBuf::from(""),
            Entry::Directory {
                modified: SystemTime::now(),
            },
        );
        Self {
            entries: RwLock::new(entries),
        }
    }

    /// Normalize a path: remove leading `/`, resolve `.` and `..`.
    fn normalize(path: &Path) -> PathBuf {
        let mut result = PathBuf::new();
        for component in path.components() {
            match component {
                std::path::Component::RootDir => {}
                std::path::Component::CurDir => {}
                std::path::Component::ParentDir => {
                    result.pop();
                }
                std::path::Component::Normal(s) => {
                    result.push(s);
                }
                std::path::Component::Prefix(_) => {}
            }
        }
        result
    }

    /// Ensure all parent directories exist.
    async fn ensure_parents(&self, path: &Path) -> io::Result<()> {
        let mut entries = self.entries.write().await;

        let mut current = PathBuf::new();
        for component in path.parent().into_iter().flat_map(|p| p.components()) {
            if let std::path::Component::Normal(s) = component {
                current.push(s);
                entries.entry(current.clone()).or_insert(Entry::Directory {
                    modified: SystemTime::now(),
                });
            }
        }
        Ok(())
    }
}

#[async_trait]
impl Filesystem for MemoryFs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        let normalized = Self::normalize(path);
        let entries = self.entries.read().await;

        match entries.get(&normalized) {
            Some(Entry::File { data, .. }) => Ok(data.clone()),
            Some(Entry::Directory { .. }) => Err(io::Error::new(
                io::ErrorKind::IsADirectory,
                format!("is a directory: {}", path.display()),
            )),
            Some(Entry::Symlink { target, .. }) => {
                // Clone target before dropping lock to follow the symlink
                let target = target.clone();
                drop(entries);
                self.read(&target).await
            }
            None => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("not found: {}", path.display()),
            )),
        }
    }

    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        let normalized = Self::normalize(path);

        // Ensure parent directories exist
        self.ensure_parents(&normalized).await?;

        let mut entries = self.entries.write().await;

        // Check we're not overwriting a directory
        if let Some(Entry::Directory { .. }) = entries.get(&normalized) {
            return Err(io::Error::new(
                io::ErrorKind::IsADirectory,
                format!("is a directory: {}", path.display()),
            ));
        }

        entries.insert(
            normalized,
            Entry::File {
                data: data.to_vec(),
                modified: SystemTime::now(),
            },
        );
        Ok(())
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let normalized = Self::normalize(path);
        let entries = self.entries.read().await;

        // Verify the path is a directory
        match entries.get(&normalized) {
            Some(Entry::Directory { .. }) => {}
            Some(Entry::File { .. }) => {
                return Err(io::Error::new(
                    io::ErrorKind::NotADirectory,
                    format!("not a directory: {}", path.display()),
                ))
            }
            Some(Entry::Symlink { .. }) => {
                return Err(io::Error::new(
                    io::ErrorKind::NotADirectory,
                    format!("not a directory: {}", path.display()),
                ))
            }
            None if normalized.as_os_str().is_empty() => {
                // Root directory
            }
            None => {
                return Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("not found: {}", path.display()),
                ))
            }
        }

        // Find all direct children
        let prefix = if normalized.as_os_str().is_empty() {
            PathBuf::new()
        } else {
            normalized.clone()
        };

        let mut result = Vec::new();
        for (entry_path, entry) in entries.iter() {
            if let Some(parent) = entry_path.parent()
                && parent == prefix && entry_path != &normalized
                    && let Some(name) = entry_path.file_name() {
                        let (entry_type, size, symlink_target) = match entry {
                            Entry::File { data, .. } => (EntryType::File, data.len() as u64, None),
                            Entry::Directory { .. } => (EntryType::Directory, 0, None),
                            Entry::Symlink { target, .. } => (EntryType::Symlink, 0, Some(target.clone())),
                        };
                        result.push(DirEntry {
                            name: name.to_string_lossy().into_owned(),
                            entry_type,
                            size,
                            symlink_target,
                        });
                    }
        }

        // Sort for consistent ordering
        result.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(result)
    }

    async fn stat(&self, path: &Path) -> io::Result<Metadata> {
        let normalized = Self::normalize(path);

        // Handle root directory
        if normalized.as_os_str().is_empty() {
            return Ok(Metadata {
                is_dir: true,
                is_file: false,
                is_symlink: false,
                size: 0,
                modified: Some(SystemTime::now()),
            });
        }

        // First, check what kind of entry this is
        let entry_info: Option<(Metadata, Option<PathBuf>)> = {
            let entries = self.entries.read().await;
            match entries.get(&normalized) {
                Some(Entry::File { data, modified }) => Some((
                    Metadata {
                        is_dir: false,
                        is_file: true,
                        is_symlink: false,
                        size: data.len() as u64,
                        modified: Some(*modified),
                    },
                    None, // No symlink target to follow
                )),
                Some(Entry::Directory { modified }) => Some((
                    Metadata {
                        is_dir: true,
                        is_file: false,
                        is_symlink: false,
                        size: 0,
                        modified: Some(*modified),
                    },
                    None,
                )),
                Some(Entry::Symlink { target, .. }) => {
                    // Need to follow symlink - save the target
                    Some((
                        Metadata {
                            is_dir: false,
                            is_file: false,
                            is_symlink: false,
                            size: 0,
                            modified: None, // Will be overwritten by target
                        },
                        Some(target.clone()),
                    ))
                }
                None => None,
            }
        };

        match entry_info {
            Some((meta, None)) => Ok(meta),
            Some((_, Some(target))) => {
                // Follow the symlink
                self.stat(&target).await
            }
            None => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("not found: {}", path.display()),
            )),
        }
    }

    async fn lstat(&self, path: &Path) -> io::Result<Metadata> {
        let normalized = Self::normalize(path);
        let entries = self.entries.read().await;

        // Handle root directory
        if normalized.as_os_str().is_empty() {
            return Ok(Metadata {
                is_dir: true,
                is_file: false,
                is_symlink: false,
                size: 0,
                modified: Some(SystemTime::now()),
            });
        }

        match entries.get(&normalized) {
            Some(Entry::File { data, modified }) => Ok(Metadata {
                is_dir: false,
                is_file: true,
                is_symlink: false,
                size: data.len() as u64,
                modified: Some(*modified),
            }),
            Some(Entry::Directory { modified }) => Ok(Metadata {
                is_dir: true,
                is_file: false,
                is_symlink: false,
                size: 0,
                modified: Some(*modified),
            }),
            Some(Entry::Symlink { modified, .. }) => Ok(Metadata {
                is_dir: false,
                is_file: false,
                is_symlink: true,
                size: 0,
                modified: Some(*modified),
            }),
            None => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("not found: {}", path.display()),
            )),
        }
    }

    async fn read_link(&self, path: &Path) -> io::Result<PathBuf> {
        let normalized = Self::normalize(path);
        let entries = self.entries.read().await;

        match entries.get(&normalized) {
            Some(Entry::Symlink { target, .. }) => Ok(target.clone()),
            Some(_) => Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("not a symbolic link: {}", path.display()),
            )),
            None => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("not found: {}", path.display()),
            )),
        }
    }

    async fn symlink(&self, target: &Path, link: &Path) -> io::Result<()> {
        let normalized = Self::normalize(link);

        // Ensure parent directories exist
        self.ensure_parents(&normalized).await?;

        let mut entries = self.entries.write().await;

        // Check if something already exists at this path
        if entries.contains_key(&normalized) {
            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("file exists: {}", link.display()),
            ));
        }

        entries.insert(
            normalized,
            Entry::Symlink {
                target: target.to_path_buf(),
                modified: SystemTime::now(),
            },
        );
        Ok(())
    }

    async fn mkdir(&self, path: &Path) -> io::Result<()> {
        let normalized = Self::normalize(path);

        // Ensure parent directories exist
        self.ensure_parents(&normalized).await?;

        let mut entries = self.entries.write().await;

        // Check if something already exists
        if let Some(existing) = entries.get(&normalized) {
            return match existing {
                Entry::Directory { .. } => Ok(()), // Already exists, fine
                Entry::File { .. } | Entry::Symlink { .. } => Err(io::Error::new(
                    io::ErrorKind::AlreadyExists,
                    format!("file exists: {}", path.display()),
                )),
            };
        }

        entries.insert(
            normalized,
            Entry::Directory {
                modified: SystemTime::now(),
            },
        );
        Ok(())
    }

    async fn remove(&self, path: &Path) -> io::Result<()> {
        let normalized = Self::normalize(path);

        if normalized.as_os_str().is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "cannot remove root directory",
            ));
        }

        let mut entries = self.entries.write().await;

        // Check if it's a non-empty directory
        if let Some(Entry::Directory { .. }) = entries.get(&normalized) {
            // Check for children
            let has_children = entries.keys().any(|k| {
                k.parent() == Some(&normalized) && k != &normalized
            });
            if has_children {
                return Err(io::Error::new(
                    io::ErrorKind::DirectoryNotEmpty,
                    format!("directory not empty: {}", path.display()),
                ));
            }
        }

        entries.remove(&normalized).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("not found: {}", path.display()),
            )
        })?;
        Ok(())
    }

    async fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        let from_normalized = Self::normalize(from);
        let to_normalized = Self::normalize(to);

        if from_normalized.as_os_str().is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::PermissionDenied,
                "cannot rename root directory",
            ));
        }

        // Ensure parent directories exist for destination
        drop(self.ensure_parents(&to_normalized).await);

        let mut entries = self.entries.write().await;

        // Get the source entry
        let entry = entries.remove(&from_normalized).ok_or_else(|| {
            io::Error::new(
                io::ErrorKind::NotFound,
                format!("not found: {}", from.display()),
            )
        })?;

        // Check we're not overwriting a directory with a file or vice versa
        if let Some(existing) = entries.get(&to_normalized) {
            match (&entry, existing) {
                (Entry::File { .. }, Entry::Directory { .. }) => {
                    // Put the source back and error
                    entries.insert(from_normalized, entry);
                    return Err(io::Error::new(
                        io::ErrorKind::IsADirectory,
                        format!("destination is a directory: {}", to.display()),
                    ));
                }
                (Entry::Directory { .. }, Entry::File { .. }) => {
                    entries.insert(from_normalized, entry);
                    return Err(io::Error::new(
                        io::ErrorKind::NotADirectory,
                        format!("destination is not a directory: {}", to.display()),
                    ));
                }
                _ => {}
            }
        }

        // For directories, we need to rename all children too
        if matches!(entry, Entry::Directory { .. }) {
            // Collect paths to rename (can't modify while iterating)
            let children_to_rename: Vec<(PathBuf, Entry)> = entries
                .iter()
                .filter(|(k, _)| k.starts_with(&from_normalized) && *k != &from_normalized)
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();

            // Remove old children and insert with new paths
            for (old_path, child_entry) in children_to_rename {
                entries.remove(&old_path);
                let relative = old_path.strip_prefix(&from_normalized).unwrap();
                let new_path = to_normalized.join(relative);
                entries.insert(new_path, child_entry);
            }
        }

        // Insert at new location
        entries.insert(to_normalized, entry);
        Ok(())
    }

    fn read_only(&self) -> bool {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_write_and_read() {
        let fs = MemoryFs::new();
        fs.write(Path::new("test.txt"), b"hello world").await.unwrap();
        let data = fs.read(Path::new("test.txt")).await.unwrap();
        assert_eq!(data, b"hello world");
    }

    #[tokio::test]
    async fn test_read_not_found() {
        let fs = MemoryFs::new();
        let result = fs.read(Path::new("nonexistent.txt")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::NotFound);
    }

    #[tokio::test]
    async fn test_nested_directories() {
        let fs = MemoryFs::new();
        fs.write(Path::new("a/b/c/file.txt"), b"nested").await.unwrap();

        // Should have created parent directories
        let meta = fs.stat(Path::new("a")).await.unwrap();
        assert!(meta.is_dir);

        let meta = fs.stat(Path::new("a/b")).await.unwrap();
        assert!(meta.is_dir);

        let meta = fs.stat(Path::new("a/b/c")).await.unwrap();
        assert!(meta.is_dir);

        let data = fs.read(Path::new("a/b/c/file.txt")).await.unwrap();
        assert_eq!(data, b"nested");
    }

    #[tokio::test]
    async fn test_list_directory() {
        let fs = MemoryFs::new();
        fs.write(Path::new("a.txt"), b"a").await.unwrap();
        fs.write(Path::new("b.txt"), b"b").await.unwrap();
        fs.mkdir(Path::new("subdir")).await.unwrap();

        let entries = fs.list(Path::new("")).await.unwrap();
        assert_eq!(entries.len(), 3);

        let names: Vec<_> = entries.iter().map(|e| &e.name).collect();
        assert!(names.contains(&&"a.txt".to_string()));
        assert!(names.contains(&&"b.txt".to_string()));
        assert!(names.contains(&&"subdir".to_string()));
    }

    #[tokio::test]
    async fn test_mkdir_and_stat() {
        let fs = MemoryFs::new();
        fs.mkdir(Path::new("mydir")).await.unwrap();

        let meta = fs.stat(Path::new("mydir")).await.unwrap();
        assert!(meta.is_dir);
        assert!(!meta.is_file);
    }

    #[tokio::test]
    async fn test_remove_file() {
        let fs = MemoryFs::new();
        fs.write(Path::new("file.txt"), b"data").await.unwrap();

        fs.remove(Path::new("file.txt")).await.unwrap();

        let result = fs.stat(Path::new("file.txt")).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_remove_empty_directory() {
        let fs = MemoryFs::new();
        fs.mkdir(Path::new("emptydir")).await.unwrap();

        fs.remove(Path::new("emptydir")).await.unwrap();

        let result = fs.stat(Path::new("emptydir")).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_remove_non_empty_directory_fails() {
        let fs = MemoryFs::new();
        fs.write(Path::new("dir/file.txt"), b"data").await.unwrap();

        let result = fs.remove(Path::new("dir")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::DirectoryNotEmpty);
    }

    #[tokio::test]
    async fn test_path_normalization() {
        let fs = MemoryFs::new();
        fs.write(Path::new("/a/b/c.txt"), b"data").await.unwrap();

        // Various path forms should all work
        let data1 = fs.read(Path::new("a/b/c.txt")).await.unwrap();
        let data2 = fs.read(Path::new("/a/b/c.txt")).await.unwrap();
        let data3 = fs.read(Path::new("a/./b/c.txt")).await.unwrap();
        let data4 = fs.read(Path::new("a/b/../b/c.txt")).await.unwrap();

        assert_eq!(data1, data2);
        assert_eq!(data2, data3);
        assert_eq!(data3, data4);
    }

    #[tokio::test]
    async fn test_overwrite_file() {
        let fs = MemoryFs::new();
        fs.write(Path::new("file.txt"), b"first").await.unwrap();
        fs.write(Path::new("file.txt"), b"second").await.unwrap();

        let data = fs.read(Path::new("file.txt")).await.unwrap();
        assert_eq!(data, b"second");
    }

    #[tokio::test]
    async fn test_exists() {
        let fs = MemoryFs::new();
        assert!(!fs.exists(Path::new("nope.txt")).await);

        fs.write(Path::new("yes.txt"), b"here").await.unwrap();
        assert!(fs.exists(Path::new("yes.txt")).await);
    }

    #[tokio::test]
    async fn test_rename_file() {
        let fs = MemoryFs::new();
        fs.write(Path::new("old.txt"), b"content").await.unwrap();

        fs.rename(Path::new("old.txt"), Path::new("new.txt")).await.unwrap();

        // New path exists with same content
        let data = fs.read(Path::new("new.txt")).await.unwrap();
        assert_eq!(data, b"content");

        // Old path no longer exists
        assert!(!fs.exists(Path::new("old.txt")).await);
    }

    #[tokio::test]
    async fn test_rename_directory() {
        let fs = MemoryFs::new();
        fs.write(Path::new("dir/a.txt"), b"a").await.unwrap();
        fs.write(Path::new("dir/b.txt"), b"b").await.unwrap();
        fs.write(Path::new("dir/sub/c.txt"), b"c").await.unwrap();

        fs.rename(Path::new("dir"), Path::new("renamed")).await.unwrap();

        // New paths exist
        assert!(fs.exists(Path::new("renamed")).await);
        assert!(fs.exists(Path::new("renamed/a.txt")).await);
        assert!(fs.exists(Path::new("renamed/b.txt")).await);
        assert!(fs.exists(Path::new("renamed/sub/c.txt")).await);

        // Old paths don't exist
        assert!(!fs.exists(Path::new("dir")).await);
        assert!(!fs.exists(Path::new("dir/a.txt")).await);

        // Content preserved
        let data = fs.read(Path::new("renamed/a.txt")).await.unwrap();
        assert_eq!(data, b"a");
    }

    #[tokio::test]
    async fn test_rename_not_found() {
        let fs = MemoryFs::new();
        let result = fs.rename(Path::new("nonexistent"), Path::new("dest")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::NotFound);
    }

    // --- Symlink tests ---

    #[tokio::test]
    async fn test_symlink_create_and_read_link() {
        let fs = MemoryFs::new();
        fs.write(Path::new("target.txt"), b"content").await.unwrap();
        fs.symlink(Path::new("target.txt"), Path::new("link.txt")).await.unwrap();

        // read_link returns the raw target
        let target = fs.read_link(Path::new("link.txt")).await.unwrap();
        assert_eq!(target, Path::new("target.txt"));
    }

    #[tokio::test]
    async fn test_symlink_read_follows_link() {
        let fs = MemoryFs::new();
        fs.write(Path::new("target.txt"), b"hello from target").await.unwrap();
        fs.symlink(Path::new("target.txt"), Path::new("link.txt")).await.unwrap();

        // Reading through symlink should return target's content
        let data = fs.read(Path::new("link.txt")).await.unwrap();
        assert_eq!(data, b"hello from target");
    }

    #[tokio::test]
    async fn test_symlink_stat_follows_link() {
        let fs = MemoryFs::new();
        fs.write(Path::new("target.txt"), b"12345").await.unwrap();
        fs.symlink(Path::new("target.txt"), Path::new("link.txt")).await.unwrap();

        // stat follows symlinks - should report file metadata
        let meta = fs.stat(Path::new("link.txt")).await.unwrap();
        assert!(meta.is_file);
        assert!(!meta.is_symlink);
        assert_eq!(meta.size, 5);
    }

    #[tokio::test]
    async fn test_symlink_lstat_returns_symlink_info() {
        let fs = MemoryFs::new();
        fs.write(Path::new("target.txt"), b"content").await.unwrap();
        fs.symlink(Path::new("target.txt"), Path::new("link.txt")).await.unwrap();

        // lstat does not follow symlinks
        let meta = fs.lstat(Path::new("link.txt")).await.unwrap();
        assert!(meta.is_symlink);
        assert!(!meta.is_file);
        assert!(!meta.is_dir);
    }

    #[tokio::test]
    async fn test_symlink_in_list() {
        let fs = MemoryFs::new();
        fs.write(Path::new("file.txt"), b"content").await.unwrap();
        fs.symlink(Path::new("file.txt"), Path::new("link.txt")).await.unwrap();
        fs.mkdir(Path::new("dir")).await.unwrap();

        let entries = fs.list(Path::new("")).await.unwrap();
        assert_eq!(entries.len(), 3);

        // Find the symlink entry
        let link_entry = entries.iter().find(|e| e.name == "link.txt").unwrap();
        assert_eq!(link_entry.entry_type, EntryType::Symlink);
        assert_eq!(link_entry.symlink_target, Some(PathBuf::from("file.txt")));
    }

    #[tokio::test]
    async fn test_symlink_broken_link() {
        let fs = MemoryFs::new();
        // Create symlink to non-existent target
        fs.symlink(Path::new("nonexistent.txt"), Path::new("broken.txt")).await.unwrap();

        // read_link still works
        let target = fs.read_link(Path::new("broken.txt")).await.unwrap();
        assert_eq!(target, Path::new("nonexistent.txt"));

        // lstat works (the symlink exists)
        let meta = fs.lstat(Path::new("broken.txt")).await.unwrap();
        assert!(meta.is_symlink);

        // stat fails (target doesn't exist)
        let result = fs.stat(Path::new("broken.txt")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::NotFound);

        // read fails (target doesn't exist)
        let result = fs.read(Path::new("broken.txt")).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_symlink_read_link_on_non_symlink_fails() {
        let fs = MemoryFs::new();
        fs.write(Path::new("file.txt"), b"content").await.unwrap();

        let result = fs.read_link(Path::new("file.txt")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::InvalidInput);
    }

    #[tokio::test]
    async fn test_symlink_already_exists() {
        let fs = MemoryFs::new();
        fs.write(Path::new("existing.txt"), b"content").await.unwrap();

        // Can't create symlink over existing file
        let result = fs.symlink(Path::new("target"), Path::new("existing.txt")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::AlreadyExists);
    }

    // --- Edge case tests ---

    #[tokio::test]
    async fn test_symlink_chain() {
        // a -> b -> c -> file.txt
        let fs = MemoryFs::new();
        fs.write(Path::new("file.txt"), b"end of chain").await.unwrap();
        fs.symlink(Path::new("file.txt"), Path::new("c")).await.unwrap();
        fs.symlink(Path::new("c"), Path::new("b")).await.unwrap();
        fs.symlink(Path::new("b"), Path::new("a")).await.unwrap();

        // Reading through chain should work
        let data = fs.read(Path::new("a")).await.unwrap();
        assert_eq!(data, b"end of chain");

        // stat through chain should report file
        let meta = fs.stat(Path::new("a")).await.unwrap();
        assert!(meta.is_file);
    }

    #[tokio::test]
    async fn test_symlink_to_directory() {
        let fs = MemoryFs::new();
        fs.mkdir(Path::new("realdir")).await.unwrap();
        fs.write(Path::new("realdir/file.txt"), b"inside dir").await.unwrap();
        fs.symlink(Path::new("realdir"), Path::new("linkdir")).await.unwrap();

        // stat follows symlink - should see directory
        let meta = fs.stat(Path::new("linkdir")).await.unwrap();
        assert!(meta.is_dir);

        // Note: listing through symlink requires following in list(),
        // which we don't currently support (symlink to dir returns NotADirectory)
    }

    #[tokio::test]
    async fn test_symlink_relative_path_stored_as_is() {
        let fs = MemoryFs::new();
        fs.mkdir(Path::new("subdir")).await.unwrap();
        fs.write(Path::new("subdir/target.txt"), b"content").await.unwrap();

        // Store a relative path in the symlink
        fs.symlink(Path::new("../subdir/target.txt"), Path::new("subdir/link.txt")).await.unwrap();

        // read_link returns the path as stored
        let target = fs.read_link(Path::new("subdir/link.txt")).await.unwrap();
        assert_eq!(target.to_string_lossy(), "../subdir/target.txt");
    }

    #[tokio::test]
    async fn test_symlink_absolute_path() {
        let fs = MemoryFs::new();
        fs.write(Path::new("target.txt"), b"content").await.unwrap();

        // Store absolute path
        fs.symlink(Path::new("/target.txt"), Path::new("link.txt")).await.unwrap();

        let target = fs.read_link(Path::new("link.txt")).await.unwrap();
        assert_eq!(target.to_string_lossy(), "/target.txt");

        // Following should work (normalize strips leading /)
        let data = fs.read(Path::new("link.txt")).await.unwrap();
        assert_eq!(data, b"content");
    }

    #[tokio::test]
    async fn test_symlink_remove() {
        let fs = MemoryFs::new();
        fs.write(Path::new("target.txt"), b"content").await.unwrap();
        fs.symlink(Path::new("target.txt"), Path::new("link.txt")).await.unwrap();

        // Remove symlink (not the target)
        fs.remove(Path::new("link.txt")).await.unwrap();

        // Symlink gone
        assert!(!fs.exists(Path::new("link.txt")).await);

        // Target still exists
        assert!(fs.exists(Path::new("target.txt")).await);
    }

    #[tokio::test]
    async fn test_symlink_overwrite_target_content() {
        let fs = MemoryFs::new();
        fs.write(Path::new("target.txt"), b"original").await.unwrap();
        fs.symlink(Path::new("target.txt"), Path::new("link.txt")).await.unwrap();

        // Modify target
        fs.write(Path::new("target.txt"), b"modified").await.unwrap();

        // Reading through link shows new content
        let data = fs.read(Path::new("link.txt")).await.unwrap();
        assert_eq!(data, b"modified");
    }

    #[tokio::test]
    async fn test_symlink_empty_name() {
        let fs = MemoryFs::new();
        fs.write(Path::new("target.txt"), b"content").await.unwrap();

        // Symlink with empty path components in target
        fs.symlink(Path::new("./target.txt"), Path::new("link.txt")).await.unwrap();

        let target = fs.read_link(Path::new("link.txt")).await.unwrap();
        assert_eq!(target.to_string_lossy(), "./target.txt");
    }

    #[tokio::test]
    async fn test_symlink_nested_creation() {
        let fs = MemoryFs::new();
        // Symlink in non-existent directory should create parents
        fs.symlink(Path::new("target"), Path::new("a/b/c/link")).await.unwrap();

        // Parents created
        let meta = fs.stat(Path::new("a/b")).await.unwrap();
        assert!(meta.is_dir);

        // Symlink exists (lstat)
        let meta = fs.lstat(Path::new("a/b/c/link")).await.unwrap();
        assert!(meta.is_symlink);
    }

    #[tokio::test]
    async fn test_symlink_read_link_not_found() {
        let fs = MemoryFs::new();

        let result = fs.read_link(Path::new("nonexistent")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::NotFound);
    }

    #[tokio::test]
    async fn test_symlink_read_link_on_directory() {
        let fs = MemoryFs::new();
        fs.mkdir(Path::new("dir")).await.unwrap();

        let result = fs.read_link(Path::new("dir")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::InvalidInput);
    }
}
