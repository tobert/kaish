//! In-memory filesystem implementation.
//!
//! Used for `/scratch` and testing. All data is ephemeral.

use super::traits::{DirEntry, EntryType, Filesystem, Metadata};
use async_trait::async_trait;
use std::collections::HashMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::RwLock;
use std::time::SystemTime;

/// Entry in the memory filesystem.
#[derive(Debug, Clone)]
enum Entry {
    File { data: Vec<u8>, modified: SystemTime },
    Directory { modified: SystemTime },
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
    fn ensure_parents(&self, path: &Path) -> io::Result<()> {
        let mut entries = self
            .entries
            .write()
            .map_err(|_| io::Error::other("lock poisoned"))?;

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
        let entries = self
            .entries
            .read()
            .map_err(|_| io::Error::other("lock poisoned"))?;

        match entries.get(&normalized) {
            Some(Entry::File { data, .. }) => Ok(data.clone()),
            Some(Entry::Directory { .. }) => Err(io::Error::new(
                io::ErrorKind::IsADirectory,
                format!("is a directory: {}", path.display()),
            )),
            None => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("not found: {}", path.display()),
            )),
        }
    }

    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        let normalized = Self::normalize(path);

        // Ensure parent directories exist
        self.ensure_parents(&normalized)?;

        let mut entries = self
            .entries
            .write()
            .map_err(|_| io::Error::other("lock poisoned"))?;

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
        let entries = self
            .entries
            .read()
            .map_err(|_| io::Error::other("lock poisoned"))?;

        // Verify the path is a directory
        match entries.get(&normalized) {
            Some(Entry::Directory { .. }) => {}
            Some(Entry::File { .. }) => {
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
                        let entry_type = match entry {
                            Entry::File { .. } => EntryType::File,
                            Entry::Directory { .. } => EntryType::Directory,
                        };
                        result.push(DirEntry {
                            name: name.to_string_lossy().into_owned(),
                            entry_type,
                        });
                    }
        }

        // Sort for consistent ordering
        result.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(result)
    }

    async fn stat(&self, path: &Path) -> io::Result<Metadata> {
        let normalized = Self::normalize(path);
        let entries = self
            .entries
            .read()
            .map_err(|_| io::Error::other("lock poisoned"))?;

        // Handle root directory
        if normalized.as_os_str().is_empty() {
            return Ok(Metadata {
                is_dir: true,
                is_file: false,
                size: 0,
                modified: Some(SystemTime::now()),
            });
        }

        match entries.get(&normalized) {
            Some(Entry::File { data, modified }) => Ok(Metadata {
                is_dir: false,
                is_file: true,
                size: data.len() as u64,
                modified: Some(*modified),
            }),
            Some(Entry::Directory { modified }) => Ok(Metadata {
                is_dir: true,
                is_file: false,
                size: 0,
                modified: Some(*modified),
            }),
            None => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("not found: {}", path.display()),
            )),
        }
    }

    async fn mkdir(&self, path: &Path) -> io::Result<()> {
        let normalized = Self::normalize(path);

        // Ensure parent directories exist
        self.ensure_parents(&normalized)?;

        let mut entries = self
            .entries
            .write()
            .map_err(|_| io::Error::other("lock poisoned"))?;

        // Check if something already exists
        if let Some(existing) = entries.get(&normalized) {
            return match existing {
                Entry::Directory { .. } => Ok(()), // Already exists, fine
                Entry::File { .. } => Err(io::Error::new(
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

        let mut entries = self
            .entries
            .write()
            .map_err(|_| io::Error::other("lock poisoned"))?;

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
}
