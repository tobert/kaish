//! VFS router for mount point management.
//!
//! Routes filesystem operations to the appropriate backend based on path.

use super::traits::{DirEntry, EntryType, Filesystem, Metadata};
use async_trait::async_trait;
use std::collections::BTreeMap;
use std::io;
use std::path::{Path, PathBuf};
use std::sync::Arc;

/// Information about a mount point.
#[derive(Debug, Clone)]
pub struct MountInfo {
    /// The mount path (e.g., "/mnt/project").
    pub path: PathBuf,
    /// Whether this mount is read-only.
    pub read_only: bool,
}

/// Routes filesystem operations to mounted backends.
///
/// Mount points are matched by longest prefix. For example, if `/mnt` and
/// `/mnt/project` are both mounted, a path like `/mnt/project/src/main.rs`
/// will be routed to the `/mnt/project` mount.
#[derive(Default)]
pub struct VfsRouter {
    /// Mount points, keyed by path. Uses BTreeMap for ordered iteration.
    mounts: BTreeMap<PathBuf, Arc<dyn Filesystem>>,
}

impl std::fmt::Debug for VfsRouter {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VfsRouter")
            .field("mounts", &self.mounts.keys().collect::<Vec<_>>())
            .finish()
    }
}

impl VfsRouter {
    /// Create a new empty VFS router.
    pub fn new() -> Self {
        Self {
            mounts: BTreeMap::new(),
        }
    }

    /// Mount a filesystem at the given path.
    ///
    /// The path should be absolute (start with `/`). If a filesystem is
    /// already mounted at this path, it will be replaced.
    pub fn mount(&mut self, path: impl Into<PathBuf>, fs: impl Filesystem + 'static) {
        let path = Self::normalize_mount_path(path.into());
        self.mounts.insert(path, Arc::new(fs));
    }

    /// Mount a filesystem (already wrapped in Arc) at the given path.
    pub fn mount_arc(&mut self, path: impl Into<PathBuf>, fs: Arc<dyn Filesystem>) {
        let path = Self::normalize_mount_path(path.into());
        self.mounts.insert(path, fs);
    }

    /// Unmount the filesystem at the given path.
    ///
    /// Returns `true` if a mount was removed, `false` if nothing was mounted there.
    pub fn unmount(&mut self, path: impl AsRef<Path>) -> bool {
        let path = Self::normalize_mount_path(path.as_ref().to_path_buf());
        self.mounts.remove(&path).is_some()
    }

    /// List all current mounts.
    pub fn list_mounts(&self) -> Vec<MountInfo> {
        self.mounts
            .iter()
            .map(|(path, fs)| MountInfo {
                path: path.clone(),
                read_only: fs.read_only(),
            })
            .collect()
    }

    /// Normalize a mount path: ensure it starts with `/` and has no trailing slash.
    fn normalize_mount_path(path: PathBuf) -> PathBuf {
        let s = path.to_string_lossy();
        let s = s.trim_end_matches('/');
        if s.is_empty() {
            PathBuf::from("/")
        } else if !s.starts_with('/') {
            PathBuf::from(format!("/{}", s))
        } else {
            PathBuf::from(s)
        }
    }

    /// Resolve a VFS path to a real filesystem path.
    ///
    /// Returns `Some(path)` if the VFS path maps to a real filesystem (like LocalFs),
    /// or `None` if the path is in a virtual filesystem (like MemoryFs).
    ///
    /// This is needed for tools like `git` that must use real paths with external libraries.
    pub fn resolve_real_path(&self, path: &Path) -> Option<PathBuf> {
        let (fs, relative) = self.find_mount(path).ok()?;
        fs.real_path(&relative)
    }

    /// Find the mount point for a given path.
    ///
    /// Returns the mount and the path relative to that mount.
    fn find_mount(&self, path: &Path) -> io::Result<(Arc<dyn Filesystem>, PathBuf)> {
        let path_str = path.to_string_lossy();
        let normalized = if path_str.starts_with('/') {
            path.to_path_buf()
        } else {
            PathBuf::from(format!("/{}", path_str))
        };

        // Find longest matching mount point
        let mut best_match: Option<(&PathBuf, &Arc<dyn Filesystem>)> = None;

        for (mount_path, fs) in &self.mounts {
            let mount_str = mount_path.to_string_lossy();

            // Check if the path starts with this mount point
            let is_match = if mount_str == "/" {
                true // Root matches everything
            } else {
                let normalized_str = normalized.to_string_lossy();
                normalized_str == mount_str.as_ref()
                    || normalized_str.starts_with(&format!("{}/", mount_str))
            };

            if is_match {
                // Keep the longest match
                if best_match.is_none()
                    || mount_path.as_os_str().len()
                        > best_match.expect("checked is_none").0.as_os_str().len()
                {
                    best_match = Some((mount_path, fs));
                }
            }
        }

        match best_match {
            Some((mount_path, fs)) => {
                // Calculate relative path
                let mount_str = mount_path.to_string_lossy();
                let normalized_str = normalized.to_string_lossy();

                let relative = if mount_str == "/" {
                    normalized_str.trim_start_matches('/').to_string()
                } else {
                    normalized_str
                        .strip_prefix(mount_str.as_ref())
                        .unwrap_or("")
                        .trim_start_matches('/')
                        .to_string()
                };

                Ok((Arc::clone(fs), PathBuf::from(relative)))
            }
            None => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("no mount point for path: {}", path.display()),
            )),
        }
    }
}

#[async_trait]
impl Filesystem for VfsRouter {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        let (fs, relative) = self.find_mount(path)?;
        fs.read(&relative).await
    }

    async fn write(&self, path: &Path, data: &[u8]) -> io::Result<()> {
        let (fs, relative) = self.find_mount(path)?;
        fs.write(&relative, data).await
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        // Special case: listing root might need to show mount points
        let path_str = path.to_string_lossy();
        if path_str.is_empty() || path_str == "/" {
            return self.list_root().await;
        }

        let (fs, relative) = self.find_mount(path)?;
        fs.list(&relative).await
    }

    async fn stat(&self, path: &Path) -> io::Result<Metadata> {
        // Special case: root always exists
        let path_str = path.to_string_lossy();
        if path_str.is_empty() || path_str == "/" {
            return Ok(Metadata {
                is_dir: true,
                is_file: false,
                is_symlink: false,
                size: 0,
                modified: None,
            });
        }

        // Check if path is a mount point itself
        let normalized = Self::normalize_mount_path(path.to_path_buf());
        if self.mounts.contains_key(&normalized) {
            return Ok(Metadata {
                is_dir: true,
                is_file: false,
                is_symlink: false,
                size: 0,
                modified: None,
            });
        }

        let (fs, relative) = self.find_mount(path)?;
        fs.stat(&relative).await
    }

    async fn read_link(&self, path: &Path) -> io::Result<PathBuf> {
        let (fs, relative) = self.find_mount(path)?;
        fs.read_link(&relative).await
    }

    async fn symlink(&self, target: &Path, link: &Path) -> io::Result<()> {
        let (fs, relative) = self.find_mount(link)?;
        fs.symlink(target, &relative).await
    }

    async fn lstat(&self, path: &Path) -> io::Result<Metadata> {
        // Special case: root always exists
        let path_str = path.to_string_lossy();
        if path_str.is_empty() || path_str == "/" {
            return Ok(Metadata {
                is_dir: true,
                is_file: false,
                is_symlink: false,
                size: 0,
                modified: None,
            });
        }

        // Check if path is a mount point itself
        let normalized = Self::normalize_mount_path(path.to_path_buf());
        if self.mounts.contains_key(&normalized) {
            return Ok(Metadata {
                is_dir: true,
                is_file: false,
                is_symlink: false,
                size: 0,
                modified: None,
            });
        }

        let (fs, relative) = self.find_mount(path)?;
        fs.lstat(&relative).await
    }

    async fn mkdir(&self, path: &Path) -> io::Result<()> {
        let (fs, relative) = self.find_mount(path)?;
        fs.mkdir(&relative).await
    }

    async fn remove(&self, path: &Path) -> io::Result<()> {
        let (fs, relative) = self.find_mount(path)?;
        fs.remove(&relative).await
    }

    async fn rename(&self, from: &Path, to: &Path) -> io::Result<()> {
        let (from_fs, from_relative) = self.find_mount(from)?;
        let (to_fs, to_relative) = self.find_mount(to)?;

        // Check if both paths are on the same mount by comparing Arc pointers
        if !Arc::ptr_eq(&from_fs, &to_fs) {
            return Err(io::Error::new(
                io::ErrorKind::Unsupported,
                "cannot rename across different mount points",
            ));
        }

        from_fs.rename(&from_relative, &to_relative).await
    }

    fn read_only(&self) -> bool {
        // Router itself isn't read-only; individual mounts might be
        false
    }
}

impl VfsRouter {
    /// List the root directory, synthesizing entries from mount points.
    async fn list_root(&self) -> io::Result<Vec<DirEntry>> {
        let mut entries = Vec::new();
        let mut seen_names = std::collections::HashSet::new();

        for mount_path in self.mounts.keys() {
            let mount_str = mount_path.to_string_lossy();
            if mount_str == "/" {
                // Root mount: list its contents directly
                if let Some(fs) = self.mounts.get(mount_path)
                    && let Ok(root_entries) = fs.list(Path::new("")).await {
                        for entry in root_entries {
                            if seen_names.insert(entry.name.clone()) {
                                entries.push(entry);
                            }
                        }
                    }
            } else {
                // Non-root mount: extract first path component
                let first_component = mount_str
                    .trim_start_matches('/')
                    .split('/')
                    .next()
                    .unwrap_or("");

                if !first_component.is_empty() && seen_names.insert(first_component.to_string()) {
                    entries.push(DirEntry {
                        name: first_component.to_string(),
                        entry_type: EntryType::Directory,
                        size: 0,
                        symlink_target: None,
                    });
                }
            }
        }

        entries.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(entries)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::MemoryFs;

    #[tokio::test]
    async fn test_basic_mount() {
        let mut router = VfsRouter::new();
        let scratch = MemoryFs::new();
        scratch.write(Path::new("test.txt"), b"hello").await.unwrap();
        router.mount("/scratch", scratch);

        let data = router.read(Path::new("/scratch/test.txt")).await.unwrap();
        assert_eq!(data, b"hello");
    }

    #[tokio::test]
    async fn test_multiple_mounts() {
        let mut router = VfsRouter::new();

        let scratch = MemoryFs::new();
        scratch.write(Path::new("a.txt"), b"scratch").await.unwrap();
        router.mount("/scratch", scratch);

        let data = MemoryFs::new();
        data.write(Path::new("b.txt"), b"data").await.unwrap();
        router.mount("/data", data);

        assert_eq!(
            router.read(Path::new("/scratch/a.txt")).await.unwrap(),
            b"scratch"
        );
        assert_eq!(
            router.read(Path::new("/data/b.txt")).await.unwrap(),
            b"data"
        );
    }

    #[tokio::test]
    async fn test_nested_mount() {
        let mut router = VfsRouter::new();

        let outer = MemoryFs::new();
        outer.write(Path::new("outer.txt"), b"outer").await.unwrap();
        router.mount("/mnt", outer);

        let inner = MemoryFs::new();
        inner.write(Path::new("inner.txt"), b"inner").await.unwrap();
        router.mount("/mnt/project", inner);

        // /mnt/outer.txt should come from outer mount
        assert_eq!(
            router.read(Path::new("/mnt/outer.txt")).await.unwrap(),
            b"outer"
        );

        // /mnt/project/inner.txt should come from inner mount
        assert_eq!(
            router.read(Path::new("/mnt/project/inner.txt")).await.unwrap(),
            b"inner"
        );
    }

    #[tokio::test]
    async fn test_list_root() {
        let mut router = VfsRouter::new();
        router.mount("/scratch", MemoryFs::new());
        router.mount("/mnt/a", MemoryFs::new());
        router.mount("/mnt/b", MemoryFs::new());

        let entries = router.list(Path::new("/")).await.unwrap();
        let names: Vec<_> = entries.iter().map(|e| &e.name).collect();

        assert!(names.contains(&&"scratch".to_string()));
        assert!(names.contains(&&"mnt".to_string()));
    }

    #[tokio::test]
    async fn test_unmount() {
        let mut router = VfsRouter::new();

        let fs = MemoryFs::new();
        fs.write(Path::new("test.txt"), b"data").await.unwrap();
        router.mount("/scratch", fs);

        assert!(router.read(Path::new("/scratch/test.txt")).await.is_ok());

        router.unmount("/scratch");

        assert!(router.read(Path::new("/scratch/test.txt")).await.is_err());
    }

    #[tokio::test]
    async fn test_list_mounts() {
        let mut router = VfsRouter::new();
        router.mount("/scratch", MemoryFs::new());
        router.mount("/data", MemoryFs::new());

        let mounts = router.list_mounts();
        assert_eq!(mounts.len(), 2);

        let paths: Vec<_> = mounts.iter().map(|m| &m.path).collect();
        assert!(paths.contains(&&PathBuf::from("/scratch")));
        assert!(paths.contains(&&PathBuf::from("/data")));
    }

    #[tokio::test]
    async fn test_no_mount_error() {
        let router = VfsRouter::new();
        let result = router.read(Path::new("/nothing/here.txt")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::NotFound);
    }

    #[tokio::test]
    async fn test_root_mount() {
        let mut router = VfsRouter::new();

        let root = MemoryFs::new();
        root.write(Path::new("at-root.txt"), b"root file").await.unwrap();
        router.mount("/", root);

        let data = router.read(Path::new("/at-root.txt")).await.unwrap();
        assert_eq!(data, b"root file");
    }

    #[tokio::test]
    async fn test_write_through_router() {
        let mut router = VfsRouter::new();
        router.mount("/scratch", MemoryFs::new());

        router
            .write(Path::new("/scratch/new.txt"), b"created")
            .await
            .unwrap();

        let data = router.read(Path::new("/scratch/new.txt")).await.unwrap();
        assert_eq!(data, b"created");
    }

    #[tokio::test]
    async fn test_stat_mount_point() {
        let mut router = VfsRouter::new();
        router.mount("/scratch", MemoryFs::new());

        let meta = router.stat(Path::new("/scratch")).await.unwrap();
        assert!(meta.is_dir);
    }

    #[tokio::test]
    async fn test_stat_root() {
        let router = VfsRouter::new();
        let meta = router.stat(Path::new("/")).await.unwrap();
        assert!(meta.is_dir);
    }

    #[tokio::test]
    async fn test_rename_same_mount() {
        let mut router = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("old.txt"), b"data").await.unwrap();
        router.mount("/scratch", mem);

        router.rename(Path::new("/scratch/old.txt"), Path::new("/scratch/new.txt")).await.unwrap();

        // New path exists
        let data = router.read(Path::new("/scratch/new.txt")).await.unwrap();
        assert_eq!(data, b"data");

        // Old path doesn't exist
        assert!(!router.exists(Path::new("/scratch/old.txt")).await);
    }

    #[tokio::test]
    async fn test_rename_cross_mount_fails() {
        let mut router = VfsRouter::new();
        let mem1 = MemoryFs::new();
        mem1.write(Path::new("file.txt"), b"data").await.unwrap();
        router.mount("/mount1", mem1);
        router.mount("/mount2", MemoryFs::new());

        let result = router.rename(Path::new("/mount1/file.txt"), Path::new("/mount2/file.txt")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::Unsupported);
    }
}
