//! `WalkerFs` adapter for `KernelBackend`.
//!
//! Bridges kaish-kernel's `KernelBackend` trait to kaish-glob's `WalkerFs`
//! trait so `FileWalker` and `IgnoreFilter` can work with any backend.

use async_trait::async_trait;
use std::path::{Path, PathBuf};

use crate::backend::KernelBackend;
use crate::vfs::DirEntry;
use kaish_glob::{WalkerDirEntry, WalkerError, WalkerFs};

/// Newtype wrapper to impl the external `WalkerDirEntry` trait for `DirEntry`
/// (orphan rule: both types are now in external crates).
pub struct BackendDirEntry(pub DirEntry);

impl WalkerDirEntry for BackendDirEntry {
    fn name(&self) -> &str {
        &self.0.name
    }

    fn is_dir(&self) -> bool {
        self.0.is_dir()
    }

    fn is_file(&self) -> bool {
        self.0.is_file()
    }

    fn is_symlink(&self) -> bool {
        self.0.is_symlink()
    }
}

/// Wraps a `&dyn KernelBackend` to implement `WalkerFs`.
pub struct BackendWalkerFs<'a>(pub &'a dyn KernelBackend);

#[async_trait]
impl WalkerFs for BackendWalkerFs<'_> {
    type DirEntry = BackendDirEntry;

    async fn list_dir(&self, path: &Path) -> Result<Vec<BackendDirEntry>, WalkerError> {
        self.0
            .list(path)
            .await
            .map(|entries| entries.into_iter().map(BackendDirEntry).collect())
            .map_err(|e| WalkerError::Io(e.to_string()))
    }

    async fn read_file(&self, path: &Path) -> Result<Vec<u8>, WalkerError> {
        self.0
            .read_with_purpose(path, None, crate::backend::ReadPurpose::Internal)
            .await
            .map_err(|e| WalkerError::Io(e.to_string()))
    }

    async fn is_dir(&self, path: &Path) -> bool {
        self.0.stat(path).await.is_ok_and(|info| info.is_dir())
    }

    async fn exists(&self, path: &Path) -> bool {
        self.0.exists(path).await
    }

    async fn file_size(&self, path: &Path) -> Option<u64> {
        self.0.stat(path).await.ok().map(|info| info.size)
    }

    fn walk_boundaries(&self) -> Vec<PathBuf> {
        self.0.walk_boundaries()
    }

    async fn canonicalize(&self, path: &Path) -> PathBuf {
        self.0
            .canonicalize(path, false)
            .await
            .unwrap_or_else(|_| path.to_path_buf())
    }
}

#[cfg(all(test, unix, feature = "localfs"))]
mod tests {
    #![allow(clippy::unwrap_used, clippy::expect_used)]

    use super::*;
    use std::sync::Arc;
    use crate::backend::LocalBackend;
    use crate::vfs::{LocalFs, VfsRouter};
    use kaish_glob::{FileWalker, WalkOptions};

    #[tokio::test]
    async fn follow_symlinks_descends_dir_symlink_and_stops_on_cycle() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path();
        let real_dir = root.join("real");
        std::fs::create_dir(&real_dir).unwrap();
        std::fs::write(real_dir.join("inside.txt"), "hello").unwrap();

        // Directory symlink pointing to `real`, plus a cycle back to `root`.
        std::os::unix::fs::symlink(&real_dir, root.join("link_to_real")).unwrap();
        std::os::unix::fs::symlink(root, real_dir.join("cycle_to_root")).unwrap();

        let mut vfs = VfsRouter::new();
        vfs.mount("/", LocalFs::new(root.to_path_buf()));
        let backend = LocalBackend::new(Arc::new(vfs));
        let walker_fs = BackendWalkerFs(&backend);

        let walker = FileWalker::new(&walker_fs, Path::new("/"))
            .with_options(WalkOptions {
                follow_symlinks: true,
                ..WalkOptions::default()
            });
        let paths = walker.collect().await.unwrap();
        let names: Vec<String> = paths
            .iter()
            .map(|p| p.to_string_lossy().into_owned())
            .collect();
        assert!(
            names.iter().any(|p| p.ends_with("inside.txt")),
            "expected inside.txt via directory walk: {names:?}"
        );
        // Should terminate cleanly without infinite recursion through cycle_to_root.
        assert!(
            names.len() <= 10,
            "cycle detection should prevent infinite descent: {names:?}"
        );
    }
}
