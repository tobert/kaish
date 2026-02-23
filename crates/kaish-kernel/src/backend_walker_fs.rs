//! `WalkerFs` adapter for `KernelBackend`.
//!
//! Bridges kaish-kernel's `KernelBackend` trait to kaish-glob's `WalkerFs`
//! trait so `FileWalker` and `IgnoreFilter` can work with any backend.

use async_trait::async_trait;
use std::path::Path;

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
        self.0.read(path, None).await.map_err(|e| WalkerError::Io(e.to_string()))
    }

    async fn is_dir(&self, path: &Path) -> bool {
        self.0.stat(path).await.is_ok_and(|info| info.is_dir())
    }

    async fn exists(&self, path: &Path) -> bool {
        self.0.exists(path).await
    }
}
