//! VirtualOverlayBackend: Routes /v/* paths to internal VFS while delegating everything else.
//!
//! This backend is designed for embedders who provide their own `KernelBackend` but want
//! kaish's virtual filesystems (like `/v/jobs` for job observability) to work automatically.
//!
//! # Usage
//!
//! ```ignore
//! use kaish_kernel::{VirtualOverlayBackend, KernelBackend, VfsRouter, JobFs};
//!
//! // Your custom backend for /docs, /g, etc.
//! let my_backend: Arc<dyn KernelBackend> = Arc::new(MyBackend::new());
//!
//! // Internal VFS with JobFs mounted
//! let mut vfs = VfsRouter::new();
//! vfs.mount("/v/jobs", JobFs::new(job_manager.clone()));
//! vfs.mount("/v/blobs", MemoryFs::new());
//!
//! // Wrap your backend with virtual overlay
//! let backend = VirtualOverlayBackend::new(my_backend, Arc::new(vfs));
//!
//! // Now /v/* routes to VFS, everything else to your backend
//! let kernel = Kernel::with_backend(Arc::new(backend), config)?;
//! ```
//!
//! # Path Routing
//!
//! - `/v/*` → Internal VFS (JobFs, MemoryFs for blobs, etc.)
//! - Everything else → Custom backend

use async_trait::async_trait;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::UNIX_EPOCH;

use super::{
    BackendError, BackendResult, EntryInfo, KernelBackend, LocalBackend, PatchOp, ReadRange,
    ToolInfo, ToolResult, WriteMode,
};
use crate::tools::{ExecContext, ToolArgs};
use crate::vfs::{EntryType, Filesystem, MountInfo, VfsRouter};

/// Backend that overlays virtual paths (`/v/*`) on top of a custom backend.
///
/// This enables embedders to provide their own storage backend while still
/// getting kaish's virtual filesystem features like `/v/jobs` for job observability.
pub struct VirtualOverlayBackend {
    /// Custom backend for most paths (embedder-provided).
    inner: Arc<dyn KernelBackend>,
    /// VFS for /v/* paths (internal virtual filesystems).
    vfs: Arc<VfsRouter>,
}

impl VirtualOverlayBackend {
    /// Create a new virtual overlay backend.
    ///
    /// # Arguments
    ///
    /// * `inner` - The custom backend to delegate non-virtual paths to
    /// * `vfs` - VFS router containing virtual filesystem mounts (typically at /v/*)
    ///
    /// # Example
    ///
    /// ```ignore
    /// let overlay = VirtualOverlayBackend::new(my_backend, vfs);
    /// ```
    pub fn new(inner: Arc<dyn KernelBackend>, vfs: Arc<VfsRouter>) -> Self {
        Self { inner, vfs }
    }

    /// Check if a path should be handled by the VFS (virtual paths).
    fn is_virtual_path(path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        path_str == "/v" || path_str.starts_with("/v/")
    }

    /// Get the inner backend.
    pub fn inner(&self) -> &Arc<dyn KernelBackend> {
        &self.inner
    }

    /// Get the VFS router.
    pub fn vfs(&self) -> &Arc<VfsRouter> {
        &self.vfs
    }
}

impl std::fmt::Debug for VirtualOverlayBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("VirtualOverlayBackend")
            .field("inner_type", &self.inner.backend_type())
            .field("vfs", &self.vfs)
            .finish()
    }
}

#[async_trait]
impl KernelBackend for VirtualOverlayBackend {
    // ═══════════════════════════════════════════════════════════════════════════
    // File Operations
    // ═══════════════════════════════════════════════════════════════════════════

    async fn read(&self, path: &Path, range: Option<ReadRange>) -> BackendResult<Vec<u8>> {
        if Self::is_virtual_path(path) {
            let content = self.vfs.read(path).await?;
            match range {
                Some(r) => Ok(LocalBackend::apply_read_range(&content, &r)),
                None => Ok(content),
            }
        } else {
            self.inner.read(path, range).await
        }
    }

    async fn write(&self, path: &Path, content: &[u8], mode: WriteMode) -> BackendResult<()> {
        if Self::is_virtual_path(path) {
            match mode {
                WriteMode::CreateNew => {
                    if self.vfs.exists(path).await {
                        return Err(BackendError::AlreadyExists(path.display().to_string()));
                    }
                    self.vfs.write(path, content).await?;
                }
                WriteMode::Overwrite | WriteMode::Truncate => {
                    self.vfs.write(path, content).await?;
                }
                WriteMode::UpdateOnly => {
                    if !self.vfs.exists(path).await {
                        return Err(BackendError::NotFound(path.display().to_string()));
                    }
                    self.vfs.write(path, content).await?;
                }
            }
            Ok(())
        } else {
            self.inner.write(path, content, mode).await
        }
    }

    async fn append(&self, path: &Path, content: &[u8]) -> BackendResult<()> {
        if Self::is_virtual_path(path) {
            let mut existing = match self.vfs.read(path).await {
                Ok(data) => data,
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => Vec::new(),
                Err(e) => return Err(e.into()),
            };
            existing.extend_from_slice(content);
            self.vfs.write(path, &existing).await?;
            Ok(())
        } else {
            self.inner.append(path, content).await
        }
    }

    async fn patch(&self, path: &Path, ops: &[PatchOp]) -> BackendResult<()> {
        if Self::is_virtual_path(path) {
            // Read existing content
            let data = self.vfs.read(path).await?;
            let mut content = String::from_utf8(data)
                .map_err(|e| BackendError::InvalidOperation(format!("file is not valid UTF-8: {}", e)))?;

            // Apply each patch operation
            for op in ops {
                LocalBackend::apply_patch_op(&mut content, op)?;
            }

            // Write back
            self.vfs.write(path, content.as_bytes()).await?;
            Ok(())
        } else {
            self.inner.patch(path, ops).await
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Directory Operations
    // ═══════════════════════════════════════════════════════════════════════════

    async fn list(&self, path: &Path) -> BackendResult<Vec<EntryInfo>> {
        if Self::is_virtual_path(path) {
            let entries = self.vfs.list(path).await?;
            Ok(entries
                .into_iter()
                .map(|e| {
                    let (is_dir, is_file, is_symlink) = match e.entry_type {
                        EntryType::Directory => (true, false, false),
                        EntryType::File => (false, true, false),
                        EntryType::Symlink => (false, false, true),
                    };
                    EntryInfo {
                        name: e.name,
                        is_dir,
                        is_file,
                        is_symlink,
                        size: e.size,
                        modified: None,
                        permissions: None,
                        symlink_target: e.symlink_target,
                    }
                })
                .collect())
        } else if path.to_string_lossy() == "/" || path.to_string_lossy().is_empty() {
            // Root listing: combine inner backend's root with /v
            let mut entries = self.inner.list(path).await?;
            // Add /v if not already present
            if !entries.iter().any(|e| e.name == "v") {
                entries.push(EntryInfo::directory("v"));
            }
            Ok(entries)
        } else {
            self.inner.list(path).await
        }
    }

    async fn stat(&self, path: &Path) -> BackendResult<EntryInfo> {
        if Self::is_virtual_path(path) {
            let meta = self.vfs.stat(path).await?;
            let modified = meta.modified.and_then(|t| {
                t.duration_since(UNIX_EPOCH).ok().map(|d| d.as_secs())
            });
            Ok(EntryInfo {
                name: path
                    .file_name()
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_else(|| "v".to_string()),
                is_dir: meta.is_dir,
                is_file: meta.is_file,
                is_symlink: meta.is_symlink,
                size: meta.size,
                modified,
                permissions: None,
                symlink_target: None,
            })
        } else {
            self.inner.stat(path).await
        }
    }

    async fn mkdir(&self, path: &Path) -> BackendResult<()> {
        if Self::is_virtual_path(path) {
            self.vfs.mkdir(path).await?;
            Ok(())
        } else {
            self.inner.mkdir(path).await
        }
    }

    async fn remove(&self, path: &Path, recursive: bool) -> BackendResult<()> {
        if Self::is_virtual_path(path) {
            if recursive {
                if let Ok(meta) = self.vfs.stat(path).await
                    && meta.is_dir
                {
                    if let Ok(entries) = self.vfs.list(path).await {
                        for entry in entries {
                            let child_path = path.join(&entry.name);
                            Box::pin(self.remove(&child_path, true)).await?;
                        }
                    }
                }
            }
            self.vfs.remove(path).await?;
            Ok(())
        } else {
            self.inner.remove(path, recursive).await
        }
    }

    async fn rename(&self, from: &Path, to: &Path) -> BackendResult<()> {
        let from_virtual = Self::is_virtual_path(from);
        let to_virtual = Self::is_virtual_path(to);

        if from_virtual != to_virtual {
            return Err(BackendError::InvalidOperation(
                "cannot rename between virtual and non-virtual paths".into(),
            ));
        }

        if from_virtual {
            self.vfs.rename(from, to).await?;
            Ok(())
        } else {
            self.inner.rename(from, to).await
        }
    }

    async fn exists(&self, path: &Path) -> bool {
        if Self::is_virtual_path(path) {
            self.vfs.exists(path).await
        } else {
            self.inner.exists(path).await
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Symlink Operations
    // ═══════════════════════════════════════════════════════════════════════════

    async fn read_link(&self, path: &Path) -> BackendResult<PathBuf> {
        if Self::is_virtual_path(path) {
            Ok(self.vfs.read_link(path).await?)
        } else {
            self.inner.read_link(path).await
        }
    }

    async fn symlink(&self, target: &Path, link: &Path) -> BackendResult<()> {
        if Self::is_virtual_path(link) {
            self.vfs.symlink(target, link).await?;
            Ok(())
        } else {
            self.inner.symlink(target, link).await
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Tool Dispatch
    // ═══════════════════════════════════════════════════════════════════════════

    async fn call_tool(
        &self,
        name: &str,
        args: ToolArgs,
        ctx: &mut ExecContext,
    ) -> BackendResult<ToolResult> {
        // Tools are dispatched through the inner backend
        self.inner.call_tool(name, args, ctx).await
    }

    async fn list_tools(&self) -> BackendResult<Vec<ToolInfo>> {
        self.inner.list_tools().await
    }

    async fn get_tool(&self, name: &str) -> BackendResult<Option<ToolInfo>> {
        self.inner.get_tool(name).await
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Backend Information
    // ═══════════════════════════════════════════════════════════════════════════

    fn read_only(&self) -> bool {
        // We're not read-only if either layer is writable
        self.inner.read_only() && self.vfs.read_only()
    }

    fn backend_type(&self) -> &str {
        "virtual-overlay"
    }

    fn mounts(&self) -> Vec<MountInfo> {
        let mut mounts = self.inner.mounts();
        mounts.extend(self.vfs.list_mounts());
        mounts
    }

    fn resolve_real_path(&self, path: &Path) -> Option<PathBuf> {
        if Self::is_virtual_path(path) {
            // Virtual paths don't map to real filesystem
            None
        } else {
            self.inner.resolve_real_path(path)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::backend::testing::MockBackend;
    use crate::vfs::MemoryFs;

    async fn make_overlay() -> VirtualOverlayBackend {
        // Create mock inner backend
        let (mock, _) = MockBackend::new();
        let inner: Arc<dyn KernelBackend> = Arc::new(mock);

        // Create VFS with /v mounted
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("blobs/test.bin"), b"blob data").await.unwrap();
        mem.mkdir(Path::new("jobs")).await.unwrap();
        vfs.mount("/v", mem);

        VirtualOverlayBackend::new(inner, Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_virtual_path_detection() {
        assert!(VirtualOverlayBackend::is_virtual_path(Path::new("/v")));
        assert!(VirtualOverlayBackend::is_virtual_path(Path::new("/v/")));
        assert!(VirtualOverlayBackend::is_virtual_path(Path::new("/v/jobs")));
        assert!(VirtualOverlayBackend::is_virtual_path(Path::new("/v/blobs/test.bin")));

        assert!(!VirtualOverlayBackend::is_virtual_path(Path::new("/docs")));
        assert!(!VirtualOverlayBackend::is_virtual_path(Path::new("/g/repo")));
        assert!(!VirtualOverlayBackend::is_virtual_path(Path::new("/")));
        assert!(!VirtualOverlayBackend::is_virtual_path(Path::new("/var")));
    }

    #[tokio::test]
    async fn test_read_virtual_path() {
        let overlay = make_overlay().await;
        let content = overlay.read(Path::new("/v/blobs/test.bin"), None).await.unwrap();
        assert_eq!(content, b"blob data");
    }

    #[tokio::test]
    async fn test_write_virtual_path() {
        let overlay = make_overlay().await;
        overlay
            .write(Path::new("/v/blobs/new.bin"), b"new data", WriteMode::Overwrite)
            .await
            .unwrap();
        let content = overlay.read(Path::new("/v/blobs/new.bin"), None).await.unwrap();
        assert_eq!(content, b"new data");
    }

    #[tokio::test]
    async fn test_list_virtual_path() {
        let overlay = make_overlay().await;
        let entries = overlay.list(Path::new("/v")).await.unwrap();
        let names: Vec<&str> = entries.iter().map(|e| e.name.as_str()).collect();
        assert!(names.contains(&"blobs"));
        assert!(names.contains(&"jobs"));
    }

    #[tokio::test]
    async fn test_root_listing_includes_v() {
        let overlay = make_overlay().await;
        let entries = overlay.list(Path::new("/")).await.unwrap();
        let names: Vec<&str> = entries.iter().map(|e| e.name.as_str()).collect();
        assert!(names.contains(&"v"), "Root listing should include 'v' directory");
    }

    #[tokio::test]
    async fn test_stat_virtual_path() {
        let overlay = make_overlay().await;
        let info = overlay.stat(Path::new("/v/blobs/test.bin")).await.unwrap();
        assert!(info.is_file);
        assert_eq!(info.size, 9); // "blob data".len()
    }

    #[tokio::test]
    async fn test_exists_virtual_path() {
        let overlay = make_overlay().await;
        assert!(overlay.exists(Path::new("/v/blobs/test.bin")).await);
        assert!(!overlay.exists(Path::new("/v/blobs/nonexistent")).await);
    }

    #[tokio::test]
    async fn test_mkdir_virtual_path() {
        let overlay = make_overlay().await;
        overlay.mkdir(Path::new("/v/newdir")).await.unwrap();
        assert!(overlay.exists(Path::new("/v/newdir")).await);
    }

    #[tokio::test]
    async fn test_remove_virtual_path() {
        let overlay = make_overlay().await;
        overlay.remove(Path::new("/v/blobs/test.bin"), false).await.unwrap();
        assert!(!overlay.exists(Path::new("/v/blobs/test.bin")).await);
    }

    #[tokio::test]
    async fn test_rename_within_virtual() {
        let overlay = make_overlay().await;
        overlay
            .rename(Path::new("/v/blobs/test.bin"), Path::new("/v/blobs/renamed.bin"))
            .await
            .unwrap();
        assert!(!overlay.exists(Path::new("/v/blobs/test.bin")).await);
        assert!(overlay.exists(Path::new("/v/blobs/renamed.bin")).await);
    }

    #[tokio::test]
    async fn test_rename_across_boundary_fails() {
        let overlay = make_overlay().await;
        let result = overlay
            .rename(Path::new("/v/blobs/test.bin"), Path::new("/docs/test.bin"))
            .await;
        assert!(matches!(result, Err(BackendError::InvalidOperation(_))));
    }

    #[tokio::test]
    async fn test_backend_type() {
        let overlay = make_overlay().await;
        assert_eq!(overlay.backend_type(), "virtual-overlay");
    }

    #[tokio::test]
    async fn test_resolve_real_path_virtual() {
        let overlay = make_overlay().await;
        // Virtual paths don't resolve to real paths
        assert!(overlay.resolve_real_path(Path::new("/v/blobs/test.bin")).is_none());
    }
}
