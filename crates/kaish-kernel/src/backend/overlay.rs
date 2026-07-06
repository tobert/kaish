//! VirtualOverlayBackend: Routes /v/* paths to internal VFS while delegating everything else.
//!
//! This backend is designed for embedders who provide their own `KernelBackend` but want
//! kaish's virtual filesystems (like `/v/jobs` for job observability) to work automatically.
//!
//! # Usage
//!
//! Prefer using `Kernel::with_backend()` which handles overlay setup automatically:
//!
//! ```ignore
//! let kernel = Kernel::with_backend(my_backend, config, |vfs| {
//!     vfs.mount_arc("/v/docs", docs_fs);
//! }, |_| {})?;
//! ```
//!
//! # Path Routing
//!
//! Routing is by *mount coverage* (longest prefix), not by a lexical `/v`
//! reservation:
//!
//! - Any path covered by a mount on the internal VFS (`/v/jobs`, `/v/blobs`,
//!   `/dev`, a `configure_vfs` mount) → routed to the VFS.
//! - An *unclaimed* path under `/v` (e.g. an embedder's own `/v/cas`) → falls
//!   through to the custom backend, so an embedder can mount its own storage
//!   under `/v` without kaish shadowing it. (Previously the whole `/v`
//!   namespace was reserved and a miss returned `NotFound`.)
//! - A shared *ancestor* directory like `/v` — no mount of its own, but sitting
//!   above `/v/jobs` — is presented as the *union* of both layers: `list`
//!   merges the embedder's entries with kaish's child mounts, and `stat`/
//!   `exists` synthesize it as a directory when the embedder lacks it.
//! - Everything else → custom backend

use async_trait::async_trait;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use super::{
    BackendError, BackendResult, KernelBackend, LocalBackend, PatchOp, ReadRange,
    ToolInfo, ToolResult, WriteMode,
};
use crate::tools::{ToolArgs, ToolCtx};
use crate::vfs::{DirEntry, Filesystem, MountInfo, VfsRouter};

/// The final path component, used to name a synthesized directory entry for a
/// shared ancestor (`/v` → `v`). Falls back to `/` for a component-less path.
fn dir_basename(path: &Path) -> String {
    path.file_name()
        .map(|n| n.to_string_lossy().into_owned())
        .unwrap_or_else(|| "/".to_string())
}

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

    /// Check if a path is *covered by a kaish VFS mount* and should therefore be
    /// handled by the VFS rather than the inner backend.
    ///
    /// Routing is purely by mount coverage (longest prefix) — there is no
    /// lexical `/v` reservation. `/v/jobs`, `/dev`, and any `configure_vfs`
    /// mount route to the VFS; an *unclaimed* path under `/v` (e.g. an embedder
    /// CAS at `/v/cas`) falls through to the inner backend instead of returning
    /// `NotFound`. Shared *ancestor* directories like `/v` (which have no mount
    /// of their own but sit above `/v/jobs`) are not "virtual" by this test —
    /// they're handled by the union/synthesis paths in `list`/`stat`/`exists`.
    fn is_virtual_path(&self, path: &Path) -> bool {
        self.vfs.has_mount(path)
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
        if self.is_virtual_path(path) {
            Ok(self.vfs.read_range(path, range).await?)
        } else {
            self.inner.read(path, range).await
        }
    }

    async fn write(&self, path: &Path, content: &[u8], mode: WriteMode) -> BackendResult<()> {
        if self.is_virtual_path(path) {
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
                // WriteMode is #[non_exhaustive] — treat unknown modes as Overwrite
                _ => {
                    self.vfs.write(path, content).await?;
                }
            }
            Ok(())
        } else {
            self.inner.write(path, content, mode).await
        }
    }

    async fn set_mtime(&self, path: &Path, mtime: std::time::SystemTime) -> BackendResult<()> {
        if self.is_virtual_path(path) {
            self.vfs.set_mtime(path, mtime).await?;
            Ok(())
        } else {
            self.inner.set_mtime(path, mtime).await
        }
    }

    async fn append(&self, path: &Path, content: &[u8]) -> BackendResult<()> {
        if self.is_virtual_path(path) {
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
        if self.is_virtual_path(path) {
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

    async fn list(&self, path: &Path) -> BackendResult<Vec<DirEntry>> {
        if self.is_virtual_path(path) {
            Ok(self.vfs.list(path).await?)
        } else if self.vfs.has_mount_under(path) {
            // A shared parent of kaish mounts (`/`, `/v`, `/v/etc`, …): return
            // the union of the embedder's view and kaish's child mounts, so
            // both layers' entries show at the directory. kaish entries take
            // precedence on a name clash (an explicit kaish mount wins its
            // subtree via longest-prefix routing). A `NotFound` from the inner
            // backend means "embedder has nothing here" — kaish-only, not an
            // error — but any other inner error propagates.
            let mut names = std::collections::HashSet::new();
            let mut entries = Vec::new();
            for entry in self.vfs.list(path).await? {
                names.insert(entry.name.clone());
                entries.push(entry);
            }
            match self.inner.list(path).await {
                Ok(inner_entries) => {
                    for entry in inner_entries {
                        if names.insert(entry.name.clone()) {
                            entries.push(entry);
                        }
                    }
                }
                Err(BackendError::NotFound(_)) => {}
                Err(e) => return Err(e),
            }
            Ok(entries)
        } else {
            self.inner.list(path).await
        }
    }

    async fn stat(&self, path: &Path) -> BackendResult<DirEntry> {
        if self.is_virtual_path(path) {
            Ok(self.vfs.stat(path).await?)
        } else {
            match self.inner.stat(path).await {
                // A shared ancestor of kaish mounts (e.g. `/v`) the embedder
                // doesn't have still exists as a directory on our side.
                Err(BackendError::NotFound(_)) if self.vfs.has_mount_under(path) => {
                    Ok(DirEntry::directory(dir_basename(path)))
                }
                other => other,
            }
        }
    }

    async fn mkdir(&self, path: &Path) -> BackendResult<()> {
        if self.is_virtual_path(path) {
            self.vfs.mkdir(path).await?;
            Ok(())
        } else {
            self.inner.mkdir(path).await
        }
    }

    async fn remove(&self, path: &Path, recursive: bool) -> BackendResult<()> {
        if self.is_virtual_path(path) {
            if recursive
                && let Ok(entry) = self.vfs.lstat(path).await
                && entry.is_dir()
                && let Ok(entries) = self.vfs.list(path).await
            {
                for entry in entries {
                    let child_path = path.join(&entry.name);
                    Box::pin(self.remove(&child_path, true)).await?;
                }
            }
            self.vfs.remove(path).await?;
            Ok(())
        } else {
            self.inner.remove(path, recursive).await
        }
    }

    async fn rename(&self, from: &Path, to: &Path) -> BackendResult<()> {
        let from_virtual = self.is_virtual_path(from);
        let to_virtual = self.is_virtual_path(to);

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
        if self.is_virtual_path(path) {
            self.vfs.exists(path).await
        } else {
            // A shared ancestor of kaish mounts (e.g. `/v`) exists even when the
            // embedder has nothing there.
            self.inner.exists(path).await || self.vfs.has_mount_under(path)
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Symlink Operations
    // ═══════════════════════════════════════════════════════════════════════════

    async fn lstat(&self, path: &Path) -> BackendResult<DirEntry> {
        if self.is_virtual_path(path) {
            Ok(self.vfs.lstat(path).await?)
        } else {
            match self.inner.lstat(path).await {
                Err(BackendError::NotFound(_)) if self.vfs.has_mount_under(path) => {
                    Ok(DirEntry::directory(dir_basename(path)))
                }
                other => other,
            }
        }
    }

    async fn read_link(&self, path: &Path) -> BackendResult<PathBuf> {
        if self.is_virtual_path(path) {
            Ok(self.vfs.read_link(path).await?)
        } else {
            self.inner.read_link(path).await
        }
    }

    async fn symlink(&self, target: &Path, link: &Path) -> BackendResult<()> {
        if self.is_virtual_path(link) {
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
        ctx: &mut dyn ToolCtx,
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
        if self.is_virtual_path(path) {
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

        // Production-like split: kaish mounts sit at /v/*, nothing at /v itself.
        let mut vfs = VfsRouter::new();
        let blobs = MemoryFs::new();
        blobs.write(Path::new("test.bin"), b"blob data").await.unwrap();
        vfs.mount("/v/blobs", blobs);
        vfs.mount("/v/jobs", MemoryFs::new());

        VirtualOverlayBackend::new(inner, Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_virtual_path_detection() {
        let overlay = make_overlay().await;
        // Covered by a kaish mount → virtual.
        assert!(overlay.is_virtual_path(Path::new("/v/jobs")));
        assert!(overlay.is_virtual_path(Path::new("/v/blobs")));
        assert!(overlay.is_virtual_path(Path::new("/v/blobs/test.bin")));

        // No lexical /v reservation any more: a shared ancestor with no mount of
        // its own, and an unclaimed path under /v, are NOT virtual — they route
        // to the union/synthesis paths or fall through to the embedder.
        assert!(!overlay.is_virtual_path(Path::new("/v")));
        assert!(!overlay.is_virtual_path(Path::new("/v/")));
        assert!(!overlay.is_virtual_path(Path::new("/v/unclaimed")));

        assert!(!overlay.is_virtual_path(Path::new("/docs")));
        assert!(!overlay.is_virtual_path(Path::new("/g/repo")));
        assert!(!overlay.is_virtual_path(Path::new("/")));
        assert!(!overlay.is_virtual_path(Path::new("/var")));
    }

    #[tokio::test]
    async fn test_non_v_mount_is_virtual_path() {
        // A mount outside /v (e.g. Kernel::with_backend's /dev) must also be
        // routed to the internal VFS, not silently delegated to the inner
        // backend — this is the bug that let writes to /dev/null fail as
        // "read-only filesystem" when the inner backend was read-only.
        let (mock, _) = MockBackend::new();
        let inner: Arc<dyn KernelBackend> = Arc::new(mock);
        let mut vfs = VfsRouter::new();
        vfs.mount("/dev", MemoryFs::new());
        let overlay = VirtualOverlayBackend::new(inner, Arc::new(vfs));

        assert!(overlay.is_virtual_path(Path::new("/dev/null")));
        assert!(!overlay.is_virtual_path(Path::new("/docs")));
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
        assert!(info.is_file());
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
        // Under a covered mount (/v/blobs), so it stays in the kaish VFS.
        overlay.mkdir(Path::new("/v/blobs/newdir")).await.unwrap();
        assert!(overlay.exists(Path::new("/v/blobs/newdir")).await);
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

    // Inner backend that serves content under /v (an embedder CAS at /v/cas),
    // alongside kaish's own /v/jobs + /dev mounts — the production-like split
    // (kaish mounts sit at /v/*, nothing at /v itself). `cas` toggles whether
    // the embedder actually has content under /v.
    async fn overlay_over_inner(cas: bool) -> VirtualOverlayBackend {
        let mut inner_router = VfsRouter::new();
        let inner_mem = MemoryFs::new();
        if cas {
            inner_mem
                .write(Path::new("v/cas/blob.bin"), b"cas data")
                .await
                .unwrap();
        }
        inner_router.mount("/", inner_mem);
        let inner: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(inner_router)));

        let mut vfs = VfsRouter::new();
        vfs.mount("/v/jobs", MemoryFs::new());
        vfs.mount("/dev", MemoryFs::new());
        VirtualOverlayBackend::new(inner, Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_unclaimed_v_reaches_inner_backend() {
        // /v/cas isn't mounted on kaish's side → a read must fall through to the
        // embedder's backend instead of the old NotFound reservation.
        let overlay = overlay_over_inner(true).await;
        let data = overlay.read(Path::new("/v/cas/blob.bin"), None).await.unwrap();
        assert_eq!(data, b"cas data");
        assert!(overlay.exists(Path::new("/v/cas/blob.bin")).await);
    }

    #[tokio::test]
    async fn test_v_listing_unions_kaish_and_inner() {
        let overlay = overlay_over_inner(true).await;
        let names: Vec<String> = overlay
            .list(Path::new("/v"))
            .await
            .unwrap()
            .into_iter()
            .map(|e| e.name)
            .collect();
        assert!(names.iter().any(|n| n == "jobs"), "kaish mount missing: {names:?}");
        assert!(names.iter().any(|n| n == "cas"), "embedder mount missing: {names:?}");
    }

    #[tokio::test]
    async fn test_v_synthesized_when_inner_lacks_it() {
        // Embedder has nothing under /v; kaish mounts /v/jobs. /v must still
        // stat as a directory and list the kaish mount, so `cd /v` / `ls /v`
        // work even though nothing is mounted at /v on either layer.
        let overlay = overlay_over_inner(false).await;
        assert!(overlay.stat(Path::new("/v")).await.unwrap().is_dir());
        assert!(overlay.lstat(Path::new("/v")).await.unwrap().is_dir());
        assert!(overlay.exists(Path::new("/v")).await);
        let names: Vec<String> = overlay
            .list(Path::new("/v"))
            .await
            .unwrap()
            .into_iter()
            .map(|e| e.name)
            .collect();
        assert_eq!(names, vec!["jobs".to_string()]);
    }

    #[tokio::test]
    async fn test_root_lists_both_v_and_dev() {
        // The generalized union at shared parents fixes the old root merge,
        // which injected only a synthetic `v` and dropped `dev`.
        let overlay = overlay_over_inner(false).await;
        let names: Vec<String> = overlay
            .list(Path::new("/"))
            .await
            .unwrap()
            .into_iter()
            .map(|e| e.name)
            .collect();
        assert!(names.iter().any(|n| n == "v"), "{names:?}");
        assert!(names.iter().any(|n| n == "dev"), "{names:?}");
    }
}
