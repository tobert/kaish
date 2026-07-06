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

/// Explanatory clause for errors on a shared-ancestor path (see
/// `VirtualOverlayBackend::is_shared_ancestor`), so a rejected mutation reads
/// clearly instead of the misleading `NotFound` a bare inner delegation gives.
fn synth_dir_note(path: &Path) -> String {
    format!("{} is a synthesized directory that only holds kaish mounts", path.display())
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

    /// A *shared ancestor*: a path that is not itself covered by a kaish mount
    /// but sits above one (`/`, `/v`, `/v/etc`, …). It is presented as a
    /// **read-only synthesized directory** — a union of the embedder's view and
    /// kaish's child mounts. Reads and listing treat it as a directory; every
    /// direct mutation is rejected, because the node exists only insofar as
    /// kaish mounts live beneath it, so there is nothing coherent for the
    /// embedder alone to create, remove, or re-time.
    fn is_shared_ancestor(&self, path: &Path) -> bool {
        !self.is_virtual_path(path) && self.vfs.has_mount_under(path)
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
        } else if self.is_shared_ancestor(path) {
            Err(BackendError::IsDirectory(synth_dir_note(path)))
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
        } else if self.is_shared_ancestor(path) {
            Err(BackendError::IsDirectory(synth_dir_note(path)))
        } else {
            self.inner.write(path, content, mode).await
        }
    }

    async fn set_mtime(&self, path: &Path, mtime: std::time::SystemTime) -> BackendResult<()> {
        if self.is_virtual_path(path) {
            self.vfs.set_mtime(path, mtime).await?;
            Ok(())
        } else if self.is_shared_ancestor(path) {
            Err(BackendError::InvalidOperation(format!("cannot set mtime: {}", synth_dir_note(path))))
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
        } else if self.is_shared_ancestor(path) {
            Err(BackendError::IsDirectory(synth_dir_note(path)))
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
        } else if self.is_shared_ancestor(path) {
            Err(BackendError::IsDirectory(synth_dir_note(path)))
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
        } else if self.is_shared_ancestor(path) {
            // A shared parent of kaish mounts (`/`, `/v`, `/v/etc`, …): return
            // the union of the embedder's view and kaish's child mounts. The
            // embedder's entries come first so they carry real metadata; then
            // for each kaish child, an explicit kaish *mount* (`/dev`, `/v/jobs`)
            // shadows the embedder's same-named entry (longest-prefix routing
            // owns that subtree), while a synthesized *intermediate* (`/v` under
            // `/`, when nothing is mounted at `/v`) only fills a gap — keeping
            // the embedder's real entry if it has one. Any inner error (the
            // embedder has no listable directory here: `NotFound`,
            // `NotADirectory` over an inner file, a permission error) means
            // "embedder contributes nothing"; kaish's mounts still list.
            let mut by_name: std::collections::HashMap<String, DirEntry> =
                std::collections::HashMap::new();
            if let Ok(inner_entries) = self.inner.list(path).await {
                for entry in inner_entries {
                    by_name.insert(entry.name.clone(), entry);
                }
            }
            for entry in self.vfs.list(path).await? {
                if self.vfs.has_mount(&path.join(&entry.name)) {
                    by_name.insert(entry.name.clone(), entry);
                } else {
                    by_name.entry(entry.name.clone()).or_insert(entry);
                }
            }
            let mut entries: Vec<DirEntry> = by_name.into_values().collect();
            entries.sort_by(|a, b| a.name.cmp(&b.name));
            Ok(entries)
        } else {
            self.inner.list(path).await
        }
    }

    async fn stat(&self, path: &Path) -> BackendResult<DirEntry> {
        if self.is_virtual_path(path) {
            Ok(self.vfs.stat(path).await?)
        } else if self.is_shared_ancestor(path) {
            // A shared ancestor is a directory (kaish mounts live under it).
            // Prefer the embedder's real directory metadata; otherwise (embedder
            // lacks it, has a file there, or errors) synthesize a plain dir.
            match self.inner.stat(path).await {
                Ok(entry) if entry.is_dir() => Ok(entry),
                _ => Ok(DirEntry::directory(dir_basename(path))),
            }
        } else {
            self.inner.stat(path).await
        }
    }

    async fn mkdir(&self, path: &Path) -> BackendResult<()> {
        if self.is_virtual_path(path) {
            self.vfs.mkdir(path).await?;
            Ok(())
        } else if self.is_shared_ancestor(path) {
            Err(BackendError::AlreadyExists(synth_dir_note(path)))
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
        } else if self.is_shared_ancestor(path) {
            // Refuse even `rm -rf /v`: the node holds kaish-managed mounts that
            // don't live on the embedder's backend, so it can't be removed.
            Err(BackendError::InvalidOperation(format!("cannot remove: {}", synth_dir_note(path))))
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

        if self.is_shared_ancestor(from) || self.is_shared_ancestor(to) {
            return Err(BackendError::InvalidOperation(format!(
                "cannot rename: {} is a synthesized directory",
                if self.is_shared_ancestor(from) { from.display() } else { to.display() }
            )));
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
            // A shared ancestor (e.g. `/v`) exists as a directory regardless of
            // the embedder — consistent with `stat`/`list`.
            self.is_shared_ancestor(path) || self.inner.exists(path).await
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Symlink Operations
    // ═══════════════════════════════════════════════════════════════════════════

    async fn lstat(&self, path: &Path) -> BackendResult<DirEntry> {
        if self.is_virtual_path(path) {
            Ok(self.vfs.lstat(path).await?)
        } else if self.is_shared_ancestor(path) {
            match self.inner.lstat(path).await {
                Ok(entry) if entry.is_dir() => Ok(entry),
                _ => Ok(DirEntry::directory(dir_basename(path))),
            }
        } else {
            self.inner.lstat(path).await
        }
    }

    async fn read_link(&self, path: &Path) -> BackendResult<PathBuf> {
        if self.is_virtual_path(path) {
            Ok(self.vfs.read_link(path).await?)
        } else if self.is_shared_ancestor(path) {
            Err(BackendError::InvalidOperation(format!(
                "{} is a directory, not a symlink",
                path.display()
            )))
        } else {
            self.inner.read_link(path).await
        }
    }

    async fn symlink(&self, target: &Path, link: &Path) -> BackendResult<()> {
        if self.is_virtual_path(link) {
            self.vfs.symlink(target, link).await?;
            Ok(())
        } else if self.is_shared_ancestor(link) {
            Err(BackendError::AlreadyExists(synth_dir_note(link)))
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

    #[cfg(feature = "localfs")]
    #[tokio::test]
    async fn test_unclaimed_v_resolves_to_inner_real_path() {
        use crate::vfs::LocalFs;
        // Embedder root with real content under /v.
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("v/cas")).unwrap();
        std::fs::write(dir.path().join("v/cas/blob.bin"), b"x").unwrap();

        let mut inner_router = VfsRouter::new();
        inner_router.mount("/", LocalFs::read_only(dir.path().to_path_buf()));
        let inner: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(inner_router)));
        let mut vfs = VfsRouter::new();
        vfs.mount("/v/jobs", MemoryFs::new());
        let overlay = VirtualOverlayBackend::new(inner, Arc::new(vfs));

        // Unclaimed /v/* now resolves to the embedder's REAL path (was `None`
        // under the old lexical reservation). This is the path that reaches the
        // trash/latch gate, so `is_trash_excluded` must NOT lexically exclude
        // `/v` — otherwise this real content silently loses its safety net
        // (see tools/context.rs).
        let real = overlay.resolve_real_path(Path::new("/v/cas/blob.bin"));
        assert!(real.is_some(), "unclaimed /v/* must resolve to the embedder real path");
        assert!(real.unwrap().ends_with("v/cas/blob.bin"));
        // A kaish-owned /v mount stays virtual — no real path.
        assert!(overlay.resolve_real_path(Path::new("/v/jobs/1")).is_none());
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

    #[tokio::test]
    async fn test_shared_ancestor_is_a_directory_even_over_an_inner_file() {
        // Embedder has a FILE at /v, but kaish mounts /v/jobs beneath it. /v is
        // authoritatively a directory (kaish mounts live under it); stat/exists/
        // list all agree — no file leaks from stat, no `NotADirectory` leaks
        // from list. This branch also subsumes a non-`NotFound` inner *error*
        // (e.g. PermissionDenied): existence/type never depend on inner here.
        let inner_mem = MemoryFs::new();
        inner_mem.write(Path::new("v"), b"i am a file").await.unwrap();
        let mut inner_router = VfsRouter::new();
        inner_router.mount("/", inner_mem);
        let inner: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(inner_router)));
        let mut vfs = VfsRouter::new();
        vfs.mount("/v/jobs", MemoryFs::new());
        let overlay = VirtualOverlayBackend::new(inner, Arc::new(vfs));

        assert!(overlay.stat(Path::new("/v")).await.unwrap().is_dir(), "kaish dir wins over inner file");
        assert!(overlay.lstat(Path::new("/v")).await.unwrap().is_dir());
        assert!(overlay.exists(Path::new("/v")).await);
        let names: Vec<String> = overlay
            .list(Path::new("/v"))
            .await
            .unwrap()
            .into_iter()
            .map(|e| e.name)
            .collect();
        assert_eq!(names, vec!["jobs".to_string()], "lists kaish mount; no NotADirectory error");
    }

    #[cfg(feature = "localfs")]
    #[tokio::test]
    async fn test_listing_keeps_inner_real_metadata_for_intermediate_child() {
        use crate::vfs::LocalFs;
        // Embedder has a real /v directory; kaish mounts /v/jobs and /dev.
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("v")).unwrap();

        let mut inner_router = VfsRouter::new();
        inner_router.mount("/", LocalFs::read_only(dir.path().to_path_buf()));
        let inner: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(inner_router)));
        let mut vfs = VfsRouter::new();
        vfs.mount("/v/jobs", MemoryFs::new());
        vfs.mount("/dev", MemoryFs::new());
        let overlay = VirtualOverlayBackend::new(inner, Arc::new(vfs));

        let entries = overlay.list(Path::new("/")).await.unwrap();
        let v = entries.iter().find(|e| e.name == "v").expect("v listed");
        let dev = entries.iter().find(|e| e.name == "dev").expect("dev listed");
        // `/v` is an *intermediate* (no kaish mount at /v), so the embedder's
        // real dir entry — carrying real metadata — is kept, not the synthesized
        // zero-metadata one.
        assert!(v.is_dir());
        assert!(v.modified.is_some(), "intermediate child keeps inner real metadata");
        // `/dev` IS a kaish mount, so kaish's entry wins (synthesized, no mtime).
        assert!(dev.is_dir());
        assert!(dev.modified.is_none(), "real kaish mount shadows inner");
    }

    #[tokio::test]
    async fn test_mutations_on_shared_ancestor_are_rejected_clearly() {
        // Every direct mutation of the synthesized `/v` must fail with a clear
        // error, not the misleading `NotFound` inner delegation produced — since
        // stat/ls/exists all report it present.
        let overlay = overlay_over_inner(false).await;

        assert!(
            matches!(overlay.mkdir(Path::new("/v")).await, Err(BackendError::AlreadyExists(_))),
            "mkdir on an existing synthesized dir → AlreadyExists"
        );
        assert!(
            matches!(overlay.remove(Path::new("/v"), true).await, Err(BackendError::InvalidOperation(_))),
            "remove of a synthesized dir that holds kaish mounts → InvalidOperation"
        );
        assert!(
            matches!(
                overlay.set_mtime(Path::new("/v"), std::time::SystemTime::now()).await,
                Err(BackendError::InvalidOperation(_))
            ),
            "set_mtime (touch) on a synthesized dir → InvalidOperation"
        );
        assert!(
            matches!(
                overlay.write(Path::new("/v"), b"x", WriteMode::Overwrite).await,
                Err(BackendError::IsDirectory(_))
            ),
            "write to a synthesized dir → IsDirectory"
        );
        assert!(
            matches!(overlay.read(Path::new("/v"), None).await, Err(BackendError::IsDirectory(_))),
            "read of a synthesized dir → IsDirectory"
        );
    }
}
