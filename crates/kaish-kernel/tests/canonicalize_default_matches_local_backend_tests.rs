//! `KernelBackend::canonicalize`'s default (kaish-tool-api/src/backend.rs)
//! and `LocalBackend`'s override (`VfsRouter::canonicalize`) must agree on
//! the same VFS-absolute answer for the same input — `KernelBackend` paths
//! are absolute throughout the kernel, and nothing else pins the two walks
//! against drifting apart.
//!
//! `DefaultCanonicalizeBackend` delegates every method to a real
//! `LocalBackend` except `canonicalize`, which is left at the trait
//! default. Both backends read the same `Arc<VfsRouter>`, so the default
//! walk sees exactly the `lstat`/`read_link` data the override reads —
//! this compares the two implementations against one fixture, not two
//! separately populated ones that could quietly diverge on their own.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;

use async_trait::async_trait;

use kaish_kernel::tools::{ToolArgs, ToolCtx};
use kaish_kernel::vfs::{DirEntry, Filesystem, MemoryFs, MountInfo, VfsRouter};
use kaish_kernel::{
    BackendResult, KernelBackend, LocalBackend, PatchOp, ReadRange, ToolInfo, ToolResult,
    WriteMode,
};

/// Delegates everything to an inner `LocalBackend` except `canonicalize`.
struct DefaultCanonicalizeBackend(LocalBackend);

#[async_trait]
impl KernelBackend for DefaultCanonicalizeBackend {
    async fn read(&self, path: &Path, range: Option<ReadRange>) -> BackendResult<Vec<u8>> {
        self.0.read(path, range).await
    }

    async fn write(&self, path: &Path, content: &[u8], mode: WriteMode) -> BackendResult<()> {
        self.0.write(path, content, mode).await
    }

    async fn append(&self, path: &Path, content: &[u8]) -> BackendResult<()> {
        self.0.append(path, content).await
    }

    async fn patch(&self, path: &Path, ops: &[PatchOp]) -> BackendResult<()> {
        self.0.patch(path, ops).await
    }

    async fn list(&self, path: &Path) -> BackendResult<Vec<DirEntry>> {
        self.0.list(path).await
    }

    async fn stat(&self, path: &Path) -> BackendResult<DirEntry> {
        self.0.stat(path).await
    }

    async fn mkdir(&self, path: &Path) -> BackendResult<()> {
        self.0.mkdir(path).await
    }

    async fn set_mtime(&self, path: &Path, mtime: SystemTime) -> BackendResult<()> {
        self.0.set_mtime(path, mtime).await
    }

    async fn remove(&self, path: &Path, recursive: bool) -> BackendResult<()> {
        self.0.remove(path, recursive).await
    }

    async fn rename(&self, from: &Path, to: &Path) -> BackendResult<()> {
        self.0.rename(from, to).await
    }

    async fn exists(&self, path: &Path) -> bool {
        self.0.exists(path).await
    }

    async fn lstat(&self, path: &Path) -> BackendResult<DirEntry> {
        self.0.lstat(path).await
    }

    async fn read_link(&self, path: &Path) -> BackendResult<PathBuf> {
        self.0.read_link(path).await
    }

    async fn symlink(&self, target: &Path, link: &Path) -> BackendResult<()> {
        self.0.symlink(target, link).await
    }

    // `canonicalize` is deliberately NOT overridden: this backend exercises
    // `KernelBackend`'s default walk.

    async fn call_tool(
        &self,
        name: &str,
        args: ToolArgs,
        ctx: &mut dyn ToolCtx,
    ) -> BackendResult<ToolResult> {
        self.0.call_tool(name, args, ctx).await
    }

    async fn list_tools(&self) -> BackendResult<Vec<ToolInfo>> {
        self.0.list_tools().await
    }

    async fn get_tool(&self, name: &str) -> BackendResult<Option<ToolInfo>> {
        self.0.get_tool(name).await
    }

    fn read_only(&self) -> bool {
        self.0.read_only()
    }

    fn backend_type(&self) -> &str {
        "default-canonicalize-passthrough"
    }

    fn mounts(&self) -> Vec<MountInfo> {
        self.0.mounts()
    }

    fn resolve_real_path(&self, path: &Path) -> Option<PathBuf> {
        self.0.resolve_real_path(path)
    }
}

/// One `Arc<VfsRouter>`, two backends over it: `LocalBackend`'s own
/// `canonicalize` override, and the trait default via the passthrough.
async fn make_backends() -> (LocalBackend, DefaultCanonicalizeBackend) {
    let mut router = VfsRouter::new();
    let mem = MemoryFs::new();
    mem.mkdir(Path::new("a")).await.expect("mkdir a");
    mem.mkdir(Path::new("a/x")).await.expect("mkdir a/x");
    mem.write(Path::new("a/b"), b"content").await.expect("write a/b");
    mem.mkdir(Path::new("realdir")).await.expect("mkdir realdir");
    mem.write(Path::new("realdir/file"), b"content")
        .await
        .expect("write realdir/file");
    mem.symlink(Path::new("realdir"), Path::new("link"))
        .await
        .expect("symlink link -> realdir");
    router.mount("/", mem);
    let vfs = Arc::new(router);

    let overridden = LocalBackend::new(vfs.clone());
    let default = DefaultCanonicalizeBackend(LocalBackend::new(vfs));
    (overridden, default)
}

#[tokio::test]
async fn default_canonicalize_matches_local_backend_override() {
    let (overridden, default) = make_backends().await;

    let cases: &[(&str, bool)] = &[
        ("/", false),
        ("/a/b", false),
        ("a/b", false),
        ("/./a/./x/../b", false),
        ("a/./x/../b", false),
        ("/link/file", false),
        ("/a/missing", true),
        ("/a/missing", false),
    ];

    for (input, allow_missing_final) in cases {
        let expected = overridden.canonicalize(Path::new(input), *allow_missing_final).await;
        let actual = default.canonicalize(Path::new(input), *allow_missing_final).await;
        match (&expected, &actual) {
            (Ok(e), Ok(a)) => assert_eq!(
                e, a,
                "canonicalize({input:?}, allow_missing_final={allow_missing_final}): \
                 LocalBackend override={e:?}, KernelBackend default={a:?}"
            ),
            // Both refused: the exact BackendError variant/message is not a
            // shared contract between the two walks, only that they agree
            // on whether the input resolves at all.
            (Err(_), Err(_)) => {}
            _ => panic!(
                "canonicalize({input:?}, allow_missing_final={allow_missing_final}) disagreed \
                 on success: LocalBackend override={expected:?}, KernelBackend default={actual:?}"
            ),
        }
    }
}
