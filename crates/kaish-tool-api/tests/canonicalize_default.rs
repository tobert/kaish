//! Pins `KernelBackend::canonicalize`'s default walk (crates/kaish-tool-api/
//! src/backend.rs) against drift. `KernelBackend` paths are VFS-absolute
//! (see this method's doc comment), unlike kaish-vfs's `Filesystem` trait,
//! which is root-relative in and root-relative out and whose `symlink()`
//! refuses an absolute target outright — so kaish-vfs's own conformance
//! suite (crates/kaish-vfs/src/conformance.rs) has no root or
//! absolute-target case to mirror. This file covers those two on its own,
//! alongside the shape the vfs suite does share: `.`/`..` folding, a
//! relative symlink target, a missing intermediate vs. final component,
//! and the `MAX_SYMLINK_HOPS` loop cap.
//!
//! `FakeBackend` is a minimal in-test `KernelBackend`: only `lstat` and
//! `read_link` do real work, since those are the only two methods the
//! default `canonicalize` walk calls.
//!
//! `crates/kaish-kernel/tests/canonicalize_default_matches_local_backend_tests.rs`
//! covers the output-namespace agreement between this default and
//! `LocalBackend`'s override directly, driving both off one shared
//! `VfsRouter`; this file pins the default's own behavior in isolation.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use async_trait::async_trait;

use kaish_tool_api::{KernelBackend, ToolCtx};
use kaish_types::backend::{
    BackendError, BackendResult, MountInfo, PatchOp, ReadRange, ToolInfo, ToolResult, WriteMode,
};
use kaish_types::{DirEntry, ToolArgs};

enum Kind {
    Dir,
    File,
    Symlink(PathBuf),
}

/// A flat path -> entry map. No real I/O, no containment: it exists only to
/// drive the trait's default `canonicalize` through `lstat`/`read_link`.
/// Every key is VFS-absolute, matching what the walk actually looks up
/// (it starts from "/", so even a relative `canonicalize` input reaches
/// `lstat`/`read_link` as an absolute path).
struct FakeBackend {
    entries: Mutex<HashMap<PathBuf, Kind>>,
}

impl FakeBackend {
    fn new() -> Self {
        Self { entries: Mutex::new(HashMap::new()) }
    }

    fn dir(mut self, path: &str) -> Self {
        self.entries.get_mut().unwrap().insert(PathBuf::from(path), Kind::Dir);
        self
    }

    fn file(mut self, path: &str) -> Self {
        self.entries.get_mut().unwrap().insert(PathBuf::from(path), Kind::File);
        self
    }

    fn symlink(mut self, path: &str, target: &str) -> Self {
        self.entries
            .get_mut()
            .unwrap()
            .insert(PathBuf::from(path), Kind::Symlink(PathBuf::from(target)));
        self
    }
}

#[async_trait]
impl KernelBackend for FakeBackend {
    async fn read(&self, _path: &Path, _range: Option<ReadRange>) -> BackendResult<Vec<u8>> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn write(&self, _path: &Path, _content: &[u8], _mode: WriteMode) -> BackendResult<()> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn append(&self, _path: &Path, _content: &[u8]) -> BackendResult<()> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn patch(&self, _path: &Path, _ops: &[PatchOp]) -> BackendResult<()> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn list(&self, _path: &Path) -> BackendResult<Vec<DirEntry>> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn stat(&self, _path: &Path) -> BackendResult<DirEntry> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn mkdir(&self, _path: &Path) -> BackendResult<()> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn set_mtime(&self, _path: &Path, _mtime: std::time::SystemTime) -> BackendResult<()> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn remove(&self, _path: &Path, _recursive: bool) -> BackendResult<()> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn rename(&self, _from: &Path, _to: &Path) -> BackendResult<()> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn exists(&self, _path: &Path) -> bool {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn lstat(&self, path: &Path) -> BackendResult<DirEntry> {
        let entries = self.entries.lock().unwrap();
        match entries.get(path) {
            Some(Kind::Dir) => Ok(DirEntry::directory("x")),
            Some(Kind::File) => Ok(DirEntry::file("x", 0)),
            Some(Kind::Symlink(target)) => Ok(DirEntry::symlink("x", target.clone())),
            None => Err(BackendError::NotFound(path.display().to_string())),
        }
    }

    async fn read_link(&self, path: &Path) -> BackendResult<PathBuf> {
        let entries = self.entries.lock().unwrap();
        match entries.get(path) {
            Some(Kind::Symlink(target)) => Ok(target.clone()),
            Some(_) => Err(BackendError::InvalidOperation(format!(
                "not a symlink: {}",
                path.display()
            ))),
            None => Err(BackendError::NotFound(path.display().to_string())),
        }
    }

    async fn symlink(&self, _target: &Path, _link: &Path) -> BackendResult<()> {
        unimplemented!("fixtures are built via the FakeBackend builder, not this call")
    }

    async fn call_tool(
        &self,
        _name: &str,
        _args: ToolArgs,
        _ctx: &mut dyn ToolCtx,
    ) -> BackendResult<ToolResult> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn list_tools(&self) -> BackendResult<Vec<ToolInfo>> {
        unimplemented!("not exercised by canonicalize tests")
    }

    async fn get_tool(&self, _name: &str) -> BackendResult<Option<ToolInfo>> {
        unimplemented!("not exercised by canonicalize tests")
    }

    fn read_only(&self) -> bool {
        false
    }

    fn backend_type(&self) -> &str {
        "fake"
    }

    fn mounts(&self) -> Vec<MountInfo> {
        Vec::new()
    }

    fn resolve_real_path(&self, _path: &Path) -> Option<PathBuf> {
        None
    }
}

#[tokio::test]
async fn dot_and_dotdot_fold_lexically() {
    let backend = FakeBackend::new().dir("/a").dir("/a/x").file("/a/b");

    let resolved = backend
        .canonicalize(Path::new("./a/./x/../b"), false)
        .await
        .expect("canonicalize should fold . and .. and resolve");
    assert_eq!(resolved, PathBuf::from("/a/b"));
}

#[tokio::test]
async fn missing_intermediate_component_errors_even_with_allow_missing_final() {
    let backend = FakeBackend::new();

    let err = backend
        .canonicalize(Path::new("missing/file"), true)
        .await
        .expect_err("a missing intermediate component must error regardless of the flag");
    assert!(
        matches!(err, BackendError::NotFound(_)),
        "expected BackendError::NotFound, got {err:?}"
    );
}

#[tokio::test]
async fn missing_final_component_errors_by_default() {
    let backend = FakeBackend::new().dir("/d");

    let err = backend
        .canonicalize(Path::new("d/missing"), false)
        .await
        .expect_err("a missing final component must error when allow_missing_final is false");
    assert!(
        matches!(err, BackendError::NotFound(_)),
        "expected BackendError::NotFound, got {err:?}"
    );
}

#[tokio::test]
async fn missing_final_component_allowed_when_flag_is_set() {
    let backend = FakeBackend::new().dir("/d");

    let resolved = backend
        .canonicalize(Path::new("d/missing"), true)
        .await
        .expect("a missing final component is allowed when allow_missing_final is true");
    assert_eq!(resolved, PathBuf::from("/d/missing"));
}

#[tokio::test]
async fn symlink_with_relative_target_resolves() {
    let backend = FakeBackend::new()
        .symlink("/link", "realdir")
        .dir("/realdir")
        .file("/realdir/file");

    let resolved = backend
        .canonicalize(Path::new("link/file"), false)
        .await
        .expect("canonicalize should follow the relative symlink target");
    assert_eq!(resolved, PathBuf::from("/realdir/file"));
}

#[tokio::test]
async fn symlink_with_absolute_target_resolves() {
    let backend = FakeBackend::new()
        .symlink("/link", "/abs/target")
        .dir("/abs/target")
        .file("/abs/target/file");

    let resolved = backend
        .canonicalize(Path::new("link/file"), false)
        .await
        .expect("canonicalize should follow the absolute symlink target");
    assert_eq!(resolved, PathBuf::from("/abs/target/file"));
}

#[tokio::test]
async fn root_resolves_to_slash() {
    let backend = FakeBackend::new();

    let resolved = backend
        .canonicalize(Path::new("/"), false)
        .await
        .expect("root canonicalizes without touching the backend");
    assert_eq!(resolved, PathBuf::from("/"));
}

#[tokio::test]
async fn relative_input_canonicalizes_to_an_absolute_path() {
    // KernelBackend paths are VFS-absolute in and out (see the doc comment
    // on the trait method): a relative input is anchored at "/", not at
    // some cwd the backend has no way to know. A builtin resolves against
    // its own cwd (ToolCtx::resolve_path) before ever calling this.
    let backend = FakeBackend::new().dir("/a").file("/a/b");

    let resolved = backend
        .canonicalize(Path::new("a/b"), false)
        .await
        .expect("a relative input still canonicalizes");
    assert_eq!(resolved, PathBuf::from("/a/b"));
}

#[tokio::test]
async fn symlink_loop_errors_instead_of_hanging_or_stopping_silently() {
    // stat_on_a_link_loop_errors_instead_of_hanging (kaish-vfs conformance)
    // pins MAX_SYMLINK_HOPS for stat; canonicalize walks its own hop-by-hop
    // loop through lstat/read_link and needs the same guarantee pinned
    // here. The trait doc only promises "an error, never a silent stop" —
    // not a specific BackendError variant or message — so this checks
    // exactly that contract: it errors, and it does so within a bound
    // instead of hanging.
    let backend = FakeBackend::new().symlink("/a", "b").symlink("/b", "a");

    let result = tokio::time::timeout(
        std::time::Duration::from_secs(2),
        backend.canonicalize(Path::new("a"), false),
    )
    .await
    .expect("canonicalize must not hang on a symlink loop");
    assert!(
        result.is_err(),
        "a symlink loop must error, not silently stop at some resolved path: {result:?}"
    );
}
