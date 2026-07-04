//! Test utilities for backend module.

use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use async_trait::async_trait;

use super::{
    BackendError, BackendResult, KernelBackend, PatchOp, ReadRange, ToolInfo, ToolResult, WriteMode,
};
use crate::tools::{ToolArgs, ToolCtx};
use crate::vfs::{DirEntry, MountInfo};

/// Mock backend that counts call_tool invocations.
/// Used to verify tool dispatch routes through the backend.
pub struct MockBackend {
    pub call_count: Arc<AtomicUsize>,
    /// Overrides `call_tool`'s return value when set — lets a test drive an
    /// arbitrary `ToolResult` (structured `data`/`content_type`/`baggage`) or
    /// a `BackendError` other than `ToolNotFound` through the real dispatch
    /// path, instead of always getting `ToolResult::success("mock executed")`.
    #[allow(clippy::type_complexity)]
    tool_result: Option<Arc<dyn Fn(&str) -> BackendResult<ToolResult> + Send + Sync>>,
}

impl MockBackend {
    pub fn new() -> (Self, Arc<AtomicUsize>) {
        let count = Arc::new(AtomicUsize::new(0));
        (Self { call_count: count.clone(), tool_result: None }, count)
    }

    /// Get the current call count.
    pub fn calls(&self) -> usize {
        self.call_count.load(Ordering::SeqCst)
    }

    /// Configure what `call_tool` returns for every invocation, keyed by the
    /// tool name it was called with.
    pub fn with_tool_result(
        mut self,
        result: impl Fn(&str) -> BackendResult<ToolResult> + Send + Sync + 'static,
    ) -> Self {
        self.tool_result = Some(Arc::new(result));
        self
    }
}

impl Default for MockBackend {
    fn default() -> Self {
        Self {
            call_count: Arc::new(AtomicUsize::new(0)),
            tool_result: None,
        }
    }
}

#[async_trait]
impl KernelBackend for MockBackend {
    async fn read(&self, _path: &Path, _range: Option<ReadRange>) -> BackendResult<Vec<u8>> {
        Ok(Vec::new())
    }

    async fn write(&self, _path: &Path, _content: &[u8], _mode: WriteMode) -> BackendResult<()> {
        Ok(())
    }

    async fn append(&self, _path: &Path, _content: &[u8]) -> BackendResult<()> {
        Ok(())
    }

    async fn patch(&self, _path: &Path, _ops: &[PatchOp]) -> BackendResult<()> {
        Ok(())
    }

    async fn list(&self, _path: &Path) -> BackendResult<Vec<DirEntry>> {
        Ok(Vec::new())
    }

    async fn stat(&self, _path: &Path) -> BackendResult<DirEntry> {
        Ok(DirEntry::file("mock", 0))
    }

    async fn mkdir(&self, _path: &Path) -> BackendResult<()> {
        Ok(())
    }

    async fn set_mtime(&self, _path: &Path, _mtime: std::time::SystemTime) -> BackendResult<()> {
        Ok(())
    }

    async fn remove(&self, _path: &Path, _recursive: bool) -> BackendResult<()> {
        Ok(())
    }

    async fn rename(&self, _from: &Path, _to: &Path) -> BackendResult<()> {
        Ok(())
    }

    async fn exists(&self, _path: &Path) -> bool {
        false
    }

    async fn call_tool(
        &self,
        name: &str,
        _args: ToolArgs,
        _ctx: &mut dyn ToolCtx,
    ) -> BackendResult<ToolResult> {
        self.call_count.fetch_add(1, Ordering::SeqCst);
        if let Some(f) = &self.tool_result {
            return f(name);
        }
        Ok(ToolResult::success(format!("mock executed: {}", name)))
    }

    async fn list_tools(&self) -> BackendResult<Vec<ToolInfo>> {
        Ok(Vec::new())
    }

    async fn get_tool(&self, _name: &str) -> BackendResult<Option<ToolInfo>> {
        Ok(None)
    }

    fn read_only(&self) -> bool {
        false
    }

    fn backend_type(&self) -> &str {
        "mock"
    }

    fn mounts(&self) -> Vec<MountInfo> {
        Vec::new()
    }

    fn resolve_real_path(&self, _path: &Path) -> Option<std::path::PathBuf> {
        None
    }

    async fn lstat(&self, _path: &Path) -> BackendResult<DirEntry> {
        Ok(DirEntry::file("mock", 0))
    }

    async fn read_link(&self, _path: &Path) -> BackendResult<std::path::PathBuf> {
        Err(BackendError::InvalidOperation("mock backend does not support symlinks".to_string()))
    }

    async fn symlink(&self, _target: &Path, _link: &Path) -> BackendResult<()> {
        Err(BackendError::InvalidOperation("mock backend does not support symlinks".to_string()))
    }
}
