//! Test utilities for backend module.

use std::path::Path;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use async_trait::async_trait;

use super::{
    BackendResult, EntryInfo, KernelBackend, PatchOp, ReadRange, ToolInfo, ToolResult, WriteMode,
};
use crate::tools::{ExecContext, ToolArgs};
use crate::vfs::MountInfo;

/// Mock backend that counts call_tool invocations.
/// Used to verify tool dispatch routes through the backend.
pub struct MockBackend {
    pub call_count: Arc<AtomicUsize>,
}

impl MockBackend {
    pub fn new() -> (Self, Arc<AtomicUsize>) {
        let count = Arc::new(AtomicUsize::new(0));
        (Self { call_count: count.clone() }, count)
    }

    /// Get the current call count.
    pub fn calls(&self) -> usize {
        self.call_count.load(Ordering::SeqCst)
    }
}

impl Default for MockBackend {
    fn default() -> Self {
        Self {
            call_count: Arc::new(AtomicUsize::new(0)),
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

    async fn list(&self, _path: &Path) -> BackendResult<Vec<EntryInfo>> {
        Ok(Vec::new())
    }

    async fn stat(&self, _path: &Path) -> BackendResult<EntryInfo> {
        Ok(EntryInfo::file("mock", 0))
    }

    async fn mkdir(&self, _path: &Path) -> BackendResult<()> {
        Ok(())
    }

    async fn remove(&self, _path: &Path, _recursive: bool) -> BackendResult<()> {
        Ok(())
    }

    async fn exists(&self, _path: &Path) -> bool {
        false
    }

    async fn call_tool(
        &self,
        name: &str,
        _args: ToolArgs,
        _ctx: &mut ExecContext,
    ) -> BackendResult<ToolResult> {
        self.call_count.fetch_add(1, Ordering::SeqCst);
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
}
