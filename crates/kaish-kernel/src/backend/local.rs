//! LocalBackend implementation wrapping VfsRouter.
//!
//! This is the default backend for standalone kaish operation.
//! It delegates file operations to VfsRouter and tool dispatch to ToolRegistry.

use async_trait::async_trait;
use std::path::Path;
use std::sync::Arc;
use std::time::UNIX_EPOCH;

use super::{
    BackendError, BackendResult, ConflictError, EntryInfo, KernelBackend, PatchOp, ReadRange,
    ToolInfo, ToolResult, WriteMode,
};
use crate::tools::{ExecContext, ToolArgs, ToolRegistry};
use crate::vfs::{EntryType, Filesystem, MountInfo, VfsRouter};

/// Local backend implementation using VfsRouter and ToolRegistry.
///
/// This is the default backend for standalone kaish operation. It:
/// - Delegates file operations to `VfsRouter` (handles mount points)
/// - Delegates tool dispatch to `ToolRegistry` (builtins, MCP, user tools)
pub struct LocalBackend {
    /// Virtual filesystem router with mount points.
    vfs: Arc<VfsRouter>,
    /// Tool registry for external tool dispatch.
    tools: Option<Arc<ToolRegistry>>,
}

impl LocalBackend {
    /// Create a new LocalBackend with the given VFS.
    pub fn new(vfs: Arc<VfsRouter>) -> Self {
        Self { vfs, tools: None }
    }

    /// Create a LocalBackend with both VFS and tool registry.
    pub fn with_tools(vfs: Arc<VfsRouter>, tools: Arc<ToolRegistry>) -> Self {
        Self {
            vfs,
            tools: Some(tools),
        }
    }

    /// Get the underlying VfsRouter.
    pub fn vfs(&self) -> &Arc<VfsRouter> {
        &self.vfs
    }

    /// Get the underlying ToolRegistry (if set).
    pub fn tools(&self) -> Option<&Arc<ToolRegistry>> {
        self.tools.as_ref()
    }

    /// Apply a single patch operation to file content.
    fn apply_patch_op(content: &mut String, op: &PatchOp) -> BackendResult<()> {
        match op {
            PatchOp::Insert { offset, content: insert_content } => {
                if *offset > content.len() {
                    return Err(BackendError::InvalidOperation(format!(
                        "insert offset {} exceeds content length {}",
                        offset,
                        content.len()
                    )));
                }
                content.insert_str(*offset, insert_content);
            }

            PatchOp::Delete { offset, len, expected } => {
                let end = offset.saturating_add(*len);
                if end > content.len() {
                    return Err(BackendError::InvalidOperation(format!(
                        "delete range {}..{} exceeds content length {}",
                        offset, end, content.len()
                    )));
                }
                // CAS check
                if let Some(expected_content) = expected {
                    let actual = &content[*offset..end];
                    if actual != expected_content {
                        return Err(BackendError::Conflict(ConflictError {
                            location: format!("offset {}", offset),
                            expected: expected_content.clone(),
                            actual: actual.to_string(),
                        }));
                    }
                }
                content.drain(*offset..end);
            }

            PatchOp::Replace {
                offset,
                len,
                content: replace_content,
                expected,
            } => {
                let end = offset.saturating_add(*len);
                if end > content.len() {
                    return Err(BackendError::InvalidOperation(format!(
                        "replace range {}..{} exceeds content length {}",
                        offset, end, content.len()
                    )));
                }
                // CAS check
                if let Some(expected_content) = expected {
                    let actual = &content[*offset..end];
                    if actual != expected_content {
                        return Err(BackendError::Conflict(ConflictError {
                            location: format!("offset {}", offset),
                            expected: expected_content.clone(),
                            actual: actual.to_string(),
                        }));
                    }
                }
                content.replace_range(*offset..end, replace_content);
            }

            PatchOp::InsertLine { line, content: insert_content } => {
                let lines: Vec<&str> = content.lines().collect();
                let line_idx = line.saturating_sub(1); // Convert to 0-indexed
                if line_idx > lines.len() {
                    return Err(BackendError::InvalidOperation(format!(
                        "line {} exceeds line count {}",
                        line,
                        lines.len()
                    )));
                }
                let mut new_lines: Vec<String> = lines.iter().map(|s| s.to_string()).collect();
                new_lines.insert(line_idx, insert_content.clone());
                *content = new_lines.join("\n");
                // Preserve trailing newline if original had one
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }

            PatchOp::DeleteLine { line, expected } => {
                let lines: Vec<&str> = content.lines().collect();
                let line_idx = line.saturating_sub(1); // Convert to 0-indexed
                if line_idx >= lines.len() {
                    return Err(BackendError::InvalidOperation(format!(
                        "line {} exceeds line count {}",
                        line,
                        lines.len()
                    )));
                }
                // CAS check
                if let Some(expected_content) = expected {
                    let actual = lines[line_idx];
                    if actual != expected_content {
                        return Err(BackendError::Conflict(ConflictError {
                            location: format!("line {}", line),
                            expected: expected_content.clone(),
                            actual: actual.to_string(),
                        }));
                    }
                }
                let mut new_lines: Vec<String> = lines.iter().map(|s| s.to_string()).collect();
                new_lines.remove(line_idx);
                *content = new_lines.join("\n");
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }

            PatchOp::ReplaceLine {
                line,
                content: replace_content,
                expected,
            } => {
                let lines: Vec<&str> = content.lines().collect();
                let line_idx = line.saturating_sub(1); // Convert to 0-indexed
                if line_idx >= lines.len() {
                    return Err(BackendError::InvalidOperation(format!(
                        "line {} exceeds line count {}",
                        line,
                        lines.len()
                    )));
                }
                // CAS check
                if let Some(expected_content) = expected {
                    let actual = lines[line_idx];
                    if actual != expected_content {
                        return Err(BackendError::Conflict(ConflictError {
                            location: format!("line {}", line),
                            expected: expected_content.clone(),
                            actual: actual.to_string(),
                        }));
                    }
                }
                let mut new_lines: Vec<String> = lines.iter().map(|s| s.to_string()).collect();
                new_lines[line_idx] = replace_content.clone();
                *content = new_lines.join("\n");
                if !content.is_empty() && !content.ends_with('\n') {
                    content.push('\n');
                }
            }

            PatchOp::Append { content: append_content } => {
                content.push_str(append_content);
            }
        }
        Ok(())
    }

    /// Apply range filter to file content.
    fn apply_read_range(content: &[u8], range: &ReadRange) -> Vec<u8> {
        // Handle byte-based range
        if range.offset.is_some() || range.limit.is_some() {
            let offset = range.offset.unwrap_or(0) as usize;
            let limit = range.limit.map(|l| l as usize).unwrap_or(content.len());
            let end = (offset + limit).min(content.len());
            return content.get(offset..end).unwrap_or(&[]).to_vec();
        }

        // Handle line-based range
        if range.start_line.is_some() || range.end_line.is_some() {
            let content_str = match std::str::from_utf8(content) {
                Ok(s) => s,
                Err(_) => return content.to_vec(), // Return full content if not valid UTF-8
            };
            let lines: Vec<&str> = content_str.lines().collect();
            let start = range.start_line.unwrap_or(1).saturating_sub(1);
            let end = range.end_line.unwrap_or(lines.len()).min(lines.len());
            let selected: Vec<&str> = lines.get(start..end).unwrap_or(&[]).to_vec();
            let mut result = selected.join("\n");
            // Preserve trailing newline only when reading to implicit end (no end_line specified)
            // and the original content had a trailing newline
            if range.end_line.is_none() && content_str.ends_with('\n') && !result.is_empty() {
                result.push('\n');
            }
            return result.into_bytes();
        }

        content.to_vec()
    }
}

#[async_trait]
impl KernelBackend for LocalBackend {
    // ═══════════════════════════════════════════════════════════════════════════
    // File Operations
    // ═══════════════════════════════════════════════════════════════════════════

    async fn read(&self, path: &Path, range: Option<ReadRange>) -> BackendResult<Vec<u8>> {
        let content = self.vfs.read(path).await?;
        match range {
            Some(r) => Ok(Self::apply_read_range(&content, &r)),
            None => Ok(content),
        }
    }

    async fn write(&self, path: &Path, content: &[u8], mode: WriteMode) -> BackendResult<()> {
        match mode {
            WriteMode::CreateNew => {
                // Check if file exists
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
    }

    async fn append(&self, path: &Path, content: &[u8]) -> BackendResult<()> {
        // Read existing content
        let mut existing = match self.vfs.read(path).await {
            Ok(data) => data,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => Vec::new(),
            Err(e) => return Err(e.into()),
        };
        existing.extend_from_slice(content);
        self.vfs.write(path, &existing).await?;
        Ok(())
    }

    async fn patch(&self, path: &Path, ops: &[PatchOp]) -> BackendResult<()> {
        // Read existing content
        let data = self.vfs.read(path).await?;
        let mut content = String::from_utf8(data)
            .map_err(|e| BackendError::InvalidOperation(format!("file is not valid UTF-8: {}", e)))?;

        // Apply each patch operation
        for op in ops {
            Self::apply_patch_op(&mut content, op)?;
        }

        // Write back
        self.vfs.write(path, content.as_bytes()).await?;
        Ok(())
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Directory Operations
    // ═══════════════════════════════════════════════════════════════════════════

    async fn list(&self, path: &Path) -> BackendResult<Vec<EntryInfo>> {
        let entries = self.vfs.list(path).await?;
        Ok(entries
            .into_iter()
            .map(|e| {
                let (is_dir, is_file) = match e.entry_type {
                    EntryType::Directory => (true, false),
                    EntryType::File => (false, true),
                };
                EntryInfo {
                    name: e.name,
                    is_dir,
                    is_file,
                    size: 0,      // VFS DirEntry doesn't include size
                    modified: None, // VFS DirEntry doesn't include modified time
                    permissions: None,
                }
            })
            .collect())
    }

    async fn stat(&self, path: &Path) -> BackendResult<EntryInfo> {
        let meta = self.vfs.stat(path).await?;
        let modified = meta.modified.and_then(|t| {
            t.duration_since(UNIX_EPOCH)
                .ok()
                .map(|d| d.as_secs())
        });
        Ok(EntryInfo {
            name: path
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| "/".to_string()),
            is_dir: meta.is_dir,
            is_file: meta.is_file,
            size: meta.size,
            modified,
            permissions: None,
        })
    }

    async fn mkdir(&self, path: &Path) -> BackendResult<()> {
        self.vfs.mkdir(path).await?;
        Ok(())
    }

    async fn remove(&self, path: &Path, recursive: bool) -> BackendResult<()> {
        if recursive {
            // For recursive removal, we need to check if it's a directory
            // and remove contents first
            if let Ok(meta) = self.vfs.stat(path).await
                && meta.is_dir
            {
                // List and remove children
                if let Ok(entries) = self.vfs.list(path).await {
                    for entry in entries {
                        let child_path = path.join(&entry.name);
                        // Recursive call using Box::pin to handle async recursion
                        Box::pin(self.remove(&child_path, true)).await?;
                    }
                }
            }
        }
        self.vfs.remove(path).await?;
        Ok(())
    }

    async fn exists(&self, path: &Path) -> bool {
        self.vfs.exists(path).await
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
        let registry = self.tools.as_ref().ok_or_else(|| {
            BackendError::ToolNotFound(format!("no tool registry configured for: {}", name))
        })?;

        let tool = registry.get(name).ok_or_else(|| {
            BackendError::ToolNotFound(format!("{}: command not found", name))
        })?;

        // Execute the tool and convert ExecResult to ToolResult
        let exec_result = tool.execute(args, ctx).await;
        Ok(exec_result.into())
    }

    async fn list_tools(&self) -> BackendResult<Vec<ToolInfo>> {
        match &self.tools {
            Some(registry) => {
                let schemas = registry.schemas();
                Ok(schemas
                    .into_iter()
                    .map(|schema| ToolInfo {
                        name: schema.name.clone(),
                        description: schema.description.clone(),
                        schema,
                    })
                    .collect())
            }
            None => Ok(Vec::new()),
        }
    }

    async fn get_tool(&self, name: &str) -> BackendResult<Option<ToolInfo>> {
        match &self.tools {
            Some(registry) => match registry.get(name) {
                Some(tool) => {
                    let schema = tool.schema();
                    Ok(Some(ToolInfo {
                        name: schema.name.clone(),
                        description: schema.description.clone(),
                        schema,
                    }))
                }
                None => Ok(None),
            },
            None => Ok(None),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Backend Information
    // ═══════════════════════════════════════════════════════════════════════════

    fn read_only(&self) -> bool {
        self.vfs.read_only()
    }

    fn backend_type(&self) -> &str {
        "local"
    }

    fn mounts(&self) -> Vec<MountInfo> {
        self.vfs.list_mounts()
    }

    fn resolve_real_path(&self, path: &Path) -> Option<std::path::PathBuf> {
        self.vfs.resolve_real_path(path)
    }
}

impl std::fmt::Debug for LocalBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LocalBackend")
            .field("vfs", &self.vfs)
            .field("has_tools", &self.tools.is_some())
            .finish()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::MemoryFs;

    async fn make_backend() -> LocalBackend {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("test.txt"), b"hello world")
            .await
            .unwrap();
        mem.write(Path::new("lines.txt"), b"line1\nline2\nline3\n")
            .await
            .unwrap();
        mem.mkdir(Path::new("dir")).await.unwrap();
        mem.write(Path::new("dir/nested.txt"), b"nested content")
            .await
            .unwrap();
        vfs.mount("/", mem);
        LocalBackend::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_read_full() {
        let backend = make_backend().await;
        let content = backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(content, b"hello world");
    }

    #[tokio::test]
    async fn test_read_with_byte_range() {
        let backend = make_backend().await;
        let range = ReadRange::bytes(0, 5);
        let content = backend.read(Path::new("/test.txt"), Some(range)).await.unwrap();
        assert_eq!(content, b"hello");
    }

    #[tokio::test]
    async fn test_read_with_line_range() {
        let backend = make_backend().await;
        let range = ReadRange::lines(2, 3);
        let content = backend.read(Path::new("/lines.txt"), Some(range)).await.unwrap();
        assert_eq!(std::str::from_utf8(&content).unwrap(), "line2\nline3");
    }

    #[tokio::test]
    async fn test_write_overwrite() {
        let backend = make_backend().await;
        backend
            .write(Path::new("/test.txt"), b"new content", WriteMode::Overwrite)
            .await
            .unwrap();
        let content = backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(content, b"new content");
    }

    #[tokio::test]
    async fn test_write_create_new() {
        let backend = make_backend().await;
        backend
            .write(Path::new("/new.txt"), b"created", WriteMode::CreateNew)
            .await
            .unwrap();
        let content = backend.read(Path::new("/new.txt"), None).await.unwrap();
        assert_eq!(content, b"created");
    }

    #[tokio::test]
    async fn test_write_create_new_fails_if_exists() {
        let backend = make_backend().await;
        let result = backend
            .write(Path::new("/test.txt"), b"fail", WriteMode::CreateNew)
            .await;
        assert!(matches!(result, Err(BackendError::AlreadyExists(_))));
    }

    #[tokio::test]
    async fn test_write_update_only() {
        let backend = make_backend().await;
        backend
            .write(Path::new("/test.txt"), b"updated", WriteMode::UpdateOnly)
            .await
            .unwrap();
        let content = backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(content, b"updated");
    }

    #[tokio::test]
    async fn test_write_update_only_fails_if_not_exists() {
        let backend = make_backend().await;
        let result = backend
            .write(Path::new("/nonexistent.txt"), b"fail", WriteMode::UpdateOnly)
            .await;
        assert!(matches!(result, Err(BackendError::NotFound(_))));
    }

    #[tokio::test]
    async fn test_append() {
        let backend = make_backend().await;
        backend.append(Path::new("/test.txt"), b" appended").await.unwrap();
        let content = backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(content, b"hello world appended");
    }

    #[tokio::test]
    async fn test_patch_insert() {
        let backend = make_backend().await;
        let ops = vec![PatchOp::Insert {
            offset: 5,
            content: " there".to_string(),
        }];
        backend.patch(Path::new("/test.txt"), &ops).await.unwrap();
        let content = backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(std::str::from_utf8(&content).unwrap(), "hello there world");
    }

    #[tokio::test]
    async fn test_patch_delete() {
        let backend = make_backend().await;
        let ops = vec![PatchOp::Delete {
            offset: 5,
            len: 6,
            expected: None,
        }];
        backend.patch(Path::new("/test.txt"), &ops).await.unwrap();
        let content = backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(std::str::from_utf8(&content).unwrap(), "hello");
    }

    #[tokio::test]
    async fn test_patch_delete_with_cas() {
        let backend = make_backend().await;
        let ops = vec![PatchOp::Delete {
            offset: 0,
            len: 5,
            expected: Some("hello".to_string()),
        }];
        backend.patch(Path::new("/test.txt"), &ops).await.unwrap();
        let content = backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(std::str::from_utf8(&content).unwrap(), " world");
    }

    #[tokio::test]
    async fn test_patch_delete_cas_conflict() {
        let backend = make_backend().await;
        let ops = vec![PatchOp::Delete {
            offset: 0,
            len: 5,
            expected: Some("wrong".to_string()),
        }];
        let result = backend.patch(Path::new("/test.txt"), &ops).await;
        assert!(matches!(result, Err(BackendError::Conflict(_))));
    }

    #[tokio::test]
    async fn test_patch_replace() {
        let backend = make_backend().await;
        let ops = vec![PatchOp::Replace {
            offset: 0,
            len: 5,
            content: "hi".to_string(),
            expected: None,
        }];
        backend.patch(Path::new("/test.txt"), &ops).await.unwrap();
        let content = backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(std::str::from_utf8(&content).unwrap(), "hi world");
    }

    #[tokio::test]
    async fn test_patch_replace_line() {
        let backend = make_backend().await;
        let ops = vec![PatchOp::ReplaceLine {
            line: 2,
            content: "replaced".to_string(),
            expected: None,
        }];
        backend.patch(Path::new("/lines.txt"), &ops).await.unwrap();
        let content = backend.read(Path::new("/lines.txt"), None).await.unwrap();
        let text = std::str::from_utf8(&content).unwrap();
        assert!(text.contains("line1"));
        assert!(text.contains("replaced"));
        assert!(text.contains("line3"));
        assert!(!text.contains("line2"));
    }

    #[tokio::test]
    async fn test_patch_delete_line() {
        let backend = make_backend().await;
        let ops = vec![PatchOp::DeleteLine {
            line: 2,
            expected: None,
        }];
        backend.patch(Path::new("/lines.txt"), &ops).await.unwrap();
        let content = backend.read(Path::new("/lines.txt"), None).await.unwrap();
        let text = std::str::from_utf8(&content).unwrap();
        assert!(text.contains("line1"));
        assert!(!text.contains("line2"));
        assert!(text.contains("line3"));
    }

    #[tokio::test]
    async fn test_patch_insert_line() {
        let backend = make_backend().await;
        let ops = vec![PatchOp::InsertLine {
            line: 2,
            content: "inserted".to_string(),
        }];
        backend.patch(Path::new("/lines.txt"), &ops).await.unwrap();
        let content = backend.read(Path::new("/lines.txt"), None).await.unwrap();
        let text = std::str::from_utf8(&content).unwrap();
        let lines: Vec<&str> = text.lines().collect();
        assert_eq!(lines[0], "line1");
        assert_eq!(lines[1], "inserted");
        assert_eq!(lines[2], "line2");
    }

    #[tokio::test]
    async fn test_patch_append() {
        let backend = make_backend().await;
        let ops = vec![PatchOp::Append {
            content: "!".to_string(),
        }];
        backend.patch(Path::new("/test.txt"), &ops).await.unwrap();
        let content = backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(std::str::from_utf8(&content).unwrap(), "hello world!");
    }

    #[tokio::test]
    async fn test_list() {
        let backend = make_backend().await;
        let entries = backend.list(Path::new("/")).await.unwrap();
        let names: Vec<&str> = entries.iter().map(|e| e.name.as_str()).collect();
        assert!(names.contains(&"test.txt"));
        assert!(names.contains(&"lines.txt"));
        assert!(names.contains(&"dir"));
    }

    #[tokio::test]
    async fn test_stat() {
        let backend = make_backend().await;
        let info = backend.stat(Path::new("/test.txt")).await.unwrap();
        assert!(info.is_file);
        assert!(!info.is_dir);
        assert_eq!(info.size, 11); // "hello world".len()

        let info = backend.stat(Path::new("/dir")).await.unwrap();
        assert!(info.is_dir);
        assert!(!info.is_file);
    }

    #[tokio::test]
    async fn test_mkdir() {
        let backend = make_backend().await;
        backend.mkdir(Path::new("/newdir")).await.unwrap();
        assert!(backend.exists(Path::new("/newdir")).await);
        let info = backend.stat(Path::new("/newdir")).await.unwrap();
        assert!(info.is_dir);
    }

    #[tokio::test]
    async fn test_remove() {
        let backend = make_backend().await;
        assert!(backend.exists(Path::new("/test.txt")).await);
        backend.remove(Path::new("/test.txt"), false).await.unwrap();
        assert!(!backend.exists(Path::new("/test.txt")).await);
    }

    #[tokio::test]
    async fn test_remove_recursive() {
        let backend = make_backend().await;
        assert!(backend.exists(Path::new("/dir/nested.txt")).await);
        backend.remove(Path::new("/dir"), true).await.unwrap();
        assert!(!backend.exists(Path::new("/dir")).await);
        assert!(!backend.exists(Path::new("/dir/nested.txt")).await);
    }

    #[tokio::test]
    async fn test_exists() {
        let backend = make_backend().await;
        assert!(backend.exists(Path::new("/test.txt")).await);
        assert!(!backend.exists(Path::new("/nonexistent.txt")).await);
    }

    #[tokio::test]
    async fn test_backend_info() {
        let backend = make_backend().await;
        assert_eq!(backend.backend_type(), "local");
        assert!(!backend.read_only());
        let mounts = backend.mounts();
        assert!(!mounts.is_empty());
    }
}
