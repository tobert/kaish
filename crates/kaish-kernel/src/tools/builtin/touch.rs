//! touch — Change file timestamps or create empty files.

use async_trait::async_trait;
use std::path::Path;

use crate::backend::WriteMode;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Touch tool: change file timestamps or create files.
pub struct Touch;

#[async_trait]
impl Tool for Touch {
    fn name(&self) -> &str {
        "touch"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("touch", "Change file timestamps or create empty files")
            .param(ParamSchema::required("path", "string", "File to touch"))
            .example("Create empty file", "touch newfile.txt")
            .example("Update timestamp", "touch existing.txt")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path_str = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "touch: missing path argument"),
        };

        let resolved = ctx.resolve_path(&path_str);
        let path = Path::new(&resolved);

        // Check if file exists
        if ctx.backend.exists(path).await {
            // File exists, update timestamp
            // Try to use real filesystem path if available
            if let Some(real_path) = ctx.backend.resolve_real_path(path) {
                // Update mtime to current time
                if let Err(e) = update_mtime(&real_path) {
                    return ExecResult::failure(1, format!("touch: {}: {}", path_str, e));
                }
            }
            // For virtual filesystems (MemoryFs), this is a no-op
            ExecResult::with_output(OutputData::text(""))
        } else {
            // Create empty file
            match ctx.backend.write(path, &[], WriteMode::CreateNew).await {
                Ok(()) => ExecResult::with_output(OutputData::text("")),
                Err(e) => ExecResult::failure(1, format!("touch: {}: {}", path_str, e)),
            }
        }
    }
}

/// Update modification time of a file to the current time.
///
/// Uses File::set_modified() which calls futimens/utimensat on Unix.
fn update_mtime(path: &Path) -> std::io::Result<()> {
    use std::time::SystemTime;

    let file = std::fs::OpenOptions::new()
        .write(true)
        .open(path)?;

    file.set_modified(SystemTime::now())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("existing.txt"), b"content")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_touch_create_new() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/newfile.txt".into()));

        let result = Touch.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify file was created
        assert!(ctx.backend.exists(Path::new("/newfile.txt")).await);
        let data = ctx
            .backend
            .read(Path::new("/newfile.txt"), None)
            .await
            .unwrap();
        assert!(data.is_empty());
    }

    #[tokio::test]
    async fn test_touch_existing() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));

        let result = Touch.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify file still has original content
        let data = ctx
            .backend
            .read(Path::new("/existing.txt"), None)
            .await
            .unwrap();
        assert_eq!(data, b"content");
    }

    #[tokio::test]
    async fn test_touch_missing_path() {
        let mut ctx = make_ctx().await;
        let result = Touch.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
