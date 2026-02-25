//! mv — Move (rename) files and directories.

use async_trait::async_trait;
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::backend::{BackendError, KernelBackend, WriteMode};
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Mv tool: move/rename files and directories.
pub struct Mv;

#[async_trait]
impl Tool for Mv {
    fn name(&self) -> &str {
        "mv"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("mv", "Move (rename) files and directories")
            .param(ParamSchema::required("source", "string", "Source path"))
            .param(ParamSchema::required("dest", "string", "Destination path"))
            .param(ParamSchema::optional(
                "no_clobber",
                "bool",
                Value::Bool(false),
                "Do not overwrite existing files (-n)",
            ))
            .example("Rename a file", "mv old.txt new.txt")
            .example("Move into directory", "mv file.txt /archive/")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let source = match args.get_string("source", 0) {
            Some(s) => s,
            None => return ExecResult::failure(1, "mv: missing source argument"),
        };

        let dest = match args.get_string("dest", 1) {
            Some(d) => d,
            None => return ExecResult::failure(1, "mv: missing destination argument"),
        };

        let no_clobber = args.has_flag("no_clobber") || args.has_flag("n");
        let src_path = ctx.resolve_path(&source);
        let dst_path = ctx.resolve_path(&dest);

        match move_path(&*ctx.backend, &src_path, &dst_path, no_clobber).await {
            Ok(()) => ExecResult::with_output(OutputData::text("")),
            Err(e) => ExecResult::failure(1, format!("mv: {}", e)),
        }
    }
}

/// Move a path to destination.
///
/// Uses the VFS rename operation when possible (atomic, same-mount).
/// Falls back to copy+remove for cross-mount moves.
async fn move_path(
    backend: &dyn KernelBackend,
    src: &Path,
    dst: &Path,
    no_clobber: bool,
) -> Result<(), BackendError> {
    let info = backend.stat(src).await?;

    // Determine final destination path
    let final_dst = match backend.stat(dst).await {
        Ok(dst_info) if dst_info.is_dir() => {
            // Move into directory with same filename
            let filename = src.file_name().ok_or_else(|| {
                BackendError::InvalidOperation("invalid source path".to_string())
            })?;
            dst.join(filename)
        }
        _ => dst.to_path_buf(),
    };

    // Check for no-clobber mode
    if no_clobber && backend.exists(&final_dst).await {
        return Ok(()); // Silently skip if destination exists
    }

    // Try VFS rename first (works for same-mount operations)
    match backend.rename(src, &final_dst).await {
        Ok(()) => return Ok(()),
        Err(BackendError::Io(msg)) if msg.contains("cross") || msg.contains("Unsupported") => {
            // Cross-mount rename not supported, fall through to copy+remove
        }
        Err(e) => return Err(e),
    }

    // Fall back to copy + remove (for cross-mount moves)
    if info.is_dir() {
        // Copy directory recursively, then remove source
        move_dir_recursive(backend, src, &final_dst).await?;
        remove_dir_recursive(backend, src).await?;
        backend.remove(src, false).await?;
    } else {
        // Copy file, then remove source
        let data = backend.read(src, None).await?;
        backend.write(&final_dst, &data, WriteMode::Overwrite).await?;
        backend.remove(src, false).await?;
    }

    Ok(())
}

/// Recursively copy a directory for move operation.
fn move_dir_recursive<'a>(
    backend: &'a dyn KernelBackend,
    src: &'a Path,
    dst: &'a Path,
) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), BackendError>> + Send + 'a>> {
    Box::pin(async move {
        backend.mkdir(dst).await?;

        let entries = backend.list(src).await?;

        for entry in entries {
            let src_child: PathBuf = src.join(&entry.name);
            let dst_child: PathBuf = dst.join(&entry.name);

            if entry.is_dir() {
                move_dir_recursive(backend, &src_child, &dst_child).await?;
            } else {
                let data = backend.read(&src_child, None).await?;
                backend.write(&dst_child, &data, WriteMode::Overwrite).await?;
            }
        }

        Ok(())
    })
}

/// Recursively remove directory contents (for cleanup after copy).
fn remove_dir_recursive<'a>(
    backend: &'a dyn KernelBackend,
    dir: &'a Path,
) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), BackendError>> + Send + 'a>> {
    Box::pin(async move {
        let entries = backend.list(dir).await?;

        for entry in entries {
            let child_path: PathBuf = dir.join(&entry.name);
            if entry.is_dir() {
                remove_dir_recursive(backend, &child_path).await?;
                backend.remove(&child_path, false).await?;
            } else {
                backend.remove(&child_path, false).await?;
            }
        }

        Ok(())
    })
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
        mem.write(Path::new("file.txt"), b"hello world")
            .await
            .unwrap();
        mem.write(Path::new("dir/nested.txt"), b"nested content")
            .await
            .unwrap();
        mem.write(Path::new("dir/sub/deep.txt"), b"deep content")
            .await
            .unwrap();
        mem.mkdir(Path::new("destdir")).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_mv_file_rename() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.positional.push(Value::String("/renamed.txt".into()));

        let result = Mv.execute(args, &mut ctx).await;
        assert!(result.ok());

        // New file exists with content
        let data = ctx.backend.read(Path::new("/renamed.txt"), None).await.unwrap();
        assert_eq!(data, b"hello world");

        // Original is gone
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_mv_file_into_dir() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.positional.push(Value::String("/destdir".into()));

        let result = Mv.execute(args, &mut ctx).await;
        assert!(result.ok());

        // File moved into destdir
        let data = ctx.backend.read(Path::new("/destdir/file.txt"), None).await.unwrap();
        assert_eq!(data, b"hello world");

        // Original is gone
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_mv_dir() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dir".into()));
        args.positional.push(Value::String("/moved".into()));

        let result = Mv.execute(args, &mut ctx).await;
        assert!(result.ok());

        // New location exists with all contents
        assert!(ctx.backend.exists(Path::new("/moved")).await);
        assert!(ctx.backend.exists(Path::new("/moved/nested.txt")).await);
        assert!(ctx.backend.exists(Path::new("/moved/sub/deep.txt")).await);

        let data = ctx.backend.read(Path::new("/moved/nested.txt"), None).await.unwrap();
        assert_eq!(data, b"nested content");

        // Original is gone
        assert!(!ctx.backend.exists(Path::new("/dir")).await);
    }

    #[tokio::test]
    async fn test_mv_dir_into_existing_dir() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dir".into()));
        args.positional.push(Value::String("/destdir".into()));

        let result = Mv.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Dir moved into destdir
        assert!(ctx.backend.exists(Path::new("/destdir/dir")).await);
        assert!(ctx.backend.exists(Path::new("/destdir/dir/nested.txt")).await);

        // Original is gone
        assert!(!ctx.backend.exists(Path::new("/dir")).await);
    }

    #[tokio::test]
    async fn test_mv_nonexistent_source() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));
        args.positional.push(Value::String("/dest".into()));

        let result = Mv.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_mv_missing_args() {
        let mut ctx = make_ctx().await;

        // No args
        let result = Mv.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("source"));

        // Only source
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        let result = Mv.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("destination"));
    }
}
