//! mv — Move (rename) files and directories.

use async_trait::async_trait;
use std::path::{Path, PathBuf};

use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use crate::vfs::{EntryType, Filesystem};

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

        let src_path = ctx.resolve_path(&source);
        let dst_path = ctx.resolve_path(&dest);

        match move_path(ctx, &src_path, &dst_path).await {
            Ok(()) => ExecResult::success(""),
            Err(e) => ExecResult::failure(1, format!("mv: {}", e)),
        }
    }
}

/// Move a path to destination (copy + remove).
async fn move_path(ctx: &ExecContext, src: &Path, dst: &Path) -> std::io::Result<()> {
    let meta = ctx.vfs.stat(src).await?;

    // Determine final destination path
    let final_dst = match ctx.vfs.stat(dst).await {
        Ok(dst_meta) if dst_meta.is_dir => {
            // Move into directory with same filename
            let filename = src.file_name().ok_or_else(|| {
                std::io::Error::new(std::io::ErrorKind::InvalidInput, "invalid source path")
            })?;
            dst.join(filename)
        }
        _ => dst.to_path_buf(),
    };

    if meta.is_dir {
        // Copy directory recursively, then remove source
        move_dir_recursive(ctx, src, &final_dst).await?;
        remove_dir_recursive(ctx, src).await?;
        ctx.vfs.remove(src).await?;
    } else {
        // Copy file, then remove source
        let data = ctx.vfs.read(src).await?;
        ctx.vfs.write(&final_dst, &data).await?;
        ctx.vfs.remove(src).await?;
    }

    Ok(())
}

/// Recursively copy a directory for move operation.
fn move_dir_recursive<'a>(
    ctx: &'a ExecContext,
    src: &'a Path,
    dst: &'a Path,
) -> std::pin::Pin<Box<dyn std::future::Future<Output = std::io::Result<()>> + Send + 'a>> {
    Box::pin(async move {
        ctx.vfs.mkdir(dst).await?;

        let entries = ctx.vfs.list(src).await?;

        for entry in entries {
            let src_child: PathBuf = src.join(&entry.name);
            let dst_child: PathBuf = dst.join(&entry.name);

            match entry.entry_type {
                EntryType::Directory => {
                    move_dir_recursive(ctx, &src_child, &dst_child).await?;
                }
                EntryType::File => {
                    let data = ctx.vfs.read(&src_child).await?;
                    ctx.vfs.write(&dst_child, &data).await?;
                }
            }
        }

        Ok(())
    })
}

/// Recursively remove directory contents (for cleanup after copy).
fn remove_dir_recursive<'a>(
    ctx: &'a ExecContext,
    dir: &'a Path,
) -> std::pin::Pin<Box<dyn std::future::Future<Output = std::io::Result<()>> + Send + 'a>> {
    Box::pin(async move {
        let entries = ctx.vfs.list(dir).await?;

        for entry in entries {
            let child_path: PathBuf = dir.join(&entry.name);
            match entry.entry_type {
                EntryType::Directory => {
                    remove_dir_recursive(ctx, &child_path).await?;
                    ctx.vfs.remove(&child_path).await?;
                }
                EntryType::File => {
                    ctx.vfs.remove(&child_path).await?;
                }
            }
        }

        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
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
        let data = ctx.vfs.read(Path::new("/renamed.txt")).await.unwrap();
        assert_eq!(data, b"hello world");

        // Original is gone
        assert!(!ctx.vfs.exists(Path::new("/file.txt")).await);
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
        let data = ctx.vfs.read(Path::new("/destdir/file.txt")).await.unwrap();
        assert_eq!(data, b"hello world");

        // Original is gone
        assert!(!ctx.vfs.exists(Path::new("/file.txt")).await);
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
        assert!(ctx.vfs.exists(Path::new("/moved")).await);
        assert!(ctx.vfs.exists(Path::new("/moved/nested.txt")).await);
        assert!(ctx.vfs.exists(Path::new("/moved/sub/deep.txt")).await);

        let data = ctx.vfs.read(Path::new("/moved/nested.txt")).await.unwrap();
        assert_eq!(data, b"nested content");

        // Original is gone
        assert!(!ctx.vfs.exists(Path::new("/dir")).await);
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
        assert!(ctx.vfs.exists(Path::new("/destdir/dir")).await);
        assert!(ctx.vfs.exists(Path::new("/destdir/dir/nested.txt")).await);

        // Original is gone
        assert!(!ctx.vfs.exists(Path::new("/dir")).await);
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
