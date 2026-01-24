//! cp — Copy files and directories.

use async_trait::async_trait;
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::backend::{BackendError, KernelBackend, WriteMode};
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Cp tool: copy files and directories.
pub struct Cp;

#[async_trait]
impl Tool for Cp {
    fn name(&self) -> &str {
        "cp"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("cp", "Copy files and directories")
            .param(ParamSchema::required("source", "string", "Source path"))
            .param(ParamSchema::required("dest", "string", "Destination path"))
            .param(ParamSchema::optional(
                "recursive",
                "bool",
                Value::Bool(false),
                "Copy directories recursively (-r)",
            ))
            .param(ParamSchema::optional(
                "no_clobber",
                "bool",
                Value::Bool(false),
                "Do not overwrite existing files (-n)",
            ))
            .param(ParamSchema::optional(
                "preserve",
                "bool",
                Value::Bool(false),
                "Preserve file attributes (-p)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let source = match args.get_string("source", 0) {
            Some(s) => s,
            None => return ExecResult::failure(1, "cp: missing source argument"),
        };

        let dest = match args.get_string("dest", 1) {
            Some(d) => d,
            None => return ExecResult::failure(1, "cp: missing destination argument"),
        };

        let recursive = args.has_flag("recursive") || args.has_flag("r");
        let no_clobber = args.has_flag("no_clobber") || args.has_flag("n");
        // preserve flag is recognized but VFS doesn't support attributes
        let _preserve = args.has_flag("preserve") || args.has_flag("p");

        let src_path = ctx.resolve_path(&source);
        let dst_path = ctx.resolve_path(&dest);

        match copy_path(&*ctx.backend, &src_path, &dst_path, recursive, no_clobber).await {
            Ok(()) => ExecResult::success(""),
            Err(e) => ExecResult::failure(1, format!("cp: {}", e)),
        }
    }
}

/// Copy a path to destination, optionally recursively.
async fn copy_path(
    backend: &dyn KernelBackend,
    src: &Path,
    dst: &Path,
    recursive: bool,
    no_clobber: bool,
) -> Result<(), BackendError> {
    let info = backend.stat(src).await?;

    if info.is_dir {
        if !recursive {
            return Err(BackendError::InvalidOperation(format!(
                "{}: is a directory (use -r to copy)",
                src.display()
            )));
        }
        copy_dir_recursive(backend, src, dst, no_clobber).await
    } else {
        // Check if destination is a directory
        let final_dst = match backend.stat(dst).await {
            Ok(dst_info) if dst_info.is_dir => {
                // Copy into directory with same filename
                let filename = src.file_name().ok_or_else(|| {
                    BackendError::InvalidOperation("invalid source path".to_string())
                })?;
                dst.join(filename)
            }
            _ => dst.to_path_buf(),
        };

        // Check for no-clobber mode
        if no_clobber && backend.exists(&final_dst).await {
            return Ok(()); // Silently skip existing files
        }

        let data = backend.read(src, None).await?;
        backend.write(&final_dst, &data, WriteMode::Overwrite).await
    }
}

/// Recursively copy a directory.
fn copy_dir_recursive<'a>(
    backend: &'a dyn KernelBackend,
    src: &'a Path,
    dst: &'a Path,
    no_clobber: bool,
) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), BackendError>> + Send + 'a>> {
    Box::pin(async move {
        // Create destination directory
        backend.mkdir(dst).await?;

        let entries = backend.list(src).await?;

        for entry in entries {
            let src_child: PathBuf = src.join(&entry.name);
            let dst_child: PathBuf = dst.join(&entry.name);

            if entry.is_dir {
                copy_dir_recursive(backend, &src_child, &dst_child, no_clobber).await?;
            } else {
                // Check for no-clobber mode
                if no_clobber && backend.exists(&dst_child).await {
                    continue; // Skip existing files
                }
                let data = backend.read(&src_child, None).await?;
                backend.write(&dst_child, &data, WriteMode::Overwrite).await?;
            }
        }

        Ok(())
    })
}

#[cfg(test)]
mod tests {
    use super::*;
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
        mem.mkdir(Path::new("emptydir")).await.unwrap();
        mem.mkdir(Path::new("destdir")).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_cp_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.positional.push(Value::String("/copy.txt".into()));

        let result = Cp.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify copy exists with same content
        let data = ctx.backend.read(Path::new("/copy.txt"), None).await.unwrap();
        assert_eq!(data, b"hello world");

        // Original still exists
        assert!(ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_cp_file_into_dir() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.positional.push(Value::String("/destdir".into()));

        let result = Cp.execute(args, &mut ctx).await;
        assert!(result.ok());

        // File should be copied into destdir with same name
        let data = ctx.backend.read(Path::new("/destdir/file.txt"), None).await.unwrap();
        assert_eq!(data, b"hello world");
    }

    #[tokio::test]
    async fn test_cp_dir_without_r_fails() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dir".into()));
        args.positional.push(Value::String("/dircopy".into()));

        let result = Cp.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("directory"));
    }

    #[tokio::test]
    async fn test_cp_r_recursive() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dir".into()));
        args.positional.push(Value::String("/dircopy".into()));
        args.flags.insert("r".to_string());

        let result = Cp.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify recursive copy
        assert!(ctx.backend.exists(Path::new("/dircopy")).await);
        assert!(ctx.backend.exists(Path::new("/dircopy/nested.txt")).await);
        assert!(ctx.backend.exists(Path::new("/dircopy/sub/deep.txt")).await);

        let data = ctx.backend.read(Path::new("/dircopy/nested.txt"), None).await.unwrap();
        assert_eq!(data, b"nested content");
    }

    #[tokio::test]
    async fn test_cp_recursive_flag() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/dir".into()));
        args.positional.push(Value::String("/dircopy2".into()));
        args.flags.insert("recursive".to_string());

        let result = Cp.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(ctx.backend.exists(Path::new("/dircopy2")).await);
    }

    #[tokio::test]
    async fn test_cp_nonexistent_source() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));
        args.positional.push(Value::String("/dest".into()));

        let result = Cp.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_cp_missing_args() {
        let mut ctx = make_ctx().await;

        // No args
        let result = Cp.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("source"));

        // Only source
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        let result = Cp.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("destination"));
    }
}
