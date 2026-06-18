//! mv — Move (rename) files and directories.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::{Path, PathBuf};

use crate::backend::{BackendError, KernelBackend, WriteMode};
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Mv tool: move/rename files and directories.
pub struct Mv;

/// clap-derived argv layer for mv.
#[derive(Parser, Debug)]
#[command(name = "mv", about = "Move (rename) files and directories")]
struct MvArgs {
    /// Do not overwrite existing files (-n)
    #[arg(short = 'n', long = "no-clobber", visible_alias = "no_clobber")]
    no_clobber: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Source files followed by the destination path or directory.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Mv {
    fn name(&self) -> &str {
        "mv"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &MvArgs::command(),
            "mv",
            "Move (rename) files and directories",
            [
                ("Rename a file", "mv old.txt new.txt"),
                ("Move into directory", "mv file.txt /archive/"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match MvArgs::try_parse_from(
            std::iter::once("mv".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("mv: {e}")),
        };
        parsed.global.apply(ctx);

        let no_clobber = parsed.no_clobber;

        // POSIX: `mv SRC DST` for one source, `mv SRC... DIR/` for many.
        // Last positional is the destination.
        let (sources, dest_value) = match args.positional.split_last() {
            Some((last, rest)) if !rest.is_empty() => (rest, last),
            _ => return ExecResult::failure(1, "mv: missing file operand (need source and destination)"),
        };
        let dest = crate::interpreter::value_to_string(dest_value);
        let dst_path = ctx.resolve_path(&dest);

        // Multiple sources require destination to be an existing directory.
        if sources.len() > 1 {
            let is_dir = ctx
                .backend
                .stat(Path::new(&dst_path))
                .await
                .map(|info| info.is_dir())
                .unwrap_or(false);
            if !is_dir {
                return ExecResult::failure(
                    1,
                    format!("mv: target '{}' is not a directory", dest),
                );
            }
        }

        let mut last_err: Option<String> = None;
        for src_value in sources {
            let source = crate::interpreter::value_to_string(src_value);
            let src_path = ctx.resolve_path(&source);
            if let Err(e) = move_path(&*ctx.backend, &src_path, &dst_path, no_clobber).await {
                last_err = Some(format!("mv: {}: {}", source, e));
            }
        }
        match last_err {
            Some(msg) => ExecResult::failure(1, msg),
            None => ExecResult::success(""),
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
