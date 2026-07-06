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
    /// Confirmation nonce for a latch-gated overwrite.
    #[arg(long = "confirm")]
    confirm: Option<String>,

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
        let dest = match crate::interpreter::value_to_text_sink_named(dest_value, "a path") {
            Ok(d) => d,
            Err(e) => return ExecResult::failure(1, format!("mv: {e}")),
        };
        let sources = match crate::interpreter::values_to_text_sink_named(sources, "a path") {
            Ok(s) => s,
            Err(e) => return ExecResult::failure(1, format!("mv: {e}")),
        };
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

        // Gate a direct file clobber (`mv SRC EXISTING_FILE`) through latch +
        // trash. The gate snapshots the prior destination content to trash (the
        // recovery copy) and handles the latch prompt; the move itself replaces
        // it (an atomic same-mount `rename`, or unlink+write cross-mount), so no
        // CAS is threaded. Moving *into* a directory or a recursive merge isn't
        // a single-file truncation and stays ungated (documented residual).
        if sources.len() == 1 {
            let dst_is_existing_file = ctx
                .backend
                .stat(Path::new(&dst_path))
                .await
                .map(|info| !info.is_dir())
                .unwrap_or(false);
            if dst_is_existing_file {
                let src_display = &sources[0];
                if let Err(blocked) = ctx
                    .gate_overwrites("mv", &[(dest.clone(), false)], parsed.confirm.as_deref(), |nonce, joined| {
                        format!("mv --confirm=\"{nonce}\" {src_display} {joined}")
                    })
                    .await
                {
                    return blocked;
                }
            }
        }

        let mut last_err: Option<String> = None;
        for source in &sources {
            let src_path = ctx.resolve_path(source);
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
    // lstat, not stat: classify the link itself so a symlink source (incl. a
    // dangling one, which stat would fail to find) is moved as a link, never
    // followed to its target.
    let info = backend.lstat(src).await?;

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

    // Fall back to copy + remove (for cross-mount moves). We've already passed
    // the no-clobber early-return, so either no-clobber is off (overwrite is
    // wanted) or the dest doesn't exist. Unlink any existing dest FIRST so this
    // mirrors `mv`'s overwrite semantics: recreating a symlink can't fail
    // EEXIST, and overwriting replaces a dest *symlink* itself rather than
    // writing through it to its target. `remove` is symlink-safe and tolerates
    // a missing dest.
    if info.is_symlink() {
        // Recreate the link at the destination — never copy *through* it to the
        // target (that would duplicate the target's data and drop the link).
        let target = backend.read_link(src).await?;
        let _ = backend.remove(&final_dst, false).await;
        backend.symlink(&target, &final_dst).await?;
        backend.remove(src, false).await?;
    } else if info.is_dir() {
        // Copy directory recursively, then remove source tree via the single
        // symlink-safe recursive remover on the backend.
        move_dir_recursive(backend, src, &final_dst).await?;
        backend.remove(src, true).await?;
    } else {
        // Copy file, then remove source
        let data = backend.read(src, None).await?;
        let _ = backend.remove(&final_dst, false).await;
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
