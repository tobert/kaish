//! cp — Copy files and directories.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::{Path, PathBuf};

use crate::backend::{BackendError, KernelBackend, WriteMode};
use crate::interpreter::ExecResult;
use crate::tools::{cas_overwrite, schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Cp tool: copy files and directories.
pub struct Cp;

/// clap-derived argv layer for cp.
#[derive(Parser, Debug)]
#[command(name = "cp", about = "Copy files and directories")]
struct CpArgs {
    /// Copy directories recursively (-r)
    #[arg(short = 'r', long = "recursive")]
    recursive: bool,

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
impl Tool for Cp {
    fn name(&self) -> &str {
        "cp"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &CpArgs::command(),
            "cp",
            "Copy files and directories",
            [
                ("Copy a file", "cp src.txt dest.txt"),
                ("Copy directory recursively", "cp -r src/ backup/"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match CpArgs::try_parse_from(
            std::iter::once("cp".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("cp: {e}")),
        };
        parsed.global.apply(ctx);

        let recursive = parsed.recursive;
        let no_clobber = parsed.no_clobber;
        // `-p`/`--preserve` is intentionally NOT accepted: the VFS has no mode,
        // mtime, or ownership to preserve, so advertising the flag would be a
        // silent no-op. clap rejects it loudly as an unknown argument instead.

        // POSIX: `cp SRC DST` for one source, `cp SRC... DIR/` for many.
        // Last positional is the destination.
        let (sources, dest_value) = match args.positional.split_last() {
            Some((last, rest)) if !rest.is_empty() => (rest, last),
            _ => return ExecResult::failure(1, "cp: missing file operand (need source and destination)"),
        };
        let dest = match crate::interpreter::value_to_text_sink_named(dest_value, "a path") {
            Ok(d) => d,
            Err(e) => return ExecResult::failure(1, format!("cp: {e}")),
        };
        let sources = match crate::interpreter::values_to_text_sink_named(sources, "a path") {
            Ok(s) => s,
            Err(e) => return ExecResult::failure(1, format!("cp: {e}")),
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
                    format!("cp: target '{}' is not a directory", dest),
                );
            }
        }

        // Gate a direct file clobber (`cp SRC EXISTING_FILE`) through latch +
        // trash. Copying *into* a directory or a recursive directory merge is
        // not a single-file truncation, so it stays ungated (documented
        // write-model residual). Only the named destination is gated here.
        let mut expected_dst: Option<Vec<u8>> = None;
        if sources.len() == 1 {
            let dst_is_existing_file = ctx
                .backend
                .stat(Path::new(&dst_path))
                .await
                .map(|info| !info.is_dir())
                .unwrap_or(false);
            let src_display = &sources[0];
            let src_resolved = ctx.resolve_path(src_display);
            let src_is_dir = ctx
                .backend
                .stat(Path::new(&src_resolved))
                .await
                .map(|info| info.is_dir())
                .unwrap_or(false);
            // Only a file→existing-file copy is a truncating overwrite. A dir
            // source onto a file errors at mkdir anyway, so gating it would just
            // take a spurious trash snapshot of a file that's never overwritten.
            if dst_is_existing_file && !src_is_dir {
                let snapshots = match ctx
                    .gate_overwrites("cp", &[(dest.clone(), false)], parsed.confirm.as_deref(), |nonce, joined| {
                        format!("cp --confirm=\"{nonce}\" {src_display} {joined}")
                    })
                    .await
                {
                    Ok(s) => s,
                    Err(blocked) => return blocked,
                };
                expected_dst = snapshots.get(&dst_path).cloned();
            }
        }

        let mut last_err: Option<String> = None;
        for source in &sources {
            let src_path = ctx.resolve_path(source);
            if let Err(e) = copy_path(
                &*ctx.backend,
                &src_path,
                &dst_path,
                recursive,
                no_clobber,
                expected_dst.as_deref(),
            )
            .await
            {
                last_err = Some(format!("cp: {}", e));
            }
        }
        match last_err {
            Some(msg) => ExecResult::failure(1, msg),
            None => ExecResult::success(""),
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
    expected: Option<&[u8]>,
) -> Result<(), BackendError> {
    let info = backend.stat(src).await?;

    if info.is_dir() {
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
            Ok(dst_info) if dst_info.is_dir() => {
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
        // CAS against the gate snapshot (`expected`) when this is a gated direct
        // file clobber — a concurrent change is a loud conflict, not a silent
        // clobber. `expected` is `None` for a new file or an ungated path.
        cas_overwrite(backend, &final_dst, &data, expected).await
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

            if entry.is_dir() {
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
