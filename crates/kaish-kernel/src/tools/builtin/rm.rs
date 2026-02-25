//! rm — Remove files and directories.

use async_trait::async_trait;
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::backend::{BackendError, KernelBackend};
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema, ParamSchema};

/// Rm tool: remove files and directories.
pub struct Rm;

#[async_trait]
impl Tool for Rm {
    fn name(&self) -> &str {
        "rm"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("rm", "Remove files and directories")
            .param(ParamSchema::required("path", "string", "Path to remove"))
            .param(ParamSchema::optional(
                "recursive",
                "bool",
                Value::Bool(false),
                "Remove directories and their contents recursively (-r)",
            ).with_aliases(["-r", "-R"]))
            .param(ParamSchema::optional(
                "force",
                "bool",
                Value::Bool(false),
                "Ignore nonexistent files, never prompt (-f)",
            ).with_aliases(["-f"]))
            .example("Remove a file", "rm temp.txt")
            .example("Remove directory recursively", "rm -rf build/")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "rm: missing path argument"),
        };

        let recursive = args.has_flag("recursive") || args.has_flag("r");
        let force = args.has_flag("force") || args.has_flag("f");
        let resolved = ctx.resolve_path(&path);

        match remove_path(&*ctx.backend, Path::new(&resolved), recursive, force).await {
            Ok(()) => ExecResult::with_output(OutputData::text("")),
            Err(e) => ExecResult::failure(1, format!("rm: {}: {}", path, e)),
        }
    }
}

/// Remove a path, optionally recursively.
async fn remove_path(backend: &dyn KernelBackend, path: &Path, recursive: bool, force: bool) -> Result<(), BackendError> {
    // Check if path exists
    match backend.stat(path).await {
        Ok(info) => {
            if info.is_dir() && recursive {
                // Remove contents first
                remove_dir_recursive(backend, path).await?;
            }
            backend.remove(path, false).await
        }
        Err(BackendError::NotFound(_)) if force => {
            // -f ignores nonexistent files
            Ok(())
        }
        Err(e) => Err(e),
    }
}

/// Recursively remove directory contents, then the directory itself.
fn remove_dir_recursive<'a>(
    backend: &'a dyn KernelBackend,
    dir: &'a Path,
) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), BackendError>> + Send + 'a>> {
    Box::pin(async move {
        let entries = backend.list(dir).await?;

        for entry in entries {
            let child_path: PathBuf = dir.join(&entry.name);
            if entry.is_dir() {
                // Recurse into subdirectory
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
        mem.write(Path::new("file.txt"), b"data").await.unwrap();
        mem.mkdir(Path::new("emptydir")).await.unwrap();
        mem.write(Path::new("fulldir/file.txt"), b"data").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_rm_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify deleted
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_rm_empty_dir() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/emptydir".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());

        assert!(!ctx.backend.exists(Path::new("/emptydir")).await);
    }

    #[tokio::test]
    async fn test_rm_non_empty_dir_fails() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/fulldir".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(!result.ok());
        // Directory should still exist
        assert!(ctx.backend.exists(Path::new("/fulldir")).await);
    }

    #[tokio::test]
    async fn test_rm_nonexistent() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_rm_no_arg() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Rm.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing"));
    }

    #[tokio::test]
    async fn test_rm_r_recursive() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/fulldir".into()));
        args.flags.insert("r".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify directory and contents removed
        assert!(!ctx.backend.exists(Path::new("/fulldir")).await);
        assert!(!ctx.backend.exists(Path::new("/fulldir/file.txt")).await);
    }

    #[tokio::test]
    async fn test_rm_recursive_flag() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/fulldir".into()));
        args.flags.insert("recursive".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.backend.exists(Path::new("/fulldir")).await);
    }

    #[tokio::test]
    async fn test_rm_f_force_nonexistent() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));
        args.flags.insert("f".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok()); // -f silences not-found errors
    }

    #[tokio::test]
    async fn test_rm_force_flag_nonexistent() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));
        args.flags.insert("force".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    async fn make_deep_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("deep/a/b/c/file.txt"), b"data").await.unwrap();
        mem.write(Path::new("deep/a/sibling.txt"), b"data").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_rm_r_deeply_nested() {
        let mut ctx = make_deep_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/deep".into()));
        args.flags.insert("r".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());

        assert!(!ctx.backend.exists(Path::new("/deep")).await);
        assert!(!ctx.backend.exists(Path::new("/deep/a")).await);
        assert!(!ctx.backend.exists(Path::new("/deep/a/b")).await);
    }
}
