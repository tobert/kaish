//! cd — Change working directory.

use async_trait::async_trait;
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema, ParamSchema};

/// Cd tool: change current working directory.
pub struct Cd;

#[async_trait]
impl Tool for Cd {
    fn name(&self) -> &str {
        "cd"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("cd", "Change current working directory")
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::String("/".into()),
                "Directory to change to (use - for previous directory)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path_arg = args
            .get_string("path", 0)
            .unwrap_or_else(|| "/".to_string());

        // Handle `cd -` for previous directory
        let resolved: PathBuf = if path_arg == "-" {
            match ctx.get_prev_cwd() {
                Some(prev) => prev.clone(),
                None => return ExecResult::failure(1, "cd: OLDPWD not set"),
            }
        } else {
            ctx.resolve_path(&path_arg)
        };

        // Verify the path exists and is a directory
        match ctx.backend.stat(Path::new(&resolved)).await {
            Ok(info) => {
                if info.is_dir {
                    let new_cwd = resolved.clone();
                    ctx.set_cwd(new_cwd);
                    // For `cd -`, output the new directory (like bash)
                    if path_arg == "-" {
                        ExecResult::success(resolved.to_string_lossy().to_string())
                    } else {
                        ExecResult::success("")
                    }
                } else {
                    ExecResult::failure(1, format!("cd: {}: Not a directory", path_arg))
                }
            }
            Err(e) => ExecResult::failure(1, format!("cd: {}: {}", path_arg, e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::path::PathBuf;
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.mkdir(Path::new("subdir")).await.unwrap();
        mem.write(Path::new("file.txt"), b"data").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_cd_subdir() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/subdir".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.cwd, PathBuf::from("/subdir"));
    }

    #[tokio::test]
    async fn test_cd_root() {
        let mut ctx = make_ctx().await;
        ctx.set_cwd(PathBuf::from("/subdir"));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.cwd, PathBuf::from("/"));
    }

    #[tokio::test]
    async fn test_cd_file_fails() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("Not a directory"));
    }

    #[tokio::test]
    async fn test_cd_nonexistent() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_cd_dash_previous_dir() {
        let mut ctx = make_ctx().await;

        // First cd to subdir
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/subdir".into()));
        let result = Cd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.cwd, PathBuf::from("/subdir"));

        // Now cd - should go back to /
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-".into()));
        let result = Cd.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(ctx.cwd, PathBuf::from("/"));
        // cd - prints the new directory
        assert_eq!(result.out, "/");
    }

    #[tokio::test]
    async fn test_cd_dash_toggles() {
        let mut ctx = make_ctx().await;

        // cd to subdir
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/subdir".into()));
        Cd.execute(args, &mut ctx).await;
        assert_eq!(ctx.cwd, PathBuf::from("/subdir"));

        // cd - back to /
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-".into()));
        Cd.execute(args, &mut ctx).await;
        assert_eq!(ctx.cwd, PathBuf::from("/"));

        // cd - back to /subdir
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-".into()));
        Cd.execute(args, &mut ctx).await;
        assert_eq!(ctx.cwd, PathBuf::from("/subdir"));
    }

    #[tokio::test]
    async fn test_cd_dash_no_previous() {
        let mut ctx = make_ctx().await;
        // Without any previous cd, OLDPWD is not set
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-".into()));

        let result = Cd.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("OLDPWD not set"));
    }
}
