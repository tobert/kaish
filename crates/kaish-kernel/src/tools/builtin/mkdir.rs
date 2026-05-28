//! mkdir — Create directories.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Mkdir tool: create directories.
pub struct Mkdir;

/// clap-derived argv layer for mkdir. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "mkdir", about = "Create directories")]
struct MkdirArgs {
    /// Create parent directories as needed. Accepted for POSIX compatibility;
    /// the backend always creates parent directories.
    #[arg(short = 'p', long = "parents")]
    _parents: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Directory paths to create.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Mkdir {
    fn name(&self) -> &str {
        "mkdir"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &MkdirArgs::command(),
            "mkdir",
            "Create directories",
            [
                ("Create a directory", "mkdir output"),
                ("Create nested directories", "mkdir -p src/utils/helpers"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let parsed = match MkdirArgs::try_parse_from(
            std::iter::once("mkdir".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("mkdir: {e}")),
        };
        parsed.global.apply(ctx);

        let path = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "mkdir: missing path argument"),
        };

        let resolved = ctx.resolve_path(&path);

        match ctx.backend.mkdir(Path::new(&resolved)).await {
            Ok(()) => ExecResult::success(""),
            Err(e) => ExecResult::failure(1, format!("mkdir: {}: {}", path, e)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_mkdir_simple() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/newdir".into()));

        let result = Mkdir.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify it exists via backend
        let info = ctx.backend.stat(Path::new("/newdir")).await.unwrap();
        assert!(info.is_dir());
    }

    #[tokio::test]
    async fn test_mkdir_nested() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/a/b/c".into()));

        let result = Mkdir.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify nested dirs exist
        assert!(ctx.backend.stat(Path::new("/a")).await.unwrap().is_dir());
        assert!(ctx.backend.stat(Path::new("/a/b")).await.unwrap().is_dir());
        assert!(ctx.backend.stat(Path::new("/a/b/c")).await.unwrap().is_dir());
    }

    #[tokio::test]
    async fn test_mkdir_no_arg() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Mkdir.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing"));
    }
}
