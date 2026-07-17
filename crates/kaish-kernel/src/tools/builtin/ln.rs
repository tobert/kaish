//! ln — Create symbolic links.
//!
//! Note: Hard links are not supported in the VFS abstraction.
//! Only symbolic links (-s) are implemented.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::ExecResult;
use crate::tools::builtin::get_path_string;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Ln tool: create symbolic links.
pub struct Ln;

/// clap-derived argv layer for ln.
#[derive(Parser, Debug)]
#[command(name = "ln", about = "Create symbolic links")]
struct LnArgs {
    /// Create symbolic link (-s) (required, hard links not supported)
    #[arg(short = 's', long = "symbolic")]
    symbolic: bool,

    /// Remove existing destination files (-f)
    #[arg(short = 'f', long = "force")]
    force: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Target path followed by the link name.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Ln {
    fn name(&self) -> &str {
        "ln"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &LnArgs::command(),
            "ln",
            "Create symbolic links",
            [
                ("Create symlink", "ln -s /path/to/target link_name"),
                ("Create with force", "ln -sf target.txt link.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("ln: {e}")),
        };
        let parsed = match LnArgs::try_parse_from(
            std::iter::once("ln".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("ln: {e}")),
        };
        parsed.global.apply(ctx);

        let target = match get_path_string(&args, "target", 0) {
            Ok(Some(t)) => t,
            Ok(None) => return ExecResult::failure(1, "ln: missing target argument"),
            Err(e) => return ExecResult::failure(1, format!("ln: {e}")),
        };

        let link_name = match get_path_string(&args, "link_name", 1) {
            Ok(Some(l)) => l,
            Ok(None) => return ExecResult::failure(1, "ln: missing link_name argument"),
            Err(e) => return ExecResult::failure(1, format!("ln: {e}")),
        };

        let symbolic = parsed.symbolic;
        let force = parsed.force;

        // Hard links are not supported
        if !symbolic {
            return ExecResult::failure(1, "ln: hard links not supported, use -s for symbolic links");
        }

        let link_path = ctx.resolve_path(&link_name);

        // Handle force flag - remove existing link
        if force && ctx.backend.exists(Path::new(&link_path)).await
            && let Err(e) = ctx.backend.remove(Path::new(&link_path), false).await {
                return ExecResult::failure(1, format!("ln: cannot remove '{}': {}", link_name, e));
            }

        // Create the symlink
        // Note: target is stored as-is (can be relative or absolute)
        match ctx.backend.symlink(Path::new(&target), Path::new(&link_path)).await {
            Ok(()) => ExecResult::success(""),
            Err(e) => ExecResult::failure(1, format!("ln: failed to create symbolic link '{}': {}", link_name, e)),
        }
    }
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
        mem.write(Path::new("target.txt"), b"content").await.unwrap();
        mem.mkdir(Path::new("targetdir")).await.unwrap();
        mem.write(Path::new("targetdir/file.txt"), b"nested").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ln_s_creates_symlink() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("target.txt".into()));
        args.positional.push(Value::String("/link.txt".into()));
        args.flags.insert("s".to_string());

        let result = Ln.execute(args, &mut ctx).await;
        assert!(result.ok(), "ln -s failed: {}", result.err);

        // Verify symlink was created
        let target = ctx.backend.read_link(Path::new("/link.txt")).await.unwrap();
        assert_eq!(target.to_string_lossy(), "target.txt");
    }

    #[tokio::test]
    async fn test_ln_s_with_symbolic_flag() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("target.txt".into()));
        args.positional.push(Value::String("/link2.txt".into()));
        args.flags.insert("symbolic".to_string());

        let result = Ln.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn test_ln_without_s_fails() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("target.txt".into()));
        args.positional.push(Value::String("/hardlink.txt".into()));

        let result = Ln.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("hard links not supported"));
    }

    #[tokio::test]
    async fn test_ln_s_force_replaces_existing() {
        let mut ctx = make_ctx().await;

        // Create initial symlink
        ctx.backend.symlink(Path::new("target.txt"), Path::new("/link.txt")).await.unwrap();

        // Try to create again without force - should fail
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("targetdir".into()));
        args.positional.push(Value::String("/link.txt".into()));
        args.flags.insert("s".to_string());

        let result = Ln.execute(args, &mut ctx).await;
        assert!(!result.ok(), "ln -s should fail when target exists");

        // Now with force
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("targetdir".into()));
        args.positional.push(Value::String("/link.txt".into()));
        args.flags.insert("s".to_string());
        args.flags.insert("f".to_string());

        let result = Ln.execute(args, &mut ctx).await;
        assert!(result.ok(), "ln -sf failed: {}", result.err);

        // Verify new target
        let target = ctx.backend.read_link(Path::new("/link.txt")).await.unwrap();
        assert_eq!(target.to_string_lossy(), "targetdir");
    }

    #[tokio::test]
    async fn test_ln_missing_args() {
        let mut ctx = make_ctx().await;

        // No args
        let result = Ln.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("target"));

        // Only target
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("target.txt".into()));
        args.flags.insert("s".to_string());
        let result = Ln.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("link_name"));
    }

    #[tokio::test]
    async fn test_ln_s_to_directory() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("targetdir".into()));
        args.positional.push(Value::String("/dirlink".into()));
        args.flags.insert("s".to_string());

        let result = Ln.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify symlink points to directory
        let target = ctx.backend.read_link(Path::new("/dirlink")).await.unwrap();
        assert_eq!(target.to_string_lossy(), "targetdir");
    }

    #[tokio::test]
    async fn test_ln_s_broken_symlink() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("nonexistent".into()));
        args.positional.push(Value::String("/broken.txt".into()));
        args.flags.insert("s".to_string());

        // Creating a symlink to non-existent target should succeed
        let result = Ln.execute(args, &mut ctx).await;
        assert!(result.ok(), "ln -s to nonexistent target should succeed: {}", result.err);

        // Verify symlink exists
        let target = ctx.backend.read_link(Path::new("/broken.txt")).await.unwrap();
        assert_eq!(target.to_string_lossy(), "nonexistent");
    }
}
