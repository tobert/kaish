//! ln — Create symbolic links.
//!
//! Note: Hard links are not supported in the VFS abstraction.
//! Only symbolic links (-s) are implemented.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Ln tool: create symbolic links.
pub struct Ln;

#[async_trait]
impl Tool for Ln {
    fn name(&self) -> &str {
        "ln"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("ln", "Create symbolic links")
            .param(ParamSchema::required("target", "string", "Target path (what the link points to)"))
            .param(ParamSchema::required("link_name", "string", "Name of the symbolic link to create"))
            .param(ParamSchema::optional(
                "symbolic",
                "bool",
                Value::Bool(false),
                "Create symbolic link (-s) (required, hard links not supported)",
            ).with_aliases(["-s"]))
            .param(ParamSchema::optional(
                "force",
                "bool",
                Value::Bool(false),
                "Remove existing destination files (-f)",
            ).with_aliases(["-f"]))
            .example("Create symlink", "ln -s /path/to/target link_name")
            .example("Create with force", "ln -sf target.txt link.txt")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let target = match args.get_string("target", 0) {
            Some(t) => t,
            None => return ExecResult::failure(1, "ln: missing target argument"),
        };

        let link_name = match args.get_string("link_name", 1) {
            Some(l) => l,
            None => return ExecResult::failure(1, "ln: missing link_name argument"),
        };

        let symbolic = args.has_flag("symbolic") || args.has_flag("s");
        let force = args.has_flag("force") || args.has_flag("f");

        // Hard links are not supported
        if !symbolic {
            return ExecResult::failure(1, "ln: hard links not supported, use -s for symbolic links");
        }

        let link_path = ctx.resolve_path(&link_name);

        // Handle force flag - remove existing link
        if force && ctx.backend.exists(Path::new(&link_path)).await {
            if let Err(e) = ctx.backend.remove(Path::new(&link_path), false).await {
                return ExecResult::failure(1, format!("ln: cannot remove '{}': {}", link_name, e));
            }
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
