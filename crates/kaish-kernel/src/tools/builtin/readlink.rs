//! readlink — Print symlink target or resolved path.
//!
//! Without flags, prints the raw symlink target.
//! With -f, canonicalizes the path (resolves symlinks and normalizes).

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Readlink tool: read symlink target or canonicalize a path.
pub struct Readlink;

#[async_trait]
impl Tool for Readlink {
    fn name(&self) -> &str {
        "readlink"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("readlink", "Print symlink target or resolved path")
            .param(ParamSchema::required("path", "string", "Path to read or resolve"))
            .param(ParamSchema::optional(
                "canonicalize",
                "bool",
                Value::Bool(false),
                "Resolve to canonical absolute path (-f)",
            ))
            .example("Read symlink target", "readlink link.txt")
            .example("Canonicalize path", "readlink -f ../some/./path")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path_str = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "readlink: missing path argument"),
        };

        let canonicalize = args.has_flag("canonicalize") || args.has_flag("f");
        let resolved = ctx.resolve_path(&path_str);

        if canonicalize {
            // Normalize the path (resolve . and ..)
            let resolved_str = resolved.to_string_lossy();
            let normalized = normalize_path(&resolved_str);
            return ExecResult::success(format!("{}\n", normalized));
        }

        // Without -f, read the raw symlink target
        match ctx.backend.read_link(Path::new(&resolved)).await {
            Ok(target) => ExecResult::success(format!("{}\n", target.display())),
            Err(e) => {
                use crate::backend::BackendError;
                match &e {
                    BackendError::Io(msg) if msg.contains("not a symbolic link") => {
                        ExecResult::failure(1, format!("readlink: {}: not a symbolic link", path_str))
                    }
                    BackendError::NotFound(_) => {
                        ExecResult::failure(1, format!("readlink: {}: No such file or directory", path_str))
                    }
                    _ => {
                        ExecResult::failure(1, format!("readlink: {}: {}", path_str, e))
                    }
                }
            }
        }
    }
}

/// Normalize a path by resolving . and .. components.
fn normalize_path(path: &str) -> String {
    let path = Path::new(path);
    let mut components = Vec::new();
    let is_absolute = path.is_absolute();

    for component in path.components() {
        match component {
            std::path::Component::ParentDir => {
                if !components.is_empty() && components.last() != Some(&"..") {
                    components.pop();
                } else if !is_absolute {
                    components.push("..");
                }
            }
            std::path::Component::CurDir => {}
            std::path::Component::Normal(s) => {
                if let Some(s) = s.to_str() {
                    components.push(s);
                }
            }
            std::path::Component::RootDir => {
                components.clear();
            }
            std::path::Component::Prefix(_) => {}
        }
    }

    if is_absolute {
        format!("/{}", components.join("/"))
    } else if components.is_empty() {
        ".".to_string()
    } else {
        components.join("/")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_readlink_canonicalize() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/some/../path/./file".into()));
        args.flags.insert("f".to_string());

        let result = Readlink.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "/path/file");
    }

    #[tokio::test]
    async fn test_readlink_without_flag_on_regular_file_fails() {
        use crate::vfs::Filesystem;

        // Create a context with an actual file
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("regular.txt"), b"content").await.unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/regular.txt".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        // Without -f, on a regular file should fail (not a symlink)
        assert!(!result.ok());
        assert!(result.err.contains("not a symbolic link"));
    }

    #[tokio::test]
    async fn test_readlink_missing_path() {
        let mut ctx = make_ctx();
        let result = Readlink.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }

    // --- Tests for symlink reading ---

    async fn make_ctx_with_symlink() -> ExecContext {
        use crate::vfs::Filesystem;

        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        // Create a file and a symlink to it
        mem.write(Path::new("target.txt"), b"content").await.unwrap();
        mem.symlink(Path::new("target.txt"), Path::new("link.txt")).await.unwrap();

        // Create a broken symlink
        mem.symlink(Path::new("nonexistent"), Path::new("broken.txt")).await.unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_readlink_symlink_target() {
        let mut ctx = make_ctx_with_symlink().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/link.txt".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "target.txt");
    }

    #[tokio::test]
    async fn test_readlink_on_regular_file() {
        let mut ctx = make_ctx_with_symlink().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/target.txt".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("not a symbolic link"));
    }

    #[tokio::test]
    async fn test_readlink_broken_symlink() {
        let mut ctx = make_ctx_with_symlink().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/broken.txt".into()));

        // readlink on a broken symlink should still work (returns target)
        let result = Readlink.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "nonexistent");
    }

    #[tokio::test]
    async fn test_readlink_not_found() {
        let mut ctx = make_ctx_with_symlink().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/does-not-exist".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("No such file"));
    }
}
