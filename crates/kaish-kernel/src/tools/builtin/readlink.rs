//! readlink — Print resolved path (canonicalize mode only).
//!
//! Note: kaish VFS doesn't support symlinks, so only -f (canonicalize) mode works.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Readlink tool: canonicalize a path.
pub struct Readlink;

#[async_trait]
impl Tool for Readlink {
    fn name(&self) -> &str {
        "readlink"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("readlink", "Print the resolved absolute path")
            .param(ParamSchema::required("path", "string", "Path to resolve"))
            .param(ParamSchema::optional(
                "canonicalize",
                "bool",
                Value::Bool(false),
                "Resolve to canonical absolute path (-f)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path_str = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "readlink: missing path argument"),
        };

        let canonicalize = args.has_flag("canonicalize") || args.has_flag("f");
        let resolved = ctx.resolve_path(&path_str);
        let resolved_str = resolved.to_string_lossy();

        if canonicalize {
            // Normalize the path (resolve . and ..)
            let normalized = normalize_path(&resolved_str);
            return ExecResult::success(format!("{}\n", normalized));
        }

        // Without -f, readlink operates on symlinks (not supported in VFS)
        ExecResult::failure(1, format!("readlink: {}: not a symbolic link", path_str))
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
    async fn test_readlink_without_flag_fails() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/some/path".into()));

        let result = Readlink.execute(args, &mut ctx).await;
        // Without -f, should fail (no symlink support)
        assert!(!result.ok());
        assert!(result.err.contains("not a symbolic link"));
    }

    #[tokio::test]
    async fn test_readlink_missing_path() {
        let mut ctx = make_ctx();
        let result = Readlink.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
