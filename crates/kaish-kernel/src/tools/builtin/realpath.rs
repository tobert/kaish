//! realpath — Print the resolved absolute pathname.

use async_trait::async_trait;
use std::path::Path;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Realpath tool: resolve path to absolute, canonical form.
pub struct Realpath;

#[async_trait]
impl Tool for Realpath {
    fn name(&self) -> &str {
        "realpath"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("realpath", "Print the resolved absolute pathname")
            .param(ParamSchema::required("path", "string", "Path to resolve"))
            .example("Resolve a path", "realpath ../lib")
            .example("Normalize path", "realpath /usr/bin/../lib")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path_str = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "realpath: missing path argument"),
        };

        // Resolve the path using the context (handles relative paths)
        let resolved = ctx.resolve_path(&path_str);
        let resolved_str = resolved.to_string_lossy();

        // Normalize the path (remove . and ..)
        let normalized = normalize_path(&resolved_str);

        ExecResult::with_output(OutputData::text(format!("{}\n", normalized)))
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
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_realpath_absolute() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/usr/bin/sort".into()));

        let result = Realpath.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "/usr/bin/sort");
    }

    #[tokio::test]
    async fn test_realpath_with_dotdot() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/usr/bin/../lib".into()));

        let result = Realpath.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "/usr/lib");
    }

    #[tokio::test]
    async fn test_realpath_with_dot() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/usr/./bin/./sort".into()));

        let result = Realpath.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "/usr/bin/sort");
    }

    #[tokio::test]
    async fn test_realpath_relative() {
        let mut ctx = make_ctx();
        ctx.set_cwd(std::path::PathBuf::from("/home/user"));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("documents/file.txt".into()));

        let result = Realpath.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "/home/user/documents/file.txt");
    }

    #[tokio::test]
    async fn test_realpath_missing_path() {
        let mut ctx = make_ctx();
        let result = Realpath.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }

    #[test]
    fn test_normalize_path() {
        assert_eq!(normalize_path("/usr/bin/../lib"), "/usr/lib");
        assert_eq!(normalize_path("/usr/./bin"), "/usr/bin");
        assert_eq!(normalize_path("/a/b/c/../../d"), "/a/d");
        assert_eq!(normalize_path("/"), "/");
    }
}
