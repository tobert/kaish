//! dirname — Strip last component from filename.

use async_trait::async_trait;
use std::path::Path;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Dirname tool: extract directory from path.
pub struct Dirname;

#[async_trait]
impl Tool for Dirname {
    fn name(&self) -> &str {
        "dirname"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("dirname", "Strip last component from filename")
            .param(ParamSchema::required("path", "string", "Path to process"))
            .example("Get directory part", "dirname /usr/bin/sort")
            .example("Relative path", "dirname path/to/file.txt")
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let path_str = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "dirname: missing path argument"),
        };

        let path = Path::new(&path_str);

        // Special case: root "/" has itself as parent
        if path_str == "/" {
            return ExecResult::with_output(OutputData::text("/\n"));
        }

        let parent = path
            .parent()
            .and_then(|p| p.to_str())
            .unwrap_or(".");

        // Handle empty parent (happens for relative paths like "file")
        let result = if parent.is_empty() { "." } else { parent };

        ExecResult::with_output(OutputData::text(format!("{}\n", result)))
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
    async fn test_dirname_absolute() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/usr/bin/sort".into()));

        let result = Dirname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "/usr/bin");
    }

    #[tokio::test]
    async fn test_dirname_relative() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("path/to/file.txt".into()));

        let result = Dirname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "path/to");
    }

    #[tokio::test]
    async fn test_dirname_no_directory() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("filename".into()));

        let result = Dirname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), ".");
    }

    #[tokio::test]
    async fn test_dirname_root() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Dirname.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Root's parent is itself
        assert_eq!(result.out.trim(), "/");
    }

    #[tokio::test]
    async fn test_dirname_missing_path() {
        let mut ctx = make_ctx();
        let result = Dirname.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
