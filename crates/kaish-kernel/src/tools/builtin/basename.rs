//! basename — Strip directory and suffix from filenames.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Basename tool: extract filename from path.
pub struct Basename;

#[async_trait]
impl Tool for Basename {
    fn name(&self) -> &str {
        "basename"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("basename", "Strip directory and suffix from filenames")
            .param(ParamSchema::required("path", "string", "Path to process"))
            .param(ParamSchema::optional(
                "suffix",
                "string",
                Value::Null,
                "Suffix to remove from the filename",
            ))
            .example("Extract filename", "basename /usr/bin/sort")
            .example("Remove extension", "basename /path/to/file.txt .txt")
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut ExecContext) -> ExecResult {
        let path_str = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "basename: missing path argument"),
        };

        let path = Path::new(&path_str);
        let filename = path
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or(&path_str);

        let suffix = args.get_string("suffix", 1);
        let result = match suffix {
            Some(ref s) if filename.ends_with(s.as_str()) => {
                &filename[..filename.len() - s.len()]
            }
            _ => filename,
        };

        ExecResult::with_output(OutputData::text(format!("{}\n", result)))
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
    async fn test_basename_simple() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/usr/bin/sort".into()));

        let result = Basename.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "sort");
    }

    #[tokio::test]
    async fn test_basename_with_suffix() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/path/to/file.txt".into()));
        args.positional.push(Value::String(".txt".into()));

        let result = Basename.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "file");
    }

    #[tokio::test]
    async fn test_basename_no_directory() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("filename.rs".into()));

        let result = Basename.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "filename.rs");
    }

    #[tokio::test]
    async fn test_basename_trailing_slash() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/path/to/dir/".into()));

        let result = Basename.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "dir");
    }

    #[tokio::test]
    async fn test_basename_missing_path() {
        let mut ctx = make_ctx();
        let result = Basename.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
