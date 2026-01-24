//! stat — Display file status.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Stat tool: display file or filesystem status.
pub struct Stat;

#[async_trait]
impl Tool for Stat {
    fn name(&self) -> &str {
        "stat"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("stat", "Display file status")
            .param(ParamSchema::required("path", "string", "File to stat"))
            .param(ParamSchema::optional(
                "format",
                "string",
                Value::Null,
                "Output format (--format). Supports: %n (name), %s (size), %F (type)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path_str = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "stat: missing path argument"),
        };

        let resolved = ctx.resolve_path(&path_str);
        let format = args
            .get_string("format", usize::MAX)
            .or_else(|| args.get_string("c", usize::MAX));

        match ctx.backend.stat(Path::new(&resolved)).await {
            Ok(info) => {
                let output = if let Some(fmt) = format {
                    format_stat(&fmt, &path_str, &info)
                } else {
                    // Default output similar to stat(1)
                    let file_type = if info.is_dir {
                        "directory"
                    } else {
                        "regular file"
                    };
                    format!(
                        "  File: {}\n  Size: {}\t\tType: {}\n",
                        path_str, info.size, file_type
                    )
                };
                ExecResult::success(output)
            }
            Err(e) => ExecResult::failure(1, format!("stat: {}: {}", path_str, e)),
        }
    }
}

/// Format stat output according to a format string.
fn format_stat(fmt: &str, name: &str, info: &crate::backend::EntryInfo) -> String {
    let mut result = fmt.to_string();

    // %n - file name
    result = result.replace("%n", name);

    // %s - size in bytes
    result = result.replace("%s", &info.size.to_string());

    // %F - file type
    let file_type = if info.is_dir { "directory" } else { "regular file" };
    result = result.replace("%F", file_type);

    // Add newline if not present
    if !result.ends_with('\n') {
        result.push('\n');
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("file.txt"), b"hello world")
            .await
            .unwrap();
        mem.mkdir(Path::new("mydir")).await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_stat_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("file.txt"));
        assert!(result.out.contains("11")); // "hello world" = 11 bytes
        assert!(result.out.contains("regular file"));
    }

    #[tokio::test]
    async fn test_stat_directory() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/mydir".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("directory"));
    }

    #[tokio::test]
    async fn test_stat_format() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named
            .insert("format".to_string(), Value::String("%n: %s bytes".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.out.trim(), "/file.txt: 11 bytes");
    }

    #[tokio::test]
    async fn test_stat_not_found() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_stat_missing_path() {
        let mut ctx = make_ctx().await;
        let result = Stat.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
