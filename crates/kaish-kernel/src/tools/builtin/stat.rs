//! stat — Display file status.

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
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
            .example("Show file info", "stat README.md")
            .example("Just the size", "stat --format '%s' file.txt")
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
                if let Some(fmt) = format {
                    // Custom format mode: return plain text
                    let output = format_stat(&fmt, &path_str, &info);
                    ExecResult::success(output)
                } else {
                    // Default output: use structured OutputData
                    let is_dir = info.is_dir();
                    let file_type = if is_dir {
                        "directory"
                    } else {
                        "regular file"
                    };
                    let entry_type = if is_dir {
                        EntryType::Directory
                    } else {
                        EntryType::File
                    };

                    let node = OutputNode::new(&path_str)
                        .with_cells(vec![
                            info.size.to_string(),
                            file_type.to_string(),
                        ])
                        .with_entry_type(entry_type);

                    let headers = vec![
                        "File".to_string(),
                        "Size".to_string(),
                        "Type".to_string(),
                    ];

                    ExecResult::with_output(OutputData::table(headers, vec![node]))
                }
            }
            Err(e) => ExecResult::failure(1, format!("stat: {}: {}", path_str, e)),
        }
    }
}

/// Format stat output according to a format string.
fn format_stat(fmt: &str, name: &str, info: &crate::vfs::DirEntry) -> String {
    let mut result = fmt.to_string();

    // %n - file name
    result = result.replace("%n", name);

    // %s - size in bytes
    result = result.replace("%s", &info.size.to_string());

    // %F - file type
    let file_type = if info.is_dir() { "directory" } else { "regular file" };
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
        // Check OutputData structure
        assert!(result.output.is_some());
        let output = result.output.as_ref().unwrap();
        assert!(output.headers.is_some());
        assert_eq!(output.root.len(), 1);
        // Canonical output contains file info
        assert!(result.out.contains("file.txt"));
        assert!(result.out.contains("11")); // "hello world" = 11 bytes
    }

    #[tokio::test]
    async fn test_stat_directory() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/mydir".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.output.is_some());
        let output = result.output.as_ref().unwrap();
        assert_eq!(output.root.len(), 1);
        // Check canonical output contains directory
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
