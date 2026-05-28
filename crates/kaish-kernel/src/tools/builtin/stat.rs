//! stat — Display file status.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Stat tool: display file or filesystem status.
pub struct Stat;

/// clap-derived argv layer for stat. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "stat", about = "Display file status")]
struct StatArgs {
    /// Output format (--format). Supports: %n (name), %s (size), %F (type)
    #[arg(short = 'c', long)]
    format: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Files to stat.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Stat {
    fn name(&self) -> &str {
        "stat"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &StatArgs::command(),
            "stat",
            "Display file status",
            [
                ("Show file info", "stat README.md"),
                ("Just the size", "stat --format '%s' file.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let parsed = match StatArgs::try_parse_from(
            std::iter::once("stat".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("stat: {e}")),
        };
        parsed.global.apply(ctx);

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
                    ExecResult::with_output(OutputData::text(output))
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
                        "FILE".to_string(),
                        "SIZE".to_string(),
                        "TYPE".to_string(),
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
    use crate::ast::Value;
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
        assert!(result.has_output());
        let output = result.output().unwrap();
        assert!(output.headers.is_some());
        assert_eq!(output.root.len(), 1);
        // Canonical output contains file info
        assert!(result.text_out().contains("file.txt"));
        assert!(result.text_out().contains("11")); // "hello world" = 11 bytes
    }

    #[tokio::test]
    async fn test_stat_directory() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/mydir".into()));

        let result = Stat.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.has_output());
        let output = result.output().unwrap();
        assert_eq!(output.root.len(), 1);
        // Check canonical output contains directory
        assert!(result.text_out().contains("directory"));
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
        assert_eq!(result.text_out().trim(), "/file.txt: 11 bytes");
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
