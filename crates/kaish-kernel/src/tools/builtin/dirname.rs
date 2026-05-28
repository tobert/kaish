//! dirname — Strip last component from filename.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Dirname tool: extract directory from path.
pub struct Dirname;

/// clap-derived argv layer for dirname. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "dirname", about = "Strip last component from filename")]
struct DirnameArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Paths to extract the directory component from.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Dirname {
    fn name(&self) -> &str {
        "dirname"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &DirnameArgs::command(),
            "dirname",
            "Strip last component from filename",
            [
                ("Get directory part", "dirname /usr/bin/sort"),
                ("Relative path", "dirname path/to/file.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let parsed = match DirnameArgs::try_parse_from(
            std::iter::once("dirname".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("dirname: {e}")),
        };
        parsed.global.apply(ctx);

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
        assert_eq!(result.text_out().trim(), "/usr/bin");
    }

    #[tokio::test]
    async fn test_dirname_relative() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("path/to/file.txt".into()));

        let result = Dirname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "path/to");
    }

    #[tokio::test]
    async fn test_dirname_no_directory() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("filename".into()));

        let result = Dirname.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), ".");
    }

    #[tokio::test]
    async fn test_dirname_root() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Dirname.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Root's parent is itself
        assert_eq!(result.text_out().trim(), "/");
    }

    #[tokio::test]
    async fn test_dirname_missing_path() {
        let mut ctx = make_ctx();
        let result = Dirname.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
