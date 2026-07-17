//! dirname — Strip last component from filename.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Dirname tool: extract directory from path.
pub struct Dirname;

/// clap-derived argv layer for dirname.
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

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("dirname: {e}")),
        };
        let parsed = match DirnameArgs::try_parse_from(
            std::iter::once("dirname".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("dirname: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(1, "dirname: missing path argument");
        }

        // POSIX: `dirname a/b c/d` prints one parent per line.
        let mut output = String::new();
        for value in &args.positional {
            let path_str = match crate::interpreter::value_to_text_sink_named(value, "a path") {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("dirname: {e}")),
            };
            // POSIX: a path consisting entirely of slashes (e.g. `//`, `///`)
            // has itself as its own dirname — just like `/`. `Path::parent()`
            // returns `None` for such paths, so we special-case before using it.
            let result = if !path_str.is_empty() && path_str.chars().all(|c| c == '/') {
                "/".to_string()
            } else {
                let parent = Path::new(&path_str)
                    .parent()
                    .and_then(|p| p.to_str())
                    .unwrap_or(".");
                // Empty parent (relative paths like "file") → "."
                if parent.is_empty() { ".".to_string() } else { parent.to_string() }
            };
            output.push_str(&result);
            output.push('\n');
        }
        ExecResult::with_output(OutputData::text(output))
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
