//! basename — Strip directory and suffix from filenames.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

#[cfg(test)]
use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Basename tool: extract filename from path.
pub struct Basename;

/// clap-derived argv layer for basename.
#[derive(Parser, Debug)]
#[command(name = "basename", about = "Strip directory and suffix from filenames")]
struct BasenameArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Path followed by an optional suffix to strip.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Basename {
    fn name(&self) -> &str {
        "basename"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &BasenameArgs::command(),
            "basename",
            "Strip directory and suffix from filenames",
            [
                ("Extract filename", "basename /usr/bin/sort"),
                ("Remove extension", "basename /path/to/file.txt .txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let parsed = match BasenameArgs::try_parse_from(
            std::iter::once("basename".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("basename: {e}")),
        };
        parsed.global.apply(ctx);

        let path_str = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "basename: missing path argument"),
        };

        let path = Path::new(&path_str);
        // POSIX: a path consisting entirely of slashes (e.g. `//`) has itself
        // reduced to a single `/`. `Path::file_name()` returns `None` for such
        // paths, so we special-case before delegating to it.
        let filename = if !path_str.is_empty() && path_str.chars().all(|c| c == '/') {
            "/"
        } else {
            path.file_name()
                .and_then(|s| s.to_str())
                .unwrap_or(&path_str)
        };

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
        assert_eq!(result.text_out().trim(), "sort");
    }

    #[tokio::test]
    async fn test_basename_with_suffix() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/path/to/file.txt".into()));
        args.positional.push(Value::String(".txt".into()));

        let result = Basename.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "file");
    }

    #[tokio::test]
    async fn test_basename_no_directory() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("filename.rs".into()));

        let result = Basename.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "filename.rs");
    }

    #[tokio::test]
    async fn test_basename_trailing_slash() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/path/to/dir/".into()));

        let result = Basename.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "dir");
    }

    #[tokio::test]
    async fn test_basename_missing_path() {
        let mut ctx = make_ctx();
        let result = Basename.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
