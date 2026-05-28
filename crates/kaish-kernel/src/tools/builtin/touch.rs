//! touch — Change file timestamps or create empty files.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::backend::WriteMode;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Touch tool: change file timestamps or create files.
pub struct Touch;

/// clap-derived argv layer for touch. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "touch", about = "Change file timestamps or create empty files")]
struct TouchArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Files to create or whose timestamps to update.
    paths: Vec<String>,
}

#[async_trait]
impl Tool for Touch {
    fn name(&self) -> &str {
        "touch"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TouchArgs::command(),
            "touch",
            "Change file timestamps or create empty files",
            [
                ("Create empty file", "touch newfile.txt"),
                ("Update timestamp", "touch existing.txt"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let parsed = match TouchArgs::try_parse_from(
            std::iter::once("touch".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("touch: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(1, "touch: missing path argument");
        }

        // POSIX: `touch a b c` touches each. Continue past errors and report
        // the last failure rather than bailing at the first.
        let mut last_err: Option<String> = None;
        for value in &args.positional {
            let path_str = crate::interpreter::value_to_string(value);
            let resolved = ctx.resolve_path(&path_str);
            let path = Path::new(&resolved);

            if ctx.backend.exists(path).await {
                // File exists, update timestamp via real-FS path when available.
                if let Some(real_path) = ctx.backend.resolve_real_path(path)
                    && let Err(e) = update_mtime(&real_path) {
                        last_err = Some(format!("touch: {}: {}", path_str, e));
                    }
                // Virtual filesystems (MemoryFs): no-op.
            } else if let Err(e) = ctx.backend.write(path, &[], WriteMode::CreateNew).await {
                last_err = Some(format!("touch: {}: {}", path_str, e));
            }
        }
        match last_err {
            Some(msg) => ExecResult::failure(1, msg),
            None => ExecResult::success(""),
        }
    }
}

/// Update modification time of a file to the current time.
///
/// Uses File::set_modified() which calls futimens/utimensat on Unix.
fn update_mtime(path: &Path) -> std::io::Result<()> {
    use std::time::SystemTime;

    let file = std::fs::OpenOptions::new()
        .write(true)
        .open(path)?;

    file.set_modified(SystemTime::now())
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
        mem.write(Path::new("existing.txt"), b"content")
            .await
            .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_touch_create_new() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/newfile.txt".into()));

        let result = Touch.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify file was created
        assert!(ctx.backend.exists(Path::new("/newfile.txt")).await);
        let data = ctx
            .backend
            .read(Path::new("/newfile.txt"), None)
            .await
            .unwrap();
        assert!(data.is_empty());
    }

    #[tokio::test]
    async fn test_touch_existing() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));

        let result = Touch.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify file still has original content
        let data = ctx
            .backend
            .read(Path::new("/existing.txt"), None)
            .await
            .unwrap();
        assert_eq!(data, b"content");
    }

    #[tokio::test]
    async fn test_touch_missing_path() {
        let mut ctx = make_ctx().await;
        let result = Touch.execute(ToolArgs::new(), &mut ctx).await;
        assert!(!result.ok());
    }
}
