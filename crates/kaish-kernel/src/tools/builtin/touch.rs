//! touch — Change file timestamps or create empty files.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;
use kaish_types::clock::system_now;

use crate::backend::WriteMode;
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Touch tool: change file timestamps or create files.
pub struct Touch;

/// clap-derived argv layer for touch.
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

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("touch: {e}")),
        };
        let parsed = match TouchArgs::try_parse_from(
            std::iter::once("touch".to_string()).chain(argv),
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
            let path_str = match crate::interpreter::value_to_text_sink_named(value, "a path") {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("touch: {e}")),
            };
            let resolved = ctx.resolve_path(&path_str);
            let path = Path::new(&resolved);

            // Always route through the VFS — never escape to the host via
            // resolve_real_path. The backend updates the mtime where it can
            // (local + memory mounts) and rejects on read-only/virtual mounts
            // rather than silently reporting a success it didn't deliver.
            let result = if ctx.backend.exists(path).await {
                ctx.backend.set_mtime(path, system_now()).await
            } else {
                ctx.backend.write(path, &[], WriteMode::CreateNew).await
            };
            if let Err(e) = result {
                last_err = Some(format!("touch: {}: {}", path_str, e));
            }
        }
        match last_err {
            Some(msg) => ExecResult::failure(1, msg),
            None => ExecResult::success(""),
        }
    }
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

    /// touch on an existing file must update its mtime *through the VFS*, not
    /// no-op. A MemoryFs tracks per-entry timestamps, so a regression that
    /// reinstated the silent virtual no-op would leave the mtime unchanged.
    #[tokio::test]
    async fn test_touch_updates_memory_mtime() {
        let mut ctx = make_ctx().await;
        let before = ctx
            .backend
            .stat(Path::new("/existing.txt"))
            .await
            .unwrap()
            .modified
            .expect("MemoryFs records mtime");

        // Pin the mtime to a known *past* instant. Reading it back proves
        // set_mtime actually wrote through the VFS.
        let past = before - std::time::Duration::from_secs(3600);
        ctx.backend
            .set_mtime(Path::new("/existing.txt"), past)
            .await
            .expect("set_mtime via VFS");
        let pinned = ctx
            .backend
            .stat(Path::new("/existing.txt"))
            .await
            .unwrap()
            .modified
            .unwrap();
        assert_eq!(pinned, past, "set_mtime did not record the timestamp");

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/existing.txt".into()));
        let result = Touch.execute(args, &mut ctx).await;
        assert!(result.ok());

        let after = ctx
            .backend
            .stat(Path::new("/existing.txt"))
            .await
            .unwrap()
            .modified
            .unwrap();
        // touch set it to "now", later than the pinned past — proving touch
        // advanced the mtime rather than silently no-opping.
        assert!(after > past, "touch did not advance the mtime");
    }

    /// touch on a read-only mount must fail loudly, never silently succeed by
    /// escaping to the host filesystem.
    ///
    /// Gated on `localfs`: `LocalFs` only exists with that feature, and without
    /// the gate the import broke `cargo test --lib --no-default-features`.
    #[cfg(feature = "localfs")]
    #[tokio::test]
    async fn test_touch_existing_readonly_rejects() {
        use crate::vfs::LocalFs;
        use std::sync::Arc;

        let dir = tempfile::tempdir().unwrap();
        std::fs::write(dir.path().join("ro.txt"), b"x").unwrap();

        let mut vfs = VfsRouter::new();
        vfs.mount("/", LocalFs::read_only(dir.path().to_path_buf()));
        let mut ctx = ExecContext::new(Arc::new(vfs));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/ro.txt".into()));
        let result = Touch.execute(args, &mut ctx).await;
        assert!(!result.ok(), "touch on read-only mount must fail");
    }
}
