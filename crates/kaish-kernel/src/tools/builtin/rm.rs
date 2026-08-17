//! rm — Remove files and directories.
//!
//! Under trash-on-delete (`set -o trash`) a delete lands in the trash first,
//! so a mistake is recoverable — the trash IS the recovery net for safe
//! autonomous operation.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::{Path, PathBuf};

use crate::backend::BackendError;
use crate::interpreter::ExecResult;
use crate::operation::KernelOperation;
use crate::tools::{is_trash_excluded, schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// clap-derived argv layer for rm.
#[derive(Parser, Debug)]
#[command(name = "rm", about = "Remove files and directories")]
struct RmArgs {
    /// Remove directories and their contents recursively.
    #[arg(short = 'r', long = "recursive")]
    recursive: bool,

    /// Alias for -r (same behavior).
    #[arg(short = 'R')]
    recursive_upper: bool,

    /// Ignore nonexistent files, never prompt.
    #[arg(short = 'f', long = "force")]
    force: bool,


    #[command(flatten)]
    global: GlobalFlags,

    /// Files or directories to remove.
    paths: Vec<String>,
}

/// Rm tool: remove files and directories.
pub struct Rm;

/// What the rm decision logic chose to do.
#[derive(Debug, PartialEq)]
enum RmAction {
    /// Move to freedesktop.org Trash.
    Trash(PathBuf),
    /// Permanent delete (via backend).
    Delete,
}

/// Determine the rm action from the trash settings and the file's properties.
fn decide_rm_action(
    trash_enabled: bool,
    real_path: Option<&Path>,
    file_size: Option<u64>,
    trash_max_size: u64,
    is_dir: bool,
    is_symlink: bool,
) -> RmAction {
    // A symlink is a pointer, not the data it names. `real_path` canonicalizes
    // *through* the link, so trashing a symlink would move its TARGET to trash —
    // exactly the follow-the-symlink hazard we're closing. The link itself is
    // trivially recreatable, so symlinks bypass trash and are unlinked directly.
    if is_symlink {
        return RmAction::Delete;
    }

    if trash_enabled {
        if let Some(rp) = real_path {
            // Skip trash for excluded paths (host scratch under /tmp). Shared
            // with the overwrite gate via `is_trash_excluded` so it can't drift.
            if !is_trash_excluded(Some(rp)) {
                // Directories always go to trash — stat size is unreliable
                // and trash::delete handles them atomically.
                if is_dir {
                    return RmAction::Trash(rp.to_path_buf());
                }
                let size = file_size.unwrap_or(0);
                if size <= trash_max_size {
                    return RmAction::Trash(rp.to_path_buf());
                }
                // File too big for trash — deleted directly. Nothing in
                // the kernel holds a delete back; an embedder that wants to
                // refuse one reads the plan before it runs.
                return RmAction::Delete;
            }
        }
        // Virtual path (no real path) or excluded path — fall through
    }

    RmAction::Delete
}

#[async_trait]
impl Tool for Rm {
    fn name(&self) -> &str {
        "rm"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &RmArgs::command(),
            "rm",
            "Remove files and directories",
            [
                ("Remove a file", "rm temp.txt"),
                ("Remove directory recursively", "rm -rf build/"),
            ],
        )
        .with_operations([KernelOperation::FsRemove.as_str()])
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        args.flagify_bool_named(&self.schema());

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("rm: {e}")),
        };
        let parsed = match RmArgs::try_parse_from(
            std::iter::once("rm".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("rm: {e}")),
        };
        parsed.global.apply(ctx);

        if args.positional.is_empty() {
            return ExecResult::failure(1, "rm: missing path argument");
        }

        let recursive = parsed.recursive || parsed.recursive_upper;
        let force = parsed.force;

        let trash_enabled = ctx.scope.trash_enabled();
        let trash_max_size = ctx.scope.trash_max_size();
        // Collect per-path decisions in one pass. Stat failures
        // short-circuit unless -f.
        struct Decision {
            path: String,
            resolved: PathBuf,
            action: RmAction,
        }
        let mut decisions: Vec<Decision> = Vec::with_capacity(args.positional.len());
        for value in &args.positional {
            let path = match crate::interpreter::value_to_text_sink_named(value, "a path") {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(1, format!("rm: {e}")),
            };
            let resolved = ctx.resolve_path(&path);
            // lstat, never stat: classify the link itself, so a symlink-to-dir
            // is treated as a (non-dir) symlink rather than its target. This is
            // what keeps `rm`/`rm -r` from following a link to its target.
            let entry = match ctx.backend.lstat(Path::new(&resolved)).await {
                Ok(info) => Some(info),
                Err(BackendError::NotFound(_)) if force => continue, // -f skips missing
                Err(BackendError::NotFound(_)) => {
                    return ExecResult::failure(1, format!("rm: {}: No such file or directory", path));
                }
                Err(e) => return ExecResult::failure(1, format!("rm: {}: {}", path, e)),
            };
            let real_path = ctx.backend.resolve_real_path(Path::new(&resolved));
            let file_size = entry.as_ref().map(|s| s.size);
            let is_dir = entry.as_ref().is_some_and(|s| s.is_dir());
            let is_symlink = entry.as_ref().is_some_and(|s| s.is_symlink());
            let action = decide_rm_action(
                trash_enabled,
                real_path.as_deref(),
                file_size,
                trash_max_size,
                is_dir,
                is_symlink,
            );
            decisions.push(Decision {
                path,
                resolved,
                action,
            });
        }

        if decisions.is_empty() {
            // All paths were missing under -f; nothing to do.
            return ExecResult::success("");
        }

        // Execute each decision. Continue past per-path errors so users see
        // every failure rather than just the first; final exit reflects the
        // last failure.
        let mut last_err: Option<String> = None;
        for d in &decisions {
            let result = match &d.action {
                RmAction::Trash(real) => {
                    let trash_backend = match ctx.trash_backend.as_ref() {
                        Some(tb) => tb,
                        None => {
                            last_err = Some("rm: trash backend not available".to_string());
                            continue;
                        }
                    };
                    trash_backend.trash(real).await.map_err(|e| {
                        format!(
                            "rm: {}: trash failed: {} (use `set +o trash` to delete permanently)",
                            real.display(), e
                        )
                    })
                }
                RmAction::Delete => {
                    // Single recursive remover lives on the backend (symlink-safe:
                    // it lstats the recurse decision and unlinks links directly).
                    match ctx.backend.remove(Path::new(&d.resolved), recursive).await {
                        Ok(()) => Ok(()),
                        // -f swallows a path that vanished between stat and remove.
                        Err(BackendError::NotFound(_)) if force => Ok(()),
                        Err(e) => Err(format!("rm: {}: {}", d.path, e)),
                    }
                }
            };
            if let Err(msg) = result {
                last_err = Some(msg);
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
        mem.write(Path::new("file.txt"), b"data").await.unwrap();
        mem.mkdir(Path::new("emptydir")).await.unwrap();
        mem.write(Path::new("fulldir/file.txt"), b"data").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_rm_file() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify deleted
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_rm_empty_dir() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/emptydir".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());

        assert!(!ctx.backend.exists(Path::new("/emptydir")).await);
    }

    #[tokio::test]
    async fn test_rm_non_empty_dir_fails() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/fulldir".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(!result.ok());
        // Directory should still exist
        assert!(ctx.backend.exists(Path::new("/fulldir")).await);
    }

    #[tokio::test]
    async fn test_rm_nonexistent() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_rm_no_arg() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Rm.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing"));
    }

    #[tokio::test]
    async fn test_rm_r_recursive() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/fulldir".into()));
        args.flags.insert("r".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());

        // Verify directory and contents removed
        assert!(!ctx.backend.exists(Path::new("/fulldir")).await);
        assert!(!ctx.backend.exists(Path::new("/fulldir/file.txt")).await);
    }

    #[tokio::test]
    async fn test_rm_recursive_flag() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/fulldir".into()));
        args.flags.insert("recursive".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.backend.exists(Path::new("/fulldir")).await);
    }

    #[tokio::test]
    async fn test_rm_f_force_nonexistent() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));
        args.flags.insert("f".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok()); // -f silences not-found errors
    }

    #[tokio::test]
    async fn test_rm_force_flag_nonexistent() {
        let mut ctx = make_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));
        args.flags.insert("force".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    async fn make_deep_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("deep/a/b/c/file.txt"), b"data").await.unwrap();
        mem.write(Path::new("deep/a/sibling.txt"), b"data").await.unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_rm_r_deeply_nested() {
        let mut ctx = make_deep_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/deep".into()));
        args.flags.insert("r".to_string());

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());

        assert!(!ctx.backend.exists(Path::new("/deep")).await);
        assert!(!ctx.backend.exists(Path::new("/deep/a")).await);
        assert!(!ctx.backend.exists(Path::new("/deep/a/b")).await);
    }

    // ── Decision-table tests (MemoryFs — no real filesystem) ──

    #[tokio::test]
    async fn force_on_a_missing_path_succeeds_silently() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));
        args.flags.insert("f".to_string());

        // -f on a nonexistent path succeeds silently — nothing to remove.
        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn a_missing_path_without_force_errors() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        assert!(result.err.contains("No such file"));
    }

    // ── Decision logic unit tests ──

    #[test]
    fn test_decide_rm_action_no_flags() {
        let action = decide_rm_action(false, None, Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_small_file() {
        let real = PathBuf::from("/home/user/file.txt");
        let action = decide_rm_action(true, Some(&real), Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_trash_large_falls_through() {
        let real = PathBuf::from("/home/user/bigfile.bin");
        let action = decide_rm_action(true, Some(&real), Some(100_000_000), 10_000_000, false, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_virtual_path() {
        // Virtual path (resolve_real_path returns None) → normal delete
        let action = decide_rm_action(true, None, Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_excluded_tmp() {
        let real = PathBuf::from("/tmp/scratch");
        let action = decide_rm_action(true, Some(&real), Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_real_v_path_is_trashed() {
        // A *real* path under /v (embedder content delegated by mount-coverage
        // routing) is NOT trash-excluded — it must be trashed like any real
        // file, not deleted outright. (In-memory kaish /v mounts stay `None`
        // and are handled by the no-real-path gating, not this predicate.)
        let real = PathBuf::from("/v/cas/blob.bin");
        let action = decide_rm_action(true, Some(&real), Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    // ── Directory-specific tests ──

    #[test]
    fn test_decide_rm_action_dir_always_trashes() {
        let real = PathBuf::from("/home/user/mydir");
        // Directory with size=0 (stat behavior) — should trash regardless of threshold
        let action = decide_rm_action(true, Some(&real), Some(0), 10_000_000, true, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_dir_trashes() {
        let real = PathBuf::from("/home/user/mydir");
        // Directory always trashes when trash enabled
        let action = decide_rm_action(true, Some(&real), Some(0), 10_000_000, true, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_dir_excluded_tmp() {
        let real = PathBuf::from("/tmp/mydir");
        // Excluded path — directory still gets excluded
        let action = decide_rm_action(true, Some(&real), Some(0), 10_000_000, true, false);
        assert_eq!(action, RmAction::Delete);
    }

}
