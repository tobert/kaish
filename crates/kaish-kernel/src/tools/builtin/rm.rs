//! rm — Remove files and directories.
//!
//! Supports confirmation latch (`set -o latch`) and trash-on-delete
//! (`set -o trash`) for safe autonomous operation.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::{Path, PathBuf};

use crate::backend::{BackendError, KernelBackend};
use crate::interpreter::ExecResult;
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// clap-derived argv layer for rm. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "rm", about = "Remove files and directories")]
struct RmArgs {
    /// Remove directories and their contents recursively.
    #[arg(short = 'r', long = "recursive")]
    recursive: bool,

    /// Alias for -r (uppercase, muscle memory).
    #[arg(short = 'R')]
    recursive_upper: bool,

    /// Ignore nonexistent files, never prompt.
    #[arg(short = 'f', long = "force")]
    force: bool,

    /// Confirmation nonce for latch-gated operations.
    #[arg(long = "confirm")]
    confirm: Option<String>,

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
    /// Latch gate: return exit code 2 with nonce.
    Latch,
}

/// Determine the rm action based on trash/latch settings and file properties.
fn decide_rm_action(
    trash_enabled: bool,
    latch_enabled: bool,
    real_path: Option<&Path>,
    file_size: Option<u64>,
    trash_max_size: u64,
    is_dir: bool,
) -> RmAction {
    if trash_enabled {
        if let Some(rp) = real_path {
            // Skip trash for excluded paths (/tmp, /v/*)
            // Use Path::starts_with (component-aware) not str starts_with,
            // otherwise "/tmp_file.txt" would incorrectly match "/tmp".
            let excluded = rp.starts_with("/tmp")
                || rp.starts_with("/v");

            if !excluded {
                // Directories always go to trash — stat size is unreliable
                // and trash::delete handles them atomically.
                if is_dir {
                    return RmAction::Trash(rp.to_path_buf());
                }
                let size = file_size.unwrap_or(0);
                if size <= trash_max_size {
                    return RmAction::Trash(rp.to_path_buf());
                }
                // File too big for trash — fall through to latch check
                if latch_enabled {
                    return RmAction::Latch;
                }
                return RmAction::Delete;
            }
        }
        // Virtual path (no real path) or excluded path — fall through
    }

    if latch_enabled {
        return RmAction::Latch;
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
                ("Confirm latched removal", "rm --confirm=a3f7b2c1 bigfile.bin"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        args.flagify_bool_named();

        let parsed = match RmArgs::try_parse_from(
            std::iter::once("rm".to_string()).chain(args.to_argv()),
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
        let confirm = parsed.confirm.clone();

        let trash_enabled = ctx.scope.trash_enabled();
        let latch_enabled = ctx.scope.latch_enabled();
        let trash_max_size = ctx.scope.trash_max_size();

        // Collect per-path decisions in one pass so latch can issue ONE nonce
        // that authorizes the whole batch (NonceScope.paths is a set; one
        // nonce validates any subset). Stat-failures short-circuit unless -f.
        struct Decision {
            path: String,
            resolved: PathBuf,
            action: RmAction,
        }
        let mut decisions: Vec<Decision> = Vec::with_capacity(args.positional.len());
        for value in &args.positional {
            let path = crate::interpreter::value_to_string(value);
            let resolved = ctx.resolve_path(&path);
            let stat = match ctx.backend.stat(Path::new(&resolved)).await {
                Ok(info) => Some(info),
                Err(BackendError::NotFound(_)) if force => continue, // -f skips missing
                Err(BackendError::NotFound(_)) => {
                    return ExecResult::failure(1, format!("rm: {}: No such file or directory", path));
                }
                Err(e) => return ExecResult::failure(1, format!("rm: {}: {}", path, e)),
            };
            let real_path = ctx.backend.resolve_real_path(Path::new(&resolved));
            let file_size = stat.as_ref().map(|s| s.size);
            let is_dir = stat.as_ref().is_some_and(|s| s.is_dir());
            let action = decide_rm_action(
                trash_enabled,
                latch_enabled,
                real_path.as_deref(),
                file_size,
                trash_max_size,
                is_dir,
            );
            decisions.push(Decision { path, resolved, action });
        }

        if decisions.is_empty() {
            // All paths were missing under -f; nothing to do.
            return ExecResult::success("");
        }

        // If ANY decision is Latch, issue one nonce for the full set of
        // latched paths so the user can re-run with `--confirm=NONCE` on the
        // same argv. Without a valid nonce, return the latch error listing
        // every path that triggered it.
        let latched_paths: Vec<&str> = decisions
            .iter()
            .filter(|d| matches!(d.action, RmAction::Latch))
            .map(|d| d.path.as_str())
            .collect();
        if !latched_paths.is_empty() {
            if let Some(nonce) = &confirm {
                if let Err(e) = ctx.verify_nonce(nonce, "rm", &latched_paths) {
                    return ExecResult::failure(1, format!("rm: {}", e));
                }
                // Nonce valid — fall through and execute each decision.
            } else {
                let joined = latched_paths.join(" ");
                return ctx.latch_result("rm", &latched_paths, "latch enabled", |nonce| {
                    format!("rm --confirm=\"{}\" {}", nonce, joined)
                });
            }
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
                RmAction::Latch | RmAction::Delete => {
                    remove_path(&*ctx.backend, &d.resolved, recursive, force)
                        .await
                        .map_err(|e| format!("rm: {}: {}", d.path, e))
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

/// Remove a path, optionally recursively.
async fn remove_path(backend: &dyn KernelBackend, path: &Path, recursive: bool, force: bool) -> Result<(), BackendError> {
    // Check if path exists
    match backend.stat(path).await {
        Ok(info) => {
            if info.is_dir() && recursive {
                // Remove contents first
                remove_dir_recursive(backend, path).await?;
            }
            backend.remove(path, false).await
        }
        Err(BackendError::NotFound(_)) if force => {
            // -f ignores nonexistent files
            Ok(())
        }
        Err(e) => Err(e),
    }
}

/// Recursively remove directory contents, then the directory itself.
fn remove_dir_recursive<'a>(
    backend: &'a dyn KernelBackend,
    dir: &'a Path,
) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), BackendError>> + Send + 'a>> {
    Box::pin(async move {
        let entries = backend.list(dir).await?;

        for entry in entries {
            let child_path: PathBuf = dir.join(&entry.name);
            if entry.is_dir() {
                // Recurse into subdirectory
                remove_dir_recursive(backend, &child_path).await?;
                backend.remove(&child_path, false).await?;
            } else {
                backend.remove(&child_path, false).await?;
            }
        }

        Ok(())
    })
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

    // ── Latch tests (MemoryFs — no real filesystem) ──

    #[tokio::test]
    async fn test_rm_latch_off_deletes_normally() {
        let mut ctx = make_ctx().await;
        // latch is off by default

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_rm_latch_on_no_confirm_returns_code_2() {
        let mut ctx = make_ctx().await;
        ctx.scope.set_latch_enabled(true);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 2);
        // Error message should contain a hex nonce pattern and authorized paths
        assert!(result.err.contains("confirmation required"));
        assert!(result.err.contains("Authorized: /file.txt"));
        assert!(result.err.contains("--confirm="));
        assert!(result.err.contains("60 seconds"));
        // Structured latch data should be present
        let data = result.data.as_ref().expect("latch result should have data");
        if let Value::Json(json) = data {
            assert!(json["nonce"].is_string());
            assert_eq!(json["command"], "rm");
            assert_eq!(json["paths"], serde_json::json!(["/file.txt"]));
            assert_eq!(json["ttl"], 60);
            assert!(json["hint"].as_str().unwrap().contains("--confirm="));
        } else {
            panic!("expected Value::Json, got {:?}", data);
        }
        // File should still exist
        assert!(ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_rm_latch_on_valid_confirm_deletes() {
        let mut ctx = make_ctx().await;
        ctx.scope.set_latch_enabled(true);

        // Issue a nonce manually
        let nonce = ctx.nonce_store.issue("rm", &["/file.txt"]);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named.insert("confirm".to_string(), Value::String(nonce));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_rm_latch_on_invalid_confirm_fails() {
        let mut ctx = make_ctx().await;
        ctx.scope.set_latch_enabled(true);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named.insert("confirm".to_string(), Value::String("bogus123".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        assert!(result.err.contains("invalid nonce"));
        assert!(ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_rm_latch_on_force_nonexistent() {
        let mut ctx = make_ctx().await;
        ctx.scope.set_latch_enabled(true);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));
        args.flags.insert("f".to_string());

        // -f on nonexistent should succeed silently (no latch — nothing to gate)
        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn test_rm_latch_on_nonexistent_no_force() {
        let mut ctx = make_ctx().await;
        ctx.scope.set_latch_enabled(true);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        // Should error with NotFound, NOT exit code 2
        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        assert!(result.err.contains("No such file"));
    }

    #[tokio::test]
    async fn test_rm_latch_nonce_reuse_idempotent() {
        let mut ctx = make_ctx().await;
        ctx.scope.set_latch_enabled(true);

        let nonce = ctx.nonce_store.issue("rm", &["/file.txt"]);

        // First confirm: deletes the file
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named.insert("confirm".to_string(), Value::String(nonce.clone()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);

        // Second confirm: file is gone, NotFound error (not exit code 2)
        let mut args2 = ToolArgs::new();
        args2.positional.push(Value::String("/file.txt".into()));
        args2.named.insert("confirm".to_string(), Value::String(nonce));

        let result2 = Rm.execute(args2, &mut ctx).await;
        assert_eq!(result2.code, 1);
    }

    #[tokio::test]
    async fn test_rm_latch_error_message_is_parseable() {
        let mut ctx = make_ctx().await;
        ctx.scope.set_latch_enabled(true);

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 2);

        // Parse the nonce from the error message
        let err = &result.err;
        assert!(err.contains("rm --confirm="));
        assert!(err.contains("/file.txt"));
        assert!(err.contains("60 seconds"));

        // Extract nonce: find '--confirm="' then take next 8 chars
        let confirm_prefix = "rm --confirm=\"";
        let idx = err.find(confirm_prefix).expect("should contain confirm prefix");
        let nonce_start = idx + confirm_prefix.len();
        let nonce: String = err[nonce_start..].chars().take(8).collect();
        assert_eq!(nonce.len(), 8);
        assert!(nonce.chars().all(|c| c.is_ascii_hexdigit()));
    }

    // ── Decision logic unit tests ──

    #[test]
    fn test_decide_rm_action_no_flags() {
        let action = decide_rm_action(false, false, None, Some(100), 10_000_000, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_latch_only() {
        let action = decide_rm_action(false, true, None, Some(100), 10_000_000, false);
        assert_eq!(action, RmAction::Latch);
    }

    #[test]
    fn test_decide_rm_action_trash_small_file() {
        let real = PathBuf::from("/home/user/file.txt");
        let action = decide_rm_action(true, false, Some(&real), Some(100), 10_000_000, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_trash_small_with_latch() {
        // Small file → trash catches it, latch irrelevant
        let real = PathBuf::from("/home/user/file.txt");
        let action = decide_rm_action(true, true, Some(&real), Some(100), 10_000_000, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_trash_large_no_latch() {
        let real = PathBuf::from("/home/user/bigfile.bin");
        let action = decide_rm_action(true, false, Some(&real), Some(100_000_000), 10_000_000, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_large_with_latch() {
        let real = PathBuf::from("/home/user/bigfile.bin");
        let action = decide_rm_action(true, true, Some(&real), Some(100_000_000), 10_000_000, false);
        assert_eq!(action, RmAction::Latch);
    }

    #[test]
    fn test_decide_rm_action_trash_virtual_path() {
        // Virtual path (resolve_real_path returns None) → normal delete
        let action = decide_rm_action(true, false, None, Some(100), 10_000_000, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_excluded_tmp() {
        let real = PathBuf::from("/tmp/scratch");
        let action = decide_rm_action(true, false, Some(&real), Some(100), 10_000_000, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_excluded_v() {
        let real = PathBuf::from("/v/jobs/something");
        let action = decide_rm_action(true, false, Some(&real), Some(100), 10_000_000, false);
        assert_eq!(action, RmAction::Delete);
    }

    // ── Directory-specific tests ──

    #[test]
    fn test_decide_rm_action_dir_always_trashes() {
        let real = PathBuf::from("/home/user/mydir");
        // Directory with size=0 (stat behavior) — should trash regardless of threshold
        let action = decide_rm_action(true, false, Some(&real), Some(0), 10_000_000, true);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_dir_trashes_with_latch() {
        let real = PathBuf::from("/home/user/mydir");
        // Directory always trashes when trash enabled — latch irrelevant
        let action = decide_rm_action(true, true, Some(&real), Some(0), 10_000_000, true);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_dir_excluded_tmp() {
        let real = PathBuf::from("/tmp/mydir");
        // Excluded path — directory still gets excluded
        let action = decide_rm_action(true, false, Some(&real), Some(0), 10_000_000, true);
        assert_eq!(action, RmAction::Delete);
    }

    // ── Composition matrix (rstest) ──

    #[derive(Debug, PartialEq)]
    enum Outcome {
        Deleted,
        Trashed,
        Latched,
    }

    fn matrix_action_to_outcome(action: &RmAction) -> Outcome {
        match action {
            RmAction::Trash(_) => Outcome::Trashed,
            RmAction::Delete => Outcome::Deleted,
            RmAction::Latch => Outcome::Latched,
        }
    }

    #[test]
    fn test_composition_matrix() {
        let real = PathBuf::from("/home/user/file.txt");
        let small = 100u64;
        let large = 100_000_000u64;
        let max = 10_000_000u64;

        // (trash, latch, size, is_dir) → expected outcome
        let cases = vec![
            (false, false, small, false, Outcome::Deleted),
            (false, true,  small, false, Outcome::Latched),
            (true,  false, small, false, Outcome::Trashed),
            (true,  true,  small, false, Outcome::Trashed),   // trash catches small, latch irrelevant
            (false, false, large, false, Outcome::Deleted),
            (false, true,  large, false, Outcome::Latched),
            (true,  false, large, false, Outcome::Deleted),    // too big for trash, no latch → delete
            (true,  true,  large, false, Outcome::Latched),    // too big for trash + latch → gate
            // Directories always trash (size irrelevant)
            (true,  false, 0,     true,  Outcome::Trashed),
            (true,  true,  0,     true,  Outcome::Trashed),
            // Dir without trash enabled → normal flow
            (false, false, 0,     true,  Outcome::Deleted),
            (false, true,  0,     true,  Outcome::Latched),
        ];

        for (trash, latch, size, is_dir, expected) in cases {
            let action = decide_rm_action(trash, latch, Some(&real), Some(size), max, is_dir);
            let outcome = matrix_action_to_outcome(&action);
            assert_eq!(
                outcome, expected,
                "trash={}, latch={}, size={}, is_dir={}: expected {:?}, got {:?}",
                trash, latch, size, is_dir, expected, outcome
            );
        }
    }
}
