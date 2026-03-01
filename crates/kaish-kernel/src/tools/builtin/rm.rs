//! rm — Remove files and directories.
//!
//! Supports confirmation latch (`set -o latch`) and trash-on-delete
//! (`set -o trash`) for safe autonomous operation.

use async_trait::async_trait;
use std::path::{Path, PathBuf};

use crate::ast::Value;
use crate::backend::{BackendError, KernelBackend};
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, Tool, ToolArgs, ToolSchema, ParamSchema};

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
) -> RmAction {
    if trash_enabled {
        if let Some(rp) = real_path {
            // Skip trash for excluded paths (/tmp, /v/*)
            let path_str = rp.to_string_lossy();
            let excluded = path_str.starts_with("/tmp")
                || path_str.starts_with("/v/");

            if !excluded {
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
        ToolSchema::new("rm", "Remove files and directories")
            .param(ParamSchema::required("path", "string", "Path to remove"))
            .param(ParamSchema::optional(
                "recursive",
                "bool",
                Value::Bool(false),
                "Remove directories and their contents recursively (-r)",
            ).with_aliases(["-r", "-R"]))
            .param(ParamSchema::optional(
                "force",
                "bool",
                Value::Bool(false),
                "Ignore nonexistent files, never prompt (-f)",
            ).with_aliases(["-f"]))
            .param(ParamSchema::optional(
                "confirm",
                "string",
                Value::Null,
                "Confirmation nonce for latch-gated operations (--confirm=NONCE)",
            ))
            .example("Remove a file", "rm temp.txt")
            .example("Remove directory recursively", "rm -rf build/")
            .example("Confirm latched removal", "rm --confirm=a3f7b2c1 bigfile.bin")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let path = match args.get_string("path", 0) {
            Some(p) => p,
            None => return ExecResult::failure(1, "rm: missing path argument"),
        };

        let recursive = args.has_flag("recursive") || args.has_flag("r");
        let force = args.has_flag("force") || args.has_flag("f");
        let confirm = args.get_named("confirm").and_then(|v| match v {
            Value::String(s) => Some(s.clone()),
            _ => None,
        });
        let resolved = ctx.resolve_path(&path);

        // Check existence first
        let stat = match ctx.backend.stat(Path::new(&resolved)).await {
            Ok(info) => Some(info),
            Err(BackendError::NotFound(_)) if force => return ExecResult::with_output(OutputData::text("")),
            Err(BackendError::NotFound(_)) => return ExecResult::failure(1, format!("rm: {}: No such file or directory", path)),
            Err(e) => return ExecResult::failure(1, format!("rm: {}: {}", path, e)),
        };

        let trash_enabled = ctx.scope.trash_enabled();
        let latch_enabled = ctx.scope.latch_enabled();
        let trash_max_size = ctx.scope.trash_max_size();

        // Resolve real filesystem path (None for virtual paths like /v/*)
        let real_path = ctx.backend.resolve_real_path(Path::new(&resolved));
        let file_size = stat.as_ref().map(|s| s.size);

        let action = decide_rm_action(
            trash_enabled,
            latch_enabled,
            real_path.as_deref(),
            file_size,
            trash_max_size,
        );

        match action {
            RmAction::Trash(real) => {
                // Attempt to trash the file
                let real_display = real.display().to_string();
                let trash_result = tokio::task::spawn_blocking(move || trash::delete(real)).await;
                match trash_result {
                    Ok(Ok(())) => return ExecResult::with_output(OutputData::text("")),
                    Ok(Err(e)) => {
                        tracing::warn!("trash::delete failed for {}: {}, falling back to permanent delete", real_display, e);
                    }
                    Err(e) => {
                        tracing::warn!("trash::delete task failed for {}: {}, falling back to permanent delete", real_display, e);
                    }
                }
                // Fall through to normal delete
                match remove_path(&*ctx.backend, Path::new(&resolved), recursive, force).await {
                    Ok(()) => ExecResult::with_output(OutputData::text("")),
                    Err(e) => ExecResult::failure(1, format!("rm: {}: {}", path, e)),
                }
            }
            RmAction::Latch => {
                // Check if a valid confirmation nonce was provided
                if let Some(nonce) = &confirm {
                    match ctx.nonce_store.validate(nonce) {
                        Ok(_desc) => {
                            // Nonce valid — proceed with delete
                            match remove_path(&*ctx.backend, Path::new(&resolved), recursive, force).await {
                                Ok(()) => ExecResult::with_output(OutputData::text("")),
                                Err(e) => ExecResult::failure(1, format!("rm: {}: {}", path, e)),
                            }
                        }
                        Err(e) => ExecResult::failure(1, format!("rm: {}: {}", path, e)),
                    }
                } else {
                    // Issue a nonce and return exit code 2
                    let nonce = ctx.nonce_store.issue(format!("rm {}", path));
                    let ttl = ctx.nonce_store.ttl().as_secs();
                    let msg = format!(
                        "rm: {}: confirmation required (latch enabled)\nTo confirm, run: rm --confirm={} {}\nNonce expires in {} seconds.",
                        path, nonce, path, ttl
                    );
                    ExecResult::failure(2, msg)
                }
            }
            RmAction::Delete => {
                match remove_path(&*ctx.backend, Path::new(&resolved), recursive, force).await {
                    Ok(()) => ExecResult::with_output(OutputData::text("")),
                    Err(e) => ExecResult::failure(1, format!("rm: {}: {}", path, e)),
                }
            }
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
        // Error message should contain a hex nonce pattern
        assert!(result.err.contains("confirmation required"));
        assert!(result.err.contains("--confirm="));
        assert!(result.err.contains("60 seconds"));
        // File should still exist
        assert!(ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn test_rm_latch_on_valid_confirm_deletes() {
        let mut ctx = make_ctx().await;
        ctx.scope.set_latch_enabled(true);

        // Issue a nonce manually
        let nonce = ctx.nonce_store.issue("rm /file.txt");

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

        let nonce = ctx.nonce_store.issue("rm /file.txt");

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

        // Extract nonce: find "--confirm=" then take next 8 chars
        let confirm_prefix = "rm --confirm=";
        let idx = err.find(confirm_prefix).expect("should contain confirm prefix");
        let nonce_start = idx + confirm_prefix.len();
        let nonce: String = err[nonce_start..].chars().take(8).collect();
        assert_eq!(nonce.len(), 8);
        assert!(nonce.chars().all(|c| c.is_ascii_hexdigit()));
    }

    // ── Decision logic unit tests ──

    #[test]
    fn test_decide_rm_action_no_flags() {
        let action = decide_rm_action(false, false, None, Some(100), 10_000_000);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_latch_only() {
        let action = decide_rm_action(false, true, None, Some(100), 10_000_000);
        assert_eq!(action, RmAction::Latch);
    }

    #[test]
    fn test_decide_rm_action_trash_small_file() {
        let real = PathBuf::from("/home/user/file.txt");
        let action = decide_rm_action(true, false, Some(&real), Some(100), 10_000_000);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_trash_small_with_latch() {
        // Small file → trash catches it, latch irrelevant
        let real = PathBuf::from("/home/user/file.txt");
        let action = decide_rm_action(true, true, Some(&real), Some(100), 10_000_000);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_trash_large_no_latch() {
        let real = PathBuf::from("/home/user/bigfile.bin");
        let action = decide_rm_action(true, false, Some(&real), Some(100_000_000), 10_000_000);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_large_with_latch() {
        let real = PathBuf::from("/home/user/bigfile.bin");
        let action = decide_rm_action(true, true, Some(&real), Some(100_000_000), 10_000_000);
        assert_eq!(action, RmAction::Latch);
    }

    #[test]
    fn test_decide_rm_action_trash_virtual_path() {
        // Virtual path (resolve_real_path returns None) → normal delete
        let action = decide_rm_action(true, false, None, Some(100), 10_000_000);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_excluded_tmp() {
        let real = PathBuf::from("/tmp/scratch");
        let action = decide_rm_action(true, false, Some(&real), Some(100), 10_000_000);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_excluded_v() {
        let real = PathBuf::from("/v/jobs/something");
        let action = decide_rm_action(true, false, Some(&real), Some(100), 10_000_000);
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

        // (trash, latch, size) → expected outcome
        let cases = vec![
            (false, false, small, Outcome::Deleted),
            (false, true,  small, Outcome::Latched),
            (true,  false, small, Outcome::Trashed),
            (true,  true,  small, Outcome::Trashed),   // trash catches small, latch irrelevant
            (false, false, large, Outcome::Deleted),
            (false, true,  large, Outcome::Latched),
            (true,  false, large, Outcome::Deleted),    // too big for trash, no latch → delete
            (true,  true,  large, Outcome::Latched),    // too big for trash + latch → gate
        ];

        for (trash, latch, size, expected) in cases {
            let action = decide_rm_action(trash, latch, Some(&real), Some(size), max);
            let outcome = matrix_action_to_outcome(&action);
            assert_eq!(
                outcome, expected,
                "trash={}, latch={}, size={}: expected {:?}, got {:?}",
                trash, latch, size, expected, outcome
            );
        }
    }
}
