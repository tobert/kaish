//! kaish-vfs — Inspect and manage the active overlay VFS transaction.
//!
//! Subcommands: status, diff, commit, reset.
//!
//! `status` always works: it reports `mode: transaction` inside an overlay
//! session and `mode: direct` otherwise, so an agent can ask "what session am
//! I in?" without an error path. The mutating/inspecting subcommands (diff,
//! commit, reset) are meaningless without a transaction and exit 1 with a
//! clear message when no overlay is active (`--overlay` /
//! `KernelConfig::with_overlay(true)`). In no-default-features (minimal/wasm)
//! builds, the `commit` subcommand additionally fails at runtime because
//! `LocalFs` is absent — other subcommands still work against the in-memory
//! overlay.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// kaish-vfs tool: inspect and manage the overlay VFS transaction.
pub struct KaishVfs;

/// clap-derived argv layer for kaish-vfs.
#[derive(Parser, Debug)]
#[command(name = "kaish-vfs", about = "Inspect and manage the overlay VFS transaction")]
struct KaishVfsArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Subcommand (status, diff, commit, reset) and its arguments.
    args: Vec<String>,
}

/// No-overlay error message. Surface-neutral: works for REPL --overlay and
/// KernelConfig::with_overlay(true) (embedder API).
const NO_OVERLAY_MSG: &str =
    "no overlay active; start the session with --overlay (REPL/MCP) \
     or KernelConfig::with_overlay(true) (embedder API)";

#[async_trait]
impl Tool for KaishVfs {
    fn name(&self) -> &str {
        "kaish-vfs"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &KaishVfsArgs::command(),
            "kaish-vfs",
            "Inspect and manage the overlay VFS transaction (requires --overlay)",
            [
                ("Show session mode and overlay status (works in any session)", "kaish-vfs status"),
                ("Show unified diff of all changes", "kaish-vfs diff"),
                ("Show diff for a specific path", "kaish-vfs diff src/main.rs"),
                ("Commit all changes to real files", "kaish-vfs commit"),
                ("Discard all virtual edits", "kaish-vfs reset"),
                ("Discard edits to one path", "kaish-vfs reset src/main.rs"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kaish-vfs: {e}")),
        };
        let parsed = match KaishVfsArgs::try_parse_from(
            std::iter::once("kaish-vfs".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kaish-vfs: {e}")),
        };
        parsed.global.apply(ctx);

        let subcmd = match args.get_string("", 0) {
            Some(s) => s,
            None => return ExecResult::failure(1, format!("kaish-vfs: missing subcommand (status, diff, commit, reset)\n{}", NO_OVERLAY_MSG)),
        };

        match subcmd.as_str() {
            "status" => cmd_status(ctx).await,
            "diff" => {
                // Remaining positional args after "diff" are path filters
                let path_args: Vec<String> = args.positional.iter().skip(1)
                    .filter_map(|v| match v {
                        crate::ast::Value::String(s) => Some(s.clone()),
                        _ => None,
                    })
                    .collect();
                cmd_diff(&path_args, ctx).await
            }
            "commit" => cmd_commit(ctx).await,
            "reset" => {
                // Error loudly on extra positionals — reset takes at most one path.
                // (diff accepts multiple path filters, but reset's semantics are
                // per-path; silently ignoring extra args would violate fail-loud.)
                let extra: Vec<String> = args.positional.iter().skip(2)
                    .filter_map(|v| match v {
                        crate::ast::Value::String(s) => Some(s.clone()),
                        _ => None,
                    })
                    .collect();
                if !extra.is_empty() {
                    return ExecResult::failure(1, format!(
                        "kaish-vfs reset: too many arguments ({}); \
                         reset takes at most one path — run reset multiple times \
                         or omit the path to reset everything",
                        extra.join(", ")
                    ));
                }
                let path_arg = args.get_string("", 1);
                cmd_reset(path_arg.as_deref(), ctx).await
            }
            other => ExecResult::failure(1, format!(
                "kaish-vfs: unknown subcommand '{}' (try: status, diff, commit, reset)",
                other
            )),
        }
    }
}

// ── Subcommand implementations ──────────────────────────────────────────────

/// Status for a session with no overlay: not an error — `mode: direct` is a
/// valid answer to "what session am I in?". The budget row still applies
/// (kernels can be budgeted without an overlay).
fn direct_mode_status(ctx: &ExecContext) -> ExecResult {
    let budget_str = match &ctx.vfs_budget {
        Some(b) => format!(
            "{} used / {} limit",
            format_bytes(b.used()),
            format_bytes(b.limit())
        ),
        None => "unlimited".to_string(),
    };
    let headers = vec!["KEY".to_string(), "VALUE".to_string()];
    let rows = vec![
        OutputNode::new("mode").with_cells(vec!["direct".to_string()]),
        OutputNode::new("budget").with_cells(vec![budget_str]),
    ];
    ExecResult::with_output(OutputData::table(headers, rows))
}

#[cfg(all(feature = "localfs", feature = "overlay"))]
async fn cmd_status(ctx: &ExecContext) -> ExecResult {
    use kaish_vfs::Filesystem as _;  // for resident_bytes()
    let handle = match &ctx.overlay_handle {
        Some(h) => h.clone(),
        None => return direct_mode_status(ctx),
    };

    let is_dirty = handle.fs.is_dirty().await;
    let changes_result = handle.fs.changes().await;

    let (added, modified, removed) = match &changes_result {
        Ok(changes) => {
            let added = changes.iter().filter(|c| c.kind == kaish_vfs::ChangeKind::Added).count();
            let modified = changes.iter().filter(|c| c.kind == kaish_vfs::ChangeKind::Modified).count();
            let removed = changes.iter().filter(|c| c.kind == kaish_vfs::ChangeKind::Removed).count();
            (added, modified, removed)
        }
        Err(e) => {
            // Dirty symlinks or similar — report as error
            return ExecResult::failure(1, format!("kaish-vfs status: {}", e));
        }
    };

    let resident = handle.fs.resident_bytes();
    let resident_str = match resident {
        Some(b) => format_bytes(b),
        None => "-".to_string(),
    };

    let budget_str = match &ctx.vfs_budget {
        Some(b) => format!("{} used / {} limit", format_bytes(b.used()), format_bytes(b.limit())),
        None => "unlimited".to_string(),
    };

    let headers = vec!["KEY".to_string(), "VALUE".to_string()];
    let rows = vec![
        OutputNode::new("mode").with_cells(vec!["transaction".to_string()]),
        OutputNode::new("dirty").with_cells(vec![if is_dirty { "yes" } else { "no" }.to_string()]),
        OutputNode::new("added").with_cells(vec![added.to_string()]),
        OutputNode::new("modified").with_cells(vec![modified.to_string()]),
        OutputNode::new("removed").with_cells(vec![removed.to_string()]),
        OutputNode::new("overlay-mount").with_cells(vec![handle.mount_path.to_string_lossy().to_string()]),
        OutputNode::new("overlay-resident").with_cells(vec![resident_str]),
        OutputNode::new("budget").with_cells(vec![budget_str]),
    ];

    ExecResult::with_output(OutputData::table(headers, rows))
}

#[cfg(not(all(feature = "localfs", feature = "overlay")))]
async fn cmd_status(ctx: &ExecContext) -> ExecResult {
    // No overlay support in this build: every session is direct mode.
    direct_mode_status(ctx)
}

#[cfg(all(feature = "localfs", feature = "overlay"))]
async fn cmd_diff(path_args: &[String], ctx: &ExecContext) -> ExecResult {
    let handle = match &ctx.overlay_handle {
        Some(h) => h.clone(),
        None => return ExecResult::failure(1, NO_OVERLAY_MSG),
    };

    let colorize = false; // opt-in only via --color flag (matching diff.rs behavior)

    let changes = match handle.fs.changes().await {
        Ok(c) => c,
        Err(e) => return ExecResult::failure(1, format!("kaish-vfs diff: {}", e)),
    };

    // Filter by path args if provided.
    // Relative filters are resolved against ctx.cwd (same logic as cmd_reset),
    // so `kaish-vfs diff src/main.rs` works from any cwd inside the mount.
    let changes: Vec<_> = if path_args.is_empty() {
        changes
    } else {
        let filter_paths: Vec<std::path::PathBuf> = path_args.iter().map(|arg| {
            if arg.starts_with('/') {
                std::path::PathBuf::from(arg)
            } else {
                ctx.cwd.join(arg)
            }
        }).collect();
        changes.into_iter().filter(|change| {
            let vfs_path = handle.mount_path.join(&change.path);
            filter_paths.iter().any(|filter_path| {
                vfs_path == *filter_path || vfs_path.starts_with(filter_path)
            })
        }).collect()
    };

    // Exit 0 when clean (POSIX diff convention: 0 = no changes, 1 = changes).
    if changes.is_empty() {
        return ExecResult::success("no changes");
    }

    // Build unified-diff text output and --json structured array in one pass.
    let mut text_output = String::new();
    let mut json_entries: Vec<serde_json::Value> = Vec::with_capacity(changes.len());

    for change in &changes {
        // VFS-absolute display paths (e.g. /home/user/src/main.rs)
        let vfs_path = handle.mount_path.join(&change.path);
        let display = vfs_path.to_string_lossy();
        let path_str = display.to_string();

        match change.kind {
            kaish_vfs::ChangeKind::Added => {
                let current_bytes = change.current.as_deref().unwrap_or(b"");
                let base_bytes: &[u8] = b"";
                if let Ok(current_str) = std::str::from_utf8(current_bytes) {
                    // similar's unified_diff().header() emits the ---/+++ lines;
                    // do not add a manual header block or it will appear twice.
                    let diff = similar::TextDiff::from_lines("", current_str);
                    let plain = diff.unified_diff()
                        .context_radius(3)
                        .header("/dev/null", &format!("b/{}", display))
                        .to_string();
                    let block = if colorize { colorize_diff(&plain) } else { plain };
                    text_output.push_str(&block);
                } else {
                    text_output.push_str(&format!("binary file added: {}\n", display));
                }
                json_entries.push(serde_json::json!({
                    "path": path_str,
                    "kind": "added",
                    "base_bytes": base_bytes.len(),
                    "current_bytes": current_bytes.len(),
                }));
            }
            kaish_vfs::ChangeKind::Removed => {
                let base_bytes = change.base.as_deref().unwrap_or(b"");
                let current_bytes: &[u8] = b"";
                if let Ok(base_str) = std::str::from_utf8(base_bytes) {
                    let diff = similar::TextDiff::from_lines(base_str, "");
                    let plain = diff.unified_diff()
                        .context_radius(3)
                        .header(&format!("a/{}", display), "/dev/null")
                        .to_string();
                    let block = if colorize { colorize_diff(&plain) } else { plain };
                    text_output.push_str(&block);
                } else {
                    text_output.push_str(&format!("binary file removed: {}\n", display));
                }
                json_entries.push(serde_json::json!({
                    "path": path_str,
                    "kind": "removed",
                    "base_bytes": base_bytes.len(),
                    "current_bytes": current_bytes.len(),
                }));
            }
            kaish_vfs::ChangeKind::Modified => {
                let base_bytes = change.base.as_deref().unwrap_or(b"");
                let current_bytes = change.current.as_deref().unwrap_or(b"");
                let base_utf8 = std::str::from_utf8(base_bytes);
                let current_utf8 = std::str::from_utf8(current_bytes);
                match (base_utf8, current_utf8) {
                    (Ok(base_str), Ok(current_str)) => {
                        let diff = similar::TextDiff::from_lines(base_str, current_str);
                        let plain = diff.unified_diff()
                            .context_radius(3)
                            .header(&format!("a/{}", display), &format!("b/{}", display))
                            .to_string();
                        let block = if colorize { colorize_diff(&plain) } else { plain };
                        text_output.push_str(&block);
                    }
                    _ => {
                        text_output.push_str(&format!("binary file changed: {}\n", display));
                    }
                }
                json_entries.push(serde_json::json!({
                    "path": path_str,
                    "kind": "modified",
                    "base_bytes": base_bytes.len(),
                    "current_bytes": current_bytes.len(),
                }));
            }
        }
    }

    // --json: structured array of {path, kind, base_bytes, current_bytes}.
    // rich_json overrides the default text serialization when --json is active,
    // without raw file content (sizes only — callers can do a targeted read).
    let output = OutputData::text(text_output.clone())
        .with_rich_json(serde_json::Value::Array(json_entries));

    // Diff exits 1 when there are changes (POSIX convention, matching diff.rs).
    let mut result = ExecResult::from_output(1, text_output, String::new());
    result.set_output(Some(output));
    result
}

#[cfg(not(all(feature = "localfs", feature = "overlay")))]
async fn cmd_diff(_path_args: &[String], ctx: &ExecContext) -> ExecResult {
    let _ = ctx;
    ExecResult::failure(1, NO_OVERLAY_MSG)
}

#[cfg(all(feature = "localfs", feature = "overlay"))]
async fn cmd_commit(ctx: &ExecContext) -> ExecResult {
    use crate::vfs::LocalFs;

    let handle = match &ctx.overlay_handle {
        Some(h) => h.clone(),
        None => return ExecResult::failure(1, NO_OVERLAY_MSG),
    };

    let target = LocalFs::new(handle.commit_root.clone());

    // Get the change list before committing for the success report.
    let changes = match handle.fs.changes().await {
        Ok(c) => c,
        Err(e) => return ExecResult::failure(1, format!("kaish-vfs commit: pre-flight: {}", e)),
    };

    if changes.is_empty() {
        return ExecResult::success("nothing to commit (overlay is clean)");
    }

    // Commit: pre-flight all base checks, then write.
    if let Err(e) = handle.fs.commit_into(&target).await {
        // commit_into lists any already-committed paths in the error string.
        // Give the caller actionable recovery steps so they know which files
        // to clean up and how to resume work.
        return ExecResult::failure(1, format!(
            "kaish-vfs commit: {}\n\
             \n\
             Recovery:\n\
             - Any paths listed above as already committed are now in the real tree.\n\
             - Run `kaish-vfs reset <path>` for each committed path to discard their\n\
               overlay entries (they are already on disk — resetting is safe).\n\
             - Run `kaish-vfs status` to see what remains dirty.\n\
             - Resolve any conflicts in the real tree, then retry `kaish-vfs commit`.",
            e
        ));
    }

    // On success, reset the overlay so the transaction is clean.
    if let Err(e) = handle.fs.reset_all().await {
        return ExecResult::failure(1, format!(
            "kaish-vfs commit: committed successfully but reset_all failed: {}; \
             overlay may still show dirty state but real files were written.",
            e
        ));
    }

    // Report what was committed.
    let (added, modified, removed) = changes.iter().fold((0usize, 0usize, 0usize), |(a, m, r), c| {
        match c.kind {
            kaish_vfs::ChangeKind::Added => (a + 1, m, r),
            kaish_vfs::ChangeKind::Modified => (a, m + 1, r),
            kaish_vfs::ChangeKind::Removed => (a, m, r + 1),
        }
    });

    let msg = format!("committed {} added, {} modified, {} removed", added, modified, removed);
    ExecResult::success(msg)
}

#[cfg(not(all(feature = "localfs", feature = "overlay")))]
async fn cmd_commit(ctx: &ExecContext) -> ExecResult {
    let _ = ctx;
    ExecResult::failure(1, "kaish-vfs commit: not available in minimal builds (no localfs feature)")
}

#[cfg(all(feature = "localfs", feature = "overlay"))]
async fn cmd_reset(path_arg: Option<&str>, ctx: &ExecContext) -> ExecResult {
    let handle = match &ctx.overlay_handle {
        Some(h) => h.clone(),
        None => return ExecResult::failure(1, NO_OVERLAY_MSG),
    };

    if let Some(path_str) = path_arg {
        // Reset a single path. Resolve relative to overlay mount.
        let abs_path = if path_str.starts_with('/') {
            std::path::PathBuf::from(path_str)
        } else {
            ctx.cwd.join(path_str)
        };
        // Convert from VFS-absolute to overlay-relative
        let rel = match abs_path.strip_prefix(&handle.mount_path) {
            Ok(r) => r.to_path_buf(),
            Err(_) => {
                return ExecResult::failure(1, format!(
                    "kaish-vfs reset: {} is outside the overlay mount ({})",
                    path_str, handle.mount_path.display()
                ));
            }
        };
        if let Err(e) = handle.fs.reset(&rel).await {
            return ExecResult::failure(1, format!("kaish-vfs reset: {}", e));
        }
        ExecResult::success(format!("reset {}", path_str))
    } else {
        // Reset all
        let changes = handle.fs.changes().await.ok();
        let count = changes.as_ref().map(|c| c.len()).unwrap_or(0);
        if let Err(e) = handle.fs.reset_all().await {
            return ExecResult::failure(1, format!("kaish-vfs reset: {}", e));
        }
        ExecResult::success(format!("discarded {} virtual edit(s)", count))
    }
}

#[cfg(not(all(feature = "localfs", feature = "overlay")))]
async fn cmd_reset(_path_arg: Option<&str>, ctx: &ExecContext) -> ExecResult {
    let _ = ctx;
    ExecResult::failure(1, NO_OVERLAY_MSG)
}

// ── Helpers ──────────────────────────────────────────────────────────────────

/// Format a byte count in a human-readable form.
fn format_bytes(bytes: u64) -> String {
    const UNITS: &[&str] = &["B", "KiB", "MiB", "GiB", "TiB"];
    if bytes == 0 {
        return "0 B".to_string();
    }
    let mut size = bytes as f64;
    let mut idx = 0;
    while size >= 1024.0 && idx < UNITS.len() - 1 {
        size /= 1024.0;
        idx += 1;
    }
    if idx == 0 {
        format!("{} {}", bytes, UNITS[idx])
    } else if size >= 10.0 {
        format!("{:.0} {}", size, UNITS[idx])
    } else {
        format!("{:.1} {}", size, UNITS[idx])
    }
}

/// Apply ANSI colors to a pre-formatted unified diff.
/// Same logic as tools/builtin/diff.rs colorize_unified_output.
#[cfg(all(feature = "localfs", feature = "overlay"))]
fn colorize_diff(plain: &str) -> String {
    let mut output = String::with_capacity(plain.len() + 256);
    for line in plain.lines() {
        if line.starts_with("---") || line.starts_with("+++") {
            output.push_str("\x1b[1m");
            output.push_str(line);
            output.push_str("\x1b[0m");
        } else if line.starts_with("@@") {
            output.push_str("\x1b[36m");
            output.push_str(line);
            output.push_str("\x1b[0m");
        } else if line.starts_with('-') {
            output.push_str("\x1b[31m");
            output.push_str(line);
            output.push_str("\x1b[0m");
        } else if line.starts_with('+') {
            output.push_str("\x1b[32m");
            output.push_str(line);
            output.push_str("\x1b[0m");
        } else {
            output.push_str(line);
        }
        output.push('\n');
    }
    output
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx_no_overlay() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    fn args_with_subcmd(subcmd: &str) -> ToolArgs {
        let mut args = ToolArgs::new();
        args.positional.push(Value::String(subcmd.to_string()));
        args
    }

    #[tokio::test]
    async fn test_status_without_overlay_reports_direct_mode() {
        let mut ctx = make_ctx_no_overlay();
        let result = KaishVfs.execute(args_with_subcmd("status"), &mut ctx).await;
        assert!(
            result.ok(),
            "status is introspection — it answers in any session: err='{}'",
            result.err
        );
        let out = result.text_out();
        assert!(
            out.lines().any(|l| l == "mode\tdirect"),
            "expected mode=direct row, got: {}",
            out
        );
        assert!(
            out.lines().any(|l| l == "budget\tunlimited"),
            "expected budget row, got: {}",
            out
        );
    }

    #[tokio::test]
    async fn test_diff_without_overlay_returns_error() {
        let mut ctx = make_ctx_no_overlay();
        let result = KaishVfs.execute(args_with_subcmd("diff"), &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_commit_without_overlay_returns_error() {
        let mut ctx = make_ctx_no_overlay();
        let result = KaishVfs.execute(args_with_subcmd("commit"), &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_reset_without_overlay_returns_error() {
        let mut ctx = make_ctx_no_overlay();
        let result = KaishVfs.execute(args_with_subcmd("reset"), &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_unknown_subcommand() {
        let mut ctx = make_ctx_no_overlay();
        let result = KaishVfs.execute(args_with_subcmd("bogus"), &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("unknown subcommand"));
    }

    #[test]
    fn test_format_bytes() {
        assert_eq!(format_bytes(0), "0 B");
        assert_eq!(format_bytes(512), "512 B");
        assert_eq!(format_bytes(1024), "1.0 KiB");
        assert_eq!(format_bytes(1024 * 1024), "1.0 MiB");
        assert_eq!(format_bytes(64 * 1024 * 1024), "64 MiB");
    }
}
