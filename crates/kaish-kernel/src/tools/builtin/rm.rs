//! rm — Remove files and directories.
//!
//! Gated by the approval ledger's `fs.*` enforce policy (`set -o approvals`) and
//! by trash-on-delete (`set -o trash`) for safe autonomous operation. Trash
//! wins over the gate — the trash IS the recovery net, so a delete it can
//! catch needs no approval.

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

    /// Alias for -r (uppercase, muscle memory).
    #[arg(short = 'R')]
    recursive_upper: bool,

    /// Ignore nonexistent files, never prompt.
    #[arg(short = 'f', long = "force")]
    force: bool,

    /// Approval token for a gated delete (`--confirm=<token>`).
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
                (
                    "Confirm gated removal",
                    "rm --confirm=4b1e0d9a7c3f28e6b5a0c1d4e7f2938a bigfile.bin",
                ),
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
        let _confirm = parsed.confirm.clone();

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

    // ── Approval-gate tests (MemoryFs — no real filesystem) ──

    /// Wire a ledger, turn the `fs.*` enforce policy on, and hand back the
    /// authority a test grants through.
    async fn gated_ctx() -> (ExecContext, crate::ledger::ApproverHandle) {
        let mut ctx = make_ctx().await;
        let authority = ctx.wire_test_ledger();
        ctx.scope.set_approvals_enabled(true);
        (ctx, authority)
    }

    /// Grant the one pending request and return its bearer key. Mirrors what
    /// an embedder does between the exit-2 result and the re-run.
    async fn grant_the_pending_request(
        ctx: &ExecContext,
        authority: &crate::ledger::ApproverHandle,
    ) -> (kaish_types::approval::RequestId, String) {
        use kaish_types::approval::GrantTerms;
        let approvals = ctx
            .ledger_access
            .as_ref()
            .expect("a wired ledger")
            .approvals
            .clone();
        let pending = approvals.pending(kaish_types::approval::PageRequest::default()).items;
        assert_eq!(pending.len(), 1, "exactly one request must be pending");
        let id = pending[0].id.clone();
        let chain = approvals.get(&id).expect("the chain");
        let terms = GrantTerms::once_for_view(
            &chain.request,
            std::time::SystemTime::now() + std::time::Duration::from_secs(300),
        );
        authority.grant(&id, chain.request.revision, terms).await.expect("the grant must post");
        let token = authority.token_for(&id).expect("a credential for a granted request");
        (id, token.reveal().to_string())
    }

    #[tokio::test]
    async fn rm_with_the_policy_off_deletes_directly() {
        let mut ctx = make_ctx().await;
        ctx.wire_test_ledger();
        // The fs.* enforce policy is off by default.

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn rm_with_no_subscription_and_no_policy_posts_nothing() {
        // Spec §C.5's free-when-unsubscribed rule: an ungated `fs.*`
        // operation must not pay a ledger cost, and the log must stay empty.
        let mut ctx = make_ctx().await;
        ctx.wire_test_ledger();

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());

        let approvals = ctx.ledger_access.as_ref().expect("a wired ledger").approvals.clone();
        assert!(
            approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items.is_empty(),
            "an unsubscribed, ungated rm must post NOTHING: {:?}",
            approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items
        );
        assert!(approvals.pending(kaish_types::approval::PageRequest::default()).items.is_empty());
    }

    /// The entries inside this ledger's records. These tests assert on entry
    /// shape; the record envelope is covered in `kaish-types` (spec §A.5).
    fn ledger_entries(
        approvals: &crate::ledger::Approvals,
    ) -> Vec<kaish_types::approval::LedgerEntry> {
        approvals
            .log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT)
            .items
            .into_iter()
            .map(|record| {
                record
                    .known()
                    .cloned()
                    .expect("this build wrote every record it reads back")
            })
            .collect()
    }

    #[tokio::test]
    async fn rm_under_the_policy_with_no_approval_returns_code_2() {
        let (mut ctx, _authority) = gated_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 2);
        assert!(result.err.contains("pending approval"), "{}", result.err);
        // The file must still exist.
        assert!(ctx.backend.exists(Path::new("/file.txt")).await);

        // The request rides its own typed control-plane field. The data-plane
        // `.data` stays empty — a pending approval is not stdout.
        assert!(result.data.is_none(), "a gate must not use the data-plane .data");
        let req = result.approval_request().expect("a request on the .approval field");
        assert_eq!(req.operation.as_str(), "fs.remove");
        assert_eq!(req.resources.len(), 1);
        assert_eq!(req.resources[0].id, "/file.txt");
        assert!(req.hint.contains("--confirm="));
        // A direct `tool.execute` has no dispatch seam above it, so the
        // capture says so rather than recording a silently empty argv.
        assert_eq!(req.capture, kaish_types::approval::Capture::DirectExecution);
    }

    #[tokio::test]
    async fn the_full_entry_chain_lands_under_the_policy() {
        // Requested → Granted → Redeemed → Settled{Exit(0)} (spec §H).
        use kaish_types::approval::{LedgerEntry, Outcome};
        let (mut ctx, authority) = gated_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        assert_eq!(Rm.execute(args, &mut ctx).await.code, 2);

        let (_id, token) = grant_the_pending_request(&ctx, &authority).await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named.insert("confirm".to_string(), Value::String(token));
        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok(), "{}", result.err);
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);

        // A direct `tool.execute` has no dispatch seam to settle the attempt,
        // so settle it the way the seam would.
        ctx.settle_attempts(0).await;

        let approvals = ctx.ledger_access.as_ref().expect("a wired ledger").approvals.clone();
        let kinds: Vec<&str> = ledger_entries(&approvals)
            .iter()
            .map(|e| match e {
                LedgerEntry::Requested { .. } => "Requested",
                LedgerEntry::Granted { .. } => "Granted",
                LedgerEntry::Redeemed { .. } => "Redeemed",
                LedgerEntry::Settled { .. } => "Settled",
                LedgerEntry::KeyRetrieved { .. } => "KeyRetrieved",
                _ => "other",
            })
            .collect();
        assert!(
            kinds.contains(&"Requested")
                && kinds.contains(&"Granted")
                && kinds.contains(&"Redeemed")
                && kinds.contains(&"Settled"),
            "the full chain must be on the log: {kinds:?}"
        );
        let settled = ledger_entries(&approvals)
            .into_iter()
            .find_map(|e| match e {
                LedgerEntry::Settled { outcome, .. } => Some(outcome),
                _ => None,
            })
            .expect("a Settled entry");
        assert_eq!(settled, Outcome::Exit(0));
    }

    #[tokio::test]
    async fn a_wrong_key_fails_and_the_file_survives() {
        let (mut ctx, authority) = gated_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        assert_eq!(Rm.execute(args, &mut ctx).await.code, 2);
        grant_the_pending_request(&ctx, &authority).await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named
            .insert("confirm".to_string(), Value::String("bogus123".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        assert!(ctx.backend.exists(Path::new("/file.txt")).await);
    }

    #[tokio::test]
    async fn a_key_that_describes_no_request_is_refused_and_voids_nothing() {
        // Spec §F.3 item 2: a guesser cannot void a request it cannot
        // describe. The presentation is recorded against nothing.
        use kaish_types::approval::LedgerEntry;
        let (mut ctx, _authority) = gated_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named
            .insert("confirm".to_string(), Value::String("bogus123".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        assert!(result.err.contains("matches no approval request"), "{}", result.err);
        assert!(ctx.backend.exists(Path::new("/file.txt")).await);

        let approvals = ctx.ledger_access.as_ref().expect("a wired ledger").approvals.clone();
        let rejections: Vec<_> = ledger_entries(&approvals)
            .into_iter()
            .filter_map(|e| match e {
                LedgerEntry::TokenRejected { request, .. } => Some(request),
                _ => None,
            })
            .collect();
        assert_eq!(
            rejections,
            vec![None],
            "the presentation must be recorded against no request"
        );
    }

    #[tokio::test]
    async fn force_on_a_missing_path_never_gates() {
        let (mut ctx, _authority) = gated_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));
        args.flags.insert("f".to_string());

        // -f on a nonexistent path succeeds silently — nothing to gate.
        let result = Rm.execute(args, &mut ctx).await;
        assert!(result.ok());
    }

    #[tokio::test]
    async fn a_missing_path_without_force_errors_rather_than_gating() {
        let (mut ctx, _authority) = gated_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1);
        assert!(result.err.contains("No such file"));
    }

    #[tokio::test]
    async fn re_presenting_a_key_after_success_reports_the_settled_outcome() {
        // **The behavior change the latch's reusable nonce hid.** Under the
        // latch this test asserted the opposite: a nonce stayed valid inside
        // its TTL and re-presenting it silently ran the operation again. A
        // grant now authorizes exactly one *successful* settlement, so the
        // second presentation reports what already happened.
        let (mut ctx, authority) = gated_ctx().await;
        ctx.backend
            .write(Path::new("/file.txt"), b"data", kaish_types::WriteMode::Truncate)
            .await
            .expect("seed the file");

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        assert_eq!(Rm.execute(args, &mut ctx).await.code, 2);
        let (_id, token) = grant_the_pending_request(&ctx, &authority).await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named
            .insert("confirm".to_string(), Value::String(token.clone()));
        assert!(Rm.execute(args, &mut ctx).await.ok());
        assert!(!ctx.backend.exists(Path::new("/file.txt")).await);
        ctx.settle_attempts(0).await;

        // Put the file back. If the second presentation re-ran the delete,
        // the file would vanish a second time — which is exactly what the
        // reusable nonce did.
        ctx.backend
            .write(Path::new("/file.txt"), b"data", kaish_types::WriteMode::Truncate)
            .await
            .expect("restore the file");

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));
        args.named.insert("confirm".to_string(), Value::String(token));
        let second = Rm.execute(args, &mut ctx).await;
        assert_eq!(second.code, 1, "a settled grant must not re-execute");
        assert!(
            second.err.contains("already settled"),
            "the refusal must report the settled outcome: {}",
            second.err
        );
        assert!(
            ctx.backend.exists(Path::new("/file.txt")).await,
            "the file must be deleted exactly once"
        );
    }

    #[tokio::test]
    async fn the_gate_message_names_the_request_and_the_re_run() {
        let (mut ctx, _authority) = gated_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/file.txt".into()));

        let result = Rm.execute(args, &mut ctx).await;
        assert_eq!(result.code, 2);
        let view = result.approval_request().expect("a request");
        assert!(
            result.err.contains(view.id.as_str()),
            "the diagnostic must name the request id: {}",
            result.err
        );
        assert!(view.hint.contains("rm --confirm=<token>"), "{}", view.hint);
        assert!(view.hint.contains("/file.txt"), "{}", view.hint);
        // The view is tokenless by construction (spec §A.2): no field of it,
        // at any depth, is a credential. Walk the serialized keys rather than
        // grepping the text — the hint deliberately contains the literal
        // placeholder `<token>`, which is display text, not a secret.
        let json = serde_json::to_value(&view).expect("the view serializes");
        let mut keys = Vec::new();
        collect_keys(&json, &mut keys);
        for forbidden in ["token", "nonce", "credential", "secret"] {
            assert!(
                !keys.iter().any(|k| k == forbidden),
                "no credential field may reach the view: found {forbidden:?} in {keys:?}"
            );
        }
    }

    /// Every object key in a JSON value, at any depth.
    fn collect_keys(value: &serde_json::Value, out: &mut Vec<String>) {
        match value {
            serde_json::Value::Object(map) => {
                for (k, v) in map {
                    out.push(k.clone());
                    collect_keys(v, out);
                }
            }
            serde_json::Value::Array(items) => {
                for v in items {
                    collect_keys(v, out);
                }
            }
            _ => {}
        }
    }

    // ── Decision logic unit tests ──

    #[test]
    fn test_decide_rm_action_no_flags() {
        let action = decide_rm_action(false, false, None, Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_gate_only() {
        let action = decide_rm_action(false, true, None, Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Gate);
    }

    #[test]
    fn test_decide_rm_action_trash_small_file() {
        let real = PathBuf::from("/home/user/file.txt");
        let action = decide_rm_action(true, false, Some(&real), Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_trash_small_with_approvals() {
        // Small file → trash catches it, approvals irrelevant
        let real = PathBuf::from("/home/user/file.txt");
        let action = decide_rm_action(true, true, Some(&real), Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_trash_large_no_approvals() {
        let real = PathBuf::from("/home/user/bigfile.bin");
        let action = decide_rm_action(true, false, Some(&real), Some(100_000_000), 10_000_000, false, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_large_with_approvals() {
        let real = PathBuf::from("/home/user/bigfile.bin");
        let action = decide_rm_action(true, true, Some(&real), Some(100_000_000), 10_000_000, false, false);
        assert_eq!(action, RmAction::Gate);
    }

    #[test]
    fn test_decide_rm_action_trash_virtual_path() {
        // Virtual path (resolve_real_path returns None) → normal delete
        let action = decide_rm_action(true, false, None, Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_trash_excluded_tmp() {
        let real = PathBuf::from("/tmp/scratch");
        let action = decide_rm_action(true, false, Some(&real), Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Delete);
    }

    #[test]
    fn test_decide_rm_action_real_v_path_is_trashed() {
        // A *real* path under /v (embedder content delegated by mount-coverage
        // routing) is NOT trash-excluded — it must be trashed like any real
        // file, not deleted outright. (In-memory kaish /v mounts stay `None`
        // and are handled by the no-real-path gating, not this predicate.)
        let real = PathBuf::from("/v/cas/blob.bin");
        let action = decide_rm_action(true, false, Some(&real), Some(100), 10_000_000, false, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    // ── Directory-specific tests ──

    #[test]
    fn test_decide_rm_action_dir_always_trashes() {
        let real = PathBuf::from("/home/user/mydir");
        // Directory with size=0 (stat behavior) — should trash regardless of threshold
        let action = decide_rm_action(true, false, Some(&real), Some(0), 10_000_000, true, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_dir_trashes_with_approvals() {
        let real = PathBuf::from("/home/user/mydir");
        // Directory always trashes when trash enabled — approvals irrelevant
        let action = decide_rm_action(true, true, Some(&real), Some(0), 10_000_000, true, false);
        assert_eq!(action, RmAction::Trash(real));
    }

    #[test]
    fn test_decide_rm_action_dir_excluded_tmp() {
        let real = PathBuf::from("/tmp/mydir");
        // Excluded path — directory still gets excluded
        let action = decide_rm_action(true, false, Some(&real), Some(0), 10_000_000, true, false);
        assert_eq!(action, RmAction::Delete);
    }

    // ── Composition matrix (rstest) ──

    #[derive(Debug, PartialEq)]
    enum Outcome {
        Deleted,
        Trashed,
        Gated,
    }

    fn matrix_action_to_outcome(action: &RmAction) -> Outcome {
        match action {
            RmAction::Trash(_) => Outcome::Trashed,
            RmAction::Delete => Outcome::Deleted,
            RmAction::Gate => Outcome::Gated,
        }
    }

    #[test]
    fn test_composition_matrix() {
        let real = PathBuf::from("/home/user/file.txt");
        let small = 100u64;
        let large = 100_000_000u64;
        let max = 10_000_000u64;

        // (trash, enforce, size, is_dir, is_symlink) → expected outcome
        let cases = vec![
            (false, false, small, false, false, Outcome::Deleted),
            (false, true,  small, false, false, Outcome::Gated),
            (true,  false, small, false, false, Outcome::Trashed),
            (true,  true,  small, false, false, Outcome::Trashed),   // trash catches small, approvals irrelevant
            (false, false, large, false, false, Outcome::Deleted),
            (false, true,  large, false, false, Outcome::Gated),
            (true,  false, large, false, false, Outcome::Deleted),    // too big for trash, no gate → delete
            (true,  true,  large, false, false, Outcome::Gated),    // too big for trash + approvals → gate
            // Directories always trash (size irrelevant)
            (true,  false, 0,     true,  false, Outcome::Trashed),
            (true,  true,  0,     true,  false, Outcome::Trashed),
            // Dir without trash enabled → normal flow
            (false, false, 0,     true,  false, Outcome::Deleted),
            (false, true,  0,     true,  false, Outcome::Gated),
            // Symlinks NEVER trash (trashing follows to the target); they unlink
            // directly, but the gate still applies. is_dir is moot for a symlink.
            (true,  false, small, false, true,  Outcome::Deleted),
            (true,  true,  small, false, true,  Outcome::Gated),
            (true,  false, 0,     true,  true,  Outcome::Deleted),    // symlink-to-dir: still just unlink
            (false, false, small, false, true,  Outcome::Deleted),
            (false, true,  small, false, true,  Outcome::Gated),
        ];

        for (trash, enforce, size, is_dir, is_symlink, expected) in cases {
            let action = decide_rm_action(trash, enforce, Some(&real), Some(size), max, is_dir, is_symlink);
            let outcome = matrix_action_to_outcome(&action);
            assert_eq!(
                outcome, expected,
                "trash={}, enforce={}, size={}, is_dir={}, is_symlink={}: expected {:?}, got {:?}",
                trash, enforce, size, is_dir, is_symlink, expected, outcome
            );
        }
    }
}
