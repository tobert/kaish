//! Kernel-routed tests for the destructive-op safety rails: the approval
//! ledger's `fs.*` enforce policy (`set -o approvals`) and trash-on-delete
//! (`set -o trash`).
//!
//! Everything here drives real command strings through `kernel.execute()` so
//! the full pipeline runs — lex → parse → validate → clap binding → builtin.
//! The inline tests in `rm.rs`/`kaish_trash.rs` inject `confirm` directly
//! into `ToolArgs.named`, below the arg-binding layer; these tests are the
//! regression net for the layer above it.
//!
//! Trash tests root their tempdir in `CARGO_TARGET_TMPDIR` (under `target/`),
//! NOT the system temp dir: `decide_rm_action` deliberately skips trash for
//! real paths under `/tmp` and `/v`, so a `/tmp`-rooted tempdir would never
//! reach the trash arm.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use async_trait::async_trait;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::ledger::ApproverHandle;
use kaish_kernel::trash::{TrashBackend, TrashEntry, TrashError};
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{GrantTerms, RequestId};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("approval-trash-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// A kernel and the one approval authority its construction minted — the
/// embedder's posture, in one value. `Kernel::build` is what hands the
/// authority out; a session that only holds the `Kernel` can grant nothing,
/// which is the whole separation (spec §E.2, tier 1).
struct Session {
    kernel: Kernel,
    authority: ApproverHandle,
}

impl Session {
    /// Grant the single pending request and retrieve its bearer key — what an
    /// embedder does between the exit-2 result and the re-run.
    async fn approve_pending(&self) -> String {
        let (_id, token) = self.approve_pending_id().await;
        token
    }

    /// As [`Self::approve_pending`], also returning the request's id.
    async fn approve_pending_id(&self) -> (RequestId, String) {
        let approvals = self.kernel.approvals();
        let pending = approvals.pending(kaish_types::approval::PageRequest::default()).items;
        assert_eq!(
            pending.len(),
            1,
            "exactly one request must be pending: {:?}",
            pending.iter().map(|p| p.id.as_str()).collect::<Vec<_>>()
        );
        let id = pending[0].id.clone();
        self.grant(&id).await;
        let token = self
            .authority
            .token_for(&id)
            .expect("a credential for a granted request");
        (id, token.reveal().to_string())
    }

    /// Grant `id` for the next five minutes, on the terms the request itself
    /// declared — including its transitions, which become the redemption's
    /// conditions.
    async fn grant(&self, id: &RequestId) {
        let chain = self.kernel.approvals().get(id).expect("the request's chain");
        let terms = GrantTerms::once_for_view(
            &chain.request,
            std::time::SystemTime::now() + std::time::Duration::from_secs(300),
        );
        self.authority
            .grant(id, chain.request.revision, terms)
            .await
            .expect("the grant must post");
    }

    /// Grant the single pending request and replay it through
    /// `Kernel::confirm` — the approval side's own fulfillment path.
    async fn confirm_pending(&self) -> ExecResult {
        let approvals = self.kernel.approvals();
        let pending = approvals.pending(kaish_types::approval::PageRequest::default()).items;
        assert_eq!(pending.len(), 1, "exactly one request must be pending");
        let id = pending[0].id.clone();
        self.grant(&id).await;
        self.kernel
            .confirm(&self.authority, &id)
            .await
            .expect("confirm executes")
    }
}

/// Kernel with the enforce policy and trash forced OFF regardless of the
/// developer's KAISH_APPROVALS / KAISH_TRASH env (which `repl()` reads). Each
/// test opts in via `set -o approvals` / `set -o trash` so the enable path itself
/// is kernel-routed too.
fn kernel_at(dir: &Path) -> Session {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_approvals(false)
        .with_trash(false);
    let (kernel, authority) = Kernel::build(config).expect("kernel");
    Session { kernel, authority }
}

async fn run(session: &Session, script: &str) -> ExecResult {
    session.kernel.execute(script).await.expect("kernel execute")
}

/// Pull a string field off a gated result's typed `.approval` request.
fn approval_str(result: &ExecResult, key: &str) -> String {
    let req = result
        .approval_request()
        .expect("an exit-2 gate result carries a typed ApprovalRequestView");
    match key {
        "hint" => req.hint,
        "operation" => req.operation.to_string(),
        "id" => req.id.as_str().to_string(),
        other => panic!("approval field {other:?} is not a string field"),
    }
}

// ============================================================================
// The fs.* enforce policy (`set -o approvals`) — rm exit 2 → grant → confirm
// ============================================================================

#[tokio::test]
async fn latch_gates_rm_then_confirm_hint_deletes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let session = kernel_at(dir.path());

    let enable = run(&session, "set -o approvals").await;
    assert_eq!(enable.code, 0, "set -o approvals failed: {}", enable.err);

    // First rm: gated. Exit 2, file untouched, a pending request on `.approval`.
    let gated = run(&session, "rm precious.txt").await;
    assert_eq!(gated.code, 2, "expected the enforce-policy gate, exit 2, err: {}", gated.err);
    assert!(
        gated.err.contains("pending approval"),
        "gate message missing: {}",
        gated.err
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "file must survive the gate"
    );

    // The hint is a display-only re-run template carrying a `<token>`
    // placeholder — it never contains a credential (spec §A.2), unlike the
    // old latch's baked-in nonce. Grant the request, splice in the real
    // token, and run the advertised command verbatim: pins that the re-run
    // template actually parses and binds through the kernel once an operator
    // supplies the missing piece.
    let hint = approval_str(&gated, "hint");
    assert!(hint.contains("<token>"), "the hint must carry no credential: {hint}");
    let token = session.approve_pending().await;
    let confirmed = run(&session, &hint.replace("<token>", &token)).await;
    assert_eq!(
        confirmed.code, 0,
        "confirm hint {hint:?} failed: {}",
        confirmed.err
    );
    assert!(
        !dir.path().join("precious.txt").exists(),
        "file should be deleted after confirmation"
    );
}

#[tokio::test]
async fn latch_json_surfaces_the_request_under_approval_key() {
    // `--json` on a gated op surfaces the pending request under a dedicated
    // `approval` key in the error envelope — control-plane, never folded into
    // `data`. And `.approval` survives formatting, so approval_request() still
    // works on the --json'd result.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    let gated = run(&session, "rm precious.txt --json").await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);

    let envelope: serde_json::Value =
        serde_json::from_str(gated.text_out().trim()).expect("a JSON error envelope");
    assert_eq!(envelope["code"], 2, "envelope: {envelope}");
    assert!(envelope["error"].is_string(), "envelope: {envelope}");
    assert!(
        envelope["approval"]["id"].is_string(),
        "the request id must be under the `approval` key: {envelope}"
    );
    assert_eq!(envelope["approval"]["operation"], "fs.remove", "envelope: {envelope}");
    assert!(
        envelope.get("data").is_none(),
        "the approval request must not be folded under `data`: {envelope}"
    );
    assert!(
        gated.approval_request().is_some(),
        "the typed approval request must survive --json formatting"
    );
}

#[tokio::test]
async fn latch_survives_stdout_redirect() {
    // A stdout redirect on a gated `rm` must NOT disable the gate.
    // `apply_redirects` clears the *data-plane* `.data` (the structured
    // view of stdout) on a stdout redirect — but the approval request is a
    // *control-plane* signal, not stdout. Dropping it would silently turn
    // `rm precious.txt > log` into an ungateable delete-in-waiting: exit 2,
    // no recoverable request, file stranded. Regression guard for the
    // `.data`-clearing that landed with redirect-inside-`$()` support.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    let gated = run(&session, "rm precious.txt > out.log").await;

    assert_eq!(
        gated.code, 2,
        "a redirect must not bypass the gate: {}",
        gated.err
    );
    assert!(
        gated.approval_request().is_some(),
        "the approval request must survive a stdout redirect (it is control-plane, \
         not stdout); got data: {:?}",
        gated.data
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "file must survive the gate even with a redirect"
    );
}

#[tokio::test]
async fn latch_captures_the_exact_invocation() {
    // The gate stamps the dispatch seam's captured invocation on `.capture`,
    // so an embedder can inspect precisely what `Kernel::confirm` will replay.
    use kaish_types::approval::Capture;

    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    let gated = run(&session, "rm precious.txt").await;
    let req = gated.approval_request().expect("a pending approval request");

    let capture = req.capture.clone();
    let Capture::Exact(invocation) = capture else {
        panic!("a kernel-routed rm must capture Exact, got {:?}", req.capture);
    };
    assert_eq!(invocation.tool, "rm", "dispatch name should be the argv0 for replay");
    assert!(
        invocation.argv.iter().any(|a| a == "precious.txt"),
        "captured argv must contain the operand: {:?}",
        invocation.argv
    );
}

#[tokio::test]
async fn confirm_replays_rm_and_deletes() {
    // The whole point: inspect the gate, grant it, then fulfill it by
    // replaying the exact captured invocation — no hint string, no manual
    // argv reconstruction.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    let gated = run(&session, "rm precious.txt").await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);

    let done = session.confirm_pending().await;
    assert_eq!(done.code, 0, "confirm should succeed: {}", done.err);
    assert!(
        !dir.path().join("precious.txt").exists(),
        "confirm should have deleted the file"
    );
}

#[tokio::test]
async fn confirm_replays_a_path_with_spaces_the_hint_cannot() {
    // The payoff of capturing argv over the hint string: a path with a space
    // round-trips exactly through `confirm` (execute_argv, no re-parse),
    // whereas the hint (a bare, unquoted re-run template) would re-parse as
    // two paths even with a token spliced in. This is why argv capture
    // matters.
    use kaish_types::approval::Capture;

    let dir = tempdir();
    std::fs::write(dir.path().join("a b.txt"), "data").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    let gated = run(&session, r#"rm "a b.txt""#).await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);
    let req = gated.approval_request().expect("a pending approval request");
    let Capture::Exact(invocation) = req.capture.clone() else {
        panic!("a kernel-routed rm must capture Exact, got {:?}", req.capture);
    };
    assert!(
        invocation.argv.iter().any(|a| a == "a b.txt"),
        "the space-bearing path must survive as one argv token: {:?}",
        invocation.argv
    );

    let done = session.confirm_pending().await;
    assert_eq!(done.code, 0, "confirm should succeed: {}", done.err);
    assert!(
        !dir.path().join("a b.txt").exists(),
        "confirm should have deleted the space-named file"
    );
}

#[tokio::test]
async fn confirm_replays_a_gate_overwrite() {
    // The overwrite gate (`cp`/`mv`/`tee`/…) goes through `gate_overwrites`, a
    // different producer than `rm` — the dispatch-seam capture covers it too.
    let dir = tempdir();
    std::fs::write(dir.path().join("src.txt"), "fresh").expect("write");
    std::fs::write(dir.path().join("dst.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    let gated = run(&session, "cp src.txt dst.txt").await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);
    let req = gated.approval_request().expect("a pending approval request");
    assert_eq!(req.operation.as_str(), "fs.overwrite");

    let done = session.confirm_pending().await;
    assert_eq!(done.code, 0, "confirm should succeed: {}", done.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(),
        "fresh",
        "confirm should have completed the overwrite"
    );
}

#[tokio::test]
async fn confirm_replays_a_subcommand_gate() {
    // `kaish-trash empty` gates *unconditionally* (inherently destructive — no
    // `set -o approvals` needed), and its dispatch name ("kaish-trash") differs
    // from its display command ("kaish-trash empty"). Two things to prove:
    // the seam captures the argv even with the enforce policy off (so
    // `confirm` has a replayable invocation), and replaying it recomputes the
    // same command scope the request was raised under, so it validates.
    use kaish_types::approval::Capture;

    let dir = tempdir();
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    let gated = run(&session, "kaish-trash empty").await;
    assert_eq!(gated.code, 2, "empty gates unconditionally: {}", gated.err);
    let req = gated.approval_request().expect("a pending approval request");
    let Capture::Exact(invocation) = req.capture.clone() else {
        panic!("kaish-trash empty must capture Exact, got {:?}", req.capture);
    };
    assert_eq!(invocation.tool, "kaish-trash", "dispatch name for replay");
    assert!(
        invocation.argv.iter().any(|a| a == "empty"),
        "the subcommand token must be captured in argv: {:?}",
        invocation.argv
    );

    let done = session.confirm_pending().await;
    assert_eq!(done.code, 0, "confirm should empty the trash: {}", done.err);
}

// `confirm_without_captured_invocation_errors` (the old latch's bare,
// hand-constructed `LatchRequest` with empty tool/argv) has no equivalent
// under the ledger: `LatchRequest` no longer exists, and every request this
// test file can raise goes through `kernel.execute()`'s dispatch seam, which
// always captures `Capture::Exact` (see `ExecContext::capture` in
// `tools/context.rs`). Producing a `Capture::DirectExecution`/`Unavailable`/
// `CaptureFailed` request requires calling `ToolCtx::request_approval`
// directly, bypassing the kernel dispatcher entirely — there is no such path
// from a `Kernel`+`ApproverHandle` pair, which is all this file has to work
// with. `Kernel::confirm`'s refusal of a non-`Exact` capture (exit 2, naming
// the variant) is exercised at the right altitude instead, by
// `tool_ctx_approval_tests.rs`'s `kernel_request_approval_round_trips_a_request_through_the_ledger`
// (which asserts the `DirectExecution` capture itself) and by `kernel.rs`'s
// own doc comment on `confirm`. Deleted rather than faked.

#[tokio::test]
async fn latch_bogus_token_fails_and_file_survives() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    // Well-formed but never issued: rejection has to come from the ledger's
    // draft matcher, not from the shape of the string.
    let r = run(&session, "rm --confirm=\"9f2c7d1a0b4e63859c7a1e0d5b8f4a26\" precious.txt").await;
    assert_eq!(r.code, 1, "bogus token must fail, out: {}", r.text_out());
    assert!(
        r.err.contains("matches no approval request"),
        "expected the draft-matcher's refusal, got: {}",
        r.err
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "file must survive a rejected token"
    );
}

#[tokio::test]
async fn latch_batches_multiple_paths_under_one_request() {
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "a").expect("write");
    std::fs::write(dir.path().join("b.txt"), "b").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    let gated = run(&session, "rm a.txt b.txt").await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);
    let req = gated.approval_request().expect("a pending approval request");
    assert!(
        req.resources.iter().any(|r| r.id == "a.txt") && req.resources.iter().any(|r| r.id == "b.txt"),
        "the request should authorize both paths: {:?}",
        req.resources
    );

    let confirmed = session.confirm_pending().await;
    assert_eq!(confirmed.code, 0, "err: {}", confirmed.err);
    assert!(!dir.path().join("a.txt").exists(), "a.txt should be gone");
    assert!(!dir.path().join("b.txt").exists(), "b.txt should be gone");
}

#[tokio::test]
async fn latch_off_by_default_rm_deletes_directly() {
    let dir = tempdir();
    std::fs::write(dir.path().join("plain.txt"), "x").expect("write");
    let session = kernel_at(dir.path());

    let r = run(&session, "rm plain.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    assert!(!dir.path().join("plain.txt").exists());
}

#[tokio::test]
async fn latch_in_a_pipeline_stage_overrides_later_success() {
    // GH #125: an approval gate raised by an EARLIER pipeline stage must
    // survive a later stage's nominal success. `rm x | echo done` used to exit
    // 0 with the gate dropped (only the last stage's result survived), so an
    // agent gating on exit codes saw success while `rm` never ran. The gate is
    // a control-plane fact about the whole pipeline: exit 2, `.approval`
    // present, and the file untouched.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    let piped = run(&session, "rm precious.txt | echo done").await;

    assert_eq!(
        piped.code, 2,
        "an earlier stage's gate must set the pipeline exit code, not the last \
         stage's 0: {}",
        piped.err
    );
    let req = piped
        .approval_request()
        .expect("the earlier stage's approval request must ride the pipeline result");
    assert_eq!(req.operation.as_str(), "fs.remove", "the gated command is rm, not echo");
    assert!(
        dir.path().join("precious.txt").exists(),
        "the gated file must survive — the gate held, the op never ran"
    );
}

#[tokio::test]
async fn latch_first_stage_wins_when_two_stages_gate() {
    // Two gated stages in one pipeline: first gate wins (matches wait.rs
    // classify()'s first-latch-wins). `rm a | rm b` under the enforce policy
    // surfaces a's request, and BOTH files survive (each stage gated
    // independently).
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "a").expect("write");
    std::fs::write(dir.path().join("b.txt"), "b").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    let piped = run(&session, "rm a.txt | rm b.txt").await;

    assert_eq!(piped.code, 2, "err: {}", piped.err);
    let req = piped.approval_request().expect("an approval request must ride the result");
    // First stage's gate authorizes a.txt; grant it by hand and present the
    // token — that must confirm the FIRST stage's path, not the last one's.
    assert!(
        req.resources.iter().any(|r| r.id == "a.txt"),
        "the surfaced request must be the FIRST stage's, over a.txt: {:?}",
        req.resources
    );

    session.grant(&req.id).await;
    let token = session
        .authority
        .token_for(&req.id)
        .expect("a credential for the granted request")
        .reveal()
        .to_string();
    let confirm_a = run(&session, &format!("rm --confirm={token} a.txt")).await;
    assert_eq!(
        confirm_a.code, 0,
        "the granted token must confirm the FIRST stage's path: {}",
        confirm_a.err
    );
    assert!(!dir.path().join("a.txt").exists(), "a.txt confirmed and removed");
    assert!(dir.path().join("b.txt").exists(), "b.txt still gated, untouched");
}

// ============================================================================
// Trash-on-delete — mock TrashBackend covering the RmAction::Trash arm
// ============================================================================

/// Recording/failing mock. `trash()` only records (it does NOT move the
/// file) — so after a successful trash the file still existing on disk
/// proves rm *delegated* the removal and didn't also permanently delete.
#[derive(Default)]
struct MockTrash {
    trashed: Mutex<Vec<PathBuf>>,
    /// Byte snapshots recorded by `trash_bytes` (overlay/in-memory overwrites):
    /// the logical path and its captured prior content.
    snapshots: Mutex<Vec<(PathBuf, Vec<u8>)>>,
    fail: bool,
}

impl MockTrash {
    fn failing() -> Self {
        Self { fail: true, ..Self::default() }
    }

    fn trashed_paths(&self) -> Vec<PathBuf> {
        self.trashed.lock().expect("mock lock").clone()
    }

    fn snapshots(&self) -> Vec<(PathBuf, Vec<u8>)> {
        self.snapshots.lock().expect("mock lock").clone()
    }
}

#[async_trait]
impl TrashBackend for MockTrash {
    async fn trash(&self, path: &Path) -> Result<(), TrashError> {
        if self.fail {
            return Err(TrashError::Backend("mock trash refused".into()));
        }
        self.trashed
            .lock()
            .expect("mock lock")
            .push(path.to_path_buf());
        Ok(())
    }

    async fn trash_bytes(&self, original_path: &Path, bytes: &[u8]) -> Result<(), TrashError> {
        if self.fail {
            return Err(TrashError::Backend("mock trash refused".into()));
        }
        self.snapshots
            .lock()
            .expect("mock lock")
            .push((original_path.to_path_buf(), bytes.to_vec()));
        Ok(())
    }

    async fn list(&self, _filter: Option<&str>) -> Result<Vec<TrashEntry>, TrashError> {
        Ok(Vec::new())
    }

    async fn find_by_name(&self, _name: &str) -> Result<Vec<TrashEntry>, TrashError> {
        Ok(Vec::new())
    }

    async fn restore(&self, _entries: Vec<TrashEntry>) -> Result<(), TrashError> {
        Ok(())
    }

    async fn purge_all(&self) -> Result<usize, TrashError> {
        Ok(0)
    }
}

fn kernel_with_trash(dir: &Path, mock: &Arc<MockTrash>) -> Session {
    let mut session = kernel_at(dir);
    session
        .kernel
        .set_trash_backend(Some(Arc::clone(mock) as Arc<dyn TrashBackend>));
    session
}

#[tokio::test]
async fn trash_small_file_delegates_to_backend() {
    let dir = tempdir();
    std::fs::write(dir.path().join("keep.txt"), "data").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    let r = run(&session, "rm keep.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);

    let trashed = mock.trashed_paths();
    assert_eq!(trashed.len(), 1, "exactly one trash call: {trashed:?}");
    assert!(
        trashed[0].ends_with("keep.txt"),
        "trash received the real path: {trashed:?}"
    );
    // The recording mock didn't move the file; if rm ALSO deleted it the
    // delegation contract is broken (double delete = trash can't restore).
    assert!(
        dir.path().join("keep.txt").exists(),
        "rm must delegate removal to the trash backend, not delete as well"
    );
}

#[tokio::test]
async fn trash_directory_always_trashes_without_recursive_flag() {
    let dir = tempdir();
    std::fs::create_dir(dir.path().join("sub")).expect("mkdir");
    std::fs::write(dir.path().join("sub/inner.txt"), "x").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    // Directories always trash (no -r needed; trash moves them atomically).
    let r = run(&session, "rm sub").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    let trashed = mock.trashed_paths();
    assert_eq!(trashed.len(), 1, "one trash call for the dir: {trashed:?}");
    assert!(trashed[0].ends_with("sub"));
}

#[tokio::test]
async fn trash_catches_small_file_even_with_approvals_enabled() {
    let dir = tempdir();
    std::fs::write(dir.path().join("small.txt"), "tiny").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    run(&session, "set -o trash").await;
    // Priority: trash catches small files before the enforce policy gates
    // them — no exit 2, no request, straight to the backend.
    let r = run(&session, "rm small.txt").await;
    assert_eq!(r.code, 0, "trash should win over the gate, err: {}", r.err);
    assert_eq!(mock.trashed_paths().len(), 1);
}

#[tokio::test]
async fn trash_failure_is_loud_and_never_falls_through_to_delete() {
    let dir = tempdir();
    std::fs::write(dir.path().join("guarded.txt"), "data").expect("write");
    let mock = Arc::new(MockTrash::failing());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    let r = run(&session, "rm guarded.txt").await;
    assert_eq!(r.code, 1, "trash failure must be an error, not silent");
    assert!(
        r.err.contains("trash failed"),
        "error should name the trash failure: {}",
        r.err
    );
    // THE invariant: a trash failure never falls through to permanent delete.
    assert!(
        dir.path().join("guarded.txt").exists(),
        "trash failure fell through to permanent delete"
    );
}

#[tokio::test]
async fn trash_backend_absent_fails_loud() {
    let dir = tempdir();
    std::fs::write(dir.path().join("orphan.txt"), "data").expect("write");
    let mut session = kernel_at(dir.path());
    session.kernel.set_trash_backend(None);

    run(&session, "set -o trash").await;
    let r = run(&session, "rm orphan.txt").await;
    assert_eq!(r.code, 1, "missing backend must be an error");
    assert!(
        r.err.contains("trash backend not available"),
        "error should name the missing backend: {}",
        r.err
    );
    assert!(
        dir.path().join("orphan.txt").exists(),
        "missing trash backend must not fall through to delete"
    );
}

// ============================================================================
// Write-model gate: tee overwrites honor approvals + trash (like rm gates deletes)
// ============================================================================

/// In-memory kernel (`/v` mounts have no real path) wired to the mock trash —
/// for exercising the overlay `trash_bytes` snapshot path.
fn isolated_kernel_with_trash(mock: &Arc<MockTrash>) -> Session {
    let (kernel, authority) = Kernel::build(KernelConfig::isolated()).expect("kernel");
    let mut session = Session { kernel, authority };
    session
        .kernel
        .set_trash_backend(Some(Arc::clone(mock) as Arc<dyn TrashBackend>));
    session
}

#[tokio::test]
async fn tee_overwrite_under_trash_snapshots_prior_bytes_first() {
    let dir = tempdir();
    std::fs::write(dir.path().join("doc.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    let r = run(&session, "echo new | tee doc.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);

    // The prior content is COPIED to trash (not moved) — the file stays put and
    // gets the new content, and a recoverable byte-snapshot of "old" is taken.
    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "one byte-snapshot for the overwrite: {snaps:?}");
    assert!(snaps[0].0.ends_with("doc.txt"));
    assert_eq!(snaps[0].1, b"old", "the snapshot captured the prior content");
    assert!(mock.trashed_paths().is_empty(), "overwrite copies, never moves the file");
    let now = std::fs::read_to_string(dir.path().join("doc.txt")).expect("read");
    assert_eq!(now, "new\n", "the new content is written after the snapshot");
}

#[tokio::test]
async fn tee_overwrite_under_latch_requires_confirm() {
    let dir = tempdir();
    std::fs::write(dir.path().join("doc.txt"), "keep").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    // latch on, trash off: the overwrite must be confirmed.
    run(&session, "set -o approvals").await;
    let r = run(&session, "echo new | tee doc.txt").await;
    assert_eq!(r.code, 2, "latch gates the overwrite: {}", r.err);
    assert!(approval_str(&r, "hint").contains("--confirm="));
    assert_eq!(
        std::fs::read_to_string(dir.path().join("doc.txt")).expect("read"),
        "keep",
        "the file must be untouched until confirmed"
    );

    // Re-run with a granted token.
    let token = session.approve_pending().await;
    let r2 = run(&session, &format!("echo new | tee --confirm=\"{token}\" doc.txt")).await;
    assert_eq!(r2.code, 0, "confirmed overwrite succeeds: {}", r2.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("doc.txt")).expect("read"),
        "new\n"
    );
}

#[tokio::test]
async fn tee_new_file_and_append_do_not_gate() {
    let dir = tempdir();
    std::fs::write(dir.path().join("log.txt"), "line1\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    // New file: nothing to lose, no gate.
    let r = run(&session, "echo hi | tee fresh.txt").await;
    assert_eq!(r.code, 0, "new file should not gate: {}", r.err);
    // Append: doesn't destroy prior content, no gate.
    let r2 = run(&session, "echo line2 | tee -a log.txt").await;
    assert_eq!(r2.code, 0, "append should not gate: {}", r2.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("log.txt")).expect("read"),
        "line1\nline2\n"
    );
}

#[tokio::test]
async fn tee_overlay_overwrite_snapshots_bytes_via_trash_bytes() {
    let mock = Arc::new(MockTrash::default());
    let session = isolated_kernel_with_trash(&mock);

    run(&session, "set -o trash").await;
    run(&session, "write /v/f.txt \"original\"").await; // seed an in-memory file
    let r = run(&session, "echo new | tee /v/f.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);

    // No real path → prior content captured via trash_bytes, not trash().
    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "one byte-snapshot for the overlay overwrite: {snaps:?}");
    assert!(snaps[0].0.ends_with("f.txt"));
    assert_eq!(snaps[0].1, b"original", "the captured bytes are the prior content");
    assert!(mock.trashed_paths().is_empty(), "no real-path trash for an overlay file");

    let out = run(&session, "cat /v/f.txt").await;
    assert_eq!(out.text_out().trim(), "new", "new content written after the snapshot");
}

// ============================================================================
// Write-model gate: patch overwrites honor approvals + trash (same gate as tee)
// ============================================================================

/// A one-line unified diff turning `old` into `new` in `f.txt`, fed via a
/// heredoc. The `f.txt` operand overrides the diff header, so strip level
/// doesn't matter.
const PATCH_SCRIPT: &str = "patch f.txt <<'EOF'\n--- a/f.txt\n+++ b/f.txt\n@@ -1 +1 @@\n-old\n+new\nEOF\n";

#[tokio::test]
async fn patch_overwrite_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "old\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    let r = run(&session, PATCH_SCRIPT).await;
    assert_eq!(r.code, 0, "err: {}", r.err);

    // Prior content copied to trash before the patch write; file stays in place
    // (the read-modify-write still saw it) and now holds the patched content.
    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "one byte-snapshot before the patch: {snaps:?}");
    assert_eq!(snaps[0].1, b"old\n", "snapshot captured the prior content");
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "new\n"
    );
}

#[tokio::test]
async fn patch_overwrite_under_latch_requires_confirm() {
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "old\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    let r = run(&session, PATCH_SCRIPT).await;
    assert_eq!(r.code, 2, "latch gates the patch: {}", r.err);
    assert!(approval_str(&r, "hint").contains("--confirm="));
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "old\n",
        "the file must be untouched until confirmed"
    );

    let token = session.approve_pending().await;
    let confirmed_script =
        PATCH_SCRIPT.replace("patch f.txt", &format!("patch --confirm=\"{token}\" f.txt"));
    let r2 = run(&session, &confirmed_script).await;
    assert_eq!(r2.code, 0, "confirmed patch succeeds: {}", r2.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "new\n"
    );
}

#[tokio::test]
async fn patch_explicit_file_multi_group_diff_snapshots_once() {
    // A multi-group diff applied to one explicit target lists that file once per
    // group; the gate must dedup so it snapshots the prior bytes a single time,
    // not once per group.
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "alpha\nbeta\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    let script = "patch f.txt <<'EOF'\n\
        --- a/x\n+++ b/x\n@@ -1 +1 @@\n-alpha\n+ALPHA\n\
        --- a/y\n+++ b/y\n@@ -2 +2 @@\n-beta\n+BETA\n\
        EOF\n";

    run(&session, "set -o trash").await;
    let r = run(&session, script).await;
    assert_eq!(r.code, 0, "err: {}", r.err);

    let snaps = mock.snapshots();
    assert_eq!(
        snaps.len(),
        1,
        "the explicit target is deduped to one snapshot: {snaps:?}"
    );
    assert_eq!(snaps[0].1, b"alpha\nbeta\n", "snapshot captured prior content");
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "ALPHA\nBETA\n"
    );
}

#[tokio::test]
async fn patch_explicit_file_multi_group_latch_lists_target_once() {
    // The gate's resource list and hint must not list the same explicit
    // target once per hunk-group.
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "alpha\nbeta\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    let script = "patch f.txt <<'EOF'\n\
        --- a/x\n+++ b/x\n@@ -1 +1 @@\n-alpha\n+ALPHA\n\
        --- a/y\n+++ b/y\n@@ -2 +2 @@\n-beta\n+BETA\n\
        EOF\n";

    run(&session, "set -o approvals").await;
    let r = run(&session, script).await;
    assert_eq!(r.code, 2, "latch gates: {}", r.err);
    let req = r.approval_request().expect("a pending approval request");
    assert_eq!(
        req.resources.len(),
        1,
        "the deduped target must be listed once, not once per hunk-group: {:?}",
        req.resources
    );
    // A dedup miss would repeat the path within the hint's re-run list, e.g.
    // "f.txt, f.txt" / "f.txt f.txt".
    assert!(
        !req.hint.contains("f.txt, f.txt") && !req.hint.contains("f.txt f.txt"),
        "the deduped target must not repeat within the hint: {}",
        req.hint
    );
}

#[tokio::test]
async fn patch_dry_run_does_not_gate() {
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "old\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    let dry = PATCH_SCRIPT.replace("patch f.txt", "patch --dry-run f.txt");
    let r = run(&session, &dry).await;
    assert_eq!(r.code, 0, "dry-run never writes, so it never gates: {}", r.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "old\n",
        "dry-run leaves the file untouched"
    );
}

// ============================================================================
// Write-model gate: sed -i in-place edits honor approvals + trash (same gate)
// ============================================================================

#[tokio::test]
async fn sed_in_place_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "old\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    let r = run(&session, "sed -i 's/old/new/' f.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);

    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "one byte-snapshot before the in-place write: {snaps:?}");
    assert_eq!(snaps[0].1, b"old\n", "snapshot captured the prior content");
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "new\n"
    );
}

#[tokio::test]
async fn sed_in_place_under_latch_requires_confirm() {
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "old\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    let r = run(&session, "sed -i 's/old/new/' f.txt").await;
    assert_eq!(r.code, 2, "latch gates the in-place edit: {}", r.err);
    assert!(approval_str(&r, "hint").contains("--confirm="));
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "old\n",
        "the file must be untouched until confirmed"
    );

    let token = session.approve_pending().await;
    let r2 = run(&session, &format!("sed -i --confirm=\"{token}\" 's/old/new/' f.txt")).await;
    assert_eq!(r2.code, 0, "confirmed in-place edit succeeds: {}", r2.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "new\n"
    );
}

#[tokio::test]
async fn sed_in_place_latch_hint_is_runnable_once_granted() {
    // Regression: the hint must reinject `-i` and the expression. A bare
    // `sed --confirm=<token> f.txt` would read f.txt as the expression and
    // hang on stdin. The hint is display-only and carries a `<token>`
    // placeholder rather than a credential (spec §A.2) — splice in a granted
    // token and run it verbatim, requiring the advertised re-run to actually
    // work.
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "old\n").expect("write");
    let session = kernel_at(dir.path());

    run(&session, "set -o approvals").await;
    let r = run(&session, "sed -i 's/old/new/' f.txt").await;
    assert_eq!(r.code, 2, "latch gates: {}", r.err);

    let hint = approval_str(&r, "hint");
    assert!(hint.contains("-i"), "hint keeps -i: {hint}");
    assert!(hint.contains("s/old/new/"), "hint keeps the expression: {hint}");
    assert!(hint.contains("<token>"), "the hint must carry no credential: {hint}");

    let token = session.approve_pending().await;
    let r2 = run(&session, &hint.replace("<token>", &token)).await;
    assert_eq!(
        r2.code, 0,
        "running the hint (with a granted token spliced in) edits the file: {}",
        r2.err
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "new\n"
    );
}

#[tokio::test]
async fn sed_in_place_multi_file_and_e_flag_edits_all() {
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "old\n").expect("write");
    std::fs::write(dir.path().join("b.txt"), "old\n").expect("write");
    let session = kernel_at(dir.path());

    // -i alongside -e, across multiple file operands (gates off by default).
    let r = run(&session, "sed -i -e 's/old/new/' a.txt b.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("a.txt")).expect("read"), "new\n");
    assert_eq!(std::fs::read_to_string(dir.path().join("b.txt")).expect("read"), "new\n");
}

#[tokio::test]
async fn sed_in_place_continues_past_per_file_error() {
    let dir = tempdir();
    std::fs::write(dir.path().join("good.txt"), "old\n").expect("write");
    let session = kernel_at(dir.path());

    // A missing operand must not abort the batch: the good file is still edited
    // and the run reports failure for the missing one.
    let r = run(&session, "sed -i 's/old/new/' missing.txt good.txt").await;
    assert_eq!(r.code, 1, "a per-file failure yields a non-zero exit: {}", r.err);
    assert!(r.err.contains("missing.txt"), "error names the bad file: {}", r.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("good.txt")).expect("read"),
        "new\n",
        "the good file is still edited"
    );
}

#[tokio::test]
async fn sed_in_place_without_operands_is_a_loud_error() {
    let dir = tempdir();
    let session = kernel_at(dir.path());
    // Editing a stream in place is meaningless — loud error, not a fall-through.
    let r = run(&session, "echo hi | sed -i 's/h/H/'").await;
    assert_eq!(r.code, 1, "no file operands must error: {}", r.err);
    assert!(r.err.contains("requires file operands"), "err: {}", r.err);
}

// ============================================================================
// Write-model gate: write / dd / cp / mv overwrites honor approvals + trash too
// (the same gate as tee/patch/sed -i). These builtins previously bypassed it.
// ============================================================================

#[tokio::test]
async fn write_overwrite_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("doc.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    let r = run(&session, "write doc.txt \"new\"").await;
    assert_eq!(r.code, 0, "err: {}", r.err);

    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "one snapshot for the overwrite: {snaps:?}");
    assert_eq!(snaps[0].1, b"old", "the snapshot captured the prior content");
    assert_eq!(std::fs::read_to_string(dir.path().join("doc.txt")).unwrap(), "new");
}

#[tokio::test]
async fn write_overwrite_under_latch_requires_confirm() {
    let dir = tempdir();
    std::fs::write(dir.path().join("doc.txt"), "keep").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    let r = run(&session, "write doc.txt \"new\"").await;
    assert_eq!(r.code, 2, "latch gates write: {}", r.err);
    assert!(approval_str(&r, "hint").contains("--confirm="));
    assert_eq!(std::fs::read_to_string(dir.path().join("doc.txt")).unwrap(), "keep");

    let token = session.approve_pending().await;
    let r2 = run(&session, &format!("write --confirm=\"{token}\" doc.txt \"new\"")).await;
    assert_eq!(r2.code, 0, "confirmed write succeeds: {}", r2.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("doc.txt")).unwrap(), "new");
}

#[tokio::test]
async fn write_new_file_does_not_gate() {
    let dir = tempdir();
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);
    run(&session, "set -o approvals").await;
    let r = run(&session, "write fresh.txt \"hi\"").await;
    assert_eq!(r.code, 0, "a new file has nothing to lose, no gate: {}", r.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("fresh.txt")).unwrap(), "hi");
}

#[tokio::test]
async fn overwrite_too_big_for_trash_falls_through_to_latch() {
    let dir = tempdir();
    std::fs::write(dir.path().join("big.txt"), "0123456789").expect("write"); // 10 bytes
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "kaish-trash config max-size 2").await; // 2-byte cap
    run(&session, "set -o trash").await;
    run(&session, "set -o approvals").await;
    // 10 bytes > the 2-byte cap: can't snapshot, so trash is skipped and the
    // gate fires instead — mirroring rm's too-big-to-trash fall-through (#3).
    let r = run(&session, "write big.txt \"new\"").await;
    assert_eq!(r.code, 2, "a file too big to trash should gate: {}", r.err);
    assert!(mock.snapshots().is_empty(), "no snapshot when over the cap");
    assert_eq!(
        std::fs::read_to_string(dir.path().join("big.txt")).unwrap(),
        "0123456789",
        "the file is untouched until confirmed"
    );
}

#[tokio::test]
async fn cp_overwrite_under_latch_requires_confirm() {
    let dir = tempdir();
    std::fs::write(dir.path().join("src.txt"), "fresh").expect("write");
    std::fs::write(dir.path().join("dst.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    let r = run(&session, "cp src.txt dst.txt").await;
    assert_eq!(r.code, 2, "cp onto an existing file gates: {}", r.err);
    assert!(approval_str(&r, "hint").contains("--confirm="));
    assert_eq!(std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(), "old");

    let token = session.approve_pending().await;
    let r2 = run(&session, &format!("cp --confirm=\"{token}\" src.txt dst.txt")).await;
    assert_eq!(r2.code, 0, "confirmed cp succeeds: {}", r2.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(), "fresh");
}

#[tokio::test]
async fn cp_overwrite_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("src.txt"), "fresh").expect("write");
    std::fs::write(dir.path().join("dst.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    let r = run(&session, "cp src.txt dst.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "snapshot of the prior destination: {snaps:?}");
    assert_eq!(snaps[0].1, b"old");
    assert_eq!(std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(), "fresh");
}

#[tokio::test]
async fn cp_into_existing_directory_does_not_gate_the_dir() {
    // `cp SRC DIR` targets DIR/SRC (a new file here), never truncates DIR
    // itself — the named directory must not be gated or snapshotted.
    let dir = tempdir();
    std::fs::write(dir.path().join("src.txt"), "data").expect("write");
    std::fs::create_dir(dir.path().join("d")).expect("mkdir");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    let r = run(&session, "cp src.txt d").await;
    assert_eq!(r.code, 0, "cp into a directory must not gate the dir: {}", r.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("d/src.txt")).unwrap(), "data");
}

#[tokio::test]
async fn mv_overwrite_under_latch_requires_confirm() {
    let dir = tempdir();
    std::fs::write(dir.path().join("src.txt"), "fresh").expect("write");
    std::fs::write(dir.path().join("dst.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    let r = run(&session, "mv src.txt dst.txt").await;
    assert_eq!(r.code, 2, "mv onto an existing file gates: {}", r.err);
    assert!(approval_str(&r, "hint").contains("--confirm="));
    assert_eq!(std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(), "old");
    assert!(dir.path().join("src.txt").exists(), "src must survive a gated mv");

    let token = session.approve_pending().await;
    let r2 = run(&session, &format!("mv --confirm=\"{token}\" src.txt dst.txt")).await;
    assert_eq!(r2.code, 0, "confirmed mv succeeds: {}", r2.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(), "fresh");
    assert!(!dir.path().join("src.txt").exists(), "src removed after the move");
}

#[tokio::test]
async fn mv_overwrite_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("src.txt"), "fresh").expect("write");
    std::fs::write(dir.path().join("dst.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    let r = run(&session, "mv src.txt dst.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "snapshot of the prior destination: {snaps:?}");
    assert_eq!(snaps[0].1, b"old");
    assert_eq!(std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(), "fresh");
}

#[tokio::test]
async fn dd_of_overwrite_under_latch_requires_confirm() {
    let dir = tempdir();
    std::fs::write(dir.path().join("in.bin"), "fresh").expect("write");
    std::fs::write(dir.path().join("out.bin"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o approvals").await;
    let r = run(&session, "dd if=in.bin of=out.bin").await;
    assert_eq!(r.code, 2, "dd of= onto an existing file gates: {}", r.err);
    assert!(approval_str(&r, "hint").contains("confirm="));
    assert_eq!(std::fs::read_to_string(dir.path().join("out.bin")).unwrap(), "old");

    // dd's re-run hint uses its key=value idiom: `dd ... confirm="<token>"`.
    let token = session.approve_pending().await;
    let r2 = run(&session, &format!("dd if=in.bin of=out.bin confirm=\"{token}\"")).await;
    assert_eq!(r2.code, 0, "confirmed dd succeeds: {}", r2.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("out.bin")).unwrap(), "fresh");
}

#[tokio::test]
async fn dd_of_overwrite_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("in.bin"), "fresh").expect("write");
    std::fs::write(dir.path().join("out.bin"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "set -o trash").await;
    let r = run(&session, "dd if=in.bin of=out.bin").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "snapshot of the prior of= file: {snaps:?}");
    assert_eq!(snaps[0].1, b"old");
    assert_eq!(std::fs::read_to_string(dir.path().join("out.bin")).unwrap(), "fresh");
}

// ─── GH #96: a *backgrounded* confirmation gate reaches its consumers ───────
// `rm x &` under `set -o approvals` gates in the background — exit 2 + a stored
// approval request — but the gate was invisible to every job consumer: `wait`
// reported "Failed", `jobs`/`JobInfo` had no request, `/v/jobs/{id}` had no
// approval node, and `JobStatus` mapped exit 2 to `Failed`. So a backgrounded
// gate could never be *fulfilled* — the request was unreachable. These pin
// the surfaces.

/// The capstone: background a gated `rm`, surface the request via `wait`, and
/// fulfill it with `Kernel::confirm` — the whole point of #96.
#[tokio::test]
async fn backgrounded_latch_is_reachable_and_confirmable() {
    let dir = tempdir();
    let session = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&session, "set -o approvals").await;
    let bg = run(&session, "rm precious.txt &").await;
    assert_eq!(bg.code, 0, "backgrounding itself succeeds: {}", bg.err);

    // `wait` surfaces the stored request on the control-plane field, exit 2.
    let waited = run(&session, "wait 1").await;
    assert_eq!(waited.code, 2, "a gated job waits to exit 2, not 1: {waited:?}");
    let req = waited
        .approval_request()
        .expect("wait must surface the backgrounded job's approval request");
    assert!(precious.exists(), "the gate held — file still present pre-confirm");

    // and the embedder can fulfill the backgrounded gate: grant it, then
    // replay via `Kernel::confirm`.
    session.grant(&req.id).await;
    let confirmed = session
        .kernel
        .confirm(&session.authority, &req.id)
        .await
        .expect("confirm");
    assert_eq!(confirmed.code, 0, "confirm deletes: {}", confirmed.err);
    assert!(!precious.exists(), "file removed after confirm");
}

/// GH #124 part 4: a successful `confirm` of a *backgrounded* gate retires
/// the originating job — it no longer lingers in `jobs` forever as `Gated`,
/// disconnected from the fact its gate was just fulfilled. Mirrors the
/// existing manual `kill --discard %N` path, automated.
#[tokio::test]
async fn confirm_retires_the_originating_backgrounded_job() {
    use kaish_kernel::scheduler::JobId;

    let dir = tempdir();
    let session = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&session, "set -o approvals").await;
    run(&session, "rm precious.txt &").await;
    let waited = run(&session, "wait 1").await;
    let req = waited.approval_request().expect("a backgrounded approval request");
    assert_eq!(
        req.job_id,
        Some(1),
        "the surfaced request must carry the originating job's id"
    );
    assert!(
        session.kernel.jobs().get(JobId(1)).await.is_some(),
        "job must still be tracked (Gated) before confirm"
    );

    session.grant(&req.id).await;
    let confirmed = session
        .kernel
        .confirm(&session.authority, &req.id)
        .await
        .expect("confirm");
    assert_eq!(confirmed.code, 0, "confirm deletes: {}", confirmed.err);
    assert!(!precious.exists(), "file removed after confirm");
    assert!(
        session.kernel.jobs().get(JobId(1)).await.is_none(),
        "the originating job must be retired after a successful confirm"
    );
}

/// **Inverts the old latch's reusable-nonce assumption.** Under the latch
/// this test asserted the opposite: a nonce stayed valid indefinitely, so a
/// second `confirm` on an already-fulfilled gate was a harmless no-op that
/// could, in principle, run the delete again. A grant now authorizes exactly
/// one successful settlement (spec §A.1) — a second `confirm` against the
/// same request must report the settled outcome instead of re-executing, and
/// the job it already retired stays retired.
#[tokio::test]
async fn confirm_of_an_already_settled_request_reports_the_outcome_and_does_not_reexecute() {
    use kaish_kernel::scheduler::JobId;

    let dir = tempdir();
    let session = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&session, "set -o approvals").await;
    run(&session, "rm precious.txt &").await;
    let waited = run(&session, "wait 1").await;
    let req = waited.approval_request().expect("a backgrounded approval request");

    session.grant(&req.id).await;
    let first = session
        .kernel
        .confirm(&session.authority, &req.id)
        .await
        .expect("confirm");
    assert_eq!(first.code, 0, "err: {}", first.err);
    assert!(!precious.exists(), "the file is deleted on the first confirm");
    assert!(
        session.kernel.jobs().get(JobId(1)).await.is_none(),
        "job retired after the first confirm"
    );

    // Put the file back. If the second confirm re-ran the delete, it would
    // vanish again — which is exactly what the reusable nonce did.
    std::fs::write(&precious, "keep me").expect("restore the file");

    let second = session
        .kernel
        .confirm(&session.authority, &req.id)
        .await
        .expect("confirm must return a result, not error, on a settled request");
    assert_eq!(second.code, 1, "a settled grant must not re-execute");
    assert!(
        second.err.contains("already settled"),
        "the refusal must report the settled outcome: {}",
        second.err
    );
    assert!(precious.exists(), "the file must be deleted exactly once");
}

/// `jobs` and `/v/jobs/{id}/status` name the gated state distinctly, not
/// the generic "Failed".
#[tokio::test]
async fn backgrounded_latch_shows_distinct_status() {
    let dir = tempdir();
    let session = kernel_at(dir.path());
    std::fs::write(dir.path().join("p.txt"), "x").expect("write");

    run(&session, "set -o approvals").await;
    run(&session, "rm p.txt &").await;
    run(&session, "wait 1").await; // let the background job reach the gate

    let jobs = run(&session, "jobs").await;
    assert!(jobs.text_out().contains("Gated"), "jobs shows Gated: {}", jobs.text_out());
    assert!(!jobs.text_out().contains("Failed"), "not a plain failure: {}", jobs.text_out());

    let status = run(&session, "cat /v/jobs/1/status").await;
    assert_eq!(status.text_out().trim(), "gated", "status node: {}", status.text_out());
}

/// GH #124 part 2: `jobs --json` rows carry the approval object itself for a
/// Gated job, not just the STATUS column's word — a caller can act on the
/// gate straight from the row instead of a second `/v/jobs/N/approval` read.
#[tokio::test]
async fn jobs_json_row_carries_the_approval_object() {
    let dir = tempdir();
    let session = kernel_at(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");

    run(&session, "set -o approvals").await;
    run(&session, "rm precious.txt &").await;
    run(&session, "wait 1").await; // let the background job reach the gate

    let jobs = run(&session, "jobs --json").await;
    assert_eq!(jobs.code, 0, "err: {}", jobs.err);
    let rows: serde_json::Value =
        serde_json::from_str(jobs.text_out().trim()).expect("a JSON array of job rows");
    let row = rows.as_array().and_then(|a| a.first()).expect("at least one job row");
    // GH #241: JobStatus's pinned wire spelling is lowercase (matching the
    // existing /v/jobs/N/status vocabulary), not the capitalized Display
    // string this row used to derive from via job_rows_json's hand-rolled
    // `.to_string()`.
    assert_eq!(row["status"], "gated", "row: {row}");
    assert_eq!(row["approval"]["operation"], "fs.remove", "row: {row}");
    assert!(
        !row["approval"]["id"].as_str().unwrap_or("").is_empty(),
        "row must carry a usable request id: {row}"
    );
    assert!(dir.path().join("precious.txt").exists());
}

/// `/v/jobs/{id}/approval` renders the stored approval request as JSON
/// naming the request, so a VFS consumer can read (and then confirm) it.
#[tokio::test]
async fn backgrounded_latch_vfs_node_renders_json() {
    let dir = tempdir();
    let session = kernel_at(dir.path());
    std::fs::write(dir.path().join("p.txt"), "x").expect("write");

    run(&session, "set -o approvals").await;
    run(&session, "rm p.txt &").await;
    let waited = run(&session, "wait 1").await;
    let req = waited.approval_request().expect("approval request");

    let node = run(&session, "cat /v/jobs/1/approval").await;
    assert_eq!(node.code, 0, "approval node readable: {}", node.err);
    assert!(
        node.text_out().contains(req.id.as_str()),
        "approval JSON carries the request id: {}",
        node.text_out()
    );
    // round-trips through kaish's own JSON door.
    let parsed = run(&session, "cat /v/jobs/1/approval | fromjson").await;
    assert_eq!(parsed.code, 0, "approval node is valid JSON: {}", parsed.err);
}

/// `jobs --cleanup` must not reap a gated job — its cached result holds the
/// only approval request for the gated operation, so reaping silently
/// destroys the pending confirmation (the #96 guarantee).
#[tokio::test]
async fn jobs_cleanup_keeps_latched_job() {
    let dir = tempdir();
    let session = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&session, "set -o approvals").await;
    run(&session, "rm precious.txt &").await;
    run(&session, "wait 1").await; // job reaches the gate

    let cleaned = run(&session, "jobs --cleanup").await;
    assert!(
        cleaned.text_out().contains("Kept 1 gated job(s)"),
        "cleanup says loudly that it kept the gated job: {}",
        cleaned.text_out()
    );

    let jobs = run(&session, "jobs").await;
    assert!(
        jobs.text_out().contains("Gated"),
        "cleanup must keep the gated job: {}",
        jobs.text_out()
    );

    // The gate is still fulfillable end-to-end after the cleanup pass.
    let waited = run(&session, "wait 1").await;
    let req = waited.approval_request().expect("approval request survives cleanup");
    session.grant(&req.id).await;
    let confirmed = session
        .kernel
        .confirm(&session.authority, &req.id)
        .await
        .expect("confirm");
    assert_eq!(confirmed.code, 0, "confirm deletes: {}", confirmed.err);
    assert!(!precious.exists(), "file removed after confirm");
}

/// `kill %N` on a gated job refuses instead of silently destroying the only
/// handle to the pending confirmation.
#[tokio::test]
async fn kill_refuses_latched_job() {
    let dir = tempdir();
    let session = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&session, "set -o approvals").await;
    run(&session, "rm precious.txt &").await;
    run(&session, "wait 1").await;

    let killed = run(&session, "kill %1").await;
    assert_eq!(killed.code, 1, "kill refuses a gated job: {killed:?}");
    assert!(killed.err.contains("gated"), "names the reason: {}", killed.err);
    assert!(
        killed.err.contains("--discard"),
        "points at the escape hatch: {}",
        killed.err
    );

    // Still present, still confirmable.
    let jobs = run(&session, "jobs").await;
    assert!(jobs.text_out().contains("Gated"), "job survives: {}", jobs.text_out());
    let waited = run(&session, "wait 1").await;
    let req = waited.approval_request().expect("approval request survives the refused kill");
    session.grant(&req.id).await;
    let confirmed = session
        .kernel
        .confirm(&session.authority, &req.id)
        .await
        .expect("confirm");
    assert_eq!(confirmed.code, 0, "confirm deletes: {}", confirmed.err);
    assert!(!precious.exists(), "file removed after confirm");
}

/// `kill --discard %N` is the explicit way to abandon a pending gate: loud
/// about what it destroyed, and the gated operation never runs.
#[tokio::test]
async fn kill_discard_abandons_latch_loudly() {
    let dir = tempdir();
    let session = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&session, "set -o approvals").await;
    run(&session, "rm precious.txt &").await;
    run(&session, "wait 1").await;

    let killed = run(&session, "kill --discard %1").await;
    assert_eq!(killed.code, 0, "discard succeeds: {killed:?}");
    assert!(
        killed.text_out().contains("discard"),
        "says what it did: {}",
        killed.text_out()
    );

    let jobs = run(&session, "jobs").await;
    assert!(
        !jobs.text_out().contains("Gated"),
        "job gone after discard: {}",
        jobs.text_out()
    );
    assert!(precious.exists(), "the gated rm never ran — file survives the discard");
}

/// `kill --discard` on a job that doesn't exist fails loudly, and on a
/// running (non-gated) job it degrades to a plain kill — pinned so the
/// flag never becomes a silent no-op or a silent success.
#[tokio::test]
async fn kill_discard_edge_cases() {
    let dir = tempdir();
    let session = kernel_at(dir.path());

    let missing = run(&session, "kill --discard %7").await;
    assert_eq!(missing.code, 1, "nonexistent job is loud: {missing:?}");
    assert!(missing.err.contains("not found"), "names the problem: {}", missing.err);

    // A running, non-gated job: --discard is a no-op qualifier; the job
    // is killed normally.
    run(&session, "sleep 30 &").await;
    let killed = run(&session, "kill --discard %1").await;
    assert_eq!(killed.code, 0, "plain kill still works under --discard: {killed:?}");
    let jobs = run(&session, "jobs").await;
    assert!(
        !jobs.text_out().contains("Running"),
        "job gone after kill: {}",
        jobs.text_out()
    );

    // --discard conflicts with --signal: discarding delivers nothing.
    std::fs::write(dir.path().join("p.txt"), "x").expect("write");
    run(&session, "set -o approvals").await;
    run(&session, "rm p.txt &").await;
    run(&session, "wait 2").await;
    let conflict = run(&session, "kill --discard --signal STOP %2").await;
    assert_eq!(conflict.code, 2, "--discard + --signal is a usage error: {conflict:?}");
}
