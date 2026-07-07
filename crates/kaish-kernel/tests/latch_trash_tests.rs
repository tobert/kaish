//! Kernel-routed tests for the destructive-op safety rails: the confirmation
//! latch (`set -o latch`) and trash-on-delete (`set -o trash`).
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
use kaish_kernel::trash::{TrashBackend, TrashEntry, TrashError};
use kaish_kernel::{Kernel, KernelConfig};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("latch-trash-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// Kernel with latch/trash forced OFF regardless of the developer's
/// KAISH_LATCH / KAISH_TRASH env (which `repl()` reads). Each test opts in
/// via `set -o latch` / `set -o trash` so the enable path itself is
/// kernel-routed too.
fn kernel_at(dir: &Path) -> Kernel {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_latch(false)
        .with_trash(false);
    Kernel::new(config).expect("kernel")
}

async fn run(kernel: &Kernel, script: &str) -> ExecResult {
    kernel.execute(script).await.expect("kernel execute")
}

/// Pull a string field out of a latch result's typed `.latch` request.
fn latch_data_str(result: &ExecResult, key: &str) -> String {
    let req = result
        .latch_request()
        .expect("latch exit-2 result carries a typed LatchRequest");
    match key {
        "hint" => req.hint,
        "nonce" => req.nonce,
        "command" => req.command,
        other => panic!("latch field {other:?} is not a string field"),
    }
}

// ============================================================================
// Confirmation latch — set -o latch → rm exit 2 → rm --confirm=<nonce>
// ============================================================================

#[tokio::test]
async fn latch_gates_rm_then_confirm_hint_deletes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let kernel = kernel_at(dir.path());

    let enable = run(&kernel, "set -o latch").await;
    assert_eq!(enable.code, 0, "set -o latch failed: {}", enable.err);

    // First rm: gated. Exit 2, file untouched, nonce + hint in .data.
    let gated = run(&kernel, "rm precious.txt").await;
    assert_eq!(gated.code, 2, "expected latch exit 2, err: {}", gated.err);
    assert!(
        gated.err.contains("confirmation required"),
        "latch message missing: {}",
        gated.err
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "file must survive the latch gate"
    );

    // The hint is the exact re-run command (`rm --confirm="<nonce>" precious.txt`).
    // Run it verbatim — pins that the advertised recovery command actually
    // parses and binds through the kernel.
    let hint = latch_data_str(&gated, "hint");
    let confirmed = run(&kernel, &hint).await;
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
async fn latch_json_surfaces_nonce_under_latch_key() {
    // `--json` on a gated op surfaces the confirmation request under a dedicated
    // `latch` key in the error envelope — control-plane, never folded into
    // `data`. And `.latch` survives formatting, so latch_request() still works
    // on the --json'd result.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let gated = run(&kernel, "rm precious.txt --json").await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);

    let envelope: serde_json::Value =
        serde_json::from_str(gated.text_out().trim()).expect("a JSON error envelope");
    assert_eq!(envelope["code"], 2, "envelope: {envelope}");
    assert!(envelope["error"].is_string(), "envelope: {envelope}");
    assert!(
        envelope["latch"]["nonce"].is_string(),
        "nonce must be under the `latch` key: {envelope}"
    );
    assert_eq!(envelope["latch"]["command"], "rm", "envelope: {envelope}");
    assert!(
        envelope.get("data").is_none(),
        "the latch must not be folded under `data`: {envelope}"
    );
    assert!(
        gated.latch_request().is_some(),
        "the typed latch must survive --json formatting"
    );
}

#[tokio::test]
async fn latch_survives_stdout_redirect() {
    // A stdout redirect on a latched `rm` must NOT disable the confirmation
    // gate. `apply_redirects` clears the *data-plane* `.data` (the structured
    // view of stdout) on a stdout redirect — but the latch nonce is a
    // *control-plane* signal, not stdout. Dropping it would silently turn
    // `rm precious.txt > log` into an unconfirmable delete-in-waiting: exit 2,
    // no recoverable nonce, file stranded. Regression guard for the
    // `.data`-clearing that landed with redirect-inside-`$()` support.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let gated = run(&kernel, "rm precious.txt > out.log").await;

    assert_eq!(
        gated.code, 2,
        "a redirect must not bypass the latch gate: {}",
        gated.err
    );
    assert!(
        gated.latch_request().is_some(),
        "the latch nonce must survive a stdout redirect (it is control-plane, \
         not stdout); got data: {:?}",
        gated.data
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "file must survive the latch gate even with a redirect"
    );
}

#[tokio::test]
async fn latch_captures_the_exact_invocation() {
    // The latch stamps the dispatch name + exact argv captured at the seam, so
    // an embedder can inspect precisely what `confirm` will replay.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let gated = run(&kernel, "rm precious.txt").await;
    let req = gated.latch_request().expect("a latch request");

    assert_eq!(req.tool, "rm", "dispatch name should be the argv0 for replay");
    assert!(
        req.argv.iter().any(|a| a == "precious.txt"),
        "captured argv must contain the operand: {:?}",
        req.argv
    );
}

#[tokio::test]
async fn confirm_replays_rm_and_deletes() {
    // The whole point: inspect the latch, then fulfill it by replaying the exact
    // captured invocation — no hint string, no manual argv reconstruction.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let gated = run(&kernel, "rm precious.txt").await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);
    let req = gated.latch_request().expect("a latch request");

    let done = kernel.confirm(&req).await.expect("confirm executes");
    assert_eq!(done.code, 0, "confirm should succeed: {}", done.err);
    assert!(
        !dir.path().join("precious.txt").exists(),
        "confirm should have deleted the file"
    );
}

#[tokio::test]
async fn confirm_replays_a_path_with_spaces_the_hint_cannot() {
    // The payoff of capturing argv over the hint string: a path with a space
    // round-trips exactly through `confirm` (execute_argv, no re-parse), whereas
    // the hint (`rm --confirm="N" a b.txt`, unquoted) would re-parse as two
    // paths. This is why argv capture matters.
    let dir = tempdir();
    std::fs::write(dir.path().join("a b.txt"), "data").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let gated = run(&kernel, r#"rm "a b.txt""#).await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);
    let req = gated.latch_request().expect("a latch request");
    assert!(
        req.argv.iter().any(|a| a == "a b.txt"),
        "the space-bearing path must survive as one argv token: {:?}",
        req.argv
    );

    let done = kernel.confirm(&req).await.expect("confirm executes");
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    let gated = run(&kernel, "cp src.txt dst.txt").await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);
    let req = gated.latch_request().expect("a latch request");
    assert_eq!(req.tool, "cp");

    let done = kernel.confirm(&req).await.expect("confirm executes");
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
    // `set -o latch` needed), and its dispatch name ("kaish-trash") differs from
    // its display command ("kaish-trash empty"). Two things to prove: the seam
    // captures the argv even with latch off (so `confirm` has tool/argv), and
    // replaying `execute_argv("kaish-trash", ["--confirm=N", "--", "empty"])`
    // recomputes the same command scope the nonce was issued under, so it
    // validates.
    let dir = tempdir();
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    let gated = run(&kernel, "kaish-trash empty").await;
    assert_eq!(gated.code, 2, "empty gates unconditionally: {}", gated.err);
    let req = gated.latch_request().expect("a latch request");
    assert_eq!(req.tool, "kaish-trash", "dispatch name for replay");
    assert!(
        req.argv.iter().any(|a| a == "empty"),
        "the subcommand token must be captured in argv: {:?}",
        req.argv
    );

    let done = kernel.confirm(&req).await.expect("confirm executes");
    assert_eq!(done.code, 0, "confirm should empty the trash: {}", done.err);
}

#[tokio::test]
async fn confirm_without_captured_invocation_errors() {
    // A latch with no captured argv (produced outside a dispatch seam) can't be
    // replayed — `confirm` fails loud (exit 2) rather than silently no-op.
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    let bare = kaish_kernel::interpreter::LatchRequest {
        nonce: "deadbeef".to_string(),
        command: "rm".to_string(),
        paths: vec!["x".to_string()],
        hint: "rm --confirm=deadbeef x".to_string(),
        tool: String::new(),
        argv: vec![],
        ttl: 60,
        job_id: None,
    };
    let r = kernel.confirm(&bare).await.expect("confirm returns");
    assert_eq!(r.code, 2, "must not silently succeed: {r:?}");
    assert!(r.err.contains("no captured invocation"), "err: {}", r.err);
}

#[tokio::test]
async fn latch_bogus_nonce_fails_and_file_survives() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "rm --confirm=\"deadbeef\" precious.txt").await;
    assert_eq!(r.code, 1, "bogus nonce must fail, out: {}", r.text_out());
    assert!(
        r.err.contains("rm:"),
        "expected an rm error naming the bad nonce, got: {}",
        r.err
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "file must survive a rejected nonce"
    );
}

#[tokio::test]
async fn latch_batches_multiple_paths_under_one_nonce() {
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "a").expect("write");
    std::fs::write(dir.path().join("b.txt"), "b").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let gated = run(&kernel, "rm a.txt b.txt").await;
    assert_eq!(gated.code, 2, "err: {}", gated.err);
    assert!(
        gated.err.contains("a.txt") && gated.err.contains("b.txt"),
        "latch message should authorize both paths: {}",
        gated.err
    );

    let hint = latch_data_str(&gated, "hint");
    let confirmed = run(&kernel, &hint).await;
    assert_eq!(confirmed.code, 0, "err: {}", confirmed.err);
    assert!(!dir.path().join("a.txt").exists(), "a.txt should be gone");
    assert!(!dir.path().join("b.txt").exists(), "b.txt should be gone");
}

#[tokio::test]
async fn latch_off_by_default_rm_deletes_directly() {
    let dir = tempdir();
    std::fs::write(dir.path().join("plain.txt"), "x").expect("write");
    let kernel = kernel_at(dir.path());

    let r = run(&kernel, "rm plain.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    assert!(!dir.path().join("plain.txt").exists());
}

#[tokio::test]
async fn latch_in_a_pipeline_stage_overrides_later_success() {
    // GH #125: a confirmation gate raised by an EARLIER pipeline stage must
    // survive a later stage's nominal success. `rm x | echo done` used to exit
    // 0 with the latch dropped (only the last stage's result survived), so an
    // agent gating on exit codes saw success while `rm` never ran. The gate is a
    // control-plane fact about the whole pipeline: exit 2, `.latch` present, and
    // the file untouched.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let piped = run(&kernel, "rm precious.txt | echo done").await;

    assert_eq!(
        piped.code, 2,
        "an earlier stage's gate must set the pipeline exit code, not the last \
         stage's 0: {}",
        piped.err
    );
    let req = piped
        .latch_request()
        .expect("the earlier stage's latch must ride the pipeline result");
    assert_eq!(req.command, "rm", "the gated command is rm, not echo");
    assert!(!req.nonce.is_empty(), "a usable nonce must be present");
    assert!(
        dir.path().join("precious.txt").exists(),
        "the gated file must survive — the gate held, the op never ran"
    );
}

#[tokio::test]
async fn latch_first_stage_wins_when_two_stages_gate() {
    // Two gated stages in one pipeline: first latch wins (matches wait.rs
    // classify()'s first-latch-wins). `rm a | rm b` under latch surfaces a's
    // gate, and BOTH files survive (each stage gated independently).
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "a").expect("write");
    std::fs::write(dir.path().join("b.txt"), "b").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let piped = run(&kernel, "rm a.txt | rm b.txt").await;

    assert_eq!(piped.code, 2, "err: {}", piped.err);
    let req = piped.latch_request().expect("a latch must ride the result");
    // First stage's gate authorizes a.txt; the surfaced nonce must be usable to
    // confirm it (the FIRST gate, not the last stage's).
    let confirm_a = run(&kernel, &format!("rm --confirm={} a.txt", req.nonce)).await;
    assert_eq!(
        confirm_a.code, 0,
        "the surfaced nonce must confirm the FIRST stage's path: {}",
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

fn kernel_with_trash(dir: &Path, mock: &Arc<MockTrash>) -> Kernel {
    let mut kernel = kernel_at(dir);
    kernel.set_trash_backend(Some(Arc::clone(mock) as Arc<dyn TrashBackend>));
    kernel
}

#[tokio::test]
async fn trash_small_file_delegates_to_backend() {
    let dir = tempdir();
    std::fs::write(dir.path().join("keep.txt"), "data").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, "rm keep.txt").await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    // Directories always trash (no -r needed; trash moves them atomically).
    let r = run(&kernel, "rm sub").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    let trashed = mock.trashed_paths();
    assert_eq!(trashed.len(), 1, "one trash call for the dir: {trashed:?}");
    assert!(trashed[0].ends_with("sub"));
}

#[tokio::test]
async fn trash_catches_small_file_even_with_latch_enabled() {
    let dir = tempdir();
    std::fs::write(dir.path().join("small.txt"), "tiny").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    run(&kernel, "set -o trash").await;
    // Priority: trash catches small files before the latch gates them —
    // no exit 2, no nonce, straight to the backend.
    let r = run(&kernel, "rm small.txt").await;
    assert_eq!(r.code, 0, "trash should win over latch, err: {}", r.err);
    assert_eq!(mock.trashed_paths().len(), 1);
}

#[tokio::test]
async fn trash_failure_is_loud_and_never_falls_through_to_delete() {
    let dir = tempdir();
    std::fs::write(dir.path().join("guarded.txt"), "data").expect("write");
    let mock = Arc::new(MockTrash::failing());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, "rm guarded.txt").await;
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
    let mut kernel = kernel_at(dir.path());
    kernel.set_trash_backend(None);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, "rm orphan.txt").await;
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
// Write-model gate: tee overwrites honor latch + trash (like rm gates deletes)
// ============================================================================

/// In-memory kernel (`/v` mounts have no real path) wired to the mock trash —
/// for exercising the overlay `trash_bytes` snapshot path.
fn isolated_kernel_with_trash(mock: &Arc<MockTrash>) -> Kernel {
    let mut kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    kernel.set_trash_backend(Some(Arc::clone(mock) as Arc<dyn TrashBackend>));
    kernel
}

#[tokio::test]
async fn tee_overwrite_under_trash_snapshots_prior_bytes_first() {
    let dir = tempdir();
    std::fs::write(dir.path().join("doc.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, "echo new | tee doc.txt").await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    // latch on, trash off: the overwrite must be confirmed.
    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "echo new | tee doc.txt").await;
    assert_eq!(r.code, 2, "latch gates the overwrite: {}", r.err);
    assert!(r.err.contains("--confirm="));
    assert_eq!(
        std::fs::read_to_string(dir.path().join("doc.txt")).expect("read"),
        "keep",
        "the file must be untouched until confirmed"
    );

    // Re-run with the issued nonce.
    let nonce = latch_data_str(&r, "nonce");
    let r2 = run(&kernel, &format!("echo new | tee --confirm=\"{nonce}\" doc.txt")).await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    // New file: nothing to lose, no gate.
    let r = run(&kernel, "echo hi | tee fresh.txt").await;
    assert_eq!(r.code, 0, "new file should not gate: {}", r.err);
    // Append: doesn't destroy prior content, no gate.
    let r2 = run(&kernel, "echo line2 | tee -a log.txt").await;
    assert_eq!(r2.code, 0, "append should not gate: {}", r2.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("log.txt")).expect("read"),
        "line1\nline2\n"
    );
}

#[tokio::test]
async fn tee_overlay_overwrite_snapshots_bytes_via_trash_bytes() {
    let mock = Arc::new(MockTrash::default());
    let kernel = isolated_kernel_with_trash(&mock);

    run(&kernel, "set -o trash").await;
    run(&kernel, "write /v/f.txt \"original\"").await; // seed an in-memory file
    let r = run(&kernel, "echo new | tee /v/f.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);

    // No real path → prior content captured via trash_bytes, not trash().
    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "one byte-snapshot for the overlay overwrite: {snaps:?}");
    assert!(snaps[0].0.ends_with("f.txt"));
    assert_eq!(snaps[0].1, b"original", "the captured bytes are the prior content");
    assert!(mock.trashed_paths().is_empty(), "no real-path trash for an overlay file");

    let out = run(&kernel, "cat /v/f.txt").await;
    assert_eq!(out.text_out().trim(), "new", "new content written after the snapshot");
}

// ============================================================================
// Write-model gate: patch overwrites honor latch + trash (same gate as tee)
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, PATCH_SCRIPT).await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, PATCH_SCRIPT).await;
    assert_eq!(r.code, 2, "latch gates the patch: {}", r.err);
    assert!(r.err.contains("--confirm="));
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "old\n",
        "the file must be untouched until confirmed"
    );

    let nonce = latch_data_str(&r, "nonce");
    let confirmed = PATCH_SCRIPT.replace("patch f.txt", &format!("patch --confirm=\"{nonce}\" f.txt"));
    let r2 = run(&kernel, &confirmed).await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    let script = "patch f.txt <<'EOF'\n\
        --- a/x\n+++ b/x\n@@ -1 +1 @@\n-alpha\n+ALPHA\n\
        --- a/y\n+++ b/y\n@@ -2 +2 @@\n-beta\n+BETA\n\
        EOF\n";

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, script).await;
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
    // The latch prompt must not list the same explicit target once per group.
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "alpha\nbeta\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    let script = "patch f.txt <<'EOF'\n\
        --- a/x\n+++ b/x\n@@ -1 +1 @@\n-alpha\n+ALPHA\n\
        --- a/y\n+++ b/y\n@@ -2 +2 @@\n-beta\n+BETA\n\
        EOF\n";

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, script).await;
    assert_eq!(r.code, 2, "latch gates: {}", r.err);
    // The path legitimately appears twice (the "Authorized:" line and the hint),
    // but a dedup miss would repeat it within each: "f.txt, f.txt" / "f.txt f.txt".
    assert!(
        !r.err.contains("f.txt, f.txt") && !r.err.contains("f.txt f.txt"),
        "the deduped target must not repeat within the prompt: {}",
        r.err
    );
}

#[tokio::test]
async fn patch_dry_run_does_not_gate() {
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "old\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    let dry = PATCH_SCRIPT.replace("patch f.txt", "patch --dry-run f.txt");
    let r = run(&kernel, &dry).await;
    assert_eq!(r.code, 0, "dry-run never writes, so it never gates: {}", r.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "old\n",
        "dry-run leaves the file untouched"
    );
}

// ============================================================================
// Write-model gate: sed -i in-place edits honor latch + trash (same gate)
// ============================================================================

#[tokio::test]
async fn sed_in_place_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "old\n").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, "sed -i 's/old/new/' f.txt").await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "sed -i 's/old/new/' f.txt").await;
    assert_eq!(r.code, 2, "latch gates the in-place edit: {}", r.err);
    assert!(r.err.contains("--confirm="));
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "old\n",
        "the file must be untouched until confirmed"
    );

    let nonce = latch_data_str(&r, "nonce");
    let r2 = run(&kernel, &format!("sed -i --confirm=\"{nonce}\" 's/old/new/' f.txt")).await;
    assert_eq!(r2.code, 0, "confirmed in-place edit succeeds: {}", r2.err);
    assert_eq!(
        std::fs::read_to_string(dir.path().join("f.txt")).expect("read"),
        "new\n"
    );
}

#[tokio::test]
async fn sed_in_place_latch_hint_is_runnable() {
    // Regression: the hint must reinject `-i` and the expression. A bare
    // `sed --confirm=… f.txt` would read f.txt as the expression and hang on
    // stdin. Run the advertised hint verbatim and require it to edit the file.
    let dir = tempdir();
    std::fs::write(dir.path().join("f.txt"), "old\n").expect("write");
    let kernel = kernel_at(dir.path());

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "sed -i 's/old/new/' f.txt").await;
    assert_eq!(r.code, 2, "latch gates: {}", r.err);

    let hint = latch_data_str(&r, "hint");
    assert!(hint.contains("-i"), "hint keeps -i: {hint}");
    assert!(hint.contains("s/old/new/"), "hint keeps the expression: {hint}");

    let r2 = run(&kernel, &hint).await;
    assert_eq!(r2.code, 0, "running the hint verbatim edits the file: {}", r2.err);
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
    let kernel = kernel_at(dir.path());

    // -i alongside -e, across multiple file operands (gates off by default).
    let r = run(&kernel, "sed -i -e 's/old/new/' a.txt b.txt").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("a.txt")).expect("read"), "new\n");
    assert_eq!(std::fs::read_to_string(dir.path().join("b.txt")).expect("read"), "new\n");
}

#[tokio::test]
async fn sed_in_place_continues_past_per_file_error() {
    let dir = tempdir();
    std::fs::write(dir.path().join("good.txt"), "old\n").expect("write");
    let kernel = kernel_at(dir.path());

    // A missing operand must not abort the batch: the good file is still edited
    // and the run reports failure for the missing one.
    let r = run(&kernel, "sed -i 's/old/new/' missing.txt good.txt").await;
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
    let kernel = kernel_at(dir.path());
    // Editing a stream in place is meaningless — loud error, not a fall-through.
    let r = run(&kernel, "echo hi | sed -i 's/h/H/'").await;
    assert_eq!(r.code, 1, "no file operands must error: {}", r.err);
    assert!(r.err.contains("requires file operands"), "err: {}", r.err);
}

// ============================================================================
// Write-model gate: write / dd / cp / mv overwrites honor latch + trash too
// (the same gate as tee/patch/sed -i). These builtins previously bypassed it.
// ============================================================================

#[tokio::test]
async fn write_overwrite_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("doc.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, "write doc.txt \"new\"").await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "write doc.txt \"new\"").await;
    assert_eq!(r.code, 2, "latch gates write: {}", r.err);
    assert!(r.err.contains("--confirm="));
    assert_eq!(std::fs::read_to_string(dir.path().join("doc.txt")).unwrap(), "keep");

    let nonce = latch_data_str(&r, "nonce");
    let r2 = run(&kernel, &format!("write --confirm=\"{nonce}\" doc.txt \"new\"")).await;
    assert_eq!(r2.code, 0, "confirmed write succeeds: {}", r2.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("doc.txt")).unwrap(), "new");
}

#[tokio::test]
async fn write_new_file_does_not_gate() {
    let dir = tempdir();
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);
    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "write fresh.txt \"hi\"").await;
    assert_eq!(r.code, 0, "a new file has nothing to lose, no gate: {}", r.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("fresh.txt")).unwrap(), "hi");
}

#[tokio::test]
async fn overwrite_too_big_for_trash_falls_through_to_latch() {
    let dir = tempdir();
    std::fs::write(dir.path().join("big.txt"), "0123456789").expect("write"); // 10 bytes
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "kaish-trash config max-size 2").await; // 2-byte cap
    run(&kernel, "set -o trash").await;
    run(&kernel, "set -o latch").await;
    // 10 bytes > the 2-byte cap: can't snapshot, so trash is skipped and latch
    // gates instead — mirroring rm's too-big-to-trash fall-through (#3).
    let r = run(&kernel, "write big.txt \"new\"").await;
    assert_eq!(r.code, 2, "a file too big to trash should latch: {}", r.err);
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "cp src.txt dst.txt").await;
    assert_eq!(r.code, 2, "cp onto an existing file gates: {}", r.err);
    assert!(r.err.contains("--confirm="));
    assert_eq!(std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(), "old");

    let nonce = latch_data_str(&r, "nonce");
    let r2 = run(&kernel, &format!("cp --confirm=\"{nonce}\" src.txt dst.txt")).await;
    assert_eq!(r2.code, 0, "confirmed cp succeeds: {}", r2.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(), "fresh");
}

#[tokio::test]
async fn cp_overwrite_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("src.txt"), "fresh").expect("write");
    std::fs::write(dir.path().join("dst.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, "cp src.txt dst.txt").await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "cp src.txt d").await;
    assert_eq!(r.code, 0, "cp into a directory must not gate the dir: {}", r.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("d/src.txt")).unwrap(), "data");
}

#[tokio::test]
async fn mv_overwrite_under_latch_requires_confirm() {
    let dir = tempdir();
    std::fs::write(dir.path().join("src.txt"), "fresh").expect("write");
    std::fs::write(dir.path().join("dst.txt"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "mv src.txt dst.txt").await;
    assert_eq!(r.code, 2, "mv onto an existing file gates: {}", r.err);
    assert!(r.err.contains("--confirm="));
    assert_eq!(std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(), "old");
    assert!(dir.path().join("src.txt").exists(), "src must survive a gated mv");

    let nonce = latch_data_str(&r, "nonce");
    let r2 = run(&kernel, &format!("mv --confirm=\"{nonce}\" src.txt dst.txt")).await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, "mv src.txt dst.txt").await;
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
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o latch").await;
    let r = run(&kernel, "dd if=in.bin of=out.bin").await;
    assert_eq!(r.code, 2, "dd of= onto an existing file gates: {}", r.err);
    assert!(r.err.contains("confirm="));
    assert_eq!(std::fs::read_to_string(dir.path().join("out.bin")).unwrap(), "old");

    // dd's latch hint uses its key=value idiom: `dd ... confirm="<nonce>"`.
    let nonce = latch_data_str(&r, "nonce");
    let r2 = run(&kernel, &format!("dd if=in.bin of=out.bin confirm=\"{nonce}\"")).await;
    assert_eq!(r2.code, 0, "confirmed dd succeeds: {}", r2.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("out.bin")).unwrap(), "fresh");
}

#[tokio::test]
async fn dd_of_overwrite_under_trash_snapshots_prior_bytes() {
    let dir = tempdir();
    std::fs::write(dir.path().join("in.bin"), "fresh").expect("write");
    std::fs::write(dir.path().join("out.bin"), "old").expect("write");
    let mock = Arc::new(MockTrash::default());
    let kernel = kernel_with_trash(dir.path(), &mock);

    run(&kernel, "set -o trash").await;
    let r = run(&kernel, "dd if=in.bin of=out.bin").await;
    assert_eq!(r.code, 0, "err: {}", r.err);
    let snaps = mock.snapshots();
    assert_eq!(snaps.len(), 1, "snapshot of the prior of= file: {snaps:?}");
    assert_eq!(snaps[0].1, b"old");
    assert_eq!(std::fs::read_to_string(dir.path().join("out.bin")).unwrap(), "fresh");
}

// ─── GH #96: a *backgrounded* confirmation latch reaches its consumers ───────
// `rm x &` under `set -o latch` gates in the background — exit 2 + a stored
// LatchRequest — but the latch was invisible to every job consumer: `wait`
// reported "Failed", `jobs`/`JobInfo` had no latch, `/v/jobs/{id}` had no latch
// node, and `JobStatus` mapped exit 2 to `Failed`. So a backgrounded gate could
// never be *fulfilled* — the nonce was unreachable. These pin the surfaces.

/// The capstone: background a gated `rm`, surface the latch via `wait`, and
/// fulfill it with `Kernel::confirm` — the whole point of #96.
#[tokio::test]
async fn backgrounded_latch_is_reachable_and_confirmable() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&kernel, "set -o latch").await;
    let bg = run(&kernel, "rm precious.txt &").await;
    assert_eq!(bg.code, 0, "backgrounding itself succeeds: {}", bg.err);

    // `wait` surfaces the stored latch on the control-plane field, exit 2.
    let waited = run(&kernel, "wait 1").await;
    assert_eq!(waited.code, 2, "a latched job waits to exit 2, not 1: {waited:?}");
    let latch = waited
        .latch_request()
        .expect("wait must surface the backgrounded job's LatchRequest");
    assert!(precious.exists(), "the gate held — file still present pre-confirm");

    // and the embedder can fulfill the backgrounded gate.
    let confirmed = kernel.confirm(&latch).await.expect("confirm");
    assert_eq!(confirmed.code, 0, "confirm deletes: {}", confirmed.err);
    assert!(!precious.exists(), "file removed after confirm");
}

/// GH #124 part 4: a successful `confirm` of a *backgrounded* latch retires
/// the originating job — it no longer lingers in `jobs` forever as `Latched`,
/// disconnected from the fact its gate was just fulfilled. Mirrors the
/// existing manual `kill --discard %N` path, automated.
#[tokio::test]
async fn confirm_retires_the_originating_backgrounded_job() {
    use kaish_kernel::scheduler::JobId;

    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&kernel, "set -o latch").await;
    run(&kernel, "rm precious.txt &").await;
    let waited = run(&kernel, "wait 1").await;
    let latch = waited.latch_request().expect("a backgrounded LatchRequest");
    assert_eq!(
        latch.job_id,
        Some(1),
        "the surfaced latch must carry the originating job's id"
    );
    assert!(
        kernel.jobs().get(JobId(1)).await.is_some(),
        "job must still be tracked (Latched) before confirm"
    );

    let confirmed = kernel.confirm(&latch).await.expect("confirm");
    assert_eq!(confirmed.code, 0, "confirm deletes: {}", confirmed.err);
    assert!(!precious.exists(), "file removed after confirm");
    assert!(
        kernel.jobs().get(JobId(1)).await.is_none(),
        "the originating job must be retired after a successful confirm"
    );
}

/// A repeat `confirm` of an already-retired job is a safe no-op — nonces are
/// reusable within TTL (not consumed on validate), so the second replay still
/// succeeds (idempotent delete-of-already-deleted via rm's own semantics is
/// out of scope here; what matters is `confirm` itself doesn't panic/error
/// trying to retire a job that's already gone).
#[tokio::test]
async fn confirm_of_an_already_retired_job_does_not_error() {
    use kaish_kernel::scheduler::JobId;

    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");

    run(&kernel, "set -o latch").await;
    run(&kernel, "rm precious.txt &").await;
    let waited = run(&kernel, "wait 1").await;
    let latch = waited.latch_request().expect("a backgrounded LatchRequest");

    let first = kernel.confirm(&latch).await.expect("confirm");
    assert_eq!(first.code, 0, "err: {}", first.err);
    assert!(kernel.jobs().get(JobId(1)).await.is_none(), "job retired");

    // Second confirm with the same (still-valid, reusable) nonce: rm's own
    // idempotent-retry story applies to the replay itself; the job-retirement
    // step must not error just because the job is already gone.
    let second = kernel.confirm(&latch).await;
    assert!(second.is_ok(), "confirm must not error on an already-retired job");
}

/// `jobs` and `/v/jobs/{id}/status` name the latched state distinctly, not
/// the generic "Failed".
#[tokio::test]
async fn backgrounded_latch_shows_distinct_status() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    std::fs::write(dir.path().join("p.txt"), "x").expect("write");

    run(&kernel, "set -o latch").await;
    run(&kernel, "rm p.txt &").await;
    run(&kernel, "wait 1").await; // let the background job reach the gate

    let jobs = run(&kernel, "jobs").await;
    assert!(jobs.text_out().contains("Latched"), "jobs shows Latched: {}", jobs.text_out());
    assert!(!jobs.text_out().contains("Failed"), "not a plain failure: {}", jobs.text_out());

    let status = run(&kernel, "cat /v/jobs/1/status").await;
    assert_eq!(status.text_out().trim(), "latched", "status node: {}", status.text_out());
}

/// GH #124 part 2: `jobs --json` rows carry the latch object itself for a
/// Latched job, not just the STATUS column's word — a caller can act on the
/// gate straight from the row instead of a second `/v/jobs/N/latch` read.
#[tokio::test]
async fn jobs_json_row_carries_the_latch_object() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    std::fs::write(dir.path().join("precious.txt"), "keep me").expect("write");

    run(&kernel, "set -o latch").await;
    run(&kernel, "rm precious.txt &").await;
    run(&kernel, "wait 1").await; // let the background job reach the gate

    let jobs = run(&kernel, "jobs --json").await;
    assert_eq!(jobs.code, 0, "err: {}", jobs.err);
    let rows: serde_json::Value =
        serde_json::from_str(jobs.text_out().trim()).expect("a JSON array of job rows");
    let row = rows.as_array().and_then(|a| a.first()).expect("at least one job row");
    assert_eq!(row["status"], "Latched", "row: {row}");
    assert_eq!(row["latch"]["command"], "rm", "row: {row}");
    assert!(
        !row["latch"]["nonce"].as_str().unwrap_or("").is_empty(),
        "row must carry a usable nonce: {row}"
    );
    assert!(dir.path().join("precious.txt").exists());
}

/// `/v/jobs/{id}/latch` renders the stored LatchRequest as JSON carrying the
/// nonce, so a VFS consumer can read (and then confirm) it.
#[tokio::test]
async fn backgrounded_latch_vfs_node_renders_json() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    std::fs::write(dir.path().join("p.txt"), "x").expect("write");

    run(&kernel, "set -o latch").await;
    run(&kernel, "rm p.txt &").await;
    let waited = run(&kernel, "wait 1").await;
    let nonce = waited.latch_request().expect("latch").nonce;

    let node = run(&kernel, "cat /v/jobs/1/latch").await;
    assert_eq!(node.code, 0, "latch node readable: {}", node.err);
    assert!(node.text_out().contains(&nonce), "latch JSON carries the nonce: {}", node.text_out());
    // round-trips through kaish's own JSON door.
    let parsed = run(&kernel, "cat /v/jobs/1/latch | fromjson").await;
    assert_eq!(parsed.code, 0, "latch node is valid JSON: {}", parsed.err);
}

/// `jobs --cleanup` must not reap a latched job — its cached result holds the
/// only LatchRequest for the gated operation, so reaping silently destroys the
/// pending confirmation (the #96 guarantee).
#[tokio::test]
async fn jobs_cleanup_keeps_latched_job() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&kernel, "set -o latch").await;
    run(&kernel, "rm precious.txt &").await;
    run(&kernel, "wait 1").await; // job reaches the gate

    let cleaned = run(&kernel, "jobs --cleanup").await;
    assert!(
        cleaned.text_out().contains("Kept 1 latched job(s)"),
        "cleanup says loudly that it kept the latched job: {}",
        cleaned.text_out()
    );

    let jobs = run(&kernel, "jobs").await;
    assert!(
        jobs.text_out().contains("Latched"),
        "cleanup must keep the latched job: {}",
        jobs.text_out()
    );

    // The gate is still fulfillable end-to-end after the cleanup pass.
    let waited = run(&kernel, "wait 1").await;
    let latch = waited.latch_request().expect("latch survives cleanup");
    let confirmed = kernel.confirm(&latch).await.expect("confirm");
    assert_eq!(confirmed.code, 0, "confirm deletes: {}", confirmed.err);
    assert!(!precious.exists(), "file removed after confirm");
}

/// `kill %N` on a latched job refuses instead of silently destroying the only
/// handle to the pending confirmation.
#[tokio::test]
async fn kill_refuses_latched_job() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&kernel, "set -o latch").await;
    run(&kernel, "rm precious.txt &").await;
    run(&kernel, "wait 1").await;

    let killed = run(&kernel, "kill %1").await;
    assert_eq!(killed.code, 1, "kill refuses a latched job: {killed:?}");
    assert!(killed.err.contains("latched"), "names the reason: {}", killed.err);
    assert!(
        killed.err.contains("--discard"),
        "points at the escape hatch: {}",
        killed.err
    );

    // Still present, still confirmable.
    let jobs = run(&kernel, "jobs").await;
    assert!(jobs.text_out().contains("Latched"), "job survives: {}", jobs.text_out());
    let waited = run(&kernel, "wait 1").await;
    let latch = waited.latch_request().expect("latch survives the refused kill");
    let confirmed = kernel.confirm(&latch).await.expect("confirm");
    assert_eq!(confirmed.code, 0, "confirm deletes: {}", confirmed.err);
    assert!(!precious.exists(), "file removed after confirm");
}

/// `kill --discard %N` is the explicit way to abandon a pending gate: loud
/// about what it destroyed, and the gated operation never runs.
#[tokio::test]
async fn kill_discard_abandons_latch_loudly() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    let precious = dir.path().join("precious.txt");
    std::fs::write(&precious, "keep me").expect("write");

    run(&kernel, "set -o latch").await;
    run(&kernel, "rm precious.txt &").await;
    run(&kernel, "wait 1").await;

    let killed = run(&kernel, "kill --discard %1").await;
    assert_eq!(killed.code, 0, "discard succeeds: {killed:?}");
    assert!(
        killed.text_out().contains("discard"),
        "says what it did: {}",
        killed.text_out()
    );

    let jobs = run(&kernel, "jobs").await;
    assert!(
        !jobs.text_out().contains("Latched"),
        "job gone after discard: {}",
        jobs.text_out()
    );
    assert!(precious.exists(), "the gated rm never ran — file survives the discard");
}

/// `kill --discard` on a job that doesn't exist fails loudly, and on a
/// running (non-latched) job it degrades to a plain kill — pinned so the
/// flag never becomes a silent no-op or a silent success.
#[tokio::test]
async fn kill_discard_edge_cases() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());

    let missing = run(&kernel, "kill --discard %7").await;
    assert_eq!(missing.code, 1, "nonexistent job is loud: {missing:?}");
    assert!(missing.err.contains("not found"), "names the problem: {}", missing.err);

    // A running, non-latched job: --discard is a no-op qualifier; the job
    // is killed normally.
    run(&kernel, "sleep 30 &").await;
    let killed = run(&kernel, "kill --discard %1").await;
    assert_eq!(killed.code, 0, "plain kill still works under --discard: {killed:?}");
    let jobs = run(&kernel, "jobs").await;
    assert!(
        !jobs.text_out().contains("Running"),
        "job gone after kill: {}",
        jobs.text_out()
    );

    // --discard conflicts with --signal: discarding delivers nothing.
    std::fs::write(dir.path().join("p.txt"), "x").expect("write");
    run(&kernel, "set -o latch").await;
    run(&kernel, "rm p.txt &").await;
    run(&kernel, "wait 2").await;
    let conflict = run(&kernel, "kill --discard --signal STOP %2").await;
    assert_eq!(conflict.code, 2, "--discard + --signal is a usage error: {conflict:?}");
}
