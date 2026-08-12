//! Trash-on-delete and trash-on-overwrite (`set -o trash`).
//!
//! Every destructive builtin that can clobber an existing file snapshots
//! the prior content into the trash first, so the write is recoverable. A
//! trash failure is loud and never falls through to the destructive write.

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
        .prefix("approval-trash-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// One test's kernel.
struct Session {
    kernel: Kernel,
}

/// Kernel with trash forced OFF regardless of the developer's KAISH_TRASH env
/// (which `repl()` reads). Each test opts in via `set -o trash`, so the enable
/// path itself is kernel-routed too.
fn kernel_at(dir: &Path) -> Session {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_trash(false);
    Session { kernel: Kernel::new(config).expect("kernel") }
}

async fn run(session: &Session, script: &str) -> ExecResult {
    session.kernel.execute(script).await.expect("kernel execute")
}

// ============================================================================
// The fs.* enforce policy (`set -o approvals`) — rm exit 2 → grant → confirm
// ============================================================================

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
async fn trash_catches_a_small_file() {
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
    let kernel = Kernel::new(KernelConfig::isolated()).expect("kernel");
    let mut session = Session { kernel };
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
async fn tee_new_file_and_append_are_not_overwrites() {
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
async fn patch_dry_run_snapshots_nothing() {
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
async fn write_new_file_is_not_an_overwrite() {
    let dir = tempdir();
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);
    run(&session, "set -o approvals").await;
    let r = run(&session, "write fresh.txt \"hi\"").await;
    assert_eq!(r.code, 0, "a new file has nothing to lose, no gate: {}", r.err);
    assert_eq!(std::fs::read_to_string(dir.path().join("fresh.txt")).unwrap(), "hi");
}

#[tokio::test]
async fn overwrite_too_big_for_trash_is_written_directly() {
    let dir = tempdir();
    std::fs::write(dir.path().join("big.txt"), "0123456789").expect("write"); // 10 bytes
    let mock = Arc::new(MockTrash::default());
    let session = kernel_with_trash(dir.path(), &mock);

    run(&session, "kaish-trash config max-size 2").await; // 2-byte cap
    run(&session, "set -o trash").await;
    // 10 bytes > the 2-byte cap: the prior content cannot be snapshotted, so
    // trash is skipped and the overwrite proceeds unbacked. Nothing in the
    // kernel holds it — mirroring rm's too-big-to-trash fall-through. An
    // embedder that wants to refuse this reads the plan before running it.
    let r = run(&session, "write big.txt \"new\"").await;
    assert_eq!(r.code, 0, "an oversize overwrite is not held: {}", r.err);
    assert!(mock.snapshots().is_empty(), "no snapshot when over the cap");
    assert_eq!(
        std::fs::read_to_string(dir.path().join("big.txt")).unwrap(),
        "new",
        "the overwrite lands, with no recoverable prior copy"
    );
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
async fn cp_into_existing_directory_does_not_snapshot_the_dir() {
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

/// `kaish-trash empty` always asks, and the flag it names actually works.
///
/// Routed through `kernel.execute` on purpose: an earlier version of this
/// test built `ToolArgs` by hand and passed because it never touched clap —
/// while the real `--confirm` was declared as `Option<String>` and rejected
/// the valueless spelling the refusal message told the user to run. The
/// message and the flag have to agree, and only the dispatch chain proves it.
#[tokio::test]
async fn trash_empty_asks_and_the_confirm_flag_it_names_is_accepted() {
    let dir = tempdir();
    let session = kernel_at(dir.path());

    let refused = run(&session, "kaish-trash empty").await;
    assert_eq!(refused.code, 2, "a bare empty must refuse: {refused:?}");
    assert!(refused.err.contains("irreversible"), "{}", refused.err);
    assert!(
        refused.err.contains("--confirm"),
        "the refusal must name the flag: {}",
        refused.err
    );

    // Exactly the spelling the message just advertised.
    let confirmed = run(&session, "kaish-trash empty --confirm").await;
    assert_ne!(
        confirmed.code, 2,
        "the advertised re-run must not be refused again: {confirmed:?}"
    );
    assert!(
        !confirmed.err.contains("a value is required"),
        "--confirm must take no value: {}",
        confirmed.err
    );
}
