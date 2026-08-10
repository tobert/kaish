//! The operation matrix (`docs/approval-ledger.md` §H): operation × trash ×
//! approval × reversible × foreground/background/direct, and what each row
//! must produce — the decision, and the entries that land on the ledger.
//!
//! Written before the gate sites were reimplemented, because a migration is
//! only safe if the table it has to reproduce is written down first. The row
//! **"a trash failure is loud, never falls through to an unprotected
//! overwrite"** is the one that must not change, and it is here twice: once
//! for `rm`'s delete gate and once for the overwrite gate.
//!
//! Axes, and the values each takes:
//!
//! | Axis | Values |
//! |---|---|
//! | operation | `fs.remove` · `fs.overwrite` · `fs.rename` · `trash.empty` |
//! | trash | `set -o trash` on / off; backend present, absent, or failing |
//! | approval | the `fs.*` enforce policy on / off; `trash.empty` always enforced |
//! | reversible | small file · oversize · directory · symlink · nonexistent · new target · append |
//! | context | foreground · background (`&`) · direct (`tool.execute`, no seam) |
//!
//! The context axis lives in `approval_trash_tests.rs` (background jobs, the
//! VFS node, `wait`/`jobs`/`kill`) and in `rm.rs`'s unit tests (direct
//! execution, which records `Capture::DirectExecution`). This file owns the
//! decision table and the entry chains.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use std::path::Path;
use std::sync::{Arc, Mutex};

use async_trait::async_trait;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::ledger::ApproverHandle;
use kaish_kernel::trash::{TrashBackend, TrashEntry, TrashError};
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{GrantTerms, LedgerEntry, Outcome, RequestId};
use rstest::rstest;

/// The entries inside a ledger's records. These tests assert on entry shape;
/// the [`LedgerRecord`] envelope has its own coverage in `kaish-types` (spec
/// §A.5), and an entry this build does not recognize cannot occur here.
#[allow(dead_code)]
fn entries(records: Vec<kaish_types::approval::LedgerRecord>) -> Vec<LedgerEntry> {
    records
        .into_iter()
        .map(|record| {
            record
                .known()
                .cloned()
                .expect("this build wrote every record it reads back")
        })
        .collect()
}


fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("approval-matrix-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// What a matrix row expects the gate to decide. The loud-trash-failure row
/// is its own test rather than a variant here — it asserts on the surviving
/// content, not just the decision.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Expect {
    /// The operation ran. Exit 0, and the ledger stays empty — §C.5's
    /// free-when-unsubscribed rule.
    Ran,
    /// The trash caught it. Exit 0, the ledger stays empty, and the prior
    /// content is recoverable.
    Trashed,
    /// The gate held it. Exit 2, one `Requested` entry, target untouched.
    Gated,
}

/// A kernel and the authority its construction minted.
struct Session {
    kernel: Kernel,
    authority: ApproverHandle,
}

impl Session {
    async fn run(&self, script: &str) -> ExecResult {
        self.kernel.execute(script).await.expect("kernel execute")
    }

    /// Every retained entry's variant name, in commit order.
    fn entry_kinds(&self) -> Vec<&'static str> {
        entries(self.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
            .iter()
            .filter(|e| !is_statement_tap(e))
            .map(|e| match e {
                LedgerEntry::Requested { .. } => "Requested",
                LedgerEntry::Granted { .. } => "Granted",
                LedgerEntry::Denied { .. } => "Denied",
                LedgerEntry::Redeemed { .. } => "Redeemed",
                LedgerEntry::Settled { .. } => "Settled",
                LedgerEntry::KeyRetrieved { .. } => "KeyRetrieved",
                LedgerEntry::TokenRejected { .. } => "TokenRejected",
                LedgerEntry::Voided { .. } => "Voided",
                LedgerEntry::Expired { .. } => "Expired",
                LedgerEntry::Abandoned { .. } => "Abandoned",
                LedgerEntry::Refused { .. } => "Refused",
                _ => "other",
            })
            .collect()
    }

    /// Grant `id` for the next five minutes, on the terms the request itself
    /// declared. `GrantTerms::once_for` needs the stamped request; the
    /// tokenless view carries every field it reads.
    async fn grant(&self, id: &RequestId) {
        let view = self
            .kernel
            .approvals()
            .get(id)
            .expect("the request's chain")
            .request;
        self.authority
            .grant(
                id,
                view.revision,
                GrantTerms::once_for_view(
                    &view,
                    std::time::SystemTime::now() + std::time::Duration::from_secs(300),
                ),
            )
            .await
            .expect("the grant must post");
    }

    /// Grant the single pending request and retrieve its bearer key.
    async fn approve_pending(&self) -> (RequestId, String) {
        let approvals = self.kernel.approvals();
        let pending = approvals.pending(kaish_types::approval::PageRequest::default()).items;
        assert_eq!(pending.len(), 1, "exactly one request must be pending");
        let view = pending[0].clone();
        self.authority
            .grant(
                &view.id,
                view.revision,
                GrantTerms::once_for_view(
                    &view,
                    std::time::SystemTime::now() + std::time::Duration::from_secs(300),
                ),
            )
            .await
            .expect("the grant must post");
        let token = self
            .authority
            .token_for(&view.id)
            .expect("a credential for a granted request")
            .reveal()
            .to_string();
        (view.id, token)
    }
}

fn session_at(dir: &Path) -> Session {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_approvals(false)
        .with_trash(false);
    let (kernel, authority) = Kernel::build(config).expect("kernel");
    Session { kernel, authority }
}

fn session_with_trash(dir: &Path, mock: &Arc<MockTrash>) -> Session {
    let mut session = session_at(dir);
    session
        .kernel
        .set_trash_backend(Some(Arc::clone(mock) as Arc<dyn TrashBackend>));
    session
}

/// A trash backend that records what it was asked to do, and can be told to
/// fail — the only way to exercise the loud-failure row without breaking a
/// real trash.
#[derive(Default)]
struct MockTrash {
    trashed: Mutex<Vec<std::path::PathBuf>>,
    /// Byte snapshots recorded by `trash_bytes` (the overwrite gate's path):
    /// the logical path and its captured prior content.
    snapshots: Mutex<Vec<(std::path::PathBuf, Vec<u8>)>>,
    fail: bool,
}

impl MockTrash {
    fn failing() -> Arc<Self> {
        Arc::new(Self { fail: true, ..Self::default() })
    }

    /// How many items reached the trash, by either route.
    fn count(&self) -> usize {
        self.trashed.lock().expect("mock lock").len() + self.snapshots.lock().expect("mock lock").len()
    }
}

#[async_trait]
impl TrashBackend for MockTrash {
    async fn trash(&self, path: &Path) -> Result<(), TrashError> {
        if self.fail {
            return Err(TrashError::Backend("mock trash refused".into()));
        }
        self.trashed.lock().expect("mock lock").push(path.to_path_buf());
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

// ============================================================================
// The decision table — `fs.remove`
// ============================================================================

/// `rm` over one small file, across the trash × enforce square, plus the
/// cases that fall out of the trash's reach.
#[rstest]
// trash off: the enforce policy is the only gate.
#[case::plain_delete(false, false, "small", Expect::Ran)]
#[case::gated_delete(false, true, "small", Expect::Gated)]
// trash on: the trash IS the recovery net, so it wins over the gate.
#[case::trash_catches_it(true, false, "small", Expect::Trashed)]
#[case::trash_wins_over_the_gate(true, true, "small", Expect::Trashed)]
// too big to snapshot: the trash cannot catch it, so the gate applies again.
#[case::oversize_falls_through_to_the_gate(true, true, "oversize", Expect::Gated)]
#[case::oversize_with_no_policy_just_deletes(true, false, "oversize", Expect::Ran)]
// a symlink is a pointer, not the data it names — it bypasses the trash.
#[case::symlink_bypasses_trash_and_gates(true, true, "symlink", Expect::Gated)]
#[case::symlink_with_no_policy_just_unlinks(true, false, "symlink", Expect::Ran)]
// a directory always goes to the trash: stat size is unreliable for one.
#[case::directory_always_trashes(true, true, "dir", Expect::Trashed)]
#[tokio::test]
async fn the_rm_decision_table(
    #[case] trash: bool,
    #[case] enforce: bool,
    #[case] target: &str,
    #[case] expect: Expect,
) {
    let dir = tempdir();
    let mock = Arc::new(MockTrash::default());
    let session = session_with_trash(dir.path(), &mock);

    let name = match target {
        "small" => {
            std::fs::write(dir.path().join("t"), "data").unwrap();
            "t"
        }
        "oversize" => {
            std::fs::write(dir.path().join("t"), vec![b'x'; 4096]).unwrap();
            session.run("kaish-trash config max-size 16").await;
            "t"
        }
        "symlink" => {
            std::fs::write(dir.path().join("real"), "data").unwrap();
            std::os::unix::fs::symlink(dir.path().join("real"), dir.path().join("t")).unwrap();
            "t"
        }
        "dir" => {
            std::fs::create_dir(dir.path().join("t")).unwrap();
            std::fs::write(dir.path().join("t/inner"), "data").unwrap();
            "t"
        }
        other => panic!("unknown target {other}"),
    };

    if trash {
        assert!(session.run("set -o trash").await.ok());
    }
    if enforce {
        assert!(session.run("set -o approvals").await.ok());
    }

    let recursive = if target == "dir" { "-r " } else { "" };
    let result = session.run(&format!("rm {recursive}{name}")).await;
    let still_there = dir.path().join(name).symlink_metadata().is_ok();

    match expect {
        Expect::Ran => {
            assert_eq!(result.code, 0, "expected a plain delete: {}", result.err);
            assert!(!still_there, "the target should be gone");
            assert_eq!(mock.count(), 0, "nothing should have reached the trash");
            assert!(
                session.entry_kinds().is_empty(),
                "an ungated, unsubscribed operation must post NOTHING: {:?}",
                session.entry_kinds()
            );
        }
        Expect::Trashed => {
            assert_eq!(result.code, 0, "expected a trashed delete: {}", result.err);
            assert_eq!(mock.count(), 1, "exactly one item should reach the trash");
            assert!(
                session.entry_kinds().is_empty(),
                "the trash catching it means no approval was needed: {:?}",
                session.entry_kinds()
            );
        }
        Expect::Gated => {
            assert_eq!(result.code, 2, "expected an approval gate: {}", result.err);
            assert!(still_there, "the target must survive the gate");
            assert_eq!(mock.count(), 0, "a gated delete trashes nothing");
            assert_eq!(
                session.entry_kinds(),
                vec!["Requested"],
                "a deferred gate posts exactly one entry"
            );
            let view = result.approval_request().expect("a pending request");
            assert_eq!(view.operation.as_str(), "fs.remove");
        }
    }
}

/// The row that must not change: a trash failure is loud, and the file
/// survives. Never a fall-through to an unprotected delete.
#[rstest]
#[case::with_the_policy_on(true)]
#[case::with_the_policy_off(false)]
#[tokio::test]
async fn a_trash_failure_is_loud_and_never_falls_through_to_a_delete(#[case] enforce: bool) {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let mock = MockTrash::failing();
    let session = session_with_trash(dir.path(), &mock);

    assert!(session.run("set -o trash").await.ok());
    if enforce {
        assert!(session.run("set -o approvals").await.ok());
    }

    let result = session.run("rm precious.txt").await;
    assert_eq!(
        result.code, 1,
        "a trash failure must be loud, not a silent permanent delete: {}",
        result.err
    );
    assert!(
        result.err.contains("trash failed"),
        "the error must name the trash failure: {}",
        result.err
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "the file must survive a failed trash — this is the invariant"
    );
}

/// The same invariant on the overwrite gate: `gate_overwrites` snapshots to
/// the trash before the caller writes, and a snapshot failure must stop the
/// write rather than clobbering the prior content.
#[tokio::test]
async fn a_trash_failure_never_falls_through_to_an_unprotected_overwrite() {
    let dir = tempdir();
    std::fs::write(dir.path().join("dst.txt"), "old").unwrap();
    std::fs::write(dir.path().join("src.txt"), "new").unwrap();
    let mock = MockTrash::failing();
    let session = session_with_trash(dir.path(), &mock);

    assert!(session.run("set -o trash").await.ok());
    let result = session.run("cp src.txt dst.txt").await;

    assert_eq!(
        result.code, 1,
        "a failed snapshot must stop the overwrite: {}",
        result.err
    );
    assert_eq!(
        std::fs::read_to_string(dir.path().join("dst.txt")).unwrap(),
        "old",
        "the prior content must survive a failed trash snapshot"
    );
}

// ============================================================================
// The decision table — the overwrite gate
// ============================================================================

#[rstest]
// An existing target with content to lose.
#[case::overwrite_existing(false, true, "existing", Expect::Gated)]
#[case::overwrite_existing_ungated(false, false, "existing", Expect::Ran)]
#[case::trash_wins_over_the_overwrite_gate(true, true, "existing", Expect::Trashed)]
// Nothing to lose: a new file, or an append.
#[case::a_new_target_has_nothing_to_lose(false, true, "new", Expect::Ran)]
#[tokio::test]
async fn the_overwrite_decision_table(
    #[case] trash: bool,
    #[case] enforce: bool,
    #[case] target: &str,
    #[case] expect: Expect,
) {
    let dir = tempdir();
    let mock = Arc::new(MockTrash::default());
    let session = session_with_trash(dir.path(), &mock);
    std::fs::write(dir.path().join("src.txt"), "new").unwrap();
    if target == "existing" {
        std::fs::write(dir.path().join("dst.txt"), "old").unwrap();
    }

    if trash {
        assert!(session.run("set -o trash").await.ok());
    }
    if enforce {
        assert!(session.run("set -o approvals").await.ok());
    }

    let result = session.run("cp src.txt dst.txt").await;
    let content = std::fs::read_to_string(dir.path().join("dst.txt")).ok();

    match expect {
        Expect::Ran => {
            assert_eq!(result.code, 0, "expected a plain overwrite: {}", result.err);
            assert_eq!(content.as_deref(), Some("new"));
            assert!(session.entry_kinds().is_empty(), "nothing should be posted");
        }
        Expect::Trashed => {
            assert_eq!(result.code, 0, "expected a snapshot-then-write: {}", result.err);
            assert_eq!(content.as_deref(), Some("new"));
            assert_eq!(mock.count(), 1, "the prior content should reach the trash");
            assert!(session.entry_kinds().is_empty(), "nothing should be posted");
        }
        Expect::Gated => {
            assert_eq!(result.code, 2, "expected an approval gate: {}", result.err);
            assert_eq!(
                content.as_deref(),
                Some("old"),
                "the prior content must survive the gate"
            );
            assert_eq!(session.entry_kinds(), vec!["Requested"]);
            let view = result.approval_request().expect("a pending request");
            assert_eq!(view.operation.as_str(), "fs.overwrite");
        }
    }
}

/// An append has nothing to lose, so it never gates however the axes are set.
#[tokio::test]
async fn an_append_never_gates() {
    let dir = tempdir();
    std::fs::write(dir.path().join("log.txt"), "old\n").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());

    let result = session.run("echo new | tee -a log.txt").await;
    assert_eq!(result.code, 0, "an append must not gate: {}", result.err);
    assert!(std::fs::read_to_string(dir.path().join("log.txt"))
        .unwrap()
        .contains("old"));
    assert!(session.entry_kinds().is_empty(), "an append posts nothing");
}

/// Each gate site posts under its own operation, so a policy engine can tell
/// a rename from an overwrite from a delete.
#[rstest]
#[case::rm("rm t", "fs.remove")]
#[case::cp("cp src.txt t", "fs.overwrite")]
#[case::mv("mv src.txt t", "fs.rename")]
#[case::tee("echo x | tee t", "fs.overwrite")]
#[case::write("write t hello", "fs.overwrite")]
#[case::trash_empty("kaish-trash empty", "trash.empty")]
#[tokio::test]
async fn every_gate_site_posts_its_own_operation(#[case] script: &str, #[case] operation: &str) {
    let dir = tempdir();
    std::fs::write(dir.path().join("t"), "old").unwrap();
    std::fs::write(dir.path().join("src.txt"), "new").unwrap();
    let mock = Arc::new(MockTrash::default());
    let session = session_with_trash(dir.path(), &mock);
    assert!(session.run("set -o approvals").await.ok());

    let result = session.run(script).await;
    assert_eq!(result.code, 2, "{script} should gate: {}", result.err);
    let view = result.approval_request().expect("a pending request");
    assert_eq!(view.operation.as_str(), operation, "for {script}");
}

// ============================================================================
// The entry chains
// ============================================================================

#[tokio::test]
async fn a_granted_and_redeemed_delete_posts_the_full_chain() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());

    assert_eq!(session.run("rm precious.txt").await.code, 2);
    assert_eq!(session.entry_kinds(), vec!["Requested"]);

    let (_id, token) = session.approve_pending().await;
    let done = session
        .run(&format!("rm --confirm=\"{token}\" precious.txt"))
        .await;
    assert_eq!(done.code, 0, "{}", done.err);
    assert!(!dir.path().join("precious.txt").exists());

    let kinds = session.entry_kinds();
    for expected in ["Requested", "Granted", "Redeemed", "Settled"] {
        assert!(kinds.contains(&expected), "missing {expected} in {kinds:?}");
    }
    let settled = entries(session.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
        .into_iter()
        .find_map(|e| match e {
            LedgerEntry::Settled { outcome, .. } => Some(outcome),
            _ => None,
        })
        .expect("a Settled entry");
    assert_eq!(settled, Outcome::Exit(0), "the settlement records the real code");
}

#[tokio::test]
async fn a_failed_attempt_leaves_the_grant_live_for_a_retry() {
    // Spec §A.1: a grant is consumed by one *successful* settlement. A
    // transient failure must not burn it, or an operator has to re-approve
    // work that never happened.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());
    assert_eq!(session.run("rm precious.txt").await.code, 2);
    let (id, token) = session.approve_pending().await;

    // Delete the file out from under the replay so `rm` fails.
    std::fs::remove_file(dir.path().join("precious.txt")).unwrap();
    let failed = session
        .run(&format!("rm --confirm=\"{token}\" precious.txt"))
        .await;
    assert_eq!(failed.code, 1, "the delete should fail: {}", failed.err);

    // Put it back and retry with the same key: the grant is still live.
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let retried = session
        .run(&format!("rm --confirm=\"{token}\" precious.txt"))
        .await;
    assert_eq!(
        retried.code, 0,
        "a failed attempt must not consume the grant: {}",
        retried.err
    );
    assert!(!dir.path().join("precious.txt").exists());

    let state = session.kernel.approvals().state(&id);
    assert!(state.is_some(), "the chain must still be readable");
}

#[tokio::test]
async fn a_key_presented_after_success_reports_the_outcome_and_deletes_once() {
    // The behavior change the latch's reusable nonce hid. This test is the
    // point of §F.3 item 4.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());
    assert_eq!(session.run("rm precious.txt").await.code, 2);
    let (_id, token) = session.approve_pending().await;

    let first = session
        .run(&format!("rm --confirm=\"{token}\" precious.txt"))
        .await;
    assert_eq!(first.code, 0, "{}", first.err);
    assert!(!dir.path().join("precious.txt").exists());

    // Restore the file. If the second presentation re-ran the delete — which
    // is exactly what a reusable nonce did — it would vanish again.
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let second = session
        .run(&format!("rm --confirm=\"{token}\" precious.txt"))
        .await;
    assert_eq!(second.code, 1, "a settled grant must not re-execute");
    assert!(
        second.err.contains("already settled"),
        "the refusal must report the settled outcome: {}",
        second.err
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "the file must be deleted exactly once"
    );
}

#[tokio::test]
async fn a_replay_whose_draft_does_not_match_is_refused_and_posts_no_second_request() {
    // Spec §B.4: a bare replay that turned into a different operation must
    // not be authorized by the grant it carries, and must not post a fresh
    // `Requested` that nobody is waiting on.
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "a").unwrap();
    std::fs::write(dir.path().join("b.txt"), "b").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());

    assert_eq!(session.run("rm a.txt").await.code, 2);
    let (_id, token) = session.approve_pending().await;
    let before = session.entry_kinds().len();

    // The key was granted for a.txt; present it against b.txt. The draft
    // matcher finds no request describing *that* operation, so nothing is
    // redeemed and nothing is deleted.
    let wrong = session.run(&format!("rm --confirm=\"{token}\" b.txt")).await;
    assert_eq!(wrong.code, 1, "a mismatched replay must fail: {}", wrong.err);
    assert!(
        dir.path().join("b.txt").exists(),
        "the unapproved file must survive"
    );
    let after = session.entry_kinds();
    assert!(
        !after[before..].contains(&"Requested"),
        "a mismatched presentation must not post a second request: {after:?}"
    );
}

// ============================================================================
// The policy pin
// ============================================================================

fn pinned_session_at(dir: &Path) -> Session {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_approvals(true)
        .with_policy_pinned(true)
        .with_trash(false);
    let (kernel, authority) = Kernel::build(config).expect("kernel");
    Session { kernel, authority }
}

/// The pin holds through every scope a script can reach for (spec §F.3 item
/// 3). Three of these never reach the builtin at all: `set` is a grammar
/// keyword in kaish, so `$(set …)`, `set … | cat`, and `set … &` are parse
/// errors before any policy check runs. That is a *stronger* guarantee than
/// the refusal, and worth pinning as a fact rather than assumed — if `set`
/// ever becomes an ordinary command, these shapes start reaching the builtin
/// and the pin's refusal is what has to catch them.
#[rstest]
#[case::plain("set +o approvals")]
#[case::in_a_cmdsub("x=$(set +o approvals)")]
#[case::in_a_pipeline_stage("set +o approvals | cat")]
#[case::backgrounded("set +o approvals &")]
#[case::after_a_conjunction("true && set +o approvals")]
#[tokio::test]
async fn the_pin_survives_every_scope_a_script_can_reach_for(#[case] script: &str) {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let session = pinned_session_at(dir.path());

    // Either the parser refuses the shape or the pin refuses the change.
    // Never a success: a successful `set +o approvals` here would be the hole.
    match session.kernel.execute(script).await {
        Err(_parse_error) => {}
        Ok(result) => assert_ne!(
            result.code, 0,
            "{script:?} must not succeed under a pin: {}",
            result.err
        ),
    }

    // The proof that matters: a delete still gates.
    let gated = session.run("rm precious.txt").await;
    assert_eq!(
        gated.code, 2,
        "the policy must still be enforced after {script:?}: {}",
        gated.err
    );
    assert!(dir.path().join("precious.txt").exists());
}

#[tokio::test]
async fn the_pin_refuses_loudly_rather_than_no_opping() {
    let dir = tempdir();
    let session = pinned_session_at(dir.path());

    let refused = session.run("set +o approvals").await;
    assert_eq!(refused.code, 1, "a pinned policy must refuse, loudly");
    assert!(
        refused.err.contains("pinned by the embedder"),
        "the refusal must name the pin: {}",
        refused.err
    );
}

#[tokio::test]
async fn the_pin_survives_a_kai_script() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    std::fs::write(dir.path().join("disarm.kai"), "set +o approvals\n").unwrap();
    let session = pinned_session_at(dir.path());

    session.run("source disarm.kai").await;

    let gated = session.run("rm precious.txt").await;
    assert_eq!(
        gated.code, 2,
        "a sourced script must not disarm the session: {}",
        gated.err
    );
    assert!(dir.path().join("precious.txt").exists());
}

#[tokio::test]
async fn the_pin_survives_a_kernel_reset() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let session = pinned_session_at(dir.path());

    session.kernel.reset().await.expect("reset");

    let refused = session.run("set +o approvals").await;
    assert_eq!(refused.code, 1, "a reset must not drop the pin: {}", refused.err);
    // `reset()` returns the cwd to `/`, so name the file absolutely.
    let target = dir.path().join("precious.txt");
    let gated = session.run(&format!("rm {}", target.display())).await;
    assert_eq!(gated.code, 2, "the policy must survive a reset: {}", gated.err);
    assert!(target.exists());
}

// ============================================================================
// The authority boundary
// ============================================================================

/// A session that was never handed an `ApproverHandle` has no reachable path
/// to a grant. Walk the builtin registry: nothing there grants, and the only
/// script-reachable approval surface is `--confirm=<token>`, which needs a
/// credential the session cannot mint (spec §E.2, tier 1).
#[tokio::test]
async fn a_session_with_no_handle_has_no_reachable_grant_path() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let config = KernelConfig::repl()
        .with_cwd(dir.path().to_path_buf())
        .with_approvals(true)
        .with_trash(false);
    // `Kernel::new` is `build` with the authority dropped — the posture of a
    // session an embedder gave no authority to.
    let kernel = Kernel::new(config).expect("kernel");

    // Exactly one builtin bridges to the approval side (spec §D.3), and
    // ledger PR 7 landed it: `approvals`. Walk the registry and assert
    // nothing else names an approval verb — a second bridge would reopen the
    // hole the authority check closes.
    let bridges: Vec<String> = kernel
        .tool_schemas()
        .into_iter()
        .filter(|schema| {
            schema
                .subcommands
                .iter()
                .any(|sub| matches!(sub.name.as_str(), "grant" | "deny" | "revoke"))
                || matches!(schema.name.as_str(), "grant" | "approve")
        })
        .map(|schema| schema.name)
        .collect();
    assert_eq!(bridges, vec!["approvals".to_string()]);

    // And the one bridge refuses this session, because it holds no handle.
    let refused = kernel.execute("approvals grant req_00000000_1").await.expect("approvals");
    assert_eq!(refused.code, 1, "a session with no handle cannot grant: {refused:?}");
    assert!(
        refused.err.contains("no approval authority"),
        "the refusal must name the reason: {}",
        refused.err
    );

    // And the gate really is unfulfillable from inside the session.
    let gated = kernel.execute("rm precious.txt").await.expect("rm");
    assert_eq!(gated.code, 2);
    assert!(
        kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.len() == 1,
        "the request is posted and waiting on an authority the session lacks"
    );
    assert!(dir.path().join("precious.txt").exists());
}

// ============================================================================
// Control-plane reach: every statement aggregator carries the request
// ============================================================================
//
// Four inner aggregators (`execute_user_tool`, `source`, `execute_block_capturing`,
// and the PATH-resolved `.kai` runner) rebuild an `ExecResult` from parts rather
// than returning the last statement's own. Each used to keep only
// out/err/code/data, so a body ending in a gated operation reduced to a bare
// exit 2 with no request — indistinguishable from an ordinary failure, and
// unfulfillable, because the caller never learns the request's id.
//
// The latch had this same gap. It is fixed here because a dropped control-plane
// signal IS the silent bypass the ledger exists to end.

#[tokio::test]
async fn a_gate_inside_a_function_reaches_the_caller() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());

    let defined = session.run("cleanup() { rm precious.txt; }").await;
    assert!(defined.ok(), "{}", defined.err);

    let gated = session.run("cleanup").await;
    assert_eq!(gated.code, 2, "the function must surface the gate: {}", gated.err);
    let view = gated
        .approval_request()
        .expect("a function body's gate must reach its caller");
    assert_eq!(view.operation.as_str(), "fs.remove");
    assert!(dir.path().join("precious.txt").exists());
}

#[tokio::test]
async fn a_gate_inside_a_sourced_script_reaches_the_caller() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    std::fs::write(dir.path().join("cleanup.kai"), "rm precious.txt\n").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());

    let gated = session.run("source cleanup.kai").await;
    assert_eq!(gated.code, 2, "source must surface the gate: {}", gated.err);
    assert!(
        gated.approval_request().is_some(),
        "a sourced script's gate must reach its caller"
    );
    assert!(dir.path().join("precious.txt").exists());
}

#[tokio::test]
async fn a_gate_inside_a_path_resolved_kai_script_reaches_the_caller() {
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    std::fs::write(dir.path().join("cleanup.kai"), "rm precious.txt\n").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());
    assert!(session
        .run(&format!("export PATH={}", dir.path().display()))
        .await
        .ok());

    let gated = session.run("cleanup").await;
    assert_eq!(gated.code, 2, "a .kai script must surface the gate: {}", gated.err);
    assert!(
        gated.approval_request().is_some(),
        "a PATH-resolved .kai script's gate must reach its caller"
    );
    assert!(dir.path().join("precious.txt").exists());
}

// ============================================================================
// The pin survives every scope reset a script can reach
// ============================================================================

#[tokio::test]
async fn a_path_resolved_kai_script_runs_under_the_callers_policy() {
    // A `.kai` script gets an isolated scope. The approval policy is not
    // session state a script may shed — a blank scope would run the delete
    // **ungated** under a pinned-on policy, which is the hole the pin exists
    // to close.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    std::fs::write(dir.path().join("danger.kai"), "rm precious.txt\n").unwrap();
    let session = pinned_session_at(dir.path());
    assert!(session
        .run(&format!("export PATH={}", dir.path().display()))
        .await
        .ok());

    let result = session.run("danger").await;
    assert_eq!(
        result.code, 2,
        "a .kai script must inherit the caller's policy, not a blank scope: {}",
        result.err
    );
    assert!(
        dir.path().join("precious.txt").exists(),
        "the file must survive — an isolated scope must not disarm the gate"
    );
}

#[tokio::test]
async fn kaish_clear_does_not_disarm_a_pinned_policy() {
    // `kaish-clear` clears variables and cwd. A policy is neither — and a
    // blank scope defaults the gate off and unpinned, which would make
    // clearing the session a script-reachable way around the pin.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let session = pinned_session_at(dir.path());

    let cleared = session.run("kaish-clear").await;
    assert!(cleared.ok(), "{}", cleared.err);

    // `kaish-clear` resets the cwd to `/`, so name the file absolutely.
    let target = dir.path().join("precious.txt");
    let gated = session.run(&format!("rm {}", target.display())).await;
    assert_eq!(
        gated.code, 2,
        "kaish-clear must not disarm a pinned policy: {}",
        gated.err
    );
    assert!(target.exists());

    let refused = session.run("set +o approvals").await;
    assert_eq!(refused.code, 1, "the pin itself must survive the clear");
}

// ============================================================================
// `Kernel::confirm` never strands its reservation
// ============================================================================

#[tokio::test]
async fn a_replay_that_fails_before_its_gate_does_not_strand_the_attempt() {
    // `confirm` reserves the attempt *before* dispatching, so a replay that
    // returns before it ever reaches `request_gate` — `rm` on a path that
    // vanished between the grant and the replay fails at its `lstat` — would
    // leave the attempt `Reserved` forever. Every later redemption would then
    // fail `AttemptInFlight`: a grant the operator can no longer use.
    let dir = tempdir();
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());
    assert_eq!(session.run("rm precious.txt").await.code, 2);

    let id = session.kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items[0].id.clone();
    session.grant(&id).await;

    // Delete the target out from under the replay.
    std::fs::remove_file(dir.path().join("precious.txt")).unwrap();
    let failed = session
        .kernel
        .confirm(&session.authority, &id)
        .await
        .expect("confirm executes");
    assert_ne!(failed.code, 0, "the replay should fail: {}", failed.err);

    // Put it back and confirm again. A non-zero settlement does not consume
    // the grant, so this must succeed rather than reporting an in-flight
    // attempt.
    std::fs::write(dir.path().join("precious.txt"), "data").unwrap();
    let retried = session
        .kernel
        .confirm(&session.authority, &id)
        .await
        .expect("confirm executes");
    assert_eq!(
        retried.code, 0,
        "a failed replay must leave the grant usable, not stranded: {}",
        retried.err
    );
    assert!(!dir.path().join("precious.txt").exists());
}

#[tokio::test]
async fn two_concurrent_confirms_each_replay_their_own_request() {
    // The reservation and the replay correlation are single slots on shared
    // kernel state. Interleaved confirms would let one replay adopt the
    // other's authorization, and leave the other posting a fresh request.
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "a").unwrap();
    std::fs::write(dir.path().join("b.txt"), "b").unwrap();
    let session = session_at(dir.path());
    assert!(session.run("set -o approvals").await.ok());

    assert_eq!(session.run("rm a.txt").await.code, 2);
    assert_eq!(session.run("rm b.txt").await.code, 2);
    let pending = session.kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items;
    assert_eq!(pending.len(), 2);
    for view in &pending {
        session.grant(&view.id).await;
    }
    let (first, second) = (pending[0].id.clone(), pending[1].id.clone());

    let (r1, r2) = tokio::join!(
        session.kernel.confirm(&session.authority, &first),
        session.kernel.confirm(&session.authority, &second),
    );
    let r1 = r1.expect("confirm executes");
    let r2 = r2.expect("confirm executes");
    assert_eq!(r1.code, 0, "first confirm: {}", r1.err);
    assert_eq!(r2.code, 0, "second confirm: {}", r2.err);
    assert!(!dir.path().join("a.txt").exists(), "a.txt should be deleted");
    assert!(!dir.path().join("b.txt").exists(), "b.txt should be deleted");

    // Neither replay may have posted a fresh request.
    assert!(
        session.kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.is_empty(),
        "a replay must never post a second request: {:?}",
        session.kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items
    );
}

/// The statement tap (`docs/approval-ledger.md` §C.6) posts one chainless
/// `Observed{cmd.execute}` entry per top-level statement, unconditionally and
/// with nothing to subscribe to. These tables are about the `fs.*` chain, so
/// they read the log with the tap filtered out — "nothing was posted" here
/// means nothing on the fs chain, which is still the claim that matters and
/// still O(paths)-free.
fn is_statement_tap(entry: &LedgerEntry) -> bool {
    matches!(entry, LedgerEntry::Observed { operation, .. } if operation.as_str() == "cmd.execute")
}
