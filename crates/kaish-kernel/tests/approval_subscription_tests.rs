//! `fs.*` observability subscriptions: the glob filter, the `Observed`
//! entry, and the two modes.
//!
//! Everything drives real command strings through `kernel.execute()`, so the
//! full path runs — glob expansion, the gate site's per-path classification,
//! the decision chain, and the dispatch seam's settlement.
//!
//! The free-when-unsubscribed proof lives in `approval_zero_cost_tests.rs`,
//! alone in its own binary: its counter is process-wide, and these tests
//! deliberately build requests.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use std::path::Path;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::ledger::ApproverHandle;
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{
    LedgerEntry, ObservedResource, OperationPattern, ResourcePattern, Subscription,
    SubscriptionId, SubscriptionMode,
};

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
        .prefix("approval-subscription-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// A kernel, the authority its construction minted, and a two-directory tree:
/// `workspace/` (what the tests subscribe to) and `scratch/` (what they leave
/// unsubscribed).
struct Session {
    kernel: Kernel,
    authority: ApproverHandle,
    root: tempfile::TempDir,
}

impl Session {
    fn new() -> Self {
        let root = tempdir();
        std::fs::create_dir(root.path().join("workspace")).unwrap();
        std::fs::create_dir(root.path().join("scratch")).unwrap();
        let config = KernelConfig::repl()
            .with_cwd(root.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false);
        let (kernel, authority) = Kernel::build(config).expect("kernel");
        Self {
            kernel,
            authority,
            root,
        }
    }

    fn workspace(&self, name: &str) -> std::path::PathBuf {
        self.root.path().join("workspace").join(name)
    }

    fn scratch(&self, name: &str) -> std::path::PathBuf {
        self.root.path().join("scratch").join(name)
    }

    fn write(&self, path: &Path, contents: &str) {
        std::fs::write(path, contents).unwrap();
    }

    async fn run(&self, script: &str) -> ExecResult {
        self.kernel.execute(script).await.expect("kernel execute")
    }

    /// Subscribe `mode` to `operations` over everything under `dir/`.
    async fn subscribe_dir(
        &self,
        dir: &str,
        operations: &[&str],
        mode: SubscriptionMode,
    ) -> SubscriptionId {
        let glob = format!("{}/**", self.root.path().join(dir).display());
        self.authority
            .subscribe(Subscription::new(
                operations.iter().map(|o| OperationPattern::new(*o)).collect(),
                vec![ResourcePattern::new("path", glob)],
                mode,
                format!("the test subscribes to {dir}"),
            ))
            .await
            .expect("the subscription must register")
    }

    /// Subscribe `mode` to `operations` over everything under `workspace/`.
    async fn subscribe_workspace(
        &self,
        operations: &[&str],
        mode: SubscriptionMode,
    ) -> SubscriptionId {
        self.subscribe_dir("workspace", operations, mode).await
    }

    /// Every retained entry's variant name, in commit order.
    fn entry_kinds(&self) -> Vec<&'static str> {
        entries(self.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
            .iter()
            .filter(|e| !is_statement_tap(e))
            .map(entry_kind)
            .collect()
    }

    /// Every resource on every `Observed` entry, in commit order.
    fn observed_resources(&self) -> Vec<ObservedResource> {
        entries(self.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
            .into_iter()
            .filter(|entry| !is_statement_tap(entry))
            .filter_map(|entry| match entry {
                LedgerEntry::Observed { resources, .. } => Some(resources),
                _ => None,
            })
            .flatten()
            .collect()
    }
}

fn entry_kind(entry: &LedgerEntry) -> &'static str {
    match entry {
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
        LedgerEntry::StandingIssued { .. } => "StandingIssued",
        LedgerEntry::StandingRevoked { .. } => "StandingRevoked",
        LedgerEntry::Subscribed { .. } => "Subscribed",
        LedgerEntry::Observed { .. } => "Observed",
        LedgerEntry::Unsubscribed { .. } => "Unsubscribed",
        _ => "other",
    }
}

/// The scoping case: subscribe `fs.remove` under the workspace as `observe`,
/// and everything outside it stays unsubscribed and free.
#[tokio::test]
async fn an_observe_subscription_records_matching_paths_and_stays_silent_about_the_rest() {
    let session = Session::new();
    let inside = session.workspace("a.txt");
    let outside = session.scratch("b.txt");
    session.write(&inside, "inside");
    session.write(&outside, "outside");

    let id = session
        .subscribe_workspace(&["fs.remove"], SubscriptionMode::Observe)
        .await;

    let result = session
        .run(&format!("rm {} {}", inside.display(), outside.display()))
        .await;
    assert_eq!(result.code, 0, "{}", result.text_out());
    assert!(!inside.exists() && !outside.exists(), "both must be deleted");

    // One `Observed` entry for the covered path — a record with no chain
    // behind it: no request, no grant, no attempt.
    assert_eq!(session.entry_kinds(), vec!["Subscribed", "Observed"]);
    let resources = session.observed_resources();
    assert_eq!(resources.len(), 1, "{resources:?}");
    assert_eq!(resources[0].id, inside.display().to_string());
    assert_eq!(resources[0].resolved, inside.display().to_string());
    assert_eq!(
        resources[0].subscription,
        Some(id),
        "the entry must name the subscription that covered it"
    );
}

/// The same subscription over `fs.overwrite`, through the other gate site
/// (`gate_overwrites`) — so the filter is proven at both, not just at `rm`.
#[tokio::test]
async fn an_observe_subscription_records_an_overwrite_through_the_write_gate() {
    let session = Session::new();
    let inside = session.workspace("a.txt");
    let outside = session.scratch("b.txt");
    session.write(&inside, "before");
    session.write(&outside, "before");

    session
        .subscribe_workspace(&["fs.*"], SubscriptionMode::Observe)
        .await;

    let result = session
        .run(&format!(
            "echo after | tee {} {}",
            inside.display(),
            outside.display()
        ))
        .await;
    assert_eq!(result.code, 0, "{}", result.text_out());
    assert_eq!(std::fs::read_to_string(&inside).unwrap().trim(), "after");
    assert_eq!(std::fs::read_to_string(&outside).unwrap().trim(), "after");

    let resources = session.observed_resources();
    assert_eq!(
        resources.iter().map(|r| r.id.clone()).collect::<Vec<_>>(),
        vec![inside.display().to_string()],
        "only the covered path may reach the ledger"
    );
}

/// The property that separates the two modes: `observe` is a note, not a
/// permission. It never defers, never returns exit 2, and never attaches a
/// pending request to the result.
#[tokio::test]
async fn an_observe_subscription_never_blocks_and_never_returns_exit_two() {
    let session = Session::new();
    let target = session.workspace("a.txt");
    session.write(&target, "content");

    session
        .subscribe_workspace(&["fs.*"], SubscriptionMode::Observe)
        .await;

    // The `Observed` entry below proves the tap really engaged — without
    // it, "never exit 2" would also pass on a filter that matched nothing
    // at all. That the log holds no `Requested` proves the tap built no
    // request while doing it. (`ApprovalRequest::constructed_count` is
    // process-wide and this binary's other tests gate things in parallel,
    // so this session's own log is the assertion, not the counter.)
    let result = session.run(&format!("rm {}", target.display())).await;

    assert_eq!(result.code, 0, "{}", result.text_out());
    assert!(
        result.approval_request().is_none(),
        "an observe subscription must not surface a pending request"
    );
    assert!(!target.exists(), "the delete must have run");
    assert!(
        session.kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.is_empty(),
        "an observe subscription must leave nothing undecided"
    );
    assert_eq!(
        session.entry_kinds(),
        vec!["Subscribed", "Observed"],
        "the tap must have engaged"
    );
}

/// The other mode over the identical glob: `enforce` sends the same operation
/// through the real decision chain, so it holds at exit 2 with a pending
/// request.
#[tokio::test]
async fn an_enforce_subscription_over_the_same_glob_gates() {
    let session = Session::new();
    let inside = session.workspace("a.txt");
    let outside = session.scratch("b.txt");
    session.write(&inside, "inside");
    session.write(&outside, "outside");

    session
        .subscribe_workspace(&["fs.remove"], SubscriptionMode::Enforce)
        .await;

    let result = session
        .run(&format!("rm {} {}", inside.display(), outside.display()))
        .await;
    assert_eq!(result.code, 2, "{}", result.text_out());
    let view = result.approval_request().expect("a pending request");
    assert_eq!(view.operation.as_str(), "fs.remove");
    assert_eq!(
        view.resources
            .iter()
            .map(|r| r.id.clone())
            .collect::<Vec<_>>(),
        vec![inside.display().to_string()],
        "only the covered path may be gated"
    );

    // Nothing ran — a batch held at the gate holds every path in it, covered
    // or not, because the command returns before it deletes anything.
    assert!(inside.exists() && outside.exists());
    assert_eq!(session.entry_kinds(), vec!["Subscribed", "Requested"]);
}

/// An audit scope that changed with no record of the change makes the record
/// it produced unreadable, so the scope changes are themselves entries.
#[tokio::test]
async fn subscription_and_revocation_are_themselves_ledger_entries() {
    let session = Session::new();
    assert!(
        !session.kernel.approvals().any_subscriptions(),
        "a fresh ledger is subscribed to nothing"
    );

    let id = session
        .subscribe_workspace(&["fs.*"], SubscriptionMode::Observe)
        .await;
    assert!(session.kernel.approvals().any_subscriptions());
    assert_eq!(session.kernel.approvals().subscriptions().len(), 1);

    match entries(session.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).as_slice() {
        [LedgerEntry::Subscribed { subscription, .. }] => {
            assert_eq!(subscription.id, id, "the entry carries the allocated id");
            assert_eq!(subscription.mode, SubscriptionMode::Observe);
            assert_eq!(subscription.reason, "the test subscribes to workspace");
        }
        other => panic!("expected exactly one Subscribed entry, got {other:?}"),
    }

    session
        .authority
        .unsubscribe(&id, "the test is done watching")
        .await
        .expect("the revocation must post");

    assert!(
        !session.kernel.approvals().any_subscriptions(),
        "the fast path must disarm once the registry is empty"
    );
    assert!(session.kernel.approvals().subscriptions().is_empty());
    assert_eq!(session.entry_kinds(), vec!["Subscribed", "Unsubscribed"]);
    match entries(session.kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last() {
        Some(LedgerEntry::Unsubscribed { id: revoked, reason, .. }) => {
            assert_eq!(*revoked, id);
            assert_eq!(reason, "the test is done watching");
        }
        other => panic!("expected an Unsubscribed entry, got {other:?}"),
    }

    // And it takes effect: a delete after the revocation posts nothing.
    let target = session.workspace("a.txt");
    session.write(&target, "content");
    let result = session.run(&format!("rm {}", target.display())).await;
    assert_eq!(result.code, 0, "{}", result.text_out());
    assert_eq!(
        session.entry_kinds(),
        vec!["Subscribed", "Unsubscribed"],
        "a revoked subscription must record nothing further"
    );
}

/// The reviewers' relative-path case: the glob matches the resolved path, so
/// `cd workspace && rm a.txt` must land inside `/…/workspace/**` — and the
/// record must keep both spellings, the one the command named and the one
/// the glob matched. Under the chain-backed observe this exited 1 because a
/// second matcher re-globbed the display path; the tap has no second
/// matcher.
#[tokio::test]
async fn an_observe_subscription_records_a_relative_path_by_its_resolved_form() {
    let session = Session::new();
    let target = session.workspace("a.txt");
    session.write(&target, "content");

    let id = session
        .subscribe_workspace(&["fs.remove"], SubscriptionMode::Observe)
        .await;

    session.run("cd workspace").await;
    let result = session.run("rm a.txt").await;
    assert_eq!(result.code, 0, "{}", result.text_out());
    assert!(!target.exists(), "the delete must have run");

    let resources = session.observed_resources();
    assert_eq!(resources.len(), 1, "{resources:?}");
    assert_eq!(resources[0].id, "a.txt", "the record keeps what the command named");
    assert_eq!(
        resources[0].resolved,
        target.display().to_string(),
        "the record keeps what the glob matched"
    );
    assert_eq!(resources[0].subscription, Some(id));
}

/// The reviewers' disjoint-subscription case: one command touching paths
/// covered by two different observe subscriptions records both, each tagged
/// with the subscription that covered it. Under the chain-backed observe
/// this exited 1 because no single subscription covered the whole batch.
#[tokio::test]
async fn one_batch_spanning_two_observe_subscriptions_records_both_paths() {
    let session = Session::new();
    let in_workspace = session.workspace("a.txt");
    let in_scratch = session.scratch("b.txt");
    session.write(&in_workspace, "a");
    session.write(&in_scratch, "b");

    let workspace_id = session
        .subscribe_workspace(&["fs.remove"], SubscriptionMode::Observe)
        .await;
    let scratch_id = session
        .subscribe_dir("scratch", &["fs.remove"], SubscriptionMode::Observe)
        .await;

    let result = session
        .run(&format!(
            "rm {} {}",
            in_workspace.display(),
            in_scratch.display()
        ))
        .await;
    assert_eq!(result.code, 0, "{}", result.text_out());
    assert!(!in_workspace.exists() && !in_scratch.exists());

    let resources = session.observed_resources();
    assert_eq!(
        resources
            .iter()
            .map(|r| (r.id.clone(), r.subscription))
            .collect::<Vec<_>>(),
        vec![
            (in_workspace.display().to_string(), Some(workspace_id)),
            (in_scratch.display().to_string(), Some(scratch_id)),
        ],
        "each path must name the subscription that covered it"
    );
}

/// The reviewers' overlap case, pinned end to end: with an `enforce` and an
/// `observe` subscription over the same glob, the gate must hold at exit 2.
/// Under the chain-backed observe, stage 1b matched the observe rule against
/// the posted enforce request and silently downgraded the gate to a note.
#[tokio::test]
async fn an_observe_subscription_over_the_same_glob_must_not_bypass_enforce() {
    let session = Session::new();
    let target = session.workspace("a.txt");
    session.write(&target, "content");

    session
        .subscribe_workspace(&["fs.remove"], SubscriptionMode::Observe)
        .await;
    session
        .subscribe_workspace(&["fs.remove"], SubscriptionMode::Enforce)
        .await;

    let result = session.run(&format!("rm {}", target.display())).await;
    assert_eq!(
        result.code, 2,
        "enforce must win over observe: {}",
        result.text_out()
    );
    assert!(
        result.approval_request().is_some(),
        "the gate must surface its pending request"
    );
    assert!(target.exists(), "nothing may run while the gate holds");
    assert_eq!(
        session.entry_kinds(),
        vec!["Subscribed", "Subscribed", "Requested"],
        "no grant and no observe record may appear for a held gate"
    );
}

/// Revoking an id that was never issued is loud, not a no-op — a caller that
/// believes it turned off an audit scope must not be told it succeeded.
#[tokio::test]
async fn revoking_an_unknown_subscription_fails_loudly() {
    let session = Session::new();
    let err = session
        .authority
        .unsubscribe(&SubscriptionId::new(99), "never issued")
        .await
        .expect_err("an unknown id must not succeed");
    assert!(
        err.to_string().contains("subscription 99 does not exist"),
        "{err}"
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
