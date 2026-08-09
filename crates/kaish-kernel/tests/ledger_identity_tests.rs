//! Identity, binding, and the versioned record (`docs/approval-ledger.md`
//! §A.5, §A.7, §A.9).
//!
//! Four properties this lane exists to guarantee: a request raised in one
//! session is invisible to another session's handles; a gate reached from
//! inside a gated statement names that statement as its parent; a grant
//! redeemed from outside the context it was decided in is not redeemed at
//! all; and every recorded transition moves the request's revision.
//!
//! The unknown-entry rule (§A.5's "a reader must not silently drop an unknown
//! entry variant") is proven in `kaish-types`, where the envelope lives.
//!
//! No file-wide feature gate: the ledger has no OS dependency and these
//! tests must pass featureless. The two kernel-level tests at the bottom
//! mount a real filesystem and carry their own `localfs` gate.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::time::{Duration, SystemTime};

use kaish_kernel::ledger::{Ledger, LedgerConfig, LedgerError, Requester, SystemClock};
use kaish_types::approval::{
    ApprovalRequest, ApprovalScope, Capture, GrantTerms, KernelId, LedgerEntry, PlanBinding,
    PlanDigest, Principal, PrincipalKind, RequestOrigin, RequestState, RiskClass, SessionId,
};

fn agent(id: &str) -> Principal {
    Principal::new(id, PrincipalKind::Agent)
}

fn session(name: &str) -> SessionId {
    SessionId::new(name)
}

/// One kernel's scope, optionally serving a named session.
fn scope_for(kernel: KernelId, name: Option<&str>) -> ApprovalScope {
    let scope = ApprovalScope::kernel(kernel);
    match name {
        Some(name) => scope.with_session(session(name)),
        None => scope,
    }
}

/// Post one `fs.remove` request in `scope`.
async fn post_in(requester: &Requester, scope: ApprovalScope) -> ApprovalRequest {
    let draft = ApprovalRequest::builder("fs.remove")
        .risk(RiskClass::Irreversible)
        .build()
        .unwrap();
    let origin = RequestOrigin::new(
        scope.clone(),
        PlanBinding::new(PlanDigest::new("test"), "/w", scope),
        agent("agent-1"),
        Capture::DirectExecution);
    requester.post_request(draft, origin).await.unwrap()
}

fn far_future() -> SystemTime {
    SystemTime::now() + Duration::from_secs(300)
}

// ───────────────────── §A.7 scope: the read side ─────────────────────

#[tokio::test]
async fn a_request_raised_in_one_session_is_invisible_to_another_sessions_read_side() {
    let kernel = KernelId::mint();
    let (requester, approvals, _authority) =
        Ledger::build(LedgerConfig::default(), scope_for(kernel, None), None, std::sync::Arc::new(SystemClock)).unwrap();

    let mine = post_in(&requester, scope_for(kernel, Some("a"))).await;
    let theirs = post_in(&requester, scope_for(kernel, Some("b"))).await;

    let a = approvals.scope(session("a"));
    let b = approvals.scope(session("b"));

    assert_eq!(a.ids(), vec![mine.id.clone()]);
    assert_eq!(b.ids(), vec![theirs.id.clone()]);

    // Every read surface is scoped, not just the listing — a request carries
    // the command text that raised it (spec §A.7), so a reader reaching one
    // request of another session's is the whole leak.
    assert_eq!(a.state(&theirs.id), None, "state must not cross the session line");
    assert!(a.get(&theirs.id).is_none(), "get must not cross the session line");
    assert_eq!(
        a.pending().iter().map(|v| v.id.clone()).collect::<Vec<_>>(),
        vec![mine.id.clone()]
    );
    assert!(
        a.log(0)
            .iter()
            .all(|record| record.scope.session_id == Some(session("a"))),
        "the log a scoped reader sees carries only its own session's records"
    );

    // The unscoped view still sees both — that is the kernel-wide authority
    // the embedder holds, not a leak between sessions.
    assert_eq!(approvals.ids().len(), 2);
}

#[tokio::test]
async fn an_unattributed_request_belongs_to_the_kernel_and_no_scoped_reader_sees_it() {
    let kernel = KernelId::mint();
    let (requester, approvals, _authority) =
        Ledger::build(LedgerConfig::default(), scope_for(kernel, None), None, std::sync::Arc::new(SystemClock)).unwrap();
    let request = post_in(&requester, scope_for(kernel, None)).await;

    assert!(
        approvals.scope(session("a")).ids().is_empty(),
        "a request with no session must not fall to whichever session asks first"
    );
    assert_eq!(approvals.ids(), vec![request.id]);
}

// ───────────────────── §A.7 scope: the grant side ─────────────────────

#[tokio::test]
async fn a_scoped_authority_decides_only_within_its_session() {
    let kernel = KernelId::mint();
    let (requester, _approvals, authority) =
        Ledger::build(LedgerConfig::default(), scope_for(kernel, None), None, std::sync::Arc::new(SystemClock)).unwrap();

    let mine = post_in(&requester, scope_for(kernel, Some("a"))).await;
    let theirs = post_in(&requester, scope_for(kernel, Some("b"))).await;
    let a = authority.scope(session("a"));

    a.grant(&mine.id, mine.revision, GrantTerms::once_for(&mine, far_future()))
        .await
        .expect("a scoped authority decides inside its own session");

    let err = a
        .grant(&theirs.id, theirs.revision, GrantTerms::once_for(&theirs, far_future()))
        .await
        .expect_err("a scoped authority must not decide another session's request");
    assert!(
        matches!(&err, LedgerError::OutOfScope { request, session } if *request == theirs.id && *session == SessionId::new("a")),
        "{err}"
    );
    assert!(err.to_string().contains("another session"), "{err}");

    // Denial and key retrieval hold the same line. Retrieval most of all: the
    // key is a bearer credential (spec §A.2), so handing one over is the
    // widest thing an authority does.
    assert!(a.deny(&theirs.id, theirs.revision, "no").await.is_err());
    assert!(a.token_for(&mine.id).is_some());
    assert!(
        a.token_for(&theirs.id).is_none(),
        "a scoped authority must not retrieve another session's key"
    );

    // Nothing moved on the request it could not decide.
    assert_eq!(
        authority.approvals_view().state(&theirs.id),
        Some(RequestState::Requested)
    );
}

// ───────────────────── §A.5 the record envelope ─────────────────────

#[tokio::test]
async fn every_record_carries_the_scope_of_the_request_it_is_about() {
    let kernel = KernelId::mint();
    let (requester, approvals, authority) =
        Ledger::build(LedgerConfig::default(), scope_for(kernel, None), None, std::sync::Arc::new(SystemClock)).unwrap();
    let request = post_in(&requester, scope_for(kernel, Some("a"))).await;
    authority
        .grant(&request.id, request.revision, GrantTerms::once_for(&request, far_future()))
        .await
        .unwrap();

    let records = approvals.log(0);
    assert_eq!(records.len(), 2, "Requested then Granted");
    for record in &records {
        assert!(record.schema_is_known());
        assert_eq!(
            record.scope,
            scope_for(kernel, Some("a")),
            "a record about a request carries that request's scope, not the ledger's"
        );
        assert!(record.known().is_some());
    }
    // The envelope's sequence and time are the entry's own — they cannot
    // disagree about order or when a thing happened.
    for record in &records {
        let entry = record.known().unwrap();
        assert_eq!(record.sequence, entry.seq());
        assert_eq!(record.at, entry.at());
    }
}

// ───────────────────── §A.7 revision ─────────────────────

#[tokio::test]
async fn every_recorded_transition_bumps_the_revision_and_posting_does_not() {
    let kernel = KernelId::mint();
    let (requester, approvals, authority) =
        Ledger::build(LedgerConfig::default(), scope_for(kernel, None), None, std::sync::Arc::new(SystemClock)).unwrap();
    let request = post_in(&requester, scope_for(kernel, None)).await;
    assert_eq!(request.revision, 0, "a posted request starts at revision 0");

    let revision = |id: &kaish_types::approval::RequestId| -> u64 {
        approvals.get(id).expect("the chain").request.revision
    };
    assert_eq!(revision(&request.id), 0);

    authority
        .grant(&request.id, request.revision, GrantTerms::once_for(&request, far_future()))
        .await
        .unwrap();
    assert_eq!(revision(&request.id), 1, "Granted is a transition");

    // Retrieval is not: bumping here would invalidate the revision an
    // approver is holding for a decision it has not made yet (spec §A.7).
    authority.token_for(&request.id).expect("a granted request has a key");
    assert_eq!(revision(&request.id), 1, "KeyRetrieved moves nothing");

    let attempt = requester
        .redeem(
            &request.id,
            agent("agent-1"),
            kaish_kernel::ledger::ConditionReport::none(),
        )
        .await
        .unwrap();
    assert_eq!(revision(&request.id), 2, "Redeemed is a transition");

    requester
        .settle(&attempt, kaish_types::approval::Outcome::Exit(0))
        .await
        .unwrap();
    assert_eq!(revision(&request.id), 3, "Settled is a transition");
}

#[tokio::test]
async fn a_chainless_observed_record_carries_the_posting_sessions_scope() {
    // An `Observed` entry has no chain to read a scope off (spec §C.5), so a
    // record whose scope defaulted to the kernel's would be invisible to the
    // very session that produced it.
    let kernel = KernelId::mint();
    let (requester, approvals, _authority) =
        Ledger::build(LedgerConfig::default(), scope_for(kernel, None), None, std::sync::Arc::new(SystemClock)).unwrap();
    requester
        .observed(
            kaish_types::approval::OperationId::new("fs.remove").unwrap(),
            scope_for(kernel, Some("a")),
            agent("agent-1"),
            vec![kaish_types::approval::ObservedResource::planned("path", "/w/x")],
            None,
        )
        .await
        .unwrap();

    let scoped = approvals.scope(session("a")).log(0);
    assert_eq!(scoped.len(), 1);
    assert!(matches!(
        scoped[0].known(),
        Some(LedgerEntry::Observed { .. })
    ));
    assert!(approvals.scope(session("b")).log(0).is_empty());
}

// ───────────────────── §A.7 parenthood, §A.9 binding ─────────────────────
//
// Both need a real kernel: parenthood is a relationship between two gate
// sites, and the binding's cwd only means something against a real one.

#[cfg(feature = "localfs")]
mod kernel_level {
    use std::sync::Arc;

    use kaish_kernel::ledger::{ApproverHandle, CommandNameClassifier, StatementClassifier};
    use kaish_kernel::{Kernel, KernelConfig};
    use kaish_types::approval::{
        GrantTerms, OperationPattern, Principal, PrincipalKind, RequestId, RequestState,
        ResourcePattern, RiskClass, StandingGrant,
    };

    use super::far_future;

    fn tempdir() -> tempfile::TempDir {
        tempfile::Builder::new()
            .prefix("ledger-identity-")
            .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
            .expect("tempdir under CARGO_TARGET_TMPDIR")
    }

    /// A kernel rooted in its own directory, plus the authority it minted.
    struct Session {
        kernel: Kernel,
        authority: ApproverHandle,
        root: tempfile::TempDir,
    }

    impl Session {
        fn build(classifier: Option<Arc<dyn StatementClassifier>>) -> Self {
            let root = tempdir();
            let mut config = KernelConfig::repl()
                .with_cwd(root.path().to_path_buf())
                .with_approvals(false)
                .with_trash(false)
                .with_principal(Principal::new("test-agent", PrincipalKind::Agent));
            if let Some(classifier) = classifier {
                config = config.with_statement_classifier(classifier);
            }
            let (kernel, authority) = Kernel::build(config).expect("kernel");
            Self {
                kernel,
                authority,
                root,
            }
        }

        async fn run(&self, source: &str) -> kaish_kernel::interpreter::ExecResult {
            self.kernel.execute(source).await.expect("kernel execute")
        }

        fn write(&self, name: &str, contents: &str) {
            std::fs::write(self.root.path().join(name), contents).unwrap();
        }

        fn exists(&self, name: &str) -> bool {
            self.root.path().join(name).exists()
        }

        /// The one request the ledger holds for `operation`, newest first.
        fn request_for(&self, operation: &str) -> kaish_types::approval::ApprovalRequestView {
            let approvals = self.kernel.approvals();
            let mut ids = approvals.ids();
            ids.sort_by_key(RequestId::seq);
            ids.into_iter()
                .rev()
                .filter_map(|id| approvals.get(&id))
                .map(|chain| chain.request)
                .find(|view| view.operation.as_str() == operation)
                .unwrap_or_else(|| panic!("no {operation} request on the ledger"))
        }

        async fn grant_and_key(&self, id: &RequestId) -> String {
            let view = self.kernel.approvals().get(id).expect("the chain").request;
            self.authority
                .grant(id, view.revision, GrantTerms::once_for_view(&view, far_future()))
                .await
                .expect("the grant must post");
            self.authority
                .token_for(id)
                .expect("a granted request has a key")
                .reveal()
                .to_string()
        }
    }

    /// Auto-approve every statement, so execution reaches the `fs.*` gate
    /// underneath instead of halting at the statement gate.
    async fn auto_approve_statements(session: &Session) {
        session
            .authority
            .grant_standing(
                StandingGrant::new(
                    vec![OperationPattern::new("cmd.*")],
                    vec![ResourcePattern::new("cmd", "*")],
                    None,
                    None,
                    Principal::new("operator", PrincipalKind::Human),
                    "the test auto-approves statements to reach the gate underneath",
                )
                .unlimited_uses(),
            )
            .await
            .expect("the standing rule must issue");
    }

    /// Spec §A.7: a statement gate that grants can still reach an `fs.*` gate
    /// underneath it, and `parent` names the first.
    #[tokio::test]
    async fn a_nested_fs_gate_names_the_statements_request_as_parent() {
        let session = Session::build(Some(Arc::new(CommandNameClassifier::new(
            ["rm"],
            "the statement plans a destructive command",
            RiskClass::Irreversible,
        ))));
        session.write("precious.txt", "keep me");
        auto_approve_statements(&session).await;
        session.run("set -o approvals").await;

        let held = session.run("rm precious.txt").await;
        assert_eq!(
            held.code, 2,
            "an approved parent must not auto-approve the child: {}",
            held.err
        );

        let statement = session.request_for("cmd.execute");
        let child = session.request_for("fs.remove");
        assert_eq!(
            session.kernel.approvals().state(&statement.id),
            Some(RequestState::Granted),
            "the standing rule should have granted the statement"
        );
        assert_eq!(
            child.parent,
            Some(statement.id.clone()),
            "the nested gate must name the statement's request as its parent"
        );
        assert_eq!(statement.parent, None, "the statement itself is nested under nothing");
        assert!(session.exists("precious.txt"), "nothing ran");
    }

    /// Spec §A.9: a grant is a decision about an operation *in a context*. A
    /// key presented from a different working directory redeems nothing, and
    /// the operation asks again rather than failing.
    #[tokio::test]
    async fn a_key_presented_from_another_cwd_posts_a_fresh_request_instead_of_redeeming() {
        let session = Session::build(None);
        session.write("precious.txt", "keep me");
        session.run("set -o approvals").await;
        let target = session.root.path().join("precious.txt");
        let target = target.display().to_string();

        let held = session.run(&format!("rm {target}")).await;
        assert_eq!(held.code, 2, "{}", held.err);
        let first = held.approval_request().expect("a pending request").id.clone();
        let key = session.grant_and_key(&first).await;

        // Same command, same absolute target, different working directory.
        std::fs::create_dir(session.root.path().join("sub")).unwrap();
        assert_eq!(session.run("cd sub").await.code, 0);
        let rebound = session.run(&format!("rm --confirm={key} {target}")).await;

        assert_eq!(
            rebound.code, 2,
            "a moved binding must ask again, not redeem: {}",
            rebound.err
        );
        let second = rebound
            .approval_request()
            .expect("a fresh pending request")
            .id
            .clone();
        assert_ne!(second, first, "the re-run must post a new request, not reuse the granted one");
        assert!(session.exists("precious.txt"), "nothing was deleted");
        assert_eq!(
            session.kernel.approvals().state(&first),
            Some(RequestState::Granted),
            "the original grant is untouched — it was not redeemed and not voided"
        );
        assert_eq!(
            session.kernel.approvals().state(&second),
            Some(RequestState::Requested)
        );
    }

    /// The same key from the same working directory still redeems — the
    /// binding check must not break the ordinary path.
    #[tokio::test]
    async fn a_key_presented_from_the_binding_it_was_granted_in_still_redeems() {
        let session = Session::build(None);
        session.write("precious.txt", "delete me");
        session.run("set -o approvals").await;
        let target = session.root.path().join("precious.txt");
        let target = target.display().to_string();

        let held = session.run(&format!("rm {target}")).await;
        assert_eq!(held.code, 2, "{}", held.err);
        let id = held.approval_request().expect("a pending request").id.clone();
        let key = session.grant_and_key(&id).await;

        let done = session.run(&format!("rm --confirm={key} {target}")).await;
        assert_eq!(done.code, 0, "{}", done.err);
        assert!(!session.exists("precious.txt"), "the approved delete ran");
    }
}
