//! The approval-ledger core (`docs/approval-ledger.md`, ledger PR 2).
//!
//! This file IS the §B.3 transition table, turned into tests: every legal
//! row proves the specified entries land and the state moves; every illegal
//! row proves the specific `LedgerError` comes back and nothing moved. No
//! `#![cfg(feature = ...)]` gate — the ledger has no OS dependency by
//! design and must compile and pass featureless (`--no-default-features`).

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex};
use std::time::{Duration, SystemTime};

use kaish_kernel::ledger::{ConditionReport, Ledger, LedgerConfig, LedgerError, LedgerSink, LedgerSinkError, SystemClock};
use kaish_types::approval::{
    ApprovalRequest, CancelReason, Decision, GrantTerms, LedgerEntry, Observation, OperationPattern, Outcome,
    Principal, PrincipalKind, RequestId, RequestState, Resource,
    ResourceRef, RiskClass, StandingGrant, StateClaim,
};

/// The entries inside a ledger's records. These tests assert on entry shape;
/// the [`LedgerRecord`] envelope has its own coverage in `kaish-types` (spec
/// §A.5), and an entry this build does not recognize cannot occur here.
// A panic on a known-good fixture IS the test failing; this is a plain
// helper fn, so clippy's in-test allowance does not reach it.
#[allow(dead_code, clippy::expect_used)]
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


/// This file's ledger scope (spec §A.7): a fresh kernel id per ledger, and
/// no session — an unscoped ledger is the single-session shape.
fn test_scope() -> kaish_types::approval::ApprovalScope {
    kaish_types::approval::ApprovalScope::kernel(kaish_types::approval::KernelId::mint())
}

/// The origin a request posted by this file is stamped with (spec §A.7,
/// §A.9). One fixed binding: these tests exercise the state machine, not the
/// replay rules.
#[allow(dead_code)]
fn test_origin(principal: kaish_types::approval::Principal) -> kaish_types::approval::RequestOrigin {
    let scope = test_scope();
    kaish_types::approval::RequestOrigin::new(
        scope.clone(),
        kaish_types::approval::PlanBinding::new(
            kaish_types::approval::PlanDigest::new("test"),
            "/",
            scope,
        ),
        principal,
        kaish_types::approval::Capture::DirectExecution)
}

fn agent(id: &str) -> Principal {
    Principal::new(id, PrincipalKind::Agent)
}

fn draft(op: &str) -> kaish_types::approval::ApprovalRequestDraft {
    #[allow(clippy::unwrap_used)]
    ApprovalRequest::builder(op).risk(RiskClass::Reversible).build().unwrap()
}

async fn post(requester: &kaish_kernel::ledger::Requester, op: &str) -> ApprovalRequest {
    #[allow(clippy::unwrap_used)]
    requester
        .post_request(draft(op), test_origin(agent("agent-1")))
        .await
        .unwrap()
}

/// Post with the optional deadline an embedder may set (spec §A.10). Nothing
/// enforces it on a timer, so a test states a deadline already in the past
/// rather than sleeping for one.
async fn post_with_deadline(
    requester: &kaish_kernel::ledger::Requester,
    op: &str,
    deadline: SystemTime,
) -> ApprovalRequest {
    #[allow(clippy::unwrap_used)]
    requester
        .post_request(
            draft(op),
            test_origin(agent("agent-1")).with_deadline(Some(deadline)),
        )
        .await
        .unwrap()
}

fn terms(req: &ApprovalRequest, not_after: SystemTime) -> GrantTerms {
    // `GrantTerms` is `#[non_exhaustive]` — `once_for` is the only external
    // constructor. For a request with no declared resource transitions this
    // is equivalent to an unconditioned `{ not_after, conditions: vec![] }`.
    GrantTerms::once_for(req, not_after)
}

fn far_future() -> SystemTime {
    SystemTime::now() + Duration::from_secs(300)
}

// ─────────────────────── §B.3 request-level table ───────────────────────

#[tokio::test]
async fn post_request_creates_a_requested_chain() {
    let (requester, approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    assert_eq!(approvals.state(&req.id), Some(RequestState::Requested));
    let log = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items);
    assert!(matches!(log.last(), Some(LedgerEntry::Requested { .. })));
}

#[tokio::test]
async fn grant_moves_requested_to_granted_and_appends_granted() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted));
    assert!(matches!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(), Some(LedgerEntry::Granted { .. })));
}

#[tokio::test]
async fn deny_moves_requested_to_denied_and_appends_denied() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.deny(&req.id, req.revision, "no").await.unwrap();
    assert_eq!(approvals.state(&req.id), Some(RequestState::Denied));
    assert!(matches!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(), Some(LedgerEntry::Denied { .. })));
}

/// §A.10: there is **no** request TTL, so an undecided request with no
/// deadline stays `Requested` no matter how long nobody looks at it.
#[tokio::test]
async fn an_undecided_request_with_no_deadline_never_expires() {
    let (requester, approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    assert_eq!(req.deadline, None, "a request carries no deadline by default");
    // Anything that enumerates the ledger runs the sweep, which is the only
    // place expiry could ever materialize.
    let _ = approvals.pending(kaish_types::approval::PageRequest::default()).items;
    let _ = approvals.ids();
    assert_eq!(approvals.state(&req.id), Some(RequestState::Requested));
    assert!(
        !entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
            .iter()
            .any(|e| matches!(e, LedgerEntry::Expired { .. })),
        "nothing expires a request the embedder set no deadline on"
    );
}

/// §A.10's other half: an embedder that *does* set a deadline gets it
/// compared when the request is observed — never enforced on a timer.
#[tokio::test]
async fn an_embedder_set_deadline_materializes_expired_on_read() {
    let (requester, approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let deadline = SystemTime::now() - Duration::from_secs(1);
    let req = post_with_deadline(&requester, "fs.remove", deadline).await;
    assert_eq!(approvals.state(&req.id), Some(RequestState::Expired));
    assert!(matches!(
        entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(),
        Some(LedgerEntry::Expired {
            what: kaish_types::approval::Expiring::Request,
            ..
        })
    ));
}

#[tokio::test]
async fn redeem_before_any_decision_is_not_authorized_and_counts_a_rejection() {
    let (requester, approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    let err = requester
        .redeem_with_token(&req.id, "whatever", agent("agent-1"), ConditionReport::none())
        .await
        .unwrap_err();
    assert!(matches!(err, LedgerError::NotAuthorized(id) if id == req.id));
    assert_eq!(approvals.state(&req.id), Some(RequestState::Requested), "state must not move");
    assert!(matches!(
        entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(),
        Some(LedgerEntry::TokenRejected { request: Some(id), attempts: 1, .. }) if *id == req.id
    ));
}

#[tokio::test]
async fn bad_key_against_a_granted_request_is_not_authorized_and_counts() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let err = requester
        .redeem_with_token(&req.id, "wrong", agent("agent-1"), ConditionReport::none())
        .await
        .unwrap_err();
    assert!(matches!(err, LedgerError::NotAuthorized(id) if id == req.id));
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted), "state must not move");
    assert!(matches!(
        entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(),
        Some(LedgerEntry::TokenRejected { request: Some(id), attempts: 1, .. }) if *id == req.id
    ));
}

#[tokio::test]
async fn fifth_bad_key_voids_and_a_later_good_key_fails_naming_the_void() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let real = approver.token_for(&req.id).expect("granted request has a credential");

    for n in 1..5 {
        let err = requester
            .redeem_with_token(&req.id, "wrong", agent("agent-1"), ConditionReport::none())
            .await
            .unwrap_err();
        assert!(matches!(err, LedgerError::NotAuthorized(_)), "rejection {n}");
        assert_eq!(approvals.state(&req.id), Some(RequestState::Granted), "still live after rejection {n}");
    }

    // The 5th rejection voids the request.
    let err = requester
        .redeem_with_token(&req.id, "wrong", agent("agent-1"), ConditionReport::none())
        .await
        .unwrap_err();
    assert!(matches!(err, LedgerError::NotAuthorized(_)));
    assert_eq!(approvals.state(&req.id), Some(RequestState::Voided));
    let log = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items);
    assert!(matches!(log.get(log.len() - 2), Some(LedgerEntry::TokenRejected { attempts: 5, .. })));
    assert!(matches!(log.last(), Some(LedgerEntry::Voided { .. })));

    // Even the REAL key fails now, naming the void.
    let err = requester
        .redeem_with_token(&req.id, real.reveal(), agent("agent-1"), ConditionReport::none())
        .await
        .unwrap_err();
    match err {
        LedgerError::Terminal { state, detail, .. } => {
            assert_eq!(state, RequestState::Voided);
            let detail = detail.expect("void reason must be present");
            assert!(detail.contains('5'), "detail should name the void: {detail}");
        }
        other => panic!("expected Terminal naming the void, got {other:?}"),
    }
}

#[tokio::test]
async fn bad_key_matching_no_live_request_appends_token_rejected_none_and_voids_nothing() {
    let (requester, approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    // A syntactically valid but entirely unknown id.
    let bogus = RequestId::new(0xdead_beef, 999_999);

    for _ in 0..5 {
        let err = requester
            .redeem_with_token(&bogus, "guess", agent("agent-1"), ConditionReport::none())
            .await
            .unwrap_err();
        assert!(matches!(err, LedgerError::NotAuthorized(id) if id == bogus));
    }

    let log = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items);
    let none_count = log
        .iter()
        .filter(|e| matches!(e, LedgerEntry::TokenRejected { request: None, .. }))
        .count();
    assert_eq!(none_count, 5, "every guess against an unknown id records TokenRejected{{None}}");
    assert!(
        log.iter().all(|e| !matches!(e, LedgerEntry::Voided { .. })),
        "nothing should have voided — there was never a live request to void"
    );
}

#[tokio::test]
async fn redeem_with_the_real_credential_succeeds_and_reserves_an_attempt() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let token = approver.token_for(&req.id).unwrap();

    let attempt = requester
        .redeem_with_token(&req.id, token.reveal(), agent("someone-else"), ConditionReport::none())
        .await
        .unwrap();
    assert_eq!(attempt.request_id(), &req.id);
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted));
    assert!(matches!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(), Some(LedgerEntry::Redeemed { .. })));
}

#[tokio::test]
async fn condition_failure_refuses_and_voids_reserving_no_attempt() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let mut d = draft("git.push");
    d.resources.push(Resource::transition(
        "git.ref",
        "refs/heads/main",
        StateClaim::Exact("a1b2".to_string()),
        StateClaim::Exact("c3d4".to_string()),
    ));
    let req = requester
        .post_request(d, test_origin(agent("agent-1")))
        .await
        .unwrap();
    let grant_terms = GrantTerms::once_for(&req, far_future());
    assert_eq!(grant_terms.conditions.len(), 1);
    approver.grant(&req.id, req.revision, grant_terms).await.unwrap();

    // The world moved: the ref is no longer at a1b2.
    let observed = vec![Observation {
        resource: ResourceRef {
            kind: "git.ref".to_string(),
            id: "refs/heads/main".to_string(),
        },
        claim: StateClaim::Exact("e5f6".to_string()),
        at: SystemTime::now(),
    }];
    let err = requester.redeem(&req.id, agent("agent-1"), ConditionReport::observed(observed)).await.unwrap_err();
    assert!(matches!(err, LedgerError::Refused { .. }));
    assert_eq!(approvals.state(&req.id), Some(RequestState::Voided));
    let log = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items);
    assert!(matches!(log.get(log.len() - 2), Some(LedgerEntry::Refused { .. })));
    assert!(matches!(log.last(), Some(LedgerEntry::Voided { .. })));
    // No attempt was reserved.
    let chain = approvals.get(&req.id).unwrap();
    assert!(chain.attempts.is_empty());
}

#[tokio::test]
async fn redeem_while_an_attempt_is_in_flight_is_rejected() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let attempt = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap();
    let before = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).len();

    let err = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap_err();
    assert!(matches!(err, LedgerError::AttemptInFlight(id) if id == req.id));
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted), "state must not move");
    assert_eq!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).len(), before, "a rejected in-flight redemption must append nothing");
    let chain = approvals.get(&req.id).unwrap();
    assert_eq!(chain.attempts.len(), 1, "no second attempt was reserved");
    assert_eq!(chain.attempts[0].attempt, attempt.attempt_id());
}

#[tokio::test]
async fn redeem_after_a_successful_settlement_reports_the_outcome_and_does_not_reexecute() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let attempt = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap();
    requester.settle(&attempt, Outcome::Exit(0)).await.unwrap();

    let before = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).len();
    let err = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap_err();
    match err {
        LedgerError::AlreadySettled { outcome, .. } => {
            assert!(matches!(outcome, Some(Outcome::Exit(0))));
        }
        other => panic!("expected AlreadySettled, got {other:?}"),
    }
    assert_eq!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).len(), before, "no new Redeemed should have been appended");
}

#[tokio::test]
async fn a_reported_failure_leaves_the_grant_live_for_a_retry() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let attempt1 = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap();
    requester.settle(&attempt1, Outcome::Exit(1)).await.unwrap();

    // The chain must still be Granted — a retry can redeem again.
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted));
    let attempt2 = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap();
    assert_ne!(attempt1.attempt_id(), attempt2.attempt_id());
    requester.settle(&attempt2, Outcome::Exit(0)).await.unwrap();
    // Now it really is closed.
    let err = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap_err();
    assert!(matches!(err, LedgerError::AlreadySettled { .. }));
}

#[tokio::test]
async fn an_unknown_outcome_closes_the_chain_without_reopening_the_grant() {
    let (requester, _approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let attempt = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap();
    requester
        .settle(&attempt, Outcome::Unknown { cause: kaish_types::approval::LostCause::Cancelled })
        .await
        .unwrap();

    let err = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap_err();
    assert!(matches!(err, LedgerError::AlreadySettled { outcome: Some(Outcome::Unknown { .. }), .. }));
}

#[tokio::test]
async fn grant_not_after_elapsing_materializes_expired() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver
        .grant(&req.id, req.revision, terms(&req, SystemTime::now() + Duration::from_millis(1)))
        .await
        .unwrap();
    tokio::time::sleep(Duration::from_millis(20)).await;
    assert_eq!(approvals.state(&req.id), Some(RequestState::Expired));
    assert!(matches!(
        entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(),
        Some(LedgerEntry::Expired { what: kaish_types::approval::Expiring::Grant, .. })
    ));
}

#[tokio::test]
async fn granting_an_already_decided_request_is_rejected() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let before = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).clone();
    let err = approver.grant(&req.id, approvals.get(&req.id).unwrap().request.revision, terms(&req, far_future())).await.unwrap_err();
    assert!(matches!(err, LedgerError::AlreadyDecided(id) if id == req.id));
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted), "state must not move");
    assert_eq!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items), before, "no new entry from the rejected second decision — the log is byte-for-byte unchanged");
}

#[tokio::test]
async fn terminal_states_reject_any_further_transition_and_leave_state_unchanged() {
    // Denied.
    {
        let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
        let req = post(&requester, "fs.remove").await;
        approver.deny(&req.id, req.revision, "no").await.unwrap();
        let before = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).clone();
        let err = approver.deny(&req.id, approvals.get(&req.id).unwrap().request.revision, "no again").await.unwrap_err();
        assert!(matches!(err, LedgerError::Terminal { state: RequestState::Denied, .. }));
        assert_eq!(approvals.state(&req.id), Some(RequestState::Denied));
        assert_eq!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items), before, "a rejected second decision must append nothing");
    }
    // Abandoned.
    {
        let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
        let req = post(&requester, "fs.remove").await;
        requester.abandon_request(&req.id, "job discarded").await.unwrap();
        let before = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).clone();
        let err = approver.grant(&req.id, approvals.get(&req.id).unwrap().request.revision, terms(&req, far_future())).await.unwrap_err();
        assert!(matches!(err, LedgerError::Terminal { state: RequestState::Abandoned, .. }));
        assert_eq!(approvals.state(&req.id), Some(RequestState::Abandoned));
        assert_eq!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items), before, "a grant against an abandoned request must append nothing");
    }
    // Voided (via redemption-time condition failure).
    {
        let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
        let mut d = draft("git.push");
        d.resources.push(Resource::transition(
            "git.ref",
            "refs/heads/main",
            StateClaim::Exact("a1b2".to_string()),
            StateClaim::Exact("c3d4".to_string()),
        ));
        let req = requester
            .post_request(d, test_origin(agent("agent-1")))
            .await
            .unwrap();
        approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
        let observed = vec![Observation {
            resource: ResourceRef {
                kind: "git.ref".to_string(),
                id: "refs/heads/main".to_string(),
            },
            claim: StateClaim::Exact("moved".to_string()),
            at: SystemTime::now(),
        }];
        requester.redeem(&req.id, agent("agent-1"), ConditionReport::observed(observed)).await.unwrap_err();
        assert_eq!(approvals.state(&req.id), Some(RequestState::Voided));
        let before = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).clone();
        let err = approver.grant(&req.id, approvals.get(&req.id).unwrap().request.revision, terms(&req, far_future())).await.unwrap_err();
        assert!(matches!(err, LedgerError::Terminal { state: RequestState::Voided, .. }));
        assert_eq!(approvals.state(&req.id), Some(RequestState::Voided));
        assert_eq!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items), before, "a grant against a voided request must append nothing");
    }
}

/// §B.5: cancel closes an undecided request, records who and why, and
/// returns its live slot.
#[tokio::test]
async fn cancel_closes_an_undecided_request_and_records_the_reason() {
    let (requester, approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;

    let closed = requester
        .cancel(&req.id, req.revision, agent("agent-1"), CancelReason::Withdrawn)
        .await
        .unwrap();
    assert_eq!(closed.id, req.id);
    assert_eq!(approvals.state(&req.id), Some(RequestState::Cancelled));
    assert!(approvals.pending(kaish_types::approval::PageRequest::default()).items.is_empty(), "the live slot comes back");
    assert!(matches!(
        entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(),
        Some(LedgerEntry::Cancelled {
            by,
            reason: CancelReason::Withdrawn,
            ..
        }) if by.id == "agent-1"
    ));
}

/// §B.3: `Cancelled` is terminal — a second cancellation appends nothing.
#[tokio::test]
async fn cancelling_a_cancelled_request_is_terminal_and_appends_nothing() {
    let (requester, approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    requester
        .cancel(&req.id, req.revision, agent("agent-1"), CancelReason::Withdrawn)
        .await
        .unwrap();
    let before = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).clone();

    let err = requester
        .cancel(&req.id, approvals.get(&req.id).unwrap().request.revision, agent("agent-1"), CancelReason::Withdrawn)
        .await
        .unwrap_err();
    assert!(matches!(
        err,
        LedgerError::Terminal {
            state: RequestState::Cancelled,
            ..
        }
    ));
    assert_eq!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items), before, "a refused cancellation appends nothing");
}

/// §B.3: a decided request is not cancellable — a decision that landed is
/// not undone by the requester losing interest.
#[tokio::test]
async fn cancelling_a_granted_request_is_already_decided() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();

    let err = requester
        .cancel(&req.id, approvals.get(&req.id).unwrap().request.revision, agent("agent-1"), CancelReason::Withdrawn)
        .await
        .unwrap_err();
    assert!(matches!(err, LedgerError::AlreadyDecided(id) if id == req.id));
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted));
}

// ─────────────────────── §B.6 revision checks ───────────────────────
//
// `grant`/`deny`/`cancel` quote the revision they read; a stale quote is
// refused as `LedgerError::StaleRevision` and recorded as
// `LedgerEntry::RevisionRejected` rather than applied — never silently
// dropped, and never silently applied anyway. Idempotent settlement
// (`settling_the_same_attempt_twice_appends_one_entry_and_returns_ok`,
// above) is the other half of "must not be confused": that is one
// operation arriving twice, not two operations disagreeing about state, and
// nothing about revision-checking changes it — `settle` takes no revision
// at all.

/// A wrong credential presentation bumps `revision` (`TokenRejected`
/// bumps — spec §A.7) without moving the request off `Requested`, which is
/// exactly the shape a caller needs to test a stale-but-still-open quote
/// against: the request is live, but the revision a caller read before this
/// happened is now behind.
async fn bump_revision_without_deciding(requester: &kaish_kernel::ledger::Requester, id: &RequestId) {
    let _ = requester
        .redeem_with_token(id, "wrong", agent("agent-1"), ConditionReport::none())
        .await;
}

#[tokio::test]
async fn granting_with_a_stale_revision_is_refused_and_recorded() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    assert_eq!(req.revision, 0);
    bump_revision_without_deciding(&requester, &req.id).await;
    assert_eq!(approvals.get(&req.id).unwrap().request.revision, 1, "TokenRejected bumped the revision");

    let before_len = approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items.len();
    // Quotes 0 — the revision this test read before the bump above, exactly
    // the shape an out-of-band approver holding a stale view is in.
    let err = approver.grant(&req.id, 0, terms(&req, far_future())).await.unwrap_err();
    match &err {
        LedgerError::StaleRevision { request, quoted, current, attempted } => {
            assert_eq!(*request, req.id);
            assert_eq!(*quoted, 0);
            assert_eq!(*current, 1);
            assert_eq!(*attempted, kaish_types::approval::TransitionKind::Grant);
        }
        other => panic!("expected StaleRevision, got {other:?}"),
    }

    // Recorded, not dropped — and with the request's own scope and a real
    // sequence number, the same as any other entry (spec §B.6).
    let log = approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items;
    assert_eq!(log.len(), before_len + 1, "the refusal is recorded");
    let record = log.last().unwrap();
    assert_eq!(record.scope, req.scope, "scope intact");
    assert!(record.sequence > 0, "sequence intact");
    assert!(matches!(
        record.known(),
        // `by` names the deciding principal — the minted authority's own
        // default identity here, since this test never assigns one — not
        // the requester (`agent-1`, whom `bump_revision_without_deciding`
        // presented a bad key as).
        Some(LedgerEntry::RevisionRejected { quoted: 0, current: 1, by, .. }) if by.id == "approver-handle-1"
    ));

    // Never applied: the request is exactly as it was — still `Requested`,
    // and the refusal itself did not move the revision further (spec §B.6's
    // transition table: "unchanged").
    assert_eq!(approvals.state(&req.id), Some(RequestState::Requested));
    assert_eq!(approvals.get(&req.id).unwrap().request.revision, 1);
}

#[tokio::test]
async fn denying_with_a_stale_revision_is_refused_and_recorded() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    bump_revision_without_deciding(&requester, &req.id).await;

    let before_len = approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items.len();
    let err = approver.deny(&req.id, 0, "no").await.unwrap_err();
    assert!(
        matches!(&err, LedgerError::StaleRevision { quoted: 0, current: 1, attempted: kaish_types::approval::TransitionKind::Deny, .. }),
        "{err:?}"
    );
    assert_eq!(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items.len(), before_len + 1, "the refusal is recorded");
    assert_eq!(approvals.state(&req.id), Some(RequestState::Requested), "never applied");
}

#[tokio::test]
async fn cancelling_with_a_stale_revision_is_refused_and_recorded() {
    let (requester, approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    bump_revision_without_deciding(&requester, &req.id).await;

    let before_len = approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items.len();
    let err = requester
        .cancel(&req.id, 0, agent("agent-1"), CancelReason::Withdrawn)
        .await
        .unwrap_err();
    assert!(
        matches!(&err, LedgerError::StaleRevision { quoted: 0, current: 1, attempted: kaish_types::approval::TransitionKind::Cancel, .. }),
        "{err:?}"
    );
    assert_eq!(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items.len(), before_len + 1, "the refusal is recorded");
    assert_eq!(approvals.state(&req.id), Some(RequestState::Requested), "never applied");
}

/// The race the late-answer rule exists for (spec §B.5, §B.6): a deadline
/// timer and a human answering at the same moment. Both sides read the
/// request at revision 0 and both act on that reading — only the one whose
/// transaction commits first can still be quoting the current revision by
/// the time it runs.
#[tokio::test]
async fn a_cancel_racing_a_grant_leaves_exactly_one_winner_and_exactly_one_revision_rejected() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    assert_eq!(req.revision, 0);

    let grant_task = {
        let approver = approver.clone();
        let req = req.clone();
        tokio::spawn(async move { approver.grant(&req.id, 0, terms(&req, far_future())).await })
    };
    let cancel_task = {
        let requester = requester.clone();
        let id = req.id.clone();
        tokio::spawn(async move { requester.cancel(&id, 0, agent("agent-1"), CancelReason::Withdrawn).await })
    };
    let grant_result = grant_task.await.unwrap();
    let cancel_result = cancel_task.await.unwrap();

    match (&grant_result, &cancel_result) {
        (Ok(_), Err(err)) | (Err(err), Ok(_)) => {
            assert!(
                matches!(err, LedgerError::StaleRevision { .. }),
                "the losing side must be refused as a stale quote, not fail some other way: {err:?}"
            );
        }
        other => panic!("exactly one side must win the race, got {other:?}"),
    }

    let rejected_count = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
        .iter()
        .filter(|e| matches!(e, LedgerEntry::RevisionRejected { .. }))
        .count();
    assert_eq!(rejected_count, 1, "exactly one RevisionRejected — the loser's, and only the loser's");
}

// ─────────────────────── Attempt-level table ───────────────────────

#[tokio::test]
async fn settling_the_same_attempt_twice_appends_one_entry_and_returns_ok() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let attempt = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap();

    let first = requester.settle(&attempt, Outcome::Exit(0)).await.unwrap();
    assert!(first, "first settlement appends an entry");
    let count_after_first = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).iter().filter(|e| matches!(e, LedgerEntry::Settled { .. })).count();
    assert_eq!(count_after_first, 1, "exactly one Settled entry after the first settlement");

    let second = requester.settle(&attempt, Outcome::Exit(0)).await.unwrap();
    assert!(!second, "second settlement is a no-op");
    let count_after_second = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).iter().filter(|e| matches!(e, LedgerEntry::Settled { .. })).count();
    assert_eq!(count_after_first, count_after_second, "idempotent by AttemptId — no duplicate Settled entry");
}

// The recovery sweep itself (`LedgerInner::sweep`) is `pub(crate)` — no
// public API in this PR calls it on a timer (that wiring is a later PR's
// job; see the PR body). Its behavior is proven as a white-box test in
// `src/ledger/core.rs` instead, where the sweep can be invoked directly.

// ─────────────────────── Concurrency ───────────────────────

#[tokio::test]
async fn concurrent_redemptions_of_one_grant_produce_exactly_one_redeemed() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();

    let mut tasks = Vec::new();
    for i in 0..16 {
        let requester = requester.clone();
        let id = req.id.clone();
        tasks.push(tokio::spawn(async move {
            requester.redeem(&id, agent(&format!("racer-{i}")), ConditionReport::none()).await
        }));
    }
    let mut ok_count = 0;
    let mut in_flight_count = 0;
    for task in tasks {
        match task.await.unwrap() {
            Ok(_) => ok_count += 1,
            Err(LedgerError::AttemptInFlight(_)) => in_flight_count += 1,
            Err(other) => panic!("unexpected error: {other:?}"),
        }
    }
    assert_eq!(ok_count, 1, "exactly one racer should win the reservation");
    assert_eq!(in_flight_count, 15);
    let redeemed_count = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).iter().filter(|e| matches!(e, LedgerEntry::Redeemed { .. })).count();
    assert_eq!(redeemed_count, 1);
}

#[tokio::test]
async fn seq_is_gap_free_under_concurrent_posts_from_sixteen_tasks() {
    let (requester, approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let mut tasks = Vec::new();
    for i in 0..16 {
        let requester = requester.clone();
        tasks.push(tokio::spawn(async move { post(&requester, &format!("fs.remove.{i}")).await }));
    }
    for task in tasks {
        task.await.unwrap();
    }
    let mut seqs: Vec<u64> = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).iter().map(LedgerEntry::seq).collect();
    seqs.sort_unstable();
    let expected: Vec<u64> = (1..=16).collect();
    assert_eq!(seqs, expected, "no gaps and no duplicates across 16 concurrent posts");
}

// ─────────────────────── Ring / sink capacity ───────────────────────

#[tokio::test]
async fn ring_refuses_loudly_rather_than_evicting_a_live_chain() {
    // A tiny ring: room for exactly 2 entries. One live request's own
    // Requested entry already occupies a slot forever (the chain never
    // closes), so a second post should be refused once the ring cannot
    // evict anything.
    let config = LedgerConfig::default().with_retained_entries(1);
    let (requester, _approvals, _approver) = Ledger::build(config, test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let _first = post(&requester, "fs.remove").await;
    // The ring holds exactly the first Requested entry now, and that
    // request is still live (Requested, undecided) — nothing is evictable.
    let err = requester
        .post_request(draft("fs.remove"), test_origin(agent("agent-1")))
        .await
        .unwrap_err();
    assert!(matches!(err, LedgerError::RingAtCapacity));
}

#[tokio::test]
async fn ring_evicts_closed_chains_to_make_room() {
    let config = LedgerConfig::default().with_retained_entries(2);
    let (requester, approvals, approver) = Ledger::build(config, test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req1 = post(&requester, "fs.remove").await;
    approver.deny(&req1.id, req1.revision, "no").await.unwrap(); // closes req1 — its 2 entries become evictable
    let req2 = post(&requester, "fs.remove").await;
    assert_eq!(approvals.state(&req2.id), Some(RequestState::Requested));
}

#[derive(Default)]
struct RecordingSink {
    posted: Mutex<Vec<kaish_types::approval::LedgerRecord>>,
    fail_after: AtomicUsize,
}

impl LedgerSink for RecordingSink {
    fn post(&self, record: &kaish_types::approval::LedgerRecord) -> Result<(), LedgerSinkError> {
        let remaining = self.fail_after.load(Ordering::SeqCst);
        if remaining == 0 {
            return Err(LedgerSinkError("synthetic sink failure".to_string()));
        }
        self.fail_after.store(remaining - 1, Ordering::SeqCst);
        #[allow(clippy::unwrap_used)]
        self.posted.lock().unwrap().push(record.clone());
        Ok(())
    }
}

#[tokio::test]
async fn full_sink_queue_returns_ledger_unavailable_rather_than_blocking_or_dropping() {
    let sink = Arc::new(RecordingSink {
        fail_after: AtomicUsize::new(usize::MAX),
        ..Default::default()
    });
    // Depth 1: the background drain task is never polled (no `.await`
    // yields control back to it before we saturate the channel), so the
    // second post should see the queue still full.
    let config = LedgerConfig::default().with_sink_queue(1);
    let (requester, _approvals, _approver) = Ledger::build(config, test_scope(), Some(sink), std::sync::Arc::new(SystemClock)).unwrap();
    // Neither call below has any internal `.await` point (the transaction
    // is fully synchronous — see `core.rs`'s module doc), so the background
    // drain task gets no chance to run between them: the one queue slot is
    // still occupied by the first entry when the second post is attempted,
    // and this is deterministic, not a race.
    let _first = post(&requester, "fs.remove").await;
    let err = requester
        .post_request(draft("fs.remove"), test_origin(agent("agent-1")))
        .await
        .unwrap_err();
    assert!(matches!(err, LedgerError::SinkUnavailable(_)));
}

#[tokio::test]
async fn a_sink_error_fails_subsequent_requests_closed() {
    let sink = Arc::new(RecordingSink {
        fail_after: AtomicUsize::new(0), // every post() call fails immediately
        ..Default::default()
    });
    let (requester, _approvals, _approver) = Ledger::build(LedgerConfig::default(), test_scope(), Some(sink), std::sync::Arc::new(SystemClock)).unwrap();
    let _first = post(&requester, "fs.remove").await;
    // Give the background drain task a chance to observe the failure.
    for _ in 0..50 {
        tokio::time::sleep(Duration::from_millis(5)).await;
        let err = requester
            .post_request(draft("fs.remove"), test_origin(agent("agent-1")))
            .await;
        if let Err(LedgerError::SinkUnavailable(_)) = err {
            return; // fail-closed observed
        }
    }
    panic!("expected the ledger to fail closed after the sink reported an error");
}

// ─────────────────────── Live capacity ───────────────────────

#[tokio::test]
async fn live_capacity_refuses_a_new_request_rather_than_evicting() {
    let config = LedgerConfig::default().with_live_capacity(1);
    let (requester, _approvals, _approver) = Ledger::build(config, test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let _first = post(&requester, "fs.remove").await;
    let err = requester
        .post_request(draft("fs.remove"), test_origin(agent("agent-1")))
        .await
        .unwrap_err();
    assert!(matches!(err, LedgerError::LiveCapacity { limit: 1 }));
}

// ─────────────────────── Credential accountability ───────────────────────

#[tokio::test]
async fn token_for_appends_key_retrieved_naming_the_retriever() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver.grant(&req.id, req.revision, terms(&req, far_future())).await.unwrap();
    let _token = approver.token_for(&req.id).unwrap();
    assert!(matches!(
        entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(),
        Some(LedgerEntry::KeyRetrieved { request, .. }) if *request == req.id
    ));
}

// ─────────────────────── Standing grants (pure record) ───────────────────────

#[tokio::test]
async fn grant_standing_and_revoke_standing_are_pure_record_operations() {
    let (_requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let g = kaish_types::approval::StandingGrant::new(
        vec![kaish_types::approval::OperationPattern::new("git.commit")],
        Vec::new(),
        None,
        None,
        agent("human-1"),
        "trusted agent workflow",
    )
    .with_max_uses(3);
    let id = approver.grant_standing(g).await.unwrap();
    assert_eq!(approvals.standing().len(), 1);
    assert!(matches!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(), Some(LedgerEntry::StandingIssued { .. })));

    approver.revoke_standing(&id, "no longer needed").await.unwrap();
    assert!(approvals.standing().is_empty());
    assert!(matches!(entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items).last(), Some(LedgerEntry::StandingRevoked { .. })));
}

// ─────────────────────── API surface: Requester cannot grant ───────────────────────

/// Snapshot the `impl Requester` block's method signatures and assert none
/// of them return a `Grant` — the structural half of the spec's "there is no
/// method on `Requester` that produces a `Grant`" guarantee (the other half,
/// `ApproverHandle` having no public constructor, is the `compile_fail`
/// doctest on `ApproverHandle` itself). A `trybuild` dependency was judged
/// not worth adding for this one property; see the PR body.
#[test]
fn requester_has_no_method_producing_a_grant() {
    let source = include_str!("../src/ledger/handles.rs");
    let start = source.find("impl Requester {").expect("impl Requester block must exist");
    let body = &source[start..];
    let end = find_matching_brace(body);
    let block = &body[..end];
    assert!(
        !block.contains("-> Result<Grant") && !block.contains("-> Grant") && !block.contains("Result<Grant,"),
        "Requester must never gain a method that produces a Grant:\n{block}"
    );
    insta::assert_snapshot!("requester_impl_block", block);
}

/// Finds the index just past the closing `}` matching the opening `{` in
/// `"impl Requester {"` at the start of `text`.
fn find_matching_brace(text: &str) -> usize {
    let mut depth = 0i32;
    for (i, ch) in text.char_indices() {
        match ch {
            '{' => depth += 1,
            '}' => {
                depth -= 1;
                if depth == 0 {
                    return i + 1;
                }
            }
            _ => {}
        }
    }
    text.len()
}

// ─────────────────────── §D.2/§E.7 deny_self_approval ───────────────────────

#[tokio::test]
async fn deny_self_approval_off_by_default_lets_the_requesting_principal_grant() {
    // The solo-human-REPL case: one principal is legitimately both
    // requester and approver, and the default must not break it.
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver
        .with_principal(agent("agent-1"))
        .grant(&req.id, req.revision, terms(&req, far_future()))
        .await
        .unwrap();
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted));
}

#[tokio::test]
async fn deny_self_approval_refuses_a_grant_from_the_requesting_principal() {
    let config = LedgerConfig::default().with_deny_self_approval(true);
    let (requester, approvals, approver) = Ledger::build(config, test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    let err = approver
        .with_principal(agent("agent-1"))
        .grant(&req.id, req.revision, terms(&req, far_future()))
        .await
        .unwrap_err();
    match &err {
        LedgerError::SelfApproval {
            request,
            requested_by,
            granted_by,
        } => {
            assert_eq!(*request, req.id);
            assert_eq!(*requested_by, agent("agent-1"));
            assert_eq!(*granted_by, agent("agent-1"));
        }
        other => panic!("expected LedgerError::SelfApproval, got {other:?}"),
    }
    let message = err.to_string();
    assert!(
        message.contains("agent-1"),
        "the message must name the principal, got: {message}"
    );
    assert_eq!(
        approvals.state(&req.id),
        Some(RequestState::Requested),
        "a refused grant must not move the request off Requested"
    );
}

#[tokio::test]
async fn deny_self_approval_on_still_allows_a_grant_from_a_distinct_principal() {
    let config = LedgerConfig::default().with_deny_self_approval(true);
    let (requester, approvals, approver) = Ledger::build(config, test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    approver
        .with_principal(agent("agent-2"))
        .grant(&req.id, req.revision, terms(&req, far_future()))
        .await
        .unwrap();
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted));
}

#[tokio::test]
async fn deny_self_approval_refuses_a_standing_grant_issued_by_the_requesting_principal() {
    // The same policy at the third `Granted`-producing path: a standing
    // rule whose own `issued_by` is the requester's principal must be
    // refused the same way an explicit grant is (one chokepoint, spec's
    // "not three checks").
    let config = LedgerConfig::default().with_deny_self_approval(true);
    let (requester, approvals, approver) = Ledger::build(config, test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = post(&requester, "fs.remove").await;
    let rule = StandingGrant::new(
        vec![OperationPattern::new("fs.remove")],
        Vec::new(),
        None,
        None,
        agent("agent-1"),
        "self-issued rule",
    )
    .unlimited_uses();
    approver.grant_standing(rule).await.unwrap();

    let err = approver
        .grant_from_standing(&req.id, Duration::from_secs(300))
        .await
        .unwrap_err();
    assert!(
        matches!(&err, LedgerError::SelfApproval { requested_by, granted_by, .. }
            if *requested_by == agent("agent-1") && *granted_by == agent("agent-1")),
        "expected LedgerError::SelfApproval naming agent-1 on both sides, got {err:?}"
    );
    assert_eq!(
        approvals.state(&req.id),
        Some(RequestState::Requested),
        "a refused standing auto-approval must not move the request off Requested"
    );
}

// ─────────────────────── the decision vocabulary ───────────────────────

#[test]
fn decision_defer_is_the_default_shape_used_by_a_deferring_policy() {
    // `Decision` is the vocabulary the ledger core speaks to the chain:
    // `Defer` is what `Policy::evaluate` returns by default, and "not my
    // call" must never read as "yes".
    let d = Decision::Defer;
    assert!(matches!(d, Decision::Defer));
}
