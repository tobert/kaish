//! The dispatcher's drop-safe `AttemptGuard` (`docs/approval-ledger.md` §C.1,
//! ledger PR 3).
//!
//! Nothing in production constructs one of these yet — no gate site calls
//! `ToolCtx::request_approval` (that begins at the PR 5 cutover) — so these
//! tests build the guard directly against a real ledger, the same way
//! `ledger_core_tests.rs` exercises the ledger's own handles. No
//! `#![cfg(feature = ...)]` gate: the ledger and its guard have no OS
//! dependency and must compile and pass featureless.

use std::time::{Duration, SystemTime};

use kaish_kernel::ledger::{AttemptGuard, ConditionReport, Ledger, LedgerConfig, SystemClock};
use kaish_types::approval::{
    ApprovalRequest, GrantTerms, LostCause, Outcome, Principal, PrincipalKind, RequestState,
};

/// This file's ledger scope (spec §A.7): a fresh kernel id per ledger, and
/// no session — an unscoped ledger is the single-session shape.
#[allow(dead_code)]
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
    ApprovalRequest::builder(op)
        .risk(kaish_types::approval::RiskClass::Reversible)
        .build()
        .unwrap()
}

fn far_future() -> SystemTime {
    SystemTime::now() + Duration::from_secs(300)
}

/// Drains the outbox without depending on any `pub(crate)` internal:
/// `Approvals::pending()` runs the full sweep, and PR 3 wires the sweep to
/// drain the outbox first (spec §C.1).
fn force_drain(approvals: &kaish_kernel::ledger::Approvals) {
    let _ = approvals.pending(kaish_types::approval::PageRequest::default()).items;
}

#[tokio::test]
async fn dropped_attempt_guard_settles_as_unknown_cancelled_never_an_exit_code() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = requester
        .post_request(draft("plugin.dangerous"), test_origin(agent("agent-1")))
        .await
        .unwrap();
    approver.grant(&req.id, req.revision, GrantTerms::once_for(&req, far_future())).await.unwrap();
    let attempt = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap();

    // The dispatcher's shape: build the guard, then the tool's future is
    // dropped before it ever reports an outcome (cancellation, task abort).
    let guard = AttemptGuard::new(requester.clone(), attempt);
    drop(guard);

    force_drain(&approvals);

    let chain = approvals.get(&req.id).expect("chain must still exist");
    assert_eq!(chain.attempts.len(), 1, "exactly one attempt was reserved");
    let settled = &chain.attempts[0];
    assert!(
        matches!(settled.outcome, Some(Outcome::Unknown { cause: LostCause::Cancelled })),
        "a dropped guard must settle Unknown{{Cancelled}}, got {:?}",
        settled.outcome
    );
    assert!(
        !matches!(settled.outcome, Some(Outcome::Exit(_))),
        "a dropped guard must never settle as an exit code"
    );
    // Unknown closes the chain (spec §B.2) — it stays nominally `Granted`
    // (there is no separate "closed" state) but is not reservable again.
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted));
    let err = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap_err();
    assert!(
        matches!(err, kaish_kernel::ledger::LedgerError::AlreadySettled { .. }),
        "a closed chain must refuse a second reservation, got {err:?}"
    );
}

#[tokio::test]
async fn panicking_tool_future_settles_the_same_way_as_a_drop() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = requester
        .post_request(draft("plugin.dangerous"), test_origin(agent("agent-1")))
        .await
        .unwrap();
    approver.grant(&req.id, req.revision, GrantTerms::once_for(&req, far_future())).await.unwrap();
    let attempt = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap();

    let task_requester = requester.clone();
    let join = tokio::spawn(async move {
        let _guard = AttemptGuard::new(task_requester, attempt);
        // Rust unwinds through `_guard`'s `Drop` on the way out of this
        // frame, the same as a real tool panicking mid-`execute()`.
        panic!("simulated tool panic mid-execution");
    });
    let panicked = join.await;
    assert!(panicked.is_err(), "the spawned task must have panicked");

    force_drain(&approvals);

    let chain = approvals.get(&req.id).expect("chain must still exist");
    let settled = &chain.attempts[0];
    assert!(
        matches!(settled.outcome, Some(Outcome::Unknown { cause: LostCause::Cancelled })),
        "a panicking tool must settle Unknown{{Cancelled}} via the guard's Drop, got {:?}",
        settled.outcome
    );
}

#[tokio::test]
async fn explicit_settle_before_drop_wins_and_the_drop_push_is_a_no_op() {
    let (requester, approvals, approver) = Ledger::build(LedgerConfig::default(), test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();
    let req = requester
        .post_request(draft("plugin.dangerous"), test_origin(agent("agent-1")))
        .await
        .unwrap();
    approver.grant(&req.id, req.revision, GrantTerms::once_for(&req, far_future())).await.unwrap();
    let attempt = requester.redeem(&req.id, agent("agent-1"), ConditionReport::none()).await.unwrap();

    let guard = AttemptGuard::new(requester.clone(), attempt);
    // The dispatcher's normal-return path: settle with the real outcome
    // before the guard drops.
    let appended = guard.settle(Outcome::Exit(0)).await.unwrap();
    assert!(appended);
    drop(guard);
    force_drain(&approvals);

    let chain = approvals.get(&req.id).expect("chain must still exist");
    let settled = &chain.attempts[0];
    assert_eq!(
        settled.outcome,
        Some(Outcome::Exit(0)),
        "the explicit settle must win — the guard's later Drop push must be an idempotent no-op"
    );
}

/// Regression for a gap a review round caught: draining only at the top of
/// `settle`/`redeem`/`redeem_with_token`/`abandon_request`/`sweep` (the
/// methods that read `live_attempt` directly) is not enough. `post_request`
/// reads `live_count_total`/`live_count_by_principal` and reserves ring/sink
/// capacity — both of which a closed-but-undrained chain still occupies —
/// so it needs the same drain, even though it never touches `live_attempt`
/// itself. At `live_capacity: 1`, a dropped guard's queued `Unknown` must
/// not make the very next `post_request` see the ledger as full.
#[tokio::test]
async fn dropped_attempt_guard_does_not_falsely_exhaust_capacity_for_the_next_post() {
    let config = LedgerConfig::default().with_live_capacity(1);
    let (requester, approvals, approver) = Ledger::build(config, test_scope(), None, std::sync::Arc::new(SystemClock)).unwrap();

    let req_a = requester
        .post_request(draft("plugin.dangerous"), test_origin(agent("agent-1")))
        .await
        .unwrap();
    approver.grant(&req_a.id, req_a.revision, GrantTerms::once_for(&req_a, far_future())).await.unwrap();
    let attempt = requester.redeem(&req_a.id, agent("agent-1"), ConditionReport::none()).await.unwrap();

    // Drop without an explicit settle and without any intervening drain
    // (no `force_drain`, no `redeem`/`settle`/`abandon_request` call) —
    // exactly the gap: only `post_request` itself stands between this and
    // the next post.
    drop(AttemptGuard::new(requester.clone(), attempt));

    let req_b = requester
        .post_request(draft("plugin.dangerous"), test_origin(agent("agent-1")))
        .await;
    assert!(
        req_b.is_ok(),
        "post_request must drain the dropped guard's queued settlement itself — \
         request A's chain closed via Unknown and must not still count against \
         live_capacity: 1, got {req_b:?}"
    );

    // Confirm the mechanism, not just the outcome: A really did close.
    assert_eq!(approvals.state(&req_a.id), Some(RequestState::Granted));
    let chain_a = approvals.get(&req_a.id).unwrap();
    assert!(matches!(
        chain_a.attempts[0].outcome,
        Some(Outcome::Unknown { cause: LostCause::Cancelled })
    ));
}
