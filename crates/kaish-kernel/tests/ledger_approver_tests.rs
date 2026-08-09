//! The approval decision chain (`docs/approval-ledger.md`, ledger PR 4).
//!
//! Four stages, tried in order, first non-`Defer` wins (§C.2); standing
//! grants matched all-or-nothing with exact kinds and globbed ids (§C.4);
//! `decide` under a patient hold and never under the ledger lock (§B.1).
//! No `#![cfg(feature = ...)]` gate — the chain has no OS dependency and
//! must compile and pass featureless, like the ledger core it sits on. The
//! one test that needs a live kernel builds a `MemoryFs`-backed one, which
//! also works featureless.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Duration, SystemTime};

use async_trait::async_trait;
use kaish_kernel::ledger::{
    Approver, Approvals, ApproverHandle, ChainContext, ChainOutcome, ChainStage, DecisionChain,
    Ledger, LedgerConfig, LedgerError, Requester,
};
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{ExecContext, ExecuteOptions, Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::approval::{
    ApprovalRequest, ApprovalRequestView, Decision, Grounds, GrantTerms, LedgerEntry,
    OperationPattern, Principal, PrincipalKind, RequestState, Resource,
    ResourcePattern, RiskClass, StandingGrant, StandingId, StateClaim,
};
use kaish_types::{ExecResult, Value};

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


// ─────────────────────────────── fixtures ───────────────────────────────

fn agent(id: &str) -> Principal {
    Principal::new(id, PrincipalKind::Agent)
}

fn operator() -> Principal {
    Principal::new("operator", PrincipalKind::Human)
}

fn far_future() -> SystemTime {
    SystemTime::now() + Duration::from_secs(3_600)
}

/// Post a request with the given operation and resources.
async fn post(requester: &Requester, op: &str, resources: Vec<Resource>) -> ApprovalRequest {
    let mut builder = ApprovalRequest::builder(op).risk(RiskClass::Reversible);
    for resource in resources {
        builder = builder.resource(resource);
    }
    requester
        .post_request(builder.build().unwrap(), test_origin(agent("agent-1")))
        .await
        .unwrap()
}

/// Issue a standing grant covering `operations` and `resources`. `None`
/// keeps the old helper semantics (unlimited) so tests that grant
/// repeatedly stay honest about it; `Some(n)` is an explicit bound.
async fn standing(
    authority: &ApproverHandle,
    operations: &[&str],
    resources: &[(&str, &str)],
    max_uses: Option<u32>,
) -> StandingId {
    let grant = StandingGrant::new(
        operations.iter().map(|o| OperationPattern::new(*o)).collect(),
        resources
            .iter()
            .map(|(kind, pattern)| ResourcePattern::new(*kind, *pattern))
            .collect(),
        None,
        None,
        operator(),
        "test rule",
    );
    let grant = match max_uses {
        Some(n) => grant.with_max_uses(n),
        None => grant.unlimited_uses(),
    };
    authority.grant_standing(grant).await.unwrap()
}

/// What a gate site does with a chain outcome — the exit-code mapping
/// `ApprovalOutcome::proceed()` owns once PR 3 lands (spec §C.1). Kept here
/// so this PR can assert the exit-2 contract without reaching into a
/// surface it does not own.
fn gate_result(outcome: &ChainOutcome, request: &ApprovalRequest) -> ExecResult {
    match outcome {
        ChainOutcome::Granted { .. } => ExecResult::success("proceeded"),
        ChainOutcome::Denied { reason, .. } => ExecResult::failure(1, format!("denied: {reason}")),
        ChainOutcome::Deferred => ExecResult::failure(
            2,
            format!("pending approval {} — an operator must grant it", request.id),
        ),
        ChainOutcome::Cancelled => ExecResult::failure(130, "interrupted"),
        _ => ExecResult::failure(1, "unknown approval outcome"),
    }
}

/// An approver scripted per stage, recording every call it receives in
/// order so a test can assert which stages fired and which did not.
struct ScriptedApprover {
    policy: Mutex<Option<Decision>>,
    decide: Mutex<Option<Decision>>,
    calls: Mutex<Vec<&'static str>>,
    seen: Mutex<Vec<ApprovalRequestView>>,
}

impl ScriptedApprover {
    fn new(policy: Decision, decide: Decision) -> Arc<Self> {
        Arc::new(Self {
            policy: Mutex::new(Some(policy)),
            decide: Mutex::new(Some(decide)),
            calls: Mutex::new(Vec::new()),
            seen: Mutex::new(Vec::new()),
        })
    }

    fn calls(&self) -> Vec<&'static str> {
        self.calls.lock().unwrap().clone()
    }
}

#[async_trait]
impl Approver for ScriptedApprover {
    fn policy(&self, req: &ApprovalRequestView, _ledger: &Approvals) -> Decision {
        self.calls.lock().unwrap().push("policy");
        self.seen.lock().unwrap().push(req.clone());
        self.policy.lock().unwrap().clone().unwrap_or(Decision::Defer)
    }

    async fn decide(&self, req: &ApprovalRequestView) -> Decision {
        self.calls.lock().unwrap().push("decide");
        self.seen.lock().unwrap().push(req.clone());
        self.decide.lock().unwrap().clone().unwrap_or(Decision::Defer)
    }

    fn principal(&self) -> Principal {
        Principal::new("clearance-model", PrincipalKind::Automation)
    }
}

/// Build a ledger and a chain over it, with an optional approver.
fn chain_over(approver: Option<Arc<dyn Approver>>) -> (Requester, Approvals, ApproverHandle, DecisionChain) {
    let (requester, approvals, authority) = Ledger::build(LedgerConfig::default(), test_scope(), None).unwrap();
    let chain = DecisionChain::new(authority.clone(), approvals.clone(), approver);
    (requester, approvals, authority, chain)
}

// ─────────────────────── §C.2 the four stages, in order ───────────────────────

#[tokio::test]
async fn stages_fire_in_order_and_a_policy_grant_short_circuits_decide() {
    let approver = ScriptedApprover::new(
        Decision::Grant(GrantTerms::once_for(
            &ApprovalRequest::builder("fs.write")
                .risk(RiskClass::Reversible)
                .build()
                .unwrap()
                .stamp(
                    kaish_types::approval::RequestId::new(0, 0),
                    SystemTime::now(),
                    test_origin(agent("agent-1")),
                ),
            far_future(),
        )),
        Decision::Deny {
            reason: "decide must never run".into(),
        },
    );
    let (requester, approvals, _authority, chain) = chain_over(Some(approver.clone()));
    let request = post(&requester, "fs.write", vec![]).await;

    let outcome = chain.decide(&request, &ChainContext::detached()).await.unwrap();
    assert!(
        matches!(
            outcome,
            ChainOutcome::Granted {
                stage: ChainStage::Policy,
                ..
            }
        ),
        "policy must decide, got {outcome:?}"
    );
    assert_eq!(approver.calls(), vec!["policy"], "decide must not run after policy decided");
    assert_eq!(approvals.state(&request.id), Some(RequestState::Granted));
}

#[tokio::test]
async fn a_standing_grant_short_circuits_both_hooks() {
    let approver = ScriptedApprover::new(
        Decision::Deny {
            reason: "policy must never run".into(),
        },
        Decision::Defer,
    );
    let (requester, approvals, authority, chain) = chain_over(Some(approver.clone()));
    standing(&authority, &["fs.write"], &[("path", "/workspace/*")], None).await;
    let request = post(
        &requester,
        "fs.write",
        vec![Resource::plain("path", "/workspace/a")],
    )
    .await;

    let outcome = chain.decide(&request, &ChainContext::detached()).await.unwrap();
    assert!(
        matches!(
            outcome,
            ChainOutcome::Granted {
                stage: ChainStage::Standing,
                ..
            }
        ),
        "the standing grant must decide, got {outcome:?}"
    );
    assert!(approver.calls().is_empty(), "no hook may run after stage 1 decided");
    assert_eq!(approvals.state(&request.id), Some(RequestState::Granted));
}

#[tokio::test]
async fn decide_runs_only_after_policy_defers_and_its_denial_is_recorded() {
    let approver = ScriptedApprover::new(
        Decision::Defer,
        Decision::Deny {
            reason: "not on my watch".into(),
        },
    );
    let (requester, approvals, _authority, chain) = chain_over(Some(approver.clone()));
    let request = post(&requester, "fs.remove", vec![]).await;

    let outcome = chain.decide(&request, &ChainContext::detached()).await.unwrap();
    assert!(
        matches!(
            &outcome,
            ChainOutcome::Denied {
                stage: ChainStage::Decide,
                reason
            } if reason == "not on my watch"
        ),
        "decide must deny, got {outcome:?}"
    );
    assert_eq!(approver.calls(), vec!["policy", "decide"]);
    assert_eq!(approvals.state(&request.id), Some(RequestState::Denied));
    assert_eq!(gate_result(&outcome, &request).code, 1);
}

#[tokio::test]
async fn defer_through_all_four_stages_is_exit_2_with_a_pending_view() {
    let approver = ScriptedApprover::new(Decision::Defer, Decision::Defer);
    let (requester, approvals, _authority, chain) = chain_over(Some(approver.clone()));
    let request = post(&requester, "fs.remove", vec![Resource::plain("path", "/etc/hosts")]).await;

    let outcome = chain.decide(&request, &ChainContext::detached()).await.unwrap();
    assert_eq!(outcome, ChainOutcome::Deferred);
    assert_eq!(approver.calls(), vec!["policy", "decide"]);

    // Stage 4: the request stays `Requested`, nothing was decided, and the
    // gate site returns exit 2 carrying the pending view.
    assert_eq!(approvals.state(&request.id), Some(RequestState::Requested));
    let pending = approvals.pending();
    assert_eq!(pending.len(), 1);
    assert_eq!(pending[0].id, request.id);
    let result = gate_result(&outcome, &request);
    assert_eq!(result.code, 2, "deferring all the way through is exit 2");
    assert!(result.err.contains(&request.id.to_string()));
    // Nothing beyond the caller's own `Requested` entry was appended.
    assert_eq!(entries(approvals.log(0)).len(), 1);
}

#[tokio::test]
async fn a_kernel_with_no_approver_defers_exactly_as_today() {
    let (requester, approvals, _authority, chain) = chain_over(None);
    assert!(!chain.has_approver());
    let request = post(&requester, "fs.remove", vec![]).await;

    let outcome = chain.decide(&request, &ChainContext::detached()).await.unwrap();
    assert_eq!(outcome, ChainOutcome::Deferred);
    assert_eq!(approvals.state(&request.id), Some(RequestState::Requested));
    assert_eq!(gate_result(&outcome, &request).code, 2);
}

// ─────────────────────── §C.4 standing-grant matching ───────────────────────

#[tokio::test]
async fn a_standing_grant_covering_three_of_four_resources_defers() {
    let (requester, approvals, authority, chain) = chain_over(None);
    standing(&authority, &["fs.write"], &[("path", "/workspace/*")], None).await;
    let request = post(
        &requester,
        "fs.write",
        vec![
            Resource::plain("path", "/workspace/a"),
            Resource::plain("path", "/workspace/b"),
            Resource::plain("path", "/workspace/c"),
            Resource::plain("path", "/etc/passwd"),
        ],
    )
    .await;

    let outcome = chain.decide(&request, &ChainContext::detached()).await.unwrap();
    assert_eq!(
        outcome,
        ChainOutcome::Deferred,
        "all-or-nothing: partial coverage must not auto-approve the covered three"
    );
    assert_eq!(approvals.state(&request.id), Some(RequestState::Requested));
}

#[tokio::test]
async fn kind_matches_exactly_and_only_the_id_globs() {
    let (requester, _approvals, authority, chain) = chain_over(None);
    standing(&authority, &["git.commit"], &[("git.ref", "refs/heads/agent/*")], None).await;

    let matching = post(
        &requester,
        "git.commit",
        vec![Resource::plain("git.ref", "refs/heads/agent/work")],
    )
    .await;
    assert!(matches!(
        chain.decide(&matching, &ChainContext::detached()).await.unwrap(),
        ChainOutcome::Granted { .. }
    ));

    // Same id, different kind: the kind is compared, never globbed.
    let wrong_kind = post(
        &requester,
        "git.commit",
        vec![Resource::plain("path", "refs/heads/agent/work")],
    )
    .await;
    assert_eq!(
        chain.decide(&wrong_kind, &ChainContext::detached()).await.unwrap(),
        ChainOutcome::Deferred
    );

    // Same kind, id outside the glob.
    let wrong_id = post(
        &requester,
        "git.commit",
        vec![Resource::plain("git.ref", "refs/heads/main")],
    )
    .await;
    assert_eq!(
        chain.decide(&wrong_id, &ChainContext::detached()).await.unwrap(),
        ChainOutcome::Deferred
    );
}

#[tokio::test]
async fn a_standing_grant_copies_the_requests_transitions_into_the_grants_conditions() {
    let (requester, approvals, authority, chain) = chain_over(None);
    let rule = standing(&authority, &["git.commit"], &[("git.ref", "refs/heads/agent/*")], None).await;
    let request = post(
        &requester,
        "git.commit",
        vec![
            Resource::transition(
                "git.ref",
                "refs/heads/agent/work",
                StateClaim::Exact("a1b2c3d".into()),
                StateClaim::Exact("c3d4e5f".into()),
            ),
            Resource::plain("git.ref", "refs/heads/agent/notes"),
        ],
    )
    .await;

    let outcome = chain.decide(&request, &ChainContext::detached()).await.unwrap();
    let ChainOutcome::Granted { grant, stage } = outcome else {
        panic!("expected a standing grant to fire");
    };
    assert_eq!(stage, ChainStage::Standing);
    assert_eq!(grant.grounds, Grounds::Standing { grant: rule });
    // The decider is whoever wrote the rule, not whoever holds the handle.
    assert_eq!(grant.decided_by, operator());
    // Exactly the declared transition, conditioned; the resource with no
    // transition contributes no condition.
    assert_eq!(grant.conditions.len(), 1);
    assert_eq!(grant.conditions[0].resource.kind, "git.ref");
    assert_eq!(grant.conditions[0].resource.id, "refs/heads/agent/work");
    assert_eq!(
        grant.conditions[0].expected_from,
        StateClaim::Exact("a1b2c3d".into())
    );
    assert!(matches!(entries(approvals.log(0)).last(), Some(LedgerEntry::Granted { .. })));
}

#[tokio::test]
async fn an_auto_approval_never_outlives_the_rule_that_produced_it() {
    let (requester, _approvals, authority, chain) = chain_over(None);
    let rule_expiry = SystemTime::now() + Duration::from_secs(30);
    authority
        .grant_standing(StandingGrant::new(
            vec![OperationPattern::new("fs.write")],
            vec![ResourcePattern::new("path", "*")],
            None,
            Some(rule_expiry),
            operator(),
            "short-lived rule",
        ))
        .await
        .unwrap();
    let request = post(&requester, "fs.write", vec![Resource::plain("path", "/x")]).await;

    let ChainOutcome::Granted { grant, .. } = chain.decide(&request, &ChainContext::detached()).await.unwrap()
    else {
        panic!("expected a grant");
    };
    assert!(
        grant.not_after <= rule_expiry,
        "grant not_after {:?} must be clamped to the rule's expiry {rule_expiry:?}",
        grant.not_after
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 8)]
async fn max_uses_consumption_is_exact_under_eight_concurrent_matching_requests() {
    const MAX_USES: u32 = 3;
    const REQUESTS: usize = 8;

    let (requester, approvals, authority, chain) = chain_over(None);
    let rule = standing(
        &authority,
        &["fs.write"],
        &[("path", "/workspace/*")],
        Some(MAX_USES),
    )
    .await;

    let mut requests = Vec::new();
    for _ in 0..REQUESTS {
        requests.push(post(&requester, "fs.write", vec![Resource::plain("path", "/workspace/a")]).await);
    }

    let chain = Arc::new(chain);
    let mut handles = Vec::new();
    for request in requests {
        let chain = Arc::clone(&chain);
        handles.push(tokio::spawn(async move {
            chain.decide(&request, &ChainContext::detached()).await.unwrap()
        }));
    }
    let mut granted = 0usize;
    let mut deferred = 0usize;
    for handle in handles {
        match handle.await.unwrap() {
            ChainOutcome::Granted { stage, .. } => {
                assert_eq!(stage, ChainStage::Standing);
                granted += 1;
            }
            ChainOutcome::Deferred => deferred += 1,
            other => panic!("unexpected outcome {other:?}"),
        }
    }

    assert_eq!(granted, MAX_USES as usize, "exactly max_uses requests may be granted");
    assert_eq!(deferred, REQUESTS - MAX_USES as usize);
    assert_eq!(authority.standing_uses(&rule), MAX_USES);

    // The log agrees with the counter: one `Granted{Standing}` per use.
    let from_log = entries(approvals.log(0))
        .into_iter()
        .filter(|entry| {
            matches!(entry, LedgerEntry::Granted { grant, .. } if grant.grounds == Grounds::Standing { grant: rule })
        })
        .count();
    assert_eq!(from_log, MAX_USES as usize);
}

#[tokio::test]
async fn an_exhausted_rule_stops_matching_and_the_request_falls_through() {
    let approver = ScriptedApprover::new(Decision::Defer, Decision::Defer);
    let (requester, _approvals, authority, chain) = chain_over(Some(approver.clone()));
    standing(&authority, &["fs.write"], &[("path", "*")], Some(1)).await;

    let first = post(&requester, "fs.write", vec![Resource::plain("path", "/x")]).await;
    assert!(matches!(
        chain.decide(&first, &ChainContext::detached()).await.unwrap(),
        ChainOutcome::Granted { .. }
    ));
    assert!(approver.calls().is_empty());

    let second = post(&requester, "fs.write", vec![Resource::plain("path", "/x")]).await;
    assert_eq!(
        chain.decide(&second, &ChainContext::detached()).await.unwrap(),
        ChainOutcome::Deferred
    );
    assert_eq!(
        approver.calls(),
        vec!["policy", "decide"],
        "exhaustion must fall through to the later stages, not fail"
    );
}

#[tokio::test]
async fn when_several_rules_match_the_earliest_issued_one_wins_and_is_the_only_one_charged() {
    let (requester, _approvals, authority, chain) = chain_over(None);
    let first = standing(&authority, &["fs.write"], &[("path", "/workspace/*")], Some(5)).await;
    let second = standing(&authority, &["fs.*"], &[("path", "*")], Some(5)).await;
    let request = post(&requester, "fs.write", vec![Resource::plain("path", "/workspace/a")]).await;

    let ChainOutcome::Granted { grant, .. } = chain.decide(&request, &ChainContext::detached()).await.unwrap()
    else {
        panic!("expected a grant");
    };
    assert_eq!(grant.grounds, Grounds::Standing { grant: first });
    assert_eq!(authority.standing_uses(&first), 1);
    assert_eq!(authority.standing_uses(&second), 0);
}

#[tokio::test]
async fn a_revoked_rule_stops_matching_immediately() {
    let (requester, _approvals, authority, chain) = chain_over(None);
    let rule = standing(&authority, &["fs.write"], &[("path", "*")], None).await;
    authority.revoke_standing(&rule, "no longer needed").await.unwrap();
    let request = post(&requester, "fs.write", vec![Resource::plain("path", "/x")]).await;

    assert_eq!(
        chain.decide(&request, &ChainContext::detached()).await.unwrap(),
        ChainOutcome::Deferred
    );
}

// ─────────────────────── §B.1 the lock is never held across a hook ───────────────────────

/// An approver that reads the ledger from inside both hooks. If the chain
/// held the ledger lock across either call this would deadlock on a
/// re-entrant `std::sync::Mutex` acquisition — which is the assertion.
struct LedgerReadingApprover {
    approvals: OnceLock<Approvals>,
    pending_seen: AtomicUsize,
}

#[async_trait]
impl Approver for LedgerReadingApprover {
    fn policy(&self, _req: &ApprovalRequestView, ledger: &Approvals) -> Decision {
        // Stage 2 gets the read side as an argument and may use it freely.
        self.pending_seen.fetch_add(ledger.pending().len(), Ordering::SeqCst);
        Decision::Defer
    }

    async fn decide(&self, _req: &ApprovalRequestView) -> Decision {
        let approvals = self.approvals.get().expect("approvals installed");
        self.pending_seen.fetch_add(approvals.pending().len(), Ordering::SeqCst);
        let _ = entries(approvals.log(0));
        let _ = approvals.standing();
        Decision::Defer
    }
}

#[tokio::test]
async fn neither_hook_is_invoked_while_the_ledger_lock_is_held() {
    let approver = Arc::new(LedgerReadingApprover {
        approvals: OnceLock::new(),
        pending_seen: AtomicUsize::new(0),
    });
    let (requester, approvals, authority, chain) = chain_over(Some(approver.clone()));
    approver.approvals.set(approvals.clone()).ok();
    // A live standing rule so stage 1 really does take the lock first.
    standing(&authority, &["nothing.matches"], &[], None).await;
    let request = post(&requester, "fs.remove", vec![]).await;

    // A deadlock is a hang, not a failure, so bound it: the whole chain must
    // resolve well inside this budget.
    let outcome = tokio::time::timeout(
        Duration::from_secs(10),
        chain.decide(&request, &ChainContext::detached()),
    )
    .await
    .expect("the chain must not deadlock — a hook ran under the ledger lock")
    .unwrap();

    assert_eq!(outcome, ChainOutcome::Deferred);
    assert_eq!(
        approver.pending_seen.load(Ordering::SeqCst),
        2,
        "both hooks must have read the ledger and seen the one pending request"
    );
}

// ─────────────────────── §C.2 stage 3: patient hold and cancellation ───────────────────────

/// An approver that takes `delay` to make up its mind.
struct SlowApprover {
    delay: Duration,
    decision: Decision,
    budget: Duration,
    started: Arc<AtomicUsize>,
}

#[async_trait]
impl Approver for SlowApprover {
    async fn decide(&self, _req: &ApprovalRequestView) -> Decision {
        self.started.fetch_add(1, Ordering::SeqCst);
        tokio::time::sleep(self.delay).await;
        self.decision.clone()
    }

    fn decide_budget(&self) -> Duration {
        self.budget
    }
}

/// `gate <operation>`: post a request through the kernel's own requester,
/// run the kernel's own decision chain against it, and return what a gate
/// site would. This is the driver PR 3's `ToolCtx::request_approval`
/// replaces.
struct GateTool {
    kernel: Arc<OnceLock<std::sync::Weak<Kernel>>>,
}

#[async_trait]
impl Tool for GateTool {
    fn name(&self) -> &str {
        "gate"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("gate", "test tool: run the approval decision chain")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let operation = match args.get_positional(0) {
            Some(Value::String(s)) => s.clone(),
            other => return ExecResult::failure(2, format!("gate: bad operation {other:?}")),
        };
        let Some(kernel) = self.kernel.get().and_then(std::sync::Weak::upgrade) else {
            return ExecResult::failure(1, "gate: no kernel");
        };
        let request = match kernel
            .requester()
            .post_request(
                ApprovalRequest::builder(operation)
                    .risk(RiskClass::Irreversible)
                    .build()
                    .expect("draft"),
                test_origin(kernel.principal().clone()).with_ttl(Duration::from_secs(600)),
            )
            .await
        {
            Ok(request) => request,
            Err(err) => return ExecResult::failure(1, format!("gate: {err}")),
        };

        let Some(exec_ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "gate requires ExecContext");
        };
        let cancel = exec_ctx.cancel.clone();
        let chain_ctx = ChainContext::new(exec_ctx, cancel);
        match kernel.decision_chain().decide(&request, &chain_ctx).await {
            Ok(outcome) => gate_result(&outcome, &request),
            Err(err) => ExecResult::failure(1, format!("gate: {err}")),
        }
    }
}

/// A `MemoryFs`-backed kernel with the `gate` tool registered, sharing
/// `authority`'s ledger.
fn gate_kernel(approver: Arc<dyn Approver>, authority: ApproverHandle) -> Arc<Kernel> {
    let cell: Arc<OnceLock<std::sync::Weak<Kernel>>> = Arc::new(OnceLock::new());
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let config = KernelConfig::isolated()
        .with_approver(approver)
        .with_approver_handle(authority)
        .with_principal(agent("agent-1"));
    let kernel = Kernel::with_backend(backend, config, |_| {}, {
        let cell = Arc::clone(&cell);
        move |tools| {
            tools.register(GateTool { kernel: cell });
        }
    })
    .expect("with_backend kernel")
    .into_arc();
    cell.set(Arc::downgrade(&kernel)).ok();
    kernel
}

/// A 90-second decision under a patient hold does not trip a 30-second
/// script timeout — the whole reason stage 3 holds one (spec §C.2).
#[tokio::test(start_paused = true)]
async fn a_ninety_second_decision_does_not_trip_a_thirty_second_script_timeout() {
    let (_requester, _approvals, authority) = Ledger::build(LedgerConfig::default(), test_scope(), None).unwrap();
    let started = Arc::new(AtomicUsize::new(0));
    let approver = Arc::new(SlowApprover {
        delay: Duration::from_secs(90),
        decision: Decision::Grant(GrantTerms::once_for(
            &ApprovalRequest::builder("fs.remove")
                .risk(RiskClass::Irreversible)
                .build()
                .unwrap()
                .stamp(
                    kaish_types::approval::RequestId::new(0, 0),
                    SystemTime::now(),
                    test_origin(agent("agent-1")).with_ttl(Duration::from_secs(600)),
                ),
            SystemTime::now() + Duration::from_secs(3_600),
        )),
        budget: Duration::from_secs(300),
        started: Arc::clone(&started),
    });
    let kernel = gate_kernel(approver, authority);

    let result = kernel
        .execute_with_options(
            "gate fs.remove",
            ExecuteOptions::new().with_timeout(Duration::from_secs(30)),
        )
        .await
        .expect("execute");

    assert_eq!(started.load(Ordering::SeqCst), 1, "decide must have run");
    assert_eq!(
        result.code, 0,
        "a decision under the patient hold must outlive the script timeout; got code={} err={}",
        result.code, result.err
    );
}

/// The hold is bounded by the approver's own budget: a decision that
/// overruns it still trips the watchdog, so a hung approver cannot wait
/// forever.
#[tokio::test(start_paused = true)]
async fn a_decision_that_overruns_its_own_budget_still_trips_the_watchdog() {
    let (_requester, approvals, authority) = Ledger::build(LedgerConfig::default(), test_scope(), None).unwrap();
    let approver = Arc::new(SlowApprover {
        delay: Duration::from_secs(3_600),
        decision: Decision::Deny {
            reason: "never reached".into(),
        },
        budget: Duration::from_secs(60),
        started: Arc::new(AtomicUsize::new(0)),
    });
    let kernel = gate_kernel(approver, authority);

    let result = kernel
        .execute_with_options(
            "gate fs.remove",
            ExecuteOptions::new().with_timeout(Duration::from_secs(30)),
        )
        .await
        .expect("execute");

    assert_eq!(result.code, 124, "got code={} err={}", result.code, result.err);
    // Nothing was decided: the request is still pending, with no decision
    // entry behind it.
    assert_eq!(approvals.pending().len(), 1);
    // One `Requested`, plus the statement tap's unconditional `Observed`
    // (spec §C.6) for the `gate fs.remove` line itself.
    let chain_entries: Vec<_> = entries(approvals.log(0))
        .into_iter()
        .filter(|e| !matches!(e, LedgerEntry::Observed { .. }))
        .collect();
    assert_eq!(chain_entries.len(), 1, "{chain_entries:?}");
}

/// Cancellation during `decide` posts nothing and never grants (spec §C.2).
#[tokio::test]
async fn cancellation_during_decide_posts_nothing_and_never_grants() {
    let (requester, approvals, _authority, chain) = chain_over(Some(Arc::new(SlowApprover {
        delay: Duration::from_secs(3_600),
        decision: Decision::Grant(GrantTerms::once_for(
            &ApprovalRequest::builder("fs.remove")
                .risk(RiskClass::Irreversible)
                .build()
                .unwrap()
                .stamp(
                    kaish_types::approval::RequestId::new(0, 0),
                    SystemTime::now(),
                    test_origin(agent("agent-1")).with_ttl(Duration::from_secs(600)),
                ),
            far_future(),
        )),
        budget: Duration::from_secs(300),
        started: Arc::new(AtomicUsize::new(0)),
    })));
    let request = post(&requester, "fs.remove", vec![]).await;

    let cancel = tokio_util::sync::CancellationToken::new();
    let waiter = cancel.clone();
    tokio::spawn(async move {
        tokio::time::sleep(Duration::from_millis(50)).await;
        waiter.cancel();
    });

    let outcome = chain
        .decide(&request, &ChainContext::new(&NoPatient, cancel))
        .await
        .unwrap();

    assert_eq!(outcome, ChainOutcome::Cancelled);
    assert_eq!(
        approvals.state(&request.id),
        Some(RequestState::Requested),
        "a cancelled decision must leave the request exactly as posted"
    );
    assert_eq!(entries(approvals.log(0)).len(), 1, "nothing beyond the Requested entry was posted");
    assert_eq!(gate_result(&outcome, &request).code, 130);
}

/// An already-cancelled execution never reaches a hook at all — an
/// execution on its way out must not acquire authority.
#[tokio::test]
async fn an_already_cancelled_execution_is_never_granted() {
    let approver = ScriptedApprover::new(
        Decision::Deny {
            reason: "must not run".into(),
        },
        Decision::Defer,
    );
    let (requester, approvals, authority, chain) = chain_over(Some(approver.clone()));
    standing(&authority, &["fs.write"], &[("path", "*")], None).await;
    let request = post(&requester, "fs.write", vec![Resource::plain("path", "/x")]).await;

    let cancel = tokio_util::sync::CancellationToken::new();
    cancel.cancel();
    let outcome = chain
        .decide(&request, &ChainContext::new(&NoPatient, cancel))
        .await
        .unwrap();

    assert_eq!(outcome, ChainOutcome::Cancelled);
    assert!(approver.calls().is_empty());
    assert_eq!(
        approvals.state(&request.id),
        Some(RequestState::Requested),
        "not even the standing rule may fire for a cancelled execution"
    );
}

/// An approver that fires the cancellation token from inside the narrowest
/// window there is: `principal()`, which the chain calls *after* its
/// post-decide cancellation check and *before* the grant reaches the
/// ledger. Nothing a timing-based test can hit reliably — this makes the
/// race deterministic.
struct CancelsWhileGranting {
    cancel: tokio_util::sync::CancellationToken,
    decision: Decision,
    from_policy: bool,
}

#[async_trait]
impl Approver for CancelsWhileGranting {
    fn policy(&self, _req: &ApprovalRequestView, _ledger: &Approvals) -> Decision {
        if self.from_policy {
            self.decision.clone()
        } else {
            Decision::Defer
        }
    }

    async fn decide(&self, _req: &ApprovalRequestView) -> Decision {
        if self.from_policy {
            Decision::Defer
        } else {
            self.decision.clone()
        }
    }

    fn principal(&self) -> Principal {
        // The chain has already decided to grant and has not yet posted.
        self.cancel.cancel();
        Principal::new("racing-approver", PrincipalKind::Automation)
    }
}

fn grant_terms_for(operation: &str) -> Decision {
    Decision::Grant(GrantTerms::once_for(
        &ApprovalRequest::builder(operation)
            .risk(RiskClass::Irreversible)
            .build()
            .unwrap()
            .stamp(
                kaish_types::approval::RequestId::new(0, 0),
                SystemTime::now(),
                test_origin(agent("agent-1")).with_ttl(Duration::from_secs(600)),
            ),
        far_future(),
    ))
}

/// A cancellation that lands *after* the chain's own check and *before* the
/// grant commits must still not leave a live grant. The window cannot be
/// closed — the ledger knows nothing of cancellation tokens — so the
/// guarantee is stated as an outcome: the grant is undone, the request ends
/// `Abandoned`, and the caller is told `Cancelled`.
#[tokio::test]
async fn a_cancellation_that_beats_the_commit_leaves_no_live_grant() {
    for from_policy in [true, false] {
        let cancel = tokio_util::sync::CancellationToken::new();
        let approver = Arc::new(CancelsWhileGranting {
            cancel: cancel.clone(),
            decision: grant_terms_for("fs.remove"),
            from_policy,
        });
        let (requester, approvals, _authority, chain) = chain_over(Some(approver));
        let request = post(&requester, "fs.remove", vec![]).await;

        let outcome = chain
            .decide(&request, &ChainContext::new(&NoPatient, cancel))
            .await
            .unwrap();

        assert_eq!(
            outcome,
            ChainOutcome::Cancelled,
            "from_policy={from_policy}: a cancelled execution is never told it may proceed"
        );
        assert_eq!(
            approvals.state(&request.id),
            Some(RequestState::Abandoned),
            "from_policy={from_policy}: the grant that beat the cancellation must be undone"
        );
        // The record keeps both halves: the decision, and its undoing.
        let log = entries(approvals.log(0));
        assert!(log.iter().any(|e| matches!(e, LedgerEntry::Granted { .. })));
        assert!(log.iter().any(|e| matches!(e, LedgerEntry::Abandoned { .. })));
        // And the credential is gone — an abandoned chain is closed.
        assert_eq!(gate_result(&outcome, &request).code, 130);
    }
}

/// A cancellation racing a *denial* leaves the denial standing: it
/// authorizes nothing, and erasing it would lose a real decision.
#[tokio::test]
async fn a_cancellation_racing_a_denial_keeps_the_denial() {
    let cancel = tokio_util::sync::CancellationToken::new();
    let approver = Arc::new(CancelsWhileGranting {
        cancel: cancel.clone(),
        decision: Decision::Deny {
            reason: "denied on the way out".into(),
        },
        from_policy: true,
    });
    let (requester, approvals, _authority, chain) = chain_over(Some(approver));
    let request = post(&requester, "fs.remove", vec![]).await;

    let outcome = chain
        .decide(&request, &ChainContext::new(&NoPatient, cancel))
        .await
        .unwrap();

    assert!(matches!(outcome, ChainOutcome::Denied { .. }), "got {outcome:?}");
    assert_eq!(approvals.state(&request.id), Some(RequestState::Denied));
}

/// A `PatientSource` with no watchdog behind it — the shape a non-kernel
/// caller has.
struct NoPatient;

impl kaish_kernel::ledger::PatientSource for NoPatient {
    fn patient(&self, _budget: Duration) -> kaish_tool_api::PatientGuard {
        kaish_tool_api::PatientGuard::inert()
    }
}

// ─────────────────────── §A.4 an approver may narrow, never widen ───────────────────────

#[tokio::test]
async fn an_approver_that_drops_a_declared_condition_is_refused() {
    // The approver grants unconditioned terms for a request that declared a
    // transition — that authorizes more than was asked for.
    let approver = ScriptedApprover::new(
        Decision::Grant(GrantTerms::once_for(
            &ApprovalRequest::builder("git.push")
                .risk(RiskClass::Irreversible)
                .build()
                .unwrap()
                .stamp(
                    kaish_types::approval::RequestId::new(0, 0),
                    SystemTime::now(),
                    test_origin(agent("agent-1")),
                ),
            far_future(),
        )),
        Decision::Defer,
    );
    let (requester, approvals, _authority, chain) = chain_over(Some(approver));
    let request = post(
        &requester,
        "git.push",
        vec![Resource::transition(
            "git.ref",
            "refs/heads/main",
            StateClaim::Exact("a1b2c3d".into()),
            StateClaim::Exact("c3d4e5f".into()),
        )],
    )
    .await;

    let err = chain
        .decide(&request, &ChainContext::detached())
        .await
        .expect_err("a widening grant must be refused");
    assert!(
        matches!(&err, LedgerError::ConditionsWidened { request: id, .. } if id == &request.id),
        "got {err:?}"
    );
    assert_eq!(
        approvals.state(&request.id),
        Some(RequestState::Requested),
        "a refused grant posts nothing"
    );
}

// ─────────────────────── §D.2 the authority capability ───────────────────────

#[tokio::test]
async fn kernel_build_mints_one_authority_and_a_plain_session_holds_none() {
    let (kernel, authority) = Kernel::build(KernelConfig::isolated()).expect("build");
    assert!(
        kernel.session_authority().is_none(),
        "a session built without a handle must hold no authority"
    );
    // The minted authority works: it can decide against this kernel's ledger.
    let request = post(kernel.requester(), "fs.remove", vec![]).await;
    assert_eq!(kernel.approvals().pending().len(), 1);
    authority
        .grant(&request.id, GrantTerms::once_for(&request, far_future()))
        .await
        .unwrap();
    assert_eq!(kernel.approvals().state(&request.id), Some(RequestState::Granted));
}

#[tokio::test]
async fn with_deny_self_approval_wires_kernelconfig_through_to_the_minted_ledger() {
    // `post()` always posts as `agent("agent-1")` — re-attribute the minted
    // authority to that same principal so this exercises the self-approval
    // path, proving `KernelConfig::with_deny_self_approval` actually reaches
    // the `LedgerConfig` `Kernel::build` mints the ledger with, not just the
    // ledger-core `deny_self_approval` field tested directly elsewhere.
    let (kernel, authority) = Kernel::build(KernelConfig::isolated().with_deny_self_approval(true)).expect("build");
    let request = post(kernel.requester(), "fs.remove", vec![]).await;
    let err = authority
        .with_principal(agent("agent-1"))
        .grant(&request.id, GrantTerms::once_for(&request, far_future()))
        .await
        .unwrap_err();
    assert!(
        matches!(err, LedgerError::SelfApproval { .. }),
        "KernelConfig::with_deny_self_approval must reach the minted ledger's config, got {err:?}"
    );
}

#[tokio::test]
async fn a_session_given_a_handle_holds_authority_and_joins_its_ledger() {
    let (_requester, approvals, authority) = Ledger::build(LedgerConfig::default(), test_scope(), None).unwrap();
    let kernel = Kernel::new(
        KernelConfig::isolated()
            .with_approver_handle(authority.clone())
            .with_principal(operator()),
    )
    .expect("new");

    assert!(kernel.session_authority().is_some());
    assert_eq!(kernel.principal(), &operator());
    // One ledger, two views of it.
    let request = post(kernel.requester(), "fs.remove", vec![]).await;
    assert_eq!(approvals.pending().len(), 1, "the kernel joined the supplied ledger");
    assert_eq!(approvals.pending()[0].id, request.id);
}

#[tokio::test]
async fn a_configured_principal_is_the_one_recorded_on_a_grant() {
    let (kernel, authority) = Kernel::build(KernelConfig::isolated().with_principal(operator())).expect("build");
    let request = post(kernel.requester(), "fs.remove", vec![]).await;
    authority
        .grant(&request.id, GrantTerms::once_for(&request, far_future()))
        .await
        .unwrap();

    let chain = kernel.approvals().get(&request.id).expect("chain");
    let grant = chain.grant.expect("grant");
    assert_eq!(
        grant.decided_by,
        operator(),
        "the configured principal replaces the ledger's placeholder identity"
    );
}

#[tokio::test]
async fn a_fork_shares_its_parents_ledger() {
    let (kernel, _authority) = Kernel::build(KernelConfig::isolated()).expect("build");
    let kernel = kernel.into_arc();
    let fork = kernel.fork().await;
    post(fork.requester(), "fs.remove", vec![]).await;
    assert_eq!(
        kernel.approvals().pending().len(),
        1,
        "a background job's requests must land in the log the foreground reads"
    );
}

// ─────────────────────── §A.2 an approver has no path to a credential ───────────────────────

#[tokio::test]
async fn an_approver_receives_the_tokenless_view_of_the_request_it_decides() {
    let approver = ScriptedApprover::new(Decision::Defer, Decision::Defer);
    let (requester, _approvals, _authority, chain) = chain_over(Some(approver.clone()));
    let request = post(
        &requester,
        "git.push",
        vec![Resource::plain("git.ref", "refs/heads/main")],
    )
    .await;

    chain.decide(&request, &ChainContext::detached()).await.unwrap();

    let seen = approver.seen.lock().unwrap().clone();
    assert_eq!(seen.len(), 2, "both hooks saw the request");
    for view in seen {
        assert_eq!(view, ApprovalRequestView::from(&request));
    }
}

/// The structural half of "an `Approver` has no path to a credential": the
/// trait's own definition mentions no credential type, so there is nothing
/// for an implementor to reach through. The other half is the `compile_fail`
/// doctest on the trait itself, which proves the view carries no such field.
/// `trybuild` was judged not worth a workspace dependency for this class of
/// assertion; PR 2 made the same call.
#[test]
fn the_approver_trait_names_no_credential_type() {
    let source = include_str!("../src/ledger/approver.rs");
    let start = source
        .find("pub trait Approver: Send + Sync {")
        .expect("the Approver trait must exist");
    let body = &source[start..];
    let block = &body[..find_matching_brace(body)];
    assert!(
        !block.contains("Token") && !block.contains("token"),
        "the Approver trait must never name a credential:\n{block}"
    );
    assert!(
        !block.contains("ApproverHandle"),
        "an Approver must never be handed the authority capability:\n{block}"
    );
    insta::assert_snapshot!("approver_trait_block", block);
}

/// Finds the index just past the closing `}` matching the first `{` in
/// `text`.
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
