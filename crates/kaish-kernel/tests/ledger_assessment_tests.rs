//! Assessments (`docs/approval-ledger.md` §C.7): the append-only
//! `AssessmentRecorder`, `DecisionContext`, and the `Assessed` entry.
//!
//! An assessment explains a decision; it never is one — only
//! `Granted`/`Denied` decide (spec §A.1's balance rule is untouched by how
//! many `Assessed` entries a request accumulates on the way there). No
//! `#![cfg(feature = ...)]` gate — like the ledger core it sits on, this has
//! no OS dependency and must compile and pass featureless.

use std::sync::Arc;
use std::time::{Duration, SystemTime};

use kaish_kernel::ledger::{Ledger, LedgerConfig, LedgerError, SystemClock};
use kaish_types::approval::{
    ApprovalAssessment, ApprovalRequest, AssessmentOutcome, AssessmentStage, AssessorId, GrantTerms,
    LedgerEntry, Principal, PrincipalKind, RequestState, RiskClass,
};

fn test_scope() -> kaish_types::approval::ApprovalScope {
    kaish_types::approval::ApprovalScope::kernel(kaish_types::approval::KernelId::mint())
}

fn test_origin(principal: Principal) -> kaish_types::approval::RequestOrigin {
    let scope = test_scope();
    kaish_types::approval::RequestOrigin::new(
        scope.clone(),
        kaish_types::approval::PlanBinding::new(kaish_types::approval::PlanDigest::new("test"), "/", scope),
        principal,
        kaish_types::approval::Capture::DirectExecution,
    )
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

fn build_ledger() -> (
    kaish_kernel::ledger::Requester,
    kaish_kernel::ledger::Approvals,
    kaish_kernel::ledger::ApproverHandle,
) {
    #[allow(clippy::unwrap_used)]
    Ledger::build(LedgerConfig::default(), test_scope(), None, Arc::new(SystemClock)).unwrap()
}

fn entries(records: Vec<kaish_types::approval::LedgerRecord>) -> Vec<LedgerEntry> {
    #[allow(clippy::expect_used)]
    records
        .into_iter()
        .map(|record| record.known().cloned().expect("this build wrote every record it reads back"))
        .collect()
}

fn specialist_assessment(request: kaish_types::approval::RequestId) -> ApprovalAssessment {
    ApprovalAssessment::new(
        request,
        AssessorId::new("specialist"),
        AssessmentStage::Specialist,
        AssessmentOutcome::Escalate,
        "escalating to a human — outside the specialist's confidence band",
    )
    .with_risk(RiskClass::Irreversible)
    .with_confidence(0.4)
}

/// The R4 acceptance test named directly in the spec (§H): an assessment
/// appended from an embedder that then abandons the decision survives and
/// remains readable through `Approvals::get` — the whole point of a
/// recorder that is not a return value (spec §C.7: "an approver that
/// returned its assessments alongside its decision would lose them in
/// exactly the case they matter most").
#[tokio::test]
async fn assessments_appended_while_deliberating_survive_an_abandoned_request() {
    let (requester, approvals, approver) = build_ledger();
    let req = post(&requester, "cmd.execute").await;

    approver
        .assessments()
        .append(specialist_assessment(req.id.clone()))
        .await
        .expect("appending an assessment against a live request must succeed");

    // The embedder gives up on the decision entirely.
    requester
        .abandon_request(&req.id, "operator walked away before deciding")
        .await
        .expect("abandoning an undecided request must succeed");

    let chain = approvals.get(&req.id).expect("the chain must still be readable after abandonment");
    assert_eq!(chain.state, RequestState::Abandoned);
    assert_eq!(chain.assessments.len(), 1, "{:?}", chain.assessments);
    assert_eq!(chain.assessments[0].assessor, AssessorId::new("specialist"));
    assert_eq!(chain.assessments[0].outcome, AssessmentOutcome::Escalate);

    // It is on the log too, not only reconstructible from the chain.
    let assessed_count = entries(approvals.log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items)
        .into_iter()
        .filter(|e| matches!(e, LedgerEntry::Assessed { .. }))
        .count();
    assert_eq!(assessed_count, 1);
}

/// Several assessments from several stages of one pipeline all land, in
/// order, and all survive — the "a router feeding specialists feeding a
/// model feeding a human" shape spec §C.7 names explicitly.
#[tokio::test]
async fn several_assessments_from_several_stages_all_survive_in_order() {
    let (requester, approvals, approver) = build_ledger();
    let req = post(&requester, "cmd.execute").await;
    let recorder = approver.assessments();

    for (assessor, stage, outcome) in [
        ("classifier", AssessmentStage::Classifier, AssessmentOutcome::Escalate),
        ("policy-rule-7", AssessmentStage::Policy, AssessmentOutcome::Abstain),
        ("specialist", AssessmentStage::Specialist, AssessmentOutcome::Escalate),
        ("amy", AssessmentStage::Human, AssessmentOutcome::Allow),
    ] {
        recorder
            .append(ApprovalAssessment::new(
                req.id.clone(),
                AssessorId::new(assessor),
                stage,
                outcome,
                "test pipeline stage",
            ))
            .await
            .expect("each stage's assessment must append");
    }

    let chain = approvals.get(&req.id).expect("the chain");
    assert_eq!(chain.assessments.len(), 4);
    let assessors: Vec<&str> = chain.assessments.iter().map(|a| a.assessor.as_str()).collect();
    assert_eq!(assessors, vec!["classifier", "policy-rule-7", "specialist", "amy"]);
}

/// Appending an assessment must never bump `revision` (spec §A.7's
/// `KeyRetrieved` rationale, applied identically to `Assessed`): an approver
/// still deliberating quotes the revision it read when the request went
/// `Pending`, and several assessments can land on the way to that decision.
/// If appending bumped revision, the eventual `grant` would be refused as
/// `StaleRevision` purely because assessments were recorded — the exact
/// hazard this test pins closed.
#[tokio::test]
async fn appending_an_assessment_does_not_bump_revision_and_the_original_quote_still_grants() {
    let (requester, approvals, approver) = build_ledger();
    let req = post(&requester, "cmd.execute").await;
    assert_eq!(req.revision, 0);

    approver
        .assessments()
        .append(specialist_assessment(req.id.clone()))
        .await
        .unwrap_or_else(|e| panic!("append must succeed: {e}"));
    approver
        .assessments()
        .append(specialist_assessment(req.id.clone()))
        .await
        .unwrap_or_else(|e| panic!("second append must succeed: {e}"));

    let after_assessments = approvals.get(&req.id).expect("the chain");
    assert_eq!(
        after_assessments.request.revision, 0,
        "two Assessed entries must not have bumped the request's revision"
    );

    // The revision-0 quote `post_request` originally handed back still
    // grants — proof that a deliberating approver's held revision was never
    // invalidated out from under it.
    let not_after = SystemTime::now() + Duration::from_secs(300);
    approver
        .grant(&req.id, req.revision, GrantTerms::once_for(&req, not_after))
        .await
        .unwrap_or_else(|e| panic!("the original revision-0 quote must still grant: {e}"));
    assert_eq!(approvals.state(&req.id), Some(RequestState::Granted));
}

/// An assessment about a request nobody posted is a caller bug, not a state
/// the ledger absorbs — `post_assessment` requires the chain to exist.
#[tokio::test]
async fn an_assessment_against_no_request_is_refused() {
    let (_requester, _approvals, approver) = build_ledger();
    let ghost = kaish_types::approval::RequestId::new(0xdead_beef, 1);
    let err = approver
        .assessments()
        .append(specialist_assessment(ghost.clone()))
        .await
        .expect_err("an assessment against a request that was never posted must be refused");
    assert!(matches!(err, LedgerError::NotFound(id) if id == ghost));
}

/// `AssessmentRecorder` is reached from both `ApproverHandle` (the
/// embedder's own authority, appending while `Pending`) and `Requester` (the
/// obligation side — what the statement tap uses to record a classifier's
/// own judgment before any decision exists, spec §C.6). Both write to the
/// same chain.
#[tokio::test]
async fn the_recorder_reached_from_requester_and_from_the_authority_write_the_same_chain() {
    let (requester, approvals, approver) = build_ledger();
    let req = post(&requester, "cmd.execute").await;

    requester
        .assessments()
        .append(ApprovalAssessment::new(
            req.id.clone(),
            AssessorId::new("statement-classifier"),
            AssessmentStage::Classifier,
            AssessmentOutcome::Escalate,
            "the tap's own classifier judgment",
        ))
        .await
        .expect("the obligation side may record an assessment");

    approver
        .assessments()
        .append(specialist_assessment(req.id.clone()))
        .await
        .expect("the authority side may record one too");

    let chain = approvals.get(&req.id).expect("the chain");
    assert_eq!(chain.assessments.len(), 2);
}
