//! The approval ledger (`docs/approval-ledger.md`).
//!
//! One append-only log, two posting authorities, and a linearization
//! contract enforced by a single lock (spec §A.1, §B.1). This module is the
//! state machine: [`Ledger::build`] mints a [`Requester`] (obligations —
//! `Requested`/`Redeemed`/`Settled`), an [`Approvals`] (read-only), and one
//! [`ApproverHandle`] (authorizations — `Granted`/`Denied`/standing grants/
//! credential retrieval). There is no method on `Requester` that produces a
//! `Grant`; that split, enforced by the type system, is the whole point.
//!
//! **What is here:** both state machines (spec §B.2–§B.3), the credential
//! index and its rejected-attempt voiding (§F.3), idempotent settlement
//! (§A.1), partitioned retention with a ring that refuses to evict a live
//! chain (§D.4), sink backpressure, the invariant checks, the sweep, the
//! [`DecisionChain`] (standing → [`Policy::evaluate`] → defer) with
//! standing-grant matching, redemption-time precondition
//! verification through the [`StateResolver`]s, the subscription registry
//! with its [`SubscriptionFilter`], and the statement gate's operation and
//! classifier seam ([`KernelOperation::CmdExecute`], [`StatementClassifier`]).
//!
//! **What is not:** the gate sites. They live where the operations do —
//! `tools/context.rs`'s `gate_overwrites`, `rm`'s `decide_rm_action`, and
//! `kaish-trash empty` — and reach this module through `ToolCtx`'s
//! `request_approval`/`approvals`/`settle_with` and the dispatcher's
//! [`AttemptGuard`]. `Kernel::build` is what constructs a ledger and a
//! [`DecisionChain`] and hands the embedder the one [`ApproverHandle`].

mod attempt_guard;
mod clock;
mod config;
mod core;
pub(crate) mod error;
mod handles;
mod operation;
mod patterns;
mod policy;
mod resolver;
mod standing;
mod subscription;
mod teardown;
mod watch;

pub use attempt_guard::AttemptGuard;
pub use clock::{Clock, SystemClock};
pub use config::{LedgerConfig, LedgerSink, LedgerSinkError};
pub use error::LedgerError;
pub use operation::KernelOperation;
pub(crate) use operation::static_gate_floor;
pub use policy::{ChainContext, ChainOutcome, ChainStage, DecisionChain, Policy};
pub use resolver::{
    ConditionReport, PathResolver, ResolverError, StateResolver, StateResolverConflict,
    StateResolvers, PATH_DIGEST_ALG, PATH_KIND,
};
pub use subscription::{Posture, SubscriptionFilter};
pub(crate) use teardown::{cancel_job_request, cancel_scope};
// The statement gate's scoping seam (spec §C.6) lives in `kaish-tool-api`,
// beside `StateResolver` and for the same reason: it names no kernel type.
// Re-exported here because an embedder registers it through `KernelConfig`,
// next to `with_policy` and `with_state_resolver`.
pub use kaish_tool_api::{
    ClassificationError, CommandNameClassifier, ExecutionContext, MountAccess, MountClass,
    MountDescriptor, StatementAssessment, StatementClassificationInput,
    StatementClassifier, StatementPosture,
};
// What a caller reads back from a gate (spec §C.1), re-exported for the same
// reason: an embedder deciding one names these types, and it should not have
// to depend on `kaish-tool-api` to say what `ExecResult.approval` means or
// how to resume it.
pub use kaish_tool_api::{ApprovalOutcome, PendingApproval, ResumeAction};
pub(crate) use resolver::{conditions_to_observe, digest_path};
pub use handles::{
    ApproverHandle, AssessmentRecorder, AttemptHandle, AttemptView, Approvals, DecisionContext, Ledger,
    RequestChain, Requester,
};
pub use watch::{LedgerStream, WatchEvent};

/// Test-only: a stamped, tokenless view for exercising the control-plane
/// `.approval` field (job rows, scatter rows, pipeline overrides) without
/// standing up a live ledger. In-crate unit tests only — an integration test
/// gets a real view from a real ledger.
#[cfg(test)]
pub(crate) fn sample_view(
    operation: KernelOperation,
    paths: &[&str],
) -> kaish_types::approval::ApprovalRequestView {
    use kaish_types::approval::{
        ApprovalRequest, ApprovalScope, Capture, Invocation, KernelId, PlanBinding, PlanDigest,
        Principal, PrincipalKind, RequestId, RequestOrigin, Resource,
    };
    let scope = ApprovalScope::kernel(KernelId::new(1));
    let draft = ApprovalRequest::builder(operation.as_str())
        .risk(operation.risk())
        .reason("the fs.* enforce policy is on")
        .hint(format!("rm --confirm=<token> {}", paths.join(" ")));
    paths
        .iter()
        .fold(draft, |b, p| b.resource(Resource::plain("path", *p)))
        .build()
        .expect("a well-formed draft")
        .stamp(
            RequestId::new(0x0badcafe, 1),
            std::time::UNIX_EPOCH,
            RequestOrigin::new(
                scope.clone(),
                PlanBinding::new(PlanDigest::new("sample"), "/", scope),
                Principal::new("session", PrincipalKind::Agent),
                Capture::Exact(Invocation {
                    tool: "rm".to_string(),
                    argv: paths.iter().map(|p| (*p).to_string()).collect(),
                }),
            ),
        )
        .into()
}
