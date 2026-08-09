//! The decision chain (`docs/approval-ledger.md` §C.2) and the [`Policy`]
//! hook an embedder installs into it.
//!
//! Three stages, tried in order, the **first non-`Defer` wins**, and **none
//! of them waits**:
//!
//! 1. **Standing grants, then `observe` subscriptions** — pure ledger
//!    lookups with no hook and no I/O, the only stage that runs under the
//!    ledger lock. A standing grant authorizes the request; an observe
//!    subscription only records it, so the authorization answers first.
//! 2. **[`Policy::evaluate`]** — synchronous, on the request path,
//!    contractually non-blocking. Allowlists and risk-class rules.
//! 3. **Defer through both** ⇒ the request stays `Requested` and the gate
//!    site returns exit 2 with the pending view. **This is what a kernel
//!    with no [`Policy`] configured does** — the trait's one method
//!    defaults to `Decision::Defer`, so an empty impl changes nothing.
//!
//! **The kernel never awaits an embedder** (spec §0.1). A decision that
//! cannot be made synchronously is not made here at all: it comes back as
//! `Pending`, and the embedder decides in its own task, on its own clock,
//! under its own cancellation, then returns through
//! [`ApproverHandle`]. Both ways of awaiting one are wrong — a bounded wait
//! is a clock-driven decision, which §A.10 forbids, and an unbounded wait is
//! a liveness hazard the kernel cannot cancel on anyone's behalf correctly.
//!
//! **`evaluate` is never called while the ledger lock is held** (§B.1). The
//! chain's structure is what enforces it: stage 1 is one self-contained
//! ledger transaction that returns before stage 2 starts, and stage 2 takes
//! no lock at all — it calls back into [`Approvals`] freely, which is
//! exactly what the deadlock-shaped test in `ledger_policy_tests.rs`
//! proves.

use std::sync::Arc;
use std::time::Duration;

use kaish_types::approval::{
    ApprovalRequest, ApprovalRequestView, Decision, Grant, Grounds, Principal, PrincipalKind,
};
use tokio_util::sync::CancellationToken;

use super::error::LedgerError;
use super::handles::{ApproverHandle, Approvals, Requester};

/// How long a grant the chain issues stays redeemable if nothing redeems it.
/// The chain grants in order to let an operation proceed immediately, so
/// this is a short leash, not a standing authorization.
const DEFAULT_GRANT_TTL: Duration = Duration::from_secs(300);

/// The embedder's synchronous decision policy (spec §C.2).
///
/// **One method, and it does not wait.** A policy is a pure function of the
/// request and the ledger: the kernel asks it a question and gets an answer,
/// which is why it can sit on the path of every gated operation. Anything
/// that has to be thought about is not decided here — the request comes back
/// to the embedder as `Pending`, and the embedder decides in its own task
/// and returns through [`ApproverHandle`].
///
/// [`Self::evaluate`] is defaulted to `Decision::Defer`, so an empty impl
/// changes no behavior. A policy receives the tokenless
/// [`ApprovalRequestView`] — it decides, it does not redeem. There is no
/// path from this trait to a credential and none should be added: the view
/// type has no field for one (§A.2), and retrieval lives on
/// [`ApproverHandle`], which the chain never hands out.
///
/// **[`Self::evaluate`] may not panic, and a panic propagates.** kaish
/// installs no `catch_unwind` around it — a hook that panics is an embedder
/// bug, and swallowing it would let an operation proceed under a decision
/// nothing made. The unwind corrupts nothing: an in-flight attempt settles
/// `Unknown{Cancelled}` through its drop guard, and the kernel's locks do
/// not poison.
/// [`StatementClassifier::classify`](crate::ledger::StatementClassifier::classify)
/// carries the same contract, for the same reason.
///
/// ```compile_fail
/// use kaish_kernel::ledger::Policy;
/// use kaish_types::approval::{ApprovalRequestView, Decision};
///
/// struct Peeker;
///
/// impl Policy for Peeker {
///     fn evaluate(&self, req: &ApprovalRequestView, _l: &kaish_kernel::ledger::Approvals) -> Decision {
///         // There is no credential on the view — this does not compile,
///         // in this or any other spelling.
///         let _ = &req.token;
///         Decision::Defer
///     }
/// }
/// ```
pub trait Policy: Send + Sync {
    /// Stage 2: synchronous, on the request path, **contractually
    /// non-blocking**. Suitable for allowlists, risk-class rules, and
    /// "never `git.push.force`, full stop". `ledger` is the read side —
    /// pending requests, states, the log tail; it grants nothing.
    ///
    /// Blocking here blocks the gate site and every other execution behind
    /// it. Anything that can take time returns `Defer` and is decided out
    /// of band, after the gate site has returned `Pending`.
    fn evaluate(&self, req: &ApprovalRequestView, ledger: &Approvals) -> Decision {
        let _ = (req, ledger);
        Decision::Defer
    }

    /// Who decided. Recorded on every grant and denial this policy
    /// produces, so `approvals log` distinguishes machine clearance from
    /// human judgment (spec §E.6). Defaults to an `Automation` principal
    /// named `policy`.
    fn principal(&self) -> Principal {
        Principal::new("policy", PrincipalKind::Automation)
    }
}

/// Which stage of the chain produced an outcome (spec §C.2's three stages).
#[non_exhaustive]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ChainStage {
    /// Stage 1 — a standing grant covered the request.
    Standing,
    /// Stage 2 — [`Policy::evaluate`] decided.
    Policy,
}

/// What the chain concluded. Only [`ChainOutcome::Granted`] authorizes
/// anything; every other variant fails closed at the gate site.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum ChainOutcome {
    /// A stage granted, and the `Granted` entry is on the log.
    Granted {
        /// The posted grant.
        grant: Grant,
        /// Which stage decided.
        stage: ChainStage,
    },
    /// A stage denied, and the `Denied` entry is on the log.
    Denied {
        /// Why.
        reason: String,
        /// Which stage decided.
        stage: ChainStage,
    },
    /// Both stages deferred. The request stays `Requested`, nothing was
    /// posted beyond the `Requested` entry the caller already wrote, and
    /// fulfilment happens out of band — the gate site returns exit 2 with
    /// the pending view (spec §C.2 stage 3).
    Deferred,
    /// The **execution** was cancelled, so nothing was granted: either the
    /// cancellation had already fired when the chain was entered, or it
    /// fired in the window between deciding to grant and the grant landing
    /// on the log, and [`DecisionChain`]'s undo took it back.
    ///
    /// Not a decision, and nothing to do with an embedder: no hook is
    /// awaited here (spec §C.2), so there is no decision in flight for a
    /// cancellation to race.
    Cancelled,
}

/// The one thing the chain needs from the execution it is deciding for: the
/// token that says that execution is already unwinding.
///
/// There is no patient hold here and no budget. The chain takes no time it
/// could need one for — nothing in it awaits an embedder (spec §C.2), so a
/// gated statement returns rather than being held open.
///
/// [`ChainContext::detached`] is the no-execution form — a token nobody
/// fires — for embedder-driven and test callers.
pub struct ChainContext {
    cancel: CancellationToken,
}

impl ChainContext {
    /// Bind the chain to a live execution, so a grant is never left behind
    /// for an execution that is unwinding.
    pub fn new(cancel: CancellationToken) -> Self {
        Self { cancel }
    }

    /// A token nobody fires.
    pub fn detached() -> Self {
        Self {
            cancel: CancellationToken::new(),
        }
    }
}

/// The chain, holding everything a decision needs: the authority that posts
/// the outcome, the read side a policy consults, and the embedder's policy
/// when one is installed.
///
/// A chain with no policy is stages 1 and 3 only: no standing rule means
/// Defer means exit 2.
#[derive(Clone)]
pub struct DecisionChain {
    authority: ApproverHandle,
    approvals: Approvals,
    /// Derived from `authority`, and used for exactly one thing: abandoning
    /// a request whose execution was cancelled out from under a grant
    /// (see [`DecisionChain::undo_if_cancelled`]). Grants nothing.
    requester: Requester,
    policy: Option<Arc<dyn Policy>>,
    grant_ttl: Duration,
}

impl std::fmt::Debug for DecisionChain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("DecisionChain")
            .field("authority", &self.authority)
            .field("policy", &self.policy.is_some())
            .field("grant_ttl", &self.grant_ttl)
            .finish()
    }
}

impl DecisionChain {
    /// Build a chain over one ledger's authority and read side. `policy` is
    /// `None` for a kernel with no decision hook — stage 2 is then skipped
    /// outright.
    pub fn new(authority: ApproverHandle, approvals: Approvals, policy: Option<Arc<dyn Policy>>) -> Self {
        let (requester, _, _) = authority.join();
        Self {
            authority,
            approvals,
            requester,
            policy,
            grant_ttl: DEFAULT_GRANT_TTL,
        }
    }

    /// Override how long a grant this chain issues stays redeemable.
    pub fn with_grant_ttl(mut self, ttl: Duration) -> Self {
        self.grant_ttl = ttl;
        self
    }

    /// Whether a [`Policy`] is installed. Stage 2 does nothing when this is
    /// `false`.
    pub fn has_policy(&self) -> bool {
        self.policy.is_some()
    }

    /// Run the chain against an already-posted request.
    ///
    /// The caller posts `Requested` (that is the requester's obligation) and
    /// hands the stamped request here; the chain posts at most one decision
    /// against it. Stages run in order and the first non-`Defer` short-
    /// circuits — a later stage never sees a request an earlier one decided.
    ///
    /// # Errors
    ///
    /// Any ledger transaction failure: the request was already decided, it
    /// is terminal, the ring or sink is full, or a policy returned terms
    /// that widen the request it was shown.
    pub async fn decide(
        &self,
        request: &ApprovalRequest,
        ctx: &ChainContext,
    ) -> Result<ChainOutcome, LedgerError> {
        // Fail closed on a cancellation that fired before we got here: an
        // execution that is already unwinding must not acquire authority on
        // its way out.
        if ctx.cancel.is_cancelled() {
            return Ok(ChainOutcome::Cancelled);
        }

        // ── Stage 1: standing grants ────────────────────────────────
        // One self-contained ledger transaction. It returns — releasing the
        // lock — before any hook below is called.
        if let Some(grant) = self
            .authority
            .grant_from_standing(&request.id, self.grant_ttl)
            .await?
        {
            return self
                .undo_if_cancelled(
                    request,
                    ChainOutcome::Granted {
                        grant,
                        stage: ChainStage::Standing,
                    },
                    ctx,
                )
                .await;
        }

        let Some(policy) = self.policy.as_ref() else {
            return Ok(ChainOutcome::Deferred);
        };
        let view = ApprovalRequestView::from(request);

        // ── Stage 2: policy ─────────────────────────────────────────
        // No lock is held here. `evaluate` is handed the read side and may
        // consult it freely. It is synchronous by contract, so the chain
        // returns in microseconds whatever the answer is.
        match policy.evaluate(&view, &self.approvals) {
            Decision::Defer => {}
            decision => {
                return self
                    .post(request, decision, policy.as_ref(), ChainStage::Policy, ctx)
                    .await
            }
        }

        // ── Stage 3: defer ──────────────────────────────────────────
        // Nobody decided, and the kernel will not wait to find out. The
        // request stays `Requested` and the gate site returns exit 2 with
        // the pending view (spec §0.1, §C.2).
        Ok(ChainOutcome::Deferred)
    }

    /// Post a policy's non-`Defer` decision through the authority handle,
    /// attributed to the policy's own principal.
    async fn post(
        &self,
        request: &ApprovalRequest,
        decision: Decision,
        policy: &dyn Policy,
        stage: ChainStage,
        ctx: &ChainContext,
    ) -> Result<ChainOutcome, LedgerError> {
        let principal = policy.principal();
        match decision {
            Decision::Grant(terms) => {
                // Terms that drop or alter a condition the request declared
                // are refused by the ledger itself (`LedgerError::
                // ConditionsWidened`, spec §A.4) — the check lives at the
                // one place every grant passes through, not here, so an
                // approver cannot route around it.
                let grounds = match stage {
                    // The rule that matched is the policy itself — the trait
                    // gives it no way to name a finer rule, and inventing one
                    // would put a made-up name in the audit record.
                    ChainStage::Policy => Grounds::Policy {
                        rule: principal.id.clone(),
                    },
                    _ => Grounds::Embedder,
                };
                let grant = self
                    .authority
                    .clone()
                    .with_principal(principal)
                    // `request` is the value `post_request` just returned to
                    // this same call chain, so `request.revision` (0) is what
                    // the chain has actually observed — the same
                    // caller-quotes-its-own-read contract spec §B.6 gives
                    // every other decision path (§D.2).
                    .grant_with_grounds(&request.id, request.revision, terms, grounds)
                    .await?;
                self.undo_if_cancelled(request, ChainOutcome::Granted { grant, stage }, ctx)
                    .await
            }
            Decision::Deny { reason } => {
                self.authority
                    .clone()
                    .with_principal(principal)
                    .deny(&request.id, request.revision, &reason)
                    .await?;
                // A denial needs no undo: it authorizes nothing, and
                // erasing it would lose the record of a real decision.
                Ok(ChainOutcome::Denied { reason, stage })
            }
            Decision::Defer => Ok(ChainOutcome::Deferred),
            // `Decision` is `#[non_exhaustive]`: a variant added upstream
            // without a case here must not silently mean "yes".
            _ => Ok(ChainOutcome::Deferred),
        }
    }

    /// Close the window between "we decided to grant" and "the grant is on
    /// the log": if cancellation fired anywhere in it, abandon the request,
    /// which kills the grant and drops its credential.
    ///
    /// The window cannot be removed. A grant commits inside the ledger's
    /// critical section, and the ledger deliberately knows nothing about
    /// cancellation tokens — teaching it would run one feature's plumbing
    /// through another's boundary for a check that would still race the
    /// instant before the lock. So the guarantee is stated as an outcome
    /// rather than as timing: **a cancelled execution never leaves a live
    /// grant behind.** A grant that beat the cancellation is undone by
    /// `Abandoned`, which is the same transition a discarded job takes
    /// (spec §B.2), and the record shows both the decision and its undoing
    /// rather than hiding either.
    ///
    /// A failure to undo is **loud**: it returns the ledger's error rather
    /// than the grant, because reporting `Granted` for an execution that is
    /// unwinding is the one answer that could authorize something nobody
    /// will perform.
    async fn undo_if_cancelled(
        &self,
        request: &ApprovalRequest,
        outcome: ChainOutcome,
        ctx: &ChainContext,
    ) -> Result<ChainOutcome, LedgerError> {
        if !ctx.cancel.is_cancelled() {
            return Ok(outcome);
        }
        let ChainOutcome::Granted { .. } = &outcome else {
            return Ok(outcome);
        };
        self.requester
            .abandon_request(
                &request.id,
                "the execution was cancelled before its grant could be used",
            )
            .await?;
        Ok(ChainOutcome::Cancelled)
    }
}
