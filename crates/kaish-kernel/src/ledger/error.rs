//! Errors the ledger core returns. Every variant here is a *reachable*
//! runtime outcome (a bad credential, a race lost, a full ring) except
//! [`LedgerError::InvariantViolated`], which is a kernel bug: it additionally
//! `debug_assert!`s at the call site before being constructed in a debug
//! build, and is only ever returned (not panicked) in release, per the
//! debug/release split the spec draws (`docs/approval-ledger.md` §B.3).
//!
//! Every message here is governed prose (`CLAUDE.md`, "Writing style"):
//! constraint and consequence first, the number stated, no hedging.

use std::fmt;

use kaish_types::approval::{
    Outcome, Principal, RequestId, RequestState, ResourceRef, SessionId, StandingId, StateClaim,
    SubscriptionId,
};

/// Why a ledger transaction did not commit. Every non-`InvariantViolated`
/// variant is an ordinary runtime outcome, not a bug (spec §B.3).
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq)]
pub enum LedgerError {
    /// No request exists with this id — never posted, or already evicted
    /// from the retained-closed pool.
    NotFound(RequestId),
    /// A second decision was posted against a request that already has one.
    AlreadyDecided(RequestId),
    /// A redemption was attempted against a request with no live grant.
    NotAuthorized(RequestId),
    /// A redemption was attempted while another attempt against the same
    /// grant is still `Reserved`.
    AttemptInFlight(RequestId),
    /// The request is in a terminal state (`Denied`, `Voided`, `Abandoned`,
    /// or `Expired` outside a `renew`) and accepts no further transitions.
    /// `detail` carries the specific reason for `Voided` (spec §F.3's "a
    /// later good key fails naming the void").
    Terminal {
        /// The request that is terminal.
        id: RequestId,
        /// Which terminal state it is in.
        state: RequestState,
        /// Extra detail, e.g. why a `Voided` request was voided.
        detail: Option<String>,
    },
    /// No standing grant exists with this id.
    StandingNotFound(StandingId),
    /// No subscription exists with this id.
    SubscriptionNotFound(SubscriptionId),
    /// A redemption arrived after the grant already closed — either a
    /// successful (or `Unknown`) settlement, or the recovery sweep closing
    /// an unreported `Reserved` attempt as `Abandoned` (spec §B.2 treats
    /// that the same as `Unknown`: effects unknown, no retry against this
    /// grant). The kernel reports the outcome instead of re-executing (spec
    /// §B.4); `outcome` is `None` when the chain closed via the sweep
    /// rather than an actual `Settled` entry — there is no outcome to
    /// report, only the fact that nobody ever will.
    AlreadySettled {
        /// The request whose grant already closed.
        id: RequestId,
        /// What the settling attempt reported, if it could be recovered.
        outcome: Option<Outcome>,
    },
    /// A redemption-time condition no longer held — the world moved under
    /// the approval. The grant is voided as part of the same transaction
    /// that returns this error (spec §B.4).
    Refused {
        /// The request whose redemption was refused.
        id: RequestId,
        /// Why.
        detail: String,
    },
    /// `renew` was called on a request that is not `Expired` — only an
    /// expired request may be renewed (spec §B.5).
    NotRenewable {
        /// The request that cannot be renewed.
        id: RequestId,
        /// Its actual current state.
        state: RequestState,
    },
    /// The OS could not supply entropy for a new credential. No fallback —
    /// a guessable credential is worse than a loud failure.
    CredentialUnavailable(String),
    /// The live index is at its configured capacity — no request-level quota
    /// distinction.
    LiveCapacity {
        /// The configured `LedgerConfig::live_capacity`.
        limit: usize,
    },
    /// One principal's share of the live index is at its configured
    /// capacity.
    LiveCapacityPerPrincipal {
        /// The principal whose quota is exhausted.
        principal: String,
        /// The configured `LedgerConfig::live_capacity_per_principal`.
        limit: usize,
    },
    /// The retained-entry ring is full and its oldest entry belongs to a
    /// still-live request, so nothing can be evicted to make room (spec
    /// §D.4 — the ring fails loud rather than evicting a live chain).
    RingAtCapacity,
    /// The audit sink cannot currently accept more entries — its bounded
    /// queue is full, or a prior `LedgerSink::post` call failed and the
    /// ledger has stopped accepting new obligations until an operator
    /// restarts it (spec §D.4 — a sink error fails the request closed).
    SinkUnavailable(String),
    /// A `Grant` was posted whose `conditions` drop or alter a transition
    /// claim the request itself declared — an approver may narrow (add or
    /// tighten) and may never widen (spec §A.4). This is an ordinary
    /// runtime outcome, not a bug: it is entirely reachable from an
    /// embedder-supplied `GrantTerms` and carries no `debug_assert!`,
    /// unlike [`Self::InvariantViolated`].
    ConditionsWidened {
        /// The request whose declared transition would have been widened.
        request: RequestId,
        /// The resource whose condition was missing or altered.
        resource: ResourceRef,
        /// What the request declared and the grant failed to preserve.
        expected: StateClaim,
    },
    /// A replay's fresh draft does not describe the operation and resources
    /// that were approved (spec §B.4). The replay did not turn into the
    /// operation that was granted, so it is refused rather than authorized.
    DraftMismatch {
        /// The request the replay claimed to fulfill.
        request: RequestId,
        /// How the draft differs from what was approved.
        detail: String,
    },
    /// A scoped handle acted on a request belonging to another session
    /// (spec §A.7). API hygiene, not a process boundary: it stops a
    /// session's code from reaching another session's requests by accident
    /// or by confusion, and it does not pretend to stop hostile Rust in the
    /// same process.
    OutOfScope {
        /// The request the caller named.
        request: RequestId,
        /// The session the handle is restricted to.
        session: SessionId,
    },
    /// A kernel bug: an unmatched `Redeemed`/terminal pair, a `seq` gap, or
    /// a second successful settlement against one grant. Never means
    /// "proceed" (spec §A.1).
    InvariantViolated(String),
    /// `LedgerConfig::deny_self_approval` is on and the principal that would
    /// decide this grant is the same principal that requested it (spec
    /// §D.2, §E.7). Refused before the grant is appended — catches
    /// misconfiguration, not an attacker; the ledger still records both
    /// principals on every grant regardless of this policy. Both fields are
    /// equal by construction — this variant is only ever raised when they
    /// match — but the error names both roles explicitly rather than one
    /// value, so the message reads as "requested by X, would be granted by
    /// X" instead of leaving the reader to infer the second role.
    SelfApproval {
        /// The request this grant would have authorized.
        request: RequestId,
        /// The requesting principal.
        requested_by: Principal,
        /// The principal that would have decided the grant.
        granted_by: Principal,
    },
}

impl fmt::Display for LedgerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::NotFound(id) => write!(f, "approval request {id} does not exist"),
            Self::AlreadyDecided(id) => write!(
                f,
                "request {id} was already decided — a request accepts exactly one grant or denial"
            ),
            Self::NotAuthorized(id) => {
                write!(f, "request {id} has no live grant — nothing to redeem yet")
            }
            Self::AttemptInFlight(id) => write!(
                f,
                "request {id} has an attempt already in flight — settle it before redeeming again"
            ),
            Self::Terminal { id, state, detail } => {
                write!(f, "request {id} is {state:?} and accepts no further transitions")?;
                if let Some(detail) = detail {
                    write!(f, " — {detail}")?;
                }
                Ok(())
            }
            Self::StandingNotFound(id) => write!(f, "standing grant {id} does not exist"),
            Self::SubscriptionNotFound(id) => write!(f, "subscription {id} does not exist"),
            Self::AlreadySettled { id, outcome } => match outcome {
                Some(outcome) => write!(
                    f,
                    "request {id} already settled ({outcome:?}) — not re-executing; present a new request to retry"
                ),
                None => write!(
                    f,
                    "request {id} closed with no reported outcome (its reservation was abandoned by the recovery sweep) — not re-executing; present a new request to retry"
                ),
            },
            Self::Refused { id, detail } => {
                write!(f, "request {id} was refused: {detail} — the grant is voided; re-request")
            }
            Self::NotRenewable { id, state } => write!(
                f,
                "request {id} is {state:?}, not Expired — only an expired request can be renewed"
            ),
            Self::CredentialUnavailable(detail) => {
                write!(f, "approval ledger could not generate a credential: {detail}")
            }
            Self::LiveCapacity { limit } => write!(
                f,
                "approval ledger at capacity ({limit} live requests) — settle or abandon pending approvals"
            ),
            Self::LiveCapacityPerPrincipal { principal, limit } => write!(
                f,
                "approval ledger at capacity for principal {principal:?} ({limit} live requests) — settle or abandon its pending approvals"
            ),
            Self::RingAtCapacity => write!(
                f,
                "approval ledger's retained-entry ring is full and its oldest entry belongs to a still-live request — settle or abandon it before more entries can be recorded"
            ),
            Self::SinkUnavailable(reason) => {
                write!(f, "approval ledger audit sink unavailable: {reason}")
            }
            Self::ConditionsWidened { request, resource, expected } => write!(
                f,
                "grant for request {request} widens the request's declared transition on {}:{} (expected {expected:?} preserved) — an approver may narrow, never widen",
                resource.kind, resource.id
            ),
            Self::DraftMismatch { request, detail } => write!(
                f,
                "the replayed invocation does not match request {request}: {detail} — nothing was performed"
            ),
            Self::OutOfScope { request, session } => write!(
                f,
                "request {request} belongs to another session — this handle is scoped to {session}"
            ),
            Self::InvariantViolated(detail) => {
                write!(f, "approval ledger invariant violated: {detail}")
            }
            Self::SelfApproval { request, requested_by, granted_by } => write!(
                f,
                "request {request} refused: requested by {requested_by:?}, would be granted by {granted_by:?} \
                 — deny_self_approval refuses a principal approving its own request"
            ),
        }
    }
}

impl std::error::Error for LedgerError {}
