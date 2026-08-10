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
    LostCause, Outcome, Principal, RequestId, RequestState, ResourceRef, SessionId, StandingId,
    StateClaim, SubscriptionId, TransitionKind,
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
    /// The request is in a terminal state (`Consumed`, `Denied`,
    /// `Cancelled`, `Expired`, `Voided`, or `Abandoned`) and accepts no
    /// further transitions.
    /// `detail` carries the specific reason for `Voided` (spec §F.3's "a
    /// later good key fails naming the void").
    ///
    /// Redemption against a `Consumed` request answers
    /// [`Self::AlreadySettled`] instead — it carries the outcome the
    /// settlement reported (spec §B.4).
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
    /// A redemption arrived after the grant already closed: an attempt
    /// settled successfully, or settled `Unknown` (spec §B.2 — effects
    /// unknown, no retry against this grant). The kernel reports the
    /// outcome instead of re-executing (spec §B.4); `outcome` is `None`
    /// when the chain closed without an actual `Settled` entry — there is
    /// no outcome to report, only the fact that nobody ever will.
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
    /// A `grant`, `deny`, or `cancel` quoted a revision other than the
    /// request's current one (spec §B.6 — the late-answer rule). Refused and
    /// recorded as `LedgerEntry::RevisionRejected` in the same transaction,
    /// never applied: the caller's view of the request is stale, whatever
    /// state the request has since moved to.
    StaleRevision {
        /// The request the stale decision targeted.
        request: RequestId,
        /// The revision the caller quoted.
        quoted: u64,
        /// The request's actual revision.
        current: u64,
        /// Which kind of transition was attempted.
        attempted: TransitionKind,
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
                    "request {id} already settled — {} — not re-executing; present a new request to retry",
                    render_outcome(outcome)
                ),
                None => write!(
                    f,
                    "request {id} closed with no reported outcome (its reservation was abandoned) — not re-executing; present a new request to retry"
                ),
            },
            Self::Refused { id, detail } => {
                write!(f, "request {id} was refused: {detail} — the grant is voided; re-request")
            }
            Self::CredentialUnavailable(detail) => {
                write!(f, "approval ledger could not generate a credential: {detail}")
            }
            Self::LiveCapacity { limit } => write!(
                f,
                "approval ledger at capacity ({limit} live requests) — settle or abandon pending approvals"
            ),
            Self::LiveCapacityPerPrincipal { principal, limit } => write!(
                f,
                "approval ledger at capacity for principal {principal} ({limit} live requests) — settle or abandon its pending approvals"
            ),
            Self::RingAtCapacity => write!(
                f,
                "approval ledger's history is full and its oldest entry belongs to a still-live request — settle or abandon that request before more entries can be recorded"
            ),
            Self::SinkUnavailable(reason) => write!(
                f,
                "the approval ledger is refusing new requests because its audit sink is not accepting records: {reason} — an unrecorded decision is worse than a refused one"
            ),
            Self::ConditionsWidened { request, resource, expected } => write!(
                f,
                "grant for request {request} widens the request's declared transition on {}:{} (expected {} preserved) — an approver may narrow, never widen",
                resource.kind,
                resource.id,
                render_state_claim(expected)
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
                "a principal may not approve its own request: {request} was requested by {} and would be \
                 granted by {} — set `deny_self_approval` to false if this kernel is legitimately both sides",
                requested_by.id, granted_by.id
            ),
            Self::StaleRevision { request, quoted, current, attempted } => write!(
                f,
                "{attempted} on request {request} quoted revision {quoted}, but it is now at revision \
                 {current} — refused and recorded; re-read the current state and decide again"
            ),
        }
    }
}

impl std::error::Error for LedgerError {}

/// A [`StateClaim`] as an operator reads it. `ConditionsWidened` names the
/// claim a grant failed to preserve, and `{:?}` would print the enum instead
/// of the digest or id the reader has to compare against.
pub(crate) fn render_state_claim(claim: &StateClaim) -> String {
    match claim {
        StateClaim::Unspecified => "no claimed prior state".to_string(),
        StateClaim::Absent => "absent".to_string(),
        StateClaim::Exact(id) => id.clone(),
        StateClaim::Digest { alg, hex } => format!("{alg}:{hex}"),
        // `StateClaim` is `#[non_exhaustive]`; an unrecognized claim is still
        // a claim, and still has to render as something an operator can read.
        other => format!("{other:?}"),
    }
}

/// An [`Outcome`] as an operator reads it — what the settled run reported, in
/// the words the exit-code contract uses. Both enums are `#[non_exhaustive]`,
/// so an unrecognized variant still has to render as something readable.
fn render_outcome(outcome: &Outcome) -> String {
    match outcome {
        Outcome::Exit(code) => format!("the run reported exit {code}"),
        Outcome::Error(detail) => format!("the run reported an error: {detail}"),
        Outcome::Unknown { cause } => format!(
            "the executor went away before reporting ({}), so whether the operation took effect is unknown",
            match cause {
                LostCause::Cancelled => "cancelled".to_string(),
                LostCause::ExecutorLost => "executor lost".to_string(),
                other => format!("{other:?}"),
            }
        ),
        other => format!("{other:?}"),
    }
}
