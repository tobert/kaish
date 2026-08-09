//! The public surface: three handles onto one [`LedgerInner`], split by
//! posting authority (spec §A.1). `Requester` posts obligations, `Approvals`
//! reads, `ApproverHandle` posts authorizations — and there is no method
//! anywhere in this file that lets a `Requester` produce a `Grant`.

use std::sync::Arc;
use std::time::Duration;

use kaish_types::approval::{
    ApprovalAssessment, ApprovalRequest, ApprovalRequestDraft, ApprovalScope, AttemptId, AttemptState,
    CancelReason, Grant,
    GrantTerms, Grounds, LedgerRecord, ObservedResource, OperationId, Outcome, Plan, Principal,
    RequestId, RequestOrigin, RequestState, SessionId, StandingGrant, StandingId, Subscription,
    SubscriptionId, Token,
};

use super::clock::Clock;
use super::core::{build_inner, LedgerInner};
use super::resolver::ConditionReport;
use super::config::{LedgerConfig, LedgerSink};
use super::error::LedgerError;

/// The implementation side's handle (spec §A.1). Obtained from `ExecContext`
/// / `ToolCtx` (PR 3). Can post obligations (`Requested`, `Redeemed`,
/// `Settled`) and read everything. Cannot grant — there is no method on this
/// type that produces a [`Grant`].
#[derive(Clone)]
pub struct Requester(Arc<LedgerInner>);

/// The read side (spec §A.1). Safe to hand to anyone: pending requests,
/// states, the log tail. Posts nothing.
///
/// The second field is the session this view is restricted to, when
/// [`Self::scope`] produced it. **The read side needs scoping as much as the
/// grant side does** (spec §A.7): under the always-on statement tap a request
/// carries the command text that raised it, so an unscoped reader in a
/// multi-session process reads every session's commands.
#[derive(Clone)]
pub struct Approvals(Option<Arc<LedgerInner>>, Option<SessionId>);

/// An append-only handle for recording assessments (spec §C.7): one
/// attributed judgment on the way to a decision. Never a decision itself —
/// appending an `Assessed` entry authorizes nothing, and it never bumps the
/// request's `revision` (spec §A.7's `KeyRetrieved` rationale applies
/// identically: an approver still deliberating must not have its quoted
/// revision invalidated by an assessment landing mid-thought).
///
/// Reached from [`Requester::assessments`] (the statement classifier's own
/// judgment, posted at the tap site before any decision exists — spec §C.6)
/// and from [`ApproverHandle::assessments`] (an embedder's pipeline,
/// appending while `Pending` — spec §C.7). Both derive one from whichever
/// handle they already hold; there is no separate construction path.
#[derive(Clone)]
pub struct AssessmentRecorder(Arc<LedgerInner>, Option<SessionId>);

impl AssessmentRecorder {
    /// Record one judgment. Requires the request to exist — an assessment
    /// about nothing to decide is a caller bug (spec §C.7 assumes a live
    /// `Requested` chain to attach to).
    pub async fn append(&self, assessment: ApprovalAssessment) -> Result<(), LedgerError> {
        self.0.check_scope(&assessment.request, self.1.as_ref())?;
        self.0.drain_outbox();
        self.0.post_assessment(assessment)
    }
}

/// What [`Policy::evaluate`](super::Policy::evaluate) sees alongside the
/// request and the read side (spec §C.7). Carries no deadline and no
/// cancellation token: `evaluate` is synchronous and non-blocking, so it has
/// nothing to bound and nothing to abandon (spec §C.2) — only a place to
/// record what it noticed on its way to a `Decision`.
#[non_exhaustive]
pub struct DecisionContext {
    /// Append a judgment here to have it survive on the log regardless of
    /// what `evaluate` returns — a `Defer` that later becomes a human's
    /// `Grant` still carries the reasoning that got it there.
    pub assessments: AssessmentRecorder,
}

/// The approval side's capability (spec §A.1). No public constructor — the
/// only way to get one is [`Ledger::build`]. Not `Default`, not
/// `Deserialize`, not reachable from script or tool code.
///
/// # Examples
///
/// This type's fields are private, so building one outside this crate is a
/// compile error — that boundary IS the type-system enforcement tier of the
/// spec's enforcement ladder (§E.2, tier 1). The example below is written to
/// fail to compile as the proof; `cargo test --doc` runs it on every gate:
///
/// ```compile_fail
/// use kaish_kernel::ledger::ApproverHandle;
///
/// // The tuple fields are private outside `kaish_kernel::ledger` — a
/// // downstream crate (what a doctest compiles as) can neither construct
/// // one from parts nor pattern the fields back out of one it already
/// // holds, e.g. from a real `Ledger::build` call.
/// fn peel(handle: ApproverHandle) {
///     let ApproverHandle(_inner, _authority, _principal, _session) = handle;
/// }
/// ```
///
/// The fourth field is the session this authority is restricted to, when
/// [`Self::scope`] produced it: a scoped handle can decide only within that
/// session (spec §A.7).
#[derive(Clone)]
pub struct ApproverHandle(Arc<LedgerInner>, AuthorityId, Principal, Option<SessionId>);

/// Names the authority and the principal it decides as. Carries no ledger
/// state and no credential — there is nothing here a log line could leak.
impl std::fmt::Debug for ApproverHandle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ApproverHandle")
            .field("authority", &self.1 .0)
            .field("principal", &self.2)
            .field("session", &self.3)
            .finish()
    }
}

/// A lightweight tag distinguishing which minted authority a clone of
/// [`ApproverHandle`] descends from. PR 2 mints exactly one per
/// [`Ledger::build`] call and every clone shares it; nothing in this PR
/// branches on it yet (`deny_self_approval` and multi-authority embedders
/// are later-PR territory) — kept because the spec's own type signature
/// (`ApproverHandle(Arc<LedgerInner>, AuthorityId)`) carries it.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct AuthorityId(u64);

/// What one execution reserved against a grant. Exposes only its own two
/// ids — never provenance (spec §C.1): a tool cannot tell whether its grant
/// came from a human, a policy hook, or a standing rule.
#[derive(Debug, Clone)]
pub struct AttemptHandle {
    request: RequestId,
    attempt: AttemptId,
}

impl AttemptHandle {
    /// Name an attempt the ledger already reserved. Kernel-internal: the
    /// only callers are this module's own redemption paths and
    /// `ExecContext`, which needs to build the guard for an attempt it just
    /// reserved. Naming an attempt does not authorize it — `settle` still
    /// refuses ids that name nothing live, and `redeem` re-checks the grant.
    pub(crate) fn from_reservation(request: RequestId, attempt: AttemptId) -> Self {
        Self { request, attempt }
    }

    /// The request this attempt was reserved against.
    pub fn request_id(&self) -> &RequestId {
        &self.request
    }

    /// This attempt's id.
    pub fn attempt_id(&self) -> AttemptId {
        self.attempt
    }
}

/// The full read-model of one request: its view, current state, grant (if
/// any), and every attempt reserved against it (spec §D.2's `Approvals::get`).
#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct RequestChain {
    /// The request itself (tokenless — see `ApprovalRequestView`).
    pub request: kaish_types::approval::ApprovalRequestView,
    /// Current top-level state.
    pub state: RequestState,
    /// The grant, once decided.
    pub grant: Option<Grant>,
    /// Every attempt reserved against this request, in no particular order.
    pub attempts: Vec<AttemptView>,
    /// Every `Assessed` judgment appended against this request, in append
    /// order (spec §C.7). Survives an abandoned or otherwise never-decided
    /// request — appending one never bumps `revision` and never requires a
    /// decision to follow it.
    pub assessments: Vec<kaish_types::approval::ApprovalAssessment>,
}

/// One attempt's read-model: its id, state, and outcome once settled.
#[non_exhaustive]
#[derive(Debug, Clone)]
pub struct AttemptView {
    /// The attempt's id.
    pub attempt: AttemptId,
    /// `Reserved`, `Settled`, or `Abandoned`.
    pub state: AttemptState,
    /// What it settled with, once `Settled`.
    pub outcome: Option<Outcome>,
}

/// Constructs a fresh ledger and its three handles. Not itself a public
/// type — `Ledger::build` is the only entry point (spec's `Ledger` is the
/// log; there is no separate `Ledger` value to hold onto afterward, only the
/// handles).
pub struct Ledger;

impl Ledger {
    /// Build a new, empty ledger and its three handles: obligations
    /// (`Requester`), read (`Approvals`), and the one minted authority
    /// (`ApproverHandle`). `sink` is optional — a ledger with none is purely
    /// in-memory and never applies sink backpressure (spec §D.4).
    ///
    /// `clock` is required rather than defaulted, so **one clock per
    /// ledger** is structural: there is no way to build one without saying
    /// which clock stamps its entries and which clock its bounds are
    /// compared against (spec §A.5). Pass
    /// `Arc::new(`[`SystemClock`](super::SystemClock)`)` for the default.
    ///
    /// # Errors
    ///
    /// Only if the OS cannot supply entropy for the ledger's id epoch (spec
    /// §A.2) — the same no-fallback stance credentials get: loud, never a
    /// weaker source.
    pub fn build(
        config: LedgerConfig,
        scope: ApprovalScope,
        sink: Option<Arc<dyn LedgerSink>>,
        clock: Arc<dyn Clock>,
    ) -> Result<(Requester, Approvals, ApproverHandle), getrandom::Error> {
        let inner = build_inner(config, scope, sink, clock)?;
        Ok((
            Requester(Arc::clone(&inner)),
            Approvals(Some(Arc::clone(&inner)), None),
            ApproverHandle(inner, AuthorityId(1), default_authority_principal(1), None),
        ))
    }
}

impl Requester {
    /// Post a request. `draft` carries only what a producer supplies
    /// (`ApprovalRequest::builder`, spec §D.1); `origin` carries everything
    /// the kernel stamps — scope, parent, binding, principal, capture — so a
    /// caller cannot forge a principal or an invocation.
    pub async fn post_request(
        &self,
        draft: ApprovalRequestDraft,
        origin: RequestOrigin,
    ) -> Result<ApprovalRequest, LedgerError> {
        // A dropped guard's queued settlement can be the only thing standing
        // between a closed-but-undrained chain and room for this post:
        // `post_request` reads `live_count_total`/`live_count_by_principal`
        // and reserves ring/sink capacity, both of which a drained close
        // would relieve. Every append-producing method on this handle and
        // `ApproverHandle` drains first for the same reason — not only the
        // methods that read `live_attempt` directly (a review finding this
        // PR's first pass missed: `cargo test` never sees a live_capacity: 1
        // + a dropped guard + an immediate second post in the same breath,
        // so a narrower drain set looked sufficient until it was pointed
        // out).
        self.0.drain_outbox();
        self.0.post_request(draft, origin)
    }

    /// Reserve an attempt by name, using an internal redemption context
    /// (the replay path — spec §B.4) rather than a presented credential.
    /// `report` is what the [`StateResolver`](super::StateResolver)s saw for
    /// each of the grant's conditions, evaluated *outside* the ledger lock
    /// and carried in (spec §B.1); pass [`ConditionReport::none`] for an
    /// unconditioned grant. A report that could not observe a resource
    /// refuses and voids the grant — it never passes.
    pub async fn redeem(
        &self,
        id: &RequestId,
        by: Principal,
        report: ConditionReport,
    ) -> Result<AttemptHandle, LedgerError> {
        // A stale queued `Unknown` for this chain's live attempt would leave
        // `live_attempt` looking occupied to this call — drain first so a
        // dropped guard's settlement lands before the reservation check
        // reads it (spec §C.1's "drains on its next append").
        self.0.drain_outbox();
        self.0
            .redeem(id, by, report)
            .map(|attempt| AttemptHandle {
                request: id.clone(),
                attempt,
            })
    }

    /// Reserve an attempt by presenting a bearer credential (the
    /// `--confirm=<token>` path — spec §E.1). `id` is the request the
    /// presentation claims to be for (in the full system, resolved by the
    /// gate site's draft matcher, PR 5 — see this PR's flagged ambiguity in
    /// the PR body for why PR 2 takes it as a direct argument). A wrong
    /// `presented` value counts against `id`'s rejection total and, on the
    /// fifth, voids it (spec §F.3).
    pub async fn redeem_with_token(
        &self,
        id: &RequestId,
        presented: &str,
        by: Principal,
        report: ConditionReport,
    ) -> Result<AttemptHandle, LedgerError> {
        // See `redeem`'s identical drain — the same `live_attempt` staleness
        // concern applies to the token-presentation path.
        self.0.drain_outbox();
        self.0
            .redeem_with_token(id, presented, by, report)
            .map(|attempt| AttemptHandle {
                request: id.clone(),
                attempt,
            })
    }

    /// Report an attempt's terminal outcome. Idempotent by `AttemptId`
    /// (spec §A.1): settling an already-terminal attempt appends nothing
    /// and returns `Ok`. Returns whether a new entry was actually appended.
    pub async fn settle(&self, attempt: &AttemptHandle, outcome: Outcome) -> Result<bool, LedgerError> {
        self.0.drain_outbox();
        self.0.settle(&attempt.request, attempt.attempt, outcome)
    }

    /// Settle by explicit ids rather than this crate's opaque
    /// [`AttemptHandle`] — for `ExecContext::settle_with` (ledger PR 3),
    /// which receives a `kaish_tool_api::AttemptHandle` (a distinct type;
    /// see that crate's `approval` module for why) and has only
    /// `request_id()`/`attempt_id()` to work with, not this handle's
    /// private fields. Same idempotency and drain contract as
    /// [`Self::settle`].
    pub async fn settle_by_ids(
        &self,
        request: &RequestId,
        attempt: AttemptId,
        outcome: Outcome,
    ) -> Result<bool, LedgerError> {
        self.0.drain_outbox();
        self.0.settle(request, attempt, outcome)
    }

    /// Queue a best-effort settlement for the next drain, without settling
    /// synchronously. For `AttemptGuard::drop` (PR 3, `kaish-kernel`'s
    /// dispatcher), which cannot `.await` `Self::settle` from a destructor —
    /// see `LedgerInner::queue_outbox_settle`'s doc for the mechanism.
    ///
    /// Not part of the supported public surface — a tool has no legitimate
    /// reason to queue a settlement without also reserving the attempt, and
    /// this method exists purely for the dispatcher's own guard.
    #[doc(hidden)]
    pub fn queue_drop_settle(&self, attempt: &AttemptHandle, outcome: Outcome) {
        self.0.queue_outbox_settle(attempt.request.clone(), attempt.attempt, outcome);
    }

    /// Abandon a request before any attempt was reserved against it (job
    /// discarded, session shutdown — spec §B.2). Fails with
    /// `LedgerError::AttemptInFlight` if an attempt is currently `Reserved`
    /// — settle or let the sweep close it first.
    pub async fn abandon_request(&self, id: &RequestId, reason: impl Into<String> + Send) -> Result<(), LedgerError> {
        // An attempt whose guard already dropped (queued but not yet
        // drained) would wrongly still look `Reserved` to this call's
        // `AttemptInFlight` check.
        self.0.drain_outbox();
        self.0.abandon_request(id, reason.into())
    }

    /// A raw reading from this ledger's clock, for stamping the I/O-time
    /// metadata a redemption carries in — `Observation::at`, taken while
    /// `StateResolver`s run outside the critical section (spec §B.1).
    ///
    /// **This is not a decision seam.** It takes no lock and does not touch
    /// the monotonicity latch, so it must never be used to compare a bound
    /// or to decide anything; the ledger's own `now` does that, under the
    /// guard. What it guarantees is that an observation stamp comes from the
    /// same clock as the entry it lands inside, instead of from whatever
    /// clock the gate site happened to reach for.
    pub fn clock_reading(&self) -> std::time::SystemTime {
        self.0.clock_reading()
    }

    /// Record a credential presentation whose draft named no request the
    /// ledger can identify (spec §F.3 item 2). It counts against nothing —
    /// a guesser cannot void a request it cannot describe.
    pub fn reject_unmatched_key(&self) {
        self.0.record_unmatched_key();
    }

    /// Close an undecided request (spec §B.5). Nothing times a request out
    /// (spec §A.10), so this is the only way one that nobody decides ever
    /// ends — and every teardown path that can strand a request calls it.
    ///
    /// Cancellation is a **requester** action: the principal that owns the
    /// request may close it holding no authority at all. Refused when the
    /// request is already terminal, or when an attempt against it is in
    /// flight — the operation is running, so nothing is stranded yet.
    ///
    /// `expected_revision` is the revision the caller's view of the request
    /// was at (spec §B.6). A cancel quoting a stale one is refused and
    /// recorded as `RevisionRejected` rather than applied — this is what
    /// lets a deadline timer and a human's decision race safely: whichever
    /// commits first invalidates the other's quote.
    ///
    /// Returns the closed request, so a caller can walk its `supersedes`
    /// chain or re-raise the same intent.
    pub async fn cancel(
        &self,
        id: &RequestId,
        expected_revision: u64,
        by: Principal,
        reason: CancelReason,
    ) -> Result<ApprovalRequest, LedgerError> {
        // An attempt whose guard already dropped (queued but not yet
        // drained) would wrongly still look `Reserved` to the in-flight
        // check — the same reason `abandon_request` drains.
        self.0.drain_outbox();
        self.0.cancel(id, expected_revision, by, reason)
    }

    /// Post one `Observed` entry: an `observe` subscription covered a
    /// mutation, or the statement tap recorded a statement, and either way
    /// it proceeds (spec §C.5, §C.6). A record, not a request — no chain is
    /// opened, nothing lands in the live index, and there is no grant to
    /// redeem. Lives on the implementation side because the gate sites are
    /// what classify a path as observed; nothing here decides anything.
    ///
    /// `plan` is `Some` exactly for the statement tap.
    ///
    /// What `Err` means differs by caller, and deliberately: an `fs.*` gate
    /// site fails the operation closed, because an operator who subscribed
    /// asked for a complete record; the statement tap warns and runs, because
    /// nobody opted into a completeness guarantee there (spec §C.6).
    pub async fn observed(
        &self,
        operation: OperationId,
        scope: ApprovalScope,
        by: Principal,
        resources: Vec<ObservedResource>,
        plan: Option<Plan>,
    ) -> Result<(), LedgerError> {
        // Same reason as `post_request`: a queued settlement can be what
        // frees the ring capacity this append reserves.
        self.0.drain_outbox();
        self.0.post_observed(operation, scope, by, resources, plan)
    }

    /// This side's [`AssessmentRecorder`] — for the statement tap posting
    /// the classifier's own judgment (spec §C.6), before any request exists
    /// to hold authority over. Recording an assessment is not a privileged
    /// act (spec §C.7: "an `Assessed` entry authorizes nothing"), so the
    /// obligation side records its own scoping the same way it records
    /// `Observed`.
    pub fn assessments(&self) -> AssessmentRecorder {
        AssessmentRecorder(Arc::clone(&self.0), None)
    }
}

impl Approvals {
    /// A view with no live ledger behind it — the default for a context
    /// that has none (a unit-test harness, a minimal embedder). Every
    /// method returns empty/`None`; grants nothing, because it cannot: it
    /// holds no `LedgerInner` to post against even internally.
    pub fn empty() -> Self {
        Self(None, None)
    }

    /// A read side restricted to one session (spec §A.7). Every method on
    /// the returned view reports only requests and records raised in
    /// `session`, and a request with no session at all is invisible to it.
    ///
    /// Needed as much as the grant side is: a request carries the command
    /// text that raised it, so an unscoped reader in a multi-session process
    /// reads every session's commands.
    pub fn scope(&self, session: SessionId) -> Self {
        Self(self.0.clone(), Some(session))
    }

    /// The session this view is restricted to, if any.
    pub fn session(&self) -> Option<&SessionId> {
        self.1.as_ref()
    }

    /// Every currently-`Requested` (undecided) request in this view's scope.
    pub fn pending(&self) -> Vec<kaish_types::approval::ApprovalRequestView> {
        self.0
            .as_ref()
            .map(|inner| {
                inner
                    .pending(self.1.as_ref())
                    .into_iter()
                    .map(Into::into)
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Every request the ledger still holds a chain for — pending, decided,
    /// and closed-but-retained — sorted by id.
    ///
    /// This is what enumerates `/v/approvals`: a request that has already
    /// been decided still has a `state`, a `grant`, and an attempt chain
    /// worth reading, so listing only the pending set would hide exactly the
    /// records §E asks an auditor to read. Bounded by the ledger's retention
    /// (`LedgerConfig::retained_entries`), never by the process's lifetime.
    pub fn ids(&self) -> Vec<RequestId> {
        self.0
            .as_ref()
            .map(|inner| inner.ids(self.1.as_ref()))
            .unwrap_or_default()
    }

    /// The current top-level state of one request, if it exists.
    pub fn state(&self, id: &RequestId) -> Option<RequestState> {
        self.0.as_ref().and_then(|inner| inner.state(id, self.1.as_ref()))
    }

    /// The full read-model — request, decision, and every attempt.
    pub fn get(&self, id: &RequestId) -> Option<RequestChain> {
        self.0.as_ref().and_then(|inner| inner.chain(id, self.1.as_ref()))
    }

    /// Every live standing grant.
    pub fn standing(&self) -> Vec<StandingGrant> {
        self.0.as_ref().map(|inner| inner.standing()).unwrap_or_default()
    }

    /// Whether anything at all is subscribed.
    ///
    /// **One relaxed atomic load and no lock**, so a gate site may call it
    /// once per command and still cost an unsubscribed session nothing.
    /// `false` means [`Self::subscriptions`] would return an empty vector, so
    /// a caller that would allocate only to find nothing can skip the
    /// allocation.
    pub fn any_subscriptions(&self) -> bool {
        self.0.as_ref().is_some_and(|inner| inner.any_subscriptions())
    }

    /// Every live subscription, in issue order. Allocates — call
    /// [`Self::any_subscriptions`] first on a hot path.
    pub fn subscriptions(&self) -> Vec<Subscription> {
        self.0
            .as_ref()
            .map(|inner| inner.subscriptions())
            .unwrap_or_default()
    }

    /// Which request a fresh draft describes: same operation, same
    /// resource-reference set, newest first (spec §B.4's draft matcher).
    ///
    /// This is how a presented credential names its request without a blind
    /// reverse lookup on the key — so a *wrong* key still counts against the
    /// right request, which is what gives the rejected-attempt limit
    /// somewhere principled to attach (§F.3 item 2). Closed chains match
    /// too, so a key presented after a successful settlement reports what
    /// already happened instead of running the operation again.
    pub fn match_draft(
        &self,
        operation: &kaish_types::approval::OperationId,
        refs: &[kaish_types::approval::ResourceRef],
    ) -> Option<RequestId> {
        self.0
            .as_ref()
            .and_then(|inner| inner.match_draft(operation, refs, self.1.as_ref()))
    }

    /// The retained log, seq-cursored: every retained [`LedgerRecord`] with
    /// `sequence > since`, restricted to this view's scope. Pass `0` for the
    /// full retained tail.
    ///
    /// Records, not bare entries: a consumer reads the `schema_version` and
    /// the scope alongside the entry, and an entry a newer writer produced
    /// surfaces as unknown rather than vanishing (spec §A.5).
    pub fn log(&self, since: u64) -> Vec<LedgerRecord> {
        self.0
            .as_ref()
            .map(|inner| inner.log(since, self.1.as_ref()))
            .unwrap_or_default()
    }
}

impl ApproverHandle {
    /// Decide yes. Fails with `LedgerError::AlreadyDecided` if the request
    /// already has a decision, or `LedgerError::Terminal` if it is no
    /// longer `Requested` for any other reason (expired, voided, ...).
    ///
    /// `rev` is the revision the caller's view of the request was at (spec
    /// §B.6) — a grant quoting a stale one is refused and recorded as
    /// `RevisionRejected` rather than applied.
    pub async fn grant(&self, id: &RequestId, rev: u64, terms: GrantTerms) -> Result<(), LedgerError> {
        self.grant_with_grounds(id, rev, terms, Grounds::Embedder).await.map(|_| ())
    }

    /// Decide yes, recording *why* — the grounds the decision chain
    /// attaches when a `policy` or `decide` hook produced the decision
    /// (spec §A.4). [`Self::grant`] is this with `Grounds::Embedder`, which
    /// is what a direct embedder grant is.
    pub async fn grant_with_grounds(
        &self,
        id: &RequestId,
        rev: u64,
        terms: GrantTerms,
        grounds: Grounds,
    ) -> Result<Grant, LedgerError> {
        self.0.check_scope(id, self.3.as_ref())?;
        // `grant` appends and so reserves ring/sink capacity — the same
        // stale-undrained-close concern as `post_request`.
        self.0.drain_outbox();
        let decided_by = self.principal();
        self.0.grant(id, rev, terms, decided_by, grounds)
    }

    /// Decide no.
    ///
    /// `rev` is the revision the caller's view of the request was at (spec
    /// §B.6) — a denial quoting a stale one is refused and recorded as
    /// `RevisionRejected` rather than applied.
    pub async fn deny(&self, id: &RequestId, rev: u64, reason: &str) -> Result<(), LedgerError> {
        self.0.check_scope(id, self.3.as_ref())?;
        self.0.drain_outbox();
        let by = self.principal();
        self.0.deny(id, rev, reason.to_string(), by)
    }

    /// Issue a standing grant. `g.id` is overwritten with a
    /// ledger-allocated id regardless of what the caller set — the
    /// returned `StandingId` is the authoritative one (there is no
    /// separate draft type in `kaish-types` for a not-yet-issued
    /// `StandingGrant`; see this PR's flagged ambiguity in the PR body).
    /// **Pure record only** — matching a live request against this rule is
    /// PR 4's decision-chain job, not this method's.
    pub async fn grant_standing(&self, g: StandingGrant) -> Result<StandingId, LedgerError> {
        self.0.drain_outbox();
        self.0.grant_standing(g)
    }

    /// Revoke a standing grant. Takes effect immediately for requests not
    /// yet granted; already-issued grants are unaffected (spec §C.4).
    pub async fn revoke_standing(&self, id: &StandingId, reason: &str) -> Result<(), LedgerError> {
        self.0.drain_outbox();
        let by = self.principal();
        self.0.revoke_standing(*id, by, reason.to_string())
    }

    /// Retrieve the credential for a live, granted request. Appends
    /// `KeyRetrieved` naming this handle's principal (spec §A.2) —
    /// retrieval is the one place authority matters on the key path (spec
    /// §D.3): everything else works by presenting whatever key you hold.
    pub fn token_for(&self, id: &RequestId) -> Option<Token> {
        // A scoped handle retrieves nothing outside its session: the key is
        // a bearer credential, so handing one over is the widest thing this
        // handle does (spec §A.2, §A.7).
        self.0.check_scope(id, self.3.as_ref()).ok()?;
        // Sync, like the rest of this method — `drain_outbox` is plain
        // `Mutex` work with no `.await` inside it (see its doc).
        self.0.drain_outbox();
        let by = self.principal();
        self.0.token_for(id, by)
    }

    /// Stage 1 of the decision chain (spec §C.2): if a live standing grant
    /// covers this request, charge one use against it and post
    /// `Granted{grounds: Standing}` — match and consumption in the same
    /// critical section, so `max_uses` holds exactly under concurrency
    /// (spec §C.4, §B.1). `Ok(None)` means no rule covered the request and
    /// the caller falls through to stage 2.
    ///
    /// `grant_ttl` bounds the resulting grant, clamped down to the winning
    /// rule's own `expires_at`. The deciding principal on the grant is the
    /// rule's `issued_by` — whoever wrote the rule made this call, not
    /// whoever happens to hold this handle.
    ///
    /// Lives on the authority handle because it posts a `Granted` entry;
    /// [`DecisionChain`](super::DecisionChain) is the only caller.
    pub async fn grant_from_standing(
        &self,
        id: &RequestId,
        grant_ttl: Duration,
    ) -> Result<Option<Grant>, LedgerError> {
        self.0.check_scope(id, self.3.as_ref())?;
        self.0.grant_from_standing(id, grant_ttl)
    }

    /// Register a subscription: a glob over (operation, resource) that puts
    /// matching filesystem operations into `observe` (record and proceed) or
    /// `enforce` (decide) posture. `s.id` is overwritten with a
    /// ledger-allocated id whatever the caller set — the returned
    /// [`SubscriptionId`] is the authoritative one, the same contract
    /// [`Self::grant_standing`] has.
    ///
    /// Appends a `Subscribed` entry, so the registration is itself a ledger
    /// fact — an audit scope that changed with no record of the change would
    /// make the record it produced unreadable.
    pub async fn subscribe(&self, s: Subscription) -> Result<SubscriptionId, LedgerError> {
        self.0.drain_outbox();
        self.0.subscribe(s)
    }

    /// Revoke a subscription, appending `Unsubscribed`. Takes effect for
    /// operations not yet posted; a request already granted under it is
    /// unaffected, the same rule standing-grant revocation follows.
    pub async fn unsubscribe(&self, id: &SubscriptionId, reason: &str) -> Result<(), LedgerError> {
        self.0.drain_outbox();
        let by = self.principal();
        self.0.unsubscribe(*id, by, reason.to_string())
    }

    /// How many uses have been charged against a standing grant so far
    /// (spec §C.4). The same number the log reconstructs by counting
    /// `Granted{grounds: Standing{id}}` entries; this is the cheap read.
    pub fn standing_uses(&self, id: &StandingId) -> u32 {
        self.0.standing_uses(*id)
    }

    /// Derive this ledger's three handles from an authority already held —
    /// how a second kernel joins an existing ledger rather than minting one
    /// (`KernelConfig::with_approver_handle`).
    ///
    /// Grants no new capability: the returned [`Requester`] and
    /// [`Approvals`] are both strictly weaker than the handle they come
    /// from, and the returned authority is this same one.
    pub fn join(&self) -> (Requester, Approvals, ApproverHandle) {
        (
            Requester(Arc::clone(&self.0)),
            Approvals(Some(Arc::clone(&self.0)), self.3.clone()),
            self.clone(),
        )
    }

    /// A view of this authority restricted to one session (spec §A.7). The
    /// derived handle can grant, deny, and retrieve keys only within that
    /// scope; every other request is [`LedgerError::OutOfScope`].
    ///
    /// This is API hygiene, not a process boundary — see
    /// [`ApprovalScope`](kaish_types::approval::ApprovalScope).
    pub fn scope(&self, session: SessionId) -> Self {
        Self(Arc::clone(&self.0), self.1, self.2.clone(), Some(session))
    }

    /// The session this authority is restricted to, if any.
    pub fn session(&self) -> Option<&SessionId> {
        self.3.as_ref()
    }

    /// The scope this ledger stamps on a record that owns no request — the
    /// kernel scope `Ledger::build` was given.
    pub fn ledger_scope(&self) -> ApprovalScope {
        self.0.scope()
    }

    /// Re-attribute this handle to `principal`. The returned handle shares
    /// the same ledger and the same minted authority — it grants exactly
    /// what this one grants — and records `principal` as the decider on
    /// every grant, denial, and credential retrieval made through it.
    ///
    /// This is the embedder's principal-assignment seam (spec §E.2, tier 2):
    /// the kernel enforces the requester/approver split and trusts the
    /// embedder to say who is who. `KernelConfig::with_principal` is what
    /// calls it in-tree.
    pub fn with_principal(mut self, principal: Principal) -> Self {
        self.2 = principal;
        self
    }

    /// This handle's own read side — for an authority that needs to look at
    /// the request it is about to act on (`Kernel::confirm` reads the
    /// capture). Grants nothing beyond what [`Approvals`] already grants;
    /// taking it from the handle is what proves the request belongs to the
    /// ledger the caller holds authority over.
    pub fn approvals_view(&self) -> Approvals {
        Approvals(Some(Arc::clone(&self.0)), self.3.clone())
    }

    /// This authority's [`AssessmentRecorder`] (spec §C.7) — for an
    /// embedder's pipeline appending judgments while a request sits
    /// `Pending`, whether or not a decision ever follows. Scoped like every
    /// other method on this handle: a session-restricted authority records
    /// assessments only within its own session.
    pub fn assessments(&self) -> AssessmentRecorder {
        AssessmentRecorder(Arc::clone(&self.0), self.3.clone())
    }

    /// The principal recorded as deciding/retrieving through this handle.
    /// Seeded to a distinct per-authority placeholder at mint time and
    /// replaced by [`Self::with_principal`] — which is what
    /// `KernelConfig::with_principal` does — so an embedder that never
    /// assigns identity still produces a name an auditor can distinguish
    /// from a real one.
    fn principal(&self) -> Principal {
        self.2.clone()
    }
}

/// The identity a freshly minted authority decides as until an embedder
/// assigns one. Deliberately not `Principal::default()` (an empty id), which
/// would collide with every other unnamed actor in the per-principal live
/// index.
fn default_authority_principal(authority: u64) -> Principal {
    Principal::new(
        format!("approver-handle-{authority}"),
        kaish_types::approval::PrincipalKind::Automation,
    )
}
