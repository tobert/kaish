//! `ToolCtx`'s approval-ledger surface (`docs/approval-ledger.md`, ledger PR
//! 3): the types a tool sees when it gates a privileged operation through
//! [`crate::ToolCtx::request_approval`].
//!
//! These are deliberately **not** `kaish_kernel::ledger`'s handle types —
//! this crate cannot depend on `kaish-kernel` (the dependency runs the other
//! way: `kaish-kernel`'s `ExecContext` implements `ToolCtx`). `AttemptHandle`
//! and `Approvals` here are small, tool-facing values that the kernel's real
//! `ToolCtx` impl builds from its own live ledger state; `kaish-kernel`'s
//! `ledger::AttemptHandle`/`ledger::Approvals` remain the embedder-level
//! handles behind `Kernel::approvals()` (spec §D.2) and are unrelated types
//! that happen to share a name with their tool-facing counterpart here.

use kaish_types::approval::{AttemptId, RequestId, RequestState, StateClaim};
use kaish_types::ExecResult;

/// What one execution reserved against a grant (spec §C.1). Exposes only its
/// own two ids — never provenance: a tool cannot tell whether its grant came
/// from a human, a policy hook, or a standing rule.
///
/// A handle is a *name*, not a capability. [`Self::from_reservation`] is
/// `#[doc(hidden)] pub` because the kernel lives in another crate and must
/// build one for an attempt it just reserved — so the boundary that makes
/// settlement safe is on the consuming side instead: `ToolCtx::settle_with`
/// refuses any handle naming an attempt the calling execution did not
/// reserve, and `redeem` re-checks the grant. Naming someone else's attempt
/// therefore buys nothing.
#[derive(Debug, Clone)]
pub struct AttemptHandle {
    request: RequestId,
    attempt: AttemptId,
}

impl AttemptHandle {
    /// The request this attempt was reserved against.
    pub fn request_id(&self) -> &RequestId {
        &self.request
    }

    /// This attempt's id.
    pub fn attempt_id(&self) -> AttemptId {
        self.attempt
    }

    /// Name an attempt the ledger reserved. Not part of the supported
    /// public surface — for the kernel's own `ToolCtx` implementation, which
    /// reserves the attempt and then has to hand the tool a handle for it
    /// across a crate boundary.
    ///
    /// A handle built here for an attempt the caller did not reserve is
    /// inert: `ToolCtx::settle_with`'s implementation checks that the
    /// attempt belongs to the calling execution before settling it, and
    /// `redeem` re-checks the grant, so a guessed or observed pair of ids
    /// authorizes nothing and settles nothing.
    #[doc(hidden)]
    pub fn from_reservation(request: RequestId, attempt: AttemptId) -> Self {
        Self { request, attempt }
    }
}

/// The decision `ToolCtx::request_approval` returns (spec §C.1). Every
/// variant but `Authorized` fails closed — see [`Self::proceed`].
#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum ApprovalOutcome {
    /// A grant existed (or was decided inline) and an attempt is reserved.
    Authorized(AttemptHandle),
    /// Nobody has decided, and the kernel will not wait to find out (spec
    /// §0.1). The normal outcome for anything a human or a model has to
    /// think about. Carries what a caller needs to present the request and
    /// to resume it; the view is tokenless (spec §A.2).
    Pending(Box<PendingApproval>),
    /// The request was denied.
    Denied {
        /// The denied request.
        request: RequestId,
        /// Why.
        reason: String,
    },
    /// A precondition on the grant no longer holds, or could not be
    /// observed. The grant is voided and the operation must re-request
    /// (spec §B.4).
    Refused {
        /// The refused request.
        request: RequestId,
        /// Why.
        detail: String,
    },
    /// The request is over: cancelled, past a deadline the embedder set,
    /// voided, or abandoned. Nothing is wrong with the ledger and retrying
    /// this request will never work — ask again, which posts a new request
    /// (spec §B.5).
    ///
    /// Distinct from [`Self::LedgerUnavailable`], which used to answer this
    /// question too. One says *the ledger could not record this, try
    /// again*; this one says *this request is over*.
    Closed {
        /// The closed request.
        request: RequestId,
        /// Which terminal state closed it.
        state: RequestState,
        /// What the ledger recorded about the closing, when it recorded
        /// something the state alone does not say — "voided after 5
        /// invalid attempts". Empty otherwise.
        detail: String,
    },
    /// This context has no ledger — a unit-test harness or a minimal
    /// embedder. The default [`crate::ToolCtx::request_approval`] impl
    /// always returns this.
    Unsupported,
    /// The ledger refused to record: sink backpressure or live capacity
    /// (spec §D.4). A condition of the *ledger*, and retryable. Never used
    /// to report a request's own state — that is [`Self::Closed`].
    LedgerUnavailable {
        /// Why.
        reason: String,
    },
    /// The execution was cancelled before a grant could be recorded.
    /// Nothing was granted and nothing will run.
    ///
    /// This is the *execution's* cancellation token, not a decision that
    /// was raced: no hook is awaited on the request path (spec §C.2), so
    /// there is never a decision in flight here.
    Cancelled {
        /// The request that was posted before the cancellation.
        request: RequestId,
    },
    /// A credential was presented for an operation no live or retained
    /// request describes (spec §B.4's draft matcher). Nothing was redeemed,
    /// and the presentation counted against no request.
    Unmatched {
        /// What was presented, as the matcher saw it.
        detail: String,
    },
}

impl ApprovalOutcome {
    /// The request this outcome is about, when it is about one. `None` for
    /// [`Self::Unsupported`] and [`Self::LedgerUnavailable`] — conditions of
    /// the ledger itself, not of any particular request.
    pub fn request_id(&self) -> Option<&RequestId> {
        match self {
            Self::Authorized(attempt) => Some(attempt.request_id()),
            Self::Pending(pending) => Some(&pending.request.id),
            Self::Denied { request, .. }
            | Self::Refused { request, .. }
            | Self::Closed { request, .. }
            | Self::Cancelled { request, .. } => Some(request),
            Self::Unsupported | Self::LedgerUnavailable { .. } | Self::Unmatched { .. } => None,
        }
    }

    /// Convert every non-`Authorized` variant into the `ExecResult` a gate
    /// site returns verbatim (spec §C.1) — the one call pattern:
    ///
    /// ```ignore
    /// let attempt = ctx.request_approval(req).await.proceed()?;
    /// ```
    ///
    /// `Pending` maps to exit 2 with the whole [`PendingApproval`] — view and
    /// resume route together — on [`ExecResult::approval`];
    /// `Denied`, `Refused`, `Closed`, `Unsupported`, and `LedgerUnavailable`
    /// map to exit 1 with a message naming the reason. `Authorized` is the
    /// only variant that lets the caller continue.
    // `ExecResult` is deliberately fat (see `kaish_trash.rs`'s identical
    // allow) — a gate site returns it verbatim, and boxing it here would
    // just move the same allocation the caller immediately unboxes again.
    #[allow(clippy::result_large_err)]
    pub fn proceed(self) -> Result<AttemptHandle, ExecResult> {
        match self {
            Self::Authorized(attempt) => Ok(attempt),
            Self::Pending(pending) => {
                let mut result = ExecResult::failure(
                    2,
                    format!(
                        "pending approval {} — an operator must grant it",
                        pending.request.id
                    ),
                );
                // Carries the whole PendingApproval, not just the view: a
                // resume-route consumer (the REPL, `Kernel::confirm`
                // callers) reads `.approval.resume` straight off the
                // result instead of re-deriving it from the capture's
                // shape (kaish#312 — the REPL had to do exactly that before
                // this field carried the route it needed).
                result.approval = Some(pending);
                Err(result)
            }
            Self::Denied { request, reason } => {
                Err(ExecResult::failure(1, format!("request {request} denied: {reason}")))
            }
            Self::Refused { request, detail } => {
                Err(ExecResult::failure(1, format!("request {request} refused: {detail}")))
            }
            Self::Closed { request, state, detail } => {
                let mut message = format!(
                    "request {request} is {state:?} and can no longer be decided — ask again to post a new one"
                );
                if !detail.is_empty() {
                    message.push_str(&format!(" ({detail})"));
                }
                Err(ExecResult::failure(1, message))
            }
            Self::Unsupported => Err(ExecResult::failure(
                1,
                "approval ledger not available in this context",
            )),
            Self::LedgerUnavailable { reason } => {
                Err(ExecResult::failure(1, format!("approval ledger unavailable: {reason}")))
            }
            Self::Cancelled { request } => Err(ExecResult::failure(
                1,
                format!("request {request} was cancelled before a decision was recorded"),
            )),
            Self::Unmatched { detail } => Err(ExecResult::failure(
                1,
                format!("the presented key matches no approval request — {detail}"),
            )),
        }
    }
}

// `PendingApproval` and `ResumeAction` live in `kaish-types` (kaish#312): the
// dependency runs `kaish-tool-api` → `kaish-types`, and `ExecResult.approval`
// (defined in `kaish-types`) carries the whole `PendingApproval`, not just its
// view — so the type cannot live up here without an impossible back-edge.
// Re-exported so every existing `kaish_tool_api::PendingApproval` import
// keeps working unchanged.
pub use kaish_types::approval::{PendingApproval, ResumeAction};

/// Read-only view for tools that surface pending approvals (`approvals`,
/// `wait`, `jobs` — spec §D.1). [`Self::empty`] backs the default
/// [`crate::ToolCtx::approvals`] for a context with no ledger and grants
/// nothing, because it cannot: it carries no authority, only a snapshot.
#[derive(Debug, Clone, Default)]
pub struct Approvals {
    pending: Vec<kaish_types::approval::ApprovalRequestView>,
}

impl Approvals {
    /// A view with nothing pending — the default for a context with no
    /// ledger.
    pub fn empty() -> Self {
        Self::default()
    }

    /// Every currently-pending (undecided) request, as of when this
    /// snapshot was taken.
    pub fn pending(&self) -> &[kaish_types::approval::ApprovalRequestView] {
        &self.pending
    }

    /// Build a snapshot from a live pending list.
    ///
    /// Not part of the supported public surface — for the kernel's
    /// `ExecContext` to populate a real snapshot from its ledger handle.
    #[doc(hidden)]
    pub fn from_pending(pending: Vec<kaish_types::approval::ApprovalRequestView>) -> Self {
        Self { pending }
    }
}

// ───────────────────────── State resolution ─────────────────────────

/// Reads the current state of one resource kind, so a grant's preconditions
/// can be re-checked at redemption (spec §B.4). The kernel ships one for
/// `path`; a plugin ships one per kind it names in a `Resource` — kaish-git
/// ships `git.ref`.
///
/// A resolver does I/O, so redemption calls it **outside** the ledger's
/// critical section and carries the observation in (spec §B.1). It therefore
/// detects a resource that moved between the grant and the redemption; it
/// does not make the operation itself atomic.
#[async_trait::async_trait]
pub trait StateResolver: Send + Sync {
    /// The [`kaish_types::approval::Resource`] kind this resolver answers
    /// for, matched exactly. `path` is reserved for the kernel.
    fn kind(&self) -> &str;

    /// The resource's current state. An unreadable resource is `Err` and
    /// **refuses the redemption** — never `Ok(StateClaim::Unspecified)`,
    /// which would silently pass a precondition nobody checked.
    async fn observe(&self, id: &str) -> Result<StateClaim, ResolverError>;
}

/// Why a [`StateResolver`] could not state a resource's current state. Every
/// variant refuses the redemption: an unobservable precondition is never a
/// passing one.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolverError {
    /// The resource's store could not be read.
    Io(String),
    /// `id` is not well-formed for this resolver's kind.
    InvalidId(String),
    /// The resource exists but this kind has no state this resolver can
    /// state — a directory has no content digest.
    Unsupported(String),
}

impl std::fmt::Display for ResolverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Io(detail) => write!(f, "{detail}"),
            Self::InvalidId(detail) => write!(f, "malformed resource id: {detail}"),
            Self::Unsupported(detail) => write!(f, "{detail}"),
        }
    }
}

impl std::error::Error for ResolverError {}

// ───────────────────────── Redaction (spec §A.8) ─────────────────────────


