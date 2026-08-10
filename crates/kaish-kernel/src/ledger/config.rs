//! Ledger sizing, the audit-sink trait, and the sink's own error type
//! (`docs/approval-ledger.md` §D.4).

use kaish_types::approval::LedgerRecord;

/// Sizing for one [`super::Ledger`] (spec §D.4). There is no timeout here
/// and there will not be one: the kernel never reads a clock to decide
/// anything (spec §A.10), so capacity is the whole backstop against an
/// embedder that asks and never answers. Every field has a default matching
/// the spec's stated number; start from [`LedgerConfig::default`] and
/// override with the `with_*` builder methods below. `#[non_exhaustive]`,
/// like the rest of this module's public types — a field added later must
/// not be a breaking change for an embedder outside this crate, so
/// struct-literal construction (and functional-update syntax) is available
/// only inside this crate; use the builder from anywhere else.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct LedgerConfig {
    /// Maximum LIVE (unclosed) requests. Closed chains do not count against
    /// it. Default 1024.
    pub live_capacity: usize,
    /// Per-principal share of `live_capacity` — one principal cannot starve
    /// the others. Default 256.
    pub live_capacity_per_principal: usize,
    /// Retained entries in the audit ring — never an entry belonging to a
    /// still-live request (spec §D.4), and never a terminal entry
    /// (`Settled`, or attempt-level `Abandoned`) for work that already ran,
    /// whose room is reserved at redemption time and is therefore never
    /// competed for. Eviction picks the *oldest evictable* entry, not
    /// strictly the oldest overall — a single long-lived request sitting at
    /// the front of the ring must not be able to permanently block eviction
    /// of closed entries behind it. Default 4096.
    pub retained_entries: usize,
    /// Bounded sink queue depth. Default 1024 entries.
    pub sink_queue: usize,
    /// The rejection count at which a request's chain voids. Default 5 (the
    /// counter kaish #259 deferred — spec §F.3).
    pub max_token_attempts: u32,
    /// Refuse a grant whose issuing principal equals the request's own
    /// principal (spec §D.2, §E.7). Default false: a solo human at the REPL
    /// is legitimately both requester and approver, so this is an opt-in
    /// policy for multi-principal embedders, not a blanket
    /// approver-never-equals-requester invariant. Enforced at the one place
    /// a `Granted` entry is appended, so it covers an explicit grant, a
    /// chain-decided grant, and a standing-grant auto-approval alike. Its
    /// job is catching misconfiguration (an agent session handed a handle
    /// it should not use to approve its own requests), not resisting an
    /// attacker — the ledger records both principals on every grant
    /// regardless of this flag.
    pub deny_self_approval: bool,
}

impl Default for LedgerConfig {
    fn default() -> Self {
        Self {
            live_capacity: 1024,
            live_capacity_per_principal: 256,
            retained_entries: 4096,
            sink_queue: 1024,
            max_token_attempts: 5,
            deny_self_approval: false,
        }
    }
}

impl LedgerConfig {
    /// Maximum LIVE (unclosed) requests. Closed chains do not count against
    /// it. Default 1024.
    pub fn with_live_capacity(mut self, live_capacity: usize) -> Self {
        self.live_capacity = live_capacity;
        self
    }

    /// Per-principal share of `live_capacity` — one principal cannot starve
    /// the others. Default 256.
    pub fn with_live_capacity_per_principal(mut self, live_capacity_per_principal: usize) -> Self {
        self.live_capacity_per_principal = live_capacity_per_principal;
        self
    }

    /// Retained entries in the audit ring. Default 4096. See the field doc
    /// on [`LedgerConfig::retained_entries`] for what is and is not
    /// evictable.
    pub fn with_retained_entries(mut self, retained_entries: usize) -> Self {
        self.retained_entries = retained_entries;
        self
    }

    /// Bounded sink queue depth. Default 1024 entries.
    pub fn with_sink_queue(mut self, sink_queue: usize) -> Self {
        self.sink_queue = sink_queue;
        self
    }

    /// The rejection count at which a request's chain voids. Default 5.
    pub fn with_max_token_attempts(mut self, max_token_attempts: u32) -> Self {
        self.max_token_attempts = max_token_attempts;
        self
    }

    /// Refuse a grant whose issuing principal equals the request's own
    /// principal. Default false. See the field doc on
    /// [`LedgerConfig::deny_self_approval`] for the full policy rationale.
    pub fn with_deny_self_approval(mut self, deny_self_approval: bool) -> Self {
        self.deny_self_approval = deny_self_approval;
        self
    }
}

/// Why a [`LedgerSink::post`] call failed.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LedgerSinkError(pub String);

impl std::fmt::Display for LedgerSinkError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ledger sink: {}", self.0)
    }
}

impl std::error::Error for LedgerSinkError {}

/// An export destination for the ledger's append-only log (spec §D.4).
///
/// **The exact delivery contract**, precisely, because it has real edges:
///
/// - `post` must be fast and non-blocking. Every entry is delivered through
///   a *reserved* [`tokio::sync::mpsc::OwnedPermit`], taken synchronously
///   (never awaited) when the entry is admitted, so `post` itself always has
///   a guaranteed queue slot waiting and only ever needs to do its own I/O —
///   never any capacity negotiation with the ledger.
/// - A bounded background task drains the queue (`LedgerConfig::sink_queue`
///   deep) and calls `post` once per entry, in commit order. While the
///   queue has room, admitting a new entry never blocks on the sink at all.
/// - **Backpressure fails new obligations closed, never the sink itself.**
///   Once the queue's `sink_queue` permits are all reserved, the *next*
///   obligation (`post_request`, `grant`, `deny`, ...) is refused with
///   `LedgerError::SinkUnavailable` rather than blocking the async executor
///   or silently dropping the entry. A sink fronting something slow (a
///   network log) that cannot tolerate this should buffer internally and
///   return `Ok` quickly, accepting the buffering risk explicitly — the
///   kernel will not make that tradeoff on the embedder's behalf.
/// - **A terminal entry (`Settled`, or attempt-level `Abandoned`) is never
///   refused by this backpressure.** Its queue slot is reserved together
///   with the `Redeemed` entry that started its attempt, before the attempt
///   is ever allowed to begin — an operation that already ran must always
///   be able to record what happened (spec §D.4 / review finding B3).
/// - **An `Err` here trips the sink for the life of the ledger — there is no
///   retry.** The background task sets an internal failed flag and stops
///   consuming; every entry still queued behind the one that failed (plus
///   the one that failed) is counted as undelivered and surfaced in later
///   `SinkUnavailable` messages ("N audit entries undelivered" — review
///   finding S3), so the loss is accounted, never silent. The ledger then
///   refuses every new obligation until the process restarts — an
///   unrecorded privileged operation is exactly the corruption this design
///   refuses to risk.
pub trait LedgerSink: Send + Sync {
    /// Append one record. See the trait doc for the exact delivery,
    /// backpressure, and failure contract.
    ///
    /// A sink receives a [`LedgerRecord`], never a bare `LedgerEntry`: the
    /// envelope carries the `schema_version` and the scope a later reader
    /// needs to tell whose record it is holding and whether it understands
    /// it (spec §A.5).
    fn post(&self, record: &LedgerRecord) -> Result<(), LedgerSinkError>;
}
