//! Ledger sizing, the audit-sink trait, and the sink's own error type
//! (`docs/approval-ledger.md` §D.4).

use std::time::Duration;

use kaish_types::approval::LedgerRecord;

/// Sizing and timeouts for one [`super::Ledger`] (spec §D.4). Every field has
/// a default matching the spec's stated number; construct with
/// `LedgerConfig { field: ..., ..Default::default() }` to override one —
/// deliberately **not** `#[non_exhaustive]`, unlike the rest of this
/// module's public types, so that functional-update pattern keeps working
/// for embedders (and this crate's own integration tests) as fields are
/// added.
#[derive(Debug, Clone)]
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
    /// How long a request stays live with no decision. Default 60s (today's
    /// nonce TTL).
    pub request_ttl: Duration,
    /// The rejection count at which a request's chain voids. Default 5 (the
    /// counter kaish #259 deferred — spec §F.3).
    pub max_token_attempts: u32,
    /// How long a `Reserved` attempt may go unreported before the recovery
    /// sweep closes it as `Abandoned`. Not in the spec's own `LedgerConfig`
    /// field list — added here because PR 3's `AttemptGuard` (the drop-safe
    /// settlement guard that gives the sweep a real "the process died"
    /// signal) does not land until the next PR, and a sweep with no
    /// staleness bound at all would never resolve to
    /// `Outcome::Unknown{ExecutorLost}`. Default 10 minutes — generous
    /// enough that no in-flight operation should ever cross it. Superseded
    /// once `AttemptGuard`'s outbox exists; flagged for review then.
    pub attempt_stale_after: Duration,
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
            request_ttl: Duration::from_secs(60),
            max_token_attempts: 5,
            attempt_stale_after: Duration::from_secs(600),
            deny_self_approval: false,
        }
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
