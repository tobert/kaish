//! The clock the ledger reads (`docs/approval-ledger.md` §A.5, §A.10).
//!
//! **The kernel does not own an opinion about which clock is true.** It owns
//! two things: that there is exactly one clock per ledger, and that the
//! ledger's view of it never goes backwards. Which clock that is belongs to
//! the embedder, the same way policy, deadlines, and redaction do (spec
//! §0.1) — a bridge that synchronizes against a coordinator, a replay
//! harness driving recorded readings, and a test are all legitimate, and
//! none of them is the kernel's business.
//!
//! What the ledger does with a reading is narrow and worth stating exactly:
//!
//! - It **stamps** every entry with the reading taken at that entry's commit
//!   point. An append-only record of security decisions with no timestamps
//!   is not auditable.
//! - It **compares** two bounds against a reading, at the moment somebody
//!   acts on the chain: a grant's `not_after`, and the optional `deadline`
//!   an embedder set on a request. Neither is enforced on a timer, and no
//!   decision in the ledger consults a clock beyond those two comparisons
//!   (spec §A.10).
//!
//! Nothing else reads it. `Instant`-based machinery elsewhere in the kernel
//! — the script watchdog, `timeout`, `ToolCtx::patient` — is unrelated and
//! is not installed here.

use std::time::SystemTime;

/// A source of readings for one ledger.
///
/// Install one with
/// [`KernelConfig::with_approval_clock`](crate::KernelConfig::with_approval_clock);
/// the default is [`SystemClock`]. Every entry stamp and every bound
/// comparison in that ledger reads this one clock, so a record's timestamps
/// and the decisions taken alongside them can never come from two different
/// sources.
///
/// **A reading may be anything; the ledger's *view* of it is monotone
/// non-decreasing.** The ledger latches the largest reading it has seen and
/// clamps a smaller one up to that latch. That is mechanism, not policy —
/// the same kind of guarantee `sequence` gives ordering — and it is what
/// makes "an expired grant stays expired" hold without the kernel having to
/// know anything about the clock behind it.
///
/// **The latch is permanent for the ledger's lifetime, and a forward spike
/// is unrecoverable without a restart.** A clock that jumps far ahead and
/// then comes back pins the view at the spike: every grant whose `not_after`
/// the spike passed is expired, and stays expired once the clock recovers,
/// because the ledger cannot tell a spike from a legitimate advance — a
/// clock that reads an hour ahead and one that is an hour ahead are the same
/// two readings. That is the price of the guarantee, and it is the side to
/// be wrong on: the alternative is a ledger whose invariants hold only while
/// the installed clock behaves. A process restart is the recovery, and it is
/// the only one. An embedder whose clock can step forward and back —
/// anything synchronizing against an external source — should install a
/// clock that smears the correction rather than stepping it.
///
/// Implementations must be cheap and must not block: `now` is called inside
/// the ledger's critical section, once per transaction.
pub trait Clock: Send + Sync {
    /// This clock's current reading.
    fn now(&self) -> SystemTime;
}

/// The default [`Clock`]: reads the system clock.
///
/// That is a fact about the default, not about the design. An embedder that
/// wants readings from somewhere else installs its own and the ledger does
/// not notice — it only ever compares readings from one clock against bounds
/// expressed in the same clock's terms.
#[derive(Debug, Default, Clone, Copy)]
pub struct SystemClock;

impl Clock for SystemClock {
    fn now(&self) -> SystemTime {
        kaish_types::clock::system_now()
    }
}
