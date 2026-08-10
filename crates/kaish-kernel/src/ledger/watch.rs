//! [`Approvals::watch`](super::Approvals::watch) — the one convenience the
//! kernel offers around waiting (spec §D.2). It reports that an entry
//! landed and nothing else: no deadline, no `wait_for_decision`, no queue
//! of parked decisions the kernel holds on the embedder's behalf. Every one
//! of those would put a clock or a decision back inside the kernel (spec
//! §0.1); this stream exists because the ledger already knows when an
//! append happens, so making every embedder poll `Approvals::log` for it
//! would be withholding a fact, not holding a line.

use std::collections::VecDeque;

use kaish_types::approval::LedgerRecord;
use tokio::sync::broadcast;

/// One event [`super::Approvals::watch`] delivers.
#[non_exhaustive]
#[derive(Debug, Clone)]
pub enum WatchEvent {
    /// One append, in `seq` order. Boxed: `LedgerRecord` carries a whole
    /// `LedgerEntry`, which makes it far larger than `Lagged`'s `u64`, and
    /// `WatchEvent` would otherwise pay that size on every value regardless
    /// of which variant it holds.
    Entry(Box<LedgerRecord>),
    /// This consumer fell behind the broadcast buffer and `count` entries
    /// were dropped before it could read them — reported, never silently
    /// skipped (spec §D.2). `count` is the ledger-wide gap, not filtered to
    /// this view's scope (the broadcast is shared, and by the time a
    /// message is dropped its scope is gone with it) — a session-scoped
    /// watcher may see `Lagged` for a gap made entirely of another
    /// session's entries. Either way, catch up with
    /// `Approvals::log(since, ..)` from the last `seq` this stream
    /// delivered; that read applies the same scope filter this stream does.
    Lagged {
        /// How many entries were dropped, ledger-wide.
        count: u64,
    },
}

/// A live tail of every ledger append from some `since` onward.
///
/// Backfills the retained tail first (a snapshot taken atomically with
/// subscribing to the live broadcast, under the same ledger lock, so
/// nothing lands in the gap between the two), then yields new entries as
/// they land. Call [`Self::next`] in a loop; there is no deadline and no
/// polling.
pub struct LedgerStream {
    backlog: VecDeque<LedgerRecord>,
    live: Option<broadcast::Receiver<LedgerRecord>>,
    session: Option<kaish_types::approval::SessionId>,
}

impl LedgerStream {
    pub(super) fn new(
        backlog: VecDeque<LedgerRecord>,
        live: broadcast::Receiver<LedgerRecord>,
        session: Option<kaish_types::approval::SessionId>,
    ) -> Self {
        Self {
            backlog,
            live: Some(live),
            session,
        }
    }

    /// A stream with nothing behind it — [`super::Approvals::watch`]'s
    /// answer for a context with no live ledger (a unit-test harness, a
    /// minimal embedder). [`Self::next`] always returns `None`.
    pub fn empty() -> Self {
        Self {
            backlog: VecDeque::new(),
            live: None,
            session: None,
        }
    }

    /// The next append this view can see, or `None` once the ledger itself
    /// is gone (the process is shutting down — this never happens while
    /// the `Kernel` that built it is alive).
    pub async fn next(&mut self) -> Option<WatchEvent> {
        if let Some(record) = self.backlog.pop_front() {
            return Some(WatchEvent::Entry(Box::new(record)));
        }
        loop {
            let live = self.live.as_mut()?;
            match live.recv().await {
                Ok(record) => {
                    if self.session.as_ref().is_none_or(|s| record.scope.in_session(s)) {
                        return Some(WatchEvent::Entry(Box::new(record)));
                    }
                    // Out of this view's scope (spec §A.7) — not an event
                    // this session watches; keep waiting rather than
                    // surfacing another session's record.
                }
                Err(broadcast::error::RecvError::Lagged(count)) => {
                    return Some(WatchEvent::Lagged { count });
                }
                Err(broadcast::error::RecvError::Closed) => {
                    self.live = None;
                    return None;
                }
            }
        }
    }
}
