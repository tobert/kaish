//! Teardown obligations (`docs/approval-ledger.md` §B.5).
//!
//! **A request whose requester is gone cannot be cancelled by its owner,
//! because its owner no longer exists.** Nothing else closes it either, so it
//! holds a live slot for the life of the process and
//! [`LedgerConfig::live_capacity`](super::LedgerConfig::live_capacity) is a
//! backstop against accumulation rather than cleanup. Under a request TTL
//! this was invisible — an orphan expired on its own and returned its slot,
//! so a missing teardown path cost sixty seconds of capacity and nothing
//! else. With no expiry (spec §A.10) it costs a slot permanently.
//!
//! Every teardown path in §B.5's table closes what it orphans:
//!
//! | Teardown | Obligation |
//! |---|---|
//! | A job is discarded | Cancel its held request, `Withdrawn` |
//! | A job is cancelled or killed | Cancel its held request, `Withdrawn` |
//! | A session shuts down | Cancel every request its principal still owns |
//! | A kernel shuts down | Cancel every live request in its scope (§A.7) |
//!
//! The last two are one call — [`cancel_scope`] — because a kaish session
//! *is* a kernel: `KernelConfig::with_session` names it, and its scope is
//! what distinguishes its requests from another session's sharing the same
//! ledger.

use kaish_types::approval::{ApprovalScope, CancelReason, Principal, RequestId};

use super::handles::{Approvals, Requester};
use super::LedgerError;
use crate::scheduler::{JobId, JobManager};

/// Close a job's held approval request, if it has one (spec §B.5, rows 1 and
/// 2). Called before the job entry is dropped — afterwards its cached result
/// is gone and with it the only reference to the request.
///
/// Returns whether a request was actually closed.
pub(crate) async fn cancel_job_request(
    requester: &Requester,
    by: &Principal,
    jobs: &JobManager,
    job: JobId,
    reason: CancelReason,
) -> bool {
    let Some(view) = jobs.get_approval(job).await else {
        return false;
    };
    cancel_one(requester, by, &view.id, reason).await
}

/// Close every live request in `scope` (spec §B.5, rows 3 and 4).
///
/// Returns how many requests were closed. A request already decided or
/// terminal, or one with an attempt in flight, is left alone: a granted
/// chain closes on its own at the grant's `not_after`, a terminal one has
/// nothing left to close, and a running attempt has not been stranded yet.
pub(crate) async fn cancel_scope(
    requester: &Requester,
    approvals: &Approvals,
    by: &Principal,
    scope: &ApprovalScope,
    reason: CancelReason,
) -> usize {
    let mut closed = 0;
    for view in approvals.pending() {
        if &view.scope != scope {
            continue;
        }
        if cancel_one(requester, by, &view.id, reason.clone()).await {
            closed += 1;
        }
    }
    closed
}

/// One cancellation, with teardown's error posture: a request that closed
/// or was decided underneath us is not a failure — a decided chain closes at
/// its grant's `not_after` and a terminal one is already closed — and
/// anything else is logged, because a teardown path has nobody to return an
/// error to.
async fn cancel_one(
    requester: &Requester,
    by: &Principal,
    id: &RequestId,
    reason: CancelReason,
) -> bool {
    match requester.cancel(id, by.clone(), reason).await {
        Ok(_) => true,
        Err(
            LedgerError::NotFound(_)
            | LedgerError::Terminal { .. }
            | LedgerError::AlreadyDecided(_),
        ) => false,
        Err(err) => {
            tracing::warn!(
                request_id = %id,
                error = %err,
                "approval ledger: teardown could not close a held request — it keeps its live slot"
            );
            false
        }
    }
}
