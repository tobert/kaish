//! The clock the ledger reads (`docs/approval-ledger.md` §A.5).
//!
//! **The kernel holds no opinion about which clock is true.** It holds two
//! properties, and this file pins both from the embedder's side of the seam:
//! one clock per ledger, installed through
//! `KernelConfig::with_approval_clock`; and a monotone non-decreasing view of
//! whatever that clock is, so an expired grant stays expired and entry stamps
//! never regress.
//!
//! The latch's own unit coverage lives in `ledger/core.rs`, where the test can
//! reach `LedgerInner` directly. What is here is the *seam*: that an installed
//! clock actually reaches the ledger a kernel mints, drives its bounds, and
//! stamps its record.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;
use std::time::{Duration, SystemTime};

use kaish_kernel::ledger::Clock;
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{
    ApprovalRequest, Capture, GrantTerms, PlanBinding, PlanDigest, Principal, PrincipalKind,
    RequestId, RequestOrigin, RequestState, Resource, RiskClass,
};

/// A clock a test drives by hand: a fixed base plus a signed offset, so a
/// test states exactly which readings the ledger sees and in what order. The
/// offset is signed because a reading that goes *backwards* is what the latch
/// exists for.
struct TestClock {
    offset_secs: AtomicI64,
}

impl TestClock {
    const BASE: SystemTime = SystemTime::UNIX_EPOCH;

    fn new() -> Arc<Self> {
        Arc::new(Self {
            offset_secs: AtomicI64::new(1_000_000),
        })
    }

    fn set(&self, offset_secs: i64) {
        self.offset_secs.store(offset_secs, Ordering::Relaxed);
    }
}

impl Clock for TestClock {
    fn now(&self) -> SystemTime {
        let offset = self.offset_secs.load(Ordering::Relaxed);
        if offset >= 0 {
            Self::BASE + Duration::from_secs(offset as u64)
        } else {
            Self::BASE - Duration::from_secs(offset.unsigned_abs())
        }
    }
}

fn agent() -> Principal {
    Principal::new("agent-1", PrincipalKind::Agent)
}

async fn post(kernel: &Kernel, authority: &kaish_kernel::ledger::ApproverHandle) -> RequestId {
    let scope = authority.ledger_scope();
    let draft = ApprovalRequest::builder("fs.remove")
        .risk(RiskClass::Irreversible)
        .resource(Resource::plain("path", "/x"))
        .build()
        .unwrap();
    kernel
        .requester()
        .post_request(
            draft,
            RequestOrigin::new(
                scope.clone(),
                PlanBinding::new(PlanDigest::new("test"), "/", scope),
                agent(),
                Capture::DirectExecution,
            ),
        )
        .await
        .unwrap()
        .id
}

/// The seam: a clock installed on `KernelConfig` is the one the minted
/// ledger reads, for stamps **and** for bounds. Nothing else has to be true
/// for a test to drive expiry deterministically.
#[tokio::test]
async fn an_installed_clock_stamps_the_record_and_answers_the_bounds() {
    let clock = TestClock::new();
    let (kernel, authority) = Kernel::build(
        KernelConfig::isolated().with_approval_clock(clock.clone() as Arc<dyn Clock>),
    )
    .expect("kernel");

    let id = post(&kernel, &authority).await;

    // The stamp is the installed clock's reading, not the system clock's.
    let stamped = kernel.approvals().log(0)[0].at;
    assert_eq!(
        stamped,
        SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_000),
        "the entry must be stamped with the installed clock's reading"
    );

    let chain = kernel.approvals().get(&id).expect("chain");
    let not_after = clock.now() + Duration::from_secs(300);
    authority
        .grant(&id, chain.request.revision, GrantTerms::once_for_view(&chain.request, not_after))
        .await
        .unwrap();
    assert_eq!(kernel.approvals().state(&id), Some(RequestState::Granted));

    // And the bound is answered against the same clock, with no sleeping.
    clock.set(1_000_299);
    assert_eq!(kernel.approvals().state(&id), Some(RequestState::Granted));
    clock.set(1_000_301);
    assert_eq!(
        kernel.approvals().state(&id),
        Some(RequestState::Expired),
        "the installed clock drives expiry — no wait, no real interval"
    );
}

/// The latch, through the public seam: a reading the ledger has already
/// taken cannot be walked back for a chain nobody had observed yet.
///
/// Once an expiry has materialized the record holds it — the chain is
/// terminal and no reading moves it — so the window that needs the latch is
/// the one where the clock passed a bound while nothing looked at *that*
/// chain. A second, unrelated request is what takes the high reading here.
#[tokio::test]
async fn a_reading_below_the_latch_cannot_un_expire_an_unobserved_grant() {
    let clock = TestClock::new();
    let (kernel, authority) = Kernel::build(
        KernelConfig::isolated().with_approval_clock(clock.clone() as Arc<dyn Clock>),
    )
    .expect("kernel");

    let id = post(&kernel, &authority).await;
    let chain = kernel.approvals().get(&id).expect("chain");
    let not_after = clock.now() + Duration::from_secs(300);
    authority
        .grant(&id, chain.request.revision, GrantTerms::once_for_view(&chain.request, not_after))
        .await
        .unwrap();

    // Past `not_after`, and an unrelated post takes that reading — but
    // nothing has read this chain, so no `Expired` entry exists for it yet.
    clock.set(1_000_301);
    post(&kernel, &authority).await;

    // Now step back below `not_after` before anyone looks.
    clock.set(1_000_100);
    assert_eq!(
        kernel.approvals().state(&id),
        Some(RequestState::Expired),
        "the ledger's view is monotone: a grant it had already passed cannot un-expire"
    );
    assert!(
        !kernel
            .approvals()
            .pending()
            .iter()
            .any(|view| view.id == id),
        "and it must not reappear in the pending set"
    );
}

/// The other half: stamps never regress, so `seq` order and `at` order can
/// never disagree in the record an auditor reads.
#[tokio::test]
async fn entry_stamps_never_regress_through_the_kernel_seam() {
    let clock = TestClock::new();
    let (kernel, authority) = Kernel::build(
        KernelConfig::isolated().with_approval_clock(clock.clone() as Arc<dyn Clock>),
    )
    .expect("kernel");

    post(&kernel, &authority).await;
    clock.set(1_000_100);
    post(&kernel, &authority).await;
    clock.set(0);
    post(&kernel, &authority).await;

    let stamps: Vec<SystemTime> = kernel
        .approvals()
        .log(0)
        .into_iter()
        .map(|record| record.at)
        .collect();
    assert!(
        stamps.windows(2).all(|pair| pair[1] >= pair[0]),
        "entry stamps must be monotone non-decreasing, got {stamps:?}"
    );
    assert_eq!(
        stamps[2],
        SystemTime::UNIX_EPOCH + Duration::from_secs(1_000_100),
        "a reading below the latch is clamped up to it, not recorded as-is"
    );
}

/// One clock per ledger, enforced loudly: `with_approver_handle` adopts a
/// ledger that already has a clock, so installing a second one is a
/// construction error rather than a silently ignored setting.
#[tokio::test]
async fn installing_a_clock_alongside_an_adopted_ledger_fails_loud() {
    let (_kernel, authority) = Kernel::build(KernelConfig::isolated()).expect("kernel");

    let err = Kernel::build(
        KernelConfig::isolated()
            .with_approver_handle(authority)
            .with_approval_clock(Arc::new(TestClock {
                offset_secs: AtomicI64::new(0),
            }) as Arc<dyn Clock>),
    )
    .err()
    .expect("two clocks for one ledger must not build");
    let message = format!("{err:#}");
    assert!(
        message.contains("with_approval_clock"),
        "the refusal must name the setting that conflicts: {message}"
    );
}
