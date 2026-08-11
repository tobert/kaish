//! Ledger PR R5 (`docs/approval-ledger.md` §A.8, §D.2): the redaction seam
//! and pagination.
//!
//! Four groups, matching §H's acceptance list for this lane:
//! - the redaction seam: no bare `String` reaches a sink, and every
//!   non-key value stays `Plain` — the kernel redacts only its own
//!   confirm key;
//! - `Approvals::pending` pagination: a listing over more than one page
//!   returns a cursor that neither repeats nor skips;
//! - `Approvals::log` pagination: the same guarantee over the retained log;
//! - `Approvals::watch`: appends delivered in order from `since`, across a
//!   page boundary, with no polling.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use std::path::Path;
use std::sync::Arc;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::ledger::{
    ClassificationError, StatementAssessment, StatementClassificationInput,
    StatementClassifier, StatementPosture,
};
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{AssessorId, LedgerEntry, PageRequest, Plan, PlannedValue, RiskClass};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("redaction-pagination-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

fn kernel_at(dir: &Path) -> Kernel {
    Kernel::new(
        KernelConfig::repl()
            .with_cwd(dir.to_path_buf())
            .with_approvals(false)
            .with_trash(false),
    )
    .expect("kernel")
}

async fn run(kernel: &Kernel, script: &str) -> ExecResult {
    kernel.execute(script).await.expect("kernel execute")
}

/// Every `Observed` entry's plan, in commit order — the statement tap's
/// record of what ran, regardless of gating.
fn observed_plans(kernel: &Kernel) -> Vec<Plan> {
    kernel
        .approvals()
        .log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT)
        .items
        .into_iter()
        .filter_map(|record| record.known().cloned())
        .filter_map(|entry| match entry {
            LedgerEntry::Observed { operation, plan, .. } if operation.as_str() == "cmd.execute" => plan,
            _ => None,
        })
        .collect()
}

// ============================================================================
// The redaction seam (spec §A.8)
// ============================================================================

/// A value inside a plan is typed `PlannedValue`, never a bare `String` —
/// the compiler enforces this at every call site that reads
/// `PlannedCommand::args`/`PlannedRedirect::target`, so this test documents
/// the property directly rather than proving a negative.
#[tokio::test]
async fn a_value_reaches_no_sink_as_a_bare_string() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    run(&kernel, "echo one two").await;

    let plans = observed_plans(&kernel);
    let plan = plans.last().expect("a plan");
    fn must_be_planned_value(_: &PlannedValue) {}
    for command in &plan.commands {
        for arg in &command.args {
            must_be_planned_value(arg);
        }
        for redirect in &command.redirects {
            must_be_planned_value(&redirect.target);
        }
    }
    assert_eq!(plan.commands[0].args.len(), 2, "sanity: both words planned");
}

/// Every non-key value is `Plain` (spec §A.8) — the kernel ships no
/// secret detector, so nothing pretends otherwise; embedder-side redaction
/// happens over the plans and records the embedder holds.
#[tokio::test]
async fn every_non_key_value_is_plain() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());
    run(&kernel, "echo not-a-secret").await;

    let plans = observed_plans(&kernel);
    let plan = plans.last().expect("a plan");
    assert_eq!(
        plan.commands[0].args,
        vec![PlannedValue::Plain("not-a-secret".to_string())]
    );
    assert!(!plan.commands[0].args[0].is_redacted());
}

// ============================================================================
// `Approvals::pending` pagination (spec §D.2)
// ============================================================================

struct GateEverything;

impl StatementClassifier for GateEverything {
    fn classify(
        &self,
        _input: &StatementClassificationInput<'_>,
    ) -> Result<StatementAssessment, ClassificationError> {
        Ok(StatementAssessment::new(
            StatementPosture::gate("the test gates everything", RiskClass::Reversible),
            AssessorId::new("gate-everything-test-fixture"),
        ))
    }
}

/// A listing over more entries than one page returns a cursor that neither
/// repeats nor skips (spec §D.2, ledger PR R5's acceptance list).
#[tokio::test]
async fn pending_pagination_neither_repeats_nor_skips() {
    let dir = tempdir();
    let kernel = Kernel::new(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_statement_classifier(Arc::new(GateEverything)),
    )
    .expect("kernel");

    // Five gated, undecided requests — more than the page size below.
    for i in 0..5 {
        let result = run(&kernel, &format!("echo statement{i}")).await;
        assert_eq!(result.code, 2, "each statement must gate: {result:?}");
    }

    let mut seen = Vec::new();
    let mut cursor = None;
    let mut pages = 0;
    loop {
        let mut page_request = PageRequest::first(2);
        if let Some(cursor) = cursor {
            page_request = page_request.with_cursor(cursor);
        }
        let page = kernel.approvals().pending(page_request);
        pages += 1;
        seen.extend(page.items.into_iter().map(|v| v.id));
        match page.next {
            Some(next) => cursor = Some(next),
            None => break,
        }
        assert!(pages <= 10, "pagination did not converge — a cursor is looping");
    }

    assert!(pages >= 3, "the fixture must actually cross a page boundary, got {pages} page(s)");
    assert_eq!(seen.len(), 5, "every pending request must appear exactly once: {seen:?}");
    let mut unique = seen.clone();
    unique.sort();
    unique.dedup();
    assert_eq!(unique.len(), 5, "a repeated id means the cursor re-served a page: {seen:?}");

    let mut all_ids = kernel.approvals().ids();
    all_ids.sort_by_key(kaish_types::approval::RequestId::seq);
    let mut seen_sorted = seen;
    seen_sorted.sort_by_key(kaish_types::approval::RequestId::seq);
    assert_eq!(seen_sorted, all_ids, "a skipped id means the cursor advanced past an entry");
}

// ============================================================================
// `Approvals::log` pagination (spec §D.2)
// ============================================================================

/// The same neither-repeats-nor-skips guarantee, over the retained log.
#[tokio::test]
async fn log_pagination_neither_repeats_nor_skips() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());

    for i in 0..7 {
        run(&kernel, &format!("echo entry{i}")).await;
    }
    let total = kernel.approvals().log(0, kaish_types::approval::DEFAULT_PAGE_LIMIT).items.len();
    assert!(total >= 7, "expected at least 7 retained entries, got {total}");

    let mut seen = Vec::new();
    let mut since = 0;
    let mut pages = 0;
    loop {
        let page = kernel.approvals().log(since, 3);
        pages += 1;
        seen.extend(page.items.iter().map(|r| r.sequence));
        match page.next {
            Some(next) => since = next.seq(),
            None => break,
        }
        assert!(pages <= 10, "pagination did not converge — a cursor is looping");
    }

    assert!(pages >= 3, "the fixture must actually cross a page boundary, got {pages} page(s)");
    assert_eq!(seen.len(), total, "every entry must appear exactly once");
    let mut unique = seen.clone();
    unique.sort_unstable();
    unique.dedup();
    assert_eq!(unique.len(), seen.len(), "a repeated seq means the cursor re-served a page: {seen:?}");
    let mut sorted = seen.clone();
    sorted.sort_unstable();
    assert_eq!(seen, sorted, "entries must arrive seq-ordered");
}

// ============================================================================
// `Approvals::watch` (spec §D.2)
// ============================================================================

/// `watch(since)` delivers every append in order from `since`, across at
/// least one page boundary a `log` reader would have to page through, with
/// no polling: this test calls `.next().await` in a loop and nothing else.
#[tokio::test]
async fn watch_delivers_appends_in_order_across_a_page_boundary() {
    let dir = tempdir();
    let kernel = kernel_at(dir.path());

    // Backlog: entries already retained before `watch` is called.
    for i in 0..3 {
        run(&kernel, &format!("echo backlog{i}")).await;
    }
    let mut stream = kernel.approvals().watch(0);

    // Live: entries appended after the stream is already subscribed. The
    // broadcast channel buffers them whether or not anything is reading yet
    // (spec §D.2 — no deadline, no filter), so running these synchronously
    // before draining the stream still exercises the live path rather than
    // the backlog: `watch` was already subscribed when these committed, and
    // the backlog snapshot taken at subscribe time did not include them.
    // `log`'s own default page size is far larger than 3, so a reader
    // paging with a small size (as the pagination test above does) would
    // cross a page boundary partway through this set; `watch` must not
    // care, because it has no page size of its own.
    for i in 0..4 {
        run(&kernel, &format!("echo live{i}")).await;
    }

    // Make the "page boundary" concrete: a `log` reader paging with the
    // same small size the pagination test above uses would need three
    // pages to see these 7 entries. `watch` delivers all of them as one
    // continuous stream regardless — it has no page size of its own to
    // cross.
    let paged = kernel.approvals().log(0, 3);
    assert!(
        paged.next.is_some(),
        "the fixture must actually need more than one small page: {paged:?}"
    );

    let mut delivered = Vec::new();
    // No polling: block on `.next()` until every expected entry (backlog +
    // live) has arrived. A bounded loop count is the test's own timeout,
    // not a poll interval `watch` imposes.
    while delivered.len() < 7 {
        match tokio::time::timeout(std::time::Duration::from_secs(5), stream.next()).await {
            Ok(Some(kaish_kernel::ledger::WatchEvent::Entry(record))) => delivered.push(record.sequence),
            Ok(Some(kaish_kernel::ledger::WatchEvent::Lagged { count })) => {
                panic!("unexpected lag in a single-writer test: {count}")
            }
            Ok(None) => panic!("the stream closed before every entry arrived"),
            Ok(Some(other)) => panic!("unexpected watch event: {other:?}"),
            Err(_) => panic!("timed out waiting for watch() to deliver an entry"),
        }
    }

    assert_eq!(delivered.len(), 7, "backlog (3) + live (4) = 7 entries");
    let mut sorted = delivered.clone();
    sorted.sort_unstable();
    assert_eq!(delivered, sorted, "watch must deliver strictly in seq order");
    let mut unique = delivered.clone();
    unique.dedup();
    assert_eq!(unique.len(), delivered.len(), "no entry may repeat: {delivered:?}");
}
