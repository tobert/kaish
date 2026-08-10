//! Ledger PR R5 (`docs/approval-ledger.md` §A.8, §D.2): the redaction seam
//! and pagination.
//!
//! Four groups, matching §H's acceptance list for this lane:
//! - the redaction seam: no bare `String` reaches a sink, an installed
//!   `Redactor` covers every sink that reads a `Plan` (including one this
//!   file adds), and with none installed every value stays `Plain`;
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
use std::sync::{Arc, Mutex};

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::ledger::{RedactionMark, Redactor, StatementClassifier, StatementPosture};
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{LedgerEntry, PageRequest, Plan, PlannedValue, RiskClass, ValueSite};

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

/// A `Redactor` that marks one exact literal secret, and nothing else —
/// standing in for an embedder's own credential detector. Records every
/// value it was asked to judge, so a test can assert the seam offered it
/// every argument and redirect target, not just the one it matched.
struct MarksOneSecret {
    secret: &'static str,
    judged: Mutex<Vec<String>>,
}

impl MarksOneSecret {
    fn new(secret: &'static str) -> Self {
        Self {
            secret,
            judged: Mutex::new(Vec::new()),
        }
    }
}

impl Redactor for MarksOneSecret {
    fn redact(&self, value: &str, _site: ValueSite) -> Option<RedactionMark> {
        self.judged.lock().expect("test mutex").push(value.to_string());
        (value == self.secret).then(|| RedactionMark::new("test-secret"))
    }
}

/// A `StatementClassifier` that never gates — it exists only to capture
/// every `Plan` it is asked to classify, so a test can assert the
/// classifier's own input already carries the redaction (spec §A.8's first
/// sink) rather than the raw plan.
struct CapturesPlans {
    seen: Mutex<Vec<Plan>>,
}

impl CapturesPlans {
    fn new() -> Self {
        Self { seen: Mutex::new(Vec::new()) }
    }
}

impl StatementClassifier for CapturesPlans {
    fn classify(&self, plan: &Plan) -> StatementPosture {
        self.seen.lock().expect("test mutex").push(plan.clone());
        StatementPosture::Observe
    }
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

/// With no `Redactor` installed, every value is `Plain` — the honest
/// default (spec §A.8), not a silent promise of protection this build does
/// not keep.
#[tokio::test]
async fn with_no_redactor_every_value_is_plain() {
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

/// An installed `Redactor` covers every sink that reads a `Plan` — the
/// classifier's input, the ledger's `Observed` entry, the `/v/approvals`
/// projection, and a fourth reader this test adds — because all four read
/// the *same* already-redacted `Plan` the one normalization point built
/// (`ast::plan::plan_statement`). None of them re-derives its own
/// redaction, which is the property this test is actually checking.
#[tokio::test]
async fn an_installed_redactor_covers_every_sink_including_one_this_test_adds() {
    let dir = tempdir();
    let secret = "sk-live-deadbeefcafe";
    let redactor = Arc::new(MarksOneSecret::new(secret));
    let classifier = Arc::new(CapturesPlans::new());
    let kernel = Kernel::new(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_redactor(redactor.clone())
            .with_statement_classifier(classifier.clone()),
    )
    .expect("kernel");

    run(&kernel, &format!("echo {secret}")).await;

    // Sink 1: the classifier's own input. Scoped so the `MutexGuard` — a
    // plain `std::sync::Mutex`, not async-aware — is dropped well before
    // this function's next `.await`, never straddling one.
    let classified_plan = {
        let classified = classifier.seen.lock().expect("test mutex");
        classified.last().expect("the classifier ran").clone()
    };
    assert!(
        classified_plan.commands[0].args[0].is_redacted(),
        "the classifier must see the redacted plan, not the raw one: {classified_plan:?}"
    );
    assert!(!classified_plan.rendered.contains(secret));

    // Sink 2: the ledger's `Observed` entry.
    let plans = observed_plans(&kernel);
    let observed_plan = plans.last().expect("an Observed entry with a plan");
    assert!(observed_plan.commands[0].args[0].is_redacted());
    assert!(!observed_plan.rendered.contains(secret));

    // Sink 3: the `/v/approvals` projection — read as text, the surface a
    // less-trusted reader actually gets.
    let log_result = run(&kernel, "cat /v/approvals/log").await;
    let log_text = log_result.text_out();
    assert!(
        !log_text.contains(secret),
        "the secret leaked into /v/approvals/log: {log_text}"
    );
    assert!(
        log_text.contains("test-secret"),
        "the redaction kind must still be visible: {log_text}"
    );

    // Sink 4: a reader this test adds, reusing the same `Plan` sink 2 already
    // read — inheriting the redaction with no logic of its own, which is
    // exactly the "sink added later" case §A.8 designs for.
    fn a_new_sink_reads_the_plan(plan: &Plan) -> bool {
        plan.commands.iter().any(|c| c.args.iter().any(PlannedValue::is_redacted))
    }
    assert!(a_new_sink_reads_the_plan(observed_plan));

    // The redactor was consulted on the real argument text, not on some
    // pre-redacted stand-in — proving the marking, not just the shape.
    let judged = redactor.judged.lock().expect("test mutex");
    assert!(judged.iter().any(|v| v == secret), "the redactor never saw the secret to judge: {judged:?}");
}

// ============================================================================
// `Approvals::pending` pagination (spec §D.2)
// ============================================================================

struct GateEverything;

impl StatementClassifier for GateEverything {
    fn classify(&self, _plan: &Plan) -> StatementPosture {
        StatementPosture::gate("the test gates everything", RiskClass::Reversible)
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
