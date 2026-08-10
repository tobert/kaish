//! §B.5's teardown obligations, and the halt §I.5 rules for.
//!
//! **Nothing times a request out** (`docs/approval-ledger.md` §A.10), so
//! every path that can strand one has to close it. Under the old request TTL
//! this was invisible: an orphan expired on its own and returned its slot, so
//! a missing teardown path cost sixty seconds of capacity and nothing else.
//! Without expiry it costs a live slot for the life of the process — and
//! forever, in an embedder where several kernels share one ledger.
//!
//! One test per row of §B.5's table, each asserting the live count returns to
//! zero:
//!
//! | Teardown | Test |
//! |---|---|
//! | A job is discarded | [`a_discarded_job_closes_its_held_request`] |
//! | A job is cancelled or killed | [`cancelling_every_job_closes_their_held_requests`] |
//! | A session shuts down | [`a_session_shutdown_closes_only_its_own_scopes_requests`] |
//! | A kernel shuts down | [`kernel_shutdown_closes_a_gated_jobs_outstanding_request`] |
//!
//! Then §I.5's ruling: a tool-level deferral halts the top-level statement
//! loop exactly the way a statement-level gate already does.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem, and these tests gate real
// `rm`/`write` operations against real files.
#![cfg(feature = "localfs")]

use std::path::Path;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{RequestState, SessionId};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("ledger-teardown-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// A kernel rooted at `dir` with the `fs.*` enforce policy available but off
/// until a script turns it on, so nothing gates by accident.
fn session(dir: &Path) -> Kernel {
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

/// Turn the gate on and raise one backgrounded gated `rm`, returning the job
/// number and the id of the request it is held on.
async fn gated_background_job(kernel: &Kernel, dir: &Path, name: &str) -> kaish_types::approval::RequestId {
    std::fs::write(dir.join(name), "keep me").expect("write");
    run(kernel, "set -o approvals").await;
    run(kernel, &format!("rm {name} &")).await;
    let waited = run(kernel, "wait").await;
    assert_eq!(waited.code, 2, "the backgrounded rm must gate: {waited:?}");
    waited.approval_request().expect("a held request").id
}

// ============================================================================
// §B.5's obligations table
// ============================================================================

/// Row 1: a job is discarded. `kill --discard %N` drops the job entry, and
/// with it the only reference to the request it was held on.
#[tokio::test]
async fn a_discarded_job_closes_its_held_request() {
    let dir = tempdir();
    let kernel = session(dir.path());
    let id = gated_background_job(&kernel, dir.path(), "precious.txt").await;
    assert_eq!(kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.len(), 1);

    let discarded = run(&kernel, "kill --discard %1").await;
    assert_eq!(discarded.code, 0, "discard: {}", discarded.err);

    assert_eq!(
        kernel.approvals().state(&id),
        Some(RequestState::Cancelled),
        "a discarded job's held request must be closed, not orphaned"
    );
    assert!(
        kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.is_empty(),
        "the live count must return to zero — nothing else will ever close it"
    );
    assert!(dir.path().join("precious.txt").exists(), "nothing ran");
}

/// Row 2: a job is cancelled or killed. `Kernel::cancel_all_jobs` is the
/// kernel-level lever, and it reaches gated jobs — `kill %N` refuses one
/// without `--discard`, which row 1 covers.
#[tokio::test]
async fn cancelling_every_job_closes_their_held_requests() {
    let dir = tempdir();
    let kernel = session(dir.path());
    let first = gated_background_job(&kernel, dir.path(), "one.txt").await;
    let second = gated_background_job(&kernel, dir.path(), "two.txt").await;
    assert_eq!(kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.len(), 2);

    kernel.cancel_all_jobs().await;

    for id in [&first, &second] {
        assert_eq!(
            kernel.approvals().state(id),
            Some(RequestState::Cancelled),
            "a cancelled job's held request must be closed"
        );
    }
    assert!(kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.is_empty());
}

/// Row 3: a session shuts down, and closes **only its own scope's** requests.
/// Two kernels share one ledger through `with_approver_handle`, which is the
/// shape where a stranded request outlives the process it belonged to.
#[tokio::test]
async fn a_session_shutdown_closes_only_its_own_scopes_requests() {
    let dir = tempdir();
    std::fs::write(dir.path().join("mine.txt"), "keep me").expect("write");
    std::fs::write(dir.path().join("theirs.txt"), "keep me").expect("write");

    let (leaving, authority) = Kernel::build(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_session(SessionId::new("leaving")),
    )
    .expect("kernel");
    let staying = Kernel::new(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_session(SessionId::new("staying"))
            .with_approver_handle(authority),
    )
    .expect("kernel");

    run(&leaving, "set -o approvals").await;
    let mine = run(&leaving, "rm mine.txt").await;
    let mine = mine.approval_request().expect("a gated request").id;
    run(&staying, "set -o approvals").await;
    let theirs = run(&staying, "rm theirs.txt").await;
    let theirs = theirs.approval_request().expect("a gated request").id;
    assert_eq!(staying.approvals().pending(kaish_types::approval::PageRequest::default()).items.len(), 2, "one shared ledger");

    leaving.shutdown().await.expect("shutdown");

    assert_eq!(
        staying.approvals().state(&mine),
        Some(RequestState::Cancelled),
        "the departing session must close what it would otherwise strand"
    );
    assert_eq!(
        staying.approvals().state(&theirs),
        Some(RequestState::Requested),
        "and must not close another session's request"
    );
    assert_eq!(staying.approvals().pending(kaish_types::approval::PageRequest::default()).items.len(), 1);
}

/// Row 4: a kernel shuts down with a gated job outstanding. Both levers fire
/// — `cancel_all_jobs` for the job's held request, and the scope sweep for
/// anything else live.
#[tokio::test]
async fn kernel_shutdown_closes_a_gated_jobs_outstanding_request() {
    let dir = tempdir();
    let kernel = session(dir.path());
    let backgrounded = gated_background_job(&kernel, dir.path(), "job.txt").await;

    // A second request with no job behind it at all, so the sweep has
    // something the job path cannot reach.
    std::fs::write(dir.path().join("foreground.txt"), "keep me").expect("write");
    let foreground = run(&kernel, "rm foreground.txt").await;
    let foreground = foreground.approval_request().expect("a gated request").id;
    assert_eq!(kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.len(), 2);

    kernel.shutdown().await.expect("shutdown");

    assert_eq!(kernel.approvals().state(&backgrounded), Some(RequestState::Cancelled));
    assert_eq!(kernel.approvals().state(&foreground), Some(RequestState::Cancelled));
    assert!(
        kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.is_empty(),
        "shutdown must return every live slot in this kernel's scope"
    );
}

// ============================================================================
// §I.5 — a tool-level deferral halts the loop
// ============================================================================

/// The ruling, in one line of shell. `rm x` gates, so `echo ok` does not run:
/// exit 2 means "this has not happened yet", and the statements after it were
/// written expecting it had.
#[tokio::test]
async fn a_tool_level_gate_halts_the_program_and_carries_the_pending_request() {
    let dir = tempdir();
    let kernel = session(dir.path());
    std::fs::write(dir.path().join("x"), "keep me").expect("write");
    run(&kernel, "set -o approvals").await;

    let result = run(&kernel, "rm x; echo ok").await;

    assert_eq!(result.code, 2, "the gate's exit code survives to the program: {result:?}");
    assert!(
        !result.text_out().contains("ok"),
        "`echo ok` must not run after a statement that has not happened yet: {:?}",
        result.text_out()
    );
    let pending = result
        .approval_request()
        .expect("the result must carry the pending request");
    assert_eq!(pending.operation.as_str(), "fs.remove");
    assert_eq!(kernel.approvals().pending(kaish_types::approval::PageRequest::default()).items.len(), 1);
    assert!(dir.path().join("x").exists(), "and nothing was deleted");
}

/// The sharper version of the case for halting: without it, a *denied*
/// operation's side effects run anyway — `rm x; touch y` creates `y` whether
/// or not `rm x` is ever approved, and nothing un-creates it.
#[tokio::test]
async fn a_halted_statements_side_effects_do_not_run() {
    let dir = tempdir();
    let kernel = session(dir.path());
    std::fs::write(dir.path().join("x"), "keep me").expect("write");
    run(&kernel, "set -o approvals").await;

    let result = run(&kernel, "rm x; write y created").await;

    assert_eq!(result.code, 2);
    assert!(
        !dir.path().join("y").exists(),
        "a statement after an unperformed one must leave no trace"
    );
}

/// The statement-level half of the same rule, which `kernel.rs` already
/// implemented and no test pinned: a gated top-level statement stops the
/// program, and the statement after it does not run.
#[tokio::test]
async fn the_statement_after_a_halted_statement_does_not_run() {
    let dir = tempdir();
    let kernel = Kernel::new(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_approvals(false)
            .with_trash(false)
            .with_statement_classifier(std::sync::Arc::new(
                kaish_kernel::ledger::CommandNameClassifier::new(
                    ["gateme"],
                    "the classifier gates this command",
                    kaish_types::approval::RiskClass::Irreversible,
                ),
            )),
    )
    .expect("kernel");

    let result = run(&kernel, "gateme; echo after").await;

    assert_eq!(result.code, 2, "the gated statement halts the program: {result:?}");
    assert!(
        !result.text_out().contains("after"),
        "nothing after a held statement runs: {:?}",
        result.text_out()
    );
    assert!(result.approval_request().is_some());
}

/// A program with no gate is unaffected: the halt fires on a pending
/// approval, not on any exit 2.
#[tokio::test]
async fn an_ordinary_exit_2_does_not_halt_the_program() {
    let dir = tempdir();
    let kernel = session(dir.path());

    // `false` exits 1 and `;` ignores it; a usage error exits 2 and `;`
    // ignores that too. Only a pending approval halts.
    let result = run(&kernel, "rm --nonsense; echo ok").await;

    assert!(
        result.text_out().contains("ok"),
        "only a pending approval halts the loop, never a plain failure: {:?}",
        result.text_out()
    );
}
