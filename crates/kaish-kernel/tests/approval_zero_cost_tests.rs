//! The subscription feature's hard requirement, stated in numbers: **with
//! nothing subscribed, an `fs.*` operation costs no request and no per-path
//! entry, however many paths it names.**
//!
//! The statement tap (`docs/approval-ledger.md` §C.6) posts one chainless
//! entry for the statement itself, and that is the whole cost: 1 entry for
//! 10,000 paths, not 10,000. The tap is O(top-level statements) by
//! construction, which is exactly what makes it tenable always-on while
//! per-path `fs.*` observe stays opt-in — so this test pins the tap's entry
//! count at 1 rather than pretending the log is empty.
//!
//! One test, alone in its own integration binary on purpose.
//! [`ApprovalRequest::constructed_count`] is a process-wide counter, so a
//! neighboring test that gated anything would inflate the delta and turn a
//! real regression into a flaky one. A file with a single test is a file
//! with a single process.
//!
//! This claim decides whether the feature is worth shipping at all. A
//! recursive delete over a large tree is a first-class kaish workload, and a
//! ledger that slows it down by default is a ledger operators turn off — at
//! which point the audit trail nobody paid for is also an audit trail nobody
//! has.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::approval::{ApprovalRequest, LedgerEntry};

/// The entries inside a ledger's records. These tests assert on entry shape;
/// the [`LedgerRecord`] envelope has its own coverage in `kaish-types` (spec
/// §A.5), and an entry this build does not recognize cannot occur here.
#[allow(dead_code)]
fn entries(records: Vec<kaish_types::approval::LedgerRecord>) -> Vec<LedgerEntry> {
    records
        .into_iter()
        .map(|record| {
            record
                .known()
                .cloned()
                .expect("this build wrote every record it reads back")
        })
        .collect()
}


/// How many paths the delete names.
const PATHS: usize = 10_000;

#[tokio::test]
async fn an_unsubscribed_ten_thousand_path_delete_posts_nothing_and_builds_no_request() {
    let dir = tempfile::Builder::new()
        .prefix("approval-zero-cost-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR");
    let tree = dir.path().join("tree");
    std::fs::create_dir(&tree).unwrap();
    for i in 0..PATHS {
        std::fs::write(tree.join(format!("f{i}")), b"x").unwrap();
    }

    let config = KernelConfig::repl()
        .with_cwd(tree.clone())
        .with_approvals(false)
        .with_trash(false);
    let (kernel, _authority) = Kernel::build(config).expect("kernel");

    // Nothing subscribed, no enforce policy, no trash: the gate site's
    // early-out fires before a request exists.
    assert!(
        !kernel.approvals().any_subscriptions(),
        "precondition: the registry must be empty"
    );

    // Read the counter as late as possible — kernel construction and the
    // glob walk below happen either side of it, and neither may build a
    // request.
    let before = ApprovalRequest::constructed_count();
    // The glob expands to one positional per file, so the gate site really
    // does classify 10,000 paths. `rm -r tree` would hand it exactly one.
    let result = kernel.execute("rm -f *").await.expect("kernel execute");
    let after = ApprovalRequest::constructed_count();

    assert_eq!(result.code, 0, "the delete must run: {}", result.text_out());
    assert_eq!(
        after - before,
        0,
        "an unsubscribed {PATHS}-path delete built {} approval requests — 0 are allowed",
        after - before
    );
    let log = entries(kernel.approvals().log(0));
    assert_eq!(
        log.len(),
        1,
        "an unsubscribed {PATHS}-path delete must post exactly the one statement-tap entry: {log:?}"
    );
    let LedgerEntry::Observed {
        operation,
        resources,
        ..
    } = &log[0]
    else {
        panic!("the only entry must be the statement tap, got {:?}", log[0]);
    };
    assert_eq!(operation.as_str(), "cmd.execute");
    assert_eq!(
        resources.len(),
        1,
        "one `cmd` resource for the one planned command, not one per path: {resources:?}"
    );
    assert_eq!(
        std::fs::read_dir(&tree).unwrap().count(),
        0,
        "every file must actually be gone"
    );
}
