//! `yes`, `no`, `TRUE`, and `False` are ordinary words, not booleans.
//!
//! The lexer used to reject them outright as "ambiguous boolean-like", which
//! made `yes | head -3` unable to run the POSIX utility, `cat no` unable to
//! read a file named `no`, and `grep TRUE data.csv` unable to search for a
//! common CSV value. Quoting was the only way through.
//!
//! The rejection was protecting nothing, and that is what these tests pin:
//! only lowercase `true` and `false` were ever boolean *literals*, so `yes`
//! and `TRUE` were always going to be strings. The lexer simply refused to
//! produce them.
//!
//! It was not even consistent with its own premise — `1`, `0`, `on`, `off`,
//! `y`, and `n` are the classic ambiguous booleans and all four were always
//! accepted.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

/// The distinction the rejection claimed to protect, which holds without it:
/// a lowercase literal is a `bool`, and every lookalike is a `string`.
#[tokio::test]
async fn only_lowercase_literals_are_booleans() {
    let k = kernel();
    for (source, expected) in [
        ("x=true; typeof $x", "bool"),
        ("x=false; typeof $x", "bool"),
        ("x=yes; typeof $x", "string"),
        ("x=no; typeof $x", "string"),
        ("x=TRUE; typeof $x", "string"),
        ("x=False; typeof $x", "string"),
        ("x=Yes; typeof $x", "string"),
        // Always accepted, and always strings — the inconsistency that
        // undercut the rule's own premise.
        ("x=on; typeof $x", "string"),
        ("x=off; typeof $x", "string"),
    ] {
        let r = k.execute(source).await.expect("kernel execute");
        assert_eq!(r.code, 0, "{source} failed: {r:?}");
        assert_eq!(r.text_out().trim(), expected, "for: {source}");
    }
}

/// Booleans still compare as booleans.
#[tokio::test]
async fn boolean_literals_still_compare() {
    let k = kernel();
    let r = k
        .execute("if [[ true == true ]]; then echo compared; fi")
        .await
        .expect("kernel execute");
    assert_eq!(r.text_out().trim(), "compared");
}

/// The shapes the rejection broke. Each is an ordinary command that a shell
/// user or an agent writes without thinking about it.
#[tokio::test]
async fn the_shapes_the_rejection_broke_now_run() {
    let k = kernel();
    for source in [
        "echo yes",
        "echo no",
        "echo Yes",
        "echo TRUE",
        "echo yes | head -1",
        // A commit message, a filename, a CSV value: all just words.
        "echo -- no",
        "echo TRUE data.csv",
    ] {
        let r = k.execute(source).await.expect("kernel execute");
        assert_eq!(r.code, 0, "{source} failed: {r:?}");
    }
}

/// `yes` reaches a command position as a name, which is what made the POSIX
/// utility unreachable. Planning is enough to show it — the plan names the
/// command without running anything.
#[tokio::test]
async fn yes_is_reachable_as_a_command_name() {
    let plans = kaish_kernel::plan_program("yes | head -3").expect("parses");
    let names: Vec<&str> = plans[0]
        .plan
        .commands
        .iter()
        .map(|c| c.name.as_str())
        .collect();
    assert_eq!(names, vec!["yes", "head"]);
}
