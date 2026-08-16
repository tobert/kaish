//! `yes`, `no`, `TRUE`, and `False` are ordinary words, not booleans.
//!
//! The lexer used to reject them outright as "ambiguous boolean-like", which
//! left `yes` unable to even be named as a command, `cat no` unable to read a
//! file named `no`, and `grep TRUE data.csv` unable to search for a common CSV
//! value. Quoting was the only way through.
//!
//! The rejection was protecting nothing, and that is what these tests pin:
//! only lowercase `true` and `false` were ever boolean *literals*, so `yes`
//! and `TRUE` were always going to be strings. The lexer simply refused to
//! produce them.
//!
//! It was not even consistent with its own premise — `1`, `0`, `on`, `off`,
//! `y`, and `n` are the classic ambiguous booleans and every one of them was
//! always accepted.

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
        ("x=FALSE; typeof $x", "string"),
        ("x=True; typeof $x", "string"),
        ("x=False; typeof $x", "string"),
        ("x=Yes; typeof $x", "string"),
        ("x=YES; typeof $x", "string"),
        ("x=NO; typeof $x", "string"),
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

/// What accepting the lookalikes actually costs, pinned so it stays deliberate.
///
/// `==` is NOT the discriminator here: `values_equal` stringifies mixed scalars,
/// so `[[ true == "true" ]]` is true whether the left side is a `bool` or the
/// string `"true"`. `typeof` is the only surface that separates them, which is
/// why the test above carries the weight.
///
/// What is new is the middle row. `[[ true == TRUE ]]` used to be a loud lexer
/// error and is now a quiet `false`, and `[[ -n TRUE ]]` is a quiet `true`.
/// That is the residual the rejection existed to catch — and it was never
/// caught for `on`, `off`, `1`, or `0`, which always reached the same place.
#[tokio::test]
async fn lookalikes_compare_as_the_strings_they_are() {
    let k = kernel();
    for (source, expected) in [
        ("if [[ true == \"true\" ]]; then echo same; else echo differ; fi", "same"),
        ("if [[ true == TRUE ]]; then echo same; else echo differ; fi", "differ"),
        ("if [[ -n TRUE ]]; then echo nonempty; else echo empty; fi", "nonempty"),
        // The always-accepted lookalikes, reaching the identical place.
        ("if [[ true == on ]]; then echo same; else echo differ; fi", "differ"),
    ] {
        let r = k.execute(source).await.expect("kernel execute");
        assert_eq!(r.text_out().trim(), expected, "for: {source}");
    }
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
