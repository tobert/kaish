//! `yes`, `no`, `TRUE`, and `False` are ordinary words, not booleans.
//!
//! Only lowercase `true` and `false` are boolean literals. Every other
//! spelling is a string — case variants, `yes`/`no`, and the classic
//! lookalikes `1`, `0`, `on`, `off`, `y`, `n` — and that is what these tests
//! pin.
//!
//! `typeof` is the only surface that separates the two. `==` stringifies
//! mixed scalars, so it cannot tell them apart.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

/// A lowercase literal is a `bool`; every lookalike is a `string`.
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
        // The classic lookalikes, and strings just the same.
        ("x=on; typeof $x", "string"),
        ("x=off; typeof $x", "string"),
    ] {
        let r = k.execute(source).await.expect("kernel execute");
        assert_eq!(r.code, 0, "{source} failed: {r:?}");
        assert_eq!(r.text_out().trim(), expected, "for: {source}");
    }
}

/// A lookalike compares and tests as the string it is, pinned so it stays
/// deliberate.
///
/// `==` is NOT the discriminator here: `values_equal` stringifies mixed scalars,
/// so `[[ true == "true" ]]` holds whether the left side is a `bool` or the
/// string `"true"`. `typeof` is the surface that separates them, which is why
/// the test above carries the weight.
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

/// Ordinary commands a shell user or an agent writes without thinking.
#[tokio::test]
async fn lookalike_words_run_as_ordinary_commands() {
    let k = kernel();
    for source in [
        "echo yes",
        "echo no",
        "echo Yes",
        "echo TRUE",
        "echo yes | head -1",
        "echo -- no",
        "echo TRUE data.csv",
    ] {
        let r = k.execute(source).await.expect("kernel execute");
        assert_eq!(r.code, 0, "{source} failed: {r:?}");
    }
}

/// `yes` reaches a command position as a name, so the POSIX utility is
/// addressable. Planning shows it without running anything.
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
