//! GH #189 item 1: a `key=value` WordAssign token after `--` must bind as a
//! literal positional, not a named shell assignment, even for the
//! word-assign-accepting builtins (`export`, `alias`, `unalias`).
//!
//! `--` is supposed to mark everything after it as literal data (matching
//! how the flag arms of the shared binder already treat `past_double_dash`).
//! The `Arg::WordAssign` arm used to ignore `past_double_dash` entirely, so
//! `export -- A=1` still bound `A=1` as a named assignment instead of a
//! stringified `"A=1"` positional. `unalias` is the sharpest demonstration:
//! its `execute()` reconstructs argv via `ToolArgs::to_argv()` and re-parses
//! with clap, which renders a lingering named entry as `--A=1` — a flag
//! `UnaliasArgs` never declares — so before the fix `unalias -- foo=bar`
//! crashed with a clap "unexpected argument" error (exit 2) instead of
//! treating `foo=bar` as a literal (if odd) alias name to remove.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

#[tokio::test]
async fn export_after_double_dash_still_sets_the_variable() {
    let k = kernel();
    let r = k
        .execute("export -- A=1; export -p")
        .await
        .expect("kernel execute");
    assert_eq!(r.code, 0, "got: {r:?}");
    assert!(
        r.text_out().contains("declare -x A=\"1\""),
        "export -- A=1 should still export A=1, got: {}",
        r.text_out()
    );
}

#[tokio::test]
async fn alias_after_double_dash_still_defines_the_alias() {
    let k = kernel();
    let r = k
        .execute("alias -- ll=foo; alias ll")
        .await
        .expect("kernel execute");
    assert_eq!(r.code, 0, "got: {r:?}");
    assert!(
        r.text_out().contains("alias ll='foo'"),
        "alias -- ll=foo should still define the alias, got: {}",
        r.text_out()
    );
}

/// The sharpest regression: before the fix, `unalias -- foo=bar` crashed
/// with clap's "unexpected argument '--foo' found" (exit 2) because the
/// lingering named entry rendered as a flag token `to_argv()` had no
/// business emitting past `--`. It must now succeed (as a no-op removal of
/// a nonexistent alias literally named "foo=bar" — `unalias` on a
/// nonexistent name is not an error, matching its pre-existing behavior).
#[tokio::test]
async fn unalias_after_double_dash_does_not_crash_on_clap_reparse() {
    let k = kernel();
    let r = k
        .execute("unalias -- foo=bar")
        .await
        .expect("kernel execute");
    assert_eq!(
        r.code, 0,
        "unalias -- foo=bar must not hit the clap unexpected-argument crash: {r:?}"
    );
}
