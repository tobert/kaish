//! `--flag=value` after `--` is one literal operand, the way `key=value`
//! already is.
//!
//! `--` means "everything after this is data". The post-`--` grammar had no
//! `--flag=value` production, so `echo -- --json=true` lexed as three tokens —
//! `LongFlag("json")`, `Eq`, `true` — and the glue guard rejected them as
//! adjacent unpasted words:
//!
//! ```text
//! 1:15 [parse]: adjacent words with no space between them are not joined
//! ```
//!
//! That error is right about pre-`--` argv, where kaish deliberately does no
//! token pasting, and wrong here: past `--` there is no flag to glue anything
//! to. `key=value` was already handled (`export -- A=1`, GH #189); this is the
//! same rule for the flag spelling, short and long.
//!
//! The fix also makes the `past_double_dash` guards on the binders' `Named`
//! arms reachable from real source for the first time — before this, no
//! program could produce a post-`--` `Arg::Named` at all.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

async fn echoed(script: &str) -> String {
    let k = kernel();
    let r = k.execute(script).await.expect("kernel execute");
    assert_eq!(r.code, 0, "`{script}` exited {}: {:?}", r.code, r.err);
    r.text_out().trim_end().to_string()
}

#[tokio::test]
async fn long_flag_value_is_one_operand() {
    assert_eq!(echoed("echo -- --flag=value").await, "--flag=value");
}

/// `-x=value` is deliberately NOT part of this: it is refused on both sides
/// of `--` today (`echo -n=1` is the same parse error), so accepting it only
/// after `--` would trade one asymmetry for another. Pinned so the decision is
/// visible if someone changes the short-flag grammar.
#[tokio::test]
async fn short_flag_value_is_still_refused_on_both_sides() {
    let k = kernel();
    for script in ["echo -n=1", "echo -- -n=1"] {
        let rejected = match k.execute(script).await {
            Err(_) => true,
            Ok(r) => r.code != 0,
        };
        assert!(rejected, "`{script}` should still be refused");
    }
}

/// The bare form leaked the same way, and predates this change: past `--`,
/// `--json` landed in `positional`, and the scan that exists for `raw_argv`
/// tools (which keep the `--` marker there) read it as the kernel's flag.
/// `echo -- --json hi` answered in JSON.
#[tokio::test]
async fn post_dash_bare_json_is_an_operand_too() {
    assert_eq!(echoed("echo -- --json hi").await, "--json hi");
}

/// ...while a `raw_argv` tool still honors `--json` from its positionals,
/// which is what that scan is for (GH #198). `test` is raw_argv.
#[tokio::test]
async fn raw_argv_tools_still_read_json_from_positionals() {
    let k = kernel();
    let r = k.execute("test --json -n abc").await.expect("kernel execute");
    assert!(
        r.text_out().trim_start().starts_with('{'),
        "test --json should still render its error as JSON, got {:?}",
        r.text_out()
    );
}

/// Several in a row, each its own word — the glue guard must not see them as
/// one pasted run.
#[tokio::test]
async fn several_flag_values_stay_separate_operands() {
    assert_eq!(echoed("echo -- --a=1 --b=2").await, "--a=1 --b=2");
}

/// The value still expands, as it does for `key=value` and as bash does.
#[tokio::test]
async fn the_value_expands() {
    let k = kernel();
    let r = k
        .execute("V=hello; echo -- --greeting=$V")
        .await
        .expect("kernel execute");
    assert_eq!(r.code, 0, "{r:?}");
    assert_eq!(r.text_out().trim_end(), "--greeting=hello");
}

/// The sharp case: past `--`, the kernel's own flag is an operand, so it must
/// be printed rather than switching the output format.
#[tokio::test]
async fn post_dash_json_is_an_operand_not_the_kernel_flag() {
    let out = echoed("echo -- --json=true").await;
    assert_eq!(out, "--json=true");
    assert!(
        serde_json::from_str::<serde_json::Value>(&out).is_err(),
        "output must not be JSON-rendered: {out:?}"
    );
}

/// Before `--`, nothing changes: `--json=true` is still the kernel's flag.
#[tokio::test]
async fn pre_dash_json_still_switches_the_format() {
    let out = echoed("echo --json=true hi").await;
    assert_eq!(out, "\"hi\\n\"");
}

/// Real pasting is still refused. `--a=1--b=2` is two argv fragments with no
/// space, which is what the glue guard exists to catch; the fix must not
/// swallow that.
#[tokio::test]
async fn glued_words_are_still_rejected() {
    let k = kernel();
    let r = k.execute("echo -- --a=$V--b").await;
    let rejected = match r {
        Err(_) => true,
        Ok(res) => res.code != 0,
    };
    assert!(rejected, "a genuinely glued post-`--` word must still fail");
}
