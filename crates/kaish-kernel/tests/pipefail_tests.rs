//! A failed pipeline stage must be detectable.
//!
//! Without `pipefail` and without `PIPESTATUS`, a producer that died was
//! invisible to anything reading the exit code:
//!
//! ```text
//! cat /nonexistent | wc -l ; echo $?     # 0
//! ```
//!
//! The stderr shows, but the status says success and there was no second way
//! to ask. That matters here more than in an interactive shell: kaish's exit
//! codes are read by embedders and models, and kaijutsu gates a tool call on
//! one — a guard whose producer failed was reading "continue".
//!
//! Both halves are POSIX-adjacent bash behavior and both are pinned against
//! `bash -c` rather than against a reading of the manual. `PIPESTATUS` is the
//! primitive: it reports every stage. `pipefail` is the mode people type, and
//! its rule is the RIGHTMOST non-zero code, not the first — `bash -c 'set -o
//! pipefail; (exit 3) | (exit 4) | true'` is 4.
//!
//! The scripts quote `"${PIPESTATUS[0]},${PIPESTATUS[1]}"` because kaish does
//! no token pasting — unquoted, the `,` between two expansions is a parse
//! error, not a separator. That bit me writing these.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::ExecResult;
use rstest::rstest;

async fn run(script: &str) -> ExecResult {
    let k = Kernel::new(KernelConfig::isolated()).expect("kernel");
    k.execute(script).await.expect("kernel execute")
}

/// `PIPESTATUS` reports every stage, in order. `test -f` is a usage error
/// (exit 2) and `false` is 1, so a mixed pipeline has distinguishable codes —
/// a table of all-1s could not tell order from coincidence.
#[rstest]
#[case("false | true; echo \"${PIPESTATUS[0]},${PIPESTATUS[1]}\"", "1,0")]
#[case("true | false; echo \"${PIPESTATUS[0]},${PIPESTATUS[1]}\"", "0,1")]
#[case("test -f | false; echo \"${PIPESTATUS[0]},${PIPESTATUS[1]}\"", "2,1")]
#[case("false | test -f; echo \"${PIPESTATUS[0]},${PIPESTATUS[1]}\"", "1,2")]
#[case("true | true | true; echo \"${PIPESTATUS[0]},${PIPESTATUS[1]},${PIPESTATUS[2]}\"", "0,0,0")]
#[tokio::test]
async fn pipestatus_reports_every_stage(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(run(script).await.text_out().trim_end(), expected, "`{script}`");
}

/// A single command is a pipeline of one, and bash sets `PIPESTATUS` for it.
#[rstest]
#[case("false; echo \"${PIPESTATUS[0]},${#PIPESTATUS}\"", "1,1")]
#[case("true; echo \"${PIPESTATUS[0]},${#PIPESTATUS}\"", "0,1")]
#[case("false | true; echo ${#PIPESTATUS}", "2")]
#[tokio::test]
async fn pipestatus_length_matches_the_stage_count(
    #[case] script: &str,
    #[case] expected: &str,
) {
    assert_eq!(run(script).await.text_out().trim_end(), expected, "`{script}`");
}

/// Without `pipefail` the pipeline's status is the LAST stage's, unchanged.
/// This is the behavior that must not move.
#[rstest]
#[case("false | true", 0)]
#[case("true | false", 1)]
#[case("cat /nonexistent | wc -l", 0)]
#[tokio::test]
async fn the_default_is_still_the_last_stages_status(
    #[case] script: &str,
    #[case] expected: i64,
) {
    assert_eq!(run(script).await.code, expected, "`{script}`");
}

/// `pipefail` answers with the RIGHTMOST non-zero code. The first-non-zero
/// reading is the easy mistake and it disagrees with bash on
/// `(exit 3) | (exit 4)`.
#[rstest]
#[case("set -o pipefail; false | true", 1)]
#[case("set -o pipefail; true | true", 0)]
#[case("set -o pipefail; false | false", 1)]
#[case("set -o pipefail; test -f | false", 1)]
#[case("set -o pipefail; false | test -f", 2)]
#[case("set -o pipefail; test -f | false | true", 1)]
#[case("set -o pipefail; test -f | true | true", 2)]
#[case("set -o pipefail; cat /nonexistent | wc -l", 1)]
#[tokio::test]
async fn pipefail_takes_the_rightmost_nonzero(#[case] script: &str, #[case] expected: i64) {
    assert_eq!(run(script).await.code, expected, "`{script}`");
}

/// The mode is a mode: it turns off, and it does not leak into the next
/// kernel.
#[tokio::test]
async fn pipefail_turns_off_again() {
    assert_eq!(run("set -o pipefail; set +o pipefail; false | true").await.code, 0);
    assert_eq!(run("false | true").await.code, 0, "a fresh kernel is not in pipefail");
}

/// `set -o` reports it, like every other option — the question a script author
/// asks when a pipeline's status surprises them.
#[tokio::test]
async fn set_dash_o_reports_pipefail() {
    let off_result = run("set -o").await;
    let off = off_result.text_out();
    assert!(off.contains("pipefail"), "`set -o` must list pipefail, got: {off}");
    let on_result = run("set -o pipefail; set -o").await;
    let on = on_result.text_out();
    let row = on
        .lines()
        .find(|l| l.contains("pipefail"))
        .unwrap_or_else(|| panic!("no pipefail row in: {on}"));
    assert!(row.contains("on"), "pipefail should read on, got: {row}");
}

/// `set -o pipefail` used to be a usage error naming an unimplemented option.
/// It is implemented, so that error must be gone — and an option kaish really
/// does not have stays loud, or the check would be worthless.
#[tokio::test]
async fn pipefail_is_no_longer_an_unknown_option() {
    let r = run("set -o pipefail; echo ALIVE").await;
    assert_eq!(r.code, 0, "err={:?}", r.err);
    assert!(!r.err.contains("not implemented"), "err={:?}", r.err);
    assert_eq!(r.text_out().trim_end(), "ALIVE");

    // No trailing statement: a script's status is its LAST statement's, so
    // `set -o nosuchoption; echo AFTER` exits 0 and would prove nothing.
    let r = run("set -o nosuchoption").await;
    assert_ne!(r.code, 0, "an unknown option must still be refused");
    assert!(r.err.contains("nosuchoption"), "the error must name it: {:?}", r.err);
}

/// `PIPESTATUS` is a list, so the collection accessors work on it. This is
/// what makes it useful without a `${arr[@]}` spelling kaish does not have.
///
/// Asked ONCE, because reading it runs a command and that command is itself a
/// pipeline of one — the read replaces what it was about to report. bash is
/// the same: `false|true; echo ${#PIPESTATUS[@]}; echo "${PIPESTATUS[*]}"`
/// prints `2` then `0`, not `2` then `1 0`. Capture first, then ask.
#[tokio::test]
async fn pipestatus_is_a_real_list() {
    let r = run("false | true; for c in $(values $PIPESTATUS); do echo \"c=$c\"; done").await;
    assert_eq!(r.text_out().trim_end(), "c=1\nc=0", "err={:?}", r.err);

    let r = run("false | true; echo ${#PIPESTATUS}").await;
    assert_eq!(r.text_out().trim_end(), "2", "err={:?}", r.err);
}

/// A read of `PIPESTATUS` is a command, so it replaces the codes it just
/// reported — matching bash. Pinned because the alternative reading (that it
/// persists until the next PIPE) is the one a script author assumes, and the
/// difference is silent.
#[tokio::test]
async fn reading_pipestatus_replaces_it() {
    let r = run("false | true; echo ${#PIPESTATUS}; echo \"again=$PIPESTATUS\"").await;
    assert_eq!(r.text_out().trim_end(), "2\nagain=[0]", "err={:?}", r.err);
}
