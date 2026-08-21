//! `$(cmd)` binds a typed value only when the tool says its data IS its value.
//!
//! `.data` was doing three jobs at once: it feeds `--json`, it is the
//! pipeline's structured sideband (`seq 1 3 | jq .` sees `[1,2,3]` where
//! `grep … | jq .` sees text), and it is what `$(cmd)` binds. Any builtin that
//! wanted the first two got the third whether it suited it or not — so
//! `y=$(cut -f2 f)` bound `["benign"]` where `y=$(awk '{print $2}' f)`, doing
//! the identical job, bound text.
//!
//! Amy's rule: a builtin without `--json` should read close to POSIX/GNU/BSD,
//! because that is what transfers. Users who want types ask with `--json`.
//! So the split is not "does it set `.data`" but "is its data its VALUE":
//!
//!   typed  — `fromjson`, `jq`, `keys`, `values`, `split`, `plan`,
//!            `kaish-last`, `typeof`: kaish-native, no POSIX counterpart,
//!            and the structured thing IS the answer.
//!   text   — `cut`, `seq`, `find`, `glob`: a POSIX-familiar tool whose
//!            `.data` is a structured VIEW of the text it printed.
//!
//! The declaration is on the tool, not inferred from which constructor it
//! called — `jq` and `cut` both use `success_with_data` and belong on
//! opposite sides.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
// The fixtures write real files, and `KernelConfig::repl` is a localfs preset.
#![cfg(feature = "localfs")]

use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

async fn out(script: &str) -> String {
    let k = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = k
        .execute(script)
        .await
        .unwrap_or_else(|e| panic!("`{script}` must run: {e}"));
    r.text_out().trim_end().to_string()
}

/// A POSIX-familiar builtin binds TEXT in a scalar capture, like its siblings
/// that never set `.data` at all.
#[rstest]
#[case("printf 'a\\tb\\n' > /tmp/kt.tsv; y=$(cut -f2 /tmp/kt.tsv); echo $y", "b")]
#[case("y=$(seq 1 3); echo $y", "1\n2\n3")]
// Path producers too: `ls` was already text, and `find`/`glob` only escaped
// that by separately setting `.data` — luck, not design.
#[case("mkdir -p /tmp/kts && touch /tmp/kts/a.txt; y=$(find /tmp/kts -name '*.txt'); echo $y", "/tmp/kts/a.txt")]
// NB: `glob` drops the leading `/` from an absolute match — pre-existing,
// identical on main, and unrelated to typing. Pinned as it is so this test
// does not quietly encode a bug as intent.
#[case("mkdir -p /tmp/ktg && touch /tmp/ktg/b.txt; y=$(glob '/tmp/ktg/*.txt'); echo $y", "tmp/ktg/b.txt")]
#[tokio::test]
async fn a_posix_familiar_builtin_binds_text(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(out(script).await, expected, "`{script}`");
}

/// The comparison that makes the old behavior indefensible: two builtins
/// doing the same job must not bind different shapes.
#[tokio::test]
async fn cut_and_awk_agree_now() {
    let setup = "printf 'a\\tb\\n' > /tmp/kt2.tsv; ";
    let cut = out(&format!("{setup}y=$(cut -f2 /tmp/kt2.tsv); echo $y")).await;
    let awk = out(&format!("{setup}y=$(awk '{{print $2}}' /tmp/kt2.tsv); echo $y")).await;
    assert_eq!(cut, awk, "cut and awk do the same job here");
    assert_eq!(cut, "b");
}

/// A builtin whose data IS its value stays typed — this is the collections
/// feature and it must not move.
#[rstest]
#[case("xs=$(fromjson '[10,20]'); echo ${xs[0]}", "10")]
#[case("r=$(fromjson '{\"a\":1}'); echo ${r[a]}", "1")]
#[case("r=$(jq -c . <<< '{\"a\":7}'); echo ${r[a]}", "7")]
#[case("ks=$(keys $(fromjson '{\"a\":1}')); echo ${ks[0]}", "a")]
#[case("vs=$(values $(fromjson '{\"a\":9}')); echo ${vs[0]}", "9")]
#[tokio::test]
async fn a_value_builtin_stays_typed(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(out(script).await, expected, "`{script}`");
}

/// The pipeline's structured sideband is a DIFFERENT consumer of `.data` and
/// must be untouched. This is what the first attempt broke: it moved the
/// payload off `.data` to fix `$()`, and `seq 1 3 | jq .` started failing the
/// way `grep | jq` does.
#[rstest]
#[case("seq 1 3 | jq -c .", "[1,2,3]")]
#[case("printf 'a\\tb\\n' > /tmp/kt3.tsv; cut -f2 /tmp/kt3.tsv | jq -c .", "[\"b\"]")]
#[tokio::test]
async fn the_pipeline_sideband_is_untouched(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(out(script).await, expected, "`{script}`");
}

/// `--json` is how a user asks for types, so it must be unchanged for exactly
/// the builtins whose capture just stopped being typed.
#[rstest]
#[case("seq 1 3 --json", "[\"1\",\"2\",\"3\"]")]
#[case("printf 'a\\tb\\n' > /tmp/kt4.tsv; cut -f2 /tmp/kt4.tsv --json", "[\"b\"]")]
#[tokio::test]
async fn json_output_is_unchanged(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(out(script).await, expected, "`{script}`");
}

/// Iteration is unchanged, because a `for` head newline-splits a text
/// substitution — which is how `grep` and `ls` have always iterated.
#[rstest]
#[case("for i in $(seq 1 3); do echo \"i=$i\"; done", "i=1\ni=2\ni=3")]
#[case("printf 'a\\tb\\nc\\td\\n' > /tmp/kt5.tsv; for v in $(cut -f2 /tmp/kt5.tsv); do echo \"v=$v\"; done", "v=b\nv=d")]
#[tokio::test]
async fn iteration_is_unchanged(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(out(script).await, expected, "`{script}`");
}

/// A typed value survives a COMPOUND statement. `accumulate_result` copied
/// `.data` without its marker, so every `if`/`for`/`while`/`case` and every
/// `&&`/`||` chain dropped the value on the way out. The marker travels with
/// the data now.
#[rstest]
#[case("x=$(if true; then fromjson '[1,2]'; fi); echo ${x[0]}", "1")]
#[case("x=$(for i in 1; do fromjson '[9,8]'; done); echo ${x[0]}", "9")]
#[case("x=$(while true; do fromjson '[5]'; break; done); echo ${x[0]}", "5")]
#[case("x=$(case a in a) fromjson '[3]';; esac); echo ${x[0]}", "3")]
#[case("x=$(true && fromjson '[1,2]'); echo ${x[0]}", "1")]
#[case("x=$(false || fromjson '[1,2]'); echo ${x[0]}", "1")]
#[tokio::test]
async fn a_typed_value_survives_a_compound_statement(
    #[case] script: &str,
    #[case] expected: &str,
) {
    assert_eq!(out(script).await, expected, "`{script}`");
}

/// And the INVERSE, which is the same bug wearing the other hat: the chain
/// kept the LEFT side's marker while taking the RIGHT side's data, so
/// `$(fromjson … && cut …)` bound `cut`'s structured view typed — the original
/// bug re-entering through a side door.
#[tokio::test]
async fn a_chain_does_not_type_the_right_side_from_the_left() {
    let script = "printf 'a\tb\n' > /tmp/ktinv.tsv;                   x=$(fromjson '[1,2]' && cut -f2 /tmp/ktinv.tsv); echo $x";
    assert_eq!(out(script).await, "[1,2]b", "both texts, neither typed");
}

/// `break`/`continue` carry no value, and must not erase the body's.
#[tokio::test]
async fn a_valueless_signal_does_not_erase_the_bodys_value() {
    assert_eq!(
        out("x=$(for i in 1 2; do fromjson '[7]'; continue; done); echo ${x[0]}").await,
        "7"
    );
    // The must-not-break direction: a body that produced only text stays text.
    assert_eq!(out("x=$(while true; do echo hi; break; done); echo $x").await, "hi");
}

/// A wrapper that re-dispatches returns the INNER result, so stamping it from
/// the wrapper's own schema erased what the inner tool declared.
///
/// `into_arc()` is required: `timeout` re-dispatches through `ctx.dispatcher`,
/// which is `None` on a kernel that was never wrapped, and the inner command
/// then runs not at all rather than running untyped.
#[tokio::test]
async fn a_redispatching_wrapper_keeps_the_inner_declaration() {
    let k = Kernel::new(KernelConfig::repl()).expect("kernel").into_arc();
    let r = k
        .execute("x=$(timeout 5 fromjson '[1,2]'); echo ${x[0]}")
        .await
        .expect("execute");
    assert_eq!(r.text_out().trim_end(), "1", "err={:?}", r.err);
}

/// The rest of the typed set, so a missing declaration shows up here rather
/// than in someone's script. `fromjsonl` was missing one.
#[rstest]
#[case("x=$(fromjsonl '{\"a\":1}'); echo ${x[0]}", "{\"a\":1}")]
#[case("x=$(split 'a b c'); echo ${x[1]}", "b")]
#[case("x=$(typeof 5); echo $x", "number")]
#[tokio::test]
async fn the_rest_of_the_typed_set(#[case] script: &str, #[case] expected: &str) {
    assert_eq!(out(script).await, expected, "`{script}`");
}
