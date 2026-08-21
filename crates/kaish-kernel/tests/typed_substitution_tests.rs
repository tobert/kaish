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
