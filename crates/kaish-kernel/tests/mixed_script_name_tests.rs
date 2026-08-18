//! A name spelled in two scripts is a warning at every door, never an error.
//!
//! `PАTH=/bin` — with CYRILLIC CAPITAL LETTER A (U+0410) where Latin `A`
//! belongs — binds a second variable and leaves `$PATH` alone. The source
//! shows one name and the kernel binds another, which no exit code reports.
//! W007 reports it, and the name still binds: a lint that refused would break
//! every legal Cyrillic, Greek, or Japanese name typed next to an ASCII one.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("kernel")
}

/// `(exit code, stdout, stderr)` — the warning rides stderr, so a helper that
/// dropped it would not see the thing under test.
async fn run(source: &str) -> (i64, String, String) {
    let k = kernel();
    let r = k.execute(source).await.expect("kernel execute");
    (r.code, r.text_out().trim().to_string(), r.err.clone())
}

/// CYRILLIC CAPITAL LETTER A, the character the whole rule exists for.
const CYRILLIC_A: char = '\u{0410}';

fn cyrillic_path() -> String {
    format!("P{CYRILLIC_A}TH")
}

#[tokio::test]
async fn an_assignment_with_a_mixed_script_name_warns_on_stderr() {
    let name = cyrillic_path();
    let (code, _, err) = run(&format!("{name}=/bin")).await;
    assert_eq!(code, 0, "a mixed-script name is a warning, not a failure");
    assert!(err.contains("W007"), "stderr should carry the code: {err:?}");
    assert!(err.contains("U+0410"), "stderr should name the codepoint: {err:?}");
    assert!(err.contains("Cyrillic"), "stderr should name the script: {err:?}");
}

#[tokio::test]
async fn an_export_with_a_mixed_script_name_warns_on_stderr() {
    let name = cyrillic_path();
    let (code, _, err) = run(&format!("export {name}=/bin")).await;
    assert_eq!(code, 0, "a mixed-script name is a warning, not a failure");
    assert!(err.contains("W007"), "stderr should carry the code: {err:?}");
    assert!(err.contains("U+0410"), "stderr should name the codepoint: {err:?}");
}

/// The whole point of a warning: the statement still runs and the name still
/// binds. This test fails the moment someone promotes W007 to an error.
#[tokio::test]
async fn a_mixed_script_name_still_binds() {
    let name = cyrillic_path();
    let (code, out, _) = run(&format!("{name}=hi; echo ${{{name}}}")).await;
    assert_eq!(code, 0, "execution must not be blocked");
    assert_eq!(out, "hi");
}

/// The message names the plain spelling the author almost certainly meant.
#[tokio::test]
async fn the_warning_names_the_spelling_the_name_reads_as() {
    let name = cyrillic_path();
    let (_, _, err) = run(&format!("{name}=/bin")).await;
    assert!(err.contains("`PATH`"), "stderr should name the ASCII reading: {err:?}");
}

/// Every name kaish already accepts stays quiet. A rule that fired on `café`
/// or `名前` would be worse than the hazard it reports.
#[tokio::test]
async fn single_script_names_are_quiet_at_every_door() {
    for source in [
        "café=ok",
        "名前=ok",
        "переменная=ok",
        "x1=ok",
        "_x=ok",
        "x😁=ok",
        "変数x=ok",
        "export café=ok",
        "unset café",
        "xs=[a]; push xs b",
        "read v",
    ] {
        let (_, _, err) = run(source).await;
        assert!(!err.contains("W007"), "{source} should not warn: {err:?}");
    }
}

/// A name that only exists at runtime has no spelling to judge before it runs,
/// so no door warns about one. This pins the outcome, not the mechanism: the
/// walker's `<dynamic>` placeholder is itself single-script Latin, so the
/// explicit guard in `mixed_script_issue` is there to say the placeholder is
/// not a name, not to carry this case.
#[tokio::test]
async fn a_name_that_is_not_known_until_runtime_is_not_judged() {
    let name = cyrillic_path();
    for source in [
        format!("n={name}; unset $n"),
        format!("n={name}; read $n"),
        format!("n={name}; xs=[a]; push $n b"),
        format!("n={name}; seq 1 2 | scatter --as $n | echo hi | gather"),
    ] {
        let (_, _, err) = run(&source).await;
        assert!(
            !err.contains("W007"),
            "{source} has no static name to judge: {err:?}"
        );
    }
}

/// `unset`, `push`, `read`, and `scatter --as` take a name as a runtime word,
/// so each needs the check in its own `Tool::validate`.
#[tokio::test]
async fn the_runtime_name_doors_warn_too() {
    let name = cyrillic_path();
    for source in [
        format!("unset {name}"),
        format!("read {name}"),
        format!("xs=[a]; push {name} b"),
        format!("seq 1 2 | scatter --as {name} | echo hi | gather"),
    ] {
        let (_, _, err) = run(&source).await;
        assert!(err.contains("W007"), "{source} should warn: {err:?}");
    }
}
