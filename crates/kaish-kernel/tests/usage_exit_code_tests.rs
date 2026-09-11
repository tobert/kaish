//! Usage errors exit 2; a result or an operational failure keeps 1.
//!
//! `docs/LANGUAGE.md`, "Exit Codes" states the rule. A builtin that answers a
//! question spends 1 on the negative answer; where 1 is free it means the
//! command ran and the world said no. Either way, "you invoked this wrong" is
//! 2 — a missing operand, an unknown subcommand, a flag value the builtin
//! cannot use.
//!
//! Kernel-routed so each case goes through lex, parse, validate, and dispatch
//! exactly as a script would.

// Every case drives a builtin against a real-FS root, so this whole binary
// needs `localfs`; without the gate a `--no-default-features` build fails to
// compile on `common::kernel_at`.
#![cfg(feature = "localfs")]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use rstest::rstest;

/// Run `script` in an isolated real-FS root and return its exit code.
async fn code_of(script: &str) -> i64 {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("a.txt"), "one\ntwo\n").expect("fixture");
    let kernel = common::kernel_at(dir.path());
    let result = kernel.execute(script).await;
    match result {
        Ok(r) => r.code,
        // A validator rejection is its own class and is covered by the CLI
        // tests; a case landing here means the script never reached the
        // builtin, so the case is not testing what it claims.
        Err(e) => panic!("script was rejected before dispatch: {e:#}"),
    }
}

#[rstest]
// Missing operand.
#[case("rm")]
#[case("mkdir")]
#[case("touch")]
#[case("stat")]
#[case("readlink")]
#[case("realpath")]
#[case("dirname")]
#[case("tee")]
#[case("printf")]
#[case("unset")]
#[case("sleep")]
#[case("tr")]
#[case("glob")]
#[case("seq")]
#[case("cp a.txt")]
#[case("mv a.txt")]
#[case("ln a.txt")]
// A flag value the builtin cannot use.
#[case("find . -type x")]
#[case("find . -maxdepth nope")]
#[case("checksum --algorithm bogus a.txt")]
#[case("seq --increment 0 1 5")]
#[case("cut a.txt")]
// An unknown subcommand.
#[case("kaish-trash bogus")]
#[case("kaish-ignore bogus")]
#[case("kaish-output-limit bogus")]
#[tokio::test]
async fn a_usage_error_exits_2(#[case] script: &str) {
    assert_eq!(
        code_of(script).await,
        2,
        "`{script}` is a usage error and must not share an exit code with a result",
    );
}

#[rstest]
// The world said no; the invocation was fine.
#[case::unreadable_file("cat no_such_file.txt", 1)]
#[case::unreadable_file_rm("rm no_such_file.txt", 1)]
// A result, not an error: exit 1 is the answer.
#[case::glob_no_matches("glob 'no_such_*.zzz'", 1)]
#[case::false_builtin("false", 1)]
#[case::test_false("test -f no_such_file.txt", 1)]
// `read` at EOF is what ends `cmd | while read x; do … done`, so its 1 is a
// loop terminator and must survive the sweep.
#[case::read_at_eof("read x", 1)]
#[case::success("cat a.txt", 0)]
#[tokio::test]
async fn a_result_or_an_operational_failure_keeps_its_code(
    #[case] script: &str,
    #[case] expected: i64,
) {
    assert_eq!(code_of(script).await, expected, "`{script}`");
}

#[tokio::test]
async fn while_read_still_terminates_on_eof() {
    // The regression this sweep could plausibly cause: if `read`'s EOF became
    // a usage error, the loop would still end, but the shell would be calling
    // a normal end-of-input "you invoked this wrong".
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = common::kernel_at(dir.path());
    let (out, code) = common::run(
        &kernel,
        "printf 'a\\nb\\n' | while read l; do echo \"got $l\"; done",
    )
    .await;
    assert_eq!(out, "got a\ngot b");
    assert_eq!(code, 0, "the loop ends cleanly, not with a usage error");
}

/// `which` is registered only with the `subprocess` feature, so in a default
/// test build a bare `which` is "command not found" (127) and would measure
/// the registry, not the exit-code rule.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn which_without_a_command_name_exits_2() {
    assert_eq!(code_of("which").await, 2);
}
