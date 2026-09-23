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
#[case("basename")]
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
#[case("awk")]
// `sed -i` with no file operand — editing a stream in place is meaningless.
#[case("sed -i 's/a/b/'")]
// A flag value the builtin cannot use.
#[case("find . -type x")]
#[case("find . -maxdepth nope")]
#[case("checksum --algorithm bogus a.txt")]
#[case("seq --increment 0 1 5")]
#[case("cut a.txt")]
#[case("sleep abc")]
#[case("kaish-output-limit set bogus")]
// A name that cannot be bound cannot be read/unset/exported either.
#[case("read a-b")]
#[case("unset a-b")]
#[case("export a-b")]
// A malformed inner program is argv, same as a rejected kaish-validate input.
#[case("kaish-ast '{'")]
// An unknown subcommand.
#[case("kaish-trash bogus")]
#[case("kaish-ignore bogus")]
#[case("kaish-output-limit bogus")]
// `set -o`/`set +o` on a name kaish doesn't implement.
#[case("set -o bogus")]
// Text the caller typed inline is argv, whether it's JSON or JSONL.
#[case("fromjson '{not json}'")]
#[case("fromjsonl 'not json'")]
// `--argjson NAME VALUE` with a VALUE that isn't JSON is an unusable flag
// value, and a *computed* filter that fails to compile is the same class of
// mistake a computed `grep` pattern is — a literal filter never reaches
// here; the validator refuses it first.
#[case("jq -n --argjson x 'not-json' '.'")]
#[case("x='.['; echo null | jq \"$x\"")]
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
// Data read from stdin or a file is the world, even though a file *path* is
// itself an argument — the same text malformed as a positional above is 2.
#[case::fromjson_stdin_content("echo '{not json}' | fromjson", 1)]
#[case::fromjsonl_stdin_content("printf 'not json' | fromjsonl", 1)]
// base64 -d never takes its data as a positional literal — a file or stdin,
// always the world.
#[case::base64_decode_content("echo 'not-base64!!!' | base64 -d", 1)]
#[tokio::test]
async fn a_result_or_an_operational_failure_keeps_its_code(
    #[case] script: &str,
    #[case] expected: i64,
) {
    assert_eq!(code_of(script).await, expected, "`{script}`");
}

/// `jq`'s `--path` file operand names *where* to read, not what's in it —
/// invalid JSON in the file is the same result (1) as invalid JSON on
/// stdin, not a usage error, even though the path is an argument.
#[tokio::test]
async fn jq_file_operand_invalid_json_keeps_its_code() {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("bad.json"), "{not json}").expect("fixture");
    let kernel = common::kernel_at(dir.path());
    let result = kernel
        .execute("jq . bad.json")
        .await
        .expect("script was not rejected");
    assert_eq!(result.code, 1, "err: {}", result.err);
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
