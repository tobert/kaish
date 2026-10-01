//! Brace groups: `{ cmd; cmd; }` runs a list of statements as one statement.
//!
//! A group runs in the current shell, as in bash: variables, `cd`, and
//! functions it sets stay set, and `exit`, `return`, `break`, and `continue`
//! inside it act on the enclosing script, function, or loop. It is a
//! pipeline stage like `if`/`for`/`while`/`case`, so the last-stage rule for
//! session state applies to it with no special case.
//!
//! Expectations are bash's, from running each row against bash, except where
//! a test says otherwise.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::ast::sexpr::format_program;
use kaish_kernel::parser::parse;
use kaish_kernel::{Kernel, KernelConfig};
use rstest::rstest;

/// An in-memory kernel: `/` and `/tmp` are memory filesystems.
fn kernel() -> Kernel {
    Kernel::new(KernelConfig::isolated()).expect("failed to create kernel")
}

async fn run(source: &str) -> (String, i64) {
    let result = kernel().execute(source).await.expect("execution failed");
    (result.text_out().into_owned(), result.code)
}

// ---- parse shape ------------------------------------------------------------

#[rstest]
#[case::group("{ echo a; echo b; }", r#"(group (cmd echo (pos (string "a"))) (cmd echo (pos (string "b"))))"#)]
#[case::closing_brace_after_a_word("{ echo a }", r#"(group (cmd echo (pos (string "a"))))"#)]
#[case::multi_line("{\n  echo a\n  echo b\n}", r#"(group (cmd echo (pos (string "a"))) (cmd echo (pos (string "b"))))"#)]
#[case::or_fallback(
    "false || { echo no; exit 1; }",
    r#"(or-chain (cmd false) (group (cmd echo (pos (string "no"))) (exit (int 1))))"#
)]
#[case::and_branch("true && { echo yes; }", r#"(and-chain (cmd true) (group (cmd echo (pos (string "yes")))))"#)]
#[case::first_stage("{ echo b; echo a; } | sort", r#"(pipeline (group (cmd echo (pos (string "b"))) (cmd echo (pos (string "a")))) (cmd sort))"#)]
#[case::last_stage("echo x | { read a; }", r#"(pipeline (cmd echo (pos (string "x"))) (group (cmd read (pos (string "a")))))"#)]
#[case::negated("! { false; }", "(not (group (cmd false)))")]
#[case::background("{ echo a; } &", r#"(background (group (cmd echo (pos (string "a")))))"#)]
#[case::nested("{ { echo a; }; }", r#"(group (group (cmd echo (pos (string "a")))))"#)]
#[case::in_then("if true; then { echo a; }; fi", r#"(if (cmd true) (then (group (cmd echo (pos (string "a"))))) (else))"#)]
fn a_brace_group_parses(#[case] source: &str, #[case] expected: &str) {
    let program = parse(source).unwrap_or_else(|e| panic!("`{source}` must parse: {e:?}"));
    assert_eq!(format_program(&program), expected, "`{source}`");
}

/// A record literal still opens with `{` in value position.
#[test]
fn a_record_literal_is_not_a_group() {
    let program = parse(r#"x={"a": 1}"#).expect("parses");
    let sexpr = format_program(&program);
    assert!(sexpr.starts_with("(assign x"), "{sexpr}");
    assert!(!sexpr.contains("group"), "{sexpr}");
}

// ---- running a group ----------------------------------------------------------

#[rstest]
#[case::in_order("{ echo a; echo b; }", "a\nb\n", 0)]
#[case::or_fallback_runs_and_exits("false || { echo no; exit 1; }; echo after", "no\n", 1)]
#[case::or_fallback_skipped("true || { echo no; exit 1; }; echo after", "after\n", 0)]
#[case::and_branch_runs("true && { echo a; echo b; }", "a\nb\n", 0)]
#[case::feeds_a_pipe("{ echo b; echo a; } | sort", "a\nb\n", 0)]
#[case::reads_a_pipe("echo x | { read a; echo \"got $a\"; }", "got x\n", 0)]
#[case::reads_two_lines("printf 'a\\nb\\n' | { read x; read y; echo \"$y$x\"; }", "ba\n", 0)]
#[case::in_cmdsubst("x=$( { echo a; echo b; } ); echo \"$x\"", "a\nb\n", 0)]
#[case::in_cmdsubst_with_redirect("x=$( { echo out; echo err >&2; } 2>&1 ); echo \"[$x]\"", "[out\nerr]\n", 0)]
#[case::status_is_the_last_command("{ true; false; }; echo $?", "1\n", 0)]
#[case::status_success("{ false; true; }; echo $?", "0\n", 0)]
#[case::negated("! { false; }; echo $?", "0\n", 0)]
#[case::exit_stops_the_script("{ echo a; exit 3; echo b; }; echo c", "a\n", 3)]
#[case::exit_code_without_output("{ exit 4; }; echo after", "", 4)]
#[case::then_branch("if true; then { echo a; }; fi", "a\n", 0)]
#[tokio::test]
async fn a_brace_group_runs(#[case] source: &str, #[case] stdout: &str, #[case] code: i64) {
    let (out, actual_code) = run(source).await;
    assert_eq!(out, stdout, "`{source}` stdout");
    assert_eq!(actual_code, code, "`{source}` exit code");
}

/// `return` inside a group returns from the enclosing function.
#[tokio::test]
async fn return_inside_a_group_returns_from_the_function() {
    let (out, code) = run("f() { { echo in; return 5; echo no; }; echo no2; }; f; echo \"rc=$?\"").await;
    assert_eq!(out, "in\nrc=5\n");
    assert_eq!(code, 0);
}

/// `break` and `continue` inside a group act on the enclosing loop.
#[rstest]
#[case::continue_skips("for i in 1 2 3; do { [[ $i == 2 ]] && continue; echo $i; }; done", "1\n3\n")]
#[case::break_stops("for i in 1 2 3; do { [[ $i == 2 ]] && break; echo $i; }; done", "1\n")]
#[tokio::test]
async fn loop_control_inside_a_group_reaches_the_loop(#[case] source: &str, #[case] stdout: &str) {
    let (out, code) = run(source).await;
    assert_eq!(out, stdout, "`{source}`");
    assert_eq!(code, 0, "`{source}`");
}

// ---- session state -------------------------------------------------------------

/// A lone group runs in the current shell: what it sets stays set.
#[rstest]
#[case::variable("{ x=2; }; echo $x", "2\n")]
#[case::cwd("{ cd /tmp; }; pwd", "/tmp\n")]
#[case::function("{ g() { echo from-g; }; }; g", "from-g\n")]
#[tokio::test]
async fn a_lone_group_keeps_session_changes(#[case] source: &str, #[case] stdout: &str) {
    let (out, code) = run(source).await;
    assert_eq!(out, stdout, "`{source}`");
    assert_eq!(code, 0, "`{source}`");
}

/// The last-stage rule: a group as the last pipeline stage keeps its
/// changes. bash runs every stage in a subshell and prints `v=` here; kaish
/// keeps the last stage's session state for every compound alike.
#[tokio::test]
async fn a_group_as_the_last_stage_keeps_its_changes() {
    let (out, code) = run("echo ab | { read v; }; echo \"v=$v\"").await;
    assert_eq!(out, "v=ab\n");
    assert_eq!(code, 0);
}

/// An earlier stage is isolated: its changes do not reach the session.
#[tokio::test]
async fn a_group_as_an_earlier_stage_is_isolated() {
    let (out, code) = run("{ y=5; echo hi; } | cat; echo \"y=${y:-unset}\"").await;
    assert_eq!(out, "hi\ny=unset\n");
    assert_eq!(code, 0);
}

// ---- set -e ----------------------------------------------------------------------

/// A failing command inside a group trips errexit. A group whose status is
/// nonzero only because a command failed where `-e` is ignored does not:
/// bash runs `echo after` for both `{ false && true; }` and `{ ! true; }`.
#[rstest]
#[case::failing_command("set -e; { false; echo in; }; echo after", "", 1)]
#[case::and_list_left_side("set -e; { false && true; }; echo after", "after\n", 0)]
#[case::negated_inside("set -e; { ! true; }; echo after", "after\n", 0)]
#[case::negated_group("set -e; ! { true; }; echo after", "after\n", 0)]
#[case::or_fallback_exit("set -e; false || { echo handled; }; echo after", "handled\nafter\n", 0)]
#[tokio::test]
async fn errexit_inside_a_group_matches_bash(#[case] source: &str, #[case] stdout: &str, #[case] code: i64) {
    let (out, actual_code) = run(source).await;
    assert_eq!(out, stdout, "`{source}` stdout");
    assert_eq!(actual_code, code, "`{source}` exit code");
}

// ---- validator --------------------------------------------------------------------

/// `break` inside a group inside a loop is inside the loop.
#[tokio::test]
async fn break_in_a_group_inside_a_loop_validates() {
    let kernel = kernel();
    let result = kernel
        .execute("for i in 1 2; do { break; }; done; echo ok")
        .await
        .expect("must validate and run");
    assert_eq!(result.text_out(), "ok\n");
}

/// `break` inside a group at top level is still outside any loop.
#[tokio::test]
async fn break_in_a_group_outside_a_loop_is_refused() {
    let kernel = kernel();
    let error = kernel.execute("{ break; }").await.expect_err("must be refused");
    assert!(format!("{error}").contains("break used outside of a loop"), "{error}");
}
