//! Tests for how the CLI and REPL present parse and lexer failures.
//!
//! `ParseError::format` (`kaish-kernel/src/parser.rs`) already produces a
//! `line:col [parse]: <message>` diagnostic plus a `  | <source line>`
//! snippet. Before this fix, `-c`, script, and REPL execution all routed a
//! parse/lexer failure through `Kernel::execute_streaming` → `ClientError` →
//! `anyhow::Context`, burying that diagnostic under `Error: execution
//! failed` / `Caused by:` / `execution error: parse error:`. These tests
//! guard that the diagnostic leads and that wrapper noise is gone — and
//! that a *runtime* failure (command not found, nonzero exit, a builtin
//! error), which never went through that wrapper, is untouched.
//!
//! Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::process::{Command, Stdio};

use kaish_repl::{ProcessResult, Repl};

/// Run `kaish <args...>` with no stdin; return (stdout, stderr, exit code).
fn run_kaish(args: &[&str]) -> (String, String, i32) {
    let kaish = env!("CARGO_BIN_EXE_kaish");
    let output = Command::new(kaish)
        .args(args)
        .stdin(Stdio::null())
        .output()
        .expect("spawn kaish");
    (
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
        output.status.code().expect("process exited via signal, not a code"),
    )
}

/// Wrapper strings a parse/lexer diagnostic must no longer carry.
const WRAPPER_NOISE: &[&str] = &["Error:", "Caused by", "execution error", "execution failed"];

fn assert_no_wrapper_noise(text: &str) {
    for phrase in WRAPPER_NOISE {
        assert!(
            !text.contains(phrase),
            "diagnostic still carries wrapper noise {phrase:?}: {text:?}"
        );
    }
}

// ── `kaish -c` ──────────────────────────────────────────────────────

#[test]
fn cli_parse_error_prints_diagnostic_directly() {
    let (stdout, stderr, code) = run_kaish(&["-c", "echo $GREET/world.txt"]);
    assert_eq!(code, 1, "a parse failure must still exit 1: stderr={stderr:?}");
    assert_eq!(stdout, "", "a parse failure must not execute anything");
    assert!(
        stderr.starts_with("1:6 [parse]:"),
        "the diagnostic must lead stderr, got: {stderr:?}"
    );
    assert!(
        stderr.contains("  | echo $GREET/world.txt"),
        "the source snippet must be present, got: {stderr:?}"
    );
    assert_no_wrapper_noise(&stderr);
}

#[test]
fn cli_lexer_error_prints_diagnostic_directly() {
    let (stdout, stderr, code) = run_kaish(&["-c", "echo `ls`"]);
    assert_eq!(code, 1, "a lexer failure must still exit 1: stderr={stderr:?}");
    assert_eq!(stdout, "");
    // `[parse]` here is inherited from `ParseError::format`, which labels
    // every diagnostic `[parse]` regardless of whether the lexer or the
    // grammar caught it (`parser::parse` wraps a lexer error in a
    // `ParseError` before this function ever sees it) — not a mislabel this
    // fix introduces.
    assert!(
        stderr.starts_with("1:6 [parse]:"),
        "the diagnostic must lead stderr, got: {stderr:?}"
    );
    assert!(
        stderr.contains("backticks are not supported"),
        "got: {stderr:?}"
    );
    assert_no_wrapper_noise(&stderr);
}

#[test]
fn cli_runtime_command_not_found_is_unchanged() {
    let (stdout, stderr, code) = run_kaish(&["-c", "totally_bogus_cmd_xyz"]);
    assert_eq!(code, 127);
    assert_eq!(stdout, "");
    assert_eq!(stderr, "command not found: totally_bogus_cmd_xyz");
}

#[test]
fn cli_runtime_builtin_error_is_unchanged() {
    let (stdout, stderr, code) = run_kaish(&["-c", "cat /nonexistent-file-xyz-abc"]);
    assert_eq!(code, 1);
    assert_eq!(stdout, "");
    assert!(
        stderr.starts_with("cat: /nonexistent-file-xyz-abc: not found:"),
        "got: {stderr:?}"
    );
}

#[test]
fn cli_nonzero_exit_is_unchanged() {
    let (stdout, stderr, code) = run_kaish(&["-c", "exit 3"]);
    assert_eq!(code, 3);
    assert_eq!(stdout, "");
    assert_eq!(stderr, "");
}

#[test]
fn cli_validation_error_keeps_the_anyhow_chain() {
    // Validation runs *after* a successful parse and produces the same
    // `line:col [code]: message` diagnostic shape as a parse error — but
    // it is a separate `Err` site in `execute_streaming_inner`
    // ("validation failed:\n…") that this fix does not touch. Pinning it
    // here records the scope boundary: only parse/lexer failures were
    // moved out of the wrapper.
    let (stdout, stderr, code) = run_kaish(&["-c", "v=1; for x in $v; do echo $x; done"]);
    assert_eq!(code, 1);
    assert_eq!(stdout, "");
    assert!(
        stderr.starts_with("Error: execution failed"),
        "validation failures are out of this fix's scope and must keep the old wrapper, got: {stderr:?}"
    );
    assert!(stderr.contains("Caused by:"), "got: {stderr:?}");
    assert!(stderr.contains("validation failed:"), "got: {stderr:?}");
}

#[test]
fn cli_missing_script_keeps_the_anyhow_chain() {
    // A non-parse `Err` path (the script file itself can't be read) must
    // stay exactly as it was — proof the fix is scoped to parse/lexer
    // failures, not every `Err` this binary can return.
    let dir = tempfile::tempdir().expect("tempdir");
    let missing = dir.path().join("does-not-exist.kai");
    let (stdout, stderr, code) = run_kaish(&[missing.to_str().expect("utf8 path")]);
    assert_eq!(code, 1);
    assert_eq!(stdout, "");
    assert!(
        stderr.starts_with("Error: Failed to read script:"),
        "got: {stderr:?}"
    );
    assert!(stderr.contains("Caused by:"), "got: {stderr:?}");
}

// ── script files ────────────────────────────────────────────────────

#[test]
fn cli_script_parse_error_prints_diagnostic_with_correct_line() {
    let dir = tempfile::tempdir().expect("tempdir");
    let script_path = dir.path().join("bad.kai");
    std::fs::write(&script_path, "echo ok\necho $GREET/world.txt\n").expect("write script");
    let (stdout, stderr, code) = run_kaish(&[script_path.to_str().expect("utf8 path")]);
    assert_eq!(code, 1);
    assert_eq!(
        stdout, "",
        "the whole script is parsed up front — a later parse failure must run nothing, including line 1"
    );
    assert!(
        stderr.starts_with("2:6 [parse]:"),
        "the line number must reflect the script (line 2), got: {stderr:?}"
    );
    assert_no_wrapper_noise(&stderr);
}

/// A shebang script is parsed with its first line blanked, so a diagnostic on
/// a later line proves the check runs on the blanked source rather than the
/// raw file. The plain-script test above cannot show that — its line 1 is
/// ordinary source.
#[test]
fn cli_shebang_script_parse_error_reports_the_source_line() {
    let dir = tempfile::tempdir().expect("tempdir");
    let script_path = dir.path().join("bad-shebang.kai");
    std::fs::write(&script_path, "#!/usr/bin/env kaish\necho ok\necho $GREET/world.txt\n")
        .expect("write script");
    let (stdout, stderr, code) = run_kaish(&[script_path.to_str().expect("utf8 path")]);
    assert_eq!(code, 1);
    assert_eq!(stdout, "", "a parse failure runs nothing");
    assert!(
        stderr.starts_with("3:6 [parse]:"),
        "the blanked shebang must still count as line 1, got: {stderr:?}"
    );
    assert_no_wrapper_noise(&stderr);
}

/// `--plan` is the one parse-failure surface this deliberately does NOT
/// reroute: it answers in JSON on both outcomes, so prose on stderr would
/// break a caller that parses one shape.
#[test]
fn plan_parse_error_stays_json_and_exits_2() {
    let (stdout, stderr, code) = run_kaish(&["--plan", "echo $GREET/world.txt"]);
    assert_eq!(code, 2, "a plan that does not parse exits 2");
    assert!(
        stdout.starts_with("{\"errors\":"),
        "the plan surface answers in JSON, got: {stdout:?}"
    );
    assert!(
        !stdout.contains("[parse]:"),
        "the human diagnostic format must not leak into the JSON surface: {stdout:?}"
    );
    assert_eq!(stderr, "", "the plan surface says everything on stdout");
}

// ── interactive REPL ────────────────────────────────────────────────

#[test]
fn repl_parse_error_prints_diagnostic_directly() {
    let mut repl = Repl::new().expect("Failed to create REPL");
    match repl.process_line("echo $GREET/world.txt") {
        ProcessResult::Output(output) => {
            assert!(
                output.starts_with("1:6 [parse]:"),
                "the diagnostic must lead, got: {output:?}"
            );
            assert_no_wrapper_noise(&output);
        }
        other => panic!("expected ProcessResult::Output, got {other:?}"),
    }
}

#[test]
fn repl_runtime_error_is_unchanged() {
    let mut repl = Repl::new().expect("Failed to create REPL");
    match repl.process_line("totally_bogus_cmd_xyz") {
        ProcessResult::Output(output) => {
            assert!(
                output.contains("command not found: totally_bogus_cmd_xyz"),
                "got: {output:?}"
            );
            // A runtime failure is `Ok(ExecResult)`, not `Err`, so it never
            // went through `ClientError`/anyhow — it must not gain a
            // wrapper just because a parse-error class exists nearby.
            assert_no_wrapper_noise(&output);
        }
        other => panic!("expected ProcessResult::Output, got {other:?}"),
    }
}
