//! Exit-code convention: what a caller may conclude from `$?`.
//!
//! kaish reserves exit 1 for a *result* wherever a builtin already uses it as
//! one — `grep` found nothing, `test` was false, `cmp`/`diff` saw a
//! difference. In those builtins every error reports 2 instead, so a caller
//! branching on 1 never mistakes a broken command for a negative answer.
//! `kaish -c` and `kaish --plan` apply the same rule to a whole program: a
//! rejection (lex, parse, or validation) exits 2, matching the usage-error
//! code a builtin returns for bad argv.
//!
//! These drive the real binary because the contract is the process one.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;
use std::process::Command;

use tempfile::TempDir;

/// A directory holding `lines.txt`, whose middle line contains `[cast:`.
fn fixture() -> TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    std::fs::write(dir.path().join("lines.txt"), "alpha\n[cast:deepseek]\nbeta\n")
        .expect("write fixture");
    dir
}

/// Run `kaish -c <source>` in `cwd`; return (exit code, stdout, stderr).
fn run_c(cwd: &Path, source: &str) -> (i32, String, String) {
    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .current_dir(cwd)
        .arg("-c")
        .arg(source)
        .output()
        .expect("run kaish -c");
    (
        out.status.code().expect("exit code"),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

/// Run `kaish --plan <source>`; return (exit code, parsed stdout).
fn plan(source: &str) -> (i32, serde_json::Value) {
    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .arg("--plan")
        .arg(source)
        .output()
        .expect("run kaish --plan");
    let stdout = String::from_utf8(out.stdout).expect("utf-8 stdout");
    let json = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout was not JSON ({e}): {stdout:?}"));
    (out.status.code().expect("exit code"), json)
}

// --- grep: 1 means "no match" and nothing else ------------------------------

#[test]
fn grep_exits_1_only_when_it_searched_and_found_nothing() {
    let dir = fixture();
    let (code, out, _) = run_c(dir.path(), "grep zzz lines.txt");
    assert_eq!(code, 1, "no-match is the one thing grep spends exit 1 on");
    assert_eq!(out, "");
}

#[test]
fn grep_exits_2_for_an_unclosed_character_class() {
    // The regression: `grep -v '[cast:'` is correctly quoted, so the pattern
    // reaches the regex engine intact and fails to compile there. Reporting
    // that as 1 is indistinguishable from "no lines matched".
    let dir = fixture();
    let (code, _, err) = run_c(dir.path(), "grep -v '[cast:' lines.txt");
    assert_eq!(code, 2, "an uncompilable pattern is a usage error, not a result");
    assert!(err.contains("unclosed character class"), "stderr was: {err}");
}

#[test]
fn grep_exits_2_for_a_bad_pattern_that_arrives_through_a_variable() {
    // The validator compiles a *literal* pattern and skips anything holding a
    // `<dynamic>` marker, so a pattern that arrives through `$p` reaches the
    // regex builders inside execute() instead. That is the same failure, and
    // it must not come back as 1 just because the validator could not see it.
    let dir = fixture();
    let (code, _, err) = run_c(dir.path(), "p='[cast:'; grep -v \"$p\" lines.txt");
    assert_eq!(code, 2, "a pattern is no less broken for being computed");
    assert!(err.contains("invalid pattern"), "stderr was: {err}");
}

#[test]
fn grep_exits_2_when_a_file_cannot_be_read() {
    let dir = fixture();
    let (code, _, _) = run_c(dir.path(), "grep alpha no_such_file.txt");
    assert_eq!(code, 2, "an unreadable file is not 'found no matches'");
}

#[test]
fn grep_exits_2_when_the_pattern_argument_is_missing() {
    let dir = fixture();
    let (code, _, _) = run_c(dir.path(), "grep");
    assert_eq!(code, 2);
}

#[test]
fn escaping_the_bracket_is_the_fix_the_error_names() {
    let dir = fixture();
    let (code, out, _) = run_c(dir.path(), r"grep -v '\[cast:' lines.txt");
    assert_eq!(code, 0);
    assert_eq!(out.trim(), "alpha\nbeta");
}

#[test]
fn the_invalid_pattern_error_names_the_fix_and_not_our_regex_crate() {
    let dir = fixture();
    let (_, _, err) = run_c(dir.path(), "grep -v '[cast:' lines.txt");
    assert!(
        err.contains(r"\["),
        "the error must name the escape that fixes it; stderr was: {err}"
    );
    assert!(
        !err.contains("docs.rs"),
        "an implementation crate is not an affordance; stderr was: {err}"
    );
}

// --- diff/cmp/test: 1 is already a result there too -------------------------

#[test]
fn diff_exits_1_for_a_difference_and_2_for_a_missing_operand() {
    let dir = fixture();
    std::fs::write(dir.path().join("other.txt"), "alpha\nchanged\nbeta\n").unwrap();

    let (differ, _, _) = run_c(dir.path(), "diff lines.txt other.txt");
    assert_eq!(differ, 1, "files differ is diff's result, not an error");

    let (usage, _, _) = run_c(dir.path(), "diff lines.txt");
    assert_eq!(usage, 2, "a missing operand is a usage error");
}

#[test]
fn cmp_keeps_1_for_a_difference() {
    let dir = fixture();
    std::fs::write(dir.path().join("other.txt"), "alpha\nchanged\nbeta\n").unwrap();
    let (code, _, _) = run_c(dir.path(), "cmp lines.txt other.txt");
    assert_eq!(code, 1);
}

#[test]
fn test_keeps_1_for_a_false_condition() {
    let dir = fixture();
    let (code, _, _) = run_c(dir.path(), "test -f no_such_file.txt");
    assert_eq!(code, 1, "false is test's result");
}

// --- whole-program rejections -----------------------------------------------

#[test]
fn a_parse_rejection_exits_2_like_the_same_source_under_plan() {
    let dir = fixture();
    let (via_c, _, _) = run_c(dir.path(), "if");
    let (via_plan, _) = plan("if");
    assert_eq!(via_plan, 2, "--plan already documents 2 for a rejection");
    assert_eq!(via_c, via_plan, "-c and --plan must agree on a rejection");
}

#[test]
fn a_validation_rejection_exits_2() {
    let dir = fixture();
    let (code, _, err) = run_c(dir.path(), "grep '[cast:' lines.txt");
    assert_eq!(code, 2);
    assert!(err.contains("validation failed"), "stderr was: {err}");
}

// --- --plan sees what the kernel would reject -------------------------------

#[test]
fn plan_reports_a_validation_failure_instead_of_a_clean_plan() {
    // `--plan` is the dry run an agent reaches for before committing to a
    // command. Printing a clean plan for a program the kernel then rejects
    // makes the dry run worse than useless.
    let (code, json) = plan("grep '[cast:' lines.txt");
    assert_eq!(code, 2, "a program that cannot run is not a plan");
    let errors = json["errors"].as_array().expect("errors array");
    let text = errors.iter().map(|e| e["message"].as_str().unwrap_or("")).collect::<String>();
    assert!(text.contains("unclosed character class"), "errors were: {errors:?}");
}

#[test]
fn plan_still_exits_0_for_a_program_that_would_run() {
    let (code, json) = plan(r"grep -v '\[cast:' lines.txt");
    assert_eq!(code, 0);
    assert!(json["statements"].is_array());
}
