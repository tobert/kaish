//! A non-terminal kaish never prompts (`docs/approval-ledger.md` §C.3, §H
//! acceptance test 2).
//!
//! These spawn the real binary with pipes on every stream, which is the only
//! way to prove the negative the spec asks for: not "the prompt was skipped"
//! but "no prompt was written anywhere a caller could collect it". The
//! approval question would otherwise land in the same stdout an agent or a
//! shell pipeline reads as data.
//!
//! Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::process::{Command, Stdio};

/// Run `kaish <args...>` with every stream a pipe (so nothing is a TTY) and
/// return `(exit code, stdout, stderr)`.
fn run(args: &[&str]) -> (i32, String, String) {
    let out = Command::new(env!("CARGO_BIN_EXE_kaish"))
        .args(args)
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .expect("spawn kaish");
    (
        out.status.code().expect("kaish exited with a code"),
        String::from_utf8_lossy(&out.stdout).into_owned(),
        String::from_utf8_lossy(&out.stderr).into_owned(),
    )
}

#[test]
fn a_gated_statement_in_a_non_tty_session_exits_2_and_never_prompts() {
    let (code, stdout, stderr) = run(&["--gate", "echo", "-c", "echo held"]);

    assert_eq!(code, 2, "a held statement exits 2\nout: {stdout}\nerr: {stderr}");
    assert!(
        stderr.contains("pending approval"),
        "the request must be named on stderr: {stderr}"
    );
    for stream in [&stdout, &stderr] {
        assert!(
            !stream.contains("grant?"),
            "no prompt may be written to a non-terminal: {stream}"
        );
        assert!(
            !stream.contains("[y/a/N]"),
            "no prompt may be written to a non-terminal: {stream}"
        );
    }
    assert!(
        !stdout.contains("held"),
        "nothing of the held statement runs: {stdout}"
    );
}

#[test]
fn an_ungated_statement_is_untouched_by_the_gate_flag() {
    // The classifier gates the names it was given and observes everything
    // else — `--gate` must not turn every statement into a question.
    let (code, stdout, _stderr) = run(&["--gate", "rm", "-c", "echo fine"]);
    assert_eq!(code, 0);
    assert_eq!(stdout.trim(), "fine");
}

#[test]
fn the_gate_flag_takes_a_list_and_an_equals_form() {
    let (code, _stdout, stderr) = run(&["--gate=rm,echo", "-c", "echo held"]);
    assert_eq!(code, 2, "{stderr}");
}

#[test]
fn a_quoted_command_name_is_an_argument_and_does_not_gate() {
    // The discrimination classifying the *plan* buys over the raw line: this
    // statement mentions `rm` and plans only `echo`.
    let (code, stdout, stderr) = run(&["--gate", "rm", "-c", "echo 'rm -rf /'"]);
    assert_eq!(code, 0, "{stderr}");
    assert_eq!(stdout.trim(), "rm -rf /");
}

// ── Every request names who raised it ────────────────────────────────

/// Pull the `approval` envelope out of a `--json` result on stdout.
fn approval_envelope(stdout: &str) -> serde_json::Value {
    let line = stdout
        .lines()
        .find(|line| line.contains("\"approval\""))
        .unwrap_or_else(|| panic!("no --json approval envelope in: {stdout}"));
    let parsed: serde_json::Value = serde_json::from_str(line).expect("the envelope is JSON");
    parsed["approval"].clone()
}

/// Who a non-interactive run says it is. `$USER` when the environment names
/// one — the harness inherits the caller's — and the fallback otherwise, so
/// this asserts the *shape* the record must never have rather than a name
/// that depends on where the tests run.
fn assert_attributed(principal: &serde_json::Value, context: &str) {
    let id = principal["id"].as_str().expect("an id");
    assert!(
        !id.is_empty(),
        "{context}: a request with an empty principal id cannot be traced back to anyone: \
         {principal}"
    );
    assert_eq!(
        principal["kind"], "automation",
        "{context}: a run with no prompt is automation, whoever launched it: {principal}"
    );
}

#[test]
fn a_gated_request_from_dash_c_names_its_principal() {
    let dir = tempfile::tempdir().expect("tempdir");
    let target = dir.path().join("notes.txt");
    std::fs::write(&target, "hi").expect("write the fixture");

    let (code, stdout, stderr) = run(&[
        "-c",
        &format!(
            "set -o approvals; rm --json {}",
            target.to_str().expect("utf-8 path")
        ),
    ]);

    assert_eq!(code, 2, "the delete is held\nout: {stdout}\nerr: {stderr}");
    assert_attributed(&approval_envelope(&stdout)["principal"], "kaish -c");
}

#[test]
fn a_gated_request_from_a_script_names_its_principal() {
    // The second non-interactive door. It had the same empty principal for a
    // release because it built its own config; both now share one
    // constructor, and this is what keeps them from drifting apart again.
    let dir = tempfile::tempdir().expect("tempdir");
    let target = dir.path().join("notes.txt");
    std::fs::write(&target, "hi").expect("write the fixture");
    let script = dir.path().join("gate.kai");
    std::fs::write(
        &script,
        format!(
            "set -o approvals\nrm --json {}\n",
            target.to_str().expect("utf-8 path")
        ),
    )
    .expect("write the script");

    let (code, stdout, stderr) = run(&[script.to_str().expect("utf-8 path")]);

    assert_eq!(code, 2, "the delete is held\nout: {stdout}\nerr: {stderr}");
    assert_attributed(&approval_envelope(&stdout)["principal"], "kaish script.kai");
}
