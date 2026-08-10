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
