//! Regression test for GH #127: a shebang'd `.kai` script must not shift
//! reported parse-error line numbers. `run_script` (kaish-repl/src/main.rs)
//! used to strip the shebang line entirely via `.lines().skip(1)`, which
//! deletes a line and shifts every subsequent line's reported number down by
//! one. This spawns the real `kaish` binary because the bug lives in the
//! frontend's script-loading path, below the kernel's own parser (which
//! reports correct line numbers against whatever source string it's given).
//!
//! Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Write;
use std::process::Command;

/// Write `contents` to a temp `.kai` file and run it with the `kaish` binary,
/// returning stderr as a `String`.
fn run_script(contents: &str) -> String {
    let kaish = env!("CARGO_BIN_EXE_kaish");
    let dir = tempfile::tempdir().expect("tempdir");
    let script_path = dir.path().join("script.kai");
    let mut file = std::fs::File::create(&script_path).expect("create script");
    file.write_all(contents.as_bytes()).expect("write script");
    drop(file);

    let output = Command::new(kaish)
        .arg(&script_path)
        .output()
        .expect("run kaish script");
    String::from_utf8_lossy(&output.stderr).into_owned()
}

#[test]
fn shebang_script_reports_correct_error_line() {
    // Line 1 is the shebang; line 2 is blank; line 3 has a deliberate parse
    // error (a bare `)` token, which the parser rejects at its own position
    // rather than at end-of-input). Without the fix, the parser sees the
    // shebang line removed and everything shifts up by one, so this error is
    // (mis)reported at line 2 instead of line 3.
    let script = "#!/usr/bin/env kaish\n\n)\n";
    let stderr = run_script(script);
    assert!(
        stderr.contains("3:1"),
        "expected error to be reported at line 3, got stderr: {stderr:?}"
    );
    assert!(
        !stderr.contains("2:1"),
        "error line number regressed to the pre-shebang-strip line, stderr: {stderr:?}"
    );
}

#[test]
fn non_shebang_script_reports_correct_error_line() {
    // Control case: no shebang, so no stripping happens at all. The error is
    // on line 2 here (no leading shebang line to shift anything).
    let script = "echo hi\n)\n";
    let stderr = run_script(script);
    assert!(
        stderr.contains("2:1"),
        "expected error to be reported at line 2, got stderr: {stderr:?}"
    );
}
