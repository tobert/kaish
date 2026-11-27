//! `kaish --validate`: validate kaish scripts from the CLI without executing.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Write;
use std::process::{Command, Stdio};

fn kaish_bin() -> &'static str {
    env!("CARGO_BIN_EXE_kaish")
}

#[test]
fn validate_valid_expression_exits_0() {
    let out = Command::new(kaish_bin())
        .args(["--validate", "-e", "echo hello"])
        .output()
        .expect("run kaish --validate");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("<expr>: valid"), "stdout: {stdout}");
}

#[test]
fn validate_parse_error_exits_2() {
    let out = Command::new(kaish_bin())
        .args(["--validate", "-e", "if true; then echo hi"])
        .output()
        .expect("run kaish --validate");
    assert_eq!(out.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("parse error"), "stderr: {stderr}");
}

#[test]
fn validate_semantic_error_exits_1() {
    let out = Command::new(kaish_bin())
        .args(["--validate", "-e", "break"])
        .output()
        .expect("run kaish --validate");
    assert_eq!(out.status.code(), Some(1));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("loop"), "stderr: {stderr}");
}

#[test]
fn validate_quiet_mode_suppresses_output() {
    let ok = Command::new(kaish_bin())
        .args(["--validate", "-q", "-e", "echo hi"])
        .output()
        .expect("run kaish --validate -q");
    assert_eq!(ok.status.code(), Some(0));
    assert!(ok.stdout.is_empty());
    assert!(ok.stderr.is_empty());

    let bad = Command::new(kaish_bin())
        .args(["--validate", "-q", "-e", "break"])
        .output()
        .expect("run kaish --validate -q");
    assert_eq!(bad.status.code(), Some(1));
    assert!(bad.stdout.is_empty());
    assert!(bad.stderr.is_empty());
}

#[test]
fn validate_warnings_flag_gates_warnings() {
    let without = Command::new(kaish_bin())
        .args(["--validate", "-e", "notacommand"])
        .output()
        .expect("run kaish --validate");
    assert_eq!(without.status.code(), Some(0));
    assert!(String::from_utf8_lossy(&without.stdout).contains(": valid"));

    let with = Command::new(kaish_bin())
        .args(["--validate", "-w", "-e", "notacommand"])
        .output()
        .expect("run kaish --validate -w");
    assert_eq!(with.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&with.stdout);
    assert!(!stdout.contains(": valid"), "warnings should be printed instead of bare valid: {stdout}");
    assert!(stdout.contains("notacommand"), "stdout: {stdout}");
}

#[test]
fn validate_script_file_with_shebang_and_stdin() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let script = dir.path().join("check.kai");
    std::fs::write(&script, "#!/usr/bin/env kaish\necho ok\n").expect("write script");

    let out = Command::new(kaish_bin())
        .args(["--validate", script.to_str().unwrap()])
        .output()
        .expect("run kaish --validate file");
    assert_eq!(out.status.code(), Some(0), "stderr: {}", String::from_utf8_lossy(&out.stderr));

    // Stdin via `-`
    let mut child = Command::new(kaish_bin())
        .args(["--validate", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("spawn kaish --validate -");
    child.stdin.take().unwrap().write_all(b"echo from stdin\n").unwrap();
    let out = child.wait_with_output().expect("wait");
    assert_eq!(out.status.code(), Some(0));
    assert!(String::from_utf8_lossy(&out.stdout).contains("<stdin>: valid"));
}

#[test]
fn validate_missing_input_exits_2() {
    let out = Command::new(kaish_bin())
        .args(["--validate"])
        .output()
        .expect("run kaish --validate");
    assert_eq!(out.status.code(), Some(2));
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(stderr.contains("missing input"), "stderr: {stderr}");
}

#[test]
fn help_mentions_validate() {
    let out = Command::new(kaish_bin())
        .arg("--help")
        .output()
        .expect("run kaish --help");
    assert_eq!(out.status.code(), Some(0));
    let stdout = String::from_utf8_lossy(&out.stdout);
    assert!(stdout.contains("--validate"), "help must advertise --validate: {stdout}");
}

#[test]
fn validate_double_dash_allows_dash_prefixed_path() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let script = dir.path().join("-weird.kai");
    std::fs::write(&script, "echo ok\n").expect("write script");

    let out = Command::new(kaish_bin())
        .current_dir(dir.path())
        .args(["--validate", "--", "-weird.kai"])
        .output()
        .expect("run kaish --validate -- -weird.kai");
    assert_eq!(out.status.code(), Some(0), "stderr: {}", String::from_utf8_lossy(&out.stderr));
}

#[test]
fn validate_expr_overlay_literal_is_not_eaten() {
    // `--overlay` passed as the `-e` value must reach the parser, not be
    // stripped by the top-level CLI `--overlay` filter.
    let out = Command::new(kaish_bin())
        .args(["--validate", "-e", "--overlay"])
        .output()
        .expect("run kaish --validate -e --overlay");
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        !stderr.contains("-e requires an expression"),
        "--overlay must not be stripped from -e value: {stderr}"
    );
}

#[test]
fn validate_duplicate_expr_and_unreadable_file_exit_2() {
    let dup = Command::new(kaish_bin())
        .args(["--validate", "-e", "echo a", "-e", "echo b"])
        .output()
        .expect("run duplicate -e");
    assert_eq!(dup.status.code(), Some(2));
    assert!(String::from_utf8_lossy(&dup.stderr).contains("only be passed once"));

    let missing = Command::new(kaish_bin())
        .args(["--validate", "/nonexistent/kaish-validate-missing.kai"])
        .output()
        .expect("run missing file");
    assert_eq!(missing.status.code(), Some(2));

    let quiet_parse = Command::new(kaish_bin())
        .args(["--validate", "-q", "-e", "if true; then echo hi"])
        .output()
        .expect("run quiet parse error");
    assert_eq!(quiet_parse.status.code(), Some(2));
    assert!(quiet_parse.stdout.is_empty());
    assert!(quiet_parse.stderr.is_empty());
}
