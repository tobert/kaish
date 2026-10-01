//! A builtin whose text form is a list or table ends every line with a
//! newline, as GNU tools do, so `cmd | wc -l` counts every row and
//! `cmd > file` writes complete lines.

#![cfg(feature = "localfs")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;
use std::fs;

fn fixture() -> tempfile::TempDir {
    let tmp = tempfile::tempdir().unwrap();
    fs::create_dir(tmp.path().join("d")).unwrap();
    for name in ["a", "b", "c"] {
        fs::write(tmp.path().join("d").join(name), b"x\ny\n").unwrap();
    }
    fs::write(tmp.path().join("f"), b"one\ntwo\n").unwrap();
    tmp
}

async fn run(script: &str) -> (String, i64) {
    let tmp = fixture();
    let kernel = kernel_at(tmp.path());
    let r = kernel.execute(script).await.expect("execute");
    (r.text_out().into_owned(), r.code)
}

fn bash(script: &str) -> String {
    let tmp = fixture();
    let out = std::process::Command::new("bash")
        .arg("-c")
        .arg(script)
        .current_dir(tmp.path())
        .output()
        .unwrap();
    String::from_utf8(out.stdout).unwrap()
}

async fn assert_counts_lines(script: &str, expected: u32) {
    let (out, code) = run(&format!("set -o pipefail; {script} | wc -l")).await;
    assert_eq!(code, 0, "{script}: {out:?}");
    assert_eq!(out.trim().parse::<u32>().unwrap(), expected, "{script}: {out:?}");
}

#[tokio::test]
async fn ls_lines_match_gnu() {
    assert_counts_lines("ls d", 3).await;
    assert_eq!(bash("ls d | wc -l").trim(), "3");
}

#[tokio::test]
async fn ls_redirect_ends_with_newline() {
    let tmp = fixture();
    let kernel = kernel_at(tmp.path());
    kernel.execute("ls d > out.txt").await.expect("execute");
    assert_eq!(fs::read(tmp.path().join("out.txt")).unwrap(), b"a\nb\nc\n");
}

#[tokio::test]
async fn stat_is_one_line() {
    assert_counts_lines("stat f", 1).await;
}

#[tokio::test]
async fn find_lines_match_gnu() {
    assert_counts_lines("find d -type f", 3).await;
    assert_eq!(bash("find d -type f | wc -l").trim(), "3");
}

#[tokio::test]
async fn glob_lines_match_gnu() {
    assert_counts_lines("glob 'd/*'", 3).await;
}

#[tokio::test]
async fn seq_lines_match_gnu() {
    assert_counts_lines("seq 1 3", 3).await;
}

#[tokio::test]
async fn wc_lines_match_gnu() {
    assert_counts_lines("wc d/a d/b d/c", 4).await;
    assert_eq!(bash("wc d/a d/b d/c | wc -l").trim(), "4");
}

#[tokio::test]
async fn checksum_lines_match_gnu() {
    assert_counts_lines("checksum d/a d/b d/c", 3).await;
    assert_eq!(bash("sha256sum d/a d/b d/c | wc -l").trim(), "3");
}

#[tokio::test]
async fn file_lines_match_gnu() {
    assert_counts_lines("file d/a d/b d/c", 3).await;
}

#[tokio::test]
async fn grep_rows_match_gnu() {
    assert_counts_lines("grep -n x d/a d/b d/c", 3).await;
    assert_counts_lines("grep -l x d/a d/b d/c", 3).await;
}

#[tokio::test]
async fn tree_lines_end_with_newline() {
    let (out, code) = run("tree d").await;
    assert_eq!(code, 0);
    assert!(out.ends_with('\n'), "{out:?}");
}

#[tokio::test]
async fn command_substitution_still_strips_the_trailing_newline() {
    let (out, _) = run("x=$(ls d); echo \"[$x]\"").await;
    assert_eq!(out, "[a\nb\nc]\n");
    let (out, _) = run("x=$(stat f); echo \"[$x]\"").await;
    assert!(out.ends_with("]\n") && !out.contains("\n]"), "{out:?}");
}

#[tokio::test]
async fn json_output_is_unchanged() {
    let (out, _) = run("ls d --json").await;
    let v: serde_json::Value = serde_json::from_str(&out).unwrap();
    assert_eq!(v.as_array().unwrap().len(), 3, "{out}");
}

#[tokio::test]
async fn next_statement_output_starts_on_its_own_line() {
    for script in ["ls d", "find d -name a", "glob 'd/a'", "stat f"] {
        let (out, _) = run(&format!("{script}; echo NEXT")).await;
        assert!(out.ends_with("\nNEXT\n"), "{script}: {out:?}");
    }
    // Control: builtins that already end their last line.
    let (out, _) = run("seq 1 2; echo NEXT").await;
    assert_eq!(out, "1\n2\nNEXT\n");
}
