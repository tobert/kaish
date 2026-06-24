//! Kernel-routed tests for `cat -n` trailing-newline preservation.
//!
//! Root cause: `.lines().join("\n")` strips the trailing newline that the
//! input ends with, silently corrupting the last line of every `cat -n`
//! invocation that passes through a pipe.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use std::fs;
use tempfile::tempdir;

fn write_file(dir: &std::path::Path, name: &str, contents: &[u8]) {
    fs::write(dir.join(name), contents).expect("write file");
}

/// `cat -n` on a newline-terminated file must preserve the trailing newline.
/// GNU cat: `printf 'a\nb\n' | cat -n` → `     1\ta\n     2\tb\n`
#[tokio::test]
async fn cat_n_file_with_trailing_newline_preserved() {
    let dir = tempdir().unwrap();
    write_file(dir.path(), "lines.txt", b"alpha\nbeta\n");
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("cat -n lines.txt").await.expect("execute");
    assert_eq!(result.code, 0);
    // Exact byte-level assertion — trailing \n must be present.
    assert_eq!(
        result.text_out().as_ref(),
        "     1\talpha\n     2\tbeta\n",
        "cat -n must preserve trailing newline; got {:?}",
        result.text_out().as_ref(),
    );
}

/// `cat -n` on a file that does NOT end with `\n` must NOT add one.
/// GNU cat: `printf 'a\nb' | cat -n` → `     1\ta\n     2\tb`  (no trailing \n)
#[tokio::test]
async fn cat_n_file_without_trailing_newline_not_added() {
    let dir = tempdir().unwrap();
    write_file(dir.path(), "noeol.txt", b"alpha\nbeta");
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("cat -n noeol.txt").await.expect("execute");
    assert_eq!(result.code, 0);
    // Must NOT end with \n.
    assert_eq!(
        result.text_out().as_ref(),
        "     1\talpha\n     2\tbeta",
        "cat -n must not add a trailing newline that was not present; got {:?}",
        result.text_out().as_ref(),
    );
}

/// `cat -n` piped from stdin with trailing newline must also preserve it.
#[tokio::test]
async fn cat_n_stdin_with_trailing_newline_preserved() {
    let dir = tempdir().unwrap();
    let kernel = kernel_at(dir.path());
    // Use a here-string / echo pipeline to feed stdin.  The script writes a
    // file and cats it so the kernel can feed it through stdin to `cat -n`.
    write_file(dir.path(), "src.txt", b"line1\nline2\n");
    // Route via pipe: `cat src.txt | cat -n`
    let result = kernel
        .execute("cat src.txt | cat -n")
        .await
        .expect("execute");
    assert_eq!(result.code, 0);
    assert_eq!(
        result.text_out().as_ref(),
        "     1\tline1\n     2\tline2\n",
        "cat -n from stdin must preserve trailing newline; got {:?}",
        result.text_out().as_ref(),
    );
}

/// `cat -n` across multiple files: trailing newline from the last file is
/// preserved, and line numbering is continuous.
#[tokio::test]
async fn cat_n_multiple_files_trailing_newline_preserved() {
    let dir = tempdir().unwrap();
    write_file(dir.path(), "a.txt", b"aaa\n");
    write_file(dir.path(), "b.txt", b"bbb\n");
    let kernel = kernel_at(dir.path());
    let result = kernel
        .execute("cat -n a.txt b.txt")
        .await
        .expect("execute");
    assert_eq!(result.code, 0);
    assert_eq!(
        result.text_out().as_ref(),
        "     1\taaa\n     2\tbbb\n",
        "cat -n multiple files must preserve trailing newline; got {:?}",
        result.text_out().as_ref(),
    );
}

/// Sanity check: the run() helper trims, so use it only for exit-code checks
/// in other tests. Here we just confirm the basic flag reaches the tool.
#[tokio::test]
async fn cat_n_basic_numbering_smoke() {
    let dir = tempdir().unwrap();
    write_file(dir.path(), "x.txt", b"hello\nworld\n");
    let kernel = kernel_at(dir.path());
    let (out, code) = run(&kernel, "cat -n x.txt").await;
    assert_eq!(code, 0);
    assert!(out.contains("1\thello"), "line 1 numbered: {out:?}");
    assert!(out.contains("2\tworld"), "line 2 numbered: {out:?}");
}
