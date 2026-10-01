//! Kernel-routed tests for `cat -A` (`-vET`) and its parts. Expected strings
//! are what GNU coreutils 9 prints for the same bytes.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::kernel_at;
use tempfile::tempdir;

/// Tab, ^A, UTF-8 e-acute, DEL, 0xFF, CRLF, an empty line, no final newline.
const BYTES: &[u8] = b"a\tb\n\x01c\xc3\xa9 \x7f \xff\r\n\nend";

async fn cat(args: &str) -> (String, String, i64) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f"), BYTES).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(&format!("cat {args} f")).await.unwrap();
    (result.text_out().to_string(), result.err.clone(), result.code)
}

#[tokio::test]
async fn cat_a_is_vet() {
    let (out, err, code) = cat("-A").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out, "a^Ib$\n^AcM-CM-) ^? M-^?^M$\n$\nend");
}

#[tokio::test]
async fn cat_v_shows_nonprinting_but_not_tabs_or_ends() {
    let (out, err, code) = cat("-v").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out, "a\tb\n^AcM-CM-) ^? M-^?^M\n\nend");
}

#[tokio::test]
async fn cat_e_upper_marks_line_ends_only() {
    let (out, err, code) = cat("-E").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out.as_bytes(), b"a\tb$\n\x01c\xc3\xa9 \x7f \xff^M$\n$\nend");
}

#[tokio::test]
async fn cat_t_upper_marks_tabs_only() {
    let (out, err, code) = cat("-T").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(out.as_bytes(), b"a^Ib\n\x01c\xc3\xa9 \x7f \xff\r\n\nend");
}

#[tokio::test]
async fn cat_a_numbers_with_n() {
    let (out, err, code) = cat("-nA").await;
    assert_eq!(code, 0, "{err}");
    assert_eq!(
        out,
        "     1\ta^Ib$\n     2\t^AcM-CM-) ^? M-^?^M$\n     3\t$\n     4\tend"
    );
}

#[tokio::test]
async fn cat_a_reads_stdin_from_a_pipe() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f"), BYTES).unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("cat f | cat -A").await.unwrap();
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out(), "a^Ib$\n^AcM-CM-) ^? M-^?^M$\n$\nend");
}

#[tokio::test]
async fn cat_a_concatenates_files_in_order() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("x"), "p\tq\n").unwrap();
    fs::write(dir.path().join("y"), "r\n").unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("cat -A x y").await.unwrap();
    assert_eq!(result.code, 0, "{}", result.err);
    assert_eq!(result.text_out(), "p^Iq$\nr$\n");
}
