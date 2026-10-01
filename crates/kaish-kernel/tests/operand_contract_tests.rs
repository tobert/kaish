//! Operand contracts: extra operands are refused by name, `head -c` applies
//! to every file, and sort exits 2 on an unreadable operand like GNU.

#![cfg(feature = "localfs")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;
use std::fs;

async fn run_with_good(script: &str) -> (String, String, i64) {
    let tmp = tempfile::tempdir().unwrap();
    fs::write(tmp.path().join("good"), b"one\ntwo\n").unwrap();
    let kernel = kernel_at(tmp.path());
    let r = kernel.execute(script).await.expect("execute");
    (r.text_out().into_owned(), r.err.clone(), r.code)
}

#[tokio::test]
async fn uniq_refuses_an_extra_operand() {
    let (out, err, code) = run_with_good("uniq good nosuch").await;
    assert_eq!(out, "", "no partial output");
    assert!(err.contains("uniq: extra operand 'nosuch'"), "{err}");
    assert!(err.contains("> nosuch") || err.contains(">"), "names the redirect: {err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn base64_refuses_an_extra_operand() {
    let (out, err, code) = run_with_good("base64 good nosuch good").await;
    assert_eq!(out, "");
    assert!(err.contains("base64: extra operand 'nosuch'"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn xxd_refuses_an_extra_operand() {
    let (out, err, code) = run_with_good("xxd good outfile").await;
    assert_eq!(out, "");
    assert!(err.contains("xxd: extra operand 'outfile'"), "{err}");
    assert_eq!(code, 1);
}

#[tokio::test]
async fn single_operand_still_works() {
    let (out, _, code) = run_with_good("uniq good").await;
    assert_eq!((out.as_str(), code), ("one\ntwo\n", 0));
    let (out, _, code) = run_with_good("base64 good").await;
    assert_eq!((out.as_str(), code), ("b25lCnR3bwo=\n", 0));
}

#[tokio::test]
async fn head_bytes_applies_to_every_file() {
    let (out, err, code) = run_with_good("head -c 3 good good").await;
    assert_eq!(out, "==> good <==\none\n==> good <==\none", "{err}");
    assert_eq!(code, 0);
}

#[tokio::test]
async fn sort_missing_operand_exits_two_with_no_output() {
    let (out, err, code) = run_with_good("sort good nosuch good").await;
    assert_eq!(out, "");
    assert!(err.contains("sort: nosuch"), "{err}");
    assert_eq!(code, 2);
}
