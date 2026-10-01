//! `jq -R` (raw input): each stdin line is one JSON string; `-s` makes the
//! whole input one string. Reference: jq 1.7.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

async fn run(script: &str) -> (String, String, i64) {
    let k = setup().await;
    let r = k.execute(script).await.expect("script ran");
    (r.text_out().into_owned(), r.err.clone(), r.code)
}

#[tokio::test]
async fn raw_input_reads_a_line_as_a_string() {
    let (out, err, code) = run(r#"echo hi | jq -R ."#).await;
    assert_eq!((out.as_str(), code), ("\"hi\"\n", 0), "err: {err}");
}

#[tokio::test]
async fn raw_input_runs_the_filter_once_per_line() {
    let (out, err, code) = run(r#"printf 'a\n\nb' | jq -R -c ."#).await;
    assert_eq!((out.as_str(), code), ("\"a\"\n\"\"\n\"b\"\n", 0), "err: {err}");
}

#[tokio::test]
async fn raw_input_filter_sees_the_string() {
    let (out, err, code) = run(r#"printf 'ab\ncde\n' | jq -R 'length'"#).await;
    assert_eq!((out.as_str(), code), ("2\n3\n", 0), "err: {err}");
}

#[tokio::test]
async fn raw_input_with_raw_output_prints_bare_lines() {
    let (out, err, code) = run(r#"printf 'x y\nz\n' | jq -R -r ."#).await;
    assert_eq!((out.as_str(), code), ("x y\nz\n", 0), "err: {err}");
}

#[tokio::test]
async fn raw_input_slurp_is_one_string() {
    let (out, err, code) = run(r#"printf 'a\nb\n' | jq -R -s -c ."#).await;
    assert_eq!((out.as_str(), code), ("\"a\\nb\\n\"\n", 0), "err: {err}");
}

#[tokio::test]
async fn raw_input_slurp_of_empty_input_is_the_empty_string() {
    let (out, err, code) = run(r#"printf '' | jq -R -s -c ."#).await;
    assert_eq!((out.as_str(), code), ("\"\"\n", 0), "err: {err}");
}

#[tokio::test]
async fn raw_input_combined_short_flags() {
    let (out, err, code) = run(r#"printf 'a\nb\n' | jq -Rs -c 'split("\n")'"#).await;
    assert_eq!((out.as_str(), code), ("[\"a\",\"b\",\"\"]\n", 0), "err: {err}");
}

#[tokio::test]
async fn raw_input_with_null_input_is_refused_with_the_alternative() {
    let (out, err, code) = run(r#"printf 'a\nb\n' | jq -n -R '[inputs]'"#).await;
    assert_eq!(out, "", "no silent output");
    assert_ne!(code, 0);
    assert!(err.contains("-R -s") || err.contains("--raw-input --slurp"), "names the alternative: {err}");
}
