//! `jq -R` (raw input): each stdin line is one JSON string; `-s` makes the
//! whole input one string. Reference: jq 1.7.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated())
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

#[tokio::test]
async fn published_slurp_help_names_the_raw_input_exception() {
    let kernel = setup().await;
    let schemas = kernel.tool_schemas();
    let jq = schemas.iter().find(|schema| schema.name == "jq").expect("jq schema");
    let slurp = jq.params.iter().find(|param| param.name == "slurp").expect("slurp");
    assert!(slurp.description.contains("-R"), "{}", slurp.description);
    assert!(slurp.description.contains("string"), "{}", slurp.description);
}

#[tokio::test]
async fn raw_input_keeps_good_lines_and_reports_filter_errors() {
    let (output, error, code) = run(r"printf '1\nbad\n2\n' | jq -R tonumber").await;
    assert_eq!(output, "1\n2\n");
    assert_eq!(code, 1);
    assert!(error.contains("raw input line 2"), "{error}");
}

#[tokio::test]
async fn raw_input_keeps_carriage_returns_like_jq() {
    let (output, error, code) = run(r"printf 'a\r\nb\r\n' | jq -R -c .").await;
    assert_eq!(code, 0, "{error}");
    assert_eq!(output, "\"a\\r\"\n\"b\\r\"\n");
}
