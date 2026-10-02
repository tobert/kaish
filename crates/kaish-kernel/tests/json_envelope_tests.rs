//! `--json` failure envelope: a non-zero exit under `--json` always prints
//! `{"code":N,"error":"..."}` on stdout. Success prints the data unwrapped.
//! Apps check the exit code first, then `error`.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;
use std::time::Duration;

use common::kernel_at;
use kaish_kernel::{Kernel, KernelConfig, OutputLimitConfig};

fn fixture() -> (tempfile::TempDir, Kernel) {
    let dir = tempfile::tempdir().unwrap();
    fs::write(dir.path().join("f"), "a\nb\n").unwrap();
    fs::write(dir.path().join("good"), "good\n").unwrap();
    let kernel = kernel_at(dir.path());
    (dir, kernel)
}

/// Run `script`; return stdout parsed as JSON, the exit code, and stderr.
async fn envelope(kernel: &Kernel, script: &str) -> (serde_json::Value, i64, String) {
    let result = kernel.execute(script).await.expect("execute");
    let out = result.text_out().to_string();
    let json = serde_json::from_str(&out)
        .unwrap_or_else(|e| panic!("`{script}` stdout is not JSON ({e}): {out:?}"));
    (json, result.code, result.err.clone())
}

#[tokio::test]
async fn failure_with_message_is_code_and_error_only() {
    let (_dir, kernel) = fixture();
    let (json, code, err) = envelope(&kernel, "glob '*.xyz' --json").await;
    assert_eq!(code, 1);
    assert_eq!(
        json,
        serde_json::json!({"code": 1, "error": "glob: no matches for pattern '*.xyz'"})
    );
    // The envelope does not move the message off stderr.
    assert!(err.contains("no matches for pattern"), "stderr: {err:?}");
}

#[tokio::test]
async fn failure_without_message_has_empty_error() {
    let (_dir, kernel) = fixture();
    let (json, code, err) = envelope(&kernel, "grep --json nomatch f").await;
    assert_eq!(code, 1);
    assert_eq!(json, serde_json::json!({"code": 1, "error": ""}));
    assert_eq!(err, "");
}

#[tokio::test]
async fn false_special_form_reports_json() {
    let (_dir, kernel) = fixture();
    let (json, code, err) = envelope(&kernel, "false --json").await;
    assert_eq!(code, 1);
    assert_eq!(json, serde_json::json!({"code": 1, "error": ""}));
    assert_eq!(err, "");
}

#[tokio::test]
async fn boolean_special_forms_respect_disabled_and_literal_json_flags() {
    let (_dir, kernel) = fixture();
    for script in ["false --json=false", "false -- --json", "true --json"] {
        let result = kernel.execute(script).await.unwrap();
        assert_eq!(result.text_out(), "", "{script}");
        assert!(result.err.is_empty(), "{script}");
    }
}

#[tokio::test]
async fn failure_keeps_structured_result_under_data() {
    let (_dir, kernel) = fixture();
    let (json, code, _) = envelope(&kernel, "diff --json f good").await;
    assert_eq!(code, 1);
    assert_eq!(json["code"], 1);
    assert_eq!(json["error"], "");
    assert_eq!(json["data"]["differ"], true, "diff result rides under data: {json}");
    assert!(json.get("hunks").is_none(), "diff keys must not sit at top level: {json}");
}

#[tokio::test]
async fn failure_with_a_missing_operand_names_it() {
    let (_dir, kernel) = fixture();
    let (json, code, _) = envelope(&kernel, "cat --json good nosuch").await;
    assert_eq!(code, 1);
    assert_eq!(json["code"], 1);
    assert!(json["error"].as_str().unwrap().contains("nosuch"), "{json}");
}

#[tokio::test]
async fn success_stays_unwrapped() {
    let (_dir, kernel) = fixture();
    let (json, code, _) = envelope(&kernel, "echo hi --json").await;
    assert_eq!(code, 0);
    assert_eq!(json, serde_json::json!("hi\n"));
    let (json, code, _) = envelope(&kernel, "ls --json").await;
    assert_eq!(code, 0);
    assert!(json.is_array(), "{json}");
}

#[tokio::test]
async fn last_pipeline_stage_failure_is_an_envelope() {
    let (_dir, kernel) = fixture();
    let (json, code, _) = envelope(&kernel, "echo hi | grep --json zzz").await;
    assert_eq!(code, 1);
    assert_eq!(json, serde_json::json!({"code": 1, "error": ""}));
}

#[tokio::test]
async fn failed_scatter_workers_keep_rows_under_data() {
    let (_dir, kernel) = fixture();
    let (json, code, _) = envelope(&kernel, "seq 1 2 | scatter | false | gather --json").await;
    assert_eq!(code, 123);
    assert_eq!(json["code"], 123);
    assert_eq!(json["data"].as_array().unwrap().len(), 2);
    assert_eq!(json["data"][0]["code"], 1);
}

#[tokio::test]
async fn scatter_input_failure_is_an_envelope() {
    let (_dir, kernel) = fixture();
    let (json, code, _) = envelope(&kernel, "fromjson '{\"x\":1}' | scatter | echo $ITEM | gather --json").await;
    assert_eq!(code, 1);
    assert_eq!(json["code"], 1);
    assert!(json["error"].as_str().unwrap().contains("scatter"));
}

#[tokio::test]
async fn formatted_pre_scatter_failure_is_not_wrapped_twice() {
    let (_dir, kernel) = fixture();
    let (json, code, _) = envelope(&kernel, "cat --json nosuch | scatter | echo $ITEM | gather --json").await;
    assert_eq!(code, 1);
    assert_eq!(json["code"], 1);
    assert!(json.get("data").is_none(), "no partial data: {json}");
}

#[tokio::test]
async fn failed_line_gather_reports_json() {
    let (_dir, kernel) = fixture();
    let (json, code, _) = envelope(&kernel, "seq 1 2 | scatter | false | gather --lines --json").await;
    assert_eq!(code, 123);
    assert_eq!(json["code"], 123);
    assert!(json.get("data").is_none());
}

#[tokio::test]
async fn failed_gather_redirect_writes_the_envelope() {
    let (dir, kernel) = fixture();
    let result = kernel.execute("seq 1 2 | scatter | false | gather --json > report").await.unwrap();
    assert_eq!(result.code, 123);
    assert_eq!(result.text_out(), "");
    let json: serde_json::Value = serde_json::from_slice(&fs::read(dir.path().join("report")).unwrap()).unwrap();
    assert_eq!(json["code"], 123);
    assert_eq!(json["data"].as_array().unwrap().len(), 2);
}

#[tokio::test]
async fn earlier_pipeline_stage_stays_a_stream() {
    // A stage that feeds another command is data for it, not a report to an app.
    let (_dir, kernel) = fixture();
    let result = kernel.execute("cat --json nosuch | wc -c").await.expect("execute");
    assert_eq!(result.text_out().trim(), "0");
}

#[tokio::test]
async fn command_substitution_captures_the_envelope() {
    let (_dir, kernel) = fixture();
    let result = kernel
        .execute("x=$(grep --json nomatch f); echo \"$x\"")
        .await
        .expect("execute");
    let json: serde_json::Value = serde_json::from_str(result.text_out().trim()).unwrap();
    assert_eq!(json, serde_json::json!({"code": 1, "error": ""}));
}

#[tokio::test]
async fn cancelled_command_is_an_envelope() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = std::sync::Arc::new(kernel_at(dir.path()));
    let canceller = kernel.clone();
    tokio::spawn(async move {
        tokio::time::sleep(Duration::from_millis(300)).await;
        canceller.cancel();
    });
    let (json, code, _) = envelope(&kernel, "sleep 30 --json").await;
    assert_eq!(code, 130);
    assert_eq!(json["code"], 130);
    assert!(json["error"].is_string(), "{json}");
}

/// Output past the limit is cut after formatting: exit 3, `original_code`
/// holds the real status, and stdout is a truncated document, not an envelope.
#[tokio::test]
async fn spill_reports_exit_3_with_the_original_code() {
    let dir = tempfile::tempdir().unwrap();
    let config = KernelConfig::repl()
        .with_cwd(dir.path().to_path_buf())
        .with_output_limit(OutputLimitConfig::agent().in_memory());
    let kernel = Kernel::new(config).unwrap();
    let result = kernel.execute("seq 1 100000 --json").await.unwrap();
    assert_eq!(result.code, 3);
    assert_eq!(result.original_code, Some(0));
    assert!(result.text_out().contains("output truncated"), "{:?}", result.text_out());
}
