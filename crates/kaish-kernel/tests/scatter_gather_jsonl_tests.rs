//! Kernel-routed tests for the scatter/gather typed-JSONL contract (GH #73).
//!
//! Ratified 2026-07-03 after a cross-model panel (docs/designing-syntax-with-llms
//! method; raw data linked from the issue): typed `ITEM` bindings, JSONL result
//! records (one per worker, item order, failures included), A′ exit codes
//! (0 / 123 any-failure / 2 usage), `--lines` as the loud raw-text escape
//! hatch, `--json` as the one-array view. Driven through `kernel.execute()` so
//! the real pipeline split, forked workers, and flag binding all run.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use tempfile::tempdir;

use common::kernel_at;
use kaish_kernel::Kernel;

/// Run a script, returning the full ExecResult (rows live in text, code
/// matters, err carries the failure summary).
async fn run_full(k: &Kernel, script: &str) -> kaish_kernel::interpreter::ExecResult {
    k.execute(script).await.expect("kernel execute")
}

fn rows(text: &str) -> Vec<serde_json::Value> {
    text.lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(l).expect("each line is one JSON record"))
        .collect()
}

// ═══════════════════════════════════════════════════════════════════════
// JSONL rows: schema, order, failures included
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn rows_carry_schema_in_item_order() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(&k, "seq 1 3 | scatter --as N | echo $N | gather").await;
    assert_eq!(r.code, 0, "all ok → 0: {:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows.len(), 3);
    for (i, row) in rows.iter().enumerate() {
        assert_eq!(row["i"], i, "item order");
        assert_eq!(row["ok"], true);
        assert_eq!(row["code"], 0);
        assert!(row.get("err").is_some(), "err always present");
        assert!(row.get("timed_out").is_none(), "omit-false");
    }
    assert_eq!(rows[0]["out"], "1");
    assert_eq!(rows[2]["out"], "3");
}

#[tokio::test]
async fn failed_worker_is_a_row_not_a_drop() {
    let k = kernel_at(tempdir().unwrap().path());
    // Worker segment is a pipeline: grep matches only item 2, so workers 1
    // and 3 exit 1 (selective failure without any control flow).
    let r = run_full(
        &k,
        r#"seq 1 3 | scatter --as N | echo $N | grep 2 | gather"#,
    )
    .await;
    assert_eq!(r.code, 123, "any failure → 123: {:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows.len(), 3, "every worker gets a row, failures included");
    assert_eq!(rows[0]["ok"], false);
    assert_eq!(rows[1]["ok"], true, "item 2 matched: {rows:?}");
    assert_eq!(rows[2]["ok"], false);
    assert!(r.err.contains("2 of 3"), "err summarizes: {}", r.err);
}

#[tokio::test]
async fn typed_record_items_subscript_and_ride_rows_typed() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"jobs=$(fromjson '[{"id":1,"host":"web1"},{"id":2,"host":"db1"}]')
values $jobs | scatter | echo "${ITEM[host]}" | gather"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows[0]["out"], "web1", "${{ITEM[host]}} subscripted the record");
    assert_eq!(rows[1]["item"]["id"], 2, "row item is the TYPED record");
}

#[tokio::test]
async fn number_and_string_items_stay_distinct() {
    // The conflation the redesign exists to kill: 1 and "1" fan out as
    // different types, observable via typeof in the worker AND the row item.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"mixed=$(fromjson '[1, "1"]')
values $mixed | scatter | typeof $ITEM | gather"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows[0]["out"], "number", "number 1 arrives as a number");
    assert_eq!(rows[1]["out"], "string", "string \"1\" arrives as a string");
    assert!(rows[0]["item"].is_number());
    assert!(rows[1]["item"].is_string());
}

#[tokio::test]
async fn worker_structured_data_rides_the_row() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"seq 1 1 | scatter | fromjson '{"k": 7}' | gather"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows[0]["data"]["k"], 7, "worker .data lands typed: {rows:?}");
}

// ═══════════════════════════════════════════════════════════════════════
// The three views: JSONL text, --json array, typed .data iteration
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn json_flag_renders_one_array() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(&k, "seq 1 2 | scatter | echo $ITEM | gather --json").await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    let doc: serde_json::Value =
        serde_json::from_str(&r.text_out()).expect("one JSON document");
    let arr = doc.as_array().expect("array");
    assert_eq!(arr.len(), 2);
    assert_eq!(arr[0]["out"], "1");
}

#[tokio::test]
async fn for_loop_iterates_typed_records() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"for r in $(seq 1 3 | scatter | echo $ITEM | gather); do
  if [[ ${r[ok]} == true ]]; then echo "row-${r[i]}:${r[out]}"; fi
done"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    assert_eq!(r.text_out().trim(), "row-0:1\nrow-1:2\nrow-2:3");
}

#[tokio::test]
async fn jq_streams_rows_with_dot_brackets() {
    // kaish jq receives the records as ONE ARRAY — `.[]` streams them (the
    // blessed idiom in help scatter; differs from real-jq-over-JSONL).
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"seq 1 3 | scatter | echo $ITEM | gather | jq -r '.[] | select(.ok) | .out'"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    assert_eq!(r.text_out().trim(), "1\n2\n3");
}

#[tokio::test]
async fn post_gather_stage_sees_typed_rows() {
    // A post-gather pipeline stage gets the rows as structured stdin.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"seq 1 2 | scatter | echo $ITEM | gather | jq '[.[] | .i] | length'"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    assert_eq!(r.text_out().trim(), "2");
}

// ═══════════════════════════════════════════════════════════════════════
// --lines: the loud raw-text escape hatch
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn lines_mode_emits_raw_outputs_in_item_order() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(&k, "seq 1 3 | scatter | echo $ITEM | gather --lines").await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    assert_eq!(r.text_out().trim(), "1\n2\n3", "bare outputs, no JSON");
}

#[tokio::test]
async fn lines_mode_hard_errors_on_any_failure() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"seq 1 2 | scatter --as N | echo $N | grep 2 | gather --lines"#,
    )
    .await;
    assert_eq!(r.code, 123);
    assert!(r.text_out().is_empty(), "no partial text: {:?}", r.text_out());
    assert!(r.err.contains("--lines"), "hints the fix: {}", r.err);
}

// ═══════════════════════════════════════════════════════════════════════
// Ingress guards
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn single_object_input_is_loud_with_hint() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"fromjson '{"jobs":[1,2]}' | scatter | echo $ITEM | gather"#,
    )
    .await;
    assert_ne!(r.code, 0);
    assert!(r.err.contains("jq '.jobs'"), "hints the array key: {}", r.err);
}

#[tokio::test]
async fn null_item_is_loud() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"fromjson '[1, null, 3]' | scatter | echo $ITEM | gather"#,
    )
    .await;
    assert_ne!(r.code, 0);
    assert!(r.err.contains("null"), "{}", r.err);
}

#[tokio::test]
async fn blank_text_lines_are_skipped() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(&k, r#"printf 'a\n\nb\n' | scatter | echo $ITEM | gather"#).await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    assert_eq!(rows(&r.text_out()).len(), 2, "no worker for the blank line");
}

#[tokio::test]
async fn empty_input_is_vacuous_success() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(&k, r#"printf '' | scatter | echo $ITEM | gather"#).await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    assert!(r.text_out().is_empty(), "no rows");
}

// ═══════════════════════════════════════════════════════════════════════
// Timeout rows
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn timed_out_worker_reports_124_and_gather_123() {
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        "seq 1 1 | scatter --timeout 200ms | sleep 5 | gather",
    )
    .await;
    assert_eq!(r.code, 123, "timeout counts as failure: {:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows[0]["code"], 124, "timeout(1) prior");
    assert_eq!(rows[0]["ok"], false);
    assert_eq!(rows[0]["timed_out"], true);
}
