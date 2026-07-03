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

// ═══════════════════════════════════════════════════════════════════════
// Reduced sync arg path: `scatter`/`gather`'s OWN flag values (`--as`,
// `--limit`, …) bind through a sync twin of the async arg binder
// (`build_tool_args`/`eval_simple_expr` in `scheduler/pipeline.rs`) because
// it runs once, before any worker forks — it can't recurse back into
// `PipelineRunner::run`. Before this fix that twin discarded a bad or
// subscripted collection access (`PathError`) as a silently-skipped
// flag/arg instead of failing, unlike the four primary eval sites (`echo`,
// assignment, `$(( ))`, `"${…}"`). See docs/issues.md (closed by this fix)
// and CHANGELOG.md.
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn bad_subscript_in_scatter_flag_value_is_a_loud_error() {
    // `--as ${u[nope]}` — a missing key on a record. Before the fix this
    // silently dropped `--as`'s value (falling back to a bare boolean flag
    // and orphaning the bad expression as an unused positional) instead of
    // failing the pipeline.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"u=$(fromjson '{"name":"amy"}')
seq 1 2 | scatter --as ${u[nope]} | echo $N | gather"#,
    )
    .await;
    assert_ne!(r.code, 0, "a bad subscript in a scatter flag value must fail, not silently proceed");
    assert!(r.err.contains("no such key"), "got: {}", r.err);
}

#[tokio::test]
async fn shape_error_in_scatter_flag_value_is_a_loud_error() {
    // An integer index on a record is misuse, not absence — loud either way.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"cfg=$(fromjson '{"port":9000}')
seq 1 2 | scatter --as ${cfg[0]} | echo $N | gather"#,
    )
    .await;
    assert_ne!(r.code, 0, "a shape error in a scatter flag value must fail loud");
    assert!(r.err.contains("record keys are strings"), "got: {}", r.err);
}

#[tokio::test]
async fn length_of_a_missing_subscripted_key_in_scatter_flag_is_loud_not_silent() {
    // `${#u[nope]}` inside an interpolated `--as` value: `eval_string_parts_sync`
    // used to discard the `PathError` from `resolve_length` and push nothing,
    // silently producing a working-but-WRONG `--as n` (the digit just missing)
    // instead of failing on the bad subscript. `echo $n` matches that mangled
    // fallback name on purpose — if the bug regresses, the worker binds fine
    // and this assertion is the only thing that catches it.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"u=$(fromjson '{"tags":["a","b"]}')
seq 1 2 | scatter --as "n${#u[nope]}" | echo $n | gather"#,
    )
    .await;
    assert_ne!(r.code, 0, "a bad ${{#...}} subscript must fail, not silently produce a mangled var name");
}

#[tokio::test]
async fn valid_subscripted_flag_value_still_works() {
    // Happy path: a VALID subscripted access as a scatter option value must
    // keep working through the reduced sync path.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"cfg=$(fromjson '{"varname":"N"}')
seq 1 2 | scatter --as ${cfg[varname]} | echo $N | gather"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows.len(), 2);
    assert_eq!(rows[0]["out"], "1");
    assert_eq!(rows[1]["out"], "2");
}

#[tokio::test]
async fn valid_length_in_scatter_flag_value_resolves_the_real_count() {
    // Happy path for `${#...}`: a valid subscripted length must resolve to
    // the real element count, never a silently-dropped/omitted digit. If the
    // length were swallowed (old bug), the var name would come out as bare
    // "n" and `$n3` would be undefined, failing the pipeline instead of
    // matching here.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"u=$(fromjson '{"tags":["a","b","c"]}')
seq 1 2 | scatter --as "n${#u[tags]}" | echo $n3 | gather"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows[0]["out"], "1", "var name resolved to n3, the real tag count");
    assert_eq!(rows[1]["out"], "2");
}

#[tokio::test]
async fn undefined_root_subscript_in_scatter_flag_value_is_loud() {
    // kaibo review finding (PR #85): `${nope[key]}` where the ROOT is entirely
    // undefined is `PathError::UndefinedRoot`, not `Absence` — the first cut
    // coalesced ALL UndefinedRoot to a skipped flag, so a typo'd root still
    // silently dropped `--as`'s value. Only a BARE undefined var may coalesce;
    // a subscripted path on an undefined root errors, same split
    // `resolve_length` draws.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"seq 1 2 | scatter --as ${nope[key]} | echo $ITEM | gather"#,
    )
    .await;
    assert_ne!(r.code, 0, "typo'd root in a subscripted flag value must fail loud");
    assert!(r.err.contains("undefined variable"), "got: {}", r.err);
}

#[tokio::test]
async fn bare_undefined_var_in_scatter_flag_still_coalesces() {
    // The bash-compatible convention this reduced context keeps: a BARE
    // undefined `$VAR` coalesces (the flag falls back to boolean, `--as`
    // keeps its ITEM default) rather than erroring. Only subscripted paths
    // went loud in the fix above.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"seq 1 2 | scatter --as $KAISH_TEST_UNSET_VAR | echo $ITEM | gather"#,
    )
    .await;
    assert_eq!(r.code, 0, "bare undefined var keeps coalescing: {:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows.len(), 2);
    assert_eq!(rows[0]["out"], "1", "ITEM default binding still in effect");
}

#[tokio::test]
async fn bare_length_bad_subscript_in_scatter_flag_is_loud() {
    // Bare (unquoted whole-token) `${#u[nope]}` parses as `Expr::VarLength`,
    // which used to fall through `eval_simple_expr`'s `_ => None` catch-all —
    // silently dropping the flag and letting the pipeline proceed. Reaching a
    // loud error here proves the new bare `VarLength` arm is wired: if the
    // arm regressed to the catch-all, this pipeline would succeed.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"u=$(fromjson '{"tags":["a","b"]}')
seq 1 2 | scatter --limit ${#u[nope]} | echo $ITEM | gather"#,
    )
    .await;
    assert_ne!(r.code, 0, "bad bare ${{#...}} flag value must fail, not be silently dropped");
    assert!(r.err.contains("no such key"), "got: {}", r.err);
}

#[tokio::test]
async fn bare_length_in_scatter_flag_value_succeeds() {
    // Happy path for the bare `Expr::VarLength` arm: `--limit ${#u[tags]}`
    // resolves to a valid limit (3) and the pipeline runs. Pairs with the
    // loud-error test above, which is what proves the arm is reachable.
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"u=$(fromjson '{"tags":["a","b","c"]}')
seq 1 2 | scatter --limit ${#u[tags]} | echo $ITEM | gather"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    assert_eq!(rows(&r.text_out()).len(), 2);
}

#[tokio::test]
async fn bare_default_in_scatter_flag_value_binds_the_var_name() {
    // Bare (unquoted whole-token) `${cfg[name]:-W}` parses as
    // `Expr::VarWithDefault`, which also used to fall through the `_ => None`
    // catch-all. Observable proof the arm is wired: the missing key folds to
    // the default `W`, workers read `$W`, rows come out. If the arm regressed
    // to the catch-all, the flag would drop, the binding would stay ITEM, and
    // the workers' `$W` would fail loud (exit 123).
    let k = kernel_at(tempdir().unwrap().path());
    let r = run_full(
        &k,
        r#"cfg=$(fromjson '{"other":1}')
seq 1 2 | scatter --as ${cfg[name]:-W} | echo $W | gather"#,
    )
    .await;
    assert_eq!(r.code, 0, "{:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows[0]["out"], "1", "default var name W bound the items");
    assert_eq!(rows[1]["out"], "2");
}
