//! Kernel-routed tests for the JSONL doors (GH #80): `fromjsonl` / `tojsonl`,
//! plus the accompanying jq changes (real `-s`/`--slurp`, JSONL-hint parse
//! errors, and the "always-slurped" runtime-error hint).
//!
//! Driven through `kernel.execute()` (never a builtin's `.execute()`
//! directly) per the project convention — the real dispatch chain (arg
//! binding, `--json`, pipeline `.data` sideband) has to run for these to mean
//! anything. Pure-data tests use `KernelConfig::isolated()` (no localfs, so
//! the pair works in every capability build); the scatter/gather-through-a-
//! file round trip needs `localfs` and is gated accordingly.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated().with_skip_validation(true))
        .expect("failed to create kernel")
        .into_arc()
}

async fn run(k: &Kernel, script: &str) -> (String, i64, String) {
    let r = k.execute(script).await.expect("kernel execute");
    (r.text_out().trim().to_string(), r.code, r.err.clone())
}

// ═══════════════════════════════════════════════════════════════════════
// fromjsonl — JSONL text → typed list
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn fromjsonl_happy_path_typed_list() {
    let k = setup().await;
    // Route the parsed .data straight through tojsonl (rather than a
    // $()-capture + tojson round trip) so this exercises the ingress door
    // end to end as a real pipeline stage.
    let (out, code, err) = run(&k, "printf '{\"a\":1}\\n{\"a\":2}\\n' | fromjsonl | tojsonl").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "{\"a\":1}\n{\"a\":2}");
}

#[tokio::test]
async fn fromjsonl_blank_lines_are_skipped() {
    let k = setup().await;
    let (out, code, err) = run(&k, "printf '1\\n\\n2\\n\\n\\n3\\n' | fromjsonl | tojsonl").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1\n2\n3");
}

#[tokio::test]
async fn fromjsonl_truncated_trailing_line_is_loud_with_line_number() {
    let k = setup().await;
    let (_, code, err) = run(&k, "printf '{\"a\":1}\\n{\"a\":2' | fromjsonl").await;
    assert_ne!(code, 0);
    assert!(err.contains("line 2"), "err: {err}");
}

#[tokio::test]
async fn fromjsonl_garbage_line_is_loud_with_line_number() {
    let k = setup().await;
    let (_, code, err) = run(&k, "printf '1\\nnot json\\n3\\n' | fromjsonl").await;
    assert_ne!(code, 0);
    assert!(err.contains("line 2"), "err: {err}");
    assert!(err.contains("invalid JSON"), "err: {err}");
}

#[tokio::test]
async fn fromjsonl_empty_input_is_empty_list_exit_0() {
    let k = setup().await;
    let (out, code, err) = run(&k, "printf '' | fromjsonl | tojsonl").await;
    assert_eq!(code, 0, "empty input is a legitimate zero-document stream; err: {err}");
    assert_eq!(out, "");
}

#[tokio::test]
async fn fromjsonl_null_element_passes_through() {
    let k = setup().await;
    let (out, code, err) = run(&k, "printf '1\\nnull\\n3\\n' | fromjsonl | tojsonl").await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1\nnull\n3");
}

#[tokio::test]
async fn fromjsonl_pretty_multiline_document_is_loud_with_hint() {
    let k = setup().await;
    let (_, code, err) = run(&k, "printf '{\\n  \"a\": 1\\n}\\n' | fromjsonl").await;
    assert_ne!(code, 0);
    assert!(err.contains("fromjson"), "err should point at fromjson: {err}");
    assert!(err.contains("jq -s"), "err should point at jq -s: {err}");
}

#[tokio::test]
async fn fromjsonl_no_input_at_all_is_a_usage_error() {
    // Distinct from an empty pipe: no stdin connected whatsoever.
    let k = setup().await;
    let (_, code, err) = run(&k, "fromjsonl").await;
    assert_ne!(code, 0);
    assert!(err.contains("no input"), "err: {err}");
}

#[tokio::test]
async fn fromjsonl_accepts_crlf_line_endings() {
    // Windows-flavored JSONL: each line's trailing \r is stripped before the
    // per-line parse (split on \n, then strip one \r), so CRLF input
    // round-trips identically to LF input.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "printf '{\"a\":1}\\r\\n{\"a\":2}\\r\\n' | fromjsonl | tojsonl",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "{\"a\":1}\n{\"a\":2}");
}

// ═══════════════════════════════════════════════════════════════════════
// tojsonl — list → JSONL text
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn tojsonl_list_to_lines() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"xs=$(fromjson '[1,"a",{"b":2}]'); tojsonl $xs"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1\n\"a\"\n{\"b\":2}");
}

#[tokio::test]
async fn tojsonl_non_list_is_loud_with_fix() {
    let k = setup().await;
    let (_, code, err) = run(&k, r#"x=$(fromjson '{"a":1}'); tojsonl $x"#).await;
    assert_ne!(code, 0);
    assert!(err.contains("tojson"), "err should name the fix: {err}");
}

#[tokio::test]
async fn tojsonl_empty_list_is_empty_output() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"xs=$(fromjson '[]'); tojsonl $xs"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "");
}

#[tokio::test]
async fn tojsonl_bytes_is_loud_never_an_envelope() {
    // `/w==` decodes to a single 0xFF byte — invalid UTF-8, so base64 -d
    // yields a Bytes value. tojsonl must refuse it loudly (same law as
    // tojson): a silent base64 envelope here would masquerade as the data.
    let k = setup().await;
    let (out, code, err) = run(&k, "x=$(echo '/w==' | base64 -d); tojsonl $x").await;
    assert_ne!(code, 0);
    assert!(err.contains("binary"), "err: {err}");
    assert!(err.contains("base64"), "err names the escape hatch: {err}");
    assert!(!out.contains("_type"), "no envelope leaked to stdout: {out}");
}

#[tokio::test]
async fn tojsonl_embedded_newline_stays_one_line() {
    // The tool's central safety property: a string element containing a real
    // newline must serialize as exactly ONE output line, with the newline
    // escaped as \n inside the JSON string — otherwise the "one document per
    // line" contract breaks and fromjsonl can't re-read its own output.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"xs=$(fromjson '[{"msg":"line1\nline2"}]'); tojsonl $xs"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"{"msg":"line1\nline2"}"#, "one line, \\n escaped");
    assert_eq!(out.lines().count(), 1, "exactly one output line: {out:?}");

    // And the safety property closes the loop: the output re-ingests.
    let (out2, code, err) = run(
        &k,
        r#"xs=$(fromjson '[{"msg":"line1\nline2"}]'); tojsonl $xs | fromjsonl | tojsonl"#,
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out2, out, "round-trips through fromjsonl unchanged");
}

#[tokio::test]
async fn tojsonl_round_trips_with_fromjsonl() {
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "printf '{\"a\":1}\\n{\"b\":2}\\n' | fromjsonl | tojsonl",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "{\"a\":1}\n{\"b\":2}");
}

// ═══════════════════════════════════════════════════════════════════════
// jq — stays one document, gets louder, real -s
// ═══════════════════════════════════════════════════════════════════════

#[tokio::test]
async fn jq_single_document_text_unchanged() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"echo '{"name":"Alice"}' | jq -r '.name'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "Alice");
}

#[tokio::test]
async fn jq_jsonl_shaped_input_gets_the_hint() {
    let k = setup().await;
    let (_, code, err) = run(
        &k,
        r#"printf '{"a":1}\n{"a":2}\n{"a":3}\n' | jq '.a'"#,
    )
    .await;
    assert_ne!(code, 0);
    assert!(err.contains("JSONL"), "err: {err}");
    assert!(err.contains("3 documents"), "err: {err}");
    assert!(err.contains("fromjsonl"), "err: {err}");
    assert!(err.contains("jq -s"), "err: {err}");
}

#[tokio::test]
async fn jq_garbage_input_stays_the_plain_error() {
    // Not JSONL-shaped — a genuine syntax error should NOT get the JSONL hint.
    let k = setup().await;
    let (_, code, err) = run(&k, "echo 'not valid json at all' | jq '.'").await;
    assert_ne!(code, 0);
    assert!(!err.contains("JSONL"), "garbage should stay plain: {err}");
}

#[tokio::test]
async fn jq_slurp_wraps_a_single_document_in_an_array() {
    // The must-not-diverge case: printf '{"a":1}' | jq -s length == 1 (array
    // length), not 1 as a coincidence of key count — a no-op -s would give
    // the same answer here by accident, so also check a scalar document.
    let k = setup().await;
    let (out, code, err) = run(&k, r#"printf '{"a":1}' | jq -s 'length'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1");

    let (out, code, err) = run(&k, r#"printf '"hello"' | jq -sc '.'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, r#"["hello"]"#, "single scalar doc still wraps: {out}");
}

#[tokio::test]
async fn jq_slurp_collects_a_multi_document_stream() {
    let k = setup().await;
    let (out, code, err) = run(&k, r#"printf '1\n2\n3\n' | jq -s 'length'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3");

    let (out, code, err) = run(&k, r#"printf '1\n2\n3\n' | jq -sc '.'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[1,2,3]");
}

#[tokio::test]
async fn jq_slurp_handles_pretty_printed_multi_doc_stream() {
    // Real-jq framing (whitespace-separated documents) tolerates pretty
    // multi-line documents back to back — this is the `jq -s` escape hatch
    // pointed to by fromjsonl's own pretty-doc error.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        "printf '{\n  \"a\": 1\n}\n{\n  \"a\": 2\n}\n' | jq -sc '[.[] | .a]'",
    )
    .await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[1,2]");
}

#[tokio::test]
async fn jq_slurp_wraps_the_data_path_value_in_an_array_of_one() {
    // GH #93 item 2: `.data` from an upstream builtin is already one
    // structured value (the single "document" real jq would have read from
    // stdin) — `-s` must still wrap *that* value in a one-element array,
    // exactly like the text path always wraps, rather than passing it
    // through untouched. A bare array happens to make `length` ambiguous
    // between "no-op" and "wrap" (both a 3-element array and a 1-element
    // array-of-that-array give different answers), so use a scalar `.data`
    // where the two behaviors are unmistakably different.
    let k = setup().await;
    // fromjson's own ExecResult carries `.data` through the pipe sideband
    // (unlike `echo $var`, which only renders text) — the same mechanism
    // scatter/gather's rows ride.
    let (out, code, err) = run(&k, r#"fromjson '42' | jq -s 'length'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1", "a no-op would error (scalar has no length); wrap gives array length 1");

    let (out, code, err) = run(&k, r#"fromjson '42' | jq -sc '.[0]'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "42", ".[0] unwraps back to the original scalar");
}

#[tokio::test]
async fn jq_slurp_still_wraps_an_array_shaped_data_value() {
    // Same law, but the `.data` value is itself an array: `-s` wraps it one
    // more level (`[1,2,3]` → `[[1,2,3]]`), it does not pass the array
    // through as if already slurped.
    let k = setup().await;
    let (out, code, err) = run(&k, r#"fromjson '[1,2,3]' | jq -sc 'length'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "1", "wrapped: outer array has one element (the inner array)");

    let (out, code, err) = run(&k, r#"fromjson '[1,2,3]' | jq -sc '.[0]'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "[1,2,3]", ".[0] unwraps back to the original array");
}

#[tokio::test]
async fn jq_slurp_on_the_data_path_does_not_affect_plain_jq() {
    // Guard: bare `jq` (no `-s`) on the `.data` path is untouched by this
    // fix — only the `-s`/`--slurp` branch changed.
    let k = setup().await;
    let (out, code, err) = run(&k, r#"fromjson '[1,2,3]' | jq 'length'"#).await;
    assert_eq!(code, 0, "err: {err}");
    assert_eq!(out, "3", "plain jq still sees the array directly, unwrapped");
}

#[tokio::test]
async fn jq_cannot_index_array_gets_the_dot_bracket_hint() {
    // kaish jq is always-slurped, so a real-jq per-row filter like `.foo`
    // against an array input is the #79 watch item this hint targets.
    let k = setup().await;
    let (_, code, err) = run(&k, r#"printf '[1,2,3]' | jq '.foo'"#).await;
    assert_ne!(code, 0);
    assert!(err.contains("cannot index"), "err: {err}");
    assert!(err.contains(".[] | "), "err should hint the fix: {err}");
}

// ═══════════════════════════════════════════════════════════════════════
// End-to-end: gather's own JSONL egress re-ingests as typed scatter input
// ═══════════════════════════════════════════════════════════════════════

mod common;

#[cfg(feature = "localfs")]
mod localfs_tests {
    use tempfile::tempdir;

    use super::common::kernel_at;

    #[tokio::test]
    async fn gather_output_reingests_through_fromjsonl_into_scatter() {
        let dir = tempdir().expect("tempdir");
        let k = kernel_at(dir.path());

        // Fan out three typed records, gather their JSONL rows to a file —
        // the lingua franca #79 made the default egress.
        let r = k
            .execute(
                r#"jobs=$(fromjson '[{"id":1,"host":"web1"},{"id":2,"host":"web2"}]')
values $jobs | scatter | echo "${ITEM[host]}" | gather > out.jsonl"#,
            )
            .await
            .expect("kernel execute");
        assert_eq!(r.code, 0, "gather stage: {:?}", r.err);

        // Re-read the file kaish itself just wrote and re-fan-out typed —
        // ${ITEM[i]}/${ITEM[item][host]} subscript the record kaish's own
        // gather produced, proving the ingress door round-trips kaish's own
        // egress door.
        let r = k
            .execute(
                r#"cat out.jsonl | fromjsonl | scatter | echo "row-${ITEM[i]}:${ITEM[item][host]}:${ITEM[out]}" | gather"#,
            )
            .await
            .expect("kernel execute");
        assert_eq!(r.code, 0, "re-scatter stage: {:?}", r.err);

        let rows: Vec<serde_json::Value> = r
            .text_out()
            .lines()
            .filter(|l| !l.trim().is_empty())
            .map(|l| serde_json::from_str(l).expect("each line is one JSON row"))
            .collect();
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0]["out"], "row-0:web1:web1");
        assert_eq!(rows[1]["out"], "row-1:web2:web2");
    }
}
