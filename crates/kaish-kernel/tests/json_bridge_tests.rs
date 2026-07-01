//! End-to-end tests for the JSON ingress/egress bridge: `fromjson` / `tojson`.
//!
//! These drive real command strings through `kernel.execute()` (never a
//! builtin's `.execute()` directly) so the full dispatch chain runs, and they
//! use `KernelConfig::isolated()` — no localfs — because the pair is pure data
//! and must work in every capability build.
//!
//! The load-bearing guarantees under test (see `docs/arrays-and-hashes.md`):
//! - **Envelope-free**: an envelope-shaped object from external JSON stays a
//!   record, never silently becomes bytes.
//! - **One document, one value**: empty input and trailing garbage are loud
//!   errors, never a silent null.
//! - **Roundtrip law**: `fromjson "$(tojson $x)"` reproduces `$x` structurally.

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

// ── fromjson: parsing ──────────────────────────────────────────────────────

#[tokio::test]
async fn fromjson_parses_object_preserving_key_order() {
    let k = setup().await;
    let (out, code, _) = run(&k, r#"fromjson '{"name":"amy","age":40}'"#).await;
    assert_eq!(code, 0);
    // preserve_order is on workspace-wide, so key order survives.
    assert_eq!(out, r#"{"name":"amy","age":40}"#);
}

#[tokio::test]
async fn fromjson_unwraps_scalars() {
    let k = setup().await;
    assert_eq!(run(&k, "fromjson 42").await.0, "42");
    assert_eq!(run(&k, "fromjson true").await.0, "true");
    assert_eq!(run(&k, "fromjson null").await.0, "null");
    // A JSON string round-trips as a quoted JSON string.
    assert_eq!(run(&k, r#"fromjson '"hi"'"#).await.0, r#""hi""#);
}

#[tokio::test]
async fn fromjson_reads_stdin() {
    let k = setup().await;
    let (out, code, _) = run(&k, r#"echo '{"a":1}' | fromjson"#).await;
    assert_eq!(code, 0);
    assert_eq!(out, r#"{"a":1}"#);
}

// ── fromjson: loud errors, never a silent null ─────────────────────────────

#[tokio::test]
async fn fromjson_empty_input_is_loud() {
    let k = setup().await;
    let (_, code, err) = run(&k, "fromjson ''").await;
    assert_ne!(code, 0);
    assert!(err.contains("empty"), "err: {err}");
}

#[tokio::test]
async fn fromjson_trailing_garbage_is_loud_with_position() {
    let k = setup().await;
    let (_, code, err) = run(&k, r#"fromjson '{"a":1} extra'"#).await;
    assert_ne!(code, 0);
    assert!(err.contains("invalid JSON"), "err: {err}");
    // serde_json's Display carries the line/column of the failure.
    assert!(err.contains("line") && err.contains("column"), "err: {err}");
}

#[tokio::test]
async fn fromjson_malformed_is_loud() {
    let k = setup().await;
    let (_, code, err) = run(&k, "fromjson '{not json}'").await;
    assert_ne!(code, 0);
    assert!(err.contains("invalid JSON"), "err: {err}");
}

#[tokio::test]
async fn fromjson_no_input_is_loud() {
    let k = setup().await;
    let (_, code, err) = run(&k, "fromjson").await;
    assert_ne!(code, 0);
    assert!(err.contains("no input"), "err: {err}");
}

// ── The envelope-free hazard ───────────────────────────────────────────────

#[tokio::test]
async fn fromjson_never_decodes_a_bytes_envelope() {
    // External JSON shaped exactly like the internal base64 byte envelope must
    // NOT silently become Value::Bytes. If it did, `tojson` would reject it as
    // binary; because it stays a record, `tojson` re-serializes the object.
    let k = setup().await;
    let (out, code, err) = run(
        &k,
        r#"x=$(fromjson '{"_type":"bytes","encoding":"base64","data":"AQID","len":3}'); tojson $x"#,
    )
    .await;
    assert_eq!(code, 0, "envelope-shaped object should stay a record; err: {err}");
    assert!(out.contains(r#""_type""#), "out: {out}");
    assert!(out.contains(r#""bytes""#), "out: {out}");
}

// ── tojson: serialize ──────────────────────────────────────────────────────

#[tokio::test]
async fn tojson_serializes_a_captured_value() {
    let k = setup().await;
    let (out, code, _) = run(&k, r#"x=$(fromjson '{"name":"amy"}'); tojson $x"#).await;
    assert_eq!(code, 0);
    assert_eq!(out, r#"{"name":"amy"}"#);
}

#[tokio::test]
async fn tojson_scalar_is_valid_json() {
    let k = setup().await;
    assert_eq!(run(&k, "tojson hello").await.0, r#""hello""#);
}

#[tokio::test]
async fn tojson_pretty_indents() {
    let k = setup().await;
    let (out, code, _) = run(&k, r#"x=$(fromjson '{"a":1}'); tojson --pretty $x"#).await;
    assert_eq!(code, 0);
    assert!(out.contains("\n"), "pretty output should have newlines: {out}");
    assert!(out.contains("  \"a\""), "pretty output should indent: {out}");
}

#[tokio::test]
async fn tojson_no_value_is_loud() {
    let k = setup().await;
    let (_, code, err) = run(&k, "tojson").await;
    assert_ne!(code, 0);
    assert!(err.contains("no value"), "err: {err}");
}

#[tokio::test]
async fn tojson_refuses_binary() {
    // `/w==` decodes to a single 0xFF byte — invalid UTF-8, so base64 -d yields
    // a Bytes value. tojson must refuse it loudly, not emit a base64 envelope.
    let k = setup().await;
    let (_, code, err) = run(&k, "x=$(echo '/w==' | base64 -d); tojson $x").await;
    assert_ne!(code, 0);
    assert!(err.contains("bytes") || err.contains("binary"), "err: {err}");
}

// ── The roundtrip law ──────────────────────────────────────────────────────

#[tokio::test]
async fn roundtrip_law_holds_structurally() {
    let k = setup().await;
    // fromjson -> tojson reproduces the input document (top-level, order kept).
    let (out, code, _) =
        run(&k, r#"x=$(fromjson '{"n":3,"tags":["a","b"]}'); tojson $x"#).await;
    assert_eq!(code, 0);
    assert_eq!(out, r#"{"n":3,"tags":["a","b"]}"#);

    // fromjson(tojson(x)) == x, pinned by serializing both and comparing text.
    let (rt, code, _) = run(
        &k,
        r#"x=$(fromjson '{"n":3,"tags":["a","b"]}'); y=$(fromjson "$(tojson $x)"); tojson $y"#,
    )
    .await;
    assert_eq!(code, 0);
    assert_eq!(rt, r#"{"n":3,"tags":["a","b"]}"#);
}
