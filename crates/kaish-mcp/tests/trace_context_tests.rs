//! End-to-end check that the MCP `execute` path forwards a client's W3C trace
//! context (lifted from the request `_meta`) into the kernel, so the kernel's
//! execution span parents onto the client's trace.
//!
//! `trace_from_meta` (the `_meta` → `McpTraceContext` parsing) is unit-tested
//! in the handler module; this test covers the other half — that
//! `execute::execute` actually wires `McpTraceContext` onto `ExecuteOptions`
//! and the kernel honours it across the dedicated execution thread it spawns.

use std::collections::BTreeMap;

use kaish_mcp::server::execute::{execute, ExecuteParams, McpTraceContext};

use opentelemetry::trace::TracerProvider as _;
use opentelemetry_sdk::trace::{InMemorySpanExporter, SdkTracerProvider};
use tracing_subscriber::prelude::*;

const TRACEPARENT: &str = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01";
const TRACE_ID: &str = "4bf92f3577b34da6a3ce929d0e0e4736";
const PARENT_SPAN_ID: &str = "00f067aa0ba902b7";

#[tokio::test(flavor = "multi_thread", worker_threads = 1)]
async fn mcp_execute_forwards_traceparent_into_kernel() {
    let exporter = InMemorySpanExporter::default();
    let provider = SdkTracerProvider::builder()
        .with_simple_exporter(exporter.clone())
        .build();
    let tracer = provider.tracer("kaish-mcp-test");
    let subscriber =
        tracing_subscriber::registry().with(tracing_opentelemetry::layer().with_tracer(tracer));
    tracing::subscriber::set_global_default(subscriber).expect("install global subscriber once");

    let params = ExecuteParams {
        script: "true".to_string(),
        cwd: None,
        env: None,
        timeout_ms: None,
    };
    let mut baggage = BTreeMap::new();
    baggage.insert("owner".to_string(), "atobey".to_string());
    let trace = McpTraceContext {
        traceparent: Some(TRACEPARENT.to_string()),
        tracestate: None,
        baggage,
    };

    let result = execute(params, 30_000, None, &[], trace)
        .await
        .expect("execute should succeed");
    assert_eq!(result.code, 0, "`true` should exit 0");
    // Egress: the client's baggage is echoed back onto the result so it can be
    // surfaced on the response `_meta` (symmetric with the ingress lift).
    assert_eq!(
        result.baggage.get("owner").map(String::as_str),
        Some("atobey"),
        "client baggage must round-trip back onto the MCP result",
    );

    provider.force_flush().expect("flush spans");
    let spans = exporter.get_finished_spans().expect("collect finished spans");

    let exec = spans
        .iter()
        .find(|s| s.name.as_ref() == "execute_with_options_inner")
        .expect("kernel execution span should have been exported");
    assert_eq!(
        exec.span_context.trace_id().to_string(),
        TRACE_ID,
        "kernel span must inherit the client's trace id forwarded via _meta",
    );
    assert_eq!(
        exec.parent_span_id.to_string(),
        PARENT_SPAN_ID,
        "kernel span must parent onto the client's span id",
    );
}
