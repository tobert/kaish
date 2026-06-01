//! End-to-end checks that embedder-supplied W3C trace context on
//! `ExecuteOptions` reaches kaish's emitted spans — both the foreground
//! execution span and spans from forked work (scatter workers).
//!
//! These guard two pieces of wiring:
//!   1. `Kernel::run_inner` attaching the extracted OTel context so the
//!      `#[instrument]` execution span parents onto the embedder's trace
//!      (`set_parent` would be too late — it rejects an entered span).
//!   2. `telemetry::bind_current_context` carrying that context across the
//!      `tokio::spawn` boundary into forked tasks, which otherwise start with
//!      an empty OTel current-context and would emit orphan-root spans.
//!
//! Both tests share one process-global subscriber, so they live in a single
//! test function (a second `set_global_default` would panic).

use kaish_kernel::Kernel;
use kaish_types::ExecuteOptions;

use opentelemetry::trace::TracerProvider as _;
use opentelemetry_sdk::trace::{InMemorySpanExporter, SdkTracerProvider};
use tracing_subscriber::prelude::*;

// A well-formed, sampled (`-01`) traceparent from the W3C spec examples.
const TRACEPARENT: &str = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01";
const TRACE_ID: &str = "4bf92f3577b34da6a3ce929d0e0e4736";
const PARENT_SPAN_ID: &str = "00f067aa0ba902b7";

#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn embedder_trace_context_reaches_foreground_and_forked_spans() {
    // In-memory OTel pipeline: a simple exporter (synchronous on span end) plus
    // the tracing-opentelemetry bridge layer installed as the global subscriber.
    let exporter = InMemorySpanExporter::default();
    let provider = SdkTracerProvider::builder()
        .with_simple_exporter(exporter.clone())
        .build();
    let tracer = provider.tracer("kaish-kernel-test");
    let subscriber =
        tracing_subscriber::registry().with(tracing_opentelemetry::layer().with_tracer(tracer));
    tracing::subscriber::set_global_default(subscriber).expect("install global subscriber once");

    let kernel = Kernel::transient().expect("build transient kernel");

    // 1. Foreground execution span parents onto the embedder's traceparent.
    let opts = ExecuteOptions::new()
        .with_traceparent(TRACEPARENT)
        .with_baggage_entry("owner", "atobey");
    let result = kernel
        .execute_with_options("true", opts)
        .await
        .expect("execute should succeed");
    assert_eq!(result.code, 0, "`true` should exit 0");
    // Egress: embedder baggage is echoed back onto the result so the embedder
    // can read a complete view of the identifiers that rode along with the call.
    assert_eq!(
        result.baggage.get("owner").map(String::as_str),
        Some("atobey"),
        "embedder baggage must be echoed back onto ExecResult.baggage",
    );

    // 2. Forked scatter workers stay in the same trace. `gather` blocks until
    //    every worker finishes, so the worker spans are flushed by the time
    //    this returns. Each worker runs in its own `tokio::spawn`ed task.
    let scatter = kernel
        .execute_with_options(
            r#"seq 1 3 | scatter | echo "$ITEM" | gather"#,
            ExecuteOptions::new().with_traceparent(TRACEPARENT),
        )
        .await
        .expect("scatter should succeed");
    assert_eq!(scatter.code, 0, "scatter/gather should exit 0");

    provider.force_flush().expect("flush spans");
    let spans = exporter.get_finished_spans().expect("collect finished spans");

    // Foreground execution span: inherits the embedder's trace and parents
    // directly onto the embedder's span id.
    let exec = spans
        .iter()
        .find(|s| s.name.as_ref() == "execute_with_options_inner")
        .expect("the kernel execution span should have been exported");
    assert_eq!(
        exec.span_context.trace_id().to_string(),
        TRACE_ID,
        "execution span must inherit the embedder's trace id",
    );
    assert_eq!(
        exec.parent_span_id.to_string(),
        PARENT_SPAN_ID,
        "execution span must parent directly onto the embedder's span id",
    );

    // Forked worker spans: present, and every one shares the embedder's trace.
    let worker_spans: Vec<_> = spans
        .iter()
        .filter(|s| s.name.as_ref() == "scatter_worker")
        .collect();
    assert!(
        !worker_spans.is_empty(),
        "expected scatter_worker spans to be exported",
    );
    // Every span kaish emitted in this trace, by span id — a worker that nests
    // under a kaish span parents onto one of these, not onto the remote parent.
    let local_span_ids: std::collections::HashSet<String> = spans
        .iter()
        .map(|s| s.span_context.span_id().to_string())
        .collect();
    for worker in &worker_spans {
        assert_eq!(
            worker.span_context.trace_id().to_string(),
            TRACE_ID,
            "forked scatter worker span must stay in the embedder's trace",
        );
        assert!(
            worker.span_context.is_sampled(),
            "worker span should inherit the sampled decision from the remote parent",
        );
        // Nesting: workers parent under a kaish-local span (the foreground
        // execution span or a descendant), NOT directly onto the embedder's
        // remote span — `bind_current_context` captures the active execute
        // span's context rather than the ambient remote parent.
        assert_ne!(
            worker.parent_span_id.to_string(),
            PARENT_SPAN_ID,
            "forked worker must nest under a kaish span, not the remote parent",
        );
        assert!(
            local_span_ids.contains(&worker.parent_span_id.to_string()),
            "worker's parent must be one of kaish's own exported spans",
        );
    }
}
