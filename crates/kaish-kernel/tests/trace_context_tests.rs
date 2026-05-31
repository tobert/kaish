//! End-to-end check that embedder-supplied W3C trace context on
//! `ExecuteOptions` parents kaish's execution span onto the embedder's trace.
//!
//! This guards the subtle wiring in `Kernel::run_inner`: the extracted OTel
//! context must be attached as *current* at the instant the `#[instrument]`
//! execution span is created, because `Span::set_parent` rejects an
//! already-entered span. If that ordering ever regresses, the execute span
//! becomes an orphan root with a fresh trace id and these assertions fail.

use kaish_kernel::Kernel;
use kaish_types::ExecuteOptions;

use opentelemetry::trace::TracerProvider as _;
use opentelemetry_sdk::trace::{InMemorySpanExporter, SdkTracerProvider};
use tracing_subscriber::prelude::*;

// A well-formed, sampled (`-01`) traceparent from the W3C spec examples.
const TRACEPARENT: &str = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01";
const TRACE_ID: &str = "4bf92f3577b34da6a3ce929d0e0e4736";
const PARENT_SPAN_ID: &str = "00f067aa0ba902b7";

#[tokio::test(flavor = "multi_thread", worker_threads = 1)]
async fn execute_span_parents_onto_embedder_traceparent() {
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
    let opts = ExecuteOptions::new()
        .with_traceparent(TRACEPARENT)
        .with_baggage_entry("owner", "atobey");
    let result = kernel
        .execute_with_options("true", opts)
        .await
        .expect("execute should succeed");
    assert_eq!(result.code, 0, "`true` should exit 0");

    provider.force_flush().expect("flush spans");
    let spans = exporter.get_finished_spans().expect("collect finished spans");

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
}
