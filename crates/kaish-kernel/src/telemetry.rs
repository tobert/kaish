//! Bridge embedder-supplied W3C trace context into kaish's own tracing spans.
//!
//! Embedders hand us `traceparent` / `tracestate` / `baggage` on
//! [`kaish_types::ExecuteOptions`]. [`extract_parent`] parses them with the
//! standard OpenTelemetry W3C propagators and returns an
//! [`opentelemetry::Context`]. The *caller* attaches that context as current
//! **before** the `#[instrument]` execution span is created, so the span
//! parents onto the embedder's remote span automatically.
//!
//! Why return a context instead of mutating a span here: `Span::set_parent`
//! only works while the span is still a builder and rejects an already-entered
//! span (`SetParentError::AlreadyStarted`). An `#[instrument]` async span is
//! already entered inside its own body, so the only correct moment to influence
//! its parent is at span *creation* time — which is what attaching the returned
//! context around the call achieves.

use std::collections::HashMap;
use std::future::Future;

use opentelemetry::baggage::BaggageExt;
use opentelemetry::context::{FutureExt, WithContext};
use opentelemetry::propagation::TextMapPropagator;
use opentelemetry::{Context, KeyValue};
use opentelemetry_sdk::propagation::TraceContextPropagator;

use std::collections::BTreeMap;

use kaish_types::{ExecResult, ExecuteOptions};

/// Build an OpenTelemetry context from the trace fields on `opts`, or `None`
/// when the embedder supplied no trace context at all (nothing to attach).
///
/// `tracestate` is only meaningful alongside a `traceparent` (W3C), so it is
/// dropped when no `traceparent` is present. Baggage is set directly from the
/// map (not round-tripped through a W3C `baggage` header) so identifier values
/// aren't subject to the header's list/percent-encoding rules.
pub(crate) fn extract_parent(opts: &ExecuteOptions) -> Option<Context> {
    if opts.traceparent.is_none() && opts.baggage.is_empty() {
        return None;
    }

    // Base on an *empty* context, never `Context::current()`: the extracted
    // remote span must be the sole parent, never an ambient span that happens
    // to be active on this task.
    let mut cx = Context::new();

    if let Some(traceparent) = &opts.traceparent {
        let mut carrier: HashMap<String, String> = HashMap::new();
        carrier.insert("traceparent".to_string(), traceparent.clone());
        if let Some(tracestate) = &opts.tracestate {
            carrier.insert("tracestate".to_string(), tracestate.clone());
        }
        cx = TraceContextPropagator::new().extract_with_context(&cx, &carrier);
    }

    if !opts.baggage.is_empty() {
        cx = cx.with_baggage(
            opts.baggage
                .iter()
                .map(|(k, v)| KeyValue::new(k.clone(), v.clone())),
        );
    }

    Some(cx)
}

/// Echo embedder-supplied baggage back onto the result on the way out
/// (trace-context *egress*).
///
/// Ingress carries the embedder's W3C baggage *into* execution
/// ([`extract_parent`]); on the way out the same identifiers are merged onto
/// [`ExecResult::baggage`] so the embedder can read a single, complete view of
/// the baggage that rode along with this call. Tool-emitted entries are the
/// freshest, execution-time values, so they **win** on key collision: an
/// embedder entry is only added when no tool already set that key. The
/// embedder already holds its own copy of what it sent, so overwriting a tool
/// value with it would discard information rather than add it.
pub(crate) fn merge_egress_baggage(result: &mut ExecResult, embedder: BTreeMap<String, String>) {
    for (key, value) in embedder {
        result.baggage.entry(key).or_insert(value);
    }
}

/// Re-bind the current OpenTelemetry context onto a future that is about to
/// cross a `tokio::spawn` boundary.
///
/// Spawned tasks start with an empty OTel current-context, so forked work
/// (background jobs, scatter workers, concurrent pipeline stages) would emit
/// spans detached from the embedder's trace. Call this at the spawn site — on
/// the parent task, while the foreground execution span is still entered — so
/// the spawned future re-attaches the captured context on every poll and its
/// spans stay in the trace.
///
/// The context is captured eagerly here (synchronously at the spawn site), not
/// inside the spawned task where it would already be empty. See
/// [`fork_parent_context`] for *which* context is captured and why.
pub(crate) fn bind_current_context<F: Future>(fut: F) -> WithContext<F> {
    fut.with_context(fork_parent_context())
}

/// The OTel context forked work should inherit.
///
/// Prefer the **foreground execution span's** context (read from the active
/// `tracing` span via the `tracing-opentelemetry` bridge) so forked spans nest
/// *under* the execute span rather than becoming siblings of it under the
/// embedder's remote parent. The span's context still carries the embedder's
/// baggage — the bridge stores it as `parent_cx.with_span(span)` — so trace and
/// baggage propagation are both preserved.
///
/// When no `tracing-opentelemetry` layer is installed, `Span::context()`
/// returns an empty default; fall back to the ambient `Context::current()`,
/// which under `run_inner`'s `with_context` is the embedder's remote parent —
/// the previous behaviour, so propagation is never *lost*, only made shallower.
fn fork_parent_context() -> Context {
    use opentelemetry::trace::TraceContextExt;
    use tracing_opentelemetry::OpenTelemetrySpanExt;

    let span_cx = tracing::Span::current().context();
    if span_cx.span().span_context().is_valid() {
        span_cx
    } else {
        Context::current()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use opentelemetry::trace::TraceContextExt;

    // A well-formed sampled traceparent from the W3C spec examples.
    const TRACEPARENT: &str = "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01";
    const TRACE_ID: &str = "4bf92f3577b34da6a3ce929d0e0e4736";
    const SPAN_ID: &str = "00f067aa0ba902b7";

    #[test]
    fn no_trace_context_yields_none() {
        assert!(extract_parent(&ExecuteOptions::new()).is_none());
    }

    #[test]
    fn traceparent_becomes_remote_parent() {
        let opts = ExecuteOptions::new().with_traceparent(TRACEPARENT);
        let cx = extract_parent(&opts).expect("traceparent should yield a context");
        let sc = cx.span().span_context().clone();

        assert_eq!(sc.trace_id().to_string(), TRACE_ID);
        assert_eq!(sc.span_id().to_string(), SPAN_ID);
        assert!(sc.is_sampled(), "01 flag should mark the parent sampled");
        assert!(sc.is_remote(), "an extracted parent must be marked remote");
    }

    #[test]
    fn tracestate_rides_with_traceparent() {
        let opts = ExecuteOptions::new()
            .with_traceparent(TRACEPARENT)
            .with_tracestate("vendor=opaque");
        let cx = extract_parent(&opts).expect("context");
        let header = cx.span().span_context().trace_state().header();
        assert_eq!(header, "vendor=opaque");
    }

    #[test]
    fn tracestate_without_traceparent_is_dropped() {
        // No traceparent and no baggage: there is nothing to parent onto, so
        // tracestate alone must not conjure a context (W3C: tracestate is
        // meaningless without traceparent).
        let opts = ExecuteOptions::new().with_tracestate("vendor=opaque");
        assert!(extract_parent(&opts).is_none());
    }

    #[test]
    fn baggage_is_carried() {
        let opts = ExecuteOptions::new()
            .with_baggage_entry("owner", "atobey")
            .with_baggage_entry("conn", "embedded-1");
        let cx = extract_parent(&opts).expect("baggage should yield a context");

        assert_eq!(cx.baggage().get("owner").map(|v| v.as_str()), Some("atobey"));
        assert_eq!(
            cx.baggage().get("conn").map(|v| v.as_str()),
            Some("embedded-1")
        );
    }

    #[test]
    fn baggage_without_trace_still_produces_context() {
        // Baggage is independent of a trace: identifiers should propagate even
        // when the embedder has no active span to parent onto.
        let opts = ExecuteOptions::new().with_baggage_entry("tenant", "acme");
        let cx = extract_parent(&opts).expect("context");
        assert!(!cx.span().span_context().is_valid());
        assert_eq!(cx.baggage().get("tenant").map(|v| v.as_str()), Some("acme"));
    }

    #[test]
    fn traceparent_and_baggage_coexist() {
        let opts = ExecuteOptions::new()
            .with_traceparent(TRACEPARENT)
            .with_baggage_entry("owner", "atobey");
        let cx = extract_parent(&opts).expect("context");

        assert_eq!(cx.span().span_context().trace_id().to_string(), TRACE_ID);
        assert_eq!(cx.baggage().get("owner").map(|v| v.as_str()), Some("atobey"));
    }

    fn embedder_baggage(pairs: &[(&str, &str)]) -> BTreeMap<String, String> {
        pairs
            .iter()
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect()
    }

    #[test]
    fn egress_echoes_embedder_baggage_when_no_tool_set_it() {
        let mut result = ExecResult::success("ok");
        merge_egress_baggage(&mut result, embedder_baggage(&[("owner", "atobey")]));
        assert_eq!(result.baggage.get("owner").map(String::as_str), Some("atobey"));
    }

    #[test]
    fn egress_tool_emitted_baggage_wins_on_collision() {
        // A tool already emitted `owner` during execution; the embedder also
        // sent `owner`. The tool's execution-time value must survive.
        let mut result = ExecResult::success("ok");
        result.baggage.insert("owner".to_string(), "tool-value".to_string());
        merge_egress_baggage(
            &mut result,
            embedder_baggage(&[("owner", "embedder-value"), ("tenant", "acme")]),
        );
        assert_eq!(
            result.baggage.get("owner").map(String::as_str),
            Some("tool-value"),
            "tool-emitted baggage must win on key collision",
        );
        assert_eq!(
            result.baggage.get("tenant").map(String::as_str),
            Some("acme"),
            "non-colliding embedder keys are still echoed back",
        );
    }

    #[test]
    fn egress_empty_embedder_baggage_is_a_noop() {
        let mut result = ExecResult::success("ok");
        result.baggage.insert("owner".to_string(), "tool-value".to_string());
        merge_egress_baggage(&mut result, BTreeMap::new());
        assert_eq!(result.baggage.len(), 1);
        assert_eq!(result.baggage.get("owner").map(String::as_str), Some("tool-value"));
    }
}
