//! Structured `.data` must reach the next pipeline stage. A builtin producer
//! (seq/jq/cut/…) emits a typed `Value` over a sideband oneshot; the consumer
//! used to `try_recv` it once at startup and, finding it not-yet-sent, fall back
//! to parsing the pipe *text* — so `seq 1 3 | jq .` failed with "trailing
//! characters". Regression for `scheduler/pipeline.rs`.
//!
//! These use a multi-thread runtime and loop: the race only loses when the
//! consumer is polled (on another worker) before the producer sends — on a
//! current-thread runtime the producer (spawned first) always wins and hides
//! the bug. Pre-fix this failed ~197/200; the fix makes it deterministic.

#![cfg(feature = "localfs")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

async fn run(script: &str) -> (i64, String) {
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = kernel.execute(script).await.expect("execute");
    (r.code, r.text_out().trim().to_string())
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn seq_into_jq_uses_structured_data() {
    for i in 0..200 {
        let (code, out) = run("seq 1 3 | jq -c .").await;
        assert_eq!(code, 0, "iter {i}: seq | jq should succeed, got out={out:?}");
        assert_eq!(out, "[1,2,3]", "iter {i}: jq should see the structured array, not raw text");
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn cut_into_jq_uses_structured_data() {
    for i in 0..200 {
        let (code, out) = run("seq 1 3 | cut -f1 | jq -c 'length'").await;
        assert_eq!(code, 0, "iter {i}: out={out:?}");
        assert_eq!(out, "3", "iter {i}");
    }
}

// A text producer with no structured data must still work (consumer falls back
// to pipe text) and must not hang or regress.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn text_json_into_jq_still_parses() {
    for i in 0..50 {
        let (code, out) = run("echo '{\"a\":1}' | jq -c '.a'").await;
        assert_eq!(code, 0, "iter {i}: out={out:?}");
        assert_eq!(out, "1", "iter {i}");
    }
}

// scatter also consumes the structured-data sideband (standalone scatter is
// validation-gated — needs a gather — so skip validation to reach the path).
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn seq_into_scatter_sees_structured_items() {
    for i in 0..100 {
        let kernel = Kernel::new(KernelConfig::repl().with_skip_validation(true)).expect("kernel");
        let r = kernel.execute("seq 1 3 | scatter").await.expect("execute");
        assert_eq!(r.code, 0, "iter {i}: out={:?} err={:?}", r.text_out(), r.err);
        assert!(
            r.text_out().contains("3 items"),
            "iter {i}: expected 3 items, got {:?}",
            r.text_out()
        );
    }
}

// --- The sideband survives a nested dispatch in the consuming stage --------
//
// Same assertion as `seq_into_jq_uses_structured_data` above, with one thing
// added to the consuming stage: a `$(…)` in its own argument list, or a
// function body around it. That nested dispatch destroys the structured
// value, and `jq` falls back to parsing the pipe *text* — the exact failure
// the sideband was built to prevent, re-entered through a different door:
//
//     seq 1 3 | jq -c .            → [1,2,3]
//     seq 1 3 | jq -c $(echo .)    → "trailing characters … looks like JSONL"
//
// Same root shape as the `pipe_stdout` loss (see
// `pipeline_nested_dispatch_tests.rs`): a per-invocation IO resource lives in
// the kernel's one shared `exec_ctx` slot, and a nested dispatch takes it and
// does not put it back. `stdin_data`/`stdin_data_rx` are moved *into* a
// command's context at every sync site and never returned, so an outer stage
// that has not consumed them yet loses them to the inner call.
//
// Unlike the `pipe_stdout` loss, this one fails loudly — a wrong answer with
// an error message attached, not silence at exit 0.
//
// Deterministic, so no loop: the race the tests above defend against is a
// different mechanism.

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn a_substitution_in_the_consumer_does_not_eat_the_structured_value() {
    let (code, out) = run("seq 1 3 | jq -c $(echo .)").await;
    assert_eq!(code, 0, "seq | jq $(echo .) should succeed, got out={out:?}");
    assert_eq!(
        out, "[1,2,3]",
        "a `$()` in the consumer's argv must not cost it the structured value"
    );
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn a_quoted_substitution_in_the_consumer_is_the_same_case() {
    let (code, out) = run("seq 1 3 | jq -c \"$(echo .)\"").await;
    assert_eq!(code, 0, "quoted form should succeed, got out={out:?}");
    assert_eq!(out, "[1,2,3]");
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn a_function_body_consumer_keeps_the_structured_value() {
    let (code, out) = run("f() { jq -c .; }; seq 1 3 | f").await;
    assert_eq!(code, 0, "function-body consumer should succeed, got out={out:?}");
    assert_eq!(out, "[1,2,3]");
}

/// The control that says this is about the sideband and not about stdin bytes:
/// a text consumer reads the pipe directly and was never affected.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn a_text_consumer_was_never_affected() {
    let (code, out) = run("seq 1 3 | wc -l").await;
    assert_eq!(code, 0);
    assert_eq!(out, "3");
}
