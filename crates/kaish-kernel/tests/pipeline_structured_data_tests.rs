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
