//! GH #212: scatter workers must apply the same spill/exit-3 contract the
//! foreground pipeline gets.
//!
//! Before the fix, `run_parallel`'s per-worker spill boundary called
//! `spill_if_needed` directly and never remapped the exit code, so a worker
//! whose output overflowed the (enabled) output limit still carried its
//! original `code == 0`. `gather`'s 0-vs-123 aggregation reads
//! `result.ok()` (i.e. `code == 0`), so a spilled-but-nominally-successful
//! worker silently counted as a SUCCESS row — the exact loud-spill contract
//! foreground commands get (GH #191/#209) never reached scatter workers.
//!
//! Driven through `kernel.execute()` (never a builtin's direct `.execute()`)
//! per AGENTS.md, so the real pipeline split + forked workers run.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

fn rows(text: &str) -> Vec<serde_json::Value> {
    text.lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| serde_json::from_str(l).expect("each line is one JSON record"))
        .collect()
}

#[tokio::test]
async fn spilled_worker_flips_gather_to_failure() {
    // NoLocal (isolated) kernel forces in-memory spill mode automatically —
    // no disk writes in this test (AGENTS.md).
    let kernel = Kernel::new(KernelConfig::isolated()).expect("failed to create kernel");
    // 10, not 4: since GH #250, `pre_scatter` (here `seq 1 3`, 6 bytes: "1\n2\n3\n")
    // is itself spill-checked before scatter runs — a 4-byte limit would trip
    // THAT guard first and short-circuit before any worker ever launches,
    // which is a different test (see `pre_scatter_spill_short_circuits_before_scatter_runs`
    // above). 10 lets the 6-byte pre_scatter output through clean while still
    // being well under every worker's own output.
    kernel.execute("set -o output-limit=10").await.expect("set limit");

    // Every worker's output ("item-N-padding") is well over the shared
    // 10-byte limit, so every worker spills AND (because the same tiny limit
    // also applies to the pipeline's own post-run check) the aggregate
    // JSONL spills too — the outer spill's loud exit-3 wins either way
    // (pre-existing behavior: ANY spill anywhere maps the final code to 3).
    // What GH #212 actually changes is `original_code`: pre-fix, every
    // worker kept its child exit code (0) despite spilling, so
    // `gather_results` saw an all-`ok()` run and reported `code: 0` —
    // the outer spill would then have recorded `original_code: Some(0)`,
    // silently certifying a run that actually failed on every worker as a
    // clean success. Post-fix, each worker's spill remaps ITS OWN code to 3
    // first, so `gather_results` correctly sees an all-failed run and
    // reports `code: 123` — which is what ends up preserved as
    // `original_code` once the outer spill also fires.
    let r = kernel
        .execute(r#"seq 1 3 | scatter --as N | echo "item-$N-padding" | gather"#)
        .await
        .expect("kernel execute");

    // At this tiny limit the aggregate JSONL itself also spills (the pipeline's
    // own post-run check shares the same `output_limit`), so `r.text_out()` is
    // now a head+tail preview, not JSONL rows — `r.original_code` is the
    // observable that distinguishes the fixed behavior from the bug: it's
    // whatever `gather_results` decided BEFORE the outer spill remapped
    // `r.code` to 3 one more time.
    assert_eq!(r.code, 3, "the outer aggregate also spilled at this tiny limit: {:?}", r.err);
    assert_eq!(
        r.original_code,
        Some(123),
        "gather must have honestly aggregated the spilled workers as failures (123), not silently \
         reported success (0) — got original_code={:?}, err={:?}",
        r.original_code,
        r.err
    );
}

// GH #250: `pre_scatter` (any pipeline stage before `scatter`) runs via
// `run_sequential`, which never applies the output-limit spill check or the
// `did_spill` -> exit-3 remap (`output_limit::apply_spill_contract`) that
// #212/#249 gave the parallel workers, the top-level pipeline, and
// background jobs. Before the fix, a `pre_scatter` command whose output
// overflowed the enabled limit kept `code == 0` (`result.ok()` stayed
// `true`), so the `!result.ok()` guard right after `run_sequential` never
// fired and `scatter` fanned out over a truncated/capped preview of the
// input instead of refusing to run at all.
#[tokio::test]
async fn pre_scatter_spill_short_circuits_before_scatter_runs() {
    let kernel = Kernel::new(KernelConfig::isolated()).expect("failed to create kernel");
    kernel.execute("set -o output-limit=4").await.expect("set limit");

    let r = kernel
        .execute(
            r#"echo "way-over-the-four-byte-limit" | scatter --as N | echo "should-never-run-$N" | gather"#,
        )
        .await
        .expect("kernel execute");

    assert_eq!(
        r.code, 3,
        "a spilled pre_scatter result must remap to exit 3 and short-circuit the whole \
         pipeline, matching the contract the worker/background/pipeline surfaces already have: {:?}",
        r.err
    );
    assert!(r.did_spill, "the pre_scatter spill must be visible on the final result: {r:?}");
    assert!(
        !r.text_out().contains("should-never-run"),
        "scatter must never fan out over a pre_scatter result that already spilled: {:?}",
        r.text_out()
    );
}

#[tokio::test]
async fn worker_under_the_limit_still_reports_success() {
    // Companion/negative case: without the limit, gather still reports a
    // clean success — pins that the contract only fires on an actual spill.
    let kernel = Kernel::new(KernelConfig::isolated()).expect("failed to create kernel");

    let r = kernel
        .execute(r#"seq 1 3 | scatter --as N | echo "$N" | gather"#)
        .await
        .expect("kernel execute");

    assert_eq!(r.code, 0, "no limit set, no spill expected: {:?}", r.err);
    let rows = rows(&r.text_out());
    assert_eq!(rows.len(), 3);
    for row in &rows {
        assert_eq!(row["ok"], true, "{row:?}");
        assert_eq!(row["code"], 0, "{row:?}");
    }
}
