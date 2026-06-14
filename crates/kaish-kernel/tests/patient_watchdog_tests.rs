//! Tests for the per-builtin "patient" watchdog seam (`ToolCtx::patient`).
//!
//! A model-backed builtin (provider calls that legitimately run minutes) needs
//! a budget separate from the script timeout: while a `PatientGuard` is held,
//! the script clock freezes and the hold's own budget governs; on drop the
//! script clock resumes with the remaining time it had at acquire. Cancellation
//! stays live throughout — only the timer pauses, never the cancel surface.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{
    ExecContext, ExecuteOptions, Kernel, KernelBackend, KernelConfig, LocalBackend, Tool,
};
use kaish_types::{ExecResult, Value};

/// A model-call stand-in: `patient-sleep <sleep_ms> <budget_ms>` sleeps under
/// a patient budget, honoring cancellation like a well-behaved builtin must.
struct PatientSleep;

fn arg_ms(args: &ToolArgs, index: usize) -> u64 {
    match args.get_positional(index) {
        Some(Value::Int(i)) => u64::try_from(*i).unwrap_or(0),
        Some(Value::String(s)) => s.parse().unwrap_or(0),
        other => panic!("patient-sleep: bad positional {index}: {other:?}"),
    }
}

#[async_trait]
impl Tool for PatientSleep {
    fn name(&self) -> &str {
        "patient-sleep"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("patient-sleep", "test tool: sleep under a patient budget")
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let sleep_ms = arg_ms(&args, 0);
        let budget_ms = arg_ms(&args, 1);

        let Some(exec_ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "patient-sleep requires ExecContext");
        };
        let cancel = exec_ctx.cancel.clone();

        // The seam under test: suspend the script watchdog while we wait on
        // the "provider", bounded by our own budget instead.
        let _guard = ctx.patient(Duration::from_millis(budget_ms));

        tokio::select! {
            _ = tokio::time::sleep(Duration::from_millis(sleep_ms)) => {
                ExecResult::success("patient-sleep: done")
            }
            _ = cancel.cancelled() => {
                ExecResult::failure(130, "patient-sleep: interrupted")
            }
        }
    }
}

/// Kernel with the patient-sleep tool registered, no host filesystem.
fn patient_kernel() -> Arc<Kernel> {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |tools| {
        tools.register(PatientSleep);
    })
    .expect("with_backend kernel")
    .into_arc()
}

/// A patient builtin that sleeps past the script timeout but under its own
/// budget completes — the script clock is frozen while the guard is held.
#[tokio::test]
async fn patient_tool_survives_script_timeout() {
    let kernel = patient_kernel();
    let result = kernel
        .execute_with_options(
            "patient-sleep 500 5000",
            ExecuteOptions::new().with_timeout(Duration::from_millis(150)),
        )
        .await
        .expect("execute");
    assert_eq!(
        result.code, 0,
        "patient sleep under budget must complete; got code={} err={}",
        result.code, result.err
    );
}

/// A pure-script spin still dies 124 at the script budget — script code has
/// no path to the patient guard, so the script-level budget keeps its teeth.
#[tokio::test]
async fn script_spin_still_dies_at_script_budget() {
    let kernel = patient_kernel();
    let result = kernel
        .execute_with_options(
            "sleep 10",
            ExecuteOptions::new().with_timeout(Duration::from_millis(150)),
        )
        .await
        .expect("execute");
    assert_eq!(result.code, 124, "expected 124, got code={} err={}", result.code, result.err);
}

/// A patient hold is bounded by its own budget: overrunning it fires the
/// watchdog (124), so a hung provider call cannot wait forever.
#[tokio::test]
async fn patient_budget_overrun_dies_124() {
    let kernel = patient_kernel();
    let started = std::time::Instant::now();
    let result = kernel
        .execute_with_options(
            "patient-sleep 30000 300",
            ExecuteOptions::new().with_timeout(Duration::from_millis(150)),
        )
        .await
        .expect("execute");
    assert_eq!(result.code, 124, "expected 124, got code={} err={}", result.code, result.err);
    assert!(
        started.elapsed() < Duration::from_secs(5),
        "overrun must fire at the patient budget, not the full sleep; took {:?}",
        started.elapsed()
    );
}

/// On guard drop the script clock resumes with the remaining time it had at
/// acquire — a subsequent spin past that remainder still dies 124.
#[tokio::test]
async fn script_clock_resumes_after_patient_hold() {
    let kernel = patient_kernel();
    let started = std::time::Instant::now();
    let result = kernel
        .execute_with_options(
            "patient-sleep 400 5000\nsleep 10",
            ExecuteOptions::new().with_timeout(Duration::from_millis(300)),
        )
        .await
        .expect("execute");
    assert_eq!(
        result.code, 124,
        "script clock must resume after the hold; got code={} err={}",
        result.code, result.err
    );
    assert!(
        started.elapsed() < Duration::from_secs(5),
        "resumed clock should fire at the leftover script budget; took {:?}",
        started.elapsed()
    );
}

/// After a patient hold, leftover script budget is enough for quick work —
/// the hold must not consume script time.
#[tokio::test]
async fn quick_work_after_patient_hold_succeeds() {
    let kernel = patient_kernel();
    let result = kernel
        .execute_with_options(
            "patient-sleep 400 5000\necho ok",
            ExecuteOptions::new().with_timeout(Duration::from_millis(300)),
        )
        .await
        .expect("execute");
    assert_eq!(result.code, 0, "got code={} err={}", result.code, result.err);
    assert!(result.text_out().contains("ok"));
}

/// `Kernel::cancel()` during a patient wait still kills promptly —
/// suspension pauses the timer, never the cancel surface.
#[tokio::test]
async fn cancel_during_patient_wait_kills_promptly() {
    let kernel = patient_kernel();
    let runner = kernel.clone();
    let started = std::time::Instant::now();
    let handle = tokio::spawn(async move {
        runner
            .execute_with_options(
                "patient-sleep 30000 60000",
                ExecuteOptions::new().with_timeout(Duration::from_secs(5)),
            )
            .await
    });
    tokio::time::sleep(Duration::from_millis(150)).await;
    kernel.cancel();
    let result = handle.await.expect("join").expect("execute");
    assert_eq!(
        result.code, 130,
        "cancel must interrupt the patient wait; got code={} err={}",
        result.code, result.err
    );
    assert!(
        started.elapsed() < Duration::from_secs(3),
        "cancel during patient wait must be prompt; took {:?}",
        started.elapsed()
    );
}

/// With no script timeout configured there is no watchdog to suspend;
/// the guard is inert and the tool just runs.
#[tokio::test]
async fn patient_is_inert_without_timeout() {
    let kernel = patient_kernel();
    let result = kernel.execute("patient-sleep 50 1000").await.expect("execute");
    assert_eq!(result.code, 0, "got code={} err={}", result.code, result.err);
}
