//! A job whose task panics still ends like a job: both streams close, and the
//! diagnostic it reports as its result reaches its stderr stream.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use async_trait::async_trait;
use kaish_kernel::scheduler::JobId;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{ExecuteOptions, Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::ExecResult;

/// An embedder tool with a bug: it panics.
struct Panicker;

#[async_trait]
impl Tool for Panicker {
    fn name(&self) -> &str {
        "panicker"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("panicker", "test tool: panic")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        panic!("kaish-test-tool-panicked");
    }
}

fn kernel() -> Arc<Kernel> {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |tools| {
        tools.register(Panicker);
    })
    .expect("build kernel")
    .into_arc()
}

async fn assert_panicked_job_ended(kernel: &Kernel, id: JobId) {
    let result = kernel.jobs().wait(id).await.expect("job result");
    assert_eq!(result.code, 1, "{result:?}");
    let streams = kernel.jobs().streams(id).await.expect("job must exist");
    assert!(streams.stdout.is_closed().await, "stdout must close when the task panics");
    assert!(streams.stderr.is_closed().await, "stderr must close when the task panics");
    let stderr = String::from_utf8(streams.stderr.read().await).expect("utf-8");
    assert!(stderr.contains("panic"), "the diagnostic must reach the stream: {stderr:?}");
    assert_eq!(stderr, result.err, "the stream must hold the job's result.err");
}

#[tokio::test]
async fn a_panicking_command_job_closes_its_streams_with_the_diagnostic() {
    let kernel = kernel();
    kernel.execute("panicker &").await.expect("spawn failed");
    assert_panicked_job_ended(&kernel, JobId(1)).await;
}

#[tokio::test]
async fn a_panicking_program_job_closes_its_streams_with_the_diagnostic() {
    let kernel = kernel();
    let id = kernel
        .execute_background_with_options("panicker", ExecuteOptions::new())
        .await
        .expect("program rejected");
    assert_panicked_job_ended(&kernel, id).await;
}

/// Stderr from statements that ran before the panic stays on the stream,
/// ahead of the diagnostic. A whole-program job writes it through a writer
/// task, which must finish before the streams close.
#[rstest::rstest]
#[case::program(true)]
#[case::command(false)]
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn stderr_before_a_panic_precedes_the_diagnostic(#[case] whole_program: bool) {
    for round in 0..50 {
        let kernel = kernel();
        let id = if whole_program {
            kernel
                .execute_background_with_options("echo kaish-before-panic >&2; panicker", ExecuteOptions::new())
                .await
                .expect("program rejected")
        } else {
            kernel
                .execute("if true; then echo kaish-before-panic >&2; panicker; fi &")
                .await
                .expect("spawn failed");
            JobId(1)
        };
        let result = kernel.jobs().wait(id).await.expect("job result");
        let stderr = String::from_utf8(kernel.jobs().read_stderr(id).await.expect("job")).expect("utf-8");
        assert_eq!(stderr, format!("kaish-before-panic\n{}", result.err), "round {round}");
    }
}
