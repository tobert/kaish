//! An embedder tool ends the script by overriding `Tool::execute_flow`;
//! a tool that does not override it never does.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use async_trait::async_trait;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::{ExecResult, ToolFlow};

/// Ends the script with exit status 5 after printing.
struct Stopper;

#[async_trait]
impl Tool for Stopper {
    fn name(&self) -> &str {
        "stopper"
    }
    fn schema(&self) -> ToolSchema {
        ToolSchema::new("stopper", "test fixture: ends the script")
    }
    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        ExecResult::failure(5, "")
    }
    async fn execute_flow(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ToolFlow {
        let mut result = ExecResult::success("bye\n");
        result.code = 5;
        ToolFlow::Exit(result)
    }
}

/// Fails with status 5 and does not override `execute_flow`.
struct Plain;

#[async_trait]
impl Tool for Plain {
    fn name(&self) -> &str {
        "plain"
    }
    fn schema(&self) -> ToolSchema {
        ToolSchema::new("plain", "test fixture: default execute_flow")
    }
    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        let mut result = ExecResult::success("plain\n");
        result.code = 5;
        result
    }
}

fn kernel_with_tools() -> Arc<Kernel> {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |tools| {
        tools.register(Stopper);
        tools.register(Plain);
    })
    .expect("with_backend kernel")
    .into_arc()
}

async fn run(script: &str) -> (String, i64) {
    let result = kernel_with_tools().execute(script).await.expect("execute");
    (result.text_out().to_string(), result.code)
}

#[tokio::test]
async fn tool_returning_exit_ends_the_script() {
    assert_eq!(run("stopper; echo after").await, ("bye\n".to_string(), 5));
}

#[tokio::test]
async fn tool_returning_exit_ends_a_function_caller() {
    assert_eq!(run("f(){ stopper; echo in-f; }; f; echo after").await, ("bye\n".to_string(), 5));
}

#[tokio::test]
async fn tool_returning_exit_in_a_pipeline_stage_ends_only_the_stage() {
    assert_eq!(run("stopper | cat; echo after").await, ("bye\nafter\n".to_string(), 0));
}

#[tokio::test]
async fn tool_without_override_continues_after_a_failure() {
    assert_eq!(run("plain; echo after").await, ("plain\nafter\n".to_string(), 0));
}
