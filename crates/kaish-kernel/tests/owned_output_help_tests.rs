//! Regression tests for #51: the kernel's generic `--help` router must NOT
//! intercept `--help`/`-h` for tools that own their output and re-parse their
//! own argv (`with_owned_output()`).
//!
//! Such a tool routes its own help — including leaf/subcommand help — through
//! its internal parser. Intercepting at the kernel layer renders the generic
//! whole-tool help and returns *before* `execute()`, so the tool can never show
//! its subcommand help. The fix: when `schema.owns_output` is set, pass `--help`
//! through to the tool. Plain tools must still get the generic router.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use async_trait::async_trait;
use kaish_kernel::tools::{ToolArgs, ToolCtx, ToolSchema};
use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend, Tool};
use kaish_types::ExecResult;

/// An owned-output tool that renders its OWN help when it sees `--help`/`-h`.
/// The sentinel text proves `execute()` was reached rather than the kernel's
/// generic `HelpTopic::Tool` router short-circuiting first.
struct OwnedTool;

#[async_trait]
impl Tool for OwnedTool {
    fn name(&self) -> &str {
        "owned"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("owned", "owned-output test tool").with_owned_output()
    }

    async fn execute(&self, args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        if args.flags.contains("help") || args.flags.contains("h") {
            // A real owned-output tool would render leaf-aware clap help here.
            return ExecResult::success("OWNED-HELP-RENDERED");
        }
        ExecResult::success("OWNED-RAN")
    }
}

/// A plain tool (does NOT own output). The kernel's generic `--help` router
/// should still intercept, so its `execute()` sentinel must never appear.
struct PlainTool;

#[async_trait]
impl Tool for PlainTool {
    fn name(&self) -> &str {
        "plain"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("plain", "plain test tool")
    }

    async fn execute(&self, _args: ToolArgs, _ctx: &mut dyn ToolCtx) -> ExecResult {
        ExecResult::success("PLAIN-RAN")
    }
}

fn kernel_with_tools() -> Arc<Kernel> {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |tools| {
        tools.register(OwnedTool);
        tools.register(PlainTool);
    })
    .expect("with_backend kernel")
    .into_arc()
}

/// The bug: `--help` on an owned-output tool must reach `execute()` so the tool
/// renders its own (leaf-aware) help, not the kernel's generic top-level help.
#[tokio::test]
async fn owned_output_tool_receives_help_flag() {
    let kernel = kernel_with_tools();
    let result = kernel.execute("owned sub --help").await.expect("execute");
    assert_eq!(
        result.text_out().trim(),
        "OWNED-HELP-RENDERED",
        "owned-output tool didn't receive --help; the kernel intercepted it",
    );
}

/// `-h` short form, same expectation.
#[tokio::test]
async fn owned_output_tool_receives_h_flag() {
    let kernel = kernel_with_tools();
    let result = kernel.execute("owned -h").await.expect("execute");
    assert_eq!(result.text_out().trim(), "OWNED-HELP-RENDERED");
}

/// Sanity: without a help flag the owned tool runs normally.
#[tokio::test]
async fn owned_output_tool_runs_without_help() {
    let kernel = kernel_with_tools();
    let result = kernel.execute("owned").await.expect("execute");
    assert_eq!(result.text_out().trim(), "OWNED-RAN");
}

/// Regression guard: a plain tool's `--help` is STILL intercepted by the kernel
/// router, so `execute()` (and its sentinel) is never reached.
#[tokio::test]
async fn plain_tool_help_is_still_intercepted() {
    let kernel = kernel_with_tools();
    let result = kernel.execute("plain --help").await.expect("execute");
    assert_ne!(
        result.text_out().trim(),
        "PLAIN-RAN",
        "plain tool's --help should be intercepted by the kernel, not run execute()",
    );
}

/// Sanity: the plain tool runs normally without a help flag.
#[tokio::test]
async fn plain_tool_runs_without_help() {
    let kernel = kernel_with_tools();
    let result = kernel.execute("plain").await.expect("execute");
    assert_eq!(result.text_out().trim(), "PLAIN-RAN");
}
