//! Integration tests for the `/v/jobs` VFS.
//!
//! Tests verify:
//! - Jobs appear in /v/jobs/ directory
//! - status file reflects job state
//! - command file contains original command
//!
//! This file used to be `job_stream_tests.rs`. GH #240 removed
//! `/v/jobs/{id}/stdout`/`stderr` because they filled only once, at
//! completion, while four docs promised a live stream; they are back and
//! genuinely live. The liveness itself is pinned by
//! `job_live_output_tests.rs` — a real `&` job, peeked mid-run. What stays
//! here is the VFS surface: the nodes exist, list, and read.

use std::path::Path;
use std::sync::Arc;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::scheduler::JobManager;
use kaish_kernel::tools::{register_builtins, ExecContext, ToolArgs, ToolRegistry};
use kaish_kernel::vfs::{Filesystem, JobFs, MemoryFs, VfsRouter};
use tokio::sync::oneshot;

// ============================================================================
// Test Helpers
// ============================================================================

fn make_vfs_with_jobs(jobs: Arc<JobManager>) -> Arc<VfsRouter> {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    vfs.mount("/v/jobs", JobFs::new(jobs));
    Arc::new(vfs)
}

fn make_ctx(jobs: Arc<JobManager>) -> ExecContext {
    let vfs = make_vfs_with_jobs(jobs.clone());
    let mut ctx = ExecContext::new(vfs);
    ctx.set_job_manager(jobs);
    ctx
}

async fn make_registry() -> ToolRegistry {
    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);
    registry
}

// ============================================================================
// Basic VFS Tests
// ============================================================================

#[tokio::test]
async fn test_jobs_creates_vfs_entry() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    let (_tx, rx) = oneshot::channel();
    let id = jobs.register("echo hello".to_string(), rx).await;

    // Job directory should exist in /v/jobs
    let entries = vfs.list(Path::new("/v/jobs")).await.unwrap();
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].name, id.0.to_string());

    // Job directory should contain expected files.
    let job_path = format!("/v/jobs/{}", id);
    let files = vfs.list(Path::new(&job_path)).await.unwrap();
    let names: Vec<_> = files.iter().map(|e| e.name.as_str()).collect();
    assert!(names.contains(&"stdout"));
    assert!(names.contains(&"stderr"));
    assert!(names.contains(&"status"));
    assert!(names.contains(&"command"));
    assert!(!names.contains(&"approval"), "the approval node went with the ledger");
}

#[tokio::test]
async fn test_status_reflects_running_job() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    let (_tx, rx) = oneshot::channel(); // Don't send result - job stays running
    let id = jobs.register("long_running".to_string(), rx).await;

    let status_path = format!("/v/jobs/{}/status", id);
    let content = vfs.read(Path::new(&status_path)).await.unwrap();
    let text = String::from_utf8_lossy(&content);

    assert_eq!(text.trim(), "running");
}

#[tokio::test]
async fn test_status_reflects_completed_job() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    let (tx, rx) = oneshot::channel();
    let id = jobs.register("quick_cmd".to_string(), rx).await;

    // Complete the job
    tx.send(ExecResult::success("done")).unwrap();
    let _ = jobs.wait(id).await;

    let status_path = format!("/v/jobs/{}/status", id);
    let content = vfs.read(Path::new(&status_path)).await.unwrap();
    let text = String::from_utf8_lossy(&content);

    assert_eq!(text.trim(), "done:0");
}

#[tokio::test]
async fn test_status_reflects_failed_job() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    let (tx, rx) = oneshot::channel();
    let id = jobs.register("failing_cmd".to_string(), rx).await;

    // Fail the job with exit code 42
    tx.send(ExecResult::from_output(42, String::new(), "error".to_string()))
        .unwrap();
    let _ = jobs.wait(id).await;

    let status_path = format!("/v/jobs/{}/status", id);
    let content = vfs.read(Path::new(&status_path)).await.unwrap();
    let text = String::from_utf8_lossy(&content);

    assert_eq!(text.trim(), "failed:42");
}

#[tokio::test]
async fn test_command_file_contains_original_command() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    let (tx, rx) = oneshot::channel();
    let id = jobs
        .register("cargo build --release".to_string(), rx)
        .await;

    let cmd_path = format!("/v/jobs/{}/command", id);
    let content = vfs.read(Path::new(&cmd_path)).await.unwrap();
    let text = String::from_utf8_lossy(&content);

    assert_eq!(text.trim(), "cargo build --release");

    let _ = tx.send(ExecResult::success(""));
}

// ============================================================================
// Multiple Jobs Tests
// ============================================================================

#[tokio::test]
async fn test_multiple_jobs_listed() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    // Create several jobs
    for i in 1..=3 {
        let (_tx, rx) = oneshot::channel();
        jobs.register(format!("job_{}", i), rx).await;
    }

    let entries = vfs.list(Path::new("/v/jobs")).await.unwrap();
    assert_eq!(entries.len(), 3);
}

// ============================================================================
// Jobs Builtin Integration Tests
// ============================================================================

#[tokio::test]
async fn test_jobs_builtin_shows_vfs_path() {
    let jobs = Arc::new(JobManager::new());
    let mut ctx = make_ctx(jobs.clone());
    let registry = make_registry().await;

    let (_tx, rx) = oneshot::channel();
    let id = jobs.register("test job".to_string(), rx).await;

    // Run jobs builtin
    let jobs_tool = registry.get("jobs").unwrap();
    let result = jobs_tool.execute(ToolArgs::new(), &mut ctx).await;

    assert!(result.ok());
    let text = result.text_out();
    assert!(text.contains(&format!("/v/jobs/{}/", id)));
    assert!(text.contains("Running"));
    assert!(text.contains("test job"));
}

#[tokio::test]
async fn test_ls_v_jobs_directory() {
    let jobs = Arc::new(JobManager::new());
    let mut ctx = make_ctx(jobs.clone());
    let registry = make_registry().await;

    let (_tx, rx) = oneshot::channel();
    let id = jobs.register("background task".to_string(), rx).await;

    // Use ls to list /v/jobs
    ctx.set_cwd("/".into());
    let mut args = ToolArgs::new();
    args.positional.push(kaish_kernel::ast::Value::String("/v/jobs".to_string()));

    let ls_tool = registry.get("ls").unwrap();
    let result = ls_tool.execute(args, &mut ctx).await;

    assert!(result.ok());
    assert!(result.text_out().contains(&id.0.to_string()));
}

#[tokio::test]
async fn test_cat_v_jobs_status() {
    let jobs = Arc::new(JobManager::new());
    let mut ctx = make_ctx(jobs.clone());
    let registry = make_registry().await;

    let (_tx, rx) = oneshot::channel();
    let id = jobs.register("check status".to_string(), rx).await;

    // Use cat to read status
    ctx.set_cwd("/".into());
    let mut args = ToolArgs::new();
    args.positional.push(kaish_kernel::ast::Value::String(
        format!("/v/jobs/{}/status", id),
    ));

    let cat_tool = registry.get("cat").unwrap();
    let result = cat_tool.execute(args, &mut ctx).await;

    assert!(result.ok());
    assert!(result.text_out().contains("running"));
}

#[tokio::test]
async fn test_cat_v_jobs_stdout_reads_empty_before_any_output() {
    // A registered job that has written nothing reads as empty, not an error:
    // "nothing yet" and "no such job" have to stay distinguishable.
    let jobs = Arc::new(JobManager::new());
    let mut ctx = make_ctx(jobs.clone());
    let registry = make_registry().await;

    let (_tx, rx) = oneshot::channel();
    let id = jobs.register("cargo build".to_string(), rx).await;

    ctx.set_cwd("/".into());
    let mut args = ToolArgs::new();
    args.positional.push(kaish_kernel::ast::Value::String(
        format!("/v/jobs/{}/stdout", id),
    ));

    let cat_tool = registry.get("cat").unwrap();
    let result = cat_tool.execute(args, &mut ctx).await;

    assert!(result.ok(), "reading a live-but-silent job's stdout is not an error");
    assert_eq!(result.text_out(), "", "nothing has been written yet");
}
