//! Integration tests for job streams and /v/jobs VFS.
//!
//! Tests verify:
//! - Jobs appear in /v/jobs/ directory
//! - stdout/stderr streams capture output
//! - status file reflects job state
//! - command file contains original command

use std::path::Path;
use std::sync::Arc;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::scheduler::{BoundedStream, JobManager};
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

    // Create a job with streams
    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (_tx, rx) = oneshot::channel();

    let id = jobs
        .register_with_streams("echo hello".to_string(), rx, stdout, stderr)
        .await;

    // Job directory should exist in /v/jobs
    let entries = vfs.list(Path::new("/v/jobs")).await.unwrap();
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].name, id.0.to_string());

    // Job directory should contain expected files
    let job_path = format!("/v/jobs/{}", id);
    let files = vfs.list(Path::new(&job_path)).await.unwrap();
    let names: Vec<_> = files.iter().map(|e| e.name.as_str()).collect();
    assert!(names.contains(&"stdout"));
    assert!(names.contains(&"stderr"));
    assert!(names.contains(&"status"));
    assert!(names.contains(&"command"));
}

#[tokio::test]
async fn test_stdout_stream_captures_output() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (tx, rx) = oneshot::channel();

    // Write to stdout stream before registration
    stdout.write(b"line 1\n").await;
    stdout.write(b"line 2\n").await;

    let id = jobs
        .register_with_streams("test_cmd".to_string(), rx, stdout.clone(), stderr)
        .await;

    // Write more after registration
    stdout.write(b"line 3\n").await;

    // Read via VFS
    let stdout_path = format!("/v/jobs/{}/stdout", id);
    let content = vfs.read(Path::new(&stdout_path)).await.unwrap();
    let text = String::from_utf8_lossy(&content);

    assert!(text.contains("line 1"));
    assert!(text.contains("line 2"));
    assert!(text.contains("line 3"));

    // Cleanup
    let _ = tx.send(ExecResult::success(""));
}

#[tokio::test]
async fn test_stderr_stream_captures_errors() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (tx, rx) = oneshot::channel();

    stderr.write(b"error: something failed\n").await;

    let id = jobs
        .register_with_streams("failing_cmd".to_string(), rx, stdout, stderr)
        .await;

    let stderr_path = format!("/v/jobs/{}/stderr", id);
    let content = vfs.read(Path::new(&stderr_path)).await.unwrap();
    let text = String::from_utf8_lossy(&content);

    assert!(text.contains("error: something failed"));

    let _ = tx.send(ExecResult::failure(1, "error"));
}

#[tokio::test]
async fn test_status_reflects_running_job() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (_tx, rx) = oneshot::channel(); // Don't send result - job stays running

    let id = jobs
        .register_with_streams("long_running".to_string(), rx, stdout, stderr)
        .await;

    let status_path = format!("/v/jobs/{}/status", id);
    let content = vfs.read(Path::new(&status_path)).await.unwrap();
    let text = String::from_utf8_lossy(&content);

    assert_eq!(text.trim(), "running");
}

#[tokio::test]
async fn test_status_reflects_completed_job() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (tx, rx) = oneshot::channel();

    let id = jobs
        .register_with_streams("quick_cmd".to_string(), rx, stdout, stderr)
        .await;

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

    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (tx, rx) = oneshot::channel();

    let id = jobs
        .register_with_streams("failing_cmd".to_string(), rx, stdout, stderr)
        .await;

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

    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (tx, rx) = oneshot::channel();

    let id = jobs
        .register_with_streams(
            "cargo build --release".to_string(),
            rx,
            stdout,
            stderr,
        )
        .await;

    let cmd_path = format!("/v/jobs/{}/command", id);
    let content = vfs.read(Path::new(&cmd_path)).await.unwrap();
    let text = String::from_utf8_lossy(&content);

    assert_eq!(text.trim(), "cargo build --release");

    let _ = tx.send(ExecResult::success(""));
}

// ============================================================================
// Bounded Stream Tests
// ============================================================================

#[tokio::test]
async fn test_bounded_stream_evicts_oldest_on_overflow() {
    let jobs = Arc::new(JobManager::new());
    let vfs = make_vfs_with_jobs(jobs.clone());

    // Small buffer to trigger eviction
    let stdout = Arc::new(BoundedStream::new(20));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (tx, rx) = oneshot::channel();

    // Write more than buffer size
    stdout.write(b"0123456789").await; // 10 bytes
    stdout.write(b"ABCDEFGHIJ").await; // 10 bytes - now at 20
    stdout.write(b"KLMNO").await; // 5 bytes - evicts first 5

    let id = jobs
        .register_with_streams("big_output".to_string(), rx, stdout, stderr)
        .await;

    let stdout_path = format!("/v/jobs/{}/stdout", id);
    let content = vfs.read(Path::new(&stdout_path)).await.unwrap();
    let text = String::from_utf8_lossy(&content);

    // Should have last 20 bytes: "56789ABCDEFGHIJKLMNO"
    assert_eq!(text.len(), 20);
    assert!(text.starts_with("56789"));
    assert!(text.ends_with("KLMNO"));

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
        let stdout = Arc::new(BoundedStream::new(1024));
        let stderr = Arc::new(BoundedStream::new(1024));
        let (_tx, rx) = oneshot::channel();

        jobs.register_with_streams(format!("job_{}", i), rx, stdout, stderr)
            .await;
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

    // Create a job
    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (_tx, rx) = oneshot::channel();

    let id = jobs
        .register_with_streams("test job".to_string(), rx, stdout, stderr)
        .await;

    // Run jobs builtin
    let jobs_tool = registry.get("jobs").unwrap();
    let result = jobs_tool.execute(ToolArgs::new(), &mut ctx).await;

    assert!(result.ok());
    assert!(result.out.contains(&format!("/v/jobs/{}/", id)));
    assert!(result.out.contains("Running"));
    assert!(result.out.contains("test job"));
}

#[tokio::test]
async fn test_ls_v_jobs_directory() {
    let jobs = Arc::new(JobManager::new());
    let mut ctx = make_ctx(jobs.clone());
    let registry = make_registry().await;

    // Create a job
    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (_tx, rx) = oneshot::channel();

    let id = jobs
        .register_with_streams("background task".to_string(), rx, stdout, stderr)
        .await;

    // Use ls to list /v/jobs
    ctx.set_cwd("/".into());
    let mut args = ToolArgs::new();
    args.positional.push(kaish_kernel::ast::Value::String("/v/jobs".to_string()));

    let ls_tool = registry.get("ls").unwrap();
    let result = ls_tool.execute(args, &mut ctx).await;

    assert!(result.ok());
    assert!(result.out.contains(&id.0.to_string()));
}

#[tokio::test]
async fn test_cat_v_jobs_status() {
    let jobs = Arc::new(JobManager::new());
    let mut ctx = make_ctx(jobs.clone());
    let registry = make_registry().await;

    // Create a job
    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (_tx, rx) = oneshot::channel();

    let id = jobs
        .register_with_streams("check status".to_string(), rx, stdout, stderr)
        .await;

    // Use cat to read status
    ctx.set_cwd("/".into());
    let mut args = ToolArgs::new();
    args.positional.push(kaish_kernel::ast::Value::String(
        format!("/v/jobs/{}/status", id),
    ));

    let cat_tool = registry.get("cat").unwrap();
    let result = cat_tool.execute(args, &mut ctx).await;

    assert!(result.ok());
    assert!(result.out.contains("running"));
}

#[tokio::test]
async fn test_cat_v_jobs_stdout() {
    let jobs = Arc::new(JobManager::new());
    let mut ctx = make_ctx(jobs.clone());
    let registry = make_registry().await;

    // Create a job with output
    let stdout = Arc::new(BoundedStream::new(1024));
    let stderr = Arc::new(BoundedStream::new(1024));
    let (_tx, rx) = oneshot::channel();

    stdout.write(b"build progress: 50%\n").await;
    stdout.write(b"build progress: 100%\n").await;

    let id = jobs
        .register_with_streams("cargo build".to_string(), rx, stdout, stderr)
        .await;

    // Use cat to read stdout
    ctx.set_cwd("/".into());
    let mut args = ToolArgs::new();
    args.positional.push(kaish_kernel::ast::Value::String(
        format!("/v/jobs/{}/stdout", id),
    ));

    let cat_tool = registry.get("cat").unwrap();
    let result = cat_tool.execute(args, &mut ctx).await;

    assert!(result.ok());
    assert!(result.out.contains("build progress: 50%"));
    assert!(result.out.contains("build progress: 100%"));
}
