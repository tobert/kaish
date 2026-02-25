//! Background job management for kaish.
//!
//! Provides the `JobManager` for tracking background jobs started with `&`.

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use tokio::sync::{oneshot, Mutex};
use tokio::task::JoinHandle;

use super::stream::BoundedStream;
use crate::interpreter::ExecResult;

// Data types re-exported from kaish-types.
pub use kaish_types::{JobId, JobInfo, JobStatus};

/// A background job.
pub struct Job {
    /// Job ID.
    pub id: JobId,
    /// Command description.
    pub command: String,
    /// Task handle (None if already awaited).
    handle: Option<JoinHandle<ExecResult>>,
    /// Channel to receive result (alternative to handle).
    result_rx: Option<oneshot::Receiver<ExecResult>>,
    /// Cached result after completion.
    result: Option<ExecResult>,
    /// Path to output file (captures stdout/stderr after completion).
    output_file: Option<PathBuf>,
    /// Live stdout stream (bounded ring buffer).
    stdout_stream: Option<Arc<BoundedStream>>,
    /// Live stderr stream (bounded ring buffer).
    stderr_stream: Option<Arc<BoundedStream>>,
    /// OS process ID (for stopped jobs).
    pid: Option<u32>,
    /// OS process group ID (for stopped jobs).
    pgid: Option<u32>,
    /// Whether this job is stopped (SIGTSTP).
    stopped: bool,
}

impl Job {
    /// Create a new job from a task handle.
    pub fn new(id: JobId, command: String, handle: JoinHandle<ExecResult>) -> Self {
        Self {
            id,
            command,
            handle: Some(handle),
            result_rx: None,
            result: None,
            output_file: None,
            stdout_stream: None,
            stderr_stream: None,
            pid: None,
            pgid: None,
            stopped: false,
        }
    }

    /// Create a new job from a result channel.
    pub fn from_channel(id: JobId, command: String, rx: oneshot::Receiver<ExecResult>) -> Self {
        Self {
            id,
            command,
            handle: None,
            result_rx: Some(rx),
            result: None,
            output_file: None,
            stdout_stream: None,
            stderr_stream: None,
            pid: None,
            pgid: None,
            stopped: false,
        }
    }

    /// Create a new job with attached output streams.
    ///
    /// The streams provide live access to job output via `/v/jobs/{id}/stdout` and `/stderr`.
    pub fn with_streams(
        id: JobId,
        command: String,
        rx: oneshot::Receiver<ExecResult>,
        stdout: Arc<BoundedStream>,
        stderr: Arc<BoundedStream>,
    ) -> Self {
        Self {
            id,
            command,
            handle: None,
            result_rx: Some(rx),
            result: None,
            output_file: None,
            stdout_stream: Some(stdout),
            stderr_stream: Some(stderr),
            pid: None,
            pgid: None,
            stopped: false,
        }
    }

    /// Create a stopped job (from Ctrl-Z on a foreground process).
    pub fn stopped(id: JobId, command: String, pid: u32, pgid: u32) -> Self {
        Self {
            id,
            command,
            handle: None,
            result_rx: None,
            result: None,
            output_file: None,
            stdout_stream: None,
            stderr_stream: None,
            pid: Some(pid),
            pgid: Some(pgid),
            stopped: true,
        }
    }

    /// Get the output file path (if available).
    pub fn output_file(&self) -> Option<&PathBuf> {
        self.output_file.as_ref()
    }

    /// Check if the job has completed.
    ///
    /// Stopped jobs are not considered done.
    pub fn is_done(&mut self) -> bool {
        if self.stopped {
            return false;
        }
        self.try_poll();
        self.result.is_some()
    }

    /// Get the job's status.
    pub fn status(&mut self) -> JobStatus {
        if self.stopped {
            return JobStatus::Stopped;
        }
        self.try_poll();
        match &self.result {
            Some(r) if r.ok() => JobStatus::Done,
            Some(_) => JobStatus::Failed,
            None => JobStatus::Running,
        }
    }

    /// Get the job's status as a string suitable for /v/jobs/{id}/status.
    ///
    /// Returns:
    /// - `"running"` if the job is still running
    /// - `"done:0"` if the job completed successfully
    /// - `"failed:{code}"` if the job failed with an exit code
    pub fn status_string(&mut self) -> String {
        self.try_poll();
        match &self.result {
            Some(r) if r.ok() => "done:0".to_string(),
            Some(r) => format!("failed:{}", r.code),
            None => "running".to_string(),
        }
    }

    /// Get the stdout stream (if attached).
    pub fn stdout_stream(&self) -> Option<&Arc<BoundedStream>> {
        self.stdout_stream.as_ref()
    }

    /// Get the stderr stream (if attached).
    pub fn stderr_stream(&self) -> Option<&Arc<BoundedStream>> {
        self.stderr_stream.as_ref()
    }

    /// Wait for the job to complete and return its result.
    ///
    /// On completion, the job's output is written to a temp file for later retrieval.
    pub async fn wait(&mut self) -> ExecResult {
        if let Some(result) = self.result.take() {
            self.result = Some(result.clone());
            return result;
        }

        let result = if let Some(handle) = self.handle.take() {
            match handle.await {
                Ok(r) => r,
                Err(e) => ExecResult::failure(1, format!("job panicked: {}", e)),
            }
        } else if let Some(rx) = self.result_rx.take() {
            match rx.await {
                Ok(r) => r,
                Err(_) => ExecResult::failure(1, "job channel closed"),
            }
        } else {
            // Already waited
            self.result.clone().unwrap_or_else(|| ExecResult::failure(1, "no result"))
        };

        // Write output to temp file for later retrieval
        if self.output_file.is_none()
            && let Some(path) = self.write_output_file(&result) {
                self.output_file = Some(path);
            }

        self.result = Some(result.clone());
        result
    }

    /// Write job output to a temp file.
    fn write_output_file(&self, result: &ExecResult) -> Option<PathBuf> {
        // Only write if there's output to capture
        if result.out.is_empty() && result.err.is_empty() {
            return None;
        }

        let tmp_dir = std::env::temp_dir().join("kaish").join("jobs");
        if std::fs::create_dir_all(&tmp_dir).is_err() {
            tracing::warn!("Failed to create job output directory");
            return None;
        }

        let filename = format!("job_{}.txt", self.id.0);
        let path = tmp_dir.join(filename);

        let mut content = String::new();
        content.push_str(&format!("# Job {}: {}\n", self.id, self.command));
        content.push_str(&format!("# Status: {}\n\n", if result.ok() { "Done" } else { "Failed" }));

        if !result.out.is_empty() {
            content.push_str("## STDOUT\n");
            content.push_str(&result.out);
            if !result.out.ends_with('\n') {
                content.push('\n');
            }
        }

        if !result.err.is_empty() {
            content.push_str("\n## STDERR\n");
            content.push_str(&result.err);
            if !result.err.ends_with('\n') {
                content.push('\n');
            }
        }

        match std::fs::write(&path, content) {
            Ok(()) => Some(path),
            Err(e) => {
                tracing::warn!("Failed to write job output file: {}", e);
                None
            }
        }
    }

    /// Remove any temp files associated with this job.
    pub fn cleanup_files(&mut self) {
        if let Some(path) = self.output_file.take() {
            if let Err(e) = std::fs::remove_file(&path) {
                // Ignore "not found" — file may not have been written
                if e.kind() != io::ErrorKind::NotFound {
                    tracing::warn!("Failed to clean up job output file {}: {}", path.display(), e);
                }
            }
        }
    }

    /// Get the result if completed, without waiting.
    pub fn try_result(&self) -> Option<&ExecResult> {
        self.result.as_ref()
    }

    /// Try to poll the result channel and update status.
    ///
    /// This is a non-blocking check that updates `self.result` if the
    /// job has completed. Returns true if the job is now done.
    pub fn try_poll(&mut self) -> bool {
        if self.result.is_some() {
            return true;
        }

        // Try to poll the oneshot channel
        if let Some(rx) = self.result_rx.as_mut() {
            match rx.try_recv() {
                Ok(result) => {
                    self.result = Some(result);
                    self.result_rx = None;
                    return true;
                }
                Err(tokio::sync::oneshot::error::TryRecvError::Empty) => {
                    // Still running
                    return false;
                }
                Err(tokio::sync::oneshot::error::TryRecvError::Closed) => {
                    // Channel closed without result - job failed
                    self.result = Some(ExecResult::failure(1, "job channel closed"));
                    self.result_rx = None;
                    return true;
                }
            }
        }

        // Check if handle is finished
        if let Some(handle) = self.handle.as_mut()
            && handle.is_finished() {
                // Take the handle and wait for it (should be instant)
                let Some(handle) = self.handle.take() else {
                    return false;
                };
                // We can't await here, so we use now_or_never
                // Note: this is synchronous since is_finished() was true
                let result = match tokio::task::block_in_place(|| {
                    tokio::runtime::Handle::current().block_on(handle)
                }) {
                    Ok(r) => r,
                    Err(e) => ExecResult::failure(1, format!("job panicked: {}", e)),
                };
                self.result = Some(result);
                return true;
            }

        false
    }
}

/// Manager for background jobs.
pub struct JobManager {
    /// Counter for generating unique job IDs.
    next_id: AtomicU64,
    /// Map of job ID to job.
    jobs: Arc<Mutex<HashMap<JobId, Job>>>,
}

impl JobManager {
    /// Create a new job manager.
    pub fn new() -> Self {
        Self {
            next_id: AtomicU64::new(1),
            jobs: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Spawn a new background job from a future.
    ///
    /// The job is inserted into the map synchronously before returning,
    /// guaranteeing it's immediately queryable via `exists()` or `get()`.
    pub fn spawn<F>(&self, command: String, future: F) -> JobId
    where
        F: std::future::Future<Output = ExecResult> + Send + 'static,
    {
        let id = JobId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let handle = tokio::spawn(future);
        let job = Job::new(id, command, handle);

        // Spin on try_lock to guarantee the job is in the map on return.
        // The lock is tokio::sync::Mutex which is only held briefly during
        // HashMap operations, so contention resolves quickly.
        let mut job_opt = Some(job);
        loop {
            match self.jobs.try_lock() {
                Ok(mut guard) => {
                    if let Some(j) = job_opt.take() {
                        guard.insert(id, j);
                    }
                    break;
                }
                Err(_) => {
                    std::hint::spin_loop();
                }
            }
        }

        id
    }

    /// Spawn a job that's already running and communicate via channel.
    pub async fn register(&self, command: String, rx: oneshot::Receiver<ExecResult>) -> JobId {
        let id = JobId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let job = Job::from_channel(id, command, rx);

        let mut jobs = self.jobs.lock().await;
        jobs.insert(id, job);

        id
    }

    /// Register a job with attached output streams.
    ///
    /// The streams provide live access to job output via `/v/jobs/{id}/stdout` and `/stderr`.
    pub async fn register_with_streams(
        &self,
        command: String,
        rx: oneshot::Receiver<ExecResult>,
        stdout: Arc<BoundedStream>,
        stderr: Arc<BoundedStream>,
    ) -> JobId {
        let id = JobId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let job = Job::with_streams(id, command, rx, stdout, stderr);

        let mut jobs = self.jobs.lock().await;
        jobs.insert(id, job);

        id
    }

    /// Wait for a specific job to complete.
    pub async fn wait(&self, id: JobId) -> Option<ExecResult> {
        let mut jobs = self.jobs.lock().await;
        if let Some(job) = jobs.get_mut(&id) {
            Some(job.wait().await)
        } else {
            None
        }
    }

    /// Wait for all jobs to complete, returning results in completion order.
    pub async fn wait_all(&self) -> Vec<(JobId, ExecResult)> {
        let mut results = Vec::new();

        // Get all job IDs
        let ids: Vec<JobId> = {
            let jobs = self.jobs.lock().await;
            jobs.keys().copied().collect()
        };

        for id in ids {
            if let Some(result) = self.wait(id).await {
                results.push((id, result));
            }
        }

        results
    }

    /// List all jobs with their status.
    pub async fn list(&self) -> Vec<JobInfo> {
        let mut jobs = self.jobs.lock().await;
        jobs.values_mut()
            .map(|job| JobInfo {
                id: job.id,
                command: job.command.clone(),
                status: job.status(),
                output_file: job.output_file.clone(),
                pid: job.pid,
            })
            .collect()
    }

    /// Get the number of running jobs.
    pub async fn running_count(&self) -> usize {
        let mut jobs = self.jobs.lock().await;
        let mut count = 0;
        for job in jobs.values_mut() {
            if !job.is_done() {
                count += 1;
            }
        }
        count
    }

    /// Remove completed jobs from tracking and clean up their temp files.
    pub async fn cleanup(&self) {
        let mut jobs = self.jobs.lock().await;
        jobs.retain(|_, job| {
            if job.is_done() {
                job.cleanup_files();
                false
            } else {
                true
            }
        });
    }

    /// Check if a specific job exists.
    pub async fn exists(&self, id: JobId) -> bool {
        let jobs = self.jobs.lock().await;
        jobs.contains_key(&id)
    }

    /// Get info for a specific job.
    pub async fn get(&self, id: JobId) -> Option<JobInfo> {
        let mut jobs = self.jobs.lock().await;
        jobs.get_mut(&id).map(|job| JobInfo {
            id: job.id,
            command: job.command.clone(),
            status: job.status(),
            output_file: job.output_file.clone(),
            pid: job.pid,
        })
    }

    /// Get the command string for a job.
    pub async fn get_command(&self, id: JobId) -> Option<String> {
        let jobs = self.jobs.lock().await;
        jobs.get(&id).map(|job| job.command.clone())
    }

    /// Get the status string for a job (for /v/jobs/{id}/status).
    pub async fn get_status_string(&self, id: JobId) -> Option<String> {
        let mut jobs = self.jobs.lock().await;
        jobs.get_mut(&id).map(|job| job.status_string())
    }

    /// Read stdout stream content for a job.
    ///
    /// Returns `None` if the job doesn't exist or has no attached stream.
    pub async fn read_stdout(&self, id: JobId) -> Option<Vec<u8>> {
        let jobs = self.jobs.lock().await;
        if let Some(job) = jobs.get(&id)
            && let Some(stream) = job.stdout_stream() {
                return Some(stream.read().await);
            }
        None
    }

    /// Read stderr stream content for a job.
    ///
    /// Returns `None` if the job doesn't exist or has no attached stream.
    pub async fn read_stderr(&self, id: JobId) -> Option<Vec<u8>> {
        let jobs = self.jobs.lock().await;
        if let Some(job) = jobs.get(&id)
            && let Some(stream) = job.stderr_stream() {
                return Some(stream.read().await);
            }
        None
    }

    /// List all job IDs.
    pub async fn list_ids(&self) -> Vec<JobId> {
        let jobs = self.jobs.lock().await;
        jobs.keys().copied().collect()
    }

    /// Register a stopped job (from Ctrl-Z on a foreground process).
    pub async fn register_stopped(&self, command: String, pid: u32, pgid: u32) -> JobId {
        let id = JobId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let job = Job::stopped(id, command, pid, pgid);
        let mut jobs = self.jobs.lock().await;
        jobs.insert(id, job);
        id
    }

    /// Mark a job as stopped with its process info.
    pub async fn stop_job(&self, id: JobId, pid: u32, pgid: u32) {
        let mut jobs = self.jobs.lock().await;
        if let Some(job) = jobs.get_mut(&id) {
            job.stopped = true;
            job.pid = Some(pid);
            job.pgid = Some(pgid);
        }
    }

    /// Mark a stopped job as resumed.
    pub async fn resume_job(&self, id: JobId) {
        let mut jobs = self.jobs.lock().await;
        if let Some(job) = jobs.get_mut(&id) {
            job.stopped = false;
        }
    }

    /// Get the most recently stopped job.
    pub async fn last_stopped(&self) -> Option<JobId> {
        let mut jobs = self.jobs.lock().await;
        // Find the highest-numbered stopped job
        let mut best: Option<JobId> = None;
        for job in jobs.values_mut() {
            if job.stopped {
                match best {
                    None => best = Some(job.id),
                    Some(b) if job.id.0 > b.0 => best = Some(job.id),
                    _ => {}
                }
            }
        }
        best
    }

    /// Get process info (pid, pgid) for a job.
    pub async fn get_process_info(&self, id: JobId) -> Option<(u32, u32)> {
        let jobs = self.jobs.lock().await;
        jobs.get(&id).and_then(|job| {
            match (job.pid, job.pgid) {
                (Some(pid), Some(pgid)) => Some((pid, pgid)),
                _ => None,
            }
        })
    }

    /// Remove a job from tracking.
    pub async fn remove(&self, id: JobId) {
        let mut jobs = self.jobs.lock().await;
        if let Some(mut job) = jobs.remove(&id) {
            job.cleanup_files();
        }
    }
}

impl Default for JobManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::time::Duration;

    #[tokio::test]
    async fn test_spawn_and_wait() {
        let manager = JobManager::new();

        let id = manager.spawn("test".to_string(), async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            ExecResult::success("done")
        });

        // Wait a bit for the job to be registered
        tokio::time::sleep(Duration::from_millis(5)).await;

        let result = manager.wait(id).await;
        assert!(result.is_some());
        let result = result.unwrap();
        assert!(result.ok());
        assert_eq!(result.out, "done");
    }

    #[tokio::test]
    async fn test_wait_all() {
        let manager = JobManager::new();

        manager.spawn("job1".to_string(), async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            ExecResult::success("one")
        });

        manager.spawn("job2".to_string(), async {
            tokio::time::sleep(Duration::from_millis(5)).await;
            ExecResult::success("two")
        });

        // Wait for jobs to register
        tokio::time::sleep(Duration::from_millis(5)).await;

        let results = manager.wait_all().await;
        assert_eq!(results.len(), 2);
    }

    #[tokio::test]
    async fn test_list_jobs() {
        let manager = JobManager::new();

        manager.spawn("test job".to_string(), async {
            tokio::time::sleep(Duration::from_millis(50)).await;
            ExecResult::success("")
        });

        // Wait for job to register
        tokio::time::sleep(Duration::from_millis(5)).await;

        let jobs = manager.list().await;
        assert_eq!(jobs.len(), 1);
        assert_eq!(jobs[0].command, "test job");
        assert_eq!(jobs[0].status, JobStatus::Running);
    }

    #[tokio::test]
    async fn test_job_status_after_completion() {
        let manager = JobManager::new();

        let id = manager.spawn("quick".to_string(), async {
            ExecResult::success("")
        });

        // Wait for job to complete
        tokio::time::sleep(Duration::from_millis(10)).await;
        let _ = manager.wait(id).await;

        let info = manager.get(id).await;
        assert!(info.is_some());
        assert_eq!(info.unwrap().status, JobStatus::Done);
    }

    #[tokio::test]
    async fn test_cleanup() {
        let manager = JobManager::new();

        let id = manager.spawn("done".to_string(), async {
            ExecResult::success("")
        });

        // Wait for completion
        tokio::time::sleep(Duration::from_millis(10)).await;
        let _ = manager.wait(id).await;

        // Should have 1 job
        assert_eq!(manager.list().await.len(), 1);

        // Cleanup
        manager.cleanup().await;

        // Should have 0 jobs
        assert_eq!(manager.list().await.len(), 0);
    }

    #[tokio::test]
    async fn test_cleanup_removes_temp_files() {
        // Bug K: cleanup should remove temp files
        let manager = JobManager::new();

        let id = manager.spawn("output job".to_string(), async {
            ExecResult::success("some output that gets written to a temp file")
        });

        // Wait for completion (triggers output file creation)
        tokio::time::sleep(Duration::from_millis(10)).await;
        let result = manager.wait(id).await;
        assert!(result.is_some());

        // Get the output file path before cleanup
        let output_file = {
            let jobs = manager.jobs.lock().await;
            jobs.get(&id).and_then(|j| j.output_file().cloned())
        };

        // Cleanup should remove the job and its files
        manager.cleanup().await;

        // If an output file was created, it should be gone now
        if let Some(path) = output_file {
            assert!(
                !path.exists(),
                "temp file should be removed after cleanup: {}",
                path.display()
            );
        }
    }

    #[tokio::test]
    async fn test_register_with_channel() {
        let manager = JobManager::new();
        let (tx, rx) = oneshot::channel();

        let id = manager.register("channel job".to_string(), rx).await;

        // Send result
        tx.send(ExecResult::success("from channel")).unwrap();

        let result = manager.wait(id).await;
        assert!(result.is_some());
        assert_eq!(result.unwrap().out, "from channel");
    }

    #[tokio::test]
    async fn test_spawn_immediately_available() {
        // Bug J: job should be queryable immediately after spawn()
        let manager = JobManager::new();

        let id = manager.spawn("instant".to_string(), async {
            tokio::time::sleep(Duration::from_millis(100)).await;
            ExecResult::success("done")
        });

        // Should be immediately visible without any sleep
        let exists = manager.exists(id).await;
        assert!(exists, "job should be immediately available after spawn()");

        let info = manager.get(id).await;
        assert!(info.is_some(), "job info should be available immediately");
    }

    #[tokio::test]
    async fn test_nonexistent_job() {
        let manager = JobManager::new();
        let result = manager.wait(JobId(999)).await;
        assert!(result.is_none());
    }
}
