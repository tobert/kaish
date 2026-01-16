//! Background job management for kaish.
//!
//! Provides the `JobManager` for tracking background jobs started with `&`.

use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::Arc;

use tokio::sync::{oneshot, Mutex};
use tokio::task::JoinHandle;

use crate::interpreter::ExecResult;

/// Unique identifier for a background job.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct JobId(pub u64);

impl std::fmt::Display for JobId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Status of a background job.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JobStatus {
    /// Job is currently running.
    Running,
    /// Job completed successfully.
    Done,
    /// Job failed with an error.
    Failed,
}

impl std::fmt::Display for JobStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JobStatus::Running => write!(f, "Running"),
            JobStatus::Done => write!(f, "Done"),
            JobStatus::Failed => write!(f, "Failed"),
        }
    }
}

/// Information about a job for listing.
#[derive(Debug, Clone)]
pub struct JobInfo {
    /// Job ID.
    pub id: JobId,
    /// Command description.
    pub command: String,
    /// Current status.
    pub status: JobStatus,
}

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
        }
    }

    /// Check if the job has completed.
    pub fn is_done(&self) -> bool {
        self.result.is_some()
    }

    /// Get the job's status.
    pub fn status(&self) -> JobStatus {
        match &self.result {
            Some(r) if r.ok() => JobStatus::Done,
            Some(_) => JobStatus::Failed,
            None => JobStatus::Running,
        }
    }

    /// Wait for the job to complete and return its result.
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

        self.result = Some(result.clone());
        result
    }

    /// Get the result if completed, without waiting.
    pub fn try_result(&self) -> Option<&ExecResult> {
        self.result.as_ref()
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
    pub fn spawn<F>(&self, command: String, future: F) -> JobId
    where
        F: std::future::Future<Output = ExecResult> + Send + 'static,
    {
        let id = JobId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let handle = tokio::spawn(future);
        let job = Job::new(id, command, handle);

        // Can't await here, so use try_lock or spawn a task to insert
        let jobs = self.jobs.clone();
        tokio::spawn(async move {
            let mut jobs = jobs.lock().await;
            jobs.insert(id, job);
        });

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
        let jobs = self.jobs.lock().await;
        jobs.values()
            .map(|job| JobInfo {
                id: job.id,
                command: job.command.clone(),
                status: job.status(),
            })
            .collect()
    }

    /// Get the number of running jobs.
    pub async fn running_count(&self) -> usize {
        let jobs = self.jobs.lock().await;
        jobs.values().filter(|j| !j.is_done()).count()
    }

    /// Remove completed jobs from tracking.
    pub async fn cleanup(&self) {
        let mut jobs = self.jobs.lock().await;
        jobs.retain(|_, job| !job.is_done());
    }

    /// Check if a specific job exists.
    pub async fn exists(&self, id: JobId) -> bool {
        let jobs = self.jobs.lock().await;
        jobs.contains_key(&id)
    }

    /// Get info for a specific job.
    pub async fn get(&self, id: JobId) -> Option<JobInfo> {
        let jobs = self.jobs.lock().await;
        jobs.get(&id).map(|job| JobInfo {
            id: job.id,
            command: job.command.clone(),
            status: job.status(),
        })
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
    async fn test_nonexistent_job() {
        let manager = JobManager::new();
        let result = manager.wait(JobId(999)).await;
        assert!(result.is_none());
    }
}
