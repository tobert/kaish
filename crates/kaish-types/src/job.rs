//! Job identification and status types.

use std::path::PathBuf;

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
    /// Job was stopped by a signal (e.g., Ctrl-Z / SIGTSTP).
    Stopped,
    /// Job completed successfully.
    Done,
    /// Job failed with an error.
    Failed,
}

impl std::fmt::Display for JobStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JobStatus::Running => write!(f, "Running"),
            JobStatus::Stopped => write!(f, "Stopped"),
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
    /// Path to output file (if available).
    pub output_file: Option<PathBuf>,
    /// OS process ID (if this is a stopped/foreground process).
    pub pid: Option<u32>,
}
