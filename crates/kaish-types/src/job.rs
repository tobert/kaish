//! Job identification and status types.

use std::path::PathBuf;

use crate::result::LatchRequest;

/// Unique identifier for a background job.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct JobId(pub u64);

impl std::fmt::Display for JobId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Status of a background job.
#[non_exhaustive]
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum JobStatus {
    /// Job is currently running.
    Running,
    /// Job was stopped by a signal (e.g., Ctrl-Z / SIGTSTP).
    Stopped,
    /// Job completed successfully.
    Done,
    /// Job is blocked on an unfulfilled confirmation latch (exit 2 with a
    /// stored `LatchRequest` — `rm x &` under `set -o latch`). Distinct from
    /// `Failed`: the op is *held*, not errored, and can still be fulfilled via
    /// `Kernel::confirm` with the request surfaced on `JobInfo.latch`.
    Latched,
    /// Job failed with an error.
    Failed,
}

impl std::fmt::Display for JobStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JobStatus::Running => write!(f, "Running"),
            JobStatus::Stopped => write!(f, "Stopped"),
            JobStatus::Done => write!(f, "Done"),
            JobStatus::Latched => write!(f, "Latched"),
            JobStatus::Failed => write!(f, "Failed"),
        }
    }
}

/// Information about a job for listing.
#[non_exhaustive]
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
    /// The pending confirmation-latch request when the job is gated
    /// (`JobStatus::Latched`) — the control-plane surface an embedder reads to
    /// fulfill a *backgrounded* destructive op via `Kernel::confirm`. `None`
    /// for any non-latched job. GH #96.
    pub latch: Option<LatchRequest>,
}

impl JobInfo {
    /// Create a `JobInfo` with the required fields; `output_file`/`pid`/`latch`
    /// default to `None`. Chain the `with_*` setters to fill them in.
    ///
    /// `#[non_exhaustive]` blocks struct-literal construction from outside this
    /// crate — this constructor plus the setters below are the replacement.
    pub fn new(id: JobId, command: impl Into<String>, status: JobStatus) -> Self {
        Self {
            id,
            command: command.into(),
            status,
            output_file: None,
            pid: None,
            latch: None,
        }
    }

    /// Set the output file path.
    pub fn with_output_file(mut self, output_file: Option<PathBuf>) -> Self {
        self.output_file = output_file;
        self
    }

    /// Set the OS process ID.
    pub fn with_pid(mut self, pid: Option<u32>) -> Self {
        self.pid = pid;
        self
    }

    /// Set the pending confirmation-latch request (see [`Self::latch`]).
    pub fn with_latch(mut self, latch: Option<LatchRequest>) -> Self {
        self.latch = latch;
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_defaults_optional_fields_to_none() {
        let info = JobInfo::new(JobId(1), "echo hi", JobStatus::Running);
        assert_eq!(info.id, JobId(1));
        assert_eq!(info.command, "echo hi");
        assert_eq!(info.status, JobStatus::Running);
        assert!(info.output_file.is_none());
        assert!(info.pid.is_none());
        assert!(info.latch.is_none());
    }

    #[test]
    fn with_setters_chain_and_override_defaults() {
        let info = JobInfo::new(JobId(2), "sleep 1", JobStatus::Done)
            .with_output_file(Some(PathBuf::from("job-output.txt")))
            .with_pid(Some(1234))
            .with_latch(None);
        assert_eq!(info.output_file, Some(PathBuf::from("job-output.txt")));
        assert_eq!(info.pid, Some(1234));
        assert!(info.latch.is_none());
    }
}
