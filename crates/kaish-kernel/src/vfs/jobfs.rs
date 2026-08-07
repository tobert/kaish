//! JobFs — Virtual filesystem for job observability.
//!
//! Provides `/proc`-like access to background job state:
//!
//! ```text
//! /v/jobs/
//! └── {job_id}/
//!     ├── status   ← "running" | "stopped" | "done:0" | "gated" | "killed:N" | "failed:N"
//!     ├── command  ← the original command string
//!     ├── stdout   ← the job's stdout so far, live while it runs
//!     ├── stderr   ← the job's stderr so far, live while it runs
//!     └── approval ← pending approval request (JSON) if gated, else empty
//! ```
//!
//! This is a read-only, synthesized filesystem. Content is generated from
//! the JobManager on each read.
//!
//! **`stdout`/`stderr` are live.** GH #240 removed both nodes because they
//! filled once, at job completion, while four docs promised a live stream.
//! They are back on the terms the docs claimed: an external command running
//! for the job tees each 8 KiB chunk into the job's stream as the child emits
//! it, so `cat /v/jobs/1/stdout` on a running `cargo build &` reports the
//! build's progress. Read `Job::stdout_stream` for exactly which bytes reach
//! them — in particular, a builtin has no byte stream to tee, so a
//! builtin-only job's output lands in one write when the job finishes.
//!
//! Each node is a 10 MB ring (`DEFAULT_STREAM_MAX_SIZE`) that evicts its
//! oldest bytes rather than growing without bound. A job that outruns the
//! ring loses its head, not its tail; redirect to a file
//! (`cmd > /tmp/out &`) when the whole output matters.

use async_trait::async_trait;
use std::io;
use std::path::Path;
use std::sync::Arc;

use super::{DirEntry, DirEntryKind, Filesystem};
use crate::scheduler::{JobId, JobManager};

/// Virtual filesystem providing job observability.
///
/// Mounted at `/v/jobs`, this filesystem synthesizes content from the JobManager:
/// - List root to see all job IDs as directories
/// - Read `{id}/status` for job status ("running", "stopped", "done:0", "gated", "killed:N", "failed:N")
/// - Read `{id}/command` for the original command string
/// - Read `{id}/stdout` / `{id}/stderr` for the job's output so far — live
/// - Read `{id}/approval` for a pending approval request (JSON), empty if not gated
pub struct JobFs {
    jobs: Arc<JobManager>,
}

impl JobFs {
    /// Create a new JobFs backed by the given JobManager.
    pub fn new(jobs: Arc<JobManager>) -> Self {
        Self { jobs }
    }

    /// Parse a path into job ID and file name.
    ///
    /// Expected formats:
    /// - "" or "/" → root (list jobs)
    /// - "{id}" → job directory
    /// - "{id}/{file}" → specific file (status, command, stdout, stderr, approval)
    fn parse_path(path: &Path) -> Option<(Option<JobId>, Option<&str>)> {
        let path_str = path.to_str()?;
        let path_str = path_str.trim_start_matches('/');

        if path_str.is_empty() {
            return Some((None, None)); // Root
        }

        let parts: Vec<&str> = path_str.split('/').collect();

        match parts.as_slice() {
            [id_str] => {
                // Just job ID
                let id: u64 = id_str.parse().ok()?;
                Some((Some(JobId(id)), None))
            }
            [id_str, file] => {
                // Job ID and file
                let id: u64 = id_str.parse().ok()?;
                Some((Some(JobId(id)), Some(*file)))
            }
            _ => None,
        }
    }
}

impl std::fmt::Debug for JobFs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("JobFs").finish()
    }
}

#[async_trait]
impl Filesystem for JobFs {
    async fn read(&self, path: &Path) -> io::Result<Vec<u8>> {
        let (job_id, file) = Self::parse_path(path).ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidInput, "invalid job path")
        })?;

        let job_id = job_id.ok_or_else(|| {
            io::Error::new(io::ErrorKind::IsADirectory, "cannot read directory")
        })?;

        let file = file.ok_or_else(|| {
            io::Error::new(io::ErrorKind::IsADirectory, "cannot read directory")
        })?;

        // Check job exists
        if !self.jobs.exists(job_id).await {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("job {} not found", job_id),
            ));
        }

        match file {
            "status" => {
                let status = self
                    .jobs
                    .get_status_string(job_id)
                    .await
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "job not found"))?;
                Ok(format!("{}\n", status).into_bytes())
            }
            "command" => {
                let command = self
                    .jobs
                    .get_command(job_id)
                    .await
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "job not found"))?;
                Ok(format!("{}\n", command).into_bytes())
            }
            "stdout" => {
                // Raw bytes, no trailing newline added: this is the child's
                // output verbatim, and a synthesized "\n" would corrupt
                // binary output and lie about text output that has none.
                self.jobs
                    .read_stdout(job_id)
                    .await
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "job not found"))
            }
            "stderr" => {
                self.jobs
                    .read_stderr(job_id)
                    .await
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "job not found"))
            }
            "approval" => {
                // A held job's pending approval request as JSON, so a VFS
                // consumer can read it and fulfill a backgrounded gate (GH
                // #96). Tokenless by construction (`docs/approval-ledger.md`
                // §A.2), so this node carries no credential. Empty body when
                // the job is not held — a consumer reads, then parses only if
                // non-empty.
                match self.jobs.get_approval(job_id).await {
                    Some(approval) => {
                        let json = serde_json::to_string_pretty(&approval)
                            .map_err(|e| io::Error::other(format!("approval serialize: {e}")))?;
                        Ok(format!("{json}\n").into_bytes())
                    }
                    None => Ok(Vec::new()),
                }
            }
            _ => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("unknown file: {}", file),
            )),
        }
    }

    async fn write(&self, _path: &Path, _data: &[u8]) -> io::Result<()> {
        Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            "jobfs is read-only",
        ))
    }

    async fn list(&self, path: &Path) -> io::Result<Vec<DirEntry>> {
        let (job_id, file) = Self::parse_path(path).ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidInput, "invalid job path")
        })?;

        // Can't list a file
        if file.is_some() {
            return Err(io::Error::new(
                io::ErrorKind::NotADirectory,
                "not a directory",
            ));
        }

        match job_id {
            None => {
                // List root: all job IDs as directories
                let job_ids = self.jobs.list_ids().await;
                let entries = job_ids
                    .into_iter()
                    .map(|id| DirEntry {
                        name: id.0.to_string(),
                        kind: DirEntryKind::Directory,
                        modified: None,
                        permissions: None,
                        size: 0,
                        symlink_target: None,
                    })
                    .collect();
                Ok(entries)
            }
            Some(id) => {
                // List job directory: status, command, stdout, stderr, approval
                if !self.jobs.exists(id).await {
                    return Err(io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("job {} not found", id),
                    ));
                }

                Ok(vec![
                    DirEntry {
                        name: "status".to_string(),
                        kind: DirEntryKind::File,
                        modified: None,
                        permissions: None,
                        size: 0,
                        symlink_target: None,
                    },
                    DirEntry {
                        name: "command".to_string(),
                        kind: DirEntryKind::File,
                        modified: None,
                        permissions: None,
                        size: 0,
                        symlink_target: None,
                    },
                    DirEntry {
                        name: "stdout".to_string(),
                        kind: DirEntryKind::File,
                        modified: None,
                        permissions: None,
                        // Reported as 0 like every other node here: the real
                        // size changes between this listing and the read that
                        // follows it, and a stale number is worse than none.
                        size: 0,
                        symlink_target: None,
                    },
                    DirEntry {
                        name: "stderr".to_string(),
                        kind: DirEntryKind::File,
                        modified: None,
                        permissions: None,
                        size: 0,
                        symlink_target: None,
                    },
                    DirEntry {
                        name: "approval".to_string(),
                        kind: DirEntryKind::File,
                        modified: None,
                        permissions: None,
                        size: 0, // JSON when held, empty otherwise
                        symlink_target: None,
                    },
                ])
            }
        }
    }

    async fn stat(&self, path: &Path) -> io::Result<DirEntry> {
        let (job_id, file) = Self::parse_path(path).ok_or_else(|| {
            io::Error::new(io::ErrorKind::InvalidInput, "invalid job path")
        })?;

        let name = path
            .file_name()
            .map(|n| n.to_string_lossy().into_owned())
            .unwrap_or_else(|| "/".to_string());

        match (job_id, file) {
            (None, None) => {
                // Root directory
                Ok(DirEntry::directory(name))
            }
            (Some(id), None) => {
                // Job directory
                if !self.jobs.exists(id).await {
                    return Err(io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("job {} not found", id),
                    ));
                }
                Ok(DirEntry::directory(name))
            }
            (Some(id), Some(file)) => {
                // File inside job directory
                if !self.jobs.exists(id).await {
                    return Err(io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("job {} not found", id),
                    ));
                }

                // Validate file name
                if !["status", "command", "stdout", "stderr", "approval"].contains(&file) {
                    return Err(io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("unknown file: {}", file),
                    ));
                }

                Ok(DirEntry::file(name, 0))
            }
            (None, Some(_)) => {
                // Invalid: file at root level
                Err(io::Error::new(
                    io::ErrorKind::NotFound,
                    "invalid path",
                ))
            }
        }
    }

    async fn mkdir(&self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            "jobfs is read-only",
        ))
    }

    async fn remove(&self, _path: &Path) -> io::Result<()> {
        Err(io::Error::new(
            io::ErrorKind::PermissionDenied,
            "jobfs is read-only",
        ))
    }

    fn read_only(&self) -> bool {
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::ExecResult;
    use tokio::sync::oneshot;

    async fn make_job_manager_with_job() -> (Arc<JobManager>, JobId) {
        let manager = Arc::new(JobManager::new());

        // Create channel for job completion
        let (tx, rx) = oneshot::channel();

        // Register job
        let id = manager.register("echo test".to_string(), rx).await;

        // Send result (job completes)
        let _ = tx.send(ExecResult::success("done"));

        (manager, id)
    }

    #[tokio::test]
    async fn test_list_root_empty() {
        let manager = Arc::new(JobManager::new());
        let fs = JobFs::new(manager);

        let entries = fs.list(Path::new("")).await.unwrap();
        assert!(entries.is_empty());
    }

    #[tokio::test]
    async fn test_list_root_with_jobs() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let entries = fs.list(Path::new("")).await.unwrap();
        assert_eq!(entries.len(), 1);
        assert_eq!(entries[0].name, id.0.to_string());
        assert_eq!(entries[0].kind, DirEntryKind::Directory);
    }

    #[tokio::test]
    async fn test_list_job_directory() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}", id);
        let entries = fs.list(Path::new(&path)).await.unwrap();

        let names: Vec<_> = entries.iter().map(|e| &e.name).collect();
        assert!(names.contains(&&"stdout".to_string()), "stdout node is back, and live");
        assert!(names.contains(&&"stderr".to_string()), "stderr node is back, and live");
        assert!(names.contains(&&"status".to_string()));
        assert!(names.contains(&&"command".to_string()));
        assert!(names.contains(&&"approval".to_string()));
    }

    #[tokio::test]
    async fn test_read_status_running() {
        let manager = Arc::new(JobManager::new());

        // Create a job that won't complete
        let (_tx, rx) = oneshot::channel();
        let id = manager.register("sleep 100".to_string(), rx).await;

        let fs = JobFs::new(manager);

        let path = format!("{}/status", id);
        let data = fs.read(Path::new(&path)).await.unwrap();
        assert_eq!(String::from_utf8_lossy(&data), "running\n");
    }

    #[tokio::test]
    async fn test_read_status_done() {
        let (manager, id) = make_job_manager_with_job().await;

        // Wait for job to complete
        manager.wait(id).await;

        let fs = JobFs::new(manager);

        let path = format!("{}/status", id);
        let data = fs.read(Path::new(&path)).await.unwrap();
        assert_eq!(String::from_utf8_lossy(&data), "done:0\n");
    }

    #[tokio::test]
    async fn test_read_command() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}/command", id);
        let data = fs.read(Path::new(&path)).await.unwrap();
        assert_eq!(String::from_utf8_lossy(&data), "echo test\n");
    }

    #[tokio::test]
    async fn test_stat_root() {
        let manager = Arc::new(JobManager::new());
        let fs = JobFs::new(manager);

        let entry = fs.stat(Path::new("")).await.unwrap();
        assert_eq!(entry.kind, DirEntryKind::Directory);
    }

    #[tokio::test]
    async fn test_stat_job_dir() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}", id);
        let entry = fs.stat(Path::new(&path)).await.unwrap();
        assert_eq!(entry.kind, DirEntryKind::Directory);
    }

    #[tokio::test]
    async fn test_stat_file() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}/status", id);
        let entry = fs.stat(Path::new(&path)).await.unwrap();
        assert_eq!(entry.kind, DirEntryKind::File);
    }

    #[tokio::test]
    async fn test_stat_stdout() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}/stdout", id);
        let entry = fs.stat(Path::new(&path)).await.unwrap();
        assert_eq!(entry.kind, DirEntryKind::File);
    }

    /// A job that has written nothing reads as an empty node, not an error —
    /// "nothing yet" and "no such job" stay distinguishable.
    #[tokio::test]
    async fn test_read_stdout_before_anything_is_written() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}/stdout", id);
        assert_eq!(fs.read(Path::new(&path)).await.unwrap(), Vec::<u8>::new());
    }

    /// The node reports whatever is in the stream at the moment of the read —
    /// no waiting for the job, no synthesized trailing newline.
    #[tokio::test]
    async fn test_read_stdout_reflects_the_live_stream() {
        let (manager, id) = make_job_manager_with_job().await;
        let streams = manager.streams(id).await.unwrap();
        streams.stdout.write(b"partial").await;

        let fs = JobFs::new(manager);
        let path = format!("{}/stdout", id);
        assert_eq!(fs.read(Path::new(&path)).await.unwrap(), b"partial".to_vec());
    }

    #[tokio::test]
    async fn test_stat_unknown_file_is_still_not_found() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}/nope", id);
        let result = fs.stat(Path::new(&path)).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::NotFound);
    }

    #[tokio::test]
    async fn test_read_only() {
        let manager = Arc::new(JobManager::new());
        let fs = JobFs::new(manager);

        assert!(fs.read_only());

        let write_result = fs.write(Path::new("1/status"), b"data").await;
        assert!(write_result.is_err());

        let mkdir_result = fs.mkdir(Path::new("1")).await;
        assert!(mkdir_result.is_err());

        let remove_result = fs.remove(Path::new("1")).await;
        assert!(remove_result.is_err());
    }

    #[tokio::test]
    async fn test_nonexistent_job() {
        let manager = Arc::new(JobManager::new());
        let fs = JobFs::new(manager);

        let result = fs.read(Path::new("999/status")).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::NotFound);
    }

    #[tokio::test]
    async fn test_unknown_file() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}/unknown", id);
        let result = fs.read(Path::new(&path)).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::NotFound);
    }

    #[tokio::test]
    async fn test_read_directory_error() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}", id);
        let result = fs.read(Path::new(&path)).await;
        assert!(result.is_err());
        assert_eq!(result.unwrap_err().kind(), io::ErrorKind::IsADirectory);
    }
}
