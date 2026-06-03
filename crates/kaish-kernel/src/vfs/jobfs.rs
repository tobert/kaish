//! JobFs — Virtual filesystem for job observability.
//!
//! Provides `/proc`-like access to background job state:
//!
//! ```text
//! /v/jobs/
//! └── {job_id}/
//!     ├── stdout   ← live output stream (ring buffer snapshot)
//!     ├── stderr   ← live error stream
//!     ├── status   ← "running" | "done:0" | "failed:1"
//!     └── command  ← the original command string
//! ```
//!
//! This is a read-only, synthesized filesystem. Content is generated from
//! the JobManager on each read.

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
/// - Read `{id}/stdout` for live stdout output
/// - Read `{id}/stderr` for live stderr output
/// - Read `{id}/status` for job status ("running", "done:0", "failed:N")
/// - Read `{id}/command` for the original command string
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
    /// - "{id}/{file}" → specific file (stdout, stderr, status, command)
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
            "stdout" => {
                // Return stream content, or empty if no stream attached
                let content = self.jobs.read_stdout(job_id).await.unwrap_or_default();
                Ok(content)
            }
            "stderr" => {
                let content = self.jobs.read_stderr(job_id).await.unwrap_or_default();
                Ok(content)
            }
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
                // List job directory: stdout, stderr, status, command
                if !self.jobs.exists(id).await {
                    return Err(io::Error::new(
                        io::ErrorKind::NotFound,
                        format!("job {} not found", id),
                    ));
                }

                Ok(vec![
                    DirEntry {
                        name: "stdout".to_string(),
                        kind: DirEntryKind::File,
                        modified: None,
                        permissions: None,
                        size: 0, // Dynamic content
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
                if !["stdout", "stderr", "status", "command"].contains(&file) {
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
    use crate::scheduler::BoundedStream;
    use tokio::sync::oneshot;

    async fn make_job_manager_with_job() -> (Arc<JobManager>, JobId) {
        let manager = Arc::new(JobManager::new());

        // Create streams
        let stdout = Arc::new(BoundedStream::new(1024));
        let stderr = Arc::new(BoundedStream::new(1024));

        // Write some output
        stdout.write(b"hello from stdout\n").await;
        stderr.write(b"error message\n").await;

        // Create channel for job completion
        let (tx, rx) = oneshot::channel();

        // Register job
        let id = manager
            .register_with_streams("echo test".to_string(), rx, stdout, stderr)
            .await;

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
        assert!(names.contains(&&"stdout".to_string()));
        assert!(names.contains(&&"stderr".to_string()));
        assert!(names.contains(&&"status".to_string()));
        assert!(names.contains(&&"command".to_string()));
    }

    #[tokio::test]
    async fn test_read_stdout() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}/stdout", id);
        let data = fs.read(Path::new(&path)).await.unwrap();
        assert_eq!(data, b"hello from stdout\n");
    }

    #[tokio::test]
    async fn test_read_stderr() {
        let (manager, id) = make_job_manager_with_job().await;
        let fs = JobFs::new(manager);

        let path = format!("{}/stderr", id);
        let data = fs.read(Path::new(&path)).await.unwrap();
        assert_eq!(data, b"error message\n");
    }

    #[tokio::test]
    async fn test_read_status_running() {
        let manager = Arc::new(JobManager::new());

        // Create a job that won't complete
        let stdout = Arc::new(BoundedStream::new(1024));
        let stderr = Arc::new(BoundedStream::new(1024));
        let (_tx, rx) = oneshot::channel();
        let id = manager
            .register_with_streams("sleep 100".to_string(), rx, stdout, stderr)
            .await;

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

        let path = format!("{}/stdout", id);
        let entry = fs.stat(Path::new(&path)).await.unwrap();
        assert_eq!(entry.kind, DirEntryKind::File);
    }

    #[tokio::test]
    async fn test_read_only() {
        let manager = Arc::new(JobManager::new());
        let fs = JobFs::new(manager);

        assert!(fs.read_only());

        let write_result = fs.write(Path::new("1/stdout"), b"data").await;
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

        let result = fs.read(Path::new("999/stdout")).await;
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
