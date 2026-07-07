//! Background job management for kaish.
//!
//! Provides the `JobManager` for tracking background jobs started with `&`.

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::future::Future;
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
    /// Owning manager's session ID — disambiguates output file paths between
    /// JobManager instances that share the process (and thus the same job ID
    /// space, since IDs restart at 1 per manager).
    session_id: u64,
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
    /// Whether to persist completed output to a host temp file. Disabled for
    /// hermetic / read-only kernels (custom backend, NoLocal) whose output must
    /// never reach the real filesystem outside the VFS — see
    /// [`JobManager::set_persist_output_files`]. Stamped from the manager when
    /// the job is registered. Live output is always available in-memory via the
    /// VFS streams (`/v/jobs/{id}/stdout`), so suppressing the file loses
    /// nothing for an in-process consumer.
    persist_output: bool,
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
    /// Cancellation token of the background fork running this job. Cancelling
    /// it stops the job whether it is an in-process builtin future or wraps
    /// external children (the cancellation cascade SIGTERM→SIGKILLs their
    /// process groups). This is how `kill %N` reaches a job that has no OS
    /// process group of its own (e.g. `sleep &`, a kaish builtin).
    cancel: Option<tokio_util::sync::CancellationToken>,
    /// Process groups of external children spawned while running this job.
    /// Lets `kill -<sig> %N` deliver an arbitrary signal (STOP/CONT/USR1/…)
    /// straight to the real processes via `killpg`, not just terminate. Empty
    /// for a pure-builtin job (nothing with a PGID ran).
    pgids: Vec<u32>,
}

impl Job {
    /// Create a new job from a task handle.
    pub fn new(id: JobId, session_id: u64, command: String, handle: JoinHandle<ExecResult>) -> Self {
        Self {
            id,
            session_id,
            command,
            handle: Some(handle),
            result_rx: None,
            result: None,
            output_file: None,
            persist_output: true,
            stdout_stream: None,
            stderr_stream: None,
            pid: None,
            pgid: None,
            stopped: false,
            cancel: None,
            pgids: Vec::new(),
        }
    }

    /// Create a new job from a result channel.
    pub fn from_channel(id: JobId, session_id: u64, command: String, rx: oneshot::Receiver<ExecResult>) -> Self {
        Self {
            id,
            session_id,
            command,
            handle: None,
            result_rx: Some(rx),
            result: None,
            output_file: None,
            persist_output: true,
            stdout_stream: None,
            stderr_stream: None,
            pid: None,
            pgid: None,
            stopped: false,
            cancel: None,
            pgids: Vec::new(),
        }
    }

    /// Create a new job with attached output streams.
    ///
    /// The streams provide live access to job output via `/v/jobs/{id}/stdout` and `/stderr`.
    pub fn with_streams(
        id: JobId,
        session_id: u64,
        command: String,
        rx: oneshot::Receiver<ExecResult>,
        stdout: Arc<BoundedStream>,
        stderr: Arc<BoundedStream>,
    ) -> Self {
        Self {
            id,
            session_id,
            command,
            handle: None,
            result_rx: Some(rx),
            result: None,
            output_file: None,
            persist_output: true,
            stdout_stream: Some(stdout),
            stderr_stream: Some(stderr),
            pid: None,
            pgid: None,
            stopped: false,
            cancel: None,
            pgids: Vec::new(),
        }
    }

    /// Create a stopped job (from Ctrl-Z on a foreground process).
    pub fn stopped(id: JobId, session_id: u64, command: String, pid: u32, pgid: u32) -> Self {
        Self {
            id,
            session_id,
            command,
            handle: None,
            result_rx: None,
            result: None,
            output_file: None,
            persist_output: true,
            stdout_stream: None,
            stderr_stream: None,
            pid: Some(pid),
            pgid: Some(pgid),
            stopped: true,
            cancel: None,
            pgids: Vec::new(),
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
            // A gated destructive op (exit 2 with a stored latch) is *held*,
            // not failed — surface it distinctly so `Kernel::confirm` can
            // still fulfill it (GH #96).
            Some(r) if r.latch_request().is_some() => JobStatus::Latched,
            Some(_) => JobStatus::Failed,
            None => JobStatus::Running,
        }
    }

    /// Get the job's status as a string suitable for /v/jobs/{id}/status.
    ///
    /// Returns:
    /// - `"running"` if the job is still running
    /// - `"done:0"` if the job completed successfully
    /// - `"latched"` if the job is blocked on an unfulfilled confirmation latch
    /// - `"failed:{code}"` if the job failed with an exit code
    pub fn status_string(&mut self) -> String {
        self.try_poll();
        match &self.result {
            Some(r) if r.ok() => "done:0".to_string(),
            Some(r) if r.latch_request().is_some() => "latched".to_string(),
            Some(r) => format!("failed:{}", r.code),
            None => "running".to_string(),
        }
    }

    /// The job's pending confirmation-latch request, if it is gated
    /// (`JobStatus::Latched`). `None` otherwise. Backs `JobInfo.latch` and the
    /// `/v/jobs/{id}/latch` node so a backgrounded gate is fulfillable (#96).
    ///
    /// Stamps `job_id` with this job's own id (GH #124 part 4) — the ONE
    /// chokepoint every latch-reading path (`list`/`get`/`get_latch`/
    /// `is_latched`/`cleanup`) goes through, so `Kernel::confirm` can later
    /// retire the originating job after a successful replay without every
    /// caller having to thread the id through separately.
    pub fn latch(&mut self) -> Option<kaish_types::result::LatchRequest> {
        self.try_poll();
        let id = self.id;
        self.result.as_ref().and_then(|r| r.latch_request()).map(|mut lr| {
            lr.job_id = Some(id.0);
            lr
        })
    }

    /// Get the stdout stream (if attached).
    pub fn stdout_stream(&self) -> Option<&Arc<BoundedStream>> {
        self.stdout_stream.as_ref()
    }

    /// Get the stderr stream (if attached).
    pub fn stderr_stream(&self) -> Option<&Arc<BoundedStream>> {
        self.stderr_stream.as_ref()
    }

    /// Write job output to a temp file.
    fn write_output_file(&self, result: &ExecResult) -> Option<PathBuf> {
        // This is a human-readable text log; a binary stdout is noted, not
        // dumped (lossy-decoding it would corrupt; raw bytes would garble the
        // log). Only its size is recorded.
        let is_bytes = result.is_bytes();
        let text = if is_bytes {
            std::borrow::Cow::Borrowed("")
        } else {
            result.text_out()
        };
        if !is_bytes && text.is_empty() && result.err.is_empty() {
            return None;
        }

        let tmp_dir = std::env::temp_dir().join("kaish").join("jobs");
        if std::fs::create_dir_all(&tmp_dir).is_err() {
            tracing::warn!("Failed to create job output directory");
            return None;
        }

        // Include the OS pid: `session_id` is only unique *within* a process
        // (it's a process-local atomic that restarts at 0), so two kaish
        // processes on one host — or two `cargo test` binaries — would
        // otherwise both write `session_0_job_1.txt` into this shared dir and
        // clobber each other (a real cross-process collision, and the source
        // of the `test_cleanup_removes_temp_files` flake). pid + session_id +
        // job id is unique across processes. Mirrors `output_limit`'s spill
        // filename convention.
        let filename = format!(
            "session_{}_job_{}.{}.txt",
            self.session_id,
            self.id.0,
            std::process::id()
        );
        let path = tmp_dir.join(filename);

        let mut content = String::new();
        content.push_str(&format!("# Job {}: {}\n", self.id, self.command));
        content.push_str(&format!("# Status: {}\n\n", if result.ok() { "Done" } else { "Failed" }));

        if is_bytes {
            let n = result.out_bytes().map(|b| b.len()).unwrap_or(0);
            content.push_str(&format!(
                "## STDOUT\n[binary output: {n} bytes — omitted from this text log]\n"
            ));
        } else if !text.is_empty() {
            content.push_str("## STDOUT\n");
            content.push_str(&text);
            if !text.ends_with('\n') {
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
                let Some(mut handle) = self.handle.take() else {
                    return false;
                };
                // Poll directly with a noop waker — safe because is_finished() was true
                let waker = std::task::Waker::noop();
                let mut cx = std::task::Context::from_waker(waker);
                let result = match std::pin::Pin::new(&mut handle).poll(&mut cx) {
                    std::task::Poll::Ready(Ok(r)) => r,
                    std::task::Poll::Ready(Err(e)) => {
                        ExecResult::failure(1, format!("job panicked: {}", e))
                    }
                    std::task::Poll::Pending => {
                        // is_finished() promised Ready, but if the runtime
                        // ever says Pending anyway, dropping the taken handle
                        // would strand the job as "Running" forever with its
                        // result silently lost. Put it back and retry on a
                        // later poll.
                        self.handle = Some(handle);
                        return false;
                    }
                };
                self.result = Some(result);
                return true;
            }

        false
    }
}

/// Process-wide counter handing each JobManager a distinct session ID. Job IDs
/// restart at 1 per manager, so the session ID is what keeps output file paths
/// from colliding between managers sharing a process (concurrent tests, forks).
/// It is process-LOCAL (restarts at 0 per process), so output filenames also
/// mix in the OS pid to stay unique across processes — see `write_output_file`.
static NEXT_SESSION_ID: AtomicU64 = AtomicU64::new(0);

/// Remove job output files in `/tmp/kaish/jobs/` that were written by processes
/// which are no longer running. Run once per process (guarded by a `Once` in
/// [`JobManager::new`]).
///
/// Strategy: filenames follow `session_S_job_J.PID.txt`. We parse the PID
/// component and skip files whose PID matches the current process (those
/// belong to live sessions in this very process). For other PIDs we check
/// `/proc/{pid}` on Linux; on non-Linux platforms we skip the prune entirely
/// since there is no cheap cross-platform liveness check.
///
/// All errors are intentionally ignored — this is opportunistic cleanup only.
fn prune_orphaned_job_files() {
    // Only prune on Linux where /proc/{pid} is a reliable liveness check.
    #[cfg(target_os = "linux")]
    {
        let jobs_dir = std::env::temp_dir().join("kaish").join("jobs");
        let Ok(entries) = std::fs::read_dir(&jobs_dir) else {
            return; // directory doesn't exist yet — nothing to prune
        };
        let current_pid = std::process::id();
        for entry in entries.flatten() {
            let name = entry.file_name();
            let name_str = name.to_string_lossy();
            // Expected format: session_S_job_J.PID.txt
            // The PID sits between the last '.' before ".txt" and the preceding '.'.
            let file_pid: Option<u32> = name_str
                .strip_suffix(".txt")
                .and_then(|s| s.rsplit_once('.'))
                .and_then(|(_, pid_str)| pid_str.parse().ok());
            let Some(pid) = file_pid else {
                continue; // not a job output file — skip
            };
            if pid == current_pid {
                continue; // belongs to the current process — leave it alone
            }
            // Check if the owning process is still alive via /proc.
            if std::path::Path::new(&format!("/proc/{}", pid)).exists() {
                continue; // process is still running — leave it alone
            }
            // Process is gone: remove the stale file. Error intentionally ignored.
            let _ = std::fs::remove_file(entry.path());
        }
    }
}

/// Manager for background jobs.
pub struct JobManager {
    /// Process-unique ID for this manager, mixed into job output file paths.
    session_id: u64,
    /// Counter for generating unique job IDs.
    next_id: AtomicU64,
    /// Map of job ID to job.
    jobs: Arc<Mutex<HashMap<JobId, Job>>>,
    /// Whether completed jobs persist their output to a host temp file. On by
    /// default; a hermetic / read-only kernel disables it so output never
    /// bypasses the VFS onto the real filesystem (see
    /// [`set_persist_output_files`](Self::set_persist_output_files)). Stamped
    /// onto each [`Job`] at registration.
    persist_output_files: std::sync::atomic::AtomicBool,
}

impl JobManager {
    /// Create a new job manager.
    ///
    /// On construction, best-effort prunes stale job output files left by
    /// previously crashed kaish processes. All errors are intentionally ignored
    /// — startup cleanup is opportunistic and must never prevent the manager
    /// from being created (silent-fallback rule: the only case where silent is
    /// correct is read-only / cleanup-only paths with no data loss risk).
    ///
    /// # Scoping decision
    /// All sessions share a single `/tmp/kaish/jobs/` directory. Filenames embed
    /// the OS PID that wrote them (`session_S_job_J.PID.txt`). Files from the
    /// current process are never touched here — only files whose embedded PID
    /// refers to a dead process are removed. On Linux we check `/proc/{pid}` for
    /// existence; on other platforms we skip the prune rather than guess.
    pub fn new() -> Self {
        // Orphans from dead sessions only need pruning once per process, not on
        // every JobManager (kernels + every fork build one). The `Once` keeps
        // the dir scan / `/proc` checks off the hot path of background jobs,
        // scatter workers, and pipeline stages.
        static PRUNE_ONCE: std::sync::Once = std::sync::Once::new();
        PRUNE_ONCE.call_once(prune_orphaned_job_files);
        Self {
            session_id: NEXT_SESSION_ID.fetch_add(1, Ordering::SeqCst),
            next_id: AtomicU64::new(1),
            jobs: Arc::new(Mutex::new(HashMap::new())),
            persist_output_files: std::sync::atomic::AtomicBool::new(true),
        }
    }

    /// Toggle whether completed jobs persist their output to a host temp file.
    ///
    /// Disable this for a hermetic / read-only kernel: the host write in
    /// [`Job::write_output_file`] uses `std::fs` directly and so bypasses the
    /// VFS (and any read-only mount). Live output stays available in-memory via
    /// the VFS streams (`/v/jobs/{id}/stdout`), so nothing is lost in-process.
    ///
    /// Must be set before jobs are spawned — the flag is stamped onto each job
    /// at registration time, not consulted at completion.
    pub fn set_persist_output_files(&self, on: bool) {
        self.persist_output_files.store(on, Ordering::Relaxed);
    }

    /// Whether completed jobs persist their output to a host temp file.
    pub fn persist_output_files(&self) -> bool {
        self.persist_output_files.load(Ordering::Relaxed)
    }

    /// Spawn a new background job from a future.
    ///
    /// The job is inserted into the map synchronously before returning,
    /// guaranteeing it's immediately queryable via `exists()` or `get()`.
    pub async fn spawn<F>(&self, command: String, future: F) -> JobId
    where
        F: std::future::Future<Output = ExecResult> + Send + 'static,
    {
        let id = JobId(self.next_id.fetch_add(1, Ordering::SeqCst));
        // Propagate the embedder's trace context across the spawn boundary so
        // background-job spans stay in the same trace (see telemetry module).
        let handle = tokio::spawn(crate::telemetry::bind_current_context(future));
        let mut job = Job::new(id, self.session_id, command, handle);
        job.persist_output = self.persist_output_files();

        // Insert under an async lock — NOT a busy-spin on try_lock. The old
        // sync spin could livelock the executor: on a current-thread runtime it
        // blocks the only worker thread, so a task holding the lock across an
        // await can never make progress to release it. `lock().await` yields
        // instead. The insert still completes before we return, so the job is
        // immediately queryable via `exists()`/`get()`.
        self.jobs.lock().await.insert(id, job);

        id
    }

    /// Spawn a job that's already running and communicate via channel.
    pub async fn register(&self, command: String, rx: oneshot::Receiver<ExecResult>) -> JobId {
        let id = JobId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let mut job = Job::from_channel(id, self.session_id, command, rx);
        job.persist_output = self.persist_output_files();

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
        let mut job = Job::with_streams(id, self.session_id, command, rx, stdout, stderr);
        job.persist_output = self.persist_output_files();

        let mut jobs = self.jobs.lock().await;
        jobs.insert(id, job);

        id
    }

    /// Wait for a specific job to complete.
    ///
    /// The job's pending awaitable (its task handle or result channel) is taken
    /// out of the map under the lock, then the lock is **released before**
    /// awaiting completion. Holding the `jobs` mutex across the await would
    /// block every other job operation (spawn/register/list/status/kill) for the
    /// whole duration of the job — so a nested `&` started under a parked
    /// `wait %N` would deadlock. The lock is re-acquired only to finalize
    /// (persist output, cache the result).
    pub async fn wait(&self, id: JobId) -> Option<ExecResult> {
        // Poll for completion WITHOUT removing the job's `JoinHandle` from the
        // map: `Job::try_poll` (via `is_done`) consumes the handle only once it
        // has finished. That matters two ways:
        //   * Drop-safe: a waiter dropped mid-wait (e.g. `timeout N wait %1`)
        //     never carries the handle off and orphans the result, so a later
        //     `wait %1` still completes instead of hanging.
        //   * No lock-across-await: we sleep between polls rather than holding
        //     the `jobs` mutex over the wait (which would block every other job
        //     op — the deadlock this method exists to avoid) or busy-spinning.
        // Cost is up to one poll interval of latency on completion — imperceptible
        // for a job wait, and the sleep keeps idle CPU at zero.
        loop {
            {
                let mut jobs = self.jobs.lock().await;
                let job = jobs.get_mut(&id)?;
                if job.is_done() {
                    let result = job
                        .result
                        .clone()
                        .unwrap_or_else(|| ExecResult::failure(1, "no result"));
                    // Finalize once: persist output (idempotent on output_file).
                    if job.persist_output
                        && job.output_file.is_none()
                        && let Some(path) = job.write_output_file(&result)
                    {
                        job.output_file = Some(path);
                    }
                    return Some(result);
                }
            }
            // Lock released between polls — other job ops run freely.
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
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
            .map(|job| {
                let status = job.status();
                let latch = job.latch();
                JobInfo::new(job.id, job.command.clone(), status)
                    .with_output_file(job.output_file.clone())
                    .with_pid(job.pid)
                    .with_latch(latch)
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

    /// Remove completed jobs from tracking and clean up their temp files,
    /// returning info for each job removed.
    ///
    /// A latched job is "done" but its cached result holds the only
    /// `LatchRequest` for the gated operation — reaping it would silently
    /// destroy the pending confirmation (GH #96). It stays until confirmed
    /// or explicitly discarded (`kill --discard %N`).
    ///
    /// Shared by `jobs --cleanup` (which only needs a count) and the REPL's
    /// pre-prompt notification (GH #131, which needs the id/command/status of
    /// each job so it can print `[N]+ Done ...` before reaping it) — one rule
    /// for "is this job safe to reap", not two copies that could drift.
    pub async fn reap_finished(&self) -> Vec<JobInfo> {
        let mut jobs = self.jobs.lock().await;
        let done_ids: Vec<JobId> = jobs
            .iter_mut()
            .filter_map(|(id, job)| (job.is_done() && job.latch().is_none()).then_some(*id))
            .collect();

        let mut removed = Vec::with_capacity(done_ids.len());
        for id in done_ids {
            let Some(mut job) = jobs.remove(&id) else {
                continue;
            };
            let status = job.status();
            let info = JobInfo::new(job.id, job.command.clone(), status).with_pid(job.pid);
            job.cleanup_files();
            removed.push(info);
        }
        removed
    }

    /// Remove completed jobs from tracking and clean up their temp files.
    ///
    /// See [`reap_finished`](Self::reap_finished) for the latch-safety rule;
    /// this is the count-only form `jobs --cleanup` reports.
    pub async fn cleanup(&self) {
        self.reap_finished().await;
    }

    /// Check if a specific job exists.
    pub async fn exists(&self, id: JobId) -> bool {
        let jobs = self.jobs.lock().await;
        jobs.contains_key(&id)
    }

    /// Whether the job's cached result is a pending confirmation gate
    /// (`JobStatus::Latched`). Consumers that would drop the job (`kill`,
    /// cleanup paths) check this so a latch is never destroyed silently.
    pub async fn is_latched(&self, id: JobId) -> bool {
        let mut jobs = self.jobs.lock().await;
        jobs.get_mut(&id).is_some_and(|job| job.latch().is_some())
    }

    /// Get info for a specific job.
    pub async fn get(&self, id: JobId) -> Option<JobInfo> {
        let mut jobs = self.jobs.lock().await;
        jobs.get_mut(&id).map(|job| {
            let status = job.status();
            let latch = job.latch();
            JobInfo::new(job.id, job.command.clone(), status)
                .with_output_file(job.output_file.clone())
                .with_pid(job.pid)
                .with_latch(latch)
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

    /// Get a gated job's pending confirmation-latch request (for
    /// `/v/jobs/{id}/latch` and any embedder reaching a backgrounded gate).
    /// `Some(None)` vs `None` distinguishes "job exists, not latched" from
    /// "no such job"; jobfs flattens both to an empty node body. GH #96.
    pub async fn get_latch(&self, id: JobId) -> Option<kaish_types::result::LatchRequest> {
        let mut jobs = self.jobs.lock().await;
        jobs.get_mut(&id).and_then(|job| job.latch())
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
        let job = Job::stopped(id, self.session_id, command, pid, pgid);
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

    /// Record the cancellation token of the fork running a background job, so
    /// `kill %N` can stop the job even when it has no OS process group of its
    /// own (e.g. a pure builtin like `sleep &`).
    pub async fn set_cancel_token(&self, id: JobId, token: tokio_util::sync::CancellationToken) {
        let mut jobs = self.jobs.lock().await;
        if let Some(job) = jobs.get_mut(&id) {
            job.cancel = Some(token);
        }
    }

    /// Cancel a job by its token. Returns `true` if a token was recorded and
    /// cancelled. The cancellation cascade stops in-process builtin futures and
    /// SIGTERM→SIGKILLs any external children's process groups.
    pub async fn cancel(&self, id: JobId) -> bool {
        let jobs = self.jobs.lock().await;
        match jobs.get(&id).and_then(|job| job.cancel.clone()) {
            Some(token) => {
                token.cancel();
                true
            }
            None => false,
        }
    }

    /// Record a process group spawned while running a background job. Lets
    /// `kill -<sig> %N` deliver an arbitrary signal directly to the real
    /// processes. Deduplicated (a job may spawn several externals).
    pub async fn add_pgid(&self, id: JobId, pgid: u32) {
        let mut jobs = self.jobs.lock().await;
        if let Some(job) = jobs.get_mut(&id) {
            if !job.pgids.contains(&pgid) {
                job.pgids.push(pgid);
            }
        }
    }

    /// The process groups recorded for a job (empty for a pure-builtin job).
    /// Includes the legacy single `pgid` recorded for *stopped* jobs (Ctrl-Z),
    /// so `kill %N` signals a stopped foreground job's group too.
    pub async fn job_pgids(&self, id: JobId) -> Vec<u32> {
        let jobs = self.jobs.lock().await;
        jobs.get(&id)
            .map(|job| {
                let mut v = job.pgids.clone();
                if let Some(pg) = job.pgid {
                    if !v.contains(&pg) {
                        v.push(pg);
                    }
                }
                v
            })
            .unwrap_or_default()
    }

    /// Remove a job from tracking.
    ///
    /// NOTE: this bypasses the latch guard — a caller that might hit a
    /// latched job must check [`is_latched`](Self::is_latched) first (see
    /// the `kill` builtin), or the job's pending confirmation is destroyed
    /// with it. `cleanup()` is the latch-safe bulk path.
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
    async fn test_no_host_output_file_when_persistence_disabled() {
        // A hermetic / read-only kernel (custom backend, or NoLocal mode)
        // disables host output-file persistence so a background job's output
        // never lands on the real filesystem via `std::fs`, bypassing the VFS.
        let manager = JobManager::new();
        assert!(manager.persist_output_files(), "default is to persist");
        manager.set_persist_output_files(false);
        assert!(!manager.persist_output_files());

        let id = manager.spawn("leaky".to_string(), async {
            ExecResult::success("output that must not hit host disk")
        }).await;
        tokio::time::sleep(Duration::from_millis(10)).await;
        let result = manager.wait(id).await;
        assert!(result.is_some());

        // No temp file should have been written to the host filesystem.
        let output_file = {
            let jobs = manager.jobs.lock().await;
            jobs.get(&id).and_then(|j| j.output_file().cloned())
        };
        assert!(
            output_file.is_none(),
            "no host output file should be written when persistence is disabled, got {output_file:?}"
        );
    }

    #[tokio::test]
    async fn test_spawn_and_wait() {
        let manager = JobManager::new();

        let id = manager.spawn("test".to_string(), async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            ExecResult::success("done")
        }).await;

        // Wait a bit for the job to be registered
        tokio::time::sleep(Duration::from_millis(5)).await;

        let result = manager.wait(id).await;
        assert!(result.is_some());
        let result = result.unwrap();
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "done");
    }

    #[tokio::test]
    async fn test_wait_all() {
        let manager = JobManager::new();

        manager.spawn("job1".to_string(), async {
            tokio::time::sleep(Duration::from_millis(10)).await;
            ExecResult::success("one")
        }).await;

        manager.spawn("job2".to_string(), async {
            tokio::time::sleep(Duration::from_millis(5)).await;
            ExecResult::success("two")
        }).await;

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
        }).await;

        // Wait for job to register
        tokio::time::sleep(Duration::from_millis(5)).await;

        let jobs = manager.list().await;
        assert_eq!(jobs.len(), 1);
        assert_eq!(jobs[0].command, "test job");
        assert_eq!(jobs[0].status, JobStatus::Running);
    }

    #[tokio::test]
    async fn latch_stamps_job_id_back_reference() {
        // GH #124 part 4: Job::latch() is the ONE chokepoint every latch-reading
        // path (list/get/get_latch/is_latched/cleanup) goes through, so it must
        // stamp the job's own id onto the surfaced LatchRequest -- otherwise
        // Kernel::confirm has no way to know which job to retire after a
        // successful replay.
        let manager = JobManager::new();

        let id = manager.spawn("gated".to_string(), async {
            let mut result = ExecResult::failure(2, "confirmation required");
            result.latch = Some(Box::new(kaish_types::result::LatchRequest {
                nonce: "a3f7b2c1".to_string(),
                command: "rm".to_string(),
                paths: vec!["x".to_string()],
                hint: "rm --confirm=a3f7b2c1 x".to_string(),
                tool: "rm".to_string(),
                argv: vec!["x".to_string()],
                ttl: 60,
                job_id: None, // unset at construction -- Job::latch() must fill it in
            }));
            result
        }).await;

        tokio::time::sleep(Duration::from_millis(10)).await;

        let latch = manager.get_latch(id).await.expect("job must be latched");
        assert_eq!(
            latch.job_id,
            Some(id.0),
            "Job::latch() must stamp this job's own id onto the surfaced request"
        );
    }

    #[tokio::test]
    async fn test_job_status_after_completion() {
        let manager = JobManager::new();

        let id = manager.spawn("quick".to_string(), async {
            ExecResult::success("")
        }).await;

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
        }).await;

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
        }).await;

        // Wait for completion (triggers output file creation)
        tokio::time::sleep(Duration::from_millis(10)).await;
        let result = manager.wait(id).await;
        assert!(result.is_some());

        // Get the output file path before cleanup. The job produced output, so
        // a temp file must have been written — otherwise this test would pass
        // vacuously.
        let output_file = {
            let jobs = manager.jobs.lock().await;
            jobs.get(&id).and_then(|j| j.output_file().cloned())
        };
        let path = output_file.expect("job with output should have written a temp file");
        assert!(path.exists(), "temp file should exist before cleanup: {}", path.display());

        // Cleanup should remove the job and its files.
        manager.cleanup().await;

        assert!(
            !path.exists(),
            "temp file should be removed after cleanup: {}",
            path.display()
        );
    }

    #[tokio::test]
    async fn test_reap_finished_returns_removed_job_info() {
        // GH #131: the REPL's pre-prompt notification needs the id/command/
        // status of each reaped job, not just a count.
        let manager = JobManager::new();
        manager.set_persist_output_files(false);

        let id = manager
            .spawn("sleep 0.1".to_string(), async { ExecResult::success("") })
            .await;
        tokio::time::sleep(Duration::from_millis(10)).await;
        let _ = manager.wait(id).await;

        let removed = manager.reap_finished().await;
        assert_eq!(removed.len(), 1);
        assert_eq!(removed[0].id, id);
        assert_eq!(removed[0].command, "sleep 0.1");
        assert_eq!(removed[0].status, JobStatus::Done);

        // And it's actually gone from tracking.
        assert!(manager.list().await.is_empty());
    }

    #[tokio::test]
    async fn test_reap_finished_never_reaps_latched_jobs() {
        // GH #131 / GH #96: a Latched job is "done" in the sense that its
        // future resolved, but it's awaiting confirmation of a pending
        // destructive-operation gate — reaping it would silently destroy the
        // only copy of the LatchRequest. Must never be auto-reaped or
        // reported as a finished job.
        use kaish_types::result::LatchRequest;

        let manager = JobManager::new();
        manager.set_persist_output_files(false);
        let (tx, rx) = oneshot::channel();
        let id = manager.register("rm precious.txt".to_string(), rx).await;

        let mut gated = ExecResult::failure(2, "rm: confirmation required (latch enabled)");
        gated.latch = Some(Box::new(LatchRequest {
            nonce: "a3f7b2c1".to_string(),
            command: "rm".to_string(),
            paths: vec!["precious.txt".to_string()],
            hint: "rm --confirm=\"a3f7b2c1\" precious.txt".to_string(),
            tool: "rm".to_string(),
            argv: vec!["precious.txt".to_string()],
            ttl: 60,
            job_id: None,
        }));
        tx.send(gated).expect("send gated result");
        tokio::time::sleep(Duration::from_millis(10)).await;

        // Confirm it's actually seen as Latched before reaping.
        let info = manager.get(id).await.expect("job exists");
        assert_eq!(info.status, JobStatus::Latched);

        let removed = manager.reap_finished().await;
        assert!(
            removed.is_empty(),
            "a latched job must never be auto-reaped: {removed:?}"
        );
        assert_eq!(
            manager.list().await.len(),
            1,
            "the latched job must still be tracked so its gate can be fulfilled"
        );
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
        assert_eq!(&*result.unwrap().text_out(), "from channel");
    }

    #[tokio::test]
    async fn test_spawn_immediately_available() {
        // Bug J: job should be queryable immediately after spawn()
        let manager = JobManager::new();

        let id = manager.spawn("instant".to_string(), async {
            tokio::time::sleep(Duration::from_millis(100)).await;
            ExecResult::success("done")
        }).await;

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

    #[tokio::test]
    async fn test_cancel_token_fires() {
        // A recorded cancel token can be tripped by id — this is how `kill %N`
        // stops a pure-builtin job that has no OS process group.
        let manager = JobManager::new();
        let token = tokio_util::sync::CancellationToken::new();
        let id = manager.spawn("bg".to_string(), async { ExecResult::success("") }).await;
        manager.set_cancel_token(id, token.clone()).await;

        assert!(!token.is_cancelled());
        assert!(manager.cancel(id).await, "cancel should report success");
        assert!(token.is_cancelled(), "the job's token must be tripped");
    }

    #[tokio::test]
    async fn test_cancel_without_token_returns_false() {
        let manager = JobManager::new();
        let id = manager.spawn("bg".to_string(), async { ExecResult::success("") }).await;
        // No token recorded → nothing to cancel.
        assert!(!manager.cancel(id).await);
        // Unknown id → also false.
        assert!(!manager.cancel(JobId(999)).await);
    }

    #[tokio::test]
    async fn test_pgids_recorded_and_deduped() {
        let manager = JobManager::new();
        let id = manager.spawn("bg".to_string(), async { ExecResult::success("") }).await;
        assert!(manager.job_pgids(id).await.is_empty());

        manager.add_pgid(id, 4242).await;
        manager.add_pgid(id, 4243).await;
        manager.add_pgid(id, 4242).await; // duplicate ignored
        assert_eq!(manager.job_pgids(id).await, vec![4242, 4243]);

        // Unknown id → empty, no panic.
        assert!(manager.job_pgids(JobId(999)).await.is_empty());
    }

    #[tokio::test]
    async fn wait_does_not_block_other_job_ops() {
        // Regression: `wait(id)` must NOT hold the jobs mutex across the job's
        // completion. The buggy version did, so while a `wait %N` was parked,
        // every other job op (list/spawn/status) blocked until the job finished
        // — a nested `&` under `wait %N` deadlocked. (Also covers the old
        // `spawn` try_lock busy-spin, which on a current-thread runtime livelocked
        // the executor when the lock was held.)
        let manager = Arc::new(JobManager::new());
        manager.set_persist_output_files(false);

        // A job that blocks until we release it.
        let (tx, rx) = oneshot::channel::<()>();
        let id = manager
            .spawn("blocker".to_string(), async move {
                let _ = rx.await;
                ExecResult::success("done")
            })
            .await;

        // Park a waiter on it (in the buggy version, holds the lock for the
        // job's whole lifetime).
        let waiter = {
            let m = manager.clone();
            tokio::spawn(async move { m.wait(id).await })
        };
        // Let the waiter acquire the lock and park on the job's completion.
        tokio::time::sleep(Duration::from_millis(50)).await;

        // Other job ops must stay responsive while the waiter is parked.
        let listed = tokio::time::timeout(Duration::from_secs(2), manager.list()).await;
        assert!(
            listed.is_ok(),
            "list() blocked while wait() was parked — jobs lock held across await"
        );
        let second = tokio::time::timeout(
            Duration::from_secs(2),
            manager.spawn("second".to_string(), async { ExecResult::success("2") }),
        )
        .await;
        assert!(
            second.is_ok(),
            "spawn() blocked/spun while wait() was parked"
        );

        // Release the job; the parked waiter must observe the result.
        let _ = tx.send(());
        let result = tokio::time::timeout(Duration::from_secs(2), waiter)
            .await
            .expect("waiter join timed out")
            .expect("waiter task panicked");
        assert_eq!(result.map(|r| r.code), Some(0), "waiter should see exit 0");
    }

    #[tokio::test(flavor = "multi_thread", worker_threads = 2)]
    async fn wait_survives_a_dropped_waiter() {
        // Regression (Gemini review): a waiter dropped mid-wait (e.g.
        // `timeout N wait %1`) must NOT orphan the job's result. The buggy
        // version took the JoinHandle out to await it, so dropping that waiter
        // detached the task and lost its result, and a SECOND `wait %1` then
        // hung forever (busy-spinning in the AlreadyWaiting branch). `wait` must
        // never take the handle until it's finished.
        let manager = Arc::new(JobManager::new());
        manager.set_persist_output_files(false);

        let (tx, rx) = oneshot::channel::<()>();
        let id = manager
            .spawn("blocker".to_string(), async move {
                let _ = rx.await;
                ExecResult::success("done")
            })
            .await;

        // Waiter A parks on the job, then is aborted (dropped) before it finishes.
        {
            let m = manager.clone();
            let a = tokio::spawn(async move { m.wait(id).await });
            tokio::time::sleep(Duration::from_millis(20)).await;
            a.abort();
            let _ = a.await;
        }

        // The job completes after A is gone.
        let _ = tx.send(());

        // Waiter B must still observe the result, not hang.
        let res = tokio::time::timeout(Duration::from_secs(2), manager.wait(id))
            .await
            .expect("wait must not hang after a prior waiter was dropped");
        assert_eq!(res.map(|r| r.code), Some(0), "B should see the completed job");
    }
}
