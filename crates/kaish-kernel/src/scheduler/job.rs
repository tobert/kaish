//! Background job management for kaish.
//!
//! Provides the `JobManager` for tracking background jobs started with `&`.

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU64, Ordering};
use std::future::Future;
use std::sync::Arc;
use std::time::SystemTime;

use tokio::sync::{oneshot, Mutex};
use tokio::task::JoinHandle;

use crate::interpreter::ExecResult;
use crate::scheduler::stream::BoundedStream;

// Data types re-exported from kaish-types.
pub use kaish_types::{JobId, JobInfo, JobStatus};

/// One job's live output streams.
///
/// Handed out by [`JobManager::streams`] so a producer (the drain task behind
/// an external command) can write into a job that outlives the command, and so
/// an embedder can tail a running job with
/// [`BoundedStream::changed_since`] instead of a poll loop.
#[derive(Clone)]
pub struct JobStreams {
    /// The job's stdout, written as the bytes arrive.
    ///
    /// Fed two ways, and never both for the same bytes:
    ///
    /// * **Live**, per 8 KiB chunk, by the drain task behind an external
    ///   command running for this job — but only from the stage whose stdout
    ///   *is* the job's stdout (`Only` or `Last` in the pipeline), so
    ///   `a | b` streams `b` and not `a`'s bytes on their way into `b`.
    /// * **At completion**, from the job's captured `ExecResult`, and only
    ///   when nothing was streamed live. That covers a builtin-only job
    ///   (`echo hi &`): a builtin returns its output as a value when it
    ///   finishes, so there is no byte stream to tee.
    ///
    /// Whichever fed it, the stream is closed once the job's result is in
    /// ([`JobManager::finalize_streams`]), so a reader can tell "no more
    /// coming" from "nothing yet".
    pub stdout: Arc<BoundedStream>,
    /// The job's stderr. Same two feeds as [`Self::stdout`], except the live
    /// one takes **every** stage's stderr, not just the last — stderr is not
    /// piped between stages. The consequence, stated rather than papered
    /// over: in a job mixing builtins and externals, once any external has
    /// written stderr the completion write is skipped, so a builtin stage's
    /// stderr stays in the job's `ExecResult` and does not reach this stream.
    pub stderr: Arc<BoundedStream>,
}

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
    /// the job is registered. A hermetic kernel still reads the job's output
    /// from its live streams (`/v/jobs/{id}/stdout`, or
    /// [`JobManager::read_stdout`]) — the host file is a convenience, not the
    /// only copy.
    persist_output: bool,
    /// Live stdout of this job. Handed out as [`JobStreams::stdout`], which
    /// documents exactly which bytes reach it.
    stdout_stream: Arc<BoundedStream>,
    /// Live stderr of this job. See [`JobStreams::stderr`].
    stderr_stream: Arc<BoundedStream>,
    /// OS process ID (for stopped jobs).
    pid: Option<u32>,
    /// OS process group ID (for stopped jobs).
    pgid: Option<u32>,
    /// Whether this job is stopped (SIGTSTP).
    stopped: bool,
    /// Whether a terminating kill was dispatched at this job (`kill %N`, or
    /// an embedder's cancel). Once the job unwinds, this turns its terminal
    /// status into `Killed` instead of `Failed` — "someone killed it" and
    /// "it errored on its own" stay distinguishable after the fact (GH #244).
    /// A job that manages to finish successfully before the cascade lands
    /// still reports `Done`: the result is the truth, the flag only colors
    /// a non-ok exit.
    killed: bool,
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
    /// Wall-clock time this job started running, stamped at construction.
    /// Acquired via `kaish_types::clock::system_now` (not `SystemTime::now()`
    /// directly) so this stays valid on `wasm32-unknown-unknown`. Surfaced on
    /// `JobInfo.started_at` (GH #243).
    started_at: SystemTime,
    /// Wall-clock time this job's result became available, stamped once by
    /// `try_poll` the moment `self.result` transitions from `None` to `Some`.
    /// Surfaced on `JobInfo.finished_at` (GH #243).
    finished_at: Option<SystemTime>,
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
            stdout_stream: Arc::new(BoundedStream::default_size()),
            stderr_stream: Arc::new(BoundedStream::default_size()),
            pid: None,
            pgid: None,
            stopped: false,
            killed: false,
            cancel: None,
            pgids: Vec::new(),
            started_at: kaish_types::clock::system_now(),
            finished_at: None,
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
            stdout_stream: Arc::new(BoundedStream::default_size()),
            stderr_stream: Arc::new(BoundedStream::default_size()),
            pid: None,
            pgid: None,
            stopped: false,
            killed: false,
            cancel: None,
            pgids: Vec::new(),
            started_at: kaish_types::clock::system_now(),
            finished_at: None,
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
            stdout_stream: Arc::new(BoundedStream::default_size()),
            stderr_stream: Arc::new(BoundedStream::default_size()),
            pid: Some(pid),
            pgid: Some(pgid),
            stopped: true,
            killed: false,
            cancel: None,
            pgids: Vec::new(),
            // The foreground process actually started earlier (before Ctrl-Z
            // stopped it into job tracking), but kaish had no job entry for it
            // until now — "now" is the best available approximation, and a
            // strict improvement over having no timestamp at all.
            started_at: kaish_types::clock::system_now(),
            finished_at: None,
        }
    }

    /// Get the output file path (if available).
    pub fn output_file(&self) -> Option<&PathBuf> {
        self.output_file.as_ref()
    }

    /// This job's live output streams (see [`JobStreams`]).
    pub fn streams(&self) -> JobStreams {
        JobStreams {
            stdout: self.stdout_stream.clone(),
            stderr: self.stderr_stream.clone(),
        }
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
            Some(_) if self.killed => JobStatus::Killed,
            Some(_) => JobStatus::Failed,
            None => JobStatus::Running,
        }
    }

    /// Get the job's status as a string suitable for /v/jobs/{id}/status.
    ///
    /// Returns:
    /// - `"running"` if the job is still running
    /// - `"stopped"` if the job is stopped (Ctrl-Z / SIGTSTP)
    /// - `"done:0"` if the job completed successfully
    /// - `"gated"` if the job is held on an unsatisfied approval gate
    /// - `"killed:{code}"` if the job was terminated by `kill %N`
    /// - `"failed:{code}"` if the job failed with an exit code
    ///
    /// The vocabulary must stay in step with [`Self::status`] — GH #252 was
    /// exactly this pair drifting: `status()` learned the `stopped` check and
    /// this string twin didn't, so `/v/jobs/N/status` reported a Ctrl-Z'd job
    /// as `"running"` forever (a stopped job has no result channel, so
    /// `try_poll` can never produce a result).
    pub fn status_string(&mut self) -> String {
        if self.stopped {
            return "stopped".to_string();
        }
        self.try_poll();
        match &self.result {
            Some(r) if r.ok() => "done:0".to_string(),
            Some(r) if self.killed => format!("killed:{}", r.code),
            Some(r) => format!("failed:{}", r.code),
            None => "running".to_string(),
        }
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
        // Same terminal-status words as `status()` — a killed job's persisted
        // log must not claim it "Failed" on its own (GH #244 review finding).
        let status = if result.ok() {
            "Done"
        } else if self.killed {
            "Killed"
        } else {
            "Failed"
        };
        content.push_str(&format!("# Status: {status}\n\n"));

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
                    self.finished_at = Some(kaish_types::clock::system_now());
                    return true;
                }
                Err(tokio::sync::oneshot::error::TryRecvError::Empty) => {
                    // Still running
                    return false;
                }
                Err(tokio::sync::oneshot::error::TryRecvError::Closed) => {
                    // The sender dropped without sending a result — the
                    // spawned task's future panicked and unwound before
                    // `tx.send(result)` ran (GH #247). `execute_background`
                    // uses this oneshot-channel path exclusively for every
                    // `&` job, so this is the ONLY place a background-job
                    // panic surfaces; a wording indistinguishable from an
                    // ordinary `exit 1` ("job channel closed", previously)
                    // hid a kernel bug behind what read as a normal command
                    // failure, and the case went to `tracing::error!` for
                    // the first time here — it was not logged at all before.
                    tracing::error!(
                        job_id = %self.id,
                        command = %self.command,
                        "background job task ended without producing a result — \
                         its future likely panicked"
                    );
                    self.result = Some(ExecResult::failure(
                        1,
                        format!(
                            "job {}: task ended without a result (likely a kernel panic, \
                             not the command's own exit) — see the kernel's logs",
                            self.id
                        ),
                    ));
                    self.result_rx = None;
                    self.finished_at = Some(kaish_types::clock::system_now());
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
                self.finished_at = Some(kaish_types::clock::system_now());
                return true;
            }

        false
    }

    /// The process groups recorded for this job, combining `pgids` (from
    /// externals spawned while running) with the legacy single `pgid`
    /// recorded for a *stopped* foreground job — the single accessor `list`/
    /// `get`/`reap_finished` use to fill `JobInfo.pgids` (GH #243), and what
    /// `JobManager::job_pgids` delegates to so the combine logic exists once.
    fn pgids_combined(&self) -> Vec<u32> {
        let mut v = self.pgids.clone();
        if let Some(pg) = self.pgid
            && !v.contains(&pg)
        {
            v.push(pg);
        }
        v
    }

    /// Build the full `JobInfo` snapshot for this job. `status`/`approval` are
    /// taken as parameters rather than recomputed here because both require
    /// `&mut self` (they poll) — callers (`list`/`get`/`reap_finished`)
    /// already did that poll to decide reap-safety before calling this. The
    /// single chokepoint that populates every `JobInfo` field (GH #243), so
    /// the three call sites can't drift on which fields they remember to set.
    fn to_info(&self, status: JobStatus) -> JobInfo {
        let exit_code = self.result.as_ref().map(|r| r.code);
        JobInfo::new(self.id, self.command.clone(), status)
            .with_output_file(self.output_file.clone())
            .with_pid(self.pid)
            .with_exit_code(exit_code)
            .with_started_at(self.started_at)
            .with_finished_at(self.finished_at)
            .with_pgids(self.pgids_combined())
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
    /// SIGTERM→SIGKILL grace of the cancellation cascade, in milliseconds —
    /// mirrored from `KernelConfig::kill_grace` at kernel construction so the
    /// `kill` builtin can bound its wait-for-death on the same number the
    /// cascade actually uses (GH #244). Milliseconds in an atomic rather than
    /// a locked `Duration` because readers are on the kill path and never
    /// need sub-millisecond precision.
    kill_grace_ms: AtomicU64,
    /// How many finished jobs stay tracked before the oldest are reaped to
    /// make room (GH #244: nothing auto-reaped, so a long-lived embedder
    /// accumulated results, streams, and temp files without bound). Enforced
    /// at registration time — see [`Self::enforce_retention_locked`] for what
    /// "finished" excludes (gated and stopped jobs are never evicted).
    finished_retention: AtomicU64,
}

/// Default for [`JobManager::set_finished_retention`]: keep the last 100
/// finished jobs. Interactive sessions never notice (the REPL reaps every
/// prompt); an embedder that never reaps stays bounded.
pub const DEFAULT_FINISHED_RETENTION: u64 = 100;

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
            kill_grace_ms: AtomicU64::new(2_000),
            finished_retention: AtomicU64::new(DEFAULT_FINISHED_RETENTION),
        }
    }

    /// Mirror `KernelConfig::kill_grace` onto the manager (see the field doc).
    pub fn set_kill_grace(&self, grace: std::time::Duration) {
        self.kill_grace_ms
            .store(grace.as_millis().min(u128::from(u64::MAX)) as u64, Ordering::Relaxed);
    }

    /// The cancellation cascade's SIGTERM→SIGKILL grace (see [`Self::set_kill_grace`]).
    pub fn kill_grace(&self) -> std::time::Duration {
        std::time::Duration::from_millis(self.kill_grace_ms.load(Ordering::Relaxed))
    }

    /// Set how many finished jobs stay tracked (default 100,
    /// `DEFAULT_FINISHED_RETENTION`). `0` keeps no finished jobs beyond the
    /// gate-safety rule — gated and stopped jobs are never evicted regardless.
    pub fn set_finished_retention(&self, keep: u64) {
        self.finished_retention.store(keep, Ordering::Relaxed);
    }

    /// Toggle whether completed jobs persist their output to a host temp file.
    ///
    /// Disable this for a hermetic / read-only kernel: the host write in
    /// `Job::write_output_file` uses `std::fs` directly and so bypasses the
    /// VFS (and any read-only mount). Turning it off costs a hermetic kernel
    /// nothing it cannot get elsewhere — the job's output is in its live
    /// streams (`/v/jobs/{id}/stdout`, or [`JobManager::read_stdout`]), bounded by a
    /// 10 MB ring. Redirect to a VFS path (`cmd > /tmp/out &`) when a job
    /// outruns that ring.
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
        let mut jobs = self.jobs.lock().await;
        jobs.insert(id, job);
        self.enforce_retention_locked(&mut jobs);

        id
    }

    /// Spawn a job that's already running and communicate via channel.
    pub async fn register(&self, command: String, rx: oneshot::Receiver<ExecResult>) -> JobId {
        let id = JobId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let mut job = Job::from_channel(id, self.session_id, command, rx);
        job.persist_output = self.persist_output_files();

        let mut jobs = self.jobs.lock().await;
        jobs.insert(id, job);
        self.enforce_retention_locked(&mut jobs);

        id
    }

    /// A job's live output streams, or `None` if there is no such job.
    ///
    /// The producer side: `Kernel::try_execute_external` takes these for the
    /// job it is running under and tees the child's pipes into them as the
    /// bytes arrive.
    pub async fn streams(&self, id: JobId) -> Option<JobStreams> {
        let jobs = self.jobs.lock().await;
        jobs.get(&id).map(|job| job.streams())
    }

    /// Snapshot a job's stdout so far, or `None` if there is no such job.
    ///
    /// **Readable while the job runs** — that is the point. `None` and
    /// `Some(vec![])` are different answers: no such job, versus a job that
    /// has not written anything yet.
    pub async fn read_stdout(&self, id: JobId) -> Option<Vec<u8>> {
        let stream = self.streams(id).await?.stdout;
        Some(stream.read().await)
    }

    /// Snapshot a job's stderr so far, or `None` if there is no such job.
    /// See [`Self::read_stdout`].
    pub async fn read_stderr(&self, id: JobId) -> Option<Vec<u8>> {
        let stream = self.streams(id).await?.stderr;
        Some(stream.read().await)
    }

    /// Close out a finished job's streams: write the captured result into a
    /// stream that received nothing live, then close both.
    ///
    /// The conditional is the no-double-write rule. A stream with live bytes
    /// in it already holds exactly what the child emitted; writing
    /// `result.text_out()` on top would repeat all of it. A stream with no
    /// live bytes belongs to a job with nothing to tee — a builtin returns
    /// its output as a value, not as a pipe — and would otherwise read empty
    /// forever.
    ///
    /// Called by the background task that owns the job, before it hands the
    /// result over, so a reader that sees a terminal `status` also sees a
    /// closed, complete stream.
    pub async fn finalize_streams(&self, id: JobId, result: &ExecResult) {
        let Some(streams) = self.streams(id).await else {
            return;
        };

        if streams.stdout.stats().await.total_written == 0 {
            // Raw bytes when the payload is binary; `text_out` would decode it
            // lossily and corrupt what a caller reads back out of the node.
            match result.out_bytes() {
                Some(bytes) => streams.stdout.write(bytes).await,
                None => streams.stdout.write(result.text_out().as_bytes()).await,
            }
        }
        if streams.stderr.stats().await.total_written == 0 {
            streams.stderr.write(result.err.as_bytes()).await;
        }

        streams.stdout.close().await;
        streams.stderr.close().await;
    }

    /// Wait for a specific job to complete.
    ///
    /// Returns `None` when the job does not exist **or is stopped** — a stopped
    /// job can never finish, so waiting on one would hang forever. Callers that
    /// need to tell the two apart check [`JobManager::get`] after a `None`.
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
                // A stopped job can never finish: `is_done()` returns `false`
                // for as long as it is stopped, so polling would spin forever —
                // the same hang `wait_all`'s stopped-skip closes, reachable
                // here directly (`wait %N` on a Ctrl-Z'd job) and by a job
                // stopping *after* `wait_all` took its snapshot (the bg reaper
                // observes a SIGSTOP and flips the flag mid-wait). Bail loud;
                // the caller resumes with `bg`/`fg` and waits again.
                if job.stopped {
                    return None;
                }
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
                    // A completion was just observed — enforce retention here
                    // too, not only at registration (an embedder that stops
                    // registering must still stay bounded). The result is
                    // already cloned, so evicting this very job (cap 0) is
                    // safe.
                    self.enforce_retention_locked(&mut jobs);
                    return Some(result);
                }
            }
            // Lock released between polls — other job ops run freely.
            tokio::time::sleep(std::time::Duration::from_millis(10)).await;
        }
    }

    /// Wait for all jobs that can still finish, returning results in completion
    /// order.
    ///
    /// **Stopped jobs are skipped, and that is load-bearing.** A Ctrl-Z'd job is
    /// registered by [`JobManager::register_stopped`] with no `JoinHandle` and no
    /// result channel, and [`Job::is_done`] returns `false` for as long as it is
    /// stopped — so nothing can ever make it done. Waiting on one here spun the
    /// 10ms poll loop forever, and since [`crate::Kernel::shutdown`] calls this, a single
    /// Ctrl-Z hung shutdown with no timeout and no escape. Skip them: `wait_all`
    /// means "wait for everything that will finish", not "wait for everything".
    ///
    /// The filter below is a snapshot; a job that stops *after* it (the bg
    /// reaper observing a SIGSTOP) is caught by [`JobManager::wait`]'s own
    /// stopped guard, which returns `None` instead of re-creating the hang.
    ///
    /// A caller that wants a stopped job to finish must resume it first (`bg`/`fg`).
    pub async fn wait_all(&self) -> Vec<(JobId, ExecResult)> {
        let mut results = Vec::new();

        let ids: Vec<JobId> = {
            let jobs = self.jobs.lock().await;
            jobs.iter()
                .filter(|(_, job)| !job.stopped)
                .map(|(id, _)| *id)
                .collect()
        };

        for id in ids {
            if let Some(result) = self.wait(id).await {
                results.push((id, result));
            }
        }

        results
    }

    /// List all jobs with their status.
    ///
    /// Listing polls every job, so this is also a completion-observation
    /// point: retention is enforced here (after the snapshot is taken — the
    /// returned list is complete even for entries evicted by it).
    ///
    /// Sorted by [`JobId`] (GH #247) — the backing map is a `HashMap`, whose
    /// iteration order is arbitrary and was leaking straight through to
    /// `jobs`, `/v/jobs`, and `--json`: two jobs could list as `[2, 1]`. An
    /// MCP caller handed that order, or a snapshot test pinned against it,
    /// saw a flake with no code change — sorting makes the order a stated
    /// contract instead of whatever the hasher happened to do.
    pub async fn list(&self) -> Vec<JobInfo> {
        let mut jobs = self.jobs.lock().await;
        let mut infos: Vec<JobInfo> = jobs
            .values_mut()
            .map(|job| {
                let status = job.status();
                job.to_info(status)
            })
            .collect();
        infos.sort_by_key(|info| info.id);
        self.enforce_retention_locked(&mut jobs);
        infos
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
    /// A held job is "done" but its cached result holds the only pending
    /// approval request for the gated operation — reaping it would silently
    /// destroy the reference an embedder needs to fulfill it (GH #96). It
    /// stays until confirmed or explicitly discarded (`kill --discard %N`).
    ///
    /// Shared by `jobs --cleanup` (which only needs a count) and the REPL's
    /// pre-prompt notification (GH #131, which needs the id/command/status of
    /// each job so it can print `[N]+ Done ...` before reaping it) — one rule
    /// for "is this job safe to reap", not two copies that could drift.
    pub async fn reap_finished(&self) -> Vec<JobInfo> {
        let mut jobs = self.jobs.lock().await;
        let done_ids: Vec<JobId> = jobs
            .iter_mut()
            .filter_map(|(id, job)| job.is_done().then_some(*id))
            .collect();

        let mut removed = Vec::with_capacity(done_ids.len());
        for id in done_ids {
            let Some(mut job) = jobs.remove(&id) else {
                continue;
            };
            let status = job.status();
            let info = job.to_info(status);
            job.cleanup_files();
            removed.push(info);
        }
        removed
    }

    /// Remove completed jobs from tracking and clean up their temp files.
    ///
    /// See [`reap_finished`](Self::reap_finished) for the gate-safety rule;
    /// this is the count-only form `jobs --cleanup` reports.
    pub async fn cleanup(&self) {
        self.reap_finished().await;
    }

    /// Evict the oldest finished jobs beyond the retention cap
    /// ([`Self::set_finished_retention`]). Called with the jobs lock held at
    /// registration time (the moment the tracked-job count grows) **and** at
    /// the completion-observation points (`list`, `wait`'s finalize) — so an
    /// embedder that stops registering but keeps observing stays bounded
    /// without a background sweeper (GH #244). A session that registers jobs
    /// and then never calls anything at all holds what it registered; there
    /// is no sweeper task by design. "Finished" follows `reap_finished`'s reap-safety
    /// rule: gated jobs are never evicted (their cached result holds the only
    /// pending approval request) and stopped jobs are not finished. Eviction
    /// is oldest `finished_at` first, so the survivors are the newest N.
    fn enforce_retention_locked(&self, jobs: &mut HashMap<JobId, Job>) {
        let keep = self.finished_retention.load(Ordering::Relaxed) as usize;
        let mut finished: Vec<(JobId, SystemTime)> = jobs
            .iter_mut()
            .filter_map(|(id, job)| {
                job.is_done()
                    .then(|| (*id, job.finished_at.unwrap_or(job.started_at)))
            })
            .collect();
        if finished.len() <= keep {
            return;
        }
        // Job IDs tie-break equal timestamps (they grow monotonically), so
        // eviction order is deterministic even under a coarse clock.
        finished.sort_by_key(|&(id, finished_at)| (finished_at, id.0));
        let evict = finished.len() - keep;
        for (id, _) in finished.into_iter().take(evict) {
            if let Some(mut job) = jobs.remove(&id) {
                job.cleanup_files();
            }
        }
    }

    /// Check if a specific job exists.
    pub async fn exists(&self, id: JobId) -> bool {
        let jobs = self.jobs.lock().await;
        jobs.contains_key(&id)
    }

    /// Get info for a specific job.
    pub async fn get(&self, id: JobId) -> Option<JobInfo> {
        let mut jobs = self.jobs.lock().await;
        jobs.get_mut(&id).map(|job| {
            let status = job.status();
            job.to_info(status)
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

    /// List all job IDs, sorted (GH #247 — see [`Self::list`]'s doc for why
    /// the backing `HashMap`'s iteration order is not good enough here: this
    /// backs the `/v/jobs` directory listing via [`crate::vfs::JobFs`]).
    pub async fn list_ids(&self) -> Vec<JobId> {
        let jobs = self.jobs.lock().await;
        let mut ids: Vec<JobId> = jobs.keys().copied().collect();
        ids.sort();
        ids
    }

    /// Register a stopped job (from Ctrl-Z on a foreground process).
    pub async fn register_stopped(&self, command: String, pid: u32, pgid: u32) -> JobId {
        let id = JobId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let job = Job::stopped(id, self.session_id, command, pid, pgid);
        let mut jobs = self.jobs.lock().await;
        jobs.insert(id, job);
        self.enforce_retention_locked(&mut jobs);
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

    /// Flag a terminating kill and trip the job's cancellation token, as one
    /// operation under the jobs lock. Returns `false` — and leaves the job
    /// **unflagged** — when there is no lever to kill with: no cancellation
    /// token recorded and no OS signal already delivered (`delivered`). The
    /// flag turns the job's terminal status into `Killed`, so setting it
    /// without a working delivery would misclassify a later *organic* failure
    /// as a kill (found in review: `JobManager::spawn`/`register` jobs have
    /// no token unless the kernel records one).
    ///
    /// The flag is set *before* the token trips (the job can unwind the
    /// instant it does; a flag set after races the status read), and the
    /// token is cancelled after the lock drops — `CancellationToken::cancel`
    /// is synchronous, but waking waiters under the jobs lock buys nothing.
    pub async fn mark_killed_and_cancel(&self, id: JobId, delivered: bool) -> bool {
        let token = {
            let mut jobs = self.jobs.lock().await;
            let Some(job) = jobs.get_mut(&id) else {
                return false;
            };
            let token = job.cancel.clone();
            if token.is_none() && !delivered {
                return false;
            }
            job.killed = true;
            token
        };
        if let Some(token) = token {
            token.cancel();
        }
        true
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
        jobs.get(&id).map(Job::pgids_combined).unwrap_or_default()
    }

    /// Non-blocking accessor for a finished job's result — `None` while the
    /// job is still `Running`/`Stopped`, or if `id` doesn't exist. Unlike
    /// [`Self::wait`], this never parks: it polls once and returns whatever is
    /// (or isn't) already available. GH #243: previously the only ways to
    /// read a job's `ExecResult` were `wait` (blocks until done) or
    /// string-parsing `failed:{code}` off `/v/jobs/N/status`.
    pub async fn try_result(&self, id: JobId) -> Option<ExecResult> {
        let mut jobs = self.jobs.lock().await;
        let job = jobs.get_mut(&id)?;
        job.try_poll();
        job.try_result().cloned()
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
    async fn test_job_info_carries_exit_code_on_failure() {
        // GH #243(a): a job that exited 42 must surface the code on
        // JobInfo.exit_code, not just JobStatus::Failed — the audit verified
        // `jobs --json` lost it entirely.
        let manager = JobManager::new();

        let id = manager
            .spawn("sh -c 'exit 42'".to_string(), async {
                ExecResult::failure(42, "")
            })
            .await;

        tokio::time::sleep(Duration::from_millis(10)).await;

        let info = manager.get(id).await.expect("job exists");
        assert_eq!(info.status, JobStatus::Failed);
        assert_eq!(info.exit_code, Some(42), "exit code must survive onto JobInfo");
    }

    #[tokio::test]
    async fn test_job_info_exit_code_none_while_running() {
        let manager = JobManager::new();
        let (_tx, rx) = oneshot::channel();
        let id = manager.register("still going".to_string(), rx).await;

        let info = manager.get(id).await.expect("job exists");
        assert_eq!(info.status, JobStatus::Running);
        assert!(info.exit_code.is_none(), "a running job has no exit code yet");
    }

    #[tokio::test]
    async fn test_job_info_started_at_and_finished_at() {
        // GH #243(b): timestamps must be present so an embedder can compute
        // "running for Ns" or sort by recency.
        let manager = JobManager::new();
        let before_spawn = kaish_types::clock::system_now();

        let id = manager
            .spawn("quick".to_string(), async {
                tokio::time::sleep(Duration::from_millis(10)).await;
                ExecResult::success("")
            })
            .await;

        // Immediately after spawn, started_at is set but finished_at is not.
        let info = manager.get(id).await.expect("job exists");
        assert!(
            info.started_at >= before_spawn,
            "started_at must be stamped at (or after) spawn time"
        );
        assert!(info.finished_at.is_none(), "not finished yet");

        let _ = manager.wait(id).await;

        let info = manager.get(id).await.expect("job exists");
        let finished_at = info.finished_at.expect("finished_at must be set once done");
        assert!(
            finished_at >= info.started_at,
            "finished_at must be at or after started_at"
        );
    }

    #[tokio::test]
    async fn test_job_info_surfaces_pgids() {
        // GH #243(c): pgids (real OS process groups an embedder actually
        // creates) must be surfaced on JobInfo — pid almost never is
        // (TTY-only, Ctrl-Z path).
        let manager = JobManager::new();
        let id = manager.spawn("bg".to_string(), async { ExecResult::success("") }).await;

        manager.add_pgid(id, 4242).await;
        manager.add_pgid(id, 4243).await;

        let info = manager.get(id).await.expect("job exists");
        assert_eq!(info.pgids, vec![4242, 4243]);
        assert!(info.pid.is_none(), "pid stays None for a non-stopped job");
    }

    #[tokio::test]
    async fn test_try_result_is_non_blocking_and_none_while_running() {
        // GH #243: previously the only way to read a finished job's ExecResult
        // was the blocking `wait`; `Job::try_result` was `pub` but the `jobs`
        // map was private, so nothing on JobManager could reach it.
        let manager = JobManager::new();
        let (_tx, rx) = oneshot::channel::<ExecResult>();
        let id = manager.register("still going".to_string(), rx).await;

        // Must return immediately (no sleep here) with None — the job never
        // got a result.
        assert!(manager.try_result(id).await.is_none());

        // Unknown id -> None too, no panic.
        assert!(manager.try_result(JobId(999)).await.is_none());
    }

    #[tokio::test]
    async fn test_try_result_returns_result_once_finished() {
        let manager = JobManager::new();
        let id = manager
            .spawn("quick".to_string(), async { ExecResult::success("hi") })
            .await;

        tokio::time::sleep(Duration::from_millis(10)).await;

        let result = manager.try_result(id).await.expect("job finished");
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hi");

        // The job is still tracked (try_result doesn't reap) — a second call
        // (and wait()) still sees it.
        assert!(manager.try_result(id).await.is_some());
        assert!(manager.wait(id).await.is_some());
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

    /// GH #247: `execute_background` uses the oneshot-channel path
    /// exclusively, so a panic inside the spawned task drops the sender
    /// without a result — the exact shape reproduced here by dropping `tx`
    /// directly rather than triggering a real panic. Before the fix this
    /// reported `failed:1` with the generic text "job channel closed",
    /// indistinguishable from a command that legitimately exited 1 and never
    /// logged. The result must now name what actually happened (a task that
    /// ended without producing a result) instead of reading like an ordinary
    /// command failure.
    #[tokio::test]
    async fn test_dropped_sender_reports_as_a_kernel_fault_not_exit_1() {
        let manager = JobManager::new();
        let (tx, rx) = oneshot::channel::<ExecResult>();

        let id = manager.register("will panic".to_string(), rx).await;
        drop(tx); // simulates the spawned task's future panicking mid-flight

        let result = manager.wait(id).await.expect("job must resolve, not hang");
        assert_eq!(result.code, 1);
        assert!(
            !result.err.contains("job channel closed"),
            "message must not use the old generic wording: {}",
            result.err
        );
        assert!(
            result.err.contains("panic") || result.err.contains("task ended without a result"),
            "message must name a kernel fault, not read like an ordinary exit 1: {}",
            result.err
        );
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

    /// GH #247: `list`/`list_ids` iterated the backing `HashMap` directly, so
    /// two jobs could come back as `[2, 1]` — arbitrary, and a flake source
    /// for any MCP caller or snapshot test that depended on the order. Job
    /// ids are minted strictly increasing (`next_id`), so ascending-by-id is
    /// the one order that is both stable and meaningful (spawn order).
    #[tokio::test]
    async fn test_list_and_list_ids_are_sorted_by_id() {
        let manager = JobManager::new();
        let mut ids = Vec::new();
        for n in 0..8 {
            let (_tx, rx) = oneshot::channel::<ExecResult>();
            ids.push(manager.register(format!("job-{n}"), rx).await);
        }

        let listed_ids = manager.list_ids().await;
        assert_eq!(listed_ids, ids, "list_ids must come back in ascending JobId order");

        let infos = manager.list().await;
        let info_ids: Vec<JobId> = infos.iter().map(|i| i.id).collect();
        assert_eq!(info_ids, ids, "list must come back in ascending JobId order");
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

    /// A Ctrl-Z'd job has no `JoinHandle` and no result channel, and `is_done()`
    /// returns `false` while `stopped` — so nothing can ever make it done.
    /// `wait_all` used to poll it forever at 10ms, and since `Kernel::shutdown`
    /// calls `wait_all`, one Ctrl-Z hung shutdown with no timeout and no escape.
    #[tokio::test]
    async fn wait_all_skips_a_stopped_job_instead_of_hanging_forever() {
        let manager = JobManager::new();

        // No real process needed: `wait_all` decides on the `stopped` flag, and
        // the hang was never about the pid.
        let stopped = manager
            .register_stopped("sleep 5".to_string(), 4242, 4242)
            .await;

        // A job that does finish, so this also proves we did not fix the hang by
        // making `wait_all` skip everything.
        let finisher = manager
            .spawn("finisher".to_string(), async { ExecResult::success("ok") })
            .await;

        let results = tokio::time::timeout(Duration::from_secs(2), manager.wait_all())
            .await
            .expect("wait_all must not hang on a stopped job");

        let ids: Vec<JobId> = results.iter().map(|(id, _)| *id).collect();
        assert!(
            !ids.contains(&stopped),
            "a stopped job can never complete, so wait_all must skip it"
        );
        assert!(
            ids.contains(&finisher),
            "wait_all must still collect jobs that can finish"
        );
    }

    /// `wait` on an already-stopped job returns `None` immediately instead of
    /// polling a job that can never become done.
    #[tokio::test]
    async fn wait_returns_none_on_a_stopped_job() {
        let manager = JobManager::new();
        let id = manager
            .register_stopped("sleep 5".to_string(), 4242, 4242)
            .await;

        let res = tokio::time::timeout(Duration::from_secs(2), manager.wait(id))
            .await
            .expect("wait on a stopped job must return, not hang");
        assert!(res.is_none(), "a stopped job has no result to wait for");
    }

    /// The `wait_all` stopped-skip is a snapshot: a job that stops *after* the
    /// filter (the bg reaper observing a SIGSTOP) used to leave the inner
    /// `wait` polling `is_done()` forever — the same shutdown hang, reached
    /// through a sub-200ms window instead of always. The stopped guard inside
    /// `wait`'s loop closes it.
    #[tokio::test]
    async fn wait_bails_when_the_job_stops_mid_wait() {
        let manager = Arc::new(JobManager::new());
        manager.set_persist_output_files(false);

        // A job that never finishes on its own: the sender side is kept alive
        // so the future stays parked until the test ends.
        let (_tx, rx) = oneshot::channel::<()>();
        let id = manager
            .spawn("blocker".to_string(), async move {
                let _ = rx.await;
                ExecResult::success("done")
            })
            .await;

        let m = manager.clone();
        let waiter = tokio::spawn(async move { m.wait(id).await });

        // Let the waiter enter its poll loop, then stop the job under it.
        tokio::time::sleep(Duration::from_millis(30)).await;
        manager.stop_job(id, 4242, 4242).await;

        let res = tokio::time::timeout(Duration::from_secs(2), waiter)
            .await
            .expect("wait must return once the job stops, not poll forever")
            .expect("waiter task must not panic");
        assert!(res.is_none(), "a job that stopped mid-wait has no result");
    }

    /// Same race through `wait_all`: the job passes the not-stopped snapshot,
    /// then stops while the inner `wait` polls it.
    #[tokio::test]
    async fn wait_all_returns_when_a_job_stops_after_the_snapshot() {
        let manager = Arc::new(JobManager::new());
        manager.set_persist_output_files(false);

        let (_tx, rx) = oneshot::channel::<()>();
        let id = manager
            .spawn("blocker".to_string(), async move {
                let _ = rx.await;
                ExecResult::success("done")
            })
            .await;

        let m = manager.clone();
        let all = tokio::spawn(async move { m.wait_all().await });

        tokio::time::sleep(Duration::from_millis(30)).await;
        manager.stop_job(id, 4242, 4242).await;

        let results = tokio::time::timeout(Duration::from_secs(2), all)
            .await
            .expect("wait_all must return once the job stops, not poll forever")
            .expect("wait_all task must not panic");
        assert!(
            !results.iter().any(|(rid, _)| *rid == id),
            "a job that stopped mid-wait_all yields no result"
        );
    }

    /// GH #252: the status *string* (backing `/v/jobs/N/status`) must agree
    /// with `status()` about a stopped job. A stopped job has no result
    /// channel, so `try_poll` can never resolve it — without the explicit
    /// check it read `running` forever while `status()` said `Stopped`.
    #[tokio::test]
    async fn status_string_reports_stopped() {
        let manager = JobManager::new();
        let id = manager.register_stopped("vi".to_string(), 4242, 4242).await;
        assert_eq!(manager.get_status_string(id).await.as_deref(), Some("stopped"));
        assert_eq!(
            manager.get(id).await.map(|info| info.status),
            Some(JobStatus::Stopped),
            "status() and status_string() must agree"
        );
    }

    /// GH #244: a killed job's terminal status is `Killed`/`killed:{code}`,
    /// not `Failed` — the flag is set by `mark_killed_and_cancel` before the
    /// cancel trips, and only colors a non-ok exit (a job that finished ok
    /// anyway still reads `Done`).
    #[tokio::test]
    async fn mark_killed_colors_the_terminal_status() {
        let manager = JobManager::new();
        manager.set_persist_output_files(false);
        let (tx, rx) = oneshot::channel::<()>();
        let id = manager
            .spawn("victim".to_string(), async move {
                let _ = rx.await;
                ExecResult::failure(130, "cancelled")
            })
            .await;
        // delivered=true stands in for a real killpg delivery — spawn()'d
        // jobs record no cancellation token.
        assert!(manager.mark_killed_and_cancel(id, true).await);
        drop(tx); // unblock the future — it returns the 130 result
        let result = manager.wait(id).await.expect("job finishes");
        assert_eq!(result.code, 130);
        assert_eq!(
            manager.get(id).await.map(|info| info.status),
            Some(JobStatus::Killed)
        );
        assert_eq!(manager.get_status_string(id).await.as_deref(), Some("killed:130"));

        // A successful exit is never re-colored: the result is the truth.
        let id2 = manager
            .spawn("survivor".to_string(), async { ExecResult::success("done") })
            .await;
        assert!(manager.mark_killed_and_cancel(id2, true).await);
        let result = manager.wait(id2).await.expect("job finishes");
        assert!(result.ok());
        assert_eq!(
            manager.get(id2).await.map(|info| info.status),
            Some(JobStatus::Done),
            "a job that finished ok before the kill landed reports Done"
        );
    }

    /// Review finding (GH #244): with no cancellation token and nothing
    /// delivered, `mark_killed_and_cancel` must refuse AND leave the flag
    /// unset — otherwise a later organic failure reads as a kill that never
    /// happened.
    #[tokio::test]
    async fn no_lever_kill_does_not_color_a_later_organic_failure() {
        let manager = JobManager::new();
        manager.set_persist_output_files(false);
        let (tx, rx) = oneshot::channel::<()>();
        let id = manager
            .spawn("doomed anyway".to_string(), async move {
                let _ = rx.await;
                ExecResult::failure(7, "organic failure")
            })
            .await;
        assert!(
            !manager.mark_killed_and_cancel(id, false).await,
            "no token + nothing delivered must refuse"
        );
        drop(tx);
        let result = manager.wait(id).await.expect("job finishes");
        assert_eq!(result.code, 7);
        assert_eq!(
            manager.get(id).await.map(|info| info.status),
            Some(JobStatus::Failed),
            "the failed kill attempt must not have colored the status"
        );
        assert_eq!(manager.get_status_string(id).await.as_deref(), Some("failed:7"));
    }

    /// Review finding (GH #244): retention must also hold when jobs finish
    /// AFTER registration stopped — `list()` observes completions and
    /// enforces. (The registration-time test above releases each job before
    /// the next spawn, which masked this.)
    #[tokio::test]
    async fn retention_enforced_when_completion_is_observed_by_list() {
        let manager = JobManager::new();
        manager.set_persist_output_files(false);
        manager.set_finished_retention(2);

        let mut releases = Vec::new();
        let mut ids = Vec::new();
        for n in 0..5 {
            let (tx, rx) = oneshot::channel::<()>();
            releases.push(tx);
            ids.push(
                manager
                    .spawn(format!("held {n}"), async move {
                        let _ = rx.await;
                        ExecResult::success("")
                    })
                    .await,
            );
        }
        // All five registered while RUNNING — registration-time enforcement
        // had nothing to evict. Now they all finish with no registration
        // following. (Not waited one-by-one: wait() itself now evicts at
        // each observed completion, so a sequential wait on the oldest ids
        // finds them already gone — which is the feature, not the fixture.)
        for tx in releases {
            drop(tx);
        }
        let deadline = std::time::Instant::now() + Duration::from_secs(5);
        loop {
            let infos = manager.list().await;
            if infos.iter().all(|info| info.status != JobStatus::Running) {
                break;
            }
            assert!(
                std::time::Instant::now() < deadline,
                "jobs did not finish in time"
            );
            tokio::time::sleep(Duration::from_millis(10)).await;
        }
        let mut tracked = 0;
        for id in &ids {
            if manager.exists(*id).await {
                tracked += 1;
            }
        }
        assert!(
            tracked <= 2,
            "finished jobs beyond the cap must be evicted once observed, still tracked: {tracked}"
        );
    }

    /// GH #244: registration evicts the oldest finished jobs beyond the
    /// retention cap, so a never-reaping embedder stays bounded. Running jobs
    /// are never evicted.
    #[tokio::test]
    async fn finished_retention_evicts_oldest_at_registration() {
        let manager = JobManager::new();
        manager.set_persist_output_files(false);
        manager.set_finished_retention(2);

        let mut finished_ids = Vec::new();
        for n in 0..4 {
            let id = manager
                .spawn(format!("quick {n}"), async { ExecResult::success("") })
                .await;
            manager.wait(id).await.expect("job finishes");
            finished_ids.push(id);
        }
        // A still-running job to prove eviction only touches finished ones.
        let (_tx, rx) = oneshot::channel::<()>();
        let running = manager
            .spawn("blocker".to_string(), async move {
                let _ = rx.await;
                ExecResult::success("")
            })
            .await;

        assert!(manager.exists(running).await, "running job is never evicted");
        let tracked_finished: Vec<bool> = {
            let mut v = Vec::new();
            for id in &finished_ids {
                v.push(manager.exists(*id).await);
            }
            v
        };
        // Registering the 3rd and 4th quick jobs (and the blocker) evicted the
        // oldest finished entries down to the cap of 2.
        assert_eq!(
            tracked_finished,
            vec![false, false, true, true],
            "oldest finished jobs evicted first: {finished_ids:?}"
        );
    }
}
