//! One spawn discipline for every external child the kernel runs.
//!
//! [`spawn_process`] owns everything that happens between "the executable and
//! argv are decided" and "the child's exit code is an [`ExecResult`]":
//! hermetic environment, stdin policy with no pre-spawn draining, captured or
//! inherited stdout/stderr, process group plus `kill_on_drop` plus the
//! parent-death signal, [`wait_or_kill`](crate::kernel::wait_or_kill) against
//! the context's cancel token, background-job pgid registration and stream
//! tees, the fixed-ring overflow signal, and the 126/127 error classes.
//!
//! Resolution — PATH lookup, relative-path checks, the virtual-cwd refusal —
//! stays with the caller: an external command resolves a bare name against
//! `$PATH`, and a wrapped command has its executable pinned at registration.
//! Those are different questions with different errors.
//!
//! # Why the caller passes a [`SpawnContext`] and not `&ExecContext`
//!
//! Everything the spawn reads out of an [`ExecContext`] is cheap to clone, and
//! [`SpawnContext::from_exec_context`] is the one line a tool holding
//! `&mut ExecContext` writes to get it. Borrowing the context instead would
//! make `Kernel::try_execute_external` hold its `RwLock<ExecContext>` guard for
//! the whole life of the child — `kernel.cwd().await` would block for as long
//! as `sleep 60` runs. A snapshot keeps the lock hold as short as it is today.

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use tokio_util::sync::CancellationToken;

use crate::dispatch::PipelinePosition;
use crate::interpreter::{ExecResult, Scope};
use crate::scheduler::{
    drain_to_stream_teed, BoundedStream, JobId, JobManager, PipeReader, DEFAULT_STREAM_MAX_SIZE,
};
use crate::tools::ExecContext;

/// What a child process reads on stdin.
///
/// A buffered prefix and a live pipe are one stream, not two candidates: after
/// `read x`, the bytes `read` over-read sit in the prefix and the rest is still
/// in the pipe, so [`StdinPolicy::Piped`] carries both and writes the prefix
/// first.
pub(crate) enum StdinPolicy {
    /// `/dev/null`. The child sees immediate EOF.
    Null,
    /// The kaish process's own stdin. Interactive use only — a child that
    /// inherits the terminal competes with the REPL for it.
    Inherit,
    /// A pipe fed by the kernel. Neither source is drained before the spawn:
    /// a pipe read can block on a still-running upstream stage, so draining
    /// first would serialize the pipeline (`sleep 60 | extern` would deadlock).
    Piped {
        /// Buffered bytes written to the child before the pipe is streamed.
        prefix: Option<Vec<u8>>,
        /// Streaming input copied to the child by a detached task.
        pipe: Option<PipeReader>,
    },
}

/// Where a child's stdout and stderr go.
pub(crate) enum OutputPolicy {
    /// Both streams are captured into fixed 10 MB tail-evicting rings. The
    /// result carries stdout (text if valid UTF-8, else `Bytes`) and stderr.
    Captured,
    /// Both streams go straight to the kaish process's terminal. The result
    /// carries empty stdout and stderr, because nothing passed through the
    /// kernel. Interactive use only.
    Inherit {
        /// Terminal handle when interactive job control is active, which
        /// switches the wait to `waitpid(WUNTRACED)` so Ctrl-Z registers a
        /// stopped job, and restores the default tty signal handlers in the
        /// child. `None` means inherited stdio without job control.
        #[cfg(unix)]
        terminal_state: Option<Arc<crate::terminal::TerminalState>>,
    },
}

/// A child process to run, fully decided: nothing here is resolved, looked up,
/// or defaulted by [`spawn_process`].
pub(crate) struct SpawnRequest {
    /// The program to execute. Already resolved — no PATH lookup happens below.
    pub executable: PathBuf,
    /// Arguments after `argv[0]`, rendered by the caller.
    pub argv: Vec<String>,
    /// Real filesystem directory for the child. A virtual cwd has no location
    /// to spawn in and must be refused by the caller, which knows how to name
    /// the command in the error.
    pub cwd: PathBuf,
    /// The child's complete environment. `env_clear()` runs first, so this is
    /// everything the child sees. Build it with [`hermetic_env`] and add pins
    /// on top.
    pub env: Vec<(String, String)>,
    /// What the child reads on stdin.
    pub stdin: StdinPolicy,
    /// Where the child's stdout and stderr go.
    pub output: OutputPolicy,
    /// The name this child answers to in errors and in the job table — the
    /// command as written, not the resolved executable path.
    pub label: String,
}

/// The pieces of an [`ExecContext`] that spawning a child reads.
///
/// Every field is either already on `ExecContext` or was moved there for this
/// helper. The table is in the module docs of `ExecContext` for the two that
/// moved; the rest were there already.
pub(crate) struct SpawnContext {
    /// Fires when this execution path is cancelled — a Ctrl-C, a request
    /// timeout, or the `timeout` builtin's child token. Drives the
    /// SIGTERM-grace-SIGKILL cascade.
    pub cancel: CancellationToken,
    /// How long a cancelled child has between SIGTERM and SIGKILL.
    pub kill_grace: Duration,
    /// Arm `PR_SET_PDEATHSIG(SIGKILL)` so a hard-killed kaish cannot orphan
    /// this child.
    pub kill_children_on_parent_death: bool,
    /// Where this command sits in its pipeline. Decides which stage's stdout
    /// tees into a background job's stdout node.
    pub pipeline_position: PipelinePosition,
    /// The job table, for recording the child's process group.
    pub job_manager: Option<Arc<JobManager>>,
    /// The background job this command runs for, if any.
    pub background_job: Option<JobId>,
}

impl SpawnContext {
    /// Snapshot what a spawn needs out of an execution context.
    pub fn from_exec_context(ctx: &ExecContext) -> Self {
        Self {
            cancel: ctx.cancel.clone(),
            kill_grace: ctx.kill_grace,
            kill_children_on_parent_death: ctx.kill_children_on_parent_death,
            pipeline_position: ctx.pipeline_position,
            job_manager: ctx.job_manager.clone(),
            background_job: ctx.background_job,
        }
    }
}

/// Build the hermetic child environment from a scope's exported variables.
///
/// The child sees only what kaish exported, never the kaish process's OS
/// environment. A frontend that wants host passthrough seeds it through
/// `KernelConfig::initial_vars` at construction.
///
/// # Errors
///
/// A structured value (list or record) and a binary value both fail rather
/// than crossing the process boundary as JSON text or as a
/// `[binary: N bytes]` placeholder.
pub(crate) fn hermetic_env(scope: &Scope) -> anyhow::Result<Vec<(String, String)>> {
    let exported = scope.exported_vars();
    if let Some(message) = crate::interpreter::structured_export_error(&exported) {
        return Err(anyhow::anyhow!(message));
    }
    let mut env = Vec::with_capacity(exported.len());
    for (name, value) in exported {
        let text = crate::interpreter::value_to_text_sink_named(
            &value,
            "an exported environment variable value",
        )
        .map_err(|e| anyhow::anyhow!("{e}"))?;
        env.push((name, text));
    }
    Ok(env)
}

/// Abort the stdin-copy task on EVERY exit path.
///
/// Once the child is reaped the copy has nothing left to deliver; parked on
/// `pipe.read()` it would leak and hold the upstream pipe reader open. A drop
/// guard is the single place that covers all returns — explicit per-return
/// aborts were error-prone (an earlier version missed the two `Inherit`
/// returns).
struct AbortStdinCopyOnDrop(Option<tokio::task::JoinHandle<()>>);

impl Drop for AbortStdinCopyOnDrop {
    fn drop(&mut self) {
        if let Some(task) = self.0.take() {
            task.abort();
        }
    }
}

/// Run a child process to completion under the kernel's discipline.
///
/// Returns an `ExecResult` for every outcome, including a failed spawn (127),
/// a non-executable target (126 — the caller checks that before resolution),
/// and a wait error (1). There is no fallible path left: the one thing that
/// could fail before the fork is building the environment, and that is
/// [`hermetic_env`], which the caller calls first.
pub(crate) async fn spawn_process(request: SpawnRequest, spawn_ctx: &SpawnContext) -> ExecResult {
    use tokio::process::Command;

    let SpawnRequest {
        executable,
        argv,
        cwd,
        env,
        stdin,
        output,
        label,
    } = request;

    let mut cmd = Command::new(&executable);
    cmd.args(&argv);
    cmd.current_dir(&cwd);

    // Hermetic env: the child sees only what the caller put in `env`.
    cmd.env_clear();
    for (name, value) in env {
        cmd.env(name, value);
    }

    cmd.stdin(match &stdin {
        StdinPolicy::Piped { .. } => std::process::Stdio::piped(),
        StdinPolicy::Inherit => std::process::Stdio::inherit(),
        StdinPolicy::Null => std::process::Stdio::null(),
    });

    let inherit_output = matches!(output, OutputPolicy::Inherit { .. });
    if inherit_output {
        cmd.stdout(std::process::Stdio::inherit());
        cmd.stderr(std::process::Stdio::inherit());
    } else {
        cmd.stdout(std::process::Stdio::piped());
        cmd.stderr(std::process::Stdio::piped());
    }

    // The job-control terminal, when the caller selected it. Only reachable
    // on the `Inherit` path.
    #[cfg(unix)]
    let terminal_state = match &output {
        OutputPolicy::Inherit { terminal_state } => terminal_state.clone(),
        OutputPolicy::Captured => None,
    };

    // On Unix, always put the child in its own process group so cancellation
    // can `killpg` the whole tree (the child plus any grandchildren).
    // Restoring default tty-related signal handlers stays gated on
    // job-control mode — those only matter when the child has a controlling
    // terminal.
    #[cfg(unix)]
    {
        let restore_jc_signals = terminal_state.is_some() && inherit_output;
        // Read before the fork: the child compares `getppid()` against it to
        // catch a parent that died inside the fork/prctl window.
        let kill_on_parent_death = spawn_ctx.kill_children_on_parent_death;
        let parent_pid = std::process::id();
        // SAFETY: setpgid, prctl, getppid, and sigaction(SIG_DFL) are all
        // async-signal-safe per POSIX; safe to call between fork and exec.
        #[allow(unsafe_code)]
        unsafe {
            cmd.pre_exec(move || {
                // Own process group — for kill scope.
                nix::unistd::setpgid(nix::unistd::Pid::from_raw(0), nix::unistd::Pid::from_raw(0))
                    .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                if kill_on_parent_death {
                    crate::dispatch::arm_parent_death_signal(parent_pid)?;
                }
                if restore_jc_signals {
                    use nix::libc::{sigaction, SIGINT, SIGTSTP, SIGTTIN, SIGTTOU, SIG_DFL};
                    let mut sa: nix::libc::sigaction = std::mem::zeroed();
                    sa.sa_sigaction = SIG_DFL;
                    if sigaction(SIGTSTP, &sa, std::ptr::null_mut()) != 0 {
                        return Err(std::io::Error::last_os_error());
                    }
                    if sigaction(SIGTTOU, &sa, std::ptr::null_mut()) != 0 {
                        return Err(std::io::Error::last_os_error());
                    }
                    if sigaction(SIGTTIN, &sa, std::ptr::null_mut()) != 0 {
                        return Err(std::io::Error::last_os_error());
                    }
                    if sigaction(SIGINT, &sa, std::ptr::null_mut()) != 0 {
                        return Err(std::io::Error::last_os_error());
                    }
                }
                Ok(())
            });
        }
    }

    // Backstop for kill on drop in case our explicit kill path is bypassed
    // (panic, early return, etc) on the **capture** wait path. We do NOT
    // set this on the JC inherit path: that uses sync `waitpid` outside
    // tokio's view of the child, so on drop tokio would try to kill an
    // already-reaped (possibly-reused) PID. The JC path has its own
    // cancel handling via the side-task watcher.
    #[cfg(unix)]
    let in_jc_inherit_path = inherit_output && terminal_state.is_some();
    #[cfg(not(unix))]
    let in_jc_inherit_path = false;
    if !in_jc_inherit_path {
        cmd.kill_on_drop(true);
    }

    // Spawn the process. Capture a `KillTarget` immediately so cancel/
    // timeout paths can deliver signals via pidfd (Linux ≥ 5.3) — bound
    // to this process's generation, immune to PID reuse if the OS reaps
    // the child before our kill syscalls fire.
    let mut child = match cmd.spawn() {
        Ok(child) => child,
        Err(e) => return ExecResult::failure(127, format!("{}: {}", label, e)),
    };
    #[cfg(unix)]
    let kill_target = crate::pidfd::KillTarget::from_child(&child);
    #[cfg(not(unix))]
    let kill_target: Option<()> = None;

    // If this child runs on behalf of a background job, record its process
    // group on the job so `kill -<sig> %N` can signal the real process
    // directly (STOP/CONT/USR1/…, not just terminate). The child did
    // `setpgid(0, 0)` in pre_exec, so its PGID equals its PID.
    if let (Some(jobs), Some(job_id)) = (&spawn_ctx.job_manager, spawn_ctx.background_job)
        && let Some(pid) = child.id()
    {
        jobs.add_pgid(job_id, pid).await;
    }

    // Same seam, for output: a background job's streams outlive this one
    // command, so the drain tasks below tee into them and the job closes
    // them itself. This is what makes `/v/jobs/{id}/stdout` grow while a
    // `cargo build &` is still building (GH #240 removed the node rather
    // than wire this tee; the tee is the half that was missing).
    let job_streams = match (&spawn_ctx.job_manager, spawn_ctx.background_job) {
        (Some(jobs), Some(job_id)) => jobs.streams(job_id).await,
        _ => None,
    };

    // Feed stdin. A streaming pipe is copied to the child by a detached task
    // (bounded memory, no pre-drain) so an upstream stage and this child run
    // concurrently — and a child that never reads stdin (or is killed) just
    // breaks the copy, which stops. A buffered byte vector is written verbatim
    // (no text detour), so binary stdin survives.
    let stdin_task: Option<tokio::task::JoinHandle<()>> = match stdin {
        StdinPolicy::Piped {
            prefix,
            pipe: Some(mut pipe_in),
        } => child.stdin.take().map(|mut child_stdin| {
            tokio::spawn(async move {
                use tokio::io::{AsyncReadExt, AsyncWriteExt};
                if let Some(data) = prefix
                    && child_stdin.write_all(&data).await.is_err()
                {
                    return; // child closed stdin; dropping it signals EOF
                }
                let mut buf = [0u8; 8192];
                loop {
                    match pipe_in.read(&mut buf).await {
                        Ok(0) => break, // EOF
                        Ok(n) => {
                            if child_stdin.write_all(&buf[..n]).await.is_err() {
                                break; // child closed stdin
                            }
                        }
                        Err(_) => break,
                    }
                }
                // Dropping child_stdin signals EOF to the child.
            })
        }),
        // Write the buffered bytes from a detached task too — NOT inline.
        // An inline write blocks once the stdin pipe fills, and the output
        // drain hasn't spawned yet, so a child that emits a lot before
        // consuming all its input (every pipe buffer full) deadlocks. A
        // write error here is normal, not a failure: a child that closes
        // stdin early (e.g. `head`) breaks the pipe. Dropping child_stdin
        // signals EOF.
        StdinPolicy::Piped {
            prefix: Some(data),
            pipe: None,
        } => child.stdin.take().map(|mut child_stdin| {
            tokio::spawn(async move {
                use tokio::io::AsyncWriteExt;
                let _ = child_stdin.write_all(&data).await;
            })
        }),
        StdinPolicy::Piped {
            prefix: None,
            pipe: None,
        }
        | StdinPolicy::Inherit
        | StdinPolicy::Null => None,
    };
    let _stdin_copy_guard = AbortStdinCopyOnDrop(stdin_task);

    if inherit_output {
        // Job control path: use waitpid with WUNTRACED for Ctrl-Z support
        #[cfg(unix)]
        if let Some(ref term) = terminal_state {
            let child_id = child.id().unwrap_or(0);
            let pid = nix::unistd::Pid::from_raw(child_id as i32);
            let pgid = pid; // child is its own pgid leader

            // Give the terminal to the child's process group
            if let Err(e) = term.give_terminal_to(pgid) {
                tracing::warn!("failed to give terminal to child: {}", e);
            }

            let term_clone = term.clone();
            let cmd_name = label.clone();
            let cmd_display = format!("{} {}", label, argv.join(" "));
            let jobs = spawn_ctx.job_manager.clone();
            let kill_grace = spawn_ctx.kill_grace;

            // Side task that watches for cancellation while the blocking
            // waitpid runs. On cancel, it SIGTERMs the process group, waits
            // the grace period, then SIGKILLs. The blocking waitpid returns
            // when the child dies. AbortOnDrop guard cancels the watcher
            // on the success path so it doesn't keep running after wait
            // returns naturally.
            //
            // `wait_complete` shrinks the PID-reuse race: the watcher
            // checks it before each kill syscall and bails out if
            // wait_for_foreground has already reaped the child. This
            // doesn't fully eliminate the race (atomic load + kill is
            // not atomic with the OS reap+reuse), but narrows the window
            // to nanoseconds — enough to be ignorable in practice.
            let wait_complete = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
            let cancel_watcher = {
                let cancel = spawn_ctx.cancel.clone();
                let wc = wait_complete.clone();
                // Ownership transfer: the JC path's sync wait inside
                // block_in_place owns the child's reaping, so the
                // cancel_watcher drives the kill side via KillTarget
                // (pidfd-bound on Linux). Pidfd is just an OwnedFd — not
                // Clone — so re-open from the pid, and fall back to the
                // PID-based path when that fails (race already reaped →
                // best-effort kill).
                let target = kill_target
                    .as_ref()
                    .map(|t| crate::pidfd::KillTarget::from_pid(t.pid()));
                tokio::spawn(async move {
                    cancel.cancelled().await;
                    if wc.load(std::sync::atomic::Ordering::SeqCst) {
                        return;
                    }
                    use nix::sys::signal::Signal;
                    if let Some(t) = &target {
                        t.signal(Signal::SIGTERM);
                        t.signal_pg(Signal::SIGTERM);
                    } else {
                        let _ = nix::sys::signal::kill(pid, Signal::SIGTERM);
                        let _ = nix::sys::signal::killpg(pid, Signal::SIGTERM);
                    }
                    if kill_grace > Duration::ZERO {
                        tokio::time::sleep(kill_grace).await;
                        if wc.load(std::sync::atomic::Ordering::SeqCst) {
                            return;
                        }
                    }
                    if let Some(t) = &target {
                        t.signal(Signal::SIGKILL);
                        t.signal_pg(Signal::SIGKILL);
                    } else {
                        let _ = nix::sys::signal::kill(pid, Signal::SIGKILL);
                        let _ = nix::sys::signal::killpg(pid, Signal::SIGKILL);
                    }
                })
            };
            struct AbortOnDrop(tokio::task::JoinHandle<()>);
            impl Drop for AbortOnDrop {
                fn drop(&mut self) {
                    self.0.abort();
                }
            }
            let _watcher_guard = AbortOnDrop(cancel_watcher);

            let wait_complete_setter = wait_complete.clone();
            let code = tokio::task::block_in_place(move || {
                let result = term_clone.wait_for_foreground(pid);
                // Mark wait done before the watcher might fire.
                wait_complete_setter.store(true, std::sync::atomic::Ordering::SeqCst);

                // Always reclaim the terminal
                if let Err(e) = term_clone.reclaim_terminal() {
                    tracing::warn!("failed to reclaim terminal: {}", e);
                }

                match result {
                    crate::terminal::WaitResult::Exited(code) => code as i64,
                    crate::terminal::WaitResult::Signaled(sig) => 128 + sig as i64,
                    crate::terminal::WaitResult::Stopped(_sig) => {
                        // Register as a stopped job. A context with job
                        // control but no job table cannot happen through the
                        // kernel; say so rather than dropping the job.
                        let Some(jobs) = jobs else {
                            tracing::error!(
                                command = %cmd_name,
                                "stopped child cannot be registered: no job manager on this context"
                            );
                            return 148;
                        };
                        let rt = tokio::runtime::Handle::current();
                        let job_id = rt.block_on(jobs.register_stopped(
                            cmd_display,
                            child_id,
                            child_id, // pgid = pid for group leader
                        ));
                        eprintln!("\n[{}]+ Stopped\t{}", job_id, cmd_name);
                        148 // 128 + SIGTSTP(20) on most systems, but we use a fixed value
                    }
                }
            });

            return ExecResult::from_output(code, String::new(), String::new());
        }

        // Non-job-control path with inherited stdio.
        let status = match crate::kernel::wait_or_kill(
            &mut child,
            kill_target.as_ref(),
            &spawn_ctx.cancel,
            spawn_ctx.kill_grace,
        )
        .await
        {
            Ok(s) => s,
            Err(e) => {
                return ExecResult::failure(1, format!("{}: failed to wait: {}", label, e));
            }
        };

        let code = crate::kernel::exit_code_from_status(&status);

        // stdout/stderr already went to the terminal
        ExecResult::from_output(code, String::new(), String::new())
    } else {
        // Capture output via bounded streams
        let stdout_stream = Arc::new(BoundedStream::new(DEFAULT_STREAM_MAX_SIZE));
        let stderr_stream = Arc::new(BoundedStream::new(DEFAULT_STREAM_MAX_SIZE));

        let stdout_pipe = child.stdout.take();
        let stderr_pipe = child.stderr.take();

        let stdout_clone = stdout_stream.clone();
        let stderr_clone = stderr_stream.clone();

        // Only the stage whose stdout *is* the job's stdout tees: in
        // `a | b`, `a`'s bytes are `b`'s stdin, and teeing them would put
        // the pipeline's intermediate data into the node alongside its
        // real output. stderr has no such routing — every stage's stderr
        // is the job's stderr — so it tees from any position.
        let stdout_tee = job_streams.as_ref().and_then(|s| {
            matches!(
                spawn_ctx.pipeline_position,
                PipelinePosition::Only | PipelinePosition::Last
            )
            .then(|| s.stdout.clone())
        });
        let stderr_tee = job_streams.as_ref().map(|s| s.stderr.clone());

        let stdout_task = stdout_pipe.map(|pipe| {
            tokio::spawn(async move {
                drain_to_stream_teed(pipe, stdout_clone, stdout_tee).await;
            })
        });

        let stderr_task = stderr_pipe.map(|pipe| {
            tokio::spawn(async move {
                drain_to_stream_teed(pipe, stderr_clone, stderr_tee).await;
            })
        });

        let cancelled_before_wait = spawn_ctx.cancel.is_cancelled();
        let status = match crate::kernel::wait_or_kill(
            &mut child,
            kill_target.as_ref(),
            &spawn_ctx.cancel,
            spawn_ctx.kill_grace,
        )
        .await
        {
            Ok(s) => s,
            Err(e) => {
                // stdin-copy task is aborted by `_stdin_copy_guard` on return.
                if let Some(task) = stdout_task {
                    task.abort();
                    let _ = task.await;
                }
                if let Some(task) = stderr_task {
                    task.abort();
                    let _ = task.await;
                }
                return ExecResult::failure(1, format!("{}: failed to wait: {}", label, e));
            }
        };

        // On cancel, abort the drain tasks (the child's pipes are gone;
        // late output is lost but predictable death beats partial capture).
        // On normal exit, await drains so we don't lose buffered output.
        if cancelled_before_wait || spawn_ctx.cancel.is_cancelled() {
            if let Some(task) = stdout_task {
                task.abort();
                let _ = task.await;
            }
            if let Some(task) = stderr_task {
                task.abort();
                let _ = task.await;
            }
        } else {
            if let Some(task) = stdout_task {
                // Ignore join error — the drain task logs its own errors
                let _ = task.await;
            }
            if let Some(task) = stderr_task {
                let _ = task.await;
            }
        }

        let code = crate::kernel::exit_code_from_status(&status);

        // Read stdout as RAW bytes: text if valid UTF-8, else a Bytes
        // result, so `curl url`, `curl url > file.bin`, etc. keep binary
        // intact. stderr stays text. See docs/binary-data.md.
        let stdout = stdout_stream.read().await;
        let mut stderr = stderr_stream.read_string().await;
        let mut result = ExecResult::success_text_or_bytes(stdout).with_code(code);

        // Both streams are fixed-size rings regardless of `ctx.output_limit`
        // (that machinery only runs post-hoc, in `execute_pipeline`, and only
        // when enabled). With the limit disabled — the repl/embedded/test
        // default — an overflow here used to be silent: `write` evicted the
        // oldest bytes and bumped `bytes_evicted`, but nothing ever read that
        // counter, so a >10MB stdout reported clean success with its head
        // quietly gone (GH #191). Surface it loudly instead.
        if stderr_stream.has_overflowed().await {
            let stats = stderr_stream.stats().await;
            stderr = format!("{}{stderr}", stats.overflow_marker("stderr"));
        }
        if stdout_stream.has_overflowed().await {
            // The marker goes in stderr, never prepended into `result`'s
            // stdout payload: stdout may be binary
            // (`success_text_or_bytes` yields a `Bytes` result for
            // non-UTF-8 data — e.g. `curl` fetching a >10MB binary), and
            // string-formatting a marker into it would lossily reinterpret
            // bytes as text, introducing a SECOND, different kind of
            // corruption on top of the eviction itself.
            //
            // Only stdout overflow flips `did_spill` — exit-code integrity
            // tracks stdout, matching the enabled-limit path's contract
            // (stderr overflow alone doesn't remap the exit code).
            let stats = stdout_stream.stats().await;
            stderr = format!("{}{stderr}", stats.overflow_marker("stdout"));
            result.did_spill = true;
        }
        result.err = stderr;
        result
    }
}
