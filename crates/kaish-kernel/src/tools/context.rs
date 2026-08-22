//! Execution context for tools.

use std::collections::HashMap;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

use async_trait::async_trait;

use crate::ast::Value;
use crate::backend::{KernelBackend, LocalBackend};
use crate::dispatch::PipelinePosition;
use crate::ignore_config::IgnoreConfig;
use crate::interpreter::{ExecResult, Scope};
use crate::output_limit::OutputLimitConfig;
use crate::scheduler::{JobManager, PipeReader, PipeWriter, StderrStream};
use crate::tools::ToolRegistry;
use crate::trash::TrashBackend;
use crate::vfs::VfsRouter;
use kaish_vfs::ByteBudget;
use tokio::sync::oneshot;
use tokio_util::sync::CancellationToken;

use crate::interpreter::OutputFormat;

use super::traits::ToolSchema;

/// Output context determines how command output should be formatted.
///
/// Different contexts prefer different output formats:
/// - **Interactive** — Pretty columns, colors, traditional tree (TTY/REPL)
/// - **Piped** — Raw output for pipeline processing
/// - **Model** — Token-efficient compact formats (MCP server / agent context)
/// - **Script** — Non-interactive script execution
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[non_exhaustive]
pub enum OutputContext {
    /// Interactive TTY/REPL - use human-friendly format with colors.
    #[default]
    Interactive,
    /// Output to another command - use raw output for pipes.
    Piped,
    /// MCP server / agent context - use token-efficient model format.
    Model,
    /// Non-interactive script - use raw output.
    Script,
}

/// Execution context passed to tools.
///
/// Provides access to the backend (for file operations and tool dispatch),
/// scope, and other kernel state.
pub struct ExecContext {
    /// Kernel backend for I/O operations.
    ///
    /// This is the preferred way to access filesystem operations.
    /// Use `backend.read()`, `backend.write()`, etc.
    pub backend: Arc<dyn KernelBackend>,
    /// Variable scope.
    pub scope: Scope,
    /// Current working directory (VFS path).
    pub cwd: PathBuf,
    /// Previous working directory (for `cd -`).
    pub prev_cwd: Option<PathBuf>,
    /// Standard input for the tool (from a redirect, heredoc, here-string, or
    /// `ExecuteOptions::stdin`). Bytes-typed (GH #176) so a `< binfile`
    /// redirect over non-UTF-8 content reaches a byte-aware builtin intact
    /// instead of erroring at redirect setup; a text-only builtin still
    /// refuses it loudly when it calls `read_stdin_to_text`.
    pub stdin: Option<Vec<u8>>,
    /// Structured data from pipeline (pre-parsed JSON from previous command).
    /// Tools can check this before parsing stdin to avoid redundant JSON parsing.
    pub stdin_data: Option<Value>,
    /// Sideband receiver for the previous stage's structured `.data`, set by the
    /// concurrent pipeline runner. Resolved lazily via [`Self::resolve_stdin`]
    /// AFTER the pipe is drained — never pre-read — so a streaming upstream that
    /// only sends its data after writing the pipe can't deadlock a consumer that
    /// awaits it. Non-`Clone`, so it's moved on resolve.
    pub stdin_data_rx: Option<oneshot::Receiver<Option<Value>>>,
    /// Streaming pipe input (set when this command is in a concurrent pipeline).
    pub pipe_stdin: Option<PipeReader>,
    /// Streaming pipe output (set when this command is in a concurrent pipeline).
    pub pipe_stdout: Option<PipeWriter>,
    /// Tool schemas for help command.
    ///
    /// `Arc<[…]>` rather than `Vec`: the full builtin schema catalog (~70
    /// entries, each with its own `Vec`s and `String`s) is snapshotted into a
    /// fresh `ExecContext` at every command dispatch and pipeline/fork child. As
    /// a `Vec` that was a deep clone of the whole catalog per command; as an
    /// `Arc<[…]>` it's a refcount bump (GH #48, item 8). Immutable after the
    /// kernel seeds it, so a shared slice is the right shape.
    pub tool_schemas: Arc<[ToolSchema]>,
    /// Tool registry reference (for tools that need to inspect available tools).
    pub tools: Option<Arc<ToolRegistry>>,
    /// Job manager for background jobs (optional).
    pub job_manager: Option<Arc<JobManager>>,
    /// Kernel stderr stream for real-time error output from pipeline stages.
    ///
    /// When set, pipeline stages write stderr here instead of buffering in
    /// `ExecResult.err`. This allows stderr from all stages to stream to
    /// the terminal (or other sink) concurrently, matching bash behavior.
    pub stderr: Option<StderrStream>,
    /// Position of this command within a pipeline (for stdio decisions).
    pub pipeline_position: PipelinePosition,
    /// Whether we're running in interactive (REPL) mode.
    pub interactive: bool,
    /// Arm `PR_SET_PDEATHSIG(SIGKILL)` on external commands spawned from this
    /// context, so a hard-killed kaish process cannot orphan them.
    ///
    /// Seeded from `KernelConfig::kill_children_on_parent_death` — read that
    /// field for the tradeoff and the macOS gap. It lives here, not on the
    /// `Kernel`, because both external-command spawn sites (`Kernel::
    /// try_execute_external` and `dispatch.rs`'s `BackendDispatcher`) reach an
    /// `ExecContext` and only one of them reaches a `Kernel`; one home keeps
    /// the two `pre_exec` blocks from drifting.
    ///
    /// `false` for a stand-alone `ExecContext` built outside a kernel, which
    /// is the pre-existing behavior.
    pub kill_children_on_parent_death: bool,
    /// Command aliases (name → expansion string).
    pub aliases: HashMap<String, String>,
    /// Ignore file configuration for file-walking tools.
    pub ignore_config: IgnoreConfig,
    /// Output size limit configuration for agent safety.
    pub output_limit: OutputLimitConfig,
    /// Whether external command execution is allowed.
    ///
    /// When `false`, external commands (PATH lookup, `exec`, `spawn`) are blocked.
    /// Only kaish builtins and backend-registered tools (MCP) are available.
    pub allow_external_commands: bool,
    /// Trash backend for safe file deletion.
    ///
    /// Always present when the kernel creates the context (even if `set -o trash`
    /// is off — the backend exists so `kaish-trash list/restore/empty` work
    /// regardless of the trash flag).
    pub trash_backend: Option<Arc<dyn TrashBackend>>,
    /// Terminal state for job control (interactive mode, Unix only).
    #[cfg(all(unix, feature = "subprocess"))]
    pub terminal_state: Option<std::sync::Arc<crate::terminal::TerminalState>>,
    /// Command dispatcher for re-dispatching through the full resolution chain.
    ///
    /// When set (via `Kernel::into_arc()`), builtins like `timeout` can dispatch
    /// inner commands through the full chain (user tools → builtins → .kai scripts
    /// → external commands) instead of being limited to `backend.call_tool()`.
    ///
    /// `None` when the Kernel was not wrapped via `into_arc()`.
    pub dispatcher: Option<Arc<dyn crate::dispatch::CommandDispatcher>>,
    /// Cancellation token for this execution path.
    ///
    /// Populated by the kernel at execute entry, then propagated through pipeline
    /// stages, foreground forks (scatter workers, concurrent pipeline stages,
    /// `$(...)` cmdsubs), and into spawned external children. When the token
    /// fires, externals receive SIGTERM/SIGKILL via the `wait_or_kill` helper.
    ///
    /// Default for stand-alone `ExecContext` constructors is a fresh, never-fired
    /// token so non-kernel test contexts behave as before.
    pub cancel: CancellationToken,
    /// Per-execution output format override set by a builtin's GlobalFlags
    /// flatten (e.g. `--json`). The dispatcher reads this after `tool.execute()`
    /// returns and applies the format via `apply_output_format`.
    ///
    /// Builtins set this via `GlobalFlags::apply(ctx)`; external commands
    /// don't touch it.
    pub output_format: Option<OutputFormat>,

    /// Shared VFS memory budget for this kernel's `MemoryFs` mounts.
    ///
    /// `Arc`-cloned from the owning `Kernel` (or its fork parent) so all
    /// concurrent execution paths draw from the same pool. `None` means
    /// unbounded. Populated by `Kernel::assemble` and forwarded through
    /// `child_for_pipeline` / `fork_inner` so background jobs and scatter
    /// workers see the same cap as foreground execution.
    pub vfs_budget: Option<Arc<ByteBudget>>,

    /// The per-execute timeout watchdog, when a script timeout is in effect.
    ///
    /// Populated by the kernel at execute entry (alongside `cancel`) and
    /// shared through `child_for_pipeline` so forks and pipeline stages can
    /// acquire patient holds against the same script clock. `None` when no
    /// timeout is configured — `ToolCtx::patient` then returns an inert guard.
    pub watchdog: Option<Arc<crate::watchdog::Watchdog>>,

    /// Active overlay handle when the kernel was constructed with `overlay: true`.
    ///
    /// `Arc`-cloned so forks and pipeline stages share the same transaction.
    /// `None` when no overlay is active (most kernels).
    #[cfg(all(feature = "localfs", feature = "overlay"))]
    pub overlay_handle: Option<Arc<crate::kernel::OverlayHandle>>,

}

/// What the write-model gate chose for a single truncating overwrite.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum MutationAction {
    /// Write now — new file, append, excluded path, or trash off.
    Proceed,
    /// Snapshot the prior content to trash, then write.
    TrashFirst,
}

/// What a snapshotted overwrite must still find at the target before it
/// writes — the compare-and-swap expectation `overwrite_checked` enforces.
///
/// The trash path already holds the prior bytes (it had to copy them to the
/// trash), so the expectation compares bytes.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum OverwriteExpectation {
    /// The exact prior bytes, from the trash snapshot.
    Bytes(Vec<u8>),
}

/// What each snapshotted target must still look like when the caller writes
/// it, keyed by resolved path (see `overwrite_checked`).
///
/// Every existing target the trash snapshotted appears. A new file, an
/// append, and an excluded or unsnapshotted path are absent, because none of
/// them has prior content to lose.
pub type GateExpectations = std::collections::HashMap<PathBuf, OverwriteExpectation>;

/// Real paths the trash gate skips: host scratch under `/tmp`, where
/// snapshotting prior content to trash is pointless. Shared by `rm`'s delete
/// gate (`decide_rm_action`) and the overwrite gate (`decide_mutation_action`)
/// so the exclusion can't drift between them. `Path::starts_with` is
/// component-aware, so `/tmp_file` does not match `/tmp`.
///
/// Note: kaish's own in-memory VFS mounts (e.g. `/v/blobs`) have `real_path ==
/// None`, so they are handled by the no-real-path gating path, not here — there
/// is deliberately no lexical `/v` exclusion. Mount-coverage routing delegates
/// unclaimed `/v/*` to the embedder's backend, whose *real* content under `/v`
/// (a real path like `/v/cas/blob.bin`) must keep the trash safety net; a
/// `/v` prefix exclusion here would silently strip it.
pub(crate) fn is_trash_excluded(real_path: Option<&Path>) -> bool {
    matches!(real_path, Some(rp) if rp.starts_with("/tmp"))
}

/// Decide whether a truncating overwrite snapshots to trash first, mirroring
/// `rm`'s trash priority. Pure so the decision table is unit-testable in
/// isolation.
///
/// - A non-existent target or an append has nothing to lose → `Proceed`.
/// - A real path under `/tmp` (host scratch) is excluded (matches `rm`) → `Proceed`.
/// - `TrashFirst` when trash is on **and** the prior content fits under
///   `trash_max_size` (a file too big to snapshot can't be backed up, so it
///   falls through, exactly like `rm`); else `Proceed`.
///
/// An overlay/in-memory target has `real_path == None`, so it is *not*
/// excluded and still snapshots — the protection is about agent-operation
/// safety, not just real-FS data (Amy, 2026-06-17).
pub(crate) fn decide_mutation_action(
    trash_enabled: bool,
    real_path: Option<&Path>,
    target_exists: bool,
    is_append: bool,
    file_size: u64,
    trash_max_size: u64,
) -> MutationAction {
    if !target_exists || is_append {
        return MutationAction::Proceed;
    }
    if is_trash_excluded(real_path) {
        return MutationAction::Proceed;
    }
    if trash_enabled && file_size <= trash_max_size {
        return MutationAction::TrashFirst;
    }
    // A target too large for the trash is written directly. kaish does not
    // hold it back: nothing in the kernel decides whether an overwrite is
    // allowed — an embedder that wants to refuse one reads the plan first.
    MutationAction::Proceed
}

/// Overwrite `resolved` with `content`, compare-and-swapping against
/// `expected` first when there is one. The target's current state is
/// re-derived and must match, else a concurrent change is a loud conflict —
/// never a silent clobber. Binary-safe (raw bytes, unlike the `String`-based
/// `PatchOp` CAS). Shared by the byte-oriented gated builtins via
/// `ExecContext::overwrite_checked` (`tee`/`write`/`dd`) and directly by
/// `cp`'s free copy path.
///
/// This catches a change between the snapshot and the write. It does not
/// make the write OS-atomic — a crash mid-write can still truncate (the
/// atomic write-temp-then-rename primitive is a tracked write-model
/// residual).
pub(crate) async fn cas_overwrite(
    backend: &dyn KernelBackend,
    resolved: &Path,
    content: &[u8],
    expected: Option<&OverwriteExpectation>,
) -> Result<(), crate::backend::BackendError> {
    // A re-read or re-digest failure propagates loudly — never
    // `unwrap_or_default()` to empty bytes, which would false-match an empty
    // snapshot (silent overwrite) or report a bogus "file changed" for a real
    // I/O error. A target that vanished since the gate is a change → abort.
    match expected {
        Some(OverwriteExpectation::Bytes(exp)) => {
            let current = backend.read(resolved, None).await?;
            if current != *exp {
                return Err(concurrent_change_error(resolved));
            }
        }
        None => {}
    }
    backend
        .write(resolved, content, crate::backend::WriteMode::Overwrite)
        .await
}

/// One wording for "somebody else wrote this while the write-model gate was
/// deciding".
fn concurrent_change_error(resolved: &Path) -> crate::backend::BackendError {
    crate::backend::BackendError::InvalidOperation(format!(
        "{}: changed since the write-model gate checked it (concurrent write); \
         aborting overwrite",
        resolved.display()
    ))
}

impl ExecContext {
    /// Create a new execution context with a VFS (uses LocalBackend without tools).
    ///
    /// This constructor is for backward compatibility and tests that don't need tool dispatch.
    /// For full tool support, use `with_vfs_and_tools`.
    pub fn new(vfs: Arc<VfsRouter>) -> Self {
        Self {
            backend: Arc::new(LocalBackend::new(vfs)),
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
        }
    }

    /// Create a new execution context with VFS and tool registry.
    ///
    /// This is the preferred constructor for full kaish operation where
    /// tools need to be dispatched through the backend.
    pub fn with_vfs_and_tools(vfs: Arc<VfsRouter>, tools: Arc<ToolRegistry>) -> Self {
        Self {
            backend: Arc::new(LocalBackend::with_tools(vfs, tools.clone())),
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: Some(tools),
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
        }
    }

    /// Create a new execution context with a custom backend.
    pub fn with_backend(backend: Arc<dyn KernelBackend>) -> Self {
        Self {
            backend,
            scope: Scope::new(),
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
        }
    }

    /// Create a context with VFS, tools, and a specific scope.
    pub fn with_vfs_tools_and_scope(vfs: Arc<VfsRouter>, tools: Arc<ToolRegistry>, scope: Scope) -> Self {
        Self {
            backend: Arc::new(LocalBackend::with_tools(vfs, tools.clone())),
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: Some(tools),
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
        }
    }

    /// Create a context with a specific scope (uses LocalBackend without tools).
    ///
    /// For tests that don't need tool dispatch. For full tool support,
    /// use `with_vfs_tools_and_scope`.
    pub fn with_scope(vfs: Arc<VfsRouter>, scope: Scope) -> Self {
        Self {
            backend: Arc::new(LocalBackend::new(vfs)),
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
        }
    }

    /// Create a context with a custom backend and scope.
    pub fn with_backend_and_scope(backend: Arc<dyn KernelBackend>, scope: Scope) -> Self {
        Self {
            backend,
            scope,
            cwd: PathBuf::from("/"),
            prev_cwd: None,
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: None,
            tool_schemas: Vec::new().into(),
            tools: None,
            job_manager: None,
            pipeline_position: PipelinePosition::Only,
            interactive: false,
            kill_children_on_parent_death: false,
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            vfs_budget: None,
            watchdog: None,
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: None,
        }
    }

    /// Set the available tool schemas (for help command).
    ///
    /// Takes a `Vec` for caller convenience and converts to the shared
    /// `Arc<[…]>` the field stores (see the field docs; GH #48).
    pub fn set_tool_schemas(&mut self, schemas: Vec<ToolSchema>) {
        self.tool_schemas = schemas.into();
    }

    /// Set the tool registry reference.
    pub fn set_tools(&mut self, tools: Arc<ToolRegistry>) {
        self.tools = Some(tools);
    }

    /// Set the job manager for background job tracking.
    pub fn set_job_manager(&mut self, manager: Arc<JobManager>) {
        self.job_manager = Some(manager);
    }

    /// Set the trash backend.
    pub fn set_trash_backend(&mut self, backend: Arc<dyn TrashBackend>) {
        self.trash_backend = Some(backend);
    }

    /// Set stdin for this execution.
    ///
    /// An explicit stdin buffer (`< file`, heredoc, here-string, or a pipeline
    /// hand-off) supersedes any inherited lazy `pipe_stdin`. Since `read_stdin_*`
    /// prefers `pipe_stdin`, clear it here so redirect precedence holds — a
    /// `< file` must beat a frontend-seeded piped stdin. Accepts anything
    /// `Into<Vec<u8>>` — a `String`/`&str` (heredocs, here-strings, most
    /// callers) or a raw `Vec<u8>` (a `< binfile` redirect, GH #176) both work.
    pub fn set_stdin(&mut self, stdin: impl Into<Vec<u8>>) {
        self.stdin = Some(stdin.into());
        self.pipe_stdin = None;
    }

    /// Get stdin, consuming it.
    pub fn take_stdin(&mut self) -> Option<Vec<u8>> {
        self.stdin.take()
    }

    /// Set both text stdin and structured data.
    ///
    /// Use this when passing output through a pipeline where the previous
    /// command produced structured data (e.g., JSON from MCP tools). The text
    /// side is always a genuine `String` here (structured-data hand-off is a
    /// JSON-producing pipeline stage, never binary).
    pub fn set_stdin_with_data(&mut self, text: String, data: Option<Value>) {
        self.stdin = Some(text.into_bytes());
        self.stdin_data = data;
    }

    /// Take structured data if available, consuming it.
    ///
    /// Tools can use this to avoid re-parsing JSON that was already parsed
    /// by a previous command in the pipeline.
    pub fn take_stdin_data(&mut self) -> Option<Value> {
        self.stdin_data.take()
    }

    /// Resolve stdin for a builtin that can consume *either* structured `.data`
    /// or raw text from the previous pipeline stage (jq, scatter, …). Returns
    /// `(Some(data), _)` when the upstream produced structured data, else
    /// `(None, text)`.
    ///
    /// Ordering matters and is the whole point: the pipe is drained to text
    /// FIRST, which runs the upstream producer to completion (it can't be parked
    /// on pipe backpressure), and only THEN is the structured-data sideband
    /// awaited — by which point the producer has definitely sent it (it sends
    /// before writing/closing its pipe). A streaming upstream that emits a lot
    /// of text before sending its (absent) data therefore can't deadlock us, and
    /// a fast structured producer (`seq`) is no longer lost to a startup race
    /// that a one-shot `try_recv` used to drop on the floor.
    pub async fn resolve_stdin(&mut self) -> Result<(Option<Value>, String), String> {
        // Data set directly on the context (not via the pipeline sideband) wins
        // and needs no pipe — e.g. a non-pipeline caller seeded `stdin_data`.
        if let Some(data) = self.stdin_data.take() {
            return Ok((Some(data), String::new()));
        }
        // Drain the pipe (and/or buffered stdin) to text — unblocks the upstream.
        let text = self.read_stdin_to_text().await?.unwrap_or_default();
        // Upstream has now finished; its structured data (if any) is waiting.
        if let Some(rx) = self.stdin_data_rx.take()
            && let Ok(Some(data)) = rx.await
        {
            return Ok((Some(data), text));
        }
        Ok((None, text))
    }

    /// Resolve a path relative to cwd, normalizing `.` and `..` components.
    pub fn resolve_path(&self, path: &str) -> PathBuf {
        let raw = if path.starts_with('/') {
            PathBuf::from(path)
        } else {
            self.cwd.join(path)
        };
        normalize_path(&raw)
    }

    /// Change the current working directory.
    ///
    /// Saves the old directory for `cd -` support.
    pub fn set_cwd(&mut self, path: PathBuf) {
        let previous = self.cwd.clone();
        self.prev_cwd = Some(previous.clone());
        self.cwd = path;
        // `$PWD`/`$OLDPWD` follow the working directory from HERE, the one
        // place it changes. They used to be whatever the process inherited and
        // nothing ever wrote them, so `cd /tmp; echo $PWD` reported the
        // directory the shell started in while `pwd` reported `/tmp` — a wrong
        // value with nothing to say so, and the validator vouched for the name
        // because `scope_tracker` lists both as known. `$OLDPWD` was worse than
        // stale: it held a directory from the INVOKING shell's history.
        //
        // Here rather than in the `cd` builtin so a second writer cannot drift
        // from the first.
        self.sync_cwd_vars(&previous);
    }

    /// Write `$PWD`/`$OLDPWD` from the context's own directories.
    ///
    /// Kept next to [`Self::set_cwd`] because the kernel also calls it once at
    /// startup: a script that reads `$PWD` before its first `cd` must not see
    /// the inherited environment's value either.
    pub(crate) fn sync_cwd_vars(&mut self, previous: &Path) {
        self.scope.set_global("PWD", Value::String(self.cwd.to_string_lossy().into_owned()));
        self.scope.set_global(
            "OLDPWD",
            Value::String(previous.to_string_lossy().into_owned()),
        );
    }

    /// Get the previous working directory (for `cd -`).
    pub fn get_prev_cwd(&self) -> Option<&PathBuf> {
        self.prev_cwd.as_ref()
    }

    /// Read stdin as text, erroring on non-UTF-8 instead of silently
    /// lossy-decoding it (which corrupts binary with `U+FFFD`).
    ///
    /// The strict counterpart to [`Self::read_stdin_to_bytes`], for text-only
    /// builtins (`grep`, `sed`, `awk`, `cut`, `sort`, `jq`, …): a binary stream
    /// is a loud error, not a mangle. Returns `Ok(None)` when there is no stdin
    /// at all. The `Err` is a ready-to-use message; callers prefix their name.
    /// See `docs/binary-data.md`.
    pub async fn read_stdin_to_text(&mut self) -> Result<Option<String>, String> {
        match self.read_stdin_to_bytes().await? {
            None => Ok(None),
            Some(bytes) => String::from_utf8(bytes).map(Some).map_err(|_| {
                "input is not valid UTF-8 (binary data?) — pipe through base64/xxd \
                 or use a binary-aware tool (cat, dd, cmp, wc -c)"
                    .to_string()
            }),
        }
    }

    /// Read all of stdin as raw bytes, preserving binary intact.
    ///
    /// The byte-clean counterpart to [`Self::read_stdin_to_text`], for
    /// binary-aware builtins (`base64`, `xxd`, `checksum`, `wc -c`, `cmp`, …).
    /// Returns `Ok(None)` when there is no stdin at all (no pipe and no
    /// buffer); an empty pipe yields `Ok(Some(vec![]))`. The buffered source is
    /// already bytes-typed (GH #176), so this is a plain move, never a
    /// re-encode. See `docs/binary-data.md`.
    ///
    /// Anything an earlier [`Self::read_stdin_line`] left behind comes first,
    /// then the rest of the pipe — `read x; cat` gives `cat` everything after
    /// the line `read` took, in order, and nothing twice.
    ///
    /// A failed pipe read is `Err`, never `Ok(None)`. "The pipe broke" and
    /// "there was no stdin" are different facts, and collapsing them hands the
    /// builtin a short read dressed up as empty input — `wc` would report 0
    /// lines and exit 0 on a stream that died halfway, and the bytes already
    /// read would go with it. [`Self::read_stdin_line`] propagates this same
    /// error from this same reader; the two now agree. The `Err` is a
    /// ready-to-use message; callers prefix their name.
    pub async fn read_stdin_to_bytes(&mut self) -> Result<Option<Vec<u8>>, String> {
        let leftover = self.stdin.take();
        match self.pipe_stdin.take() {
            Some(mut reader) => {
                use tokio::io::AsyncReadExt;
                let mut buf = leftover.unwrap_or_default();
                reader
                    .read_to_end(&mut buf)
                    .await
                    .map_err(|e| format!("reading stdin: {e}"))?;
                Ok(Some(buf))
            }
            None => Ok(leftover),
        }
    }

    /// Read one line from stdin, leaving the rest for the next reader.
    ///
    /// This is the stream-shaped counterpart to [`Self::read_stdin_to_bytes`]:
    /// it takes a single line and keeps everything after it, so `read x; read y`
    /// binds two lines and `read x; cat` hands `cat` the remainder. Draining to
    /// EOF for one line would discard the rest of the stream — there is no way
    /// to put it back once a pipe has been read.
    ///
    /// The trailing newline is stripped, and a final line without one is still
    /// a line. Returns `Ok(None)` at end of input — no line left, which is a
    /// fact the caller reports, not an empty binding. `Err` on non-UTF-8, with
    /// the same message shape as [`Self::read_stdin_to_text`].
    pub async fn read_stdin_line(&mut self) -> Result<Option<String>, String> {
        loop {
            // A complete line already buffered? Take it and keep the rest.
            if let Some(buf) = self.stdin.as_mut()
                && let Some(nl) = buf.iter().position(|b| *b == b'\n')
            {
                let rest = buf.split_off(nl + 1);
                let mut line = std::mem::replace(buf, rest);
                line.pop(); // the '\n' itself
                if line.last() == Some(&b'\r') {
                    line.pop();
                }
                // Drop an emptied buffer only when nothing can refill it, so
                // `read_stdin_to_bytes` can still tell "no stdin" (None) from
                // "stdin that is now empty" (Some(vec![])).
                if self.pipe_stdin.is_none()
                    && self.stdin.as_ref().is_some_and(|b| b.is_empty())
                {
                    self.stdin = None;
                }
                return decode_stdin_line(line).map(Some);
            }

            // No newline buffered — pull another chunk from the pipe. The
            // reader stays in place: taking it would strand the remainder.
            if let Some(reader) = self.pipe_stdin.as_mut() {
                use tokio::io::AsyncReadExt;
                let mut chunk = [0u8; 8192];
                match reader.read(&mut chunk).await {
                    Ok(0) => {
                        self.pipe_stdin = None; // EOF; fall through to the tail
                    }
                    Ok(n) => {
                        self.stdin
                            .get_or_insert_with(Vec::new)
                            .extend_from_slice(&chunk[..n]);
                    }
                    Err(e) => return Err(format!("reading stdin: {e}")),
                }
                continue;
            }

            // Nothing left to read: whatever is buffered is the last line.
            return match self.stdin.take() {
                Some(buf) if !buf.is_empty() => decode_stdin_line(buf).map(Some),
                _ => Ok(None),
            };
        }
    }

    /// Create a child context for a pipeline stage.
    ///
    /// Shares backend, tools, job_manager, aliases, cwd, and scope
    /// but has independent stdin/stdout pipes.
    pub fn child_for_pipeline(&self) -> Self {
        Self {
            backend: self.backend.clone(),
            scope: self.scope.clone(),
            cwd: self.cwd.clone(),
            prev_cwd: self.prev_cwd.clone(),
            stdin: None,
            stdin_data: None,
            stdin_data_rx: None,
            pipe_stdin: None,
            pipe_stdout: None,
            stderr: self.stderr.clone(),
            tool_schemas: self.tool_schemas.clone(),
            tools: self.tools.clone(),
            job_manager: self.job_manager.clone(),
            pipeline_position: PipelinePosition::Only,
            interactive: self.interactive,
            kill_children_on_parent_death: self.kill_children_on_parent_death,
            aliases: self.aliases.clone(),
            ignore_config: self.ignore_config.clone(),
            output_limit: self.output_limit.clone(),
            allow_external_commands: self.allow_external_commands,
            trash_backend: self.trash_backend.clone(),
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: self.terminal_state.clone(),
            dispatcher: self.dispatcher.clone(),
            cancel: self.cancel.clone(),
            // Output format is per-execution; child pipeline stages start fresh.
            output_format: None,
            // Budget is shared: the child draws from the same pool as the parent.
            vfs_budget: self.vfs_budget.clone(),
            // Watchdog is shared: a patient hold in a pipeline stage or fork
            // suspends the same script clock as foreground execution.
            watchdog: self.watchdog.clone(),
            // Overlay handle is shared: pipeline stages share the same transaction.
            #[cfg(all(feature = "localfs", feature = "overlay"))]
            overlay_handle: self.overlay_handle.clone(),
        }
    }

    /// Build an `IgnoreFilter` from the current ignore configuration.
    ///
    /// Returns `None` if no filtering is configured.
    pub async fn build_ignore_filter(&self, root: &std::path::Path) -> Option<crate::walker::IgnoreFilter> {
        use crate::backend_walker_fs::BackendWalkerFs;
        let fs = BackendWalkerFs(self.backend.as_ref());
        self.ignore_config.build_filter(root, &fs).await
    }

    /// Snapshot a batch of truncating overwrites into the trash, the way `rm`
    /// snapshots deletes — so `tee`/`patch`/`sed -i` can't clobber a file
    /// under `set -o trash` without leaving a recoverable prior copy.
    ///
    /// Each target is `(display_path, is_append)`. A path that doesn't exist
    /// yet or is an append has nothing to lose and passes. For an existing
    /// file under `set -o trash`, the prior content is copied to trash first
    /// (via `trash_bytes`) so it's recoverable; the file is left in place for
    /// the caller to overwrite. With trash off, every target passes: the
    /// kernel does not decide whether an overwrite is allowed.
    ///
    /// `Ok(snapshots)` means every snapshot is done and the caller may write
    /// all targets; `snapshots` maps each trash-snapshotted target's resolved
    /// path to its prior bytes, so a byte-oriented caller can pass them as the
    /// `expected` to `overwrite_checked` for a binary-safe compare-and-swap.
    /// `Err(result)` is what the caller must return verbatim — a trash failure
    /// is an error, never a fall-through to a destructive overwrite.
    // `ExecResult` IS the error here — `Err(result)` is what the caller
    // returns verbatim, which is the point of the signature. Boxing it to
    // satisfy `result_large_err` would put a `*deref` at every call site to
    // save an allocation on a path that already reads and copies file bytes.
    #[allow(clippy::result_large_err)]
    pub async fn snapshot_overwrites(
        &mut self,
        command: &str,
        targets: &[(String, bool)],
    ) -> Result<GateExpectations, ExecResult> {
        let mut expectations = GateExpectations::new();
        let trash_enabled = self.scope.trash_enabled();
        // Fast path: nothing is trashed, so this costs one branch and
        // allocates nothing.
        if !trash_enabled {
            return Ok(expectations);
        }
        let trash_max_size = self.scope.trash_max_size();

        struct Decided {
            display: String,
            resolved: PathBuf,
            action: MutationAction,
        }
        // Dedup by resolved path (keep first): a multi-file patch with an
        // explicit target lists the same file once per hunk-group, and we must
        // not snapshot it N times or list it N times in the request.
        let mut seen = std::collections::HashSet::new();
        let mut decided = Vec::with_capacity(targets.len());
        for (display, is_append) in targets {
            let resolved = self.resolve_path(display);
            if !seen.insert(resolved.clone()) {
                continue;
            }
            // `real` is used only for the exclusion decision (/tmp, /v); the
            // snapshot reads bytes through the backend, not the real path.
            let real = self.backend.resolve_real_path(Path::new(&resolved));
            let exists = self.backend.exists(Path::new(&resolved)).await;
            // Prior size decides trash eligibility (a file too big to snapshot
            // can't be backed up). Only stat an existing target.
            let size = if exists {
                self.backend
                    .stat(Path::new(&resolved))
                    .await
                    .map(|e| e.size)
                    .unwrap_or(0)
            } else {
                0
            };
            let action = decide_mutation_action(
                trash_enabled,
                real.as_deref(),
                exists,
                *is_append,
                size,
                trash_max_size,
            );
            decided.push(Decided {
                display: display.clone(),
                resolved,
                action,
            });
        }

        // Snapshot prior content for every trash-first target before any write,
        // keeping the bytes so a byte-oriented caller can CAS against them.
        for d in &decided {
            if matches!(d.action, MutationAction::TrashFirst) {
                match self.snapshot_for_overwrite(&d.display, &d.resolved).await {
                    Ok(bytes) => {
                        expectations.insert(d.resolved.clone(), OverwriteExpectation::Bytes(bytes));
                    }
                    Err(e) => return Err(ExecResult::failure(1, format!("{command}: {e}"))),
                }
            }
        }
        Ok(expectations)
    }

    /// Copy the prior content of `resolved` into the trash before it's
    /// overwritten, returning those bytes for the caller's compare-and-swap.
    ///
    /// We **copy** (not move): the builtin overwrites the file in place next,
    /// and read-modify-write callers (`patch`, `sed -i`) still need to read it —
    /// the file keeps its identity, only its content changes. (`rm` *moves*
    /// because removal is the op; an overwrite backs up the prior bytes.) Reads
    /// through the backend so a real, overlay, or in-memory file is handled the
    /// same way. A missing trash backend or a trash failure is an error — never
    /// a silent fall-through to a destructive overwrite.
    async fn snapshot_for_overwrite(
        &self,
        display: &str,
        resolved: &Path,
    ) -> Result<Vec<u8>, String> {
        let trash = self
            .trash_backend
            .as_ref()
            .ok_or_else(|| "trash backend not available".to_string())?;
        let bytes = self
            .backend
            .read(resolved, None)
            .await
            .map_err(|e| format!("{display}: {e}"))?;
        trash
            .trash_bytes(Path::new(display), &bytes)
            .await
            .map_err(|e| format!("{display}: trash failed: {e}"))?;
        Ok(bytes)
    }

    /// Overwrite `resolved` with `content`. When `expected` is `Some`, this is a
    /// binary-safe compare-and-swap: the current bytes are re-read and must
    /// equal `expected` (the gate's snapshot), else it errors — a concurrent
    /// change since the gate is a loud conflict, never a silent clobber. Unlike
    /// the `String`-based `PatchOp::Replace` CAS used by `patch`/`sed -i`, this
    /// operates on raw bytes, so binary overwrites (`tee`, `write`, `dd`, `cp`,
    /// `mv`) keep the same protection. It is *not* OS-atomic — a crash mid-write
    /// can still truncate; the atomic write-temp-then-rename primitive remains a
    /// tracked write-model residual.
    pub(crate) async fn overwrite_checked(
        &self,
        resolved: &Path,
        content: &[u8],
        expected: Option<&OverwriteExpectation>,
    ) -> Result<(), String> {
        cas_overwrite(&*self.backend, resolved, content, expected)
            .await
            .map_err(|e| e.to_string())
    }

    /// Expand a glob pattern to matching file paths.
    ///
    /// Returns the matched paths (absolute). Used by builtins that accept glob
    /// patterns in their path arguments (ls, cat, head, tail, wc, etc.).
    pub async fn expand_glob(&self, pattern: &str) -> Result<Vec<PathBuf>, String> {
        use crate::backend_walker_fs::BackendWalkerFs;
        use crate::walker::{EntryTypes, FileWalker, GlobPath, WalkOptions};

        let glob = GlobPath::new(pattern).map_err(|e| format!("invalid pattern: {}", e))?;

        let root = if glob.is_anchored() {
            self.resolve_path("/")
        } else {
            self.resolve_path(".")
        };

        let options = WalkOptions {
            entry_types: EntryTypes::all(),
            respect_gitignore: self.ignore_config.auto_gitignore(),
            ..WalkOptions::default()
        };

        let fs = BackendWalkerFs(self.backend.as_ref());
        let mut walker = FileWalker::new(&fs, &root)
            .with_pattern(glob)
            .with_options(options);

        // Note: if ignore_files contains ".gitignore" AND auto_gitignore is true,
        // the root .gitignore is loaded twice (once here, once by the walker).
        // This is harmless — merge is additive and rules are idempotent.
        if let Some(filter) = self.ignore_config.build_filter(&root, &fs).await {
            walker = walker.with_ignore(filter);
        }

        walker.collect().await.map_err(|e| e.to_string())
    }

    /// Expand positional arguments, resolving glob patterns to relative paths.
    ///
    /// Used by file-processing builtins (cat, head, tail, wc) that accept
    /// glob patterns in their path arguments. Non-string values are converted
    /// to strings (matching shell conventions).
    ///
    /// A `Value::Bytes` operand goes LOUD (GH #93 item 1), and `Value::Json`
    /// (list/record), `Value::Bool`, and `Value::Null` operands go LOUD too
    /// (GH #121) — none is silently dropped by a catch-all anymore. Every
    /// caller here falls back to reading stdin (or a generic "missing path"
    /// error) when the path list comes back empty, so a structured, bool, or
    /// null path used to vanish into a wrong data source instead of erroring.
    /// The match is exhaustive over all 7 `Value` variants on purpose: a
    /// future new variant fails to compile here until handled, rather than
    /// silently falling through a wildcard arm.
    pub async fn expand_paths(&self, positional: &[Value]) -> Result<Vec<String>, String> {
        let mut paths = Vec::new();
        for arg in positional {
            let s = match arg {
                Value::String(s) => s.clone(),
                Value::Int(n) => n.to_string(),
                Value::Float(f) => f.to_string(),
                Value::Bytes(_) => {
                    crate::interpreter::value_to_text_sink_named(arg, "a path").map_err(|e| e.to_string())?
                }
                Value::Json(_) => {
                    return Err(crate::interpreter::structured_boundary_error("a path", arg)
                        .unwrap_or_else(|| "cannot use this value as a path".to_string()));
                }
                Value::Bool(b) => return Err(format!("cannot use a bool ({b}) as a path")),
                Value::Null => return Err("cannot use null as a path".to_string()),
            };
            if crate::glob::contains_glob(&s) {
                let expanded = self.expand_glob(&s).await?;
                // An absolute pattern reports absolute paths. Stripping the
                // cwd unconditionally is a no-op only while cwd is a real
                // prefix; when cwd is `/` the strip removes the leading
                // separator itself and turns `/tmp/a.txt` into `tmp/a.txt`.
                // `/` is the default cwd for an isolated kernel, so that is
                // the common case for an embedder, not a corner. Same guard
                // the kernel's own argv expansion uses.
                let absolute = s.starts_with('/');
                let root = self.resolve_path(".");
                for p in expanded {
                    if absolute {
                        paths.push(p.to_string_lossy().to_string());
                    } else {
                        let rel = p.strip_prefix(&root).unwrap_or(&p);
                        paths.push(rel.to_string_lossy().to_string());
                    }
                }
            } else {
                paths.push(s);
            }
        }
        Ok(paths)
    }

    /// Default chunk size for forward file scans. Bounds the memory a
    /// scan-oriented builtin holds at once, independent of file size.
    pub const STREAM_CHUNK_SIZE: u64 = 256 * 1024;

    /// Stream a file's bytes forward in `chunk_size` slices, handing each
    /// non-empty chunk to `f`.
    ///
    /// Reads are issued as positional `read_range` requests, so backends slice
    /// without materialising the whole file (LocalFs seeks; MemoryFs/OverlayFs
    /// slice their stored bytes). The loop terminates on the first empty chunk,
    /// which every backend returns once the offset reaches EOF. `f` returns a
    /// [`ControlFlow`](std::ops::ControlFlow): `Break` stops the loop early
    /// (e.g. a consumer that has detected binary content and will discard the
    /// rest), so we don't keep reading a file the caller is done with. This is
    /// the shared engine for scan-oriented builtins (`wc`, `checksum`, `grep`)
    /// that walk a file front-to-back and must not hold it all in memory.
    pub async fn read_file_chunked<F>(
        &self,
        path: &std::path::Path,
        chunk_size: u64,
        mut f: F,
    ) -> kaish_types::backend::BackendResult<()>
    where
        F: FnMut(&[u8]) -> std::ops::ControlFlow<()>,
    {
        use kaish_types::ReadRange;
        let mut offset = 0u64;
        loop {
            let chunk = self
                .backend
                .read(path, Some(ReadRange::bytes(offset, chunk_size)))
                .await?;
            if chunk.is_empty() {
                break;
            }
            offset += chunk.len() as u64;
            if f(&chunk).is_break() {
                break;
            }
        }
        Ok(())
    }
}

/// The kernel's full execution context satisfies the trimmed portable
/// [`ToolCtx`](kaish_tool_api::ToolCtx) contract that out-of-tree tools see.
///
/// Trusted in-tree builtins recover the concrete `ExecContext` (job control,
/// pipes, dispatcher) through
/// [`ToolCtx::as_any_mut`](kaish_tool_api::ToolCtx::as_any_mut).
#[async_trait]
impl kaish_tool_api::ToolCtx for ExecContext {
    fn backend(&self) -> &Arc<dyn KernelBackend> {
        &self.backend
    }

    fn cwd(&self) -> &std::path::Path {
        self.cwd.as_path()
    }

    fn resolve_path(&self, path: &str) -> PathBuf {
        // Inherent methods shadow trait methods in call syntax, so the
        // fully-qualified inherent call here is not recursive.
        ExecContext::resolve_path(self, path)
    }

    fn var(&self, name: &str) -> Option<Value> {
        self.scope.get(name).cloned()
    }

    fn set_var(&mut self, name: &str, value: Value) {
        self.scope.set(name, value);
    }

    fn set_output_format(&mut self, format: OutputFormat) {
        self.output_format = Some(format);
    }

    fn patient(&self, budget: std::time::Duration) -> kaish_tool_api::PatientGuard {
        match &self.watchdog {
            Some(watchdog) => kaish_tool_api::PatientGuard::held(Box::new(watchdog.hold(budget))),
            None => kaish_tool_api::PatientGuard::inert(),
        }
    }

    fn as_any(&self) -> &dyn std::any::Any {
        self
    }

    fn as_any_mut(&mut self) -> &mut dyn std::any::Any {
        self
    }
}

/// Decode one line of stdin as UTF-8, refusing binary rather than mangling it.
///
/// Same rule and same wording as [`ExecContext::read_stdin_to_text`] — a
/// line-at-a-time reader must not be the one place where `U+FFFD` creeps in.
fn decode_stdin_line(bytes: Vec<u8>) -> Result<String, String> {
    String::from_utf8(bytes).map_err(|_| {
        "input is not valid UTF-8 (binary data?) — pipe through base64/xxd \
         or use a binary-aware tool (cat, dd, cmp, wc -c)"
            .to_string()
    })
}

/// Normalize a path by resolving `.` and `..` components lexically (no filesystem access).
fn normalize_path(path: &std::path::Path) -> PathBuf {
    let mut parts: Vec<Component> = Vec::new();
    for component in path.components() {
        match component {
            Component::CurDir => {} // skip `.`
            Component::ParentDir => {
                // Pop the last normal component, but don't pop past root
                if let Some(Component::Normal(_)) = parts.last() {
                    parts.pop();
                } else {
                    parts.push(component);
                }
            }
            _ => parts.push(component),
        }
    }
    if parts.is_empty() {
        PathBuf::from("/")
    } else {
        parts.iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::{decide_mutation_action, MutationAction};
    use std::path::Path;

    fn decide(
        trash: bool,
        real: Option<&str>,
        exists: bool,
        append: bool,
    ) -> MutationAction {
        // Default to a small file well under the cap; the size-cap behavior
        // has its own dedicated test below.
        decide_mutation_action(trash, real.map(Path::new), exists, append, 1, 10_000_000)
    }

    #[test]
    fn new_file_and_append_always_proceed() {
        // Non-existent target: nothing to lose.
        assert_eq!(decide(true, Some("/work/new"), false, false), MutationAction::Proceed);
        // Append to an existing file doesn't destroy prior content.
        assert_eq!(decide(true, Some("/work/log"), true, true), MutationAction::Proceed);
    }

    #[test]
    fn an_existing_file_is_snapshotted_before_it_is_overwritten() {
        assert_eq!(decide(true, Some("/work/f"), true, false), MutationAction::TrashFirst);
    }

    #[test]
    fn trash_off_proceeds() {
        assert_eq!(decide(false, Some("/work/f"), true, false), MutationAction::Proceed);
    }

    #[test]
    fn tmp_is_excluded_but_a_real_v_path_is_still_trashed() {
        // /tmp scratch proceeds even with trash on (matches rm).
        assert_eq!(decide(true, Some("/tmp/scratch"), true, false), MutationAction::Proceed);
        // A *real* path under /v is NOT excluded: mount-coverage routing
        // delegates unclaimed /v/* to the embedder's backend, so its real
        // content under /v keeps the trash safety net.
        assert_eq!(decide(true, Some("/v/cas/blob.bin"), true, false), MutationAction::TrashFirst);
    }

    #[test]
    fn file_too_big_to_trash_is_written_directly_like_rm() {
        // Prior content larger than the cap can't be snapshotted, so trash is
        // skipped and the overwrite proceeds unbacked. Nothing holds it back.
        let big = 100u64;
        let cap = 10u64;
        assert_eq!(
            decide_mutation_action(true, Some(Path::new("/work/f")), true, false, big, cap),
            MutationAction::Proceed
        );
        // Exactly at the cap still trashes (inclusive bound, matches rm).
        assert_eq!(
            decide_mutation_action(true, Some(Path::new("/work/f")), true, false, cap, cap),
            MutationAction::TrashFirst
        );
    }
}
