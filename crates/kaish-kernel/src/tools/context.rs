//! Execution context for tools.

use std::collections::HashMap;
use std::path::{Component, Path, PathBuf};
use std::sync::Arc;

use crate::ast::Value;
use crate::backend::{KernelBackend, LocalBackend};
use crate::dispatch::PipelinePosition;
use crate::ignore_config::IgnoreConfig;
use crate::interpreter::{ExecResult, Scope};
use crate::nonce::NonceStore;
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
    /// Standard input for the tool (from pipeline).
    pub stdin: Option<String>,
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
    /// Confirmation nonce store for latch-gated operations.
    ///
    /// Arc-shared across pipeline stages so nonces issued in one stage
    /// can be validated in another.
    pub nonce_store: NonceStore,
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

    /// The command currently executing, captured at the dispatch seam as
    /// `(dispatch_name, argv)` where `argv` is the canonical `ToolArgs::to_argv`.
    /// A latch producer (`latch_result`/`gate_overwrites`) stamps this into the
    /// `LatchRequest` so `Kernel::confirm` can replay the *exact* invocation with
    /// `--confirm=<nonce>` — no re-parsing of the human `hint`. `None` for a
    /// direct `tool.execute` call in a unit test (no dispatch seam ran).
    ///
    /// Boxed to keep `ExecContext` lean: it is cloned/rebuilt at every recursion
    /// level (pipeline stages, `$(...)`, functions), so an inline `(String,
    /// Vec<String>)` would grow every frame and eat into the interpreter's stack
    /// headroom (see GH #46/#47). The box is allocated once per gated command.
    pub current_invocation: Option<Box<(String, Vec<String>)>>,

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
    /// Write now — new file, append, excluded path, or both gates off.
    Proceed,
    /// Snapshot the prior content to trash, then write.
    TrashFirst,
    /// Gate behind a confirmation nonce (exit 2 until `--confirm`).
    Latch,
}

/// Prior content the gate snapshotted, keyed by resolved path, for callers that
/// compare-and-swap their overwrite against it (see `overwrite_checked`). Only
/// gated existing targets that were trash-snapshotted appear; a new file, an
/// append, an excluded/ungated path, or a latch-only target is absent — the
/// caller writes those without a CAS expectation.
pub(crate) type GateSnapshots = std::collections::HashMap<PathBuf, Vec<u8>>;

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
/// (a real path like `/v/cas/blob.bin`) must keep the trash/latch safety net; a
/// `/v` prefix exclusion here would silently strip it.
pub(crate) fn is_trash_excluded(real_path: Option<&Path>) -> bool {
    matches!(real_path, Some(rp) if rp.starts_with("/tmp"))
}

/// Decide how to gate a truncating overwrite, mirroring `rm`'s trash/latch
/// priority. Pure so the decision table is unit-testable in isolation.
///
/// - A non-existent target or an append has nothing to lose → `Proceed`.
/// - A real path under `/tmp` (host scratch) is excluded (matches `rm`) → `Proceed`.
/// - Trash wins over latch (trash IS the safety net): `TrashFirst` when trash
///   is on **and** the prior content fits under `trash_max_size` (a file too
///   big to snapshot can't be backed up, so it falls through, exactly like
///   `rm`); else `Latch` when latch is on; else `Proceed`.
///
/// An overlay/in-memory target has `real_path == None`, so it is *not* excluded
/// and stays gated — the protection is about agent-operation safety, not just
/// real-FS data (Amy, 2026-06-17).
pub(crate) fn decide_mutation_action(
    trash_enabled: bool,
    latch_enabled: bool,
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
    if latch_enabled {
        return MutationAction::Latch;
    }
    MutationAction::Proceed
}

/// Overwrite `resolved` with `content`, optionally compare-and-swapping against
/// `expected` first. When `expected` is `Some`, the current bytes are re-read
/// and must equal it (the write-model gate's snapshot), else a concurrent
/// change is a loud conflict — never a silent clobber. Binary-safe (raw bytes,
/// unlike the `String`-based `PatchOp` CAS). Shared by the byte-oriented gated
/// builtins via `ExecContext::overwrite_checked` (`tee`/`write`/`dd`) and
/// directly by `cp`'s free copy path. Not OS-atomic — a crash mid-write can
/// still truncate (the atomic write-temp-then-rename primitive is a tracked
/// write-model residual).
pub(crate) async fn cas_overwrite(
    backend: &dyn KernelBackend,
    resolved: &Path,
    content: &[u8],
    expected: Option<&[u8]>,
) -> Result<(), crate::backend::BackendError> {
    if let Some(exp) = expected {
        // Propagate a re-read failure loudly — never `unwrap_or_default()` it to
        // empty bytes, which would false-match an empty snapshot (silent
        // overwrite) or report a bogus "file changed" for a real I/O error. A
        // target that vanished since the gate (NotFound) is a change → abort.
        let current = backend.read(resolved, None).await?;
        if current != exp {
            return Err(crate::backend::BackendError::InvalidOperation(
                "file changed since the write-model gate checked it (concurrent write); \
                 aborting overwrite"
                    .to_string(),
            ));
        }
    }
    backend
        .write(resolved, content, crate::backend::WriteMode::Overwrite)
        .await
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
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            nonce_store: NonceStore::new(),
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
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
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            nonce_store: NonceStore::new(),
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
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
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            nonce_store: NonceStore::new(),
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
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
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            nonce_store: NonceStore::new(),
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
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
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            nonce_store: NonceStore::new(),
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
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
            aliases: HashMap::new(),
            ignore_config: IgnoreConfig::none(),
            output_limit: OutputLimitConfig::none(),
            allow_external_commands: true,
            nonce_store: NonceStore::new(),
            trash_backend: None,
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: None,
            dispatcher: None,
            cancel: CancellationToken::new(),
            output_format: None,
            current_invocation: None,
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
    /// An explicit stdin string (`< file`, heredoc, here-string, or a pipeline
    /// hand-off) supersedes any inherited lazy `pipe_stdin`. Since `read_stdin_*`
    /// prefers `pipe_stdin`, clear it here so redirect precedence holds — a
    /// `< file` must beat a frontend-seeded piped stdin.
    pub fn set_stdin(&mut self, stdin: String) {
        self.stdin = Some(stdin);
        self.pipe_stdin = None;
    }

    /// Get stdin, consuming it.
    pub fn take_stdin(&mut self) -> Option<String> {
        self.stdin.take()
    }

    /// Set both text stdin and structured data.
    ///
    /// Use this when passing output through a pipeline where the previous
    /// command produced structured data (e.g., JSON from MCP tools).
    pub fn set_stdin_with_data(&mut self, text: String, data: Option<Value>) {
        self.stdin = Some(text);
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
        self.prev_cwd = Some(self.cwd.clone());
        self.cwd = path;
    }

    /// Get the previous working directory (for `cd -`).
    pub fn get_prev_cwd(&self) -> Option<&PathBuf> {
        self.prev_cwd.as_ref()
    }

    /// Read all stdin (pipe or buffered string) into a String.
    ///
    /// Prefers pipe_stdin if set (streaming pipeline), otherwise falls back
    /// to the buffered stdin string. Consumes the source.
    pub async fn read_stdin_to_string(&mut self) -> Option<String> {
        if let Some(mut reader) = self.pipe_stdin.take() {
            use tokio::io::AsyncReadExt;
            let mut buf = Vec::new();
            reader.read_to_end(&mut buf).await.ok()?;
            Some(String::from_utf8_lossy(&buf).into_owned())
        } else {
            self.stdin.take()
        }
    }

    /// Read stdin as text, erroring on non-UTF-8 instead of silently
    /// lossy-decoding it (which corrupts binary with `U+FFFD`).
    ///
    /// The strict counterpart to [`Self::read_stdin_to_string`], for text-only
    /// builtins (`grep`, `sed`, `awk`, `cut`, `sort`, `jq`, …): a binary stream
    /// is a loud error, not a mangle. Returns `Ok(None)` when there is no stdin
    /// at all. The `Err` is a ready-to-use message; callers prefix their name.
    /// See `docs/binary-data.md` and `docs/issues.md`.
    pub async fn read_stdin_to_text(&mut self) -> Result<Option<String>, String> {
        match self.read_stdin_to_bytes().await {
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
    /// The byte-clean counterpart to [`Self::read_stdin_to_string`], for
    /// binary-aware builtins (`base64`, `xxd`, `checksum`, `wc -c`, `cmp`, …).
    /// Returns `None` when there is no stdin at all (no pipe and no buffer);
    /// an empty pipe yields `Some(vec![])`. A buffered text stdin is returned
    /// as its UTF-8 bytes. See `docs/binary-data.md`.
    pub async fn read_stdin_to_bytes(&mut self) -> Option<Vec<u8>> {
        if let Some(mut reader) = self.pipe_stdin.take() {
            use tokio::io::AsyncReadExt;
            let mut buf = Vec::new();
            reader.read_to_end(&mut buf).await.ok()?;
            Some(buf)
        } else {
            self.stdin.take().map(String::into_bytes)
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
            aliases: self.aliases.clone(),
            ignore_config: self.ignore_config.clone(),
            output_limit: self.output_limit.clone(),
            allow_external_commands: self.allow_external_commands,
            nonce_store: self.nonce_store.clone(),
            trash_backend: self.trash_backend.clone(),
            #[cfg(all(unix, feature = "subprocess"))]
            terminal_state: self.terminal_state.clone(),
            dispatcher: self.dispatcher.clone(),
            cancel: self.cancel.clone(),
            // Output format is per-execution; child pipeline stages start fresh.
            output_format: None,
            // Per-command; each pipeline stage stamps its own at the dispatch seam.
            current_invocation: None,
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

    /// Validate a confirmation nonce against a command and paths.
    ///
    /// Thin wrapper on `NonceStore::validate` for ergonomic use from builtins.
    pub fn verify_nonce(&self, nonce: &str, command: &str, paths: &[&str]) -> Result<(), String> {
        self.nonce_store.validate(nonce, command, paths)
    }

    /// Issue a nonce and build the standard exit-2 latch result.
    ///
    /// `reason` explains why confirmation is needed (e.g., `"latch enabled"`,
    /// `"emptying trash is destructive"`). The `confirm_hint` closure receives
    /// the nonce string so each tool can format its own re-run command.
    ///
    /// The result includes structured data in `.data` for programmatic access:
    /// ```json
    /// {"nonce": "a3f7b2c1", "command": "rm", "paths": [...], "hint": "rm --confirm=a3f7b2c1 file", "ttl": 60}
    /// ```
    pub fn latch_result(
        &self,
        command: &str,
        paths: &[&str],
        reason: &str,
        confirm_hint: impl FnOnce(&str) -> String,
    ) -> ExecResult {
        let nonce = self.nonce_store.issue(command, paths);
        let ttl = self.nonce_store.ttl().as_secs();
        let authorized = if paths.is_empty() {
            String::new()
        } else {
            format!("\nAuthorized: {}", paths.join(", "))
        };
        let hint = confirm_hint(&nonce);

        let mut result = ExecResult::failure(2, format!(
            "{command}: confirmation required ({reason}){authorized}\nTo confirm, run: {hint}\nNonce expires in {ttl} seconds."
        ));
        // Stamp the exact invocation captured at the dispatch seam so
        // `Kernel::confirm` can replay it precisely. `None` (a direct
        // `tool.execute` in a unit test, no seam) leaves tool/argv empty — such
        // a latch is confirmable only via the `hint` string, not `confirm`.
        let (tool, argv) = self.current_invocation.as_deref().cloned().unwrap_or_default();
        // The latch request rides its own typed control-plane field, distinct
        // from the data-plane `.data`. `ExecResult::latch_request` reads it back.
        result.latch = Some(Box::new(crate::interpreter::LatchRequest {
            nonce,
            command: command.to_string(),
            paths: paths.iter().map(|p| (*p).to_string()).collect(),
            hint,
            tool,
            argv,
            ttl,
        }));
        result
    }

    /// Gate a batch of truncating overwrites through latch + trash, the way
    /// `rm` gates deletes — so `tee`/`patch`/`sed -i` can't silently clobber a
    /// file with no recoverable prior copy and no confirmation.
    ///
    /// Each target is `(display_path, is_append)`. A path that doesn't exist yet
    /// or is an append has nothing to lose and passes. For an existing file
    /// under `set -o trash`, the prior content is copied to trash first (via
    /// `trash_bytes`) so it's recoverable; the file is left in place for the
    /// caller to overwrite. Under `set -o latch` (and trash off) the batch needs
    /// `--confirm=<nonce>`: the first call returns an exit-2 latch result with
    /// one nonce scoping every latched path.
    ///
    /// `Ok(snapshots)` means every snapshot is done and the caller may write
    /// all targets; `snapshots` maps each trash-snapshotted target's resolved
    /// path to its prior bytes, so a byte-oriented caller can pass them as the
    /// `expected` to `overwrite_checked` for a binary-safe compare-and-swap.
    /// `Err(result)` is what the caller must return verbatim (the latch prompt,
    /// an invalid nonce, or a trash failure — never fall through to a
    /// destructive overwrite on error).
    ///
    /// `confirm_hint` builds the re-run command shown in the latch prompt, given
    /// the nonce and the space-joined latched paths. Most callers want
    /// `|nonce, joined| format!("{command} --confirm=\"{nonce}\" {joined}")`, but
    /// a tool whose argv carries operands the operation can't run without — e.g.
    /// `sed -i`'s expression — must reinject them here, or the advertised
    /// re-run will misbehave (or hang on stdin).
    pub async fn gate_overwrites(
        &mut self,
        command: &str,
        targets: &[(String, bool)],
        confirm: Option<&str>,
        confirm_hint: impl FnOnce(&str, &str) -> String,
    ) -> Result<GateSnapshots, ExecResult> {
        let trash_enabled = self.scope.trash_enabled();
        let latch_enabled = self.scope.latch_enabled();
        // Fast path: both gates off, nothing to do.
        if !trash_enabled && !latch_enabled {
            return Ok(GateSnapshots::new());
        }
        let trash_max_size = self.scope.trash_max_size();

        struct Decided {
            display: String,
            resolved: PathBuf,
            action: MutationAction,
        }
        // Dedup by resolved path (keep first): a multi-file patch with an
        // explicit target lists the same file once per hunk-group, and we must
        // not snapshot it N times or list it N times in the latch prompt.
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
                latch_enabled,
                real.as_deref(),
                exists,
                *is_append,
                size,
                trash_max_size,
            );
            decided.push(Decided { display: display.clone(), resolved, action });
        }

        // Latch: one nonce scopes every latched path (subset confirmation).
        let latched: Vec<&str> = decided
            .iter()
            .filter(|d| matches!(d.action, MutationAction::Latch))
            .map(|d| d.display.as_str())
            .collect();
        if !latched.is_empty() {
            match confirm {
                Some(nonce) => {
                    if let Err(e) = self.verify_nonce(nonce, command, &latched) {
                        return Err(ExecResult::failure(1, format!("{command}: {e}")));
                    }
                }
                None => {
                    let joined = latched.join(" ");
                    return Err(self.latch_result(command, &latched, "latch enabled", |nonce| {
                        confirm_hint(nonce, &joined)
                    }));
                }
            }
        }

        // Snapshot prior content for every trash-first target before any write,
        // keeping the bytes so a byte-oriented caller can CAS against them.
        let mut snapshots = GateSnapshots::new();
        for d in &decided {
            if matches!(d.action, MutationAction::TrashFirst) {
                match self.snapshot_for_overwrite(&d.display, &d.resolved).await {
                    Ok(bytes) => {
                        snapshots.insert(d.resolved.clone(), bytes);
                    }
                    Err(e) => return Err(ExecResult::failure(1, format!("{command}: {e}"))),
                }
            }
        }
        Ok(snapshots)
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
        expected: Option<&[u8]>,
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
                let root = self.resolve_path(".");
                for p in expanded {
                    let rel = p.strip_prefix(&root).unwrap_or(&p);
                    paths.push(rel.to_string_lossy().to_string());
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
/// pipes, dispatcher) through [`ToolCtx::as_any_mut`].
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
        latch: bool,
        real: Option<&str>,
        exists: bool,
        append: bool,
    ) -> MutationAction {
        // Default to a small file well under the cap; the size-cap behavior
        // has its own dedicated test below.
        decide_mutation_action(trash, latch, real.map(Path::new), exists, append, 1, 10_000_000)
    }

    #[test]
    fn new_file_and_append_always_proceed() {
        // Non-existent target: nothing to lose, regardless of gates.
        assert_eq!(decide(true, true, Some("/work/new"), false, false), MutationAction::Proceed);
        // Append to an existing file doesn't destroy prior content.
        assert_eq!(decide(true, true, Some("/work/log"), true, true), MutationAction::Proceed);
    }

    #[test]
    fn trash_wins_over_latch_on_existing_file() {
        assert_eq!(decide(true, true, Some("/work/f"), true, false), MutationAction::TrashFirst);
        assert_eq!(decide(true, false, Some("/work/f"), true, false), MutationAction::TrashFirst);
    }

    #[test]
    fn latch_gates_when_trash_off() {
        assert_eq!(decide(false, true, Some("/work/f"), true, false), MutationAction::Latch);
    }

    #[test]
    fn both_gates_off_proceeds() {
        assert_eq!(decide(false, false, Some("/work/f"), true, false), MutationAction::Proceed);
    }

    #[test]
    fn tmp_bypasses_gate_but_real_v_path_stays_gated() {
        // /tmp scratch proceeds even with both gates on (matches rm).
        assert_eq!(decide(true, true, Some("/tmp/scratch"), true, false), MutationAction::Proceed);
        // A *real* path under /v is NOT excluded: mount-coverage routing now
        // delegates unclaimed /v/* to the embedder's backend, so its real
        // content under /v must keep the trash/latch safety net. Trash wins.
        assert_eq!(decide(true, true, Some("/v/cas/blob.bin"), true, false), MutationAction::TrashFirst);
    }

    #[test]
    fn overlay_no_real_path_stays_gated() {
        // No real path (overlay/in-memory) is NOT excluded — still trash-first.
        assert_eq!(decide(true, true, None, true, false), MutationAction::TrashFirst);
        assert_eq!(decide(false, true, None, true, false), MutationAction::Latch);
    }

    #[test]
    fn file_too_big_to_trash_falls_through_like_rm() {
        // Prior content larger than the cap can't be snapshotted, so trash is
        // skipped: latch gates if on, else the overwrite proceeds unbacked.
        let big = 100u64;
        let cap = 10u64;
        assert_eq!(
            decide_mutation_action(true, true, Some(Path::new("/work/f")), true, false, big, cap),
            MutationAction::Latch
        );
        assert_eq!(
            decide_mutation_action(true, false, Some(Path::new("/work/f")), true, false, big, cap),
            MutationAction::Proceed
        );
        // Exactly at the cap still trashes (inclusive bound, matches rm).
        assert_eq!(
            decide_mutation_action(true, false, Some(Path::new("/work/f")), true, false, cap, cap),
            MutationAction::TrashFirst
        );
    }
}
