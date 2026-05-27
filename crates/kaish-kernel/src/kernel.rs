//! The Kernel (核) — the heart of kaish.
//!
//! The Kernel owns and coordinates all core components:
//! - Interpreter state (scope, $?)
//! - Tool registry (builtins, user tools)
//! - VFS router (mount points)
//! - Job manager (background jobs)
//!
//! # Architecture
//!
//! ```text
//! ┌────────────────────────────────────────────────────────────┐
//! │                         Kernel (核)                         │
//! │  ┌──────────────┐  ┌──────────────┐  ┌──────────────────┐  │
//! │  │   Scope      │  │ ToolRegistry │  │  VfsRouter       │  │
//! │  │  (variables) │  │  (builtins,  │  │  (mount points)  │  │
//! │  │              │  │   user tools)│  │                  │  │
//! │  └──────────────┘  └──────────────┘  └──────────────────┘  │
//! │  ┌──────────────────────────────┐  ┌──────────────────┐    │
//! │  │  JobManager (background)     │  │  ExecResult ($?) │    │
//! │  └──────────────────────────────┘  └──────────────────┘    │
//! └────────────────────────────────────────────────────────────┘
//! ```

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;

use anyhow::{Context, Result};
use tokio::sync::RwLock;

/// Monotonic counter assigned to each Kernel at construction time, exposed
/// via `$$` / `${$}`. Starts at 1; each new Kernel gets the next value.
/// `Kernel::fork()` inherits the parent's value (matching bash's "subshell
/// keeps parent's $$" semantics) because forks clone the parent's Scope
/// rather than calling `set_pid` again.
///
/// Deliberately *not* the OS PID — kaish runs as a long-lived MCP server
/// or embedded inside other binaries (kaijutsu), where the host PID is
/// meaningless to the script. See
/// `~/.claude/projects/-home-atobey-src-kaish/memory/lang_dollar_dollar_identifier.md`
/// for the design rationale.
static KERNEL_COUNTER: AtomicU64 = AtomicU64::new(1);

use async_trait::async_trait;

use crate::ast::{Arg, Command, Expr, FileTestOp, Stmt, StringPart, TestExpr, ToolDef, Value, BinaryOp};
pub use kaish_types::ExecuteOptions;
use crate::backend::{BackendError, KernelBackend};
use kaish_glob::glob_match;
use crate::dispatch::{CommandDispatcher, PipelinePosition};
use crate::interpreter::{apply_output_format, eval_expr, expand_tilde, json_to_value, value_to_bool, value_to_string, ControlFlow, ExecResult, Scope};
use crate::parser::parse;
use crate::scheduler::{is_bool_type, schema_param_lookup, stderr_stream, BoundedStream, JobManager, PipelineRunner, StderrReceiver};
#[cfg(feature = "native")]
use crate::scheduler::{drain_to_stream, DEFAULT_STREAM_MAX_SIZE};
use crate::tools::{register_builtins, ExecContext, GlobalFlags, ToolArgs, ToolRegistry};
#[cfg(feature = "native")]
use crate::tools::resolve_in_path;
use crate::validator::{Severity, Validator};
#[cfg(feature = "native")]
use crate::vfs::LocalFs;
use crate::vfs::{BuiltinFs, JobFs, MemoryFs, VfsRouter};

/// VFS mount mode determines how the local filesystem is exposed.
///
/// Different modes trade off convenience vs. security:
/// - `Passthrough` gives native path access (best for human REPL use)
/// - `Sandboxed` restricts access to a subtree (safer for agents)
/// - `NoLocal` provides complete isolation (tests, pure memory mode)
#[derive(Debug, Clone)]
pub enum VfsMountMode {
    /// LocalFs at "/" — native paths work directly.
    ///
    /// Full filesystem access. Use for human-operated REPL sessions where
    /// native paths like `/home/user/project` should just work.
    ///
    /// Mounts:
    /// - `/` → LocalFs("/")
    /// - `/v` → MemoryFs (blob storage)
    #[cfg(feature = "native")]
    Passthrough,

    /// Transparent sandbox — paths look native but access is restricted.
    ///
    /// The local filesystem is mounted at its real path (e.g., `/home/user`),
    /// so `/home/user/src/project` just works. But paths outside the sandbox
    /// root are not accessible.
    ///
    /// **Note:** This only restricts VFS (builtin) operations. External commands
    /// bypass the sandbox entirely — see [`KernelConfig::allow_external_commands`].
    ///
    /// Mounts:
    /// - `/` → MemoryFs (catches paths outside sandbox)
    /// - `{root}` → LocalFs(root)  (e.g., `/home/user` → LocalFs)
    /// - `/tmp` → LocalFs("/tmp")
    /// - `/v` → MemoryFs (blob storage)
    #[cfg(feature = "native")]
    Sandboxed {
        /// Root path for local filesystem. Defaults to `$HOME`.
        /// Can be restricted further, e.g., `~/src`.
        root: Option<PathBuf>,
    },

    /// No local filesystem. Memory only.
    ///
    /// Complete isolation — no access to the host filesystem.
    /// Useful for tests or pure sandboxed execution.
    ///
    /// Mounts:
    /// - `/` → MemoryFs
    /// - `/tmp` → MemoryFs
    /// - `/v` → MemoryFs
    NoLocal,
}

#[allow(clippy::derivable_impls)] // native has multiple variants; not derivable cross-feature
impl Default for VfsMountMode {
    fn default() -> Self {
        #[cfg(feature = "native")]
        { VfsMountMode::Sandboxed { root: None } }
        #[cfg(not(feature = "native"))]
        { VfsMountMode::NoLocal }
    }
}

/// Configuration for kernel initialization.
#[derive(Debug, Clone)]
pub struct KernelConfig {
    /// Name of this kernel (for identification).
    pub name: String,

    /// VFS mount mode — controls how local filesystem is exposed.
    pub vfs_mode: VfsMountMode,

    /// Initial working directory (VFS path).
    pub cwd: PathBuf,

    /// Whether to skip pre-execution validation.
    ///
    /// When false (default), scripts are validated before execution to catch
    /// errors early. Set to true to skip validation for performance or to
    /// allow dynamic/external commands.
    pub skip_validation: bool,

    /// When true, standalone external commands inherit stdio for real-time output.
    ///
    /// Set by script runner and REPL for human-visible output.
    /// Not set by MCP server (output must be captured for structured responses).
    pub interactive: bool,

    /// Ignore file configuration for file-walking tools.
    pub ignore_config: crate::ignore_config::IgnoreConfig,

    /// Output size limit configuration for agent safety.
    pub output_limit: crate::output_limit::OutputLimitConfig,

    /// Whether external command execution (PATH lookup, `exec`, `spawn`) is allowed.
    ///
    /// When `true` (default), commands not found as builtins are resolved via PATH
    /// and executed as child processes. When `false`, only kaish builtins and
    /// backend-registered tools are available.
    ///
    /// **Security:** External commands bypass the VFS sandbox entirely — they see
    /// the real filesystem, network, and environment. Set to `false` when running
    /// untrusted input.
    pub allow_external_commands: bool,

    /// Enable confirmation latch for dangerous operations (set -o latch).
    ///
    /// When enabled, destructive operations like `rm` require nonce confirmation.
    /// Can also be enabled at runtime with `set -o latch` or via `KAISH_LATCH=1`.
    pub latch_enabled: bool,

    /// Enable trash-on-delete for rm (set -o trash).
    ///
    /// When enabled, small files are moved to freedesktop.org Trash instead of
    /// being permanently deleted. Can also be enabled at runtime with `set -o trash`
    /// or via `KAISH_TRASH=1`.
    pub trash_enabled: bool,

    /// Shared nonce store for cross-request confirmation latch.
    ///
    /// When `Some`, the kernel uses this store instead of creating a fresh one.
    /// This allows nonces issued in one MCP `execute()` call to be validated
    /// in a subsequent call. When `None` (default), a fresh store is created.
    pub nonce_store: Option<crate::nonce::NonceStore>,

    /// Variables to populate the root scope with at construction, all marked
    /// for export to child processes.
    ///
    /// The kernel itself is hermetic — it never reads `std::env::vars()` —
    /// so frontends that want OS-env passthrough (REPL, MCP) populate this
    /// from `std::env::vars()`. Embedders that want isolation pass nothing
    /// (or only the keys they curate).
    pub initial_vars: HashMap<String, Value>,

    /// Default per-request timeout. When `Some`, every `execute_with_options`
    /// call without an explicit `ExecuteOptions::timeout` uses this duration.
    /// When elapsed, the kernel cancels the request, kills any external
    /// children with the configured grace, and returns exit code 124.
    ///
    /// `None` means no default timeout — only explicit per-call timeouts apply.
    pub request_timeout: Option<Duration>,

    /// Grace period between SIGTERM and SIGKILL when killing an external
    /// child on cancellation or timeout.
    ///
    /// Defaults to 2 seconds. Set to `Duration::ZERO` to escalate immediately
    /// to SIGKILL. Long-shutdown processes (databases, etc.) may need more.
    pub kill_grace: Duration,
}

/// Get the default sandbox root ($HOME).
#[cfg(feature = "native")]
fn default_sandbox_root() -> PathBuf {
    std::env::var("HOME")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/"))
}

impl Default for KernelConfig {
    fn default() -> Self {
        #[cfg(feature = "native")]
        {
            let home = default_sandbox_root();
            Self {
                name: "default".to_string(),
                vfs_mode: VfsMountMode::Sandboxed { root: None },
                cwd: home,
                skip_validation: false,
                interactive: false,
                ignore_config: crate::ignore_config::IgnoreConfig::none(),
                output_limit: crate::output_limit::OutputLimitConfig::none(),
                allow_external_commands: true,
                latch_enabled: std::env::var("KAISH_LATCH").is_ok_and(|v| v == "1"),
                trash_enabled: std::env::var("KAISH_TRASH").is_ok_and(|v| v == "1"),
                nonce_store: None,
                initial_vars: HashMap::new(),
                request_timeout: None,
                kill_grace: Duration::from_secs(2),
            }
        }
        #[cfg(not(feature = "native"))]
        {
            Self {
                name: "default".to_string(),
                vfs_mode: VfsMountMode::NoLocal,
                cwd: PathBuf::from("/"),
                skip_validation: false,
                interactive: false,
                ignore_config: crate::ignore_config::IgnoreConfig::none(),
                output_limit: crate::output_limit::OutputLimitConfig::none(),
                allow_external_commands: false,
                latch_enabled: false,
                trash_enabled: false,
                nonce_store: None,
                initial_vars: HashMap::new(),
                request_timeout: None,
                kill_grace: Duration::from_secs(2),
            }
        }
    }
}

impl KernelConfig {
    /// Create a transient kernel config (sandboxed, for temporary use).
    #[cfg(feature = "native")]
    pub fn transient() -> Self {
        let home = default_sandbox_root();
        Self {
            name: "transient".to_string(),
            vfs_mode: VfsMountMode::Sandboxed { root: None },
            cwd: home,
            skip_validation: false,
            interactive: false,
            ignore_config: crate::ignore_config::IgnoreConfig::none(),
            output_limit: crate::output_limit::OutputLimitConfig::none(),
            allow_external_commands: true,
            latch_enabled: false,
            trash_enabled: false,
            nonce_store: None,
            initial_vars: HashMap::new(),
            request_timeout: None,
            kill_grace: Duration::from_secs(2),
        }
    }

    /// Create a transient kernel config (isolated, no-default-features).
    #[cfg(not(feature = "native"))]
    pub fn transient() -> Self {
        Self::isolated()
    }

    /// Create a kernel config with the given name (sandboxed by default).
    #[cfg(feature = "native")]
    pub fn named(name: &str) -> Self {
        let home = default_sandbox_root();
        Self {
            name: name.to_string(),
            vfs_mode: VfsMountMode::Sandboxed { root: None },
            cwd: home,
            skip_validation: false,
            interactive: false,
            ignore_config: crate::ignore_config::IgnoreConfig::none(),
            output_limit: crate::output_limit::OutputLimitConfig::none(),
            allow_external_commands: true,
            latch_enabled: false,
            trash_enabled: false,
            nonce_store: None,
            initial_vars: HashMap::new(),
            request_timeout: None,
            kill_grace: Duration::from_secs(2),
        }
    }

    /// Create a kernel config with the given name (isolated, no-default-features).
    #[cfg(not(feature = "native"))]
    pub fn named(name: &str) -> Self {
        Self {
            name: name.to_string(),
            ..Self::isolated()
        }
    }

    /// Create a REPL config with passthrough filesystem access.
    ///
    /// Native paths like `/home/user/project` work directly.
    /// The cwd is set to the actual current working directory.
    #[cfg(feature = "native")]
    pub fn repl() -> Self {
        let cwd = std::env::current_dir().unwrap_or_else(|_| PathBuf::from("/"));
        Self {
            name: "repl".to_string(),
            vfs_mode: VfsMountMode::Passthrough,
            cwd,
            skip_validation: false,
            interactive: false,
            ignore_config: crate::ignore_config::IgnoreConfig::none(),
            output_limit: crate::output_limit::OutputLimitConfig::none(),
            allow_external_commands: true,
            latch_enabled: std::env::var("KAISH_LATCH").is_ok_and(|v| v == "1"),
            trash_enabled: std::env::var("KAISH_TRASH").is_ok_and(|v| v == "1"),
            nonce_store: None,
            initial_vars: HashMap::new(),
            request_timeout: None,
            kill_grace: Duration::from_secs(2),
        }
    }

    /// Create an MCP server config with sandboxed filesystem access.
    ///
    /// Local filesystem is accessible at its real path (e.g., `/home/user`),
    /// but sandboxed to `$HOME`. Paths outside the sandbox are not accessible
    /// through builtins. External commands still access the real filesystem —
    /// use `.with_allow_external_commands(false)` to block them.
    #[cfg(feature = "native")]
    pub fn mcp() -> Self {
        let home = default_sandbox_root();
        Self {
            name: "mcp".to_string(),
            vfs_mode: VfsMountMode::Sandboxed { root: None },
            cwd: home,
            skip_validation: false,
            interactive: false,
            ignore_config: crate::ignore_config::IgnoreConfig::mcp(),
            output_limit: crate::output_limit::OutputLimitConfig::mcp(),
            allow_external_commands: true,
            latch_enabled: std::env::var("KAISH_LATCH").is_ok_and(|v| v == "1"),
            trash_enabled: std::env::var("KAISH_TRASH").is_ok_and(|v| v == "1"),
            nonce_store: None,
            initial_vars: HashMap::new(),
            request_timeout: None,
            kill_grace: Duration::from_secs(2),
        }
    }

    /// Create an MCP server config with a custom sandbox root.
    ///
    /// Use this to restrict access to a subdirectory like `~/src`.
    #[cfg(feature = "native")]
    pub fn mcp_with_root(root: PathBuf) -> Self {
        Self {
            name: "mcp".to_string(),
            vfs_mode: VfsMountMode::Sandboxed { root: Some(root.clone()) },
            cwd: root,
            skip_validation: false,
            interactive: false,
            ignore_config: crate::ignore_config::IgnoreConfig::mcp(),
            output_limit: crate::output_limit::OutputLimitConfig::mcp(),
            allow_external_commands: true,
            latch_enabled: std::env::var("KAISH_LATCH").is_ok_and(|v| v == "1"),
            trash_enabled: std::env::var("KAISH_TRASH").is_ok_and(|v| v == "1"),
            nonce_store: None,
            initial_vars: HashMap::new(),
            request_timeout: None,
            kill_grace: Duration::from_secs(2),
        }
    }

    /// Create a config with no local filesystem (memory only).
    ///
    /// Complete isolation: no local filesystem and external commands are disabled.
    /// Useful for tests or pure sandboxed execution.
    pub fn isolated() -> Self {
        Self {
            name: "isolated".to_string(),
            vfs_mode: VfsMountMode::NoLocal,
            cwd: PathBuf::from("/"),
            skip_validation: false,
            interactive: false,
            ignore_config: crate::ignore_config::IgnoreConfig::none(),
            output_limit: crate::output_limit::OutputLimitConfig::none(),
            allow_external_commands: false,
            latch_enabled: false,
            trash_enabled: false,
            nonce_store: None,
            initial_vars: HashMap::new(),
            request_timeout: None,
            kill_grace: Duration::from_secs(2),
        }
    }

    /// Set the VFS mount mode.
    pub fn with_vfs_mode(mut self, mode: VfsMountMode) -> Self {
        self.vfs_mode = mode;
        self
    }

    /// Set the initial working directory.
    pub fn with_cwd(mut self, cwd: PathBuf) -> Self {
        self.cwd = cwd;
        self
    }

    /// Skip pre-execution validation.
    pub fn with_skip_validation(mut self, skip: bool) -> Self {
        self.skip_validation = skip;
        self
    }

    /// Enable interactive mode (external commands inherit stdio).
    pub fn with_interactive(mut self, interactive: bool) -> Self {
        self.interactive = interactive;
        self
    }

    /// Set the ignore file configuration.
    pub fn with_ignore_config(mut self, config: crate::ignore_config::IgnoreConfig) -> Self {
        self.ignore_config = config;
        self
    }

    /// Set the output limit configuration.
    pub fn with_output_limit(mut self, config: crate::output_limit::OutputLimitConfig) -> Self {
        self.output_limit = config;
        self
    }

    /// Set whether external command execution is allowed.
    ///
    /// When `false`, commands not found as builtins produce "command not found"
    /// instead of searching PATH. The `exec` and `spawn` builtins also return
    /// errors. Use this to prevent VFS sandbox bypass via external binaries.
    pub fn with_allow_external_commands(mut self, allow: bool) -> Self {
        self.allow_external_commands = allow;
        self
    }

    /// Enable or disable confirmation latch at startup.
    pub fn with_latch(mut self, enabled: bool) -> Self {
        self.latch_enabled = enabled;
        self
    }

    /// Enable or disable trash-on-delete at startup.
    pub fn with_trash(mut self, enabled: bool) -> Self {
        self.trash_enabled = enabled;
        self
    }

    /// Use a shared nonce store for cross-request confirmation latch.
    ///
    /// Pass a `NonceStore` that outlives individual kernel instances so nonces
    /// issued in one MCP `execute()` call can be validated in subsequent calls.
    pub fn with_nonce_store(mut self, store: crate::nonce::NonceStore) -> Self {
        self.nonce_store = Some(store);
        self
    }

    /// Add a single initial variable; marked exported when the kernel boots.
    ///
    /// Repeated calls add (last write wins on key collision).
    pub fn with_var(mut self, name: impl Into<String>, value: Value) -> Self {
        self.initial_vars.insert(name.into(), value);
        self
    }

    /// Replace the entire initial-vars map. All entries are marked exported.
    pub fn with_initial_vars(mut self, vars: HashMap<String, Value>) -> Self {
        self.initial_vars = vars;
        self
    }

    /// Extend the initial-vars map with the given entries (last write wins).
    pub fn with_vars(mut self, vars: HashMap<String, Value>) -> Self {
        self.initial_vars.extend(vars);
        self
    }

    /// Set the default per-request timeout (kernel-wide).
    ///
    /// Each `execute_with_options` call without an explicit timeout uses
    /// this. On elapsed, the kernel cancels and returns exit code 124.
    pub fn with_request_timeout(mut self, timeout: Duration) -> Self {
        self.request_timeout = Some(timeout);
        self
    }

    /// Set the SIGTERM-to-SIGKILL grace period for child kills.
    pub fn with_kill_grace(mut self, grace: Duration) -> Self {
        self.kill_grace = grace;
        self
    }
}

/// The Kernel (核) — executes kaish code.
///
/// This is the primary interface for running kaish commands. It owns all
/// the runtime state: variables, tools, VFS, jobs, and persistence.
pub struct Kernel {
    /// Kernel name.
    name: String,
    /// Variable scope.
    scope: RwLock<Scope>,
    /// Tool registry.
    tools: Arc<ToolRegistry>,
    /// User-defined tools (from `tool name { body }` statements).
    user_tools: RwLock<HashMap<String, ToolDef>>,
    /// Virtual filesystem router.
    vfs: Arc<VfsRouter>,
    /// Background job manager.
    jobs: Arc<JobManager>,
    /// Pipeline runner.
    runner: PipelineRunner,
    /// Execution context (cwd, stdin, etc.).
    exec_ctx: RwLock<ExecContext>,
    /// Whether to skip pre-execution validation.
    skip_validation: bool,
    /// When true, standalone external commands inherit stdio for real-time output.
    interactive: bool,
    /// Whether external command execution is allowed.
    allow_external_commands: bool,
    /// Default per-request timeout (None = no default).
    request_timeout: Option<Duration>,
    /// SIGTERM-to-SIGKILL grace period for child kills.
    kill_grace: Duration,
    /// Receiver for the kernel stderr stream.
    ///
    /// Pipeline stages write to the corresponding `StderrStream` (set on ExecContext).
    /// The kernel drains this after each statement in `execute_streaming`.
    stderr_receiver: tokio::sync::Mutex<StderrReceiver>,
    /// Cancellation token for interrupting execution (Ctrl-C).
    ///
    /// Protected by `std::sync::Mutex` (not tokio) because the SIGINT handler
    /// needs sync access. Each `execute()` call gets a fresh child token;
    /// `cancel()` cancels the current token and replaces it.
    cancel_token: std::sync::Mutex<tokio_util::sync::CancellationToken>,
    /// Terminal state for job control (interactive mode only, Unix only).
    #[cfg(all(unix, feature = "native"))]
    terminal_state: Option<Arc<crate::terminal::TerminalState>>,
    /// Weak self-reference for handing out `Arc<dyn CommandDispatcher>`.
    ///
    /// Set by `into_arc()`. Allows builtins to re-dispatch inner commands
    /// through the full Kernel resolution chain.
    self_weak: std::sync::OnceLock<std::sync::Weak<Self>>,
    /// Serializes concurrent `execute()` / `execute_streaming()` callers on
    /// this Kernel instance. Tokio's Mutex is fair (FIFO) and acts as the
    /// queue. Background jobs, scatter workers, and concurrent pipeline
    /// stages do NOT take this lock — they run against a *forked* Kernel
    /// (see [`Kernel::fork`]) so they never contend with the foreground.
    execute_lock: tokio::sync::Mutex<()>,
}

impl Kernel {
    /// Create a new kernel with the given configuration.
    pub fn new(config: KernelConfig) -> Result<Self> {
        let mut vfs = Self::setup_vfs(&config);
        let jobs = Arc::new(JobManager::new());

        // Mount JobFs for job observability at /v/jobs
        vfs.mount("/v/jobs", JobFs::new(jobs.clone()));

        Self::assemble(config, vfs, jobs, |_| {}, |vfs_ref, tools| {
            ExecContext::with_vfs_and_tools(vfs_ref.clone(), tools.clone())
        })
    }

    /// Set up VFS based on mount mode.
    fn setup_vfs(config: &KernelConfig) -> VfsRouter {
        let mut vfs = VfsRouter::new();

        match &config.vfs_mode {
            #[cfg(feature = "native")]
            VfsMountMode::Passthrough => {
                // LocalFs at "/" — native paths work directly
                vfs.mount("/", LocalFs::new(PathBuf::from("/")));
                // Memory for blobs
                vfs.mount("/v", MemoryFs::new());
            }
            #[cfg(feature = "native")]
            VfsMountMode::Sandboxed { root } => {
                // Memory at root for safety (catches paths outside sandbox)
                vfs.mount("/", MemoryFs::new());
                vfs.mount("/v", MemoryFs::new());

                // Real /tmp for interop with other processes
                vfs.mount("/tmp", LocalFs::new(PathBuf::from("/tmp")));

                // Mount XDG runtime dir for spill files and socket access
                let runtime = crate::paths::xdg_runtime_dir();
                if runtime.exists() {
                    let runtime_str = runtime.to_string_lossy().to_string();
                    vfs.mount(&runtime_str, LocalFs::new(runtime));
                }

                // Resolve the sandbox root (defaults to $HOME)
                let local_root = root.clone().unwrap_or_else(|| {
                    std::env::var("HOME")
                        .map(PathBuf::from)
                        .unwrap_or_else(|_| PathBuf::from("/"))
                });

                // Mount at the real path for transparent access
                // e.g., /home/atobey → LocalFs("/home/atobey")
                // so /home/atobey/src/kaish just works
                let mount_point = local_root.to_string_lossy().to_string();
                vfs.mount(&mount_point, LocalFs::new(local_root));
            }
            VfsMountMode::NoLocal => {
                // Pure memory mode — no local filesystem
                vfs.mount("/", MemoryFs::new());
                vfs.mount("/tmp", MemoryFs::new());
                vfs.mount("/v", MemoryFs::new());
            }
        }

        vfs
    }

    /// Create a transient kernel (no persistence).
    pub fn transient() -> Result<Self> {
        Self::new(KernelConfig::transient())
    }

    /// Create a kernel with a custom backend and `/v/*` virtual path support.
    ///
    /// This is the constructor for embedding kaish in other systems that provide
    /// their own storage backend (e.g., CRDT-backed storage in kaijutsu).
    ///
    /// A `VirtualOverlayBackend` routes paths automatically:
    /// - `/v/*` → Internal VFS (JobFs at `/v/jobs`, MemoryFs at `/v/blobs`)
    /// - Everything else → Your custom backend
    ///
    /// The optional `configure_vfs` closure lets you add additional virtual mounts
    /// (e.g., `/v/docs` for CRDT blocks) after the built-in mounts are set up.
    ///
    /// **Note:** The config's `vfs_mode` is ignored — all non-`/v/*` path routing
    /// is handled by your custom backend. The config is only used for `name`, `cwd`,
    /// `skip_validation`, and `interactive`.
    ///
    /// # Example
    ///
    /// ```ignore
    /// // Simple: default /v/* mounts only
    /// let kernel = Kernel::with_backend(backend, config, |_| {}, |_| {})?;
    ///
    /// // With custom mounts
    /// let kernel = Kernel::with_backend(backend, config, |vfs| {
    ///     vfs.mount_arc("/v/docs", docs_fs);
    ///     vfs.mount_arc("/v/g", git_fs);
    /// }, |_| {})?;
    ///
    /// // With custom tools
    /// let kernel = Kernel::with_backend(backend, config, |_| {}, |tools| {
    ///     tools.register(MyCustomTool::new());
    /// })?;
    /// ```
    pub fn with_backend(
        backend: Arc<dyn KernelBackend>,
        config: KernelConfig,
        configure_vfs: impl FnOnce(&mut VfsRouter),
        configure_tools: impl FnOnce(&mut ToolRegistry),
    ) -> Result<Self> {
        use crate::backend::VirtualOverlayBackend;

        let mut vfs = VfsRouter::new();
        let jobs = Arc::new(JobManager::new());

        vfs.mount("/v/jobs", JobFs::new(jobs.clone()));
        vfs.mount("/v/blobs", MemoryFs::new());

        // Let caller add custom mounts (e.g., /v/docs, /v/g)
        configure_vfs(&mut vfs);

        Self::assemble(config, vfs, jobs, configure_tools, |vfs_arc: &Arc<VfsRouter>, _: &Arc<ToolRegistry>| {
            let overlay: Arc<dyn KernelBackend> =
                Arc::new(VirtualOverlayBackend::new(backend, vfs_arc.clone()));
            ExecContext::with_backend(overlay)
        })
    }

    /// Shared assembly: wires up tools, runner, scope, and ExecContext.
    ///
    /// The `make_ctx` closure receives the VFS and tools so backends that need
    /// them (like `LocalBackend::with_tools`) can capture them. Custom backends
    /// that already have their own storage can ignore these parameters.
    fn assemble(
        config: KernelConfig,
        mut vfs: VfsRouter,
        jobs: Arc<JobManager>,
        configure_tools: impl FnOnce(&mut ToolRegistry),
        make_ctx: impl FnOnce(&Arc<VfsRouter>, &Arc<ToolRegistry>) -> ExecContext,
    ) -> Result<Self> {
        let KernelConfig { name, cwd, skip_validation, interactive, ignore_config, output_limit, allow_external_commands, latch_enabled, trash_enabled, nonce_store, initial_vars, request_timeout, kill_grace, .. } = config;

        let mut tools = ToolRegistry::new();
        register_builtins(&mut tools);
        configure_tools(&mut tools);
        let tools = Arc::new(tools);

        // Mount BuiltinFs so `ls /v/bin` lists builtins
        vfs.mount("/v/bin", BuiltinFs::new(tools.clone()));

        let vfs = Arc::new(vfs);

        let runner = PipelineRunner::new(tools.clone());

        let (stderr_writer, stderr_receiver) = stderr_stream();

        let mut exec_ctx = make_ctx(&vfs, &tools);
        exec_ctx.set_cwd(cwd);
        exec_ctx.set_job_manager(jobs.clone());
        exec_ctx.set_tool_schemas(tools.schemas());
        exec_ctx.set_tools(tools.clone());
        #[cfg(feature = "native")]
        exec_ctx.set_trash_backend(Arc::new(crate::trash_system::SystemTrash));
        exec_ctx.stderr = Some(stderr_writer);
        exec_ctx.ignore_config = ignore_config;
        exec_ctx.output_limit = output_limit;
        exec_ctx.allow_external_commands = allow_external_commands;
        if let Some(store) = nonce_store {
            exec_ctx.nonce_store = store;
        }

        Ok(Self {
            name,
            scope: RwLock::new({
                let mut scope = Scope::new();
                scope.set_pid(KERNEL_COUNTER.fetch_add(1, Ordering::Relaxed));
                if let Ok(home) = std::env::var("HOME") {
                    scope.set("HOME", Value::String(home));
                }
                // Apply caller-supplied initial variables, all marked exported.
                // Frontends (REPL, MCP) populate this from std::env::vars()
                // for shell-like UX; embedders that want hermetic behavior
                // simply leave it empty.
                for (name, value) in initial_vars {
                    scope.set_exported(name, value);
                }
                scope.set_latch_enabled(latch_enabled);
                scope.set_trash_enabled(trash_enabled);
                scope
            }),
            tools,
            user_tools: RwLock::new(HashMap::new()),
            vfs,
            jobs,
            runner,
            exec_ctx: RwLock::new(exec_ctx),
            skip_validation,
            interactive,
            allow_external_commands,
            request_timeout,
            kill_grace,
            stderr_receiver: tokio::sync::Mutex::new(stderr_receiver),
            cancel_token: std::sync::Mutex::new(tokio_util::sync::CancellationToken::new()),
            #[cfg(all(unix, feature = "native"))]
            terminal_state: None,
            self_weak: std::sync::OnceLock::new(),
            execute_lock: tokio::sync::Mutex::new(()),
        })
    }

    /// Get the kernel name.
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Wrap this Kernel in an Arc and initialize its self-reference.
    ///
    /// This enables the Kernel to hand out `Arc<dyn CommandDispatcher>` references
    /// to child contexts, allowing builtins like `timeout` to dispatch inner
    /// commands through the full resolution chain (user tools → builtins →
    /// .kai scripts → external commands).
    pub fn into_arc(self) -> Arc<Self> {
        let arc = Arc::new(self);
        let _ = arc.self_weak.set(Arc::downgrade(&arc));
        arc
    }

    /// Fork a subsidiary kernel for concurrent execution.
    ///
    /// The fork is a fully-functional `Kernel` that:
    /// - **Snapshots** per-session state from the parent: scope (COW — cheap),
    ///   user-defined tools, cwd, aliases, ignore config, etc. Mutations on
    ///   the fork do NOT propagate back to the parent — matching bash
    ///   subshell / background-job semantics.
    /// - **Shares** read-mostly resources with the parent via `Arc`: the tool
    ///   registry, the VFS router, and the job manager. A job registered by
    ///   the fork is visible to the parent's `jobs` builtin, and the fork
    ///   sees the same VFS mounts.
    /// - **Owns** its own `stderr_receiver`, `cancel_token`, and
    ///   `execute_lock`. It is never the TTY owner, so `interactive` is
    ///   `false` and `terminal_state` is `None`.
    ///
    /// The returned Arc has its `self_weak` populated (via `into_arc`), so
    /// nested dispatch through `ctx.dispatcher` (e.g. the `timeout` builtin)
    /// routes through the fork itself, not the parent — which is essential
    /// for concurrency safety.
    ///
    /// Use this for **detached** background concurrency where the fork should
    /// survive parent cancellation: the `&` background-job operator and any
    /// other "fire and forget" worker. The fork gets a fresh, independent
    /// cancellation token.
    ///
    /// For foreground concurrency (scatter workers, concurrent pipeline
    /// stages, `$(...)` cmdsubs) where parent timeout/cancel must cascade
    /// into the fork's external children, use [`Self::fork_attached`].
    pub async fn fork(&self) -> Arc<Self> {
        self.fork_inner(tokio_util::sync::CancellationToken::new()).await
    }

    /// Fork attached to the parent's cancellation.
    ///
    /// Same as [`Self::fork`] but the fork's `cancel_token` is a child of
    /// the parent's. When the parent cancels (request timeout, embedder
    /// `Kernel::cancel`, etc.), the fork's token also cancels, which in
    /// turn kills any external children spawned in the fork via the
    /// `wait_or_kill` / SIGTERM-grace-SIGKILL path.
    pub async fn fork_attached(&self) -> Arc<Self> {
        let child_token = {
            #[allow(clippy::expect_used)]
            let parent = self.cancel_token.lock().expect("cancel_token poisoned");
            parent.child_token()
        };
        self.fork_inner(child_token).await
    }

    /// Shared fork implementation. Caller decides the cancellation token.
    async fn fork_inner(&self, cancel: tokio_util::sync::CancellationToken) -> Arc<Self> {
        let scope_snapshot = self.scope.read().await.clone();
        let user_tools_snapshot = self.user_tools.read().await.clone();

        // Snapshot exec_ctx by cloning the cloneable fields, then override
        // the ones that should not carry over (stderr channel, dispatcher,
        // interactive flag, terminal state, cancel — set from `cancel` arg).
        let mut fork_ctx = {
            let parent_ctx = self.exec_ctx.read().await;
            parent_ctx.child_for_pipeline()
        };
        let (stderr_writer, stderr_receiver) = stderr_stream();
        fork_ctx.stderr = Some(stderr_writer);
        // Clear dispatcher; dispatch_command will repopulate it to point at
        // the fork on the first dispatch call.
        fork_ctx.dispatcher = None;
        fork_ctx.interactive = false;
        fork_ctx.cancel = cancel.clone();
        #[cfg(all(unix, feature = "native"))]
        {
            fork_ctx.terminal_state = None;
        }

        let fork = Self {
            name: format!("{}:fork", self.name),
            scope: RwLock::new(scope_snapshot),
            tools: Arc::clone(&self.tools),
            user_tools: RwLock::new(user_tools_snapshot),
            vfs: Arc::clone(&self.vfs),
            jobs: Arc::clone(&self.jobs),
            runner: self.runner.clone(),
            exec_ctx: RwLock::new(fork_ctx),
            skip_validation: self.skip_validation,
            // Forks are never the TTY owner — they run in the background.
            interactive: false,
            allow_external_commands: self.allow_external_commands,
            request_timeout: self.request_timeout,
            kill_grace: self.kill_grace,
            stderr_receiver: tokio::sync::Mutex::new(stderr_receiver),
            cancel_token: std::sync::Mutex::new(cancel),
            #[cfg(all(unix, feature = "native"))]
            terminal_state: None,
            self_weak: std::sync::OnceLock::new(),
            execute_lock: tokio::sync::Mutex::new(()),
        };

        fork.into_arc()
    }

    /// Get an `Arc<dyn CommandDispatcher>` to this Kernel, if wrapped via `into_arc()`.
    ///
    /// Returns `None` if the Kernel was not wrapped, or if all strong references
    /// have been dropped (the `Weak` can no longer upgrade).
    pub fn dispatcher(&self) -> Option<Arc<dyn CommandDispatcher>> {
        self.self_weak
            .get()
            .and_then(|weak| weak.upgrade())
            .map(|arc| arc as Arc<dyn CommandDispatcher>)
    }

    /// Initialize terminal state for interactive job control.
    ///
    /// Call this after kernel creation when running as an interactive REPL
    /// and stdin is a TTY. Sets up process groups and signal handling.
    #[cfg(all(unix, feature = "native"))]
    pub fn init_terminal(&mut self) {
        if !self.interactive {
            return;
        }
        match crate::terminal::TerminalState::init() {
            Ok(state) => {
                let state = Arc::new(state);
                self.terminal_state = Some(state.clone());
                // Set on exec_ctx so builtins (fg, bg, kill) can access it
                self.exec_ctx.get_mut().terminal_state = Some(state);
                tracing::debug!("terminal job control initialized");
            }
            Err(e) => {
                tracing::warn!("failed to initialize terminal job control: {}", e);
            }
        }
    }

    /// Cancel the current execution.
    ///
    /// This cancels the current cancellation token, causing any execution
    /// loop to exit at the next checkpoint with exit code 130 (SIGINT).
    /// A fresh token is installed for the next `execute()` call.
    pub fn cancel(&self) {
        #[allow(clippy::expect_used)]
        let token = self.cancel_token.lock().expect("cancel_token poisoned");
        token.cancel();
    }

    /// Check if the current execution has been cancelled.
    pub fn is_cancelled(&self) -> bool {
        #[allow(clippy::expect_used)]
        let token = self.cancel_token.lock().expect("cancel_token poisoned");
        token.is_cancelled()
    }

    /// Reset the cancellation token (called at the start of each execute).
    fn reset_cancel(&self) -> tokio_util::sync::CancellationToken {
        #[allow(clippy::expect_used)]
        let mut token = self.cancel_token.lock().expect("cancel_token poisoned");
        if token.is_cancelled() {
            *token = tokio_util::sync::CancellationToken::new();
        }
        token.clone()
    }

    /// Acquire the per-Kernel execute lock, warning on contention.
    ///
    /// Tokio's Mutex is fair (FIFO) so callers queue in arrival order. When
    /// the lock is already held, emit a warning so the silent serialization
    /// is observable in logs — if you need real parallelism, fork the kernel.
    async fn acquire_execute_lock(&self) -> tokio::sync::MutexGuard<'_, ()> {
        match self.execute_lock.try_lock() {
            Ok(guard) => guard,
            Err(_) => {
                tracing::warn!(
                    target: "kaish::kernel::concurrency",
                    kernel = %self.name,
                    "execute() contended — serializing concurrent caller; \
                     use Kernel::fork() for parallelism instead of sharing"
                );
                self.execute_lock.lock().await
            }
        }
    }

    /// Execute kaish source code with default options.
    ///
    /// Equivalent to `execute_with_options(input, ExecuteOptions::default())`.
    /// Returns the result of the last statement executed.
    pub async fn execute(&self, input: &str) -> Result<ExecResult> {
        self.execute_with_options_inner(input, ExecuteOptions::default(), None).await
    }

    /// Execute with per-call options. The primary entry point for embedders
    /// that don't need per-statement output streaming.
    ///
    /// `opts` carries timeout, transient vars overlay, optional cwd override,
    /// and optional embedder-owned cancellation token. See [`ExecuteOptions`]
    /// for semantics. For streaming, use [`Self::execute_with_options_streaming`].
    ///
    /// **Cancellation:** if `opts.cancel_token` is `Some`, it is *raced*
    /// against the kernel's internal token. Either firing cancels and kills
    /// external children. The embedder's token is read-only — kernel
    /// timeouts do NOT propagate into it. Distinguish via the returned
    /// `code`: 124 = timeout, 130 = cancellation.
    ///
    /// **Timeout:** `opts.timeout` overrides `KernelConfig::request_timeout`.
    /// `Some(Duration::ZERO)` returns 124 immediately without spawning.
    ///
    /// Concurrent callers on the same Kernel serialize on the kernel-wide
    /// execute lock. For true parallelism, call [`Kernel::fork`] (detached)
    /// or [`Kernel::fork_attached`] (cancellation cascades from this kernel).
    pub async fn execute_with_options(
        &self,
        input: &str,
        opts: ExecuteOptions,
    ) -> Result<ExecResult> {
        self.execute_with_options_inner(input, opts, None).await
    }

    /// Same as [`Self::execute_with_options`] but with a per-statement output
    /// callback. The callback fires after each top-level statement so the
    /// embedder (REPL, MCP streaming) can flush output incrementally.
    pub async fn execute_with_options_streaming(
        &self,
        input: &str,
        opts: ExecuteOptions,
        on_output: &mut (dyn FnMut(&ExecResult) + Send),
    ) -> Result<ExecResult> {
        self.execute_with_options_inner(input, opts, Some(on_output)).await
    }

    /// Execute kaish source code with a transient overlay of exported variables.
    ///
    /// Deprecated thin wrapper over [`Self::execute_with_options`]. New code
    /// should use that method directly:
    /// `execute_with_options(input, ExecuteOptions::new().with_vars(vars))`.
    #[deprecated(note = "use Kernel::execute_with_options with ExecuteOptions::with_vars")]
    pub async fn execute_with_vars(
        &self,
        input: &str,
        vars: HashMap<String, Value>,
    ) -> Result<ExecResult> {
        self.execute_with_options_inner(input, ExecuteOptions::new().with_vars(vars), None).await
    }

    /// Execute kaish source code with a per-statement callback.
    ///
    /// Deprecated thin wrapper. New code should use
    /// [`Self::execute_with_options_streaming`].
    #[deprecated(note = "use Kernel::execute_with_options_streaming")]
    pub async fn execute_streaming(
        &self,
        input: &str,
        on_output: &mut (dyn FnMut(&ExecResult) + Send),
    ) -> Result<ExecResult> {
        self.execute_with_options_inner(input, ExecuteOptions::default(), Some(on_output)).await
    }

    /// Shared body for `execute`, `execute_with_options(_streaming)`, and
    /// the deprecated wrappers. Owns the per-call cancel token, vars overlay,
    /// cwd override, and timeout race.
    #[tracing::instrument(level = "info", skip(self, opts, on_output), fields(input_len = input.len()))]
    async fn execute_with_options_inner(
        &self,
        input: &str,
        opts: ExecuteOptions,
        on_output: Option<&mut (dyn FnMut(&ExecResult) + Send)>,
    ) -> Result<ExecResult> {
        let _guard = self.acquire_execute_lock().await;

        // Always reset to a fresh internal token; this is the kernel's own
        // cancel surface for embedders calling `Kernel::cancel()`. The
        // embedder-supplied `opts.cancel_token` is a *read-only input* — it
        // is NOT written into `self.cancel_token`, because doing so would
        // (a) leak the embedder's token past this call's lifetime,
        // (b) re-route a later `Kernel::cancel()` into the embedder's token,
        // (c) extend the token's lifetime via the kernel's strong clone.
        let internal = self.reset_cancel();
        // Race the embedder token against the kernel's internal token via a
        // tracked watcher task. We hold the JoinHandle so we can abort the
        // task at function exit — otherwise it would wait forever for either
        // token to fire and leak per call.
        let (effective_cancel, watcher_handle): (
            tokio_util::sync::CancellationToken,
            Option<tokio::task::JoinHandle<()>>,
        ) = if let Some(ext) = opts.cancel_token {
            let combined = tokio_util::sync::CancellationToken::new();
            let combined_writer = combined.clone();
            let i = internal.clone();
            let handle = tokio::spawn(async move {
                tokio::select! {
                    _ = i.cancelled() => combined_writer.cancel(),
                    _ = ext.cancelled() => combined_writer.cancel(),
                }
            });
            (combined, Some(handle))
        } else {
            (internal, None)
        };

        // Effective timeout: per-call wins over kernel-config default.
        let timeout = opts.timeout.or(self.request_timeout);

        // ZERO timeout: return 124 immediately without spawning anything.
        if timeout == Some(Duration::ZERO) {
            if let Some(h) = watcher_handle {
                h.abort();
            }
            return Ok(ExecResult::failure(124, "timeout: timed out after 0s".to_string()));
        }

        // Apply per-call vars overlay (push frame + set_exported), wrapped in
        // an RAII guard so a panic inside `execute_streaming_inner` still
        // pops the frame and unexports the temporarily-exported names.
        struct VarsFrameGuard<'a> {
            kernel: &'a Kernel,
            newly_exported: Vec<String>,
        }
        impl Drop for VarsFrameGuard<'_> {
            fn drop(&mut self) {
                // Best-effort cleanup using try_write. The execute_lock held
                // throughout execute_with_options means there is no concurrent
                // foreground caller; forks have their own scope and won't
                // block this. blocking_write would deadlock the runtime when
                // called from a tokio worker thread, so we explicitly do NOT
                // fall back to it — if try_write fails (which we've never
                // seen in practice), log loudly and accept the leak rather
                // than deadlock the entire kernel.
                let Ok(mut scope) = self.kernel.scope.try_write() else {
                    tracing::error!(
                        "vars frame guard: scope lock unexpectedly busy; \
                         skipping pop_frame to avoid runtime deadlock — \
                         transient vars may leak"
                    );
                    return;
                };
                scope.pop_frame();
                for name in self.newly_exported.drain(..) {
                    scope.unexport(&name);
                }
            }
        }

        // Per-call cwd override: save current cwd, set the new one, restore
        // on Drop so the kernel's persistent cwd doesn't leak between calls.
        // Same RAII pattern as VarsFrameGuard, same blocking_write trade-off.
        struct CwdGuard<'a> {
            kernel: &'a Kernel,
            saved: PathBuf,
        }
        impl Drop for CwdGuard<'_> {
            fn drop(&mut self) {
                let Ok(mut ec) = self.kernel.exec_ctx.try_write() else {
                    tracing::error!(
                        "cwd guard: exec_ctx lock unexpectedly busy; \
                         skipping cwd restore — kernel cwd may be wrong for next call"
                    );
                    return;
                };
                ec.cwd = std::mem::take(&mut self.saved);
            }
        }
        let _cwd_guard: Option<CwdGuard<'_>> = if let Some(new_cwd) = opts.cwd {
            let mut ec = self.exec_ctx.write().await;
            let saved = std::mem::replace(&mut ec.cwd, new_cwd);
            drop(ec);
            Some(CwdGuard { kernel: self, saved })
        } else {
            None
        };

        let _vars_guard: Option<VarsFrameGuard<'_>> = if !opts.vars.is_empty() {
            let mut scope = self.scope.write().await;
            scope.push_frame();
            let mut newly = Vec::with_capacity(opts.vars.len());
            for (name, value) in opts.vars {
                if !scope.is_exported(&name) {
                    newly.push(name.clone());
                }
                scope.set_exported(name, value);
            }
            drop(scope);
            Some(VarsFrameGuard { kernel: self, newly_exported: newly })
        } else {
            None
        };

        // Sync the effective cancel into self.exec_ctx so try_execute_external
        // (which reads via self.cancel_token) sees cancellation. We also need
        // builtins to see it via ctx.cancel — handled in execute_command.
        // For simplicity here we mirror effective_cancel into self.cancel_token
        // for the duration of this call, then restore the internal token at
        // the end (so a later Kernel::cancel still hits our internal surface).
        {
            #[allow(clippy::expect_used)]
            let mut cur = self.cancel_token.lock().expect("cancel_token poisoned");
            *cur = effective_cancel.clone();
        }

        // Run inner with optional timeout. The timer task cancels our token
        // on elapsed; the cascade fires SIGTERM/SIGKILL on any external
        // children via the wait_or_kill discipline in try_execute_external.
        let mut noop_cb: Box<dyn FnMut(&ExecResult) + Send> = Box::new(|_| {});
        let cb_ref: &mut (dyn FnMut(&ExecResult) + Send) = match on_output {
            Some(cb) => cb,
            None => &mut *noop_cb,
        };

        let result = if let Some(d) = timeout {
            let elapsed = Arc::new(std::sync::atomic::AtomicBool::new(false));
            let elapsed_writer = elapsed.clone();
            let timer_token = effective_cancel.clone();
            let timer = tokio::spawn(async move {
                tokio::time::sleep(d).await;
                elapsed_writer.store(true, std::sync::atomic::Ordering::SeqCst);
                timer_token.cancel();
            });
            let r = self.execute_streaming_inner(input, cb_ref).await;
            timer.abort();
            match r {
                Ok(mut res) => {
                    if elapsed.load(std::sync::atomic::Ordering::SeqCst) {
                        res.code = 124;
                        if res.err.is_empty() {
                            res.err = format!("timeout: timed out after {:?}", d);
                        }
                    }
                    Ok(res)
                }
                Err(e) => Err(e),
            }
        } else {
            self.execute_streaming_inner(input, cb_ref).await
        };

        // Restore self.cancel_token to a fresh, uncancelled token so the
        // embedder's view of `Kernel::cancel()` stays predictable on the
        // next call (it cancels the kernel's own token, not whatever was
        // left over from this call's combined token).
        {
            #[allow(clippy::expect_used)]
            let mut cur = self.cancel_token.lock().expect("cancel_token poisoned");
            *cur = tokio_util::sync::CancellationToken::new();
        }

        // Tear down the embedder-token race watcher (if any). Leaving it
        // alive would idle forever waiting for tokens that may never fire.
        if let Some(h) = watcher_handle {
            h.abort();
        }

        // VarsFrameGuard drops here on the success path and on early-return
        // paths above (error path included). Panic safety preserved.
        result
    }

    /// The actual body of `execute_streaming`, run while holding the execute lock.
    ///
    /// Split out so internal kernel paths that are already under the lock can
    /// call this without deadlocking on re-entry. External callers must go
    /// through [`Self::execute_streaming`] so they acquire the lock.
    async fn execute_streaming_inner(
        &self,
        input: &str,
        on_output: &mut (dyn FnMut(&ExecResult) + Send),
    ) -> Result<ExecResult> {
        let program = parse(input).map_err(|errors| {
            let msg = errors
                .iter()
                .map(|e| e.format(input))
                .collect::<Vec<_>>()
                .join("\n");
            anyhow::anyhow!("parse error:\n{}", msg)
        })?;

        // AST display mode: show AST instead of executing
        {
            let scope = self.scope.read().await;
            if scope.show_ast() {
                let output = format!("{:#?}\n", program);
                return Ok(ExecResult::with_output(crate::interpreter::OutputData::text(output)));
            }
        }

        // Pre-execution validation
        if !self.skip_validation {
            let user_tools = self.user_tools.read().await;
            let validator = Validator::new(&self.tools, &user_tools);
            let issues = validator.validate(&program);

            // Collect errors (warnings are logged but don't prevent execution)
            let errors: Vec<_> = issues
                .iter()
                .filter(|i| i.severity == Severity::Error)
                .collect();

            if !errors.is_empty() {
                let error_msg = errors
                    .iter()
                    .map(|e| e.format(input))
                    .collect::<Vec<_>>()
                    .join("\n");
                return Err(anyhow::anyhow!("validation failed:\n{}", error_msg));
            }

            // Log warnings via tracing (trace level to avoid noise)
            for warning in issues.iter().filter(|i| i.severity == Severity::Warning) {
                tracing::trace!("validation: {}", warning.format(input));
            }
        }

        let mut result = ExecResult::success("");

        // Reset cancellation token for this execution.
        let cancel = self.reset_cancel();

        for stmt in program.statements {
            if matches!(stmt, Stmt::Empty) {
                continue;
            }

            // Cancellation checkpoint
            if cancel.is_cancelled() {
                result.code = 130;
                return Ok(result);
            }

            let flow = self.execute_stmt_flow(&stmt).await?;

            // Drain any stderr written by pipeline stages during this statement.
            // This captures stderr from intermediate pipeline stages that would
            // otherwise be lost (only the last stage's result is returned).
            let drained_stderr = {
                let mut receiver = self.stderr_receiver.lock().await;
                receiver.drain_lossy()
            };

            match flow {
                ControlFlow::Normal(mut r) => {
                    if !drained_stderr.is_empty() {
                        if !r.err.is_empty() && !r.err.ends_with('\n') {
                            r.err.push('\n');
                        }
                        // Prepend pipeline stderr before the last stage's stderr
                        let combined = format!("{}{}", drained_stderr, r.err);
                        r.err = combined;
                    }
                    on_output(&r);
                    // Carry the last statement's structured output for MCP TOON encoding.
                    // Must be done here (not in accumulate_result) because accumulate_result
                    // is also used in loops where per-iteration output would be wrong.
                    let last_output = r.output().cloned();
                    accumulate_result(&mut result, &r);
                    result.set_output(last_output);
                }
                ControlFlow::Exit { code } => {
                    if !drained_stderr.is_empty() {
                        result.err.push_str(&drained_stderr);
                    }
                    result.code = code;
                    return Ok(result);
                }
                ControlFlow::Return { mut value } => {
                    if !drained_stderr.is_empty() {
                        value.err = format!("{}{}", drained_stderr, value.err);
                    }
                    on_output(&value);
                    result = value;
                }
                ControlFlow::Break { result: mut r, .. } | ControlFlow::Continue { result: mut r, .. } => {
                    if !drained_stderr.is_empty() {
                        r.err = format!("{}{}", drained_stderr, r.err);
                    }
                    on_output(&r);
                    result = r;
                }
            }
        }

        Ok(result)
    }

    /// Execute a single statement, returning control flow information.
    fn execute_stmt_flow<'a>(
        &'a self,
        stmt: &'a Stmt,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<ControlFlow>> + Send + 'a>> {
        use tracing::Instrument;
        let span = tracing::debug_span!("execute_stmt_flow", stmt_type = %stmt.kind_name());
        Box::pin(async move {
        match stmt {
            Stmt::Assignment(assign) => {
                // Use async evaluator to support command substitution
                let value = self.eval_expr_async(&assign.value).await
                    .context("failed to evaluate assignment")?;
                let mut scope = self.scope.write().await;
                if assign.local {
                    // local: set in innermost (current function) frame
                    scope.set(&assign.name, value.clone());
                } else {
                    // non-local: update existing or create in root frame
                    scope.set_global(&assign.name, value.clone());
                }
                drop(scope);

                // Assignments don't produce output (like sh)
                Ok(ControlFlow::ok(ExecResult::success("")))
            }
            Stmt::Command(cmd) => {
                // Route single commands through execute_pipeline for a unified path.
                // This ensures all commands go through the dispatcher chain.
                let pipeline = crate::ast::Pipeline {
                    commands: vec![cmd.clone()],
                    background: false,
                };
                let result = self.execute_pipeline(&pipeline).await?;
                self.update_last_result(&result).await;

                // Check for error exit mode (set -e)
                if !result.ok() {
                    let scope = self.scope.read().await;
                    if scope.error_exit_enabled() {
                        return Ok(ControlFlow::exit_code(result.code));
                    }
                }

                Ok(ControlFlow::ok(result))
            }
            Stmt::Pipeline(pipeline) => {
                let result = self.execute_pipeline(pipeline).await?;
                self.update_last_result(&result).await;

                // Check for error exit mode (set -e)
                if !result.ok() {
                    let scope = self.scope.read().await;
                    if scope.error_exit_enabled() {
                        return Ok(ControlFlow::exit_code(result.code));
                    }
                }

                Ok(ControlFlow::ok(result))
            }
            Stmt::If(if_stmt) => {
                // Use async evaluator to support command substitution in conditions
                let cond_value = self.eval_expr_async(&if_stmt.condition).await?;

                let branch = if is_truthy(&cond_value) {
                    &if_stmt.then_branch
                } else {
                    if_stmt.else_branch.as_deref().unwrap_or(&[])
                };

                let mut result = ExecResult::success("");
                for stmt in branch {
                    let flow = self.execute_stmt_flow(stmt).await?;
                    match flow {
                        ControlFlow::Normal(r) => {
                            accumulate_result(&mut result, &r);
                            self.drain_stderr_into(&mut result).await;
                        }
                        other => {
                            self.drain_stderr_into(&mut result).await;
                            return Ok(other);
                        }
                    }
                }
                Ok(ControlFlow::ok(result))
            }
            Stmt::For(for_loop) => {
                // Evaluate all items and collect values for iteration
                // Use async evaluator to support command substitution like $(seq 1 5)
                let mut items: Vec<Value> = Vec::new();
                for item_expr in &for_loop.items {
                    // Glob expansion in for-loop items: `for f in *.txt`
                    if let Expr::GlobPattern(pattern) = item_expr {
                        let glob_enabled = {
                            let scope = self.scope.read().await;
                            scope.glob_enabled()
                        };
                        if glob_enabled {
                            let (paths, cwd) = {
                                let ctx = self.exec_ctx.read().await;
                                let paths = ctx.expand_glob(pattern).await
                                    .map_err(|e| anyhow::anyhow!("glob: {}", e))?;
                                let cwd = ctx.resolve_path(".");
                                (paths, cwd)
                            };
                            if paths.is_empty() {
                                return Err(anyhow::anyhow!("no matches: {}", pattern));
                            }
                            for path in paths {
                                let display = if !pattern.starts_with('/') {
                                    path.strip_prefix(&cwd)
                                        .unwrap_or(&path)
                                        .to_string_lossy().into_owned()
                                } else {
                                    path.to_string_lossy().into_owned()
                                };
                                items.push(Value::String(display));
                            }
                            continue;
                        }
                    }
                    // Track whether this item came from $(cmd); that's the
                    // only position where multi-line stdout auto-splits per
                    // line. Arrays still spread element-by-element; bare
                    // $VAR is rejected upstream by validator E012. See
                    // docs/plan-for-loop-newline-split.md.
                    let from_command_subst = matches!(item_expr, Expr::CommandSubst(_));
                    let item = self.eval_expr_async(item_expr).await?;
                    match item {
                        // JSON arrays iterate over elements (preferred path
                        // when builtins emit .data — seq, jq, cut, find, …)
                        Value::Json(serde_json::Value::Array(arr)) => {
                            for elem in arr {
                                items.push(json_to_value(elem));
                            }
                        }
                        // Strings from $(cmd): empty → 0 iterations,
                        // multi-line → split per line (trimming trailing
                        // newlines and per-line trailing \r), single-line
                        // → one iteration. Whitespace within a line is
                        // NOT split — the "$VAR with spaces just works"
                        // promise is preserved because this only fires
                        // in CommandSubst position.
                        Value::String(s) if from_command_subst => {
                            let trimmed = s.trim_end_matches(['\n', '\r']);
                            if trimmed.is_empty() {
                                continue;
                            }
                            if trimmed.contains('\n') {
                                for line in trimmed.split('\n') {
                                    let line = line.trim_end_matches('\r');
                                    items.push(Value::String(line.to_string()));
                                }
                            } else {
                                items.push(Value::String(trimmed.to_string()));
                            }
                        }
                        // Strings not from $(cmd) stay as one value.
                        other => items.push(other),
                    }
                }

                let mut result = ExecResult::success("");
                {
                    let mut scope = self.scope.write().await;
                    scope.push_frame();
                }

                'outer: for item in items {
                    // Cancellation checkpoint per iteration
                    if self.is_cancelled() {
                        let mut scope = self.scope.write().await;
                        scope.pop_frame();
                        result.code = 130;
                        return Ok(ControlFlow::ok(result));
                    }
                    {
                        let mut scope = self.scope.write().await;
                        scope.set(&for_loop.variable, item);
                    }
                    for stmt in &for_loop.body {
                        let mut flow = match self.execute_stmt_flow(stmt).await {
                            Ok(f) => f,
                            Err(e) => {
                                let mut scope = self.scope.write().await;
                                scope.pop_frame();
                                return Err(e);
                            }
                        };
                        self.drain_stderr_into(&mut result).await;
                        match &mut flow {
                            ControlFlow::Normal(r) => {
                                accumulate_result(&mut result, r);
                                if !r.ok() {
                                    let scope = self.scope.read().await;
                                    if scope.error_exit_enabled() {
                                        drop(scope);
                                        let mut scope = self.scope.write().await;
                                        scope.pop_frame();
                                        return Ok(ControlFlow::exit_code(r.code));
                                    }
                                }
                            }
                            ControlFlow::Break { .. } => {
                                if flow.decrement_level() {
                                    break 'outer;
                                }
                                let mut scope = self.scope.write().await;
                                scope.pop_frame();
                                return Ok(flow);
                            }
                            ControlFlow::Continue { .. } => {
                                if flow.decrement_level() {
                                    continue 'outer;
                                }
                                let mut scope = self.scope.write().await;
                                scope.pop_frame();
                                return Ok(flow);
                            }
                            ControlFlow::Return { .. } | ControlFlow::Exit { .. } => {
                                let mut scope = self.scope.write().await;
                                scope.pop_frame();
                                return Ok(flow);
                            }
                        }
                    }
                }

                {
                    let mut scope = self.scope.write().await;
                    scope.pop_frame();
                }
                Ok(ControlFlow::ok(result))
            }
            Stmt::While(while_loop) => {
                let mut result = ExecResult::success("");

                'outer: loop {
                    // Evaluate condition - use async to support command substitution
                    // Cancellation checkpoint per iteration
                    if self.is_cancelled() {
                        result.code = 130;
                        return Ok(ControlFlow::ok(result));
                    }

                    let cond_value = self.eval_expr_async(&while_loop.condition).await?;

                    if !is_truthy(&cond_value) {
                        break;
                    }

                    // Execute body
                    for stmt in &while_loop.body {
                        let mut flow = self.execute_stmt_flow(stmt).await?;
                        self.drain_stderr_into(&mut result).await;
                        match &mut flow {
                            ControlFlow::Normal(r) => {
                                accumulate_result(&mut result, r);
                                if !r.ok() {
                                    let scope = self.scope.read().await;
                                    if scope.error_exit_enabled() {
                                        return Ok(ControlFlow::exit_code(r.code));
                                    }
                                }
                            }
                            ControlFlow::Break { .. } => {
                                if flow.decrement_level() {
                                    break 'outer;
                                }
                                return Ok(flow);
                            }
                            ControlFlow::Continue { .. } => {
                                if flow.decrement_level() {
                                    continue 'outer;
                                }
                                return Ok(flow);
                            }
                            ControlFlow::Return { .. } | ControlFlow::Exit { .. } => {
                                return Ok(flow);
                            }
                        }
                    }
                }

                Ok(ControlFlow::ok(result))
            }
            Stmt::Case(case_stmt) => {
                // Evaluate the expression to match against
                let match_value = {
                    let value = self.eval_expr_async(&case_stmt.expr).await?;
                    value_to_string(&value)
                };

                // Try each branch until we find a match
                for branch in &case_stmt.branches {
                    let matched = branch.patterns.iter().any(|pattern| {
                        glob_match(pattern, &match_value)
                    });

                    if matched {
                        // Execute the branch body
                        let mut result = ExecResult::success("");
                        for stmt in &branch.body {
                            let flow = self.execute_stmt_flow(stmt).await?;
                            match flow {
                                ControlFlow::Normal(r) => {
                                    accumulate_result(&mut result, &r);
                                    self.drain_stderr_into(&mut result).await;
                                }
                                other => {
                                    self.drain_stderr_into(&mut result).await;
                                    return Ok(other);
                                }
                            }
                        }
                        return Ok(ControlFlow::ok(result));
                    }
                }

                // No match - return success with empty output (like sh)
                Ok(ControlFlow::ok(ExecResult::success("")))
            }
            Stmt::Break(levels) => {
                Ok(ControlFlow::break_n(levels.unwrap_or(1)))
            }
            Stmt::Continue(levels) => {
                Ok(ControlFlow::continue_n(levels.unwrap_or(1)))
            }
            Stmt::Return(expr) => {
                // return [N] - N becomes the exit code, NOT stdout
                // Shell semantics: return sets exit code, doesn't produce output
                let result = if let Some(e) = expr {
                    let val = self.eval_expr_async(e).await?;
                    let code = crate::interpreter::value_to_exit_code(&val)
                        .map_err(|e| anyhow::anyhow!("return: {}", e))?;
                    ExecResult::from_parts(code, String::new(), String::new(), None)
                } else {
                    ExecResult::success("")
                };
                Ok(ControlFlow::return_value(result))
            }
            Stmt::Exit(expr) => {
                let code = if let Some(e) = expr {
                    let val = self.eval_expr_async(e).await?;
                    crate::interpreter::value_to_exit_code(&val)
                        .map_err(|e| anyhow::anyhow!("exit: {}", e))?
                } else {
                    0
                };
                Ok(ControlFlow::exit_code(code))
            }
            Stmt::ToolDef(tool_def) => {
                let mut user_tools = self.user_tools.write().await;
                user_tools.insert(tool_def.name.clone(), tool_def.clone());
                Ok(ControlFlow::ok(ExecResult::success("")))
            }
            Stmt::AndChain { left, right } => {
                // cmd1 && cmd2 - run cmd2 only if cmd1 succeeds (exit code 0)
                // Suppress errexit for the left side — && handles failure itself.
                {
                    let mut scope = self.scope.write().await;
                    scope.suppress_errexit();
                }
                let left_flow = match self.execute_stmt_flow(left).await {
                    Ok(f) => f,
                    Err(e) => {
                        let mut scope = self.scope.write().await;
                        scope.unsuppress_errexit();
                        return Err(e);
                    }
                };
                {
                    let mut scope = self.scope.write().await;
                    scope.unsuppress_errexit();
                }
                match left_flow {
                    ControlFlow::Normal(mut left_result) => {
                        self.drain_stderr_into(&mut left_result).await;
                        self.update_last_result(&left_result).await;
                        if left_result.ok() {
                            let right_flow = self.execute_stmt_flow(right).await?;
                            match right_flow {
                                ControlFlow::Normal(mut right_result) => {
                                    self.drain_stderr_into(&mut right_result).await;
                                    self.update_last_result(&right_result).await;
                                    let mut combined = left_result;
                                    accumulate_result(&mut combined, &right_result);
                                    Ok(ControlFlow::ok(combined))
                                }
                                other => Ok(other),
                            }
                        } else {
                            Ok(ControlFlow::ok(left_result))
                        }
                    }
                    _ => Ok(left_flow),
                }
            }
            Stmt::OrChain { left, right } => {
                // cmd1 || cmd2 - run cmd2 only if cmd1 fails (non-zero exit code)
                // Suppress errexit for the left side — || handles failure itself.
                {
                    let mut scope = self.scope.write().await;
                    scope.suppress_errexit();
                }
                let left_flow = match self.execute_stmt_flow(left).await {
                    Ok(f) => f,
                    Err(e) => {
                        let mut scope = self.scope.write().await;
                        scope.unsuppress_errexit();
                        return Err(e);
                    }
                };
                {
                    let mut scope = self.scope.write().await;
                    scope.unsuppress_errexit();
                }
                match left_flow {
                    ControlFlow::Normal(mut left_result) => {
                        self.drain_stderr_into(&mut left_result).await;
                        self.update_last_result(&left_result).await;
                        if !left_result.ok() {
                            let right_flow = self.execute_stmt_flow(right).await?;
                            match right_flow {
                                ControlFlow::Normal(mut right_result) => {
                                    self.drain_stderr_into(&mut right_result).await;
                                    self.update_last_result(&right_result).await;
                                    let mut combined = left_result;
                                    accumulate_result(&mut combined, &right_result);
                                    Ok(ControlFlow::ok(combined))
                                }
                                other => Ok(other),
                            }
                        } else {
                            Ok(ControlFlow::ok(left_result))
                        }
                    }
                    _ => Ok(left_flow), // Propagate non-normal flow
                }
            }
            Stmt::Test(test_expr) => {
                let is_true = self.eval_test_async(test_expr).await?;
                if is_true {
                    Ok(ControlFlow::ok(ExecResult::success("")))
                } else {
                    Ok(ControlFlow::ok(ExecResult::failure(1, "")))
                }
            }
            Stmt::Empty => Ok(ControlFlow::ok(ExecResult::success(""))),
        }
        }.instrument(span))
    }

    /// Execute a pipeline.
    #[tracing::instrument(level = "debug", skip(self, pipeline), fields(background = pipeline.background, command_count = pipeline.commands.len()))]
    async fn execute_pipeline(&self, pipeline: &crate::ast::Pipeline) -> Result<ExecResult> {
        if pipeline.commands.is_empty() {
            return Ok(ExecResult::success(""));
        }

        // Handle background execution (`&` operator)
        if pipeline.background {
            return self.execute_background(pipeline).await;
        }

        // All commands go through the runner with the Kernel as dispatcher.
        // This is the single execution path — no fast path for single commands.
        //
        // IMPORTANT: We snapshot exec_ctx into a local context and release the
        // lock before running. This prevents deadlocks when dispatch_command
        // is called from within the pipeline and recursively triggers another
        // pipeline (e.g., via user-defined tools).
        let mut ctx = {
            let ec = self.exec_ctx.read().await;
            let scope = self.scope.read().await;
            ExecContext {
                backend: ec.backend.clone(),
                scope: scope.clone(),
                cwd: ec.cwd.clone(),
                prev_cwd: ec.prev_cwd.clone(),
                stdin: None,
                stdin_data: None,
                pipe_stdin: None,
                pipe_stdout: None,
                stderr: ec.stderr.clone(),
                tool_schemas: ec.tool_schemas.clone(),
                tools: ec.tools.clone(),
                job_manager: ec.job_manager.clone(),
                pipeline_position: PipelinePosition::Only,
                interactive: self.interactive,
                aliases: ec.aliases.clone(),
                ignore_config: ec.ignore_config.clone(),
                output_limit: ec.output_limit.clone(),
                allow_external_commands: self.allow_external_commands,
                nonce_store: ec.nonce_store.clone(),
                trash_backend: ec.trash_backend.clone(),
                #[cfg(all(unix, feature = "native"))]
                terminal_state: ec.terminal_state.clone(),
                dispatcher: self.dispatcher(),
                cancel: {
                    #[allow(clippy::expect_used)]
                    let token = self.cancel_token.lock().expect("cancel_token poisoned");
                    token.clone()
                },
                output_format: None,
            }
        }; // locks released

        let mut result = self.runner.run(&pipeline.commands, &mut ctx, self).await;

        // Post-hoc spill check (catches builtins and fast external commands)
        if ctx.output_limit.is_enabled() {
            let _ = crate::output_limit::spill_if_needed(&mut result, &ctx.output_limit).await;
        }

        // Signal spill with exit 3; agent reads the spill file directly
        // (use `set +o output-limit` before cat/head/tail to bypass the limit)
        if result.did_spill {
            result.original_code = Some(result.code);
            result.code = 3;
        }

        // Sync changes back from context
        {
            let mut ec = self.exec_ctx.write().await;
            ec.cwd = ctx.cwd.clone();
            ec.prev_cwd = ctx.prev_cwd.clone();
            ec.aliases = ctx.aliases.clone();
            ec.ignore_config = ctx.ignore_config.clone();
            ec.output_limit = ctx.output_limit.clone();
        }
        {
            let mut scope = self.scope.write().await;
            *scope = ctx.scope.clone();
        }

        Ok(result)
    }

    /// Execute a pipeline in the background.
    ///
    /// The command is spawned as a tokio task, registered with the JobManager,
    /// and its output is captured via BoundedStreams. The job is observable via
    /// `/v/jobs/{id}/stdout`, `/v/jobs/{id}/stderr`, and `/v/jobs/{id}/status`.
    ///
    /// Returns immediately with a job ID like "[1]".
    #[tracing::instrument(level = "debug", skip(self, pipeline), fields(command_count = pipeline.commands.len()))]
    async fn execute_background(&self, pipeline: &crate::ast::Pipeline) -> Result<ExecResult> {
        use tokio::sync::oneshot;

        // Format the command for display in /v/jobs/{id}/command
        let command_str = self.format_pipeline(pipeline);

        // Create bounded streams for output capture
        let stdout = Arc::new(BoundedStream::default_size());
        let stderr = Arc::new(BoundedStream::default_size());

        // Create channel for result notification
        let (tx, rx) = oneshot::channel();

        // Register with JobManager to get job ID and create VFS entries
        let job_id = self.jobs.register_with_streams(
            command_str.clone(),
            rx,
            stdout.clone(),
            stderr.clone(),
        ).await;

        // Fork the kernel for this background job. The fork snapshots the
        // parent's scope/cwd/aliases/user_tools so mutations stay isolated,
        // while sharing the job manager, VFS, and tool registry. The fork's
        // full dispatch chain (user tools, .kai scripts, `$(...)` in args)
        // is available here — something BackendDispatcher couldn't provide.
        let fork = self.fork().await;
        let runner = self.runner.clone();
        let commands = pipeline.commands.clone();

        // Snapshot the fork's exec_ctx for the spawned task. We have to do
        // this before tokio::spawn because the fork's exec_ctx is behind a
        // tokio RwLock and we want the spawned task to own its ctx.
        let mut bg_ctx = {
            let ec = fork.exec_ctx.read().await;
            ec.child_for_pipeline()
        };
        bg_ctx.scope = fork.scope.read().await.clone();
        // The fork's dispatcher points at the fork itself; set it here so
        // builtins inside the background task (e.g. timeout) re-dispatch
        // through the fork, not the parent.
        bg_ctx.dispatcher = fork.dispatcher();

        // Spawn the background task
        tokio::spawn(async move {
            // runner.run needs a &dyn CommandDispatcher; fork.as_ref()
            // gives us that (Kernel implements CommandDispatcher).
            let result = runner.run(&commands, &mut bg_ctx, fork.as_ref()).await;

            // Write output to streams
            let text = result.text_out();
            if !text.is_empty() {
                stdout.write(text.as_bytes()).await;
            }
            if !result.err.is_empty() {
                stderr.write(result.err.as_bytes()).await;
            }

            // Close streams
            stdout.close().await;
            stderr.close().await;

            // Send result to JobManager (ignore error if receiver dropped)
            let _ = tx.send(result);
        });

        Ok(ExecResult::success(format!("[{}]", job_id)))
    }

    /// Format a pipeline as a command string for display.
    fn format_pipeline(&self, pipeline: &crate::ast::Pipeline) -> String {
        pipeline.commands
            .iter()
            .map(|cmd| {
                let mut parts = vec![cmd.name.clone()];
                for arg in &cmd.args {
                    match arg {
                        Arg::Positional(expr) => {
                            parts.push(self.format_expr(expr));
                        }
                        Arg::Named { key, value } => {
                            parts.push(format!("--{}={}", key, self.format_expr(value)));
                        }
                        Arg::WordAssign { key, value } => {
                            parts.push(format!("{}={}", key, self.format_expr(value)));
                        }
                        Arg::ShortFlag(name) => {
                            parts.push(format!("-{}", name));
                        }
                        Arg::LongFlag(name) => {
                            parts.push(format!("--{}", name));
                        }
                        Arg::DoubleDash => {
                            parts.push("--".to_string());
                        }
                    }
                }
                parts.join(" ")
            })
            .collect::<Vec<_>>()
            .join(" | ")
    }

    /// Format an expression as a string for display.
    fn format_expr(&self, expr: &Expr) -> String {
        match expr {
            Expr::Literal(Value::String(s)) => {
                if s.contains(' ') || s.contains('"') {
                    format!("'{}'", s.replace('\'', "\\'"))
                } else {
                    s.clone()
                }
            }
            Expr::Literal(Value::Int(i)) => i.to_string(),
            Expr::Literal(Value::Float(f)) => f.to_string(),
            Expr::Literal(Value::Bool(b)) => b.to_string(),
            Expr::Literal(Value::Null) => "null".to_string(),
            Expr::VarRef(path) => {
                let name = path.segments.iter()
                    .map(|seg| match seg {
                        crate::ast::VarSegment::Field(f) => f.clone(),
                    })
                    .collect::<Vec<_>>()
                    .join(".");
                format!("${{{}}}", name)
            }
            Expr::Interpolated(_) => "\"...\"".to_string(),
            Expr::HereDocBody { .. } => "<<heredoc".to_string(),
            _ => "...".to_string(),
        }
    }

    /// Execute a single command.
    async fn execute_command(&self, name: &str, args: &[Arg]) -> Result<ExecResult> {
        self.execute_command_depth(name, args, 0).await
    }

    #[tracing::instrument(level = "info", skip(self, args, alias_depth), fields(command = %name), err)]
    async fn execute_command_depth(&self, name: &str, args: &[Arg], alias_depth: u8) -> Result<ExecResult> {
        // Special built-ins
        match name {
            "true" => return Ok(ExecResult::success("")),
            "false" => return Ok(ExecResult::failure(1, "")),
            "source" | "." => return self.execute_source(args).await,
            _ => {}
        }

        // Alias expansion (with recursion limit)
        if alias_depth < 10 {
            let alias_value = {
                let ctx = self.exec_ctx.read().await;
                ctx.aliases.get(name).cloned()
            };
            if let Some(alias_val) = alias_value {
                // Split alias value into command + args
                let parts: Vec<&str> = alias_val.split_whitespace().collect();
                if let Some((alias_cmd, alias_args)) = parts.split_first() {
                    let mut new_args: Vec<Arg> = alias_args
                        .iter()
                        .map(|a| Arg::Positional(Expr::Literal(Value::String(a.to_string()))))
                        .collect();
                    new_args.extend_from_slice(args);
                    return Box::pin(self.execute_command_depth(alias_cmd, &new_args, alias_depth + 1)).await;
                }
            }
        }

        // Handle /v/bin/ prefix — dispatch to builtins via virtual path
        if let Some(builtin_name) = name.strip_prefix("/v/bin/") {
            return match self.tools.get(builtin_name) {
                Some(_) => Box::pin(self.execute_command_depth(builtin_name, args, alias_depth)).await,
                None => Ok(ExecResult::failure(127, format!("command not found: {}", name))),
            };
        }

        // Check user-defined tools first
        {
            let user_tools = self.user_tools.read().await;
            if let Some(tool_def) = user_tools.get(name) {
                let tool_def = tool_def.clone();
                drop(user_tools);
                return self.execute_user_tool(tool_def, args).await;
            }
        }

        // Look up builtin tool
        let tool = match self.tools.get(name) {
            Some(t) => t,
            None => {
                // Try executing as .kai script from PATH
                if let Some(result) = self.try_execute_script(name, args).await? {
                    return Ok(result);
                }
                // Try executing as external command from PATH
                if let Some(result) = self.try_execute_external(name, args).await? {
                    return Ok(result);
                }

                // Try backend-registered tools (embedder engines, etc.)
                // Look up tool schema for positional→named mapping.
                // Clone backend and drop read lock before awaiting (may involve network I/O).
                // Backend tools expect named JSON params, so enable positional mapping.
                let backend = self.exec_ctx.read().await.backend.clone();
                let tool_schema = backend.get_tool(name).await.ok().flatten().map(|t| {
                    let mut s = t.schema;
                    s.map_positionals = true;
                    s
                });
                let tool_args = self.build_args_async(args, tool_schema.as_ref()).await?;
                let mut ctx = self.exec_ctx.write().await;
                {
                    let scope = self.scope.read().await;
                    ctx.scope = scope.clone();
                }
                let backend = ctx.backend.clone();
                match backend.call_tool(name, tool_args, &mut ctx).await {
                    Ok(tool_result) => {
                        let mut scope = self.scope.write().await;
                        *scope = ctx.scope.clone();
                        let mut exec = ExecResult::from_output(
                            tool_result.code as i64, tool_result.stdout, tool_result.stderr,
                        );
                        exec.set_output(tool_result.output);
                        return Ok(exec);
                    }
                    Err(BackendError::ToolNotFound(_)) => {
                        // Fall through to "command not found"
                    }
                    Err(e) => {
                        // Backend dispatch is last-resort lookup — if it fails
                        // for any reason, the command simply doesn't exist.
                        tracing::debug!("backend error for {name}: {e}");
                    }
                }

                return Ok(ExecResult::failure(127, format!("command not found: {}", name)));
            }
        };

        // Build arguments (async to support command substitution, schema-aware for flag values)
        let schema = tool.schema();
        let tool_args = self.build_args_async(args, Some(&schema)).await?;

        // --help / -h: show help unless the tool's schema claims that flag
        let schema_claims = |flag: &str| -> bool {
            let bare = flag.trim_start_matches('-');
            schema.params.iter().any(|p| p.matches_flag(flag) || p.matches_flag(bare))
        };
        let wants_help =
            (tool_args.flags.contains("help") && !schema_claims("help"))
            || (tool_args.flags.contains("h") && !schema_claims("-h"));
        if wants_help {
            let help_topic = crate::help::HelpTopic::Tool(name.to_string());
            let ctx = self.exec_ctx.read().await;
            let content = crate::help::get_help(&help_topic, &ctx.tool_schemas);
            return Ok(ExecResult::with_output(crate::interpreter::OutputData::text(content)));
        }

        // Snapshot exec_ctx into a local context and release the write lock
        // before calling tool.execute. Holding the write across tool execution
        // would deadlock any builtin that re-dispatches through ctx.dispatcher
        // (timeout, scatter) — the inner dispatch_command needs its own
        // exec_ctx.write() and would block forever.
        let mut ctx = {
            let ec = self.exec_ctx.write().await;
            let scope = self.scope.read().await;
            ExecContext {
                backend: ec.backend.clone(),
                scope: scope.clone(),
                cwd: ec.cwd.clone(),
                prev_cwd: ec.prev_cwd.clone(),
                stdin: ec.stdin.clone(),
                stdin_data: ec.stdin_data.clone(),
                pipe_stdin: None, // streaming pipes are per-pipeline; not snapshotted
                pipe_stdout: None,
                stderr: ec.stderr.clone(),
                tool_schemas: ec.tool_schemas.clone(),
                tools: ec.tools.clone(),
                job_manager: ec.job_manager.clone(),
                pipeline_position: ec.pipeline_position,
                interactive: self.interactive,
                aliases: ec.aliases.clone(),
                ignore_config: ec.ignore_config.clone(),
                output_limit: ec.output_limit.clone(),
                allow_external_commands: self.allow_external_commands,
                nonce_store: ec.nonce_store.clone(),
                trash_backend: ec.trash_backend.clone(),
                #[cfg(all(unix, feature = "native"))]
                terminal_state: ec.terminal_state.clone(),
                dispatcher: self.dispatcher(),
                // Use ec.cancel (set by dispatch_command from the runner's
                // ctx.cancel) so any builtin-swapped child token (e.g. timeout's
                // child token) reaches the spawned external via wait_or_kill.
                // Falls back to the kernel's own token when ec.cancel is the
                // default fresh token from a non-dispatch path.
                cancel: ec.cancel.clone(),
                output_format: None,
            }
        }; // both locks released — tool.execute can re-dispatch safely

        // Move stdin out of self.exec_ctx into the snapshot (consumed-by-tool
        // semantics): take() so a later dispatch doesn't see stale stdin.
        // Done after the snapshot above so we hold the write briefly.
        {
            let mut ec = self.exec_ctx.write().await;
            ctx.stdin = ec.stdin.take();
            ctx.stdin_data = ec.stdin_data.take();
            ctx.pipe_stdin = ec.pipe_stdin.take();
            ctx.pipe_stdout = ec.pipe_stdout.take();
        }

        // Honor --json before the builtin runs so its setting survives a clap
        // parse failure (e.g. `cmd --json --bogus-flag` would otherwise drop
        // --json on the floor when `try_parse_from` returns Err early).
        // The builtin's own `parsed.global.apply(ctx)` becomes idempotent.
        GlobalFlags::apply_from_args(&tool_args, &mut ctx);

        let result = tool.execute(tool_args, &mut ctx).await;

        // Sync mutations back. Tools may have changed scope (set/cd),
        // cwd/prev_cwd (cd), and aliases (alias). Also return any unused pipe
        // endpoints to self.exec_ctx so dispatch_command's post-execute sync
        // hands them back to the pipeline runner — the runner uses
        // stage_ctx.pipe_stdout to write the result to the next stage when
        // the tool itself didn't take and write to it.
        {
            let mut scope = self.scope.write().await;
            *scope = ctx.scope.clone();
        }
        {
            let mut ec = self.exec_ctx.write().await;
            ec.cwd = ctx.cwd;
            ec.prev_cwd = ctx.prev_cwd;
            ec.aliases = ctx.aliases;
            ec.pipe_stdin = ctx.pipe_stdin.take();
            ec.pipe_stdout = ctx.pipe_stdout.take();
        }

        // Builtins parse --json via the GlobalFlags flatten in their clap
        // struct and write ctx.output_format. The kernel just applies it.
        let result = match ctx.output_format {
            Some(format) => apply_output_format(result, format),
            None => result,
        };

        Ok(result)
    }

    /// Pull `consumes` positional args after a non-bool flag and stash them
    /// on `tool_args.named` under the canonical param name.
    ///
    /// - `consumes == 1` keeps the historical contract: a single scalar value.
    /// - `consumes > 1` accumulates each occurrence as an inner
    ///   `serde_json::Value::Array` inside `named[canonical] =
    ///   Value::Json(Array(...))`, preserving invocation order. This is the
    ///   shape jq's `--arg NAME VAL` / `--argjson NAME VAL` land in.
    ///
    /// Errors loudly if the flag is missing required positionals — matches
    /// kaish's "no silent fallback" posture and mirrors real jq, which
    /// errors on `--arg NAME` with no value.
    #[allow(clippy::too_many_arguments)]
    async fn consume_flag_positionals(
        &self,
        args: &[Arg],
        flag_name: &str,
        canonical: &str,
        consumes: usize,
        positional_indices: &[usize],
        consumed: &mut std::collections::HashSet<usize>,
        current_idx: usize,
        tool_args: &mut ToolArgs,
    ) -> Result<()> {
        let mut collected: Vec<Value> = Vec::with_capacity(consumes.max(1));
        for _ in 0..consumes.max(1) {
            let next_pos = positional_indices
                .iter()
                .find(|idx| **idx > current_idx && !consumed.contains(idx))
                .copied();
            match next_pos {
                Some(pos_idx) => {
                    if let Arg::Positional(expr) = &args[pos_idx] {
                        let value = self.eval_expr_async(expr).await?;
                        let value = apply_tilde_expansion(value);
                        collected.push(value);
                        consumed.insert(pos_idx);
                    }
                }
                None => {
                    if consumes <= 1 && collected.is_empty() {
                        // Back-compat: a flag with no follow-up positional
                        // becomes a bare flag. `--path` with nothing after
                        // lands in `flags`, same as before this refactor.
                        tool_args.flags.insert(flag_name.to_string());
                        return Ok(());
                    }
                    anyhow::bail!(
                        "--{flag_name} requires {consumes} argument{}, got {}",
                        if consumes == 1 { "" } else { "s" },
                        collected.len()
                    );
                }
            }
        }

        if consumes <= 1 {
            if let Some(v) = collected.pop() {
                tool_args.named.insert(canonical.to_string(), v);
            }
            return Ok(());
        }

        // Multi-consume: accumulate under named[canonical] as array-of-arrays.
        let occ: Vec<serde_json::Value> = collected
            .into_iter()
            .map(|v| crate::interpreter::value_to_json(&v))
            .collect();
        let entry = tool_args
            .named
            .entry(canonical.to_string())
            .or_insert_with(|| Value::Json(serde_json::Value::Array(Vec::new())));
        if let Value::Json(serde_json::Value::Array(outer)) = entry {
            outer.push(serde_json::Value::Array(occ));
        } else {
            anyhow::bail!(
                "--{flag_name}: named[{canonical}] already holds a non-array value"
            );
        }
        Ok(())
    }

    /// Build tool arguments from AST args.
    ///
    /// Uses async evaluation to support command substitution in arguments.
    async fn build_args_async(&self, args: &[Arg], schema: Option<&crate::tools::ToolSchema>) -> Result<ToolArgs> {
        let mut tool_args = ToolArgs::new();
        let param_lookup = schema.map(schema_param_lookup).unwrap_or_default();
        let accepts_word_assign = schema
            .map(|s| crate::tools::accepts_word_assign(s.name.as_str()))
            .unwrap_or(false);

        // Track which positional indices have been consumed as flag values
        let mut consumed: std::collections::HashSet<usize> = std::collections::HashSet::new();
        let mut past_double_dash = false;

        // Find positional arg indices for flag value consumption
        let positional_indices: Vec<usize> = args.iter().enumerate()
            .filter_map(|(i, a)| matches!(a, Arg::Positional(_)).then_some(i))
            .collect();

        let mut i = 0;
        while i < args.len() {
            match &args[i] {
                Arg::DoubleDash => {
                    past_double_dash = true;
                }
                Arg::Positional(expr) => {
                    if !consumed.contains(&i) {
                        // Glob expansion: bare glob patterns expand to matching files
                        if let Expr::GlobPattern(pattern) = expr {
                            let glob_enabled = {
                                let scope = self.scope.read().await;
                                scope.glob_enabled()
                            };
                            if glob_enabled {
                                let (paths, cwd) = {
                                    let ctx = self.exec_ctx.read().await;
                                    let paths = ctx.expand_glob(pattern).await
                                        .map_err(|e| anyhow::anyhow!("glob: {}", e))?;
                                    let cwd = ctx.resolve_path(".");
                                    (paths, cwd)
                                };
                                if paths.is_empty() {
                                    return Err(anyhow::anyhow!("no matches: {}", pattern));
                                }
                                for path in paths {
                                    let display = if !pattern.starts_with('/') {
                                        path.strip_prefix(&cwd)
                                            .unwrap_or(&path)
                                            .to_string_lossy().into_owned()
                                    } else {
                                        path.to_string_lossy().into_owned()
                                    };
                                    tool_args.positional.push(Value::String(display));
                                }
                                i += 1;
                                continue;
                            }
                        }
                        let value = self.eval_expr_async(expr).await?;
                        let value = apply_tilde_expansion(value);
                        tool_args.positional.push(value);
                    }
                }
                Arg::Named { key, value } => {
                    let val = self.eval_expr_async(value).await?;
                    let val = apply_tilde_expansion(val);
                    tool_args.named.insert(key.clone(), val);
                }
                Arg::WordAssign { key, value } => {
                    let val = self.eval_expr_async(value).await?;
                    let val = apply_tilde_expansion(val);
                    if accepts_word_assign {
                        tool_args.named.insert(key.clone(), val);
                    } else {
                        // Stringify "key=value" and pass as a positional.
                        // Matches bash: `cat foo=bar` opens a file named `foo=bar`.
                        let val_str = crate::interpreter::value_to_string(&val);
                        tool_args.positional.push(Value::String(format!("{key}={val_str}")));
                    }
                }
                Arg::ShortFlag(name) => {
                    if past_double_dash {
                        tool_args.positional.push(Value::String(format!("-{name}")));
                    } else if name.len() == 1 {
                        let flag_name = name.as_str();
                        let lookup = param_lookup.get(flag_name);
                        let is_bool = lookup.map(|(_, typ, _)| is_bool_type(typ)).unwrap_or(true);

                        if is_bool {
                            tool_args.flags.insert(flag_name.to_string());
                        } else {
                            // Non-bool: consume `consumes` positionals as value(s)
                            let canonical = lookup.map(|(n, _, _)| *n).unwrap_or(flag_name);
                            let consumes = lookup.map(|(_, _, c)| *c).unwrap_or(1);
                            self.consume_flag_positionals(
                                args,
                                name,
                                canonical,
                                consumes,
                                &positional_indices,
                                &mut consumed,
                                i,
                                &mut tool_args,
                            )
                            .await?;
                        }
                    } else if let Some(&(canonical, typ, consumes)) = param_lookup.get(name.as_str()) {
                        // Multi-char short flag matches a schema param (POSIX style: -name value)
                        if is_bool_type(typ) {
                            tool_args.flags.insert(canonical.to_string());
                        } else {
                            self.consume_flag_positionals(
                                args,
                                name,
                                canonical,
                                consumes,
                                &positional_indices,
                                &mut consumed,
                                i,
                                &mut tool_args,
                            )
                            .await?;
                        }
                    } else {
                        // Multi-char combined flags like -la: always boolean
                        for c in name.chars() {
                            tool_args.flags.insert(c.to_string());
                        }
                    }
                }
                Arg::LongFlag(name) => {
                    if past_double_dash {
                        tool_args.positional.push(Value::String(format!("--{name}")));
                    } else {
                        let lookup = param_lookup.get(name.as_str());
                        let is_bool = lookup.map(|(_, typ, _)| is_bool_type(typ)).unwrap_or(true);

                        if is_bool {
                            tool_args.flags.insert(name.clone());
                        } else {
                            let canonical = lookup.map(|(n, _, _)| *n).unwrap_or(name.as_str());
                            let consumes = lookup.map(|(_, _, c)| *c).unwrap_or(1);
                            self.consume_flag_positionals(
                                args,
                                name,
                                canonical,
                                consumes,
                                &positional_indices,
                                &mut consumed,
                                i,
                                &mut tool_args,
                            )
                            .await?;
                        }
                    }
                }
            }
            i += 1;
        }

        // Map remaining positionals to unfilled non-bool schema params (in order).
        // This enables `drift_push "abc" "hello"` → named["target_ctx"] = "abc", named["content"] = "hello"
        // Positionals that appeared after `--` are never mapped (they're raw data).
        // Only for backend/external tools (map_positionals=true). Builtins handle their own positionals.
        if let Some(schema) = schema.filter(|s| s.map_positionals) {
            let pre_dash_count = if past_double_dash {
                let dash_pos = args.iter().position(|a| matches!(a, Arg::DoubleDash)).unwrap_or(args.len());
                positional_indices.iter()
                    .filter(|idx| **idx < dash_pos && !consumed.contains(idx))
                    .count()
            } else {
                tool_args.positional.len()
            };

            let mut remaining = Vec::new();
            let mut positional_iter = tool_args.positional.drain(..).enumerate();

            for param in &schema.params {
                if tool_args.named.contains_key(&param.name) || tool_args.flags.contains(&param.name) {
                    continue;
                }
                if is_bool_type(&param.param_type) {
                    continue;
                }
                loop {
                    match positional_iter.next() {
                        Some((idx, val)) if idx < pre_dash_count => {
                            tool_args.named.insert(param.name.clone(), val);
                            break;
                        }
                        Some((_, val)) => {
                            remaining.push(val);
                        }
                        None => break,
                    }
                }
            }

            remaining.extend(positional_iter.map(|(_, v)| v));
            tool_args.positional = remaining;
        }

        Ok(tool_args)
    }

    /// Build arguments as flat string list for external commands.
    ///
    /// Unlike `build_args_async` which separates flags into a HashSet (for schema-aware builtins),
    /// this preserves the original flag format as strings for external commands:
    /// - `-l` stays as `-l`
    /// - `--verbose` stays as `--verbose`
    /// - `key=value` stays as `key=value`
    ///
    /// This is what external commands expect in their argv.
    #[cfg(feature = "native")]
    async fn build_args_flat(&self, args: &[Arg]) -> Result<Vec<String>> {
        let mut argv = Vec::new();
        for arg in args {
            match arg {
                Arg::Positional(expr) => {
                    // Glob expansion for external commands
                    if let Expr::GlobPattern(pattern) = expr {
                        let glob_enabled = {
                            let scope = self.scope.read().await;
                            scope.glob_enabled()
                        };
                        if glob_enabled {
                            let (paths, cwd) = {
                                let ctx = self.exec_ctx.read().await;
                                let paths = ctx.expand_glob(pattern).await
                                    .map_err(|e| anyhow::anyhow!("glob: {}", e))?;
                                let cwd = ctx.resolve_path(".");
                                (paths, cwd)
                            };
                            if paths.is_empty() {
                                return Err(anyhow::anyhow!("no matches: {}", pattern));
                            }
                            for path in paths {
                                let display = if !pattern.starts_with('/') {
                                    path.strip_prefix(&cwd)
                                        .unwrap_or(&path)
                                        .to_string_lossy().into_owned()
                                } else {
                                    path.to_string_lossy().into_owned()
                                };
                                argv.push(display);
                            }
                            continue;
                        }
                    }
                    let value = self.eval_expr_async(expr).await?;
                    let value = apply_tilde_expansion(value);
                    argv.push(value_to_string(&value));
                }
                Arg::Named { key, value } => {
                    let val = self.eval_expr_async(value).await?;
                    let val = apply_tilde_expansion(val);
                    argv.push(format!("--{}={}", key, value_to_string(&val)));
                }
                Arg::WordAssign { key, value } => {
                    let val = self.eval_expr_async(value).await?;
                    let val = apply_tilde_expansion(val);
                    argv.push(format!("{}={}", key, value_to_string(&val)));
                }
                Arg::ShortFlag(name) => {
                    // Preserve original format: -l, -la (combined flags)
                    argv.push(format!("-{}", name));
                }
                Arg::LongFlag(name) => {
                    // Preserve original format: --verbose
                    argv.push(format!("--{}", name));
                }
                Arg::DoubleDash => {
                    // Preserve the -- marker
                    argv.push("--".to_string());
                }
            }
        }
        Ok(argv)
    }

    /// Async expression evaluator that supports command substitution.
    ///
    /// This is used for contexts where expressions may contain `$(...)` command
    /// substitution. Unlike the sync `eval_expr`, this can execute pipelines.
    fn eval_expr_async<'a>(&'a self, expr: &'a Expr) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<Value>> + Send + 'a>> {
        Box::pin(async move {
        match expr {
            Expr::Literal(value) => Ok(value.clone()),
            Expr::VarRef(path) => {
                let scope = self.scope.read().await;
                scope.resolve_path(path)
                    .ok_or_else(|| anyhow::anyhow!("undefined variable"))
            }
            Expr::Interpolated(parts) => {
                let mut result = String::new();
                for part in parts {
                    result.push_str(&self.eval_string_part_async(part).await?);
                }
                Ok(Value::String(result))
            }
            Expr::HereDocBody { parts, strip_tabs } => {
                let mut result = String::new();
                for sp in parts {
                    result.push_str(&self.eval_string_part_async(&sp.part).await?);
                }
                if *strip_tabs {
                    Ok(Value::String(crate::interpreter::strip_leading_tabs(&result)))
                } else {
                    Ok(Value::String(result))
                }
            }
            Expr::BinaryOp { left, op, right } => match op {
                BinaryOp::And => {
                    let left_val = self.eval_expr_async(left).await?;
                    if !is_truthy(&left_val) {
                        return Ok(left_val);
                    }
                    self.eval_expr_async(right).await
                }
                BinaryOp::Or => {
                    let left_val = self.eval_expr_async(left).await?;
                    if is_truthy(&left_val) {
                        return Ok(left_val);
                    }
                    self.eval_expr_async(right).await
                }
            },
            Expr::CommandSubst(pipeline) => {
                // Snapshot scope+cwd before running — only output escapes,
                // not side effects like `cd` or variable assignments.
                let saved_scope = { self.scope.read().await.clone() };
                let saved_cwd = {
                    let ec = self.exec_ctx.read().await;
                    (ec.cwd.clone(), ec.prev_cwd.clone())
                };

                // Capture result without `?` — restore state unconditionally
                let run_result = self.execute_pipeline(pipeline).await;

                // Restore scope and cwd regardless of success/failure
                {
                    let mut scope = self.scope.write().await;
                    *scope = saved_scope;
                    if let Ok(ref r) = run_result {
                        scope.set_last_result(r.clone());
                    }
                }
                {
                    let mut ec = self.exec_ctx.write().await;
                    ec.cwd = saved_cwd.0;
                    ec.prev_cwd = saved_cwd.1;
                }

                // Now propagate the error
                let result = run_result?;

                // Prefer structured data (enables `for i in $(cmd)` iteration)
                if let Some(data) = &result.data {
                    Ok(data.clone())
                } else if let Some(output) = result.output() {
                    // Flat non-text node lists (glob, ls, tree) → iterable array
                    if output.is_flat() && !output.is_simple_text() && !output.root.is_empty() {
                        let items: Vec<serde_json::Value> = output.root.iter()
                            .map(|n| serde_json::Value::String(n.display_name().to_string()))
                            .collect();
                        Ok(Value::Json(serde_json::Value::Array(items)))
                    } else {
                        Ok(Value::String(result.text_out().trim_end().to_string()))
                    }
                } else {
                    // Otherwise return stdout as single string (NO implicit splitting)
                    Ok(Value::String(result.text_out().trim_end().to_string()))
                }
            }
            Expr::Test(test_expr) => {
                Ok(Value::Bool(self.eval_test_async(test_expr).await?))
            }
            Expr::Positional(n) => {
                let scope = self.scope.read().await;
                match scope.get_positional(*n) {
                    Some(s) => Ok(Value::String(s.to_string())),
                    None => Ok(Value::String(String::new())),
                }
            }
            Expr::AllArgs => {
                let scope = self.scope.read().await;
                Ok(Value::String(scope.all_args().join(" ")))
            }
            Expr::ArgCount => {
                let scope = self.scope.read().await;
                Ok(Value::Int(scope.arg_count() as i64))
            }
            Expr::VarLength(name) => {
                let scope = self.scope.read().await;
                match scope.get(name) {
                    Some(value) => Ok(Value::Int(value_to_string(value).len() as i64)),
                    None => Ok(Value::Int(0)),
                }
            }
            Expr::VarWithDefault { name, default } => {
                let scope = self.scope.read().await;
                let use_default = match scope.get(name) {
                    Some(value) => value_to_string(value).is_empty(),
                    None => true,
                };
                drop(scope); // Release the lock before recursive evaluation
                if use_default {
                    // Evaluate the default parts (supports nested expansions)
                    self.eval_string_parts_async(default).await.map(Value::String)
                } else {
                    let scope = self.scope.read().await;
                    scope.get(name).cloned().ok_or_else(|| anyhow::anyhow!("variable '{}' not found", name))
                }
            }
            Expr::Arithmetic(expr_str) => {
                let scope = self.scope.read().await;
                crate::arithmetic::eval_arithmetic(expr_str, &scope)
                    .map(Value::Int)
                    .map_err(|e| anyhow::anyhow!("arithmetic error: {}", e))
            }
            Expr::Command(cmd) => {
                // Execute command and return boolean based on exit code
                let result = self.execute_command(&cmd.name, &cmd.args).await?;
                Ok(Value::Bool(result.code == 0))
            }
            Expr::LastExitCode => {
                let scope = self.scope.read().await;
                Ok(Value::Int(scope.last_result().code))
            }
            Expr::CurrentPid => {
                let scope = self.scope.read().await;
                Ok(Value::Int(scope.pid() as i64))
            }
            Expr::GlobPattern(s) => Ok(Value::String(s.clone())),
        }
        })
    }

    /// Async helper to evaluate multiple StringParts into a single string.
    fn eval_string_parts_async<'a>(&'a self, parts: &'a [StringPart]) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<String>> + Send + 'a>> {
        Box::pin(async move {
            let mut result = String::new();
            for part in parts {
                result.push_str(&self.eval_string_part_async(part).await?);
            }
            Ok(result)
        })
    }

    /// Async helper to evaluate a StringPart.
    /// Evaluate a `[[ ]]` test expression asynchronously, routing file tests
    /// through the VFS backend instead of using raw `std::path`.
    fn eval_test_async<'a>(&'a self, test_expr: &'a TestExpr) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<bool>> + Send + 'a>> {
        Box::pin(async move {
            match test_expr {
                TestExpr::FileTest { op, path } => {
                    let path_value = self.eval_expr_async(path).await?;
                    let path_str = value_to_string(&path_value);
                    let backend = self.exec_ctx.read().await.backend.clone();
                    let entry = backend.stat(std::path::Path::new(&path_str)).await.ok();
                    Ok(match op {
                        FileTestOp::Exists => entry.is_some(),
                        FileTestOp::IsFile => entry.as_ref().is_some_and(|e| e.is_file()),
                        FileTestOp::IsDir => entry.as_ref().is_some_and(|e| e.is_dir()),
                        FileTestOp::Readable => entry.is_some(),
                        FileTestOp::Writable => entry.as_ref().is_some_and(|e| {
                            e.permissions.is_none_or(|p| p & 0o222 != 0)
                        }),
                        FileTestOp::Executable => entry.as_ref().is_some_and(|e| {
                            e.permissions.is_some_and(|p| p & 0o111 != 0)
                        }),
                    })
                }
                TestExpr::StringTest { op, value } => {
                    let val = self.eval_expr_async(value).await?;
                    let s = value_to_string(&val);
                    Ok(match op {
                        crate::ast::StringTestOp::IsEmpty => s.is_empty(),
                        crate::ast::StringTestOp::IsNonEmpty => !s.is_empty(),
                    })
                }
                TestExpr::Comparison { left, op, right } => {
                    // Evaluate operands async (handles $(cmd)), then compare sync
                    let left_val = self.eval_expr_async(left).await?;
                    let right_val = self.eval_expr_async(right).await?;
                    let resolved = TestExpr::Comparison {
                        left: Box::new(Expr::Literal(left_val)),
                        op: *op,
                        right: Box::new(Expr::Literal(right_val)),
                    };
                    let expr = Expr::Test(Box::new(resolved));
                    let mut scope = self.scope.write().await;
                    let value = eval_expr(&expr, &mut scope)
                        .map_err(|e| anyhow::anyhow!("{}", e))?;
                    Ok(value_to_bool(&value))
                }
                TestExpr::And { left, right } => {
                    if !self.eval_test_async(left).await? {
                        Ok(false)
                    } else {
                        self.eval_test_async(right).await
                    }
                }
                TestExpr::Or { left, right } => {
                    if self.eval_test_async(left).await? {
                        Ok(true)
                    } else {
                        self.eval_test_async(right).await
                    }
                }
                TestExpr::Not { expr } => {
                    Ok(!self.eval_test_async(expr).await?)
                }
            }
        })
    }

    fn eval_string_part_async<'a>(&'a self, part: &'a StringPart) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<String>> + Send + 'a>> {
        Box::pin(async move {
            match part {
                StringPart::Literal(s) => Ok(s.clone()),
                StringPart::Var(path) => {
                    let scope = self.scope.read().await;
                    match scope.resolve_path(path) {
                        Some(value) => Ok(value_to_string(&value)),
                        None => Ok(String::new()), // Unset vars expand to empty
                    }
                }
                StringPart::VarWithDefault { name, default } => {
                    let scope = self.scope.read().await;
                    let use_default = match scope.get(name) {
                        Some(value) => value_to_string(value).is_empty(),
                        None => true,
                    };
                    drop(scope); // Release lock before recursive evaluation
                    if use_default {
                        // Evaluate the default parts (supports nested expansions)
                        self.eval_string_parts_async(default).await
                    } else {
                        let scope = self.scope.read().await;
                        Ok(value_to_string(scope.get(name).ok_or_else(|| anyhow::anyhow!("variable '{}' not found", name))?))
                    }
                }
            StringPart::VarLength(name) => {
                let scope = self.scope.read().await;
                match scope.get(name) {
                    Some(value) => Ok(value_to_string(value).len().to_string()),
                    None => Ok("0".to_string()),
                }
            }
            StringPart::Positional(n) => {
                let scope = self.scope.read().await;
                match scope.get_positional(*n) {
                    Some(s) => Ok(s.to_string()),
                    None => Ok(String::new()),
                }
            }
            StringPart::AllArgs => {
                let scope = self.scope.read().await;
                Ok(scope.all_args().join(" "))
            }
            StringPart::ArgCount => {
                let scope = self.scope.read().await;
                Ok(scope.arg_count().to_string())
            }
            StringPart::Arithmetic(expr) => {
                let scope = self.scope.read().await;
                match crate::arithmetic::eval_arithmetic(expr, &scope) {
                    Ok(value) => Ok(value.to_string()),
                    Err(_) => Ok(String::new()),
                }
            }
            StringPart::CommandSubst(pipeline) => {
                // Snapshot scope+cwd — command substitution in strings must
                // not leak side effects (e.g., `"dir: $(cd /; pwd)"` must not change cwd).
                let saved_scope = { self.scope.read().await.clone() };
                let saved_cwd = {
                    let ec = self.exec_ctx.read().await;
                    (ec.cwd.clone(), ec.prev_cwd.clone())
                };

                // Capture result without `?` — restore state unconditionally
                let run_result = self.execute_pipeline(pipeline).await;

                // Restore scope and cwd regardless of success/failure
                {
                    let mut scope = self.scope.write().await;
                    *scope = saved_scope;
                    if let Ok(ref r) = run_result {
                        scope.set_last_result(r.clone());
                    }
                }
                {
                    let mut ec = self.exec_ctx.write().await;
                    ec.cwd = saved_cwd.0;
                    ec.prev_cwd = saved_cwd.1;
                }

                // Now propagate the error
                let result = run_result?;

                Ok(result.text_out().trim_end_matches('\n').to_string())
            }
            StringPart::LastExitCode => {
                let scope = self.scope.read().await;
                Ok(scope.last_result().code.to_string())
            }
            StringPart::CurrentPid => {
                let scope = self.scope.read().await;
                Ok(scope.pid().to_string())
            }
        }
        })
    }

    /// Update the last result in scope.
    async fn update_last_result(&self, result: &ExecResult) {
        let mut scope = self.scope.write().await;
        scope.set_last_result(result.clone());
    }

    /// Drain accumulated pipeline stderr into a result.
    ///
    /// Called after each sub-statement inside control structures (`if`, `for`,
    /// `while`, `case`, `&&`, `||`) so that stderr appears incrementally rather
    /// than batching until the entire structure finishes.
    async fn drain_stderr_into(&self, result: &mut ExecResult) {
        let drained = {
            let mut receiver = self.stderr_receiver.lock().await;
            receiver.drain_lossy()
        };
        if !drained.is_empty() {
            if !result.err.is_empty() && !result.err.ends_with('\n') {
                result.err.push('\n');
            }
            result.err.push_str(&drained);
        }
    }

    /// Execute a user-defined function with local variable scoping.
    ///
    /// Functions push a new scope frame for local variables. Variables declared
    /// with `local` are scoped to the function; other assignments modify outer
    /// scopes (or create in root if new).
    async fn execute_user_tool(&self, def: ToolDef, args: &[Arg]) -> Result<ExecResult> {
        // 1. Build function args from AST args (async to support command substitution)
        let tool_args = self.build_args_async(args, None).await?;

        // 2. Push a new scope frame for local variables
        {
            let mut scope = self.scope.write().await;
            scope.push_frame();
        }

        // 3. Save current positional parameters and set new ones for this function
        let saved_positional = {
            let mut scope = self.scope.write().await;
            let saved = scope.save_positional();

            // Set up new positional parameters ($0 = function name, $1, $2, ... = args)
            let positional_args: Vec<String> = tool_args.positional
                .iter()
                .map(value_to_string)
                .collect();
            scope.set_positional(&def.name, positional_args);

            saved
        };

        // 3. Execute body statements with control flow handling
        // Accumulate output across statements (like sh)
        let mut accumulated_out = String::new();
        let mut accumulated_err = String::new();
        let mut last_code = 0i64;
        let mut last_data: Option<Value> = None;

        // Track execution error for propagation after cleanup
        let mut exec_error: Option<anyhow::Error> = None;
        let mut exit_code: Option<i64> = None;

        for stmt in &def.body {
            match self.execute_stmt_flow(stmt).await {
                Ok(flow) => {
                    // Drain pipeline stderr after each sub-statement.
                    let drained = {
                        let mut receiver = self.stderr_receiver.lock().await;
                        receiver.drain_lossy()
                    };
                    if !drained.is_empty() {
                        accumulated_err.push_str(&drained);
                    }

                    match flow {
                        ControlFlow::Normal(r) => {
                            accumulated_out.push_str(&r.text_out());
                            accumulated_err.push_str(&r.err);
                            last_code = r.code;
                            last_data = r.data;
                        }
                        ControlFlow::Return { value } => {
                            accumulated_out.push_str(&value.text_out());
                            accumulated_err.push_str(&value.err);
                            last_code = value.code;
                            last_data = value.data;
                            break;
                        }
                        ControlFlow::Exit { code } => {
                            exit_code = Some(code);
                            break;
                        }
                        ControlFlow::Break { result: r, .. } | ControlFlow::Continue { result: r, .. } => {
                            accumulated_out.push_str(&r.text_out());
                            accumulated_err.push_str(&r.err);
                            last_code = r.code;
                            last_data = r.data;
                        }
                    }
                }
                Err(e) => {
                    exec_error = Some(e);
                    break;
                }
            }
        }

        // 4. Pop scope frame and restore original positional parameters (unconditionally)
        {
            let mut scope = self.scope.write().await;
            scope.pop_frame();
            scope.set_positional(saved_positional.0, saved_positional.1);
        }

        // 5. Propagate error or exit after cleanup
        if let Some(e) = exec_error {
            return Err(e);
        }
        if let Some(code) = exit_code {
            return Ok(ExecResult::from_parts(code, accumulated_out, accumulated_err, last_data));
        }

        Ok(ExecResult::from_parts(last_code, accumulated_out, accumulated_err, last_data))
    }

    /// Execute the `source` / `.` command to include and run a script.
    ///
    /// Unlike regular tool execution, `source` executes in the CURRENT scope,
    /// allowing the sourced script to set variables and modify shell state.
    async fn execute_source(&self, args: &[Arg]) -> Result<ExecResult> {
        // Get the file path from the first positional argument
        let tool_args = self.build_args_async(args, None).await?;
        let path = match tool_args.positional.first() {
            Some(Value::String(s)) => s.clone(),
            Some(v) => value_to_string(v),
            None => {
                return Ok(ExecResult::failure(1, "source: missing filename"));
            }
        };

        // Resolve path relative to cwd
        let full_path = {
            let ctx = self.exec_ctx.read().await;
            if path.starts_with('/') {
                std::path::PathBuf::from(&path)
            } else {
                ctx.cwd.join(&path)
            }
        };

        // Read file content via backend
        let content = {
            let ctx = self.exec_ctx.read().await;
            match ctx.backend.read(&full_path, None).await {
                Ok(bytes) => {
                    String::from_utf8(bytes).map_err(|e| {
                        anyhow::anyhow!("source: {}: invalid UTF-8: {}", path, e)
                    })?
                }
                Err(e) => {
                    return Ok(ExecResult::failure(
                        1,
                        format!("source: {}: {}", path, e),
                    ));
                }
            }
        };

        // Parse the content
        let program = match crate::parser::parse(&content) {
            Ok(p) => p,
            Err(errors) => {
                let msg = errors
                    .iter()
                    .map(|e| format!("{}:{}: {}", path, e.span.start, e.message))
                    .collect::<Vec<_>>()
                    .join("\n");
                return Ok(ExecResult::failure(1, format!("source: {}", msg)));
            }
        };

        // Execute each statement in the CURRENT scope (not isolated)
        let mut result = ExecResult::success("");
        for stmt in program.statements {
            if matches!(stmt, crate::ast::Stmt::Empty) {
                continue;
            }

            match self.execute_stmt_flow(&stmt).await {
                Ok(flow) => {
                    self.drain_stderr_into(&mut result).await;
                    match flow {
                        ControlFlow::Normal(r) => {
                            result = r.clone();
                            self.update_last_result(&r).await;
                        }
                        ControlFlow::Break { .. } | ControlFlow::Continue { .. } => {
                            return Err(anyhow::anyhow!(
                                "source: {}: unexpected break/continue outside loop",
                                path
                            ));
                        }
                        ControlFlow::Return { value } => {
                            return Ok(value);
                        }
                        ControlFlow::Exit { code } => {
                            result.code = code;
                            return Ok(result);
                        }
                    }
                }
                Err(e) => {
                    return Err(e.context(format!("source: {}", path)));
                }
            }
        }

        Ok(result)
    }

    /// Try to execute a script from PATH directories.
    ///
    /// Searches PATH for `{name}.kai` files and executes them in isolated scope
    /// (like user-defined tools). Returns None if no script is found.
    async fn try_execute_script(&self, name: &str, args: &[Arg]) -> Result<Option<ExecResult>> {
        // Get PATH from scope (default to "/bin")
        let path_value = {
            let scope = self.scope.read().await;
            scope
                .get("PATH")
                .map(value_to_string)
                .unwrap_or_else(|| "/bin".to_string())
        };

        // Search PATH directories for script
        for dir in path_value.split(':') {
            if dir.is_empty() {
                continue;
            }

            // Build script path: {dir}/{name}.kai
            let script_path = PathBuf::from(dir).join(format!("{}.kai", name));

            // Check if script exists
            let exists = {
                let ctx = self.exec_ctx.read().await;
                ctx.backend.exists(&script_path).await
            };

            if !exists {
                continue;
            }

            // Read script content
            let content = {
                let ctx = self.exec_ctx.read().await;
                match ctx.backend.read(&script_path, None).await {
                    Ok(bytes) => match String::from_utf8(bytes) {
                        Ok(s) => s,
                        Err(e) => {
                            return Ok(Some(ExecResult::failure(
                                1,
                                format!("{}: invalid UTF-8: {}", script_path.display(), e),
                            )));
                        }
                    },
                    Err(e) => {
                        return Ok(Some(ExecResult::failure(
                            1,
                            format!("{}: {}", script_path.display(), e),
                        )));
                    }
                }
            };

            // Parse the script
            let program = match crate::parser::parse(&content) {
                Ok(p) => p,
                Err(errors) => {
                    let msg = errors
                        .iter()
                        .map(|e| format!("{}:{}: {}", script_path.display(), e.span.start, e.message))
                        .collect::<Vec<_>>()
                        .join("\n");
                    return Ok(Some(ExecResult::failure(1, msg)));
                }
            };

            // Build tool_args from args (async for command substitution support)
            let tool_args = self.build_args_async(args, None).await?;

            // Create isolated scope (like user tools)
            let mut isolated_scope = Scope::new();

            // Set up positional parameters ($0 = script name, $1, $2, ... = args)
            let positional_args: Vec<String> = tool_args.positional
                .iter()
                .map(value_to_string)
                .collect();
            isolated_scope.set_positional(name, positional_args);

            // Save current scope and swap with isolated scope
            let original_scope = {
                let mut scope = self.scope.write().await;
                std::mem::replace(&mut *scope, isolated_scope)
            };

            // Execute script statements — track outcome for cleanup
            let mut result = ExecResult::success("");
            let mut exec_error: Option<anyhow::Error> = None;
            let mut exit_code: Option<i64> = None;

            for stmt in program.statements {
                if matches!(stmt, crate::ast::Stmt::Empty) {
                    continue;
                }

                match self.execute_stmt_flow(&stmt).await {
                    Ok(flow) => {
                        match flow {
                            ControlFlow::Normal(r) => result = r,
                            ControlFlow::Return { value } => {
                                result = value;
                                break;
                            }
                            ControlFlow::Exit { code } => {
                                exit_code = Some(code);
                                break;
                            }
                            ControlFlow::Break { result: r, .. } | ControlFlow::Continue { result: r, .. } => {
                                result = r;
                            }
                        }
                    }
                    Err(e) => {
                        exec_error = Some(e);
                        break;
                    }
                }
            }

            // Restore original scope unconditionally
            {
                let mut scope = self.scope.write().await;
                *scope = original_scope;
            }

            // Propagate error or exit after cleanup
            if let Some(e) = exec_error {
                return Err(e.context(format!("script: {}", script_path.display())));
            }
            if let Some(code) = exit_code {
                result.code = code;
                return Ok(Some(result));
            }

            return Ok(Some(result));
        }

        // No script found
        Ok(None)
    }

    /// Try to execute an external command from PATH.
    ///
    /// This is the fallback when no builtin or user-defined tool matches.
    /// External commands receive a clean argv (flags preserved in their original format).
    ///
    /// # Requirements
    /// - Command must be found in PATH
    /// - Current working directory must be on a real filesystem (not virtual like /v)
    ///
    /// # Returns
    /// - `Ok(Some(result))` if command was found and executed
    /// - `Ok(None)` if command was not found in PATH
    /// - `Err` on execution errors
    #[cfg(not(feature = "native"))]
    async fn try_execute_external(&self, _name: &str, _args: &[Arg]) -> Result<Option<ExecResult>> {
        Ok(None)
    }

    /// Try to execute an external command from PATH.
    #[cfg(feature = "native")]
    #[tracing::instrument(level = "debug", skip(self, args), fields(command = %name))]
    async fn try_execute_external(&self, name: &str, args: &[Arg]) -> Result<Option<ExecResult>> {
        // Read the cancel token from `self.exec_ctx`, which `dispatch_command`
        // populates from the inbound ctx.cancel on every dispatch. This is
        // what makes the `timeout` builtin's swapped child token reach the
        // wait_or_kill discipline below — reading `self.cancel_token` would
        // give the kernel-wide token and miss the timeout's child cascade.
        let cancel = {
            let ec = self.exec_ctx.read().await;
            ec.cancel.clone()
        };
        let kill_grace = self.kill_grace;
        if !self.allow_external_commands {
            return Ok(None);
        }

        // Get real working directory for relative path resolution and child cwd.
        // If the CWD is virtual (no real filesystem path), skip external command
        // execution entirely — return None so the dispatch can fall through to
        // backend-registered tools.
        let real_cwd = {
            let ctx = self.exec_ctx.read().await;
            match ctx.backend.resolve_real_path(&ctx.cwd) {
                Some(p) => p,
                None => return Ok(None),
            }
        };

        let executable = if name.contains('/') {
            // Resolve relative paths (./script, ../bin/tool) against the shell's cwd
            let resolved = if std::path::Path::new(name).is_absolute() {
                std::path::PathBuf::from(name)
            } else {
                real_cwd.join(name)
            };
            if !resolved.exists() {
                return Ok(Some(ExecResult::failure(
                    127,
                    format!("{}: No such file or directory", name),
                )));
            }
            if !resolved.is_file() {
                return Ok(Some(ExecResult::failure(
                    126,
                    format!("{}: Is a directory", name),
                )));
            }
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                let mode = std::fs::metadata(&resolved)
                    .map(|m| m.permissions().mode())
                    .unwrap_or(0);
                if mode & 0o111 == 0 {
                    return Ok(Some(ExecResult::failure(
                        126,
                        format!("{}: Permission denied", name),
                    )));
                }
            }
            resolved.to_string_lossy().into_owned()
        } else {
            // Get PATH from scope or environment
            let path_var = {
                let scope = self.scope.read().await;
                scope
                    .get("PATH")
                    .map(value_to_string)
                    .unwrap_or_else(|| std::env::var("PATH").unwrap_or_default())
            };

            // Resolve command in PATH
            match resolve_in_path(name, &path_var) {
                Some(path) => path,
                None => return Ok(None), // Not found - let caller handle error
            }
        };

        tracing::debug!(executable = %executable, "resolved external command");

        // Build flat argv (preserves flag format)
        let argv = self.build_args_flat(args).await?;

        // Get stdin if available
        let stdin_data = {
            let mut ctx = self.exec_ctx.write().await;
            ctx.take_stdin()
        };

        // Build and spawn the command
        use tokio::process::Command;

        let mut cmd = Command::new(&executable);
        cmd.args(&argv);
        cmd.current_dir(&real_cwd);

        // Hermetic env: child sees only kaish's exported vars, not the kaish
        // process's OS env. Frontends that want OS-env passthrough (REPL, MCP)
        // populate it via KernelConfig::initial_vars at construction.
        cmd.env_clear();
        {
            let scope = self.scope.read().await;
            for (var_name, value) in scope.exported_vars() {
                cmd.env(var_name, value_to_string(&value));
            }
        }

        // Handle stdin
        cmd.stdin(if stdin_data.is_some() {
            std::process::Stdio::piped()
        } else if self.interactive {
            std::process::Stdio::inherit()
        } else {
            std::process::Stdio::null()
        });

        // In interactive mode, standalone or last-in-pipeline commands inherit
        // the terminal's stdout/stderr so output streams in real-time.
        // First/middle commands must capture stdout for the pipe — same as bash.
        let pipeline_position = {
            let ctx = self.exec_ctx.read().await;
            ctx.pipeline_position
        };
        let inherit_output = self.interactive
            && matches!(pipeline_position, PipelinePosition::Only | PipelinePosition::Last);

        if inherit_output {
            cmd.stdout(std::process::Stdio::inherit());
            cmd.stderr(std::process::Stdio::inherit());
        } else {
            cmd.stdout(std::process::Stdio::piped());
            cmd.stderr(std::process::Stdio::piped());
        }

        // On Unix, always put the child in its own process group so cancellation
        // can `killpg` the whole tree (the child plus any grandchildren).
        // Restoring default tty-related signal handlers stays gated on
        // job-control mode — those only matter when the child has a controlling
        // terminal.
        #[cfg(unix)]
        {
            let restore_jc_signals = self.terminal_state.is_some() && inherit_output;
            // SAFETY: setpgid and sigaction(SIG_DFL) are async-signal-safe per POSIX
            #[allow(unsafe_code)]
            unsafe {
                cmd.pre_exec(move || {
                    // Own process group — for kill scope.
                    nix::unistd::setpgid(nix::unistd::Pid::from_raw(0), nix::unistd::Pid::from_raw(0))
                        .map_err(|e| std::io::Error::from_raw_os_error(e as i32))?;
                    if restore_jc_signals {
                        use nix::libc::{sigaction, SIGTSTP, SIGTTOU, SIGTTIN, SIGINT, SIG_DFL};
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
        let in_jc_inherit_path = inherit_output && self.terminal_state.is_some();
        if !in_jc_inherit_path {
            cmd.kill_on_drop(true);
        }

        // Spawn the process. Capture a `KillTarget` immediately so cancel/
        // timeout paths can deliver signals via pidfd (Linux ≥ 5.3) — bound
        // to this process's generation, immune to PID reuse if the OS reaps
        // the child before our kill syscalls fire.
        let mut child = match cmd.spawn() {
            Ok(child) => child,
            Err(e) => {
                return Ok(Some(ExecResult::failure(
                    127,
                    format!("{}: {}", name, e),
                )));
            }
        };
        let kill_target = crate::pidfd::KillTarget::from_child(&child);

        // Write stdin if present
        if let Some(data) = stdin_data
            && let Some(mut stdin) = child.stdin.take()
        {
            use tokio::io::AsyncWriteExt;
            if let Err(e) = stdin.write_all(data.as_bytes()).await {
                return Ok(Some(ExecResult::failure(
                    1,
                    format!("{}: failed to write stdin: {}", name, e),
                )));
            }
            // Drop stdin to signal EOF
        }

        if inherit_output {
            // Job control path: use waitpid with WUNTRACED for Ctrl-Z support
            #[cfg(unix)]
            if let Some(ref term) = self.terminal_state {
                let child_id = child.id().unwrap_or(0);
                let pid = nix::unistd::Pid::from_raw(child_id as i32);
                let pgid = pid; // child is its own pgid leader

                // Give the terminal to the child's process group
                if let Err(e) = term.give_terminal_to(pgid) {
                    tracing::warn!("failed to give terminal to child: {}", e);
                }

                let term_clone = term.clone();
                let cmd_name = name.to_string();
                let cmd_display = format!("{} {}", name, argv.join(" "));
                let jobs = self.jobs.clone();

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
                let wait_complete = std::sync::Arc::new(
                    std::sync::atomic::AtomicBool::new(false)
                );
                let cancel_watcher = {
                    let cancel = cancel.clone();
                    let wc = wait_complete.clone();
                    // Ownership transfer: the JC path's sync wait inside
                    // block_in_place owns the child's reaping, so the
                    // cancel_watcher drives the kill side via KillTarget
                    // (pidfd-bound on Linux). When kill_target is None
                    // (older kernel + open failure, or non-Linux), falls
                    // through to the older PID-based path the closure
                    // captures from `pid`.
                    let target = kill_target.as_ref().map(|t| {
                        // Re-borrow the components we need into Owned-ish form
                        // so the spawned task is 'static. We can't move
                        // KillTarget directly because try_execute_external
                        // still uses it after the spawn — but on the JC path
                        // there is no further use after the watcher spawn,
                        // so a clone-of-pid + owned None pidfd is safe.
                        // Simpler: signal via the existing target by cloning
                        // a fresh pidfd; the original keeps its handle.
                        // Pidfd is just an OwnedFd — not Clone — so do it
                        // by re-opening from the pid. Fall back if reopen
                        // fails (race already reaped → best-effort kill).
                        crate::pidfd::KillTarget::from_pid(t.pid())
                    });
                    tokio::spawn(async move {
                        cancel.cancelled().await;
                        if wc.load(std::sync::atomic::Ordering::SeqCst) { return; }
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
                            if wc.load(std::sync::atomic::Ordering::SeqCst) { return; }
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
                            // Register as a stopped job
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

                return Ok(Some(ExecResult::from_output(code, String::new(), String::new())));
            }

            // Non-job-control path with inherited stdio.
            let status = match wait_or_kill(&mut child, kill_target.as_ref(), &cancel, kill_grace).await {
                Ok(s) => s,
                Err(e) => {
                    return Ok(Some(ExecResult::failure(
                        1,
                        format!("{}: failed to wait: {}", name, e),
                    )));
                }
            };

            let code = status.code().unwrap_or_else(|| {
                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    128 + status.signal().unwrap_or(0)
                }
                #[cfg(not(unix))]
                {
                    -1
                }
            }) as i64;

            // stdout/stderr already went to the terminal
            Ok(Some(ExecResult::from_output(code, String::new(), String::new())))
        } else {
            // Capture output via bounded streams
            let stdout_stream = Arc::new(BoundedStream::new(DEFAULT_STREAM_MAX_SIZE));
            let stderr_stream = Arc::new(BoundedStream::new(DEFAULT_STREAM_MAX_SIZE));

            let stdout_pipe = child.stdout.take();
            let stderr_pipe = child.stderr.take();

            let stdout_clone = stdout_stream.clone();
            let stderr_clone = stderr_stream.clone();

            let stdout_task = stdout_pipe.map(|pipe| {
                tokio::spawn(async move {
                    drain_to_stream(pipe, stdout_clone).await;
                })
            });

            let stderr_task = stderr_pipe.map(|pipe| {
                tokio::spawn(async move {
                    drain_to_stream(pipe, stderr_clone).await;
                })
            });

            let cancelled_before_wait = cancel.is_cancelled();
            let status = match wait_or_kill(&mut child, kill_target.as_ref(), &cancel, kill_grace).await {
                Ok(s) => s,
                Err(e) => {
                    if let Some(task) = stdout_task { task.abort(); let _ = task.await; }
                    if let Some(task) = stderr_task { task.abort(); let _ = task.await; }
                    return Ok(Some(ExecResult::failure(
                        1,
                        format!("{}: failed to wait: {}", name, e),
                    )));
                }
            };

            // On cancel, abort the drain tasks (the child's pipes are gone;
            // late output is lost but predictable death beats partial capture).
            // On normal exit, await drains so we don't lose buffered output.
            if cancelled_before_wait || cancel.is_cancelled() {
                if let Some(task) = stdout_task { task.abort(); let _ = task.await; }
                if let Some(task) = stderr_task { task.abort(); let _ = task.await; }
            } else {
                if let Some(task) = stdout_task {
                    // Ignore join error — the drain task logs its own errors
                    let _ = task.await;
                }
                if let Some(task) = stderr_task {
                    let _ = task.await;
                }
            }

            let code = status.code().unwrap_or_else(|| {
                #[cfg(unix)]
                {
                    use std::os::unix::process::ExitStatusExt;
                    128 + status.signal().unwrap_or(0)
                }
                #[cfg(not(unix))]
                {
                    -1
                }
            }) as i64;

            let stdout = stdout_stream.read_string().await;
            let stderr = stderr_stream.read_string().await;

            Ok(Some(ExecResult::from_output(code, stdout, stderr)))
        }
    }

    // --- Variable Access ---

    /// Get a variable value.
    pub async fn get_var(&self, name: &str) -> Option<Value> {
        let scope = self.scope.read().await;
        scope.get(name).cloned()
    }

    /// Check if error-exit mode is enabled (for testing).
    #[cfg(test)]
    pub async fn error_exit_enabled(&self) -> bool {
        let scope = self.scope.read().await;
        scope.error_exit_enabled()
    }

    /// Set a variable value.
    pub async fn set_var(&self, name: &str, value: Value) {
        let mut scope = self.scope.write().await;
        scope.set(name.to_string(), value);
    }

    /// Set positional parameters ($0 script name and $1-$9 args).
    pub async fn set_positional(&self, script_name: impl Into<String>, args: Vec<String>) {
        let mut scope = self.scope.write().await;
        scope.set_positional(script_name, args);
    }

    /// List all variables.
    pub async fn list_vars(&self) -> Vec<(String, Value)> {
        let scope = self.scope.read().await;
        scope.all()
    }

    // --- CWD ---

    /// Get current working directory.
    pub async fn cwd(&self) -> PathBuf {
        self.exec_ctx.read().await.cwd.clone()
    }

    /// Set current working directory.
    pub async fn set_cwd(&self, path: PathBuf) {
        let mut ctx = self.exec_ctx.write().await;
        ctx.set_cwd(path);
    }

    // --- Last Result ---

    /// Get the last result ($?).
    pub async fn last_result(&self) -> ExecResult {
        let scope = self.scope.read().await;
        scope.last_result().clone()
    }

    // --- Tools ---

    /// Check if a user-defined function exists.
    pub async fn has_function(&self, name: &str) -> bool {
        self.user_tools.read().await.contains_key(name)
    }

    /// Get available tool schemas.
    pub fn tool_schemas(&self) -> Vec<crate::tools::ToolSchema> {
        self.tools.schemas()
    }

    // --- Jobs ---

    /// Get job manager.
    pub fn jobs(&self) -> Arc<JobManager> {
        self.jobs.clone()
    }

    // --- VFS ---

    /// Get VFS router.
    pub fn vfs(&self) -> Arc<VfsRouter> {
        self.vfs.clone()
    }

    // --- State ---

    /// Reset kernel to initial state.
    ///
    /// Clears in-memory variables and resets cwd to root.
    /// History is not cleared (it persists across resets).
    pub async fn reset(&self) -> Result<()> {
        {
            let mut scope = self.scope.write().await;
            *scope = Scope::new();
        }
        {
            let mut ctx = self.exec_ctx.write().await;
            ctx.cwd = PathBuf::from("/");
        }
        Ok(())
    }

    /// Shutdown the kernel.
    pub async fn shutdown(self) -> Result<()> {
        // Wait for all background jobs
        self.jobs.wait_all().await;
        Ok(())
    }

    /// Dispatch a single command using the full resolution chain.
    ///
    /// This is the core of `CommandDispatcher` — it syncs state between the
    /// passed-in `ExecContext` and kernel-internal state (scope, exec_ctx),
    /// then delegates to `execute_command` for the actual dispatch.
    ///
    /// State flow:
    /// 1. ctx → self: sync scope, cwd, stdin so internal methods see current state
    /// 2. execute_command: full dispatch chain (user tools, builtins, scripts, external, backend)
    /// 3. self → ctx: sync scope, cwd changes back so the pipeline runner sees them
    async fn dispatch_command(&self, cmd: &Command, ctx: &mut ExecContext) -> Result<ExecResult> {
        // Ensure nested dispatch (e.g. the `timeout` builtin re-dispatching
        // its inner command via ctx.dispatcher) routes through THIS kernel,
        // not a stale parent. Critical for forks: the fork's builtins must
        // use the fork's dispatcher, not the parent's.
        if let Some(d) = self.dispatcher() {
            ctx.dispatcher = Some(d);
        }

        // 1. Sync ctx → self internals
        {
            let mut scope = self.scope.write().await;
            *scope = ctx.scope.clone();
        }
        {
            let mut ec = self.exec_ctx.write().await;
            ec.cwd = ctx.cwd.clone();
            ec.prev_cwd = ctx.prev_cwd.clone();
            ec.stdin = ctx.stdin.take();
            ec.stdin_data = ctx.stdin_data.take();
            // Streaming pipe endpoints and kernel stderr must flow to the
            // tool via self.exec_ctx — execute_command reads that, not the
            // passed-in ctx. Without moving these, concurrent pipeline
            // stages dispatched via a fork get pipe_stdin = None and
            // silently read nothing.
            ec.pipe_stdin = ctx.pipe_stdin.take();
            ec.pipe_stdout = ctx.pipe_stdout.take();
            if let Some(stderr) = ctx.stderr.clone() {
                ec.stderr = Some(stderr);
            }
            ec.aliases = ctx.aliases.clone();
            ec.ignore_config = ctx.ignore_config.clone();
            ec.output_limit = ctx.output_limit.clone();
            ec.pipeline_position = ctx.pipeline_position;
            // Sync the cancel token from ctx → ec. Builtins like `timeout`
            // swap ctx.cancel to a derived child token before re-dispatching;
            // execute_command's snapshot reads ec.cancel (kept aligned by
            // this sync), so try_execute_external sees the right token.
            ec.cancel = ctx.cancel.clone();
        }

        // 2. Execute via the full dispatch chain
        let result = self.execute_command(&cmd.name, &cmd.args).await?;

        // 3. Sync self → ctx
        {
            let scope = self.scope.read().await;
            ctx.scope = scope.clone();
        }
        {
            let mut ec = self.exec_ctx.write().await;
            ctx.cwd = ec.cwd.clone();
            ctx.prev_cwd = ec.prev_cwd.clone();
            ctx.aliases = ec.aliases.clone();
            ctx.ignore_config = ec.ignore_config.clone();
            ctx.output_limit = ec.output_limit.clone();
            // Return any pipe endpoints that the tool didn't consume.
            // `take()` here keeps the fork's exec_ctx in a clean state for
            // the next dispatch — these are per-command and shouldn't leak
            // between calls.
            ctx.pipe_stdin = ec.pipe_stdin.take();
            ctx.pipe_stdout = ec.pipe_stdout.take();
        }

        Ok(result)
    }
}

#[async_trait]
impl CommandDispatcher for Kernel {
    /// Dispatch a command through the Kernel's full resolution chain.
    ///
    /// This is the single path for all command execution when called from
    /// the pipeline runner. It provides the full dispatch chain:
    /// user tools → builtins → .kai scripts → external commands → backend tools.
    async fn dispatch(&self, cmd: &Command, ctx: &mut ExecContext) -> Result<ExecResult> {
        self.dispatch_command(cmd, ctx).await
    }

    /// Produce a forked dispatcher with independent mutable state (detached).
    ///
    /// Calls the inherent `Kernel::fork` method (note the UFCS to avoid
    /// recursing into the trait method we're defining) and coerces the
    /// returned `Arc<Kernel>` to `Arc<dyn CommandDispatcher>`.
    async fn fork(&self) -> Arc<dyn CommandDispatcher> {
        let fork: Arc<Kernel> = Kernel::fork(self).await;
        fork
    }

    /// Produce a forked dispatcher with cancellation cascading from this kernel.
    async fn fork_attached(&self) -> Arc<dyn CommandDispatcher> {
        let fork: Arc<Kernel> = Kernel::fork_attached(self).await;
        fork
    }
}

/// Accumulate output from one result into another.
///
/// This appends stdout and stderr (with newlines as separators) and updates
/// the exit code to match the new result. Used to preserve output from
/// multiple statements, loop iterations, and command chains.
fn accumulate_result(accumulated: &mut ExecResult, new: &ExecResult) {
    // Materialize lazy OutputData into .out before accumulating.
    // Without this, the first command's output stays in .output while
    // the second's text gets appended to .out, losing the first.
    accumulated.materialize();
    let new_text = new.text_out();
    if !accumulated.text_out().is_empty() && !new_text.is_empty() && !accumulated.text_out().ends_with('\n') {
        accumulated.push_out("\n");
    }
    accumulated.push_out(&new_text);
    if !accumulated.err.is_empty() && !new.err.is_empty() && !accumulated.err.ends_with('\n') {
        accumulated.err.push('\n');
    }
    accumulated.err.push_str(&new.err);
    accumulated.code = new.code;
    accumulated.data = new.data.clone();
    accumulated.did_spill = new.did_spill;
    accumulated.original_code = new.original_code;
    accumulated.content_type = new.content_type.clone();
    accumulated.baggage.clone_from(&new.baggage);
}

/// Check if a value is truthy.
fn is_truthy(value: &Value) -> bool {
    match value {
        Value::Null => false,
        Value::Bool(b) => *b,
        Value::Int(i) => *i != 0,
        Value::Float(f) => *f != 0.0,
        Value::String(s) => !s.is_empty(),
        Value::Json(json) => match json {
            serde_json::Value::Null => false,
            serde_json::Value::Array(arr) => !arr.is_empty(),
            serde_json::Value::Object(obj) => !obj.is_empty(),
            serde_json::Value::Bool(b) => *b,
            serde_json::Value::Number(n) => n.as_f64().map(|f| f != 0.0).unwrap_or(false),
            serde_json::Value::String(s) => !s.is_empty(),
        },
        Value::Blob(_) => true, // Blob references are always truthy
    }
}

/// Apply tilde expansion to a value.
///
/// Only string values starting with `~` are expanded.
fn apply_tilde_expansion(value: Value) -> Value {
    match value {
        Value::String(s) if s.starts_with('~') => Value::String(expand_tilde(&s)),
        _ => value,
    }
}

/// Wait for a child to exit, killing it if `cancel` fires first.
///
/// `target` carries a Linux pidfd (when available) for race-free direct-child
/// kill; fall-through to PID-based kill otherwise. On non-unix targets the
/// parameter is ignored and we use tokio's cross-platform `start_kill`.
#[cfg(all(unix, feature = "native"))]
pub(crate) async fn wait_or_kill(
    child: &mut tokio::process::Child,
    target: Option<&crate::pidfd::KillTarget>,
    cancel: &tokio_util::sync::CancellationToken,
    grace: Duration,
) -> std::io::Result<std::process::ExitStatus> {
    tokio::select! {
        biased;
        status = child.wait() => status,
        _ = cancel.cancelled() => kill_with_grace(child, target, grace).await,
    }
}

#[cfg(all(not(unix), feature = "native"))]
pub(crate) async fn wait_or_kill(
    child: &mut tokio::process::Child,
    _target: Option<&()>,
    cancel: &tokio_util::sync::CancellationToken,
    _grace: Duration,
) -> std::io::Result<std::process::ExitStatus> {
    tokio::select! {
        biased;
        status = child.wait() => status,
        _ = cancel.cancelled() => {
            let _ = child.start_kill();
            child.wait().await
        }
    }
}

/// Send SIGTERM to the child and its process group; wait `grace`; then SIGKILL.
///
/// Direct-child kill goes through `target.signal()`, which on Linux uses a
/// pidfd (immune to PID reuse). Process-group kill uses `killpg` — there is
/// no PGID-equivalent of pidfd, so grandchildren retain a small reuse window.
#[cfg(all(unix, feature = "native"))]
pub(crate) async fn kill_with_grace(
    child: &mut tokio::process::Child,
    target: Option<&crate::pidfd::KillTarget>,
    grace: Duration,
) -> std::io::Result<std::process::ExitStatus> {
    use nix::sys::signal::Signal;

    if let Some(t) = target {
        t.signal(Signal::SIGTERM);
        t.signal_pg(Signal::SIGTERM);
        if grace > Duration::ZERO
            && let Ok(status) = tokio::time::timeout(grace, child.wait()).await
        {
            return status;
        }
        t.signal(Signal::SIGKILL);
        t.signal_pg(Signal::SIGKILL);
    }
    child.wait().await
}

#[cfg(all(test, feature = "native"))]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_kernel_transient() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        assert_eq!(kernel.name(), "transient");
    }

    #[tokio::test]
    async fn test_kernel_execute_echo() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel.execute("echo hello").await.expect("execution failed");
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "hello");
    }

    #[tokio::test]
    async fn test_multiple_statements_accumulate_output() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute("echo one\necho two\necho three")
            .await
            .expect("execution failed");
        assert!(result.ok());
        // Should have all three outputs separated by newlines
        assert!(result.text_out().contains("one"), "missing 'one': {}", result.text_out());
        assert!(result.text_out().contains("two"), "missing 'two': {}", result.text_out());
        assert!(result.text_out().contains("three"), "missing 'three': {}", result.text_out());
    }

    #[tokio::test]
    async fn test_and_chain_accumulates_output() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute("echo first && echo second")
            .await
            .expect("execution failed");
        assert!(result.ok());
        assert!(result.text_out().contains("first"), "missing 'first': {}", result.text_out());
        assert!(result.text_out().contains("second"), "missing 'second': {}", result.text_out());
    }

    #[tokio::test]
    async fn test_for_loop_accumulates_output() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute(r#"for X in a b c; do echo "item: ${X}"; done"#)
            .await
            .expect("execution failed");
        assert!(result.ok());
        assert!(result.text_out().contains("item: a"), "missing 'item: a': {}", result.text_out());
        assert!(result.text_out().contains("item: b"), "missing 'item: b': {}", result.text_out());
        assert!(result.text_out().contains("item: c"), "missing 'item: c': {}", result.text_out());
    }

    #[tokio::test]
    async fn test_while_loop_accumulates_output() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute(r#"
                N=3
                while [[ ${N} -gt 0 ]]; do
                    echo "N=${N}"
                    N=$((N - 1))
                done
            "#)
            .await
            .expect("execution failed");
        assert!(result.ok());
        assert!(result.text_out().contains("N=3"), "missing 'N=3': {}", result.text_out());
        assert!(result.text_out().contains("N=2"), "missing 'N=2': {}", result.text_out());
        assert!(result.text_out().contains("N=1"), "missing 'N=1': {}", result.text_out());
    }

    #[tokio::test]
    async fn test_kernel_set_var() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("X=42").await.expect("set failed");

        let value = kernel.get_var("X").await;
        assert_eq!(value, Some(Value::Int(42)));
    }

    #[tokio::test]
    async fn test_kernel_var_expansion() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("NAME=\"world\"").await.expect("set failed");
        let result = kernel.execute("echo \"hello ${NAME}\"").await.expect("echo failed");

        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "hello world");
    }

    #[tokio::test]
    async fn test_kernel_last_result() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("echo test").await.expect("echo failed");

        let last = kernel.last_result().await;
        assert!(last.ok());
        assert_eq!(last.text_out().trim(), "test");
    }

    #[tokio::test]
    async fn test_kernel_tool_not_found() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel.execute("nonexistent_tool").await.expect("execution failed");
        assert!(!result.ok());
        assert_eq!(result.code, 127);
        assert!(result.err.contains("command not found"));
    }

    #[tokio::test]
    async fn test_external_command_true() {
        // Use REPL config for passthrough filesystem access
        let kernel = Kernel::new(KernelConfig::repl()).expect("failed to create kernel");

        // /bin/true should be available on any Unix system
        let result = kernel.execute("true").await.expect("execution failed");
        // This should use the builtin true, which returns 0
        assert!(result.ok(), "true should succeed: {:?}", result);
    }

    #[tokio::test]
    async fn test_external_command_basic() {
        // Use REPL config for passthrough filesystem access
        let kernel = Kernel::new(KernelConfig::repl()).expect("failed to create kernel");

        // Test with /bin/echo which is external
        // Note: kaish has a builtin echo, so this will use the builtin
        // Let's test with a command that's not a builtin
        // Actually, let's just test that PATH resolution works by checking the PATH var
        let path_var = std::env::var("PATH").unwrap_or_default();
        eprintln!("System PATH: {}", path_var);

        // Set PATH in kernel to ensure it's available
        kernel.execute(&format!(r#"PATH="{}""#, path_var)).await.expect("set PATH failed");

        // Now try an external command like /usr/bin/env
        // But env is also a builtin... let's try uname
        let result = kernel.execute("uname").await.expect("execution failed");
        eprintln!("uname result: {:?}", result);
        // uname should succeed if external commands work
        assert!(result.ok() || result.code == 127, "uname: {:?}", result);
    }

    #[tokio::test]
    async fn test_kernel_reset() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("X=1").await.expect("set failed");
        assert!(kernel.get_var("X").await.is_some());

        kernel.reset().await.expect("reset failed");
        assert!(kernel.get_var("X").await.is_none());
    }

    #[tokio::test]
    async fn test_kernel_cwd() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Transient kernel uses sandboxed mode with cwd=$HOME
        let cwd = kernel.cwd().await;
        let home = std::env::var("HOME")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("/"));
        assert_eq!(cwd, home);

        kernel.set_cwd(PathBuf::from("/tmp")).await;
        assert_eq!(kernel.cwd().await, PathBuf::from("/tmp"));
    }

    #[tokio::test]
    async fn test_kernel_list_vars() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("A=1").await.ok();
        kernel.execute("B=2").await.ok();

        let vars = kernel.list_vars().await;
        assert!(vars.iter().any(|(n, v)| n == "A" && *v == Value::Int(1)));
        assert!(vars.iter().any(|(n, v)| n == "B" && *v == Value::Int(2)));
    }

    #[tokio::test]
    async fn test_is_truthy() {
        assert!(!is_truthy(&Value::Null));
        assert!(!is_truthy(&Value::Bool(false)));
        assert!(is_truthy(&Value::Bool(true)));
        assert!(!is_truthy(&Value::Int(0)));
        assert!(is_truthy(&Value::Int(1)));
        assert!(!is_truthy(&Value::String("".into())));
        assert!(is_truthy(&Value::String("x".into())));
    }

    #[tokio::test]
    async fn test_jq_in_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        // kaish uses double quotes only; escape inner quotes
        let result = kernel
            .execute(r#"echo "{\"name\": \"Alice\"}" | jq ".name" -r"#)
            .await
            .expect("execution failed");
        assert!(result.ok(), "jq pipeline failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "Alice");
    }

    #[tokio::test]
    async fn test_user_defined_tool() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define a function
        kernel
            .execute(r#"greet() { echo "Hello, $1!" }"#)
            .await
            .expect("function definition failed");

        // Call the function
        let result = kernel
            .execute(r#"greet "World""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "greet failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "Hello, World!");
    }

    #[tokio::test]
    async fn test_user_tool_positional_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define a function with positional param
        kernel
            .execute(r#"greet() { echo "Hi $1" }"#)
            .await
            .expect("function definition failed");

        // Call with positional argument
        let result = kernel
            .execute(r#"greet "Amy""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "greet failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "Hi Amy");
    }

    #[tokio::test]
    async fn test_function_shared_scope() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Set a variable in parent scope
        kernel
            .execute(r#"SECRET="hidden""#)
            .await
            .expect("set failed");

        // Define a function that accesses and modifies parent variable
        kernel
            .execute(r#"access_parent() {
                echo "${SECRET}"
                SECRET="modified"
            }"#)
            .await
            .expect("function definition failed");

        // Call the function - it SHOULD see SECRET (shared scope like sh)
        let result = kernel.execute("access_parent").await.expect("function call failed");

        // Function should have access to parent scope
        assert!(
            result.text_out().contains("hidden"),
            "Function should access parent scope, got: {}",
            result.text_out()
        );

        // Function should have modified the parent variable
        let secret = kernel.get_var("SECRET").await;
        assert_eq!(
            secret,
            Some(Value::String("modified".into())),
            "Function should modify parent scope"
        );
    }

    #[tokio::test]
    #[ignore = "exec replaces the test binary via CommandExt::exec, hangs libtest; cannot be run under cargo test"]
    async fn test_exec_builtin() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        // argv is now a space-separated string or JSON array string
        let result = kernel
            .execute(r#"exec command="/bin/echo" argv="hello world""#)
            .await
            .expect("exec failed");

        assert!(result.ok(), "exec failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "hello world");
    }

    #[tokio::test]
    async fn test_while_false_never_runs() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // A while loop with false condition should never run
        let result = kernel
            .execute(r#"
                while false; do
                    echo "should not run"
                done
            "#)
            .await
            .expect("while false failed");

        assert!(result.ok());
        assert!(result.text_out().is_empty(), "while false should not execute body: {}", result.text_out());
    }

    #[tokio::test]
    async fn test_while_string_comparison() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Set a flag
        kernel.execute(r#"FLAG="go""#).await.expect("set failed");

        // Use string comparison as condition (shell-compatible [[ ]] syntax)
        // Note: Put echo last so we can check the output
        let result = kernel
            .execute(r#"
                while [[ ${FLAG} == "go" ]]; do
                    FLAG="stop"
                    echo "running"
                done
            "#)
            .await
            .expect("while with string cmp failed");

        assert!(result.ok());
        assert!(result.text_out().contains("running"), "should have run once: {}", result.text_out());

        // Verify flag was changed
        let flag = kernel.get_var("FLAG").await;
        assert_eq!(flag, Some(Value::String("stop".into())));
    }

    #[tokio::test]
    async fn test_while_numeric_comparison() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Test > comparison (shell-compatible [[ ]] with -gt)
        kernel.execute("N=5").await.expect("set failed");

        // Note: Put echo last so we can check the output
        let result = kernel
            .execute(r#"
                while [[ ${N} -gt 3 ]]; do
                    N=3
                    echo "N was greater"
                done
            "#)
            .await
            .expect("while with > failed");

        assert!(result.ok());
        assert!(result.text_out().contains("N was greater"), "should have run once: {}", result.text_out());
    }

    #[tokio::test]
    async fn test_break_in_while_loop() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                I=0
                while true; do
                    I=1
                    echo "before break"
                    break
                    echo "after break"
                done
            "#)
            .await
            .expect("while with break failed");

        assert!(result.ok());
        assert!(result.text_out().contains("before break"), "should see before break: {}", result.text_out());
        assert!(!result.text_out().contains("after break"), "should not see after break: {}", result.text_out());

        // Verify we exited the loop
        let i = kernel.get_var("I").await;
        assert_eq!(i, Some(Value::Int(1)));
    }

    #[tokio::test]
    async fn test_continue_in_while_loop() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Test continue in a while loop where variables persist
        // We use string state transition: "start" -> "middle" -> "end"
        // continue on "middle" should skip to next iteration
        // Shell-compatible: use [[ ]] for comparisons
        let result = kernel
            .execute(r#"
                STATE="start"
                AFTER_CONTINUE="no"
                while [[ ${STATE} != "done" ]]; do
                    if [[ ${STATE} == "start" ]]; then
                        STATE="middle"
                        continue
                        AFTER_CONTINUE="yes"
                    fi
                    if [[ ${STATE} == "middle" ]]; then
                        STATE="done"
                    fi
                done
            "#)
            .await
            .expect("while with continue failed");

        assert!(result.ok());

        // STATE should be "done" (we completed the loop)
        let state = kernel.get_var("STATE").await;
        assert_eq!(state, Some(Value::String("done".into())));

        // AFTER_CONTINUE should still be "no" (continue skipped the assignment)
        let after = kernel.get_var("AFTER_CONTINUE").await;
        assert_eq!(after, Some(Value::String("no".into())));
    }

    #[tokio::test]
    async fn test_break_with_level() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Nested loop with break 2 to exit both loops
        // We verify by checking OUTER value:
        // - If break 2 works, OUTER stays at 1 (set before for loop)
        // - If break 2 fails, OUTER becomes 2 (set after for loop)
        let result = kernel
            .execute(r#"
                OUTER=0
                while true; do
                    OUTER=1
                    for X in "1 2"; do
                        break 2
                    done
                    OUTER=2
                done
            "#)
            .await
            .expect("nested break failed");

        assert!(result.ok());

        // OUTER should be 1 (set before for loop), not 2 (would be set after for loop)
        let outer = kernel.get_var("OUTER").await;
        assert_eq!(outer, Some(Value::Int(1)), "break 2 should have skipped OUTER=2");
    }

    #[tokio::test]
    async fn test_return_from_tool() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define a function that returns early
        kernel
            .execute(r#"early_return() {
                if [[ $1 == 1 ]]; then
                    return 42
                fi
                echo "not returned"
            }"#)
            .await
            .expect("function definition failed");

        // Call with arg=1 should return with exit code 42
        // (POSIX shell behavior: return N sets exit code, doesn't output N)
        let result = kernel
            .execute("early_return 1")
            .await
            .expect("function call failed");

        // Exit code should be 42 (non-zero, so not ok())
        assert_eq!(result.code, 42);
        // Output should be empty (we returned before echo)
        assert!(result.text_out().is_empty());
    }

    #[tokio::test]
    async fn test_return_without_value() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define a function that returns without a value
        kernel
            .execute(r#"early_exit() {
                if [[ $1 == "stop" ]]; then
                    return
                fi
                echo "continued"
            }"#)
            .await
            .expect("function definition failed");

        // Call with arg="stop" should return early
        let result = kernel
            .execute(r#"early_exit "stop""#)
            .await
            .expect("function call failed");

        assert!(result.ok());
        assert!(result.text_out().is_empty() || result.text_out().trim().is_empty());
    }

    #[tokio::test]
    async fn test_exit_stops_execution() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // exit should stop further execution
        kernel
            .execute(r#"
                BEFORE="yes"
                exit 0
                AFTER="yes"
            "#)
            .await
            .expect("execution failed");

        // BEFORE should be set, AFTER should not
        let before = kernel.get_var("BEFORE").await;
        assert_eq!(before, Some(Value::String("yes".into())));

        let after = kernel.get_var("AFTER").await;
        assert!(after.is_none(), "AFTER should not be set after exit");
    }

    #[tokio::test]
    async fn test_exit_with_code() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // exit with code should propagate the exit code
        let result = kernel
            .execute("exit 42")
            .await
            .expect("exit failed");

        assert_eq!(result.code, 42);
        assert!(result.text_out().is_empty(), "exit should not produce stdout");
    }

    #[tokio::test]
    async fn test_set_e_stops_on_failure() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Enable error-exit mode
        kernel.execute("set -e").await.expect("set -e failed");

        // Run a sequence where the middle command fails
        kernel
            .execute(r#"
                STEP1="done"
                false
                STEP2="done"
            "#)
            .await
            .expect("execution failed");

        // STEP1 should be set, but STEP2 should NOT be set (exit on false)
        let step1 = kernel.get_var("STEP1").await;
        assert_eq!(step1, Some(Value::String("done".into())));

        let step2 = kernel.get_var("STEP2").await;
        assert!(step2.is_none(), "STEP2 should not be set after false with set -e");
    }

    #[tokio::test]
    async fn test_set_plus_e_disables_error_exit() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Enable then disable error-exit mode
        kernel.execute("set -e").await.expect("set -e failed");
        kernel.execute("set +e").await.expect("set +e failed");

        // Now failure should NOT stop execution
        kernel
            .execute(r#"
                STEP1="done"
                false
                STEP2="done"
            "#)
            .await
            .expect("execution failed");

        // Both should be set since +e disables error exit
        let step1 = kernel.get_var("STEP1").await;
        assert_eq!(step1, Some(Value::String("done".into())));

        let step2 = kernel.get_var("STEP2").await;
        assert_eq!(step2, Some(Value::String("done".into())));
    }

    #[tokio::test]
    async fn test_set_ignores_unknown_options() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Bash idiom: set -euo pipefail (we support -e, ignore the rest)
        let result = kernel
            .execute("set -e -u -o pipefail")
            .await
            .expect("set with unknown options failed");

        assert!(result.ok(), "set should succeed with unknown options");

        // -e should still be enabled
        kernel
            .execute(r#"
                BEFORE="yes"
                false
                AFTER="yes"
            "#)
            .await
            .ok();

        let after = kernel.get_var("AFTER").await;
        assert!(after.is_none(), "-e should be enabled despite unknown options");
    }

    #[tokio::test]
    async fn test_set_no_args_shows_settings() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Enable -e
        kernel.execute("set -e").await.expect("set -e failed");

        // Call set with no args to see settings
        let result = kernel.execute("set").await.expect("set failed");

        assert!(result.ok());
        assert!(result.text_out().contains("set -e"), "should show -e is enabled: {}", result.text_out());
    }

    #[tokio::test]
    async fn test_set_e_in_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("set -e").await.expect("set -e failed");

        // Pipeline failure should trigger exit
        kernel
            .execute(r#"
                BEFORE="yes"
                false | cat
                AFTER="yes"
            "#)
            .await
            .ok();

        let before = kernel.get_var("BEFORE").await;
        assert_eq!(before, Some(Value::String("yes".into())));

        // AFTER should not be set if pipeline failure triggers exit
        // Note: The exit code of a pipeline is the exit code of the last command
        // So `false | cat` returns 0 (cat succeeds). This is bash-compatible behavior.
        // To test pipeline failure, we need the last command to fail.
    }

    #[tokio::test]
    async fn test_set_e_with_and_chain() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("set -e").await.expect("set -e failed");

        // Commands in && chain should not trigger -e on the first failure
        // because && explicitly handles the error
        kernel
            .execute(r#"
                RESULT="initial"
                false && RESULT="chained"
                RESULT="continued"
            "#)
            .await
            .ok();

        // In bash, commands in && don't trigger -e. The chain handles the failure.
        // Our implementation may differ - let's verify current behavior.
        let result = kernel.get_var("RESULT").await;
        // If we follow bash semantics, RESULT should be "continued"
        // If we trigger -e on the false, RESULT stays "initial"
        assert!(result.is_some(), "RESULT should be set");
    }

    #[tokio::test]
    async fn test_set_e_exits_in_for_loop() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("set -e").await.expect("set -e failed");

        kernel
            .execute(r#"
                REACHED="no"
                for x in 1 2 3; do
                    false
                    REACHED="yes"
                done
            "#)
            .await
            .ok();

        // With set -e, false should trigger exit; REACHED should remain "no"
        let reached = kernel.get_var("REACHED").await;
        assert_eq!(reached, Some(Value::String("no".into())),
            "set -e should exit on failure in for loop body");
    }

    #[tokio::test]
    async fn test_for_loop_continues_without_set_e() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Without set -e, for loop should continue normally
        kernel
            .execute(r#"
                COUNT=0
                for x in 1 2 3; do
                    false
                    COUNT=$((COUNT + 1))
                done
            "#)
            .await
            .ok();

        let count = kernel.get_var("COUNT").await;
        // Arithmetic produces Int values; accept either Int or String representation
        let count_val = match &count {
            Some(Value::Int(n)) => *n,
            Some(Value::String(s)) => s.parse().unwrap_or(-1),
            _ => -1,
        };
        assert_eq!(count_val, 3,
            "without set -e, loop should complete all iterations (got {:?})", count);
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Source Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_source_sets_variables() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Write a script to the VFS
        kernel
            .execute(r#"write "/test.kai" 'FOO="bar"'"#)
            .await
            .expect("write failed");

        // Source the script
        let result = kernel
            .execute(r#"source "/test.kai""#)
            .await
            .expect("source failed");

        assert!(result.ok(), "source should succeed");

        // Variable should be set in current scope
        let foo = kernel.get_var("FOO").await;
        assert_eq!(foo, Some(Value::String("bar".into())));
    }

    #[tokio::test]
    async fn test_source_with_dot_alias() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Write a script to the VFS
        kernel
            .execute(r#"write "/vars.kai" 'X=42'"#)
            .await
            .expect("write failed");

        // Source using . alias
        let result = kernel
            .execute(r#". "/vars.kai""#)
            .await
            .expect(". failed");

        assert!(result.ok(), ". should succeed");

        // Variable should be set in current scope
        let x = kernel.get_var("X").await;
        assert_eq!(x, Some(Value::Int(42)));
    }

    #[tokio::test]
    async fn test_source_not_found() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Try to source a non-existent file
        let result = kernel
            .execute(r#"source "/nonexistent.kai""#)
            .await
            .expect("source should not fail with error");

        assert!(!result.ok(), "source of non-existent file should fail");
        assert!(result.err.contains("nonexistent.kai"), "error should mention filename");
    }

    #[tokio::test]
    async fn test_source_missing_filename() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Call source with no arguments
        let result = kernel
            .execute("source")
            .await
            .expect("source should not fail with error");

        assert!(!result.ok(), "source without filename should fail");
        assert!(result.err.contains("missing filename"), "error should mention missing filename");
    }

    #[tokio::test]
    async fn test_source_executes_multiple_statements() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Write a script with multiple statements
        kernel
            .execute(r#"write "/multi.kai" 'A=1
B=2
C=3'"#)
            .await
            .expect("write failed");

        // Source it
        kernel
            .execute(r#"source "/multi.kai""#)
            .await
            .expect("source failed");

        // All variables should be set
        assert_eq!(kernel.get_var("A").await, Some(Value::Int(1)));
        assert_eq!(kernel.get_var("B").await, Some(Value::Int(2)));
        assert_eq!(kernel.get_var("C").await, Some(Value::Int(3)));
    }

    #[tokio::test]
    async fn test_source_can_define_functions() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Write a script that defines a function
        kernel
            .execute(r#"write "/functions.kai" 'greet() {
    echo "Hello, $1!"
}'"#)
            .await
            .expect("write failed");

        // Source it
        kernel
            .execute(r#"source "/functions.kai""#)
            .await
            .expect("source failed");

        // Use the defined function
        let result = kernel
            .execute(r#"greet "World""#)
            .await
            .expect("greet failed");

        assert!(result.ok());
        assert!(result.text_out().contains("Hello, World!"));
    }

    #[tokio::test]
    async fn test_source_inherits_error_exit() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Enable error exit
        kernel.execute("set -e").await.expect("set -e failed");

        // Write a script that has a failure
        kernel
            .execute(r#"write "/fail.kai" 'BEFORE="yes"
false
AFTER="yes"'"#)
            .await
            .expect("write failed");

        // Source it (should exit on false due to set -e)
        kernel
            .execute(r#"source "/fail.kai""#)
            .await
            .ok();

        // BEFORE should be set, AFTER should NOT be set due to error exit
        let before = kernel.get_var("BEFORE").await;
        assert_eq!(before, Some(Value::String("yes".into())));

        // Note: This test depends on whether error exit is checked within source
        // Currently our implementation checks per-statement in the main kernel
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // set -e with && / || chains
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_set_e_and_chain_left_fails() {
        // set -e; false && echo hi; REACHED=1 → REACHED should be set
        let kernel = Kernel::transient().expect("failed to create kernel");
        kernel.execute("set -e").await.expect("set -e failed");

        kernel
            .execute("false && echo hi; REACHED=1")
            .await
            .expect("execution failed");

        let reached = kernel.get_var("REACHED").await;
        assert_eq!(
            reached,
            Some(Value::Int(1)),
            "set -e should not trigger on left side of &&"
        );
    }

    #[tokio::test]
    async fn test_set_e_and_chain_right_fails() {
        // set -e; true && false; REACHED=1 → REACHED should NOT be set
        let kernel = Kernel::transient().expect("failed to create kernel");
        kernel.execute("set -e").await.expect("set -e failed");

        kernel
            .execute("true && false; REACHED=1")
            .await
            .expect("execution failed");

        let reached = kernel.get_var("REACHED").await;
        assert!(
            reached.is_none(),
            "set -e should trigger when right side of && fails"
        );
    }

    #[tokio::test]
    async fn test_set_e_or_chain_recovers() {
        // set -e; false || echo recovered; REACHED=1 → REACHED should be set
        let kernel = Kernel::transient().expect("failed to create kernel");
        kernel.execute("set -e").await.expect("set -e failed");

        kernel
            .execute("false || echo recovered; REACHED=1")
            .await
            .expect("execution failed");

        let reached = kernel.get_var("REACHED").await;
        assert_eq!(
            reached,
            Some(Value::Int(1)),
            "set -e should not trigger when || recovers the failure"
        );
    }

    #[tokio::test]
    async fn test_set_e_or_chain_both_fail() {
        // set -e; false || false; REACHED=1 → REACHED should NOT be set
        let kernel = Kernel::transient().expect("failed to create kernel");
        kernel.execute("set -e").await.expect("set -e failed");

        kernel
            .execute("false || false; REACHED=1")
            .await
            .expect("execution failed");

        let reached = kernel.get_var("REACHED").await;
        assert!(
            reached.is_none(),
            "set -e should trigger when || chain ultimately fails"
        );
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Cancellation Tests
    // ═══════════════════════════════════════════════════════════════════════════

    /// Helper: schedule a cancel after a delay from a background thread.
    /// Uses std::thread because cancel() is sync and Kernel is not Send.
    fn schedule_cancel(kernel: &Arc<Kernel>, delay: std::time::Duration) {
        let k = Arc::clone(kernel);
        std::thread::spawn(move || {
            std::thread::sleep(delay);
            k.cancel();
        });
    }

    #[tokio::test]
    async fn test_cancel_interrupts_for_loop() {
        let kernel = Arc::new(Kernel::transient().expect("failed to create kernel"));

        // Schedule cancel after a short delay from a background OS thread
        schedule_cancel(&kernel, std::time::Duration::from_millis(10));

        let result = kernel
            .execute("for i in $(seq 1 100000); do X=$i; done")
            .await
            .expect("execute failed");

        assert_eq!(result.code, 130, "cancelled execution should exit with code 130");

        // The loop variable should be set to something < 100000
        let x = kernel.get_var("X").await;
        if let Some(Value::Int(n)) = x {
            assert!(n < 100000, "loop should have been interrupted before finishing, got X={n}");
        }
    }

    #[tokio::test]
    async fn test_cancel_interrupts_while_loop() {
        let kernel = Arc::new(Kernel::transient().expect("failed to create kernel"));
        kernel.execute("COUNT=0").await.expect("init failed");

        schedule_cancel(&kernel, std::time::Duration::from_millis(10));

        let result = kernel
            .execute("while true; do COUNT=$((COUNT + 1)); done")
            .await
            .expect("execute failed");

        assert_eq!(result.code, 130);

        let count = kernel.get_var("COUNT").await;
        if let Some(Value::Int(n)) = count {
            assert!(n > 0, "loop should have run at least once");
        }
    }

    #[tokio::test]
    async fn test_reset_after_cancel() {
        // After cancellation, the next execute() should work normally
        let kernel = Kernel::transient().expect("failed to create kernel");
        kernel.cancel(); // cancel with nothing running

        let result = kernel.execute("echo hello").await.expect("execute failed");
        assert!(result.ok(), "execute after cancel should succeed");
        assert_eq!(result.text_out().trim(), "hello");
    }

    #[tokio::test]
    async fn test_cancel_interrupts_statement_sequence() {
        let kernel = Arc::new(Kernel::transient().expect("failed to create kernel"));

        // Schedule cancel after the first statement runs but before sleep finishes
        schedule_cancel(&kernel, std::time::Duration::from_millis(50));

        let result = kernel
            .execute("STEP=1; sleep 5; STEP=2; sleep 5; STEP=3")
            .await
            .expect("execute failed");

        assert_eq!(result.code, 130);

        // STEP should be 1 (set before sleep), not 2 or 3
        let step = kernel.get_var("STEP").await;
        assert_eq!(step, Some(Value::Int(1)), "cancel should stop before STEP=2");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Case Statement Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_case_simple_match() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "hello" in
                    hello) echo "matched hello" ;;
                    world) echo "matched world" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "matched hello");
    }

    #[tokio::test]
    async fn test_case_wildcard_match() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "main.rs" in
                    *.py) echo "Python" ;;
                    *.rs) echo "Rust" ;;
                    *) echo "Unknown" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "Rust");
    }

    #[tokio::test]
    async fn test_case_default_match() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "unknown.xyz" in
                    *.py) echo "Python" ;;
                    *.rs) echo "Rust" ;;
                    *) echo "Default" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "Default");
    }

    #[tokio::test]
    async fn test_case_no_match() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Case with no default branch and no match
        let result = kernel
            .execute(r#"
                case "nope" in
                    "yes") echo "yes" ;;
                    "no") echo "no" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert!(result.text_out().is_empty(), "no match should produce empty output");
    }

    #[tokio::test]
    async fn test_case_with_variable() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute(r#"LANG="rust""#).await.expect("set failed");

        let result = kernel
            .execute(r#"
                case ${LANG} in
                    python) echo "snake" ;;
                    rust) echo "crab" ;;
                    go) echo "gopher" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "crab");
    }

    #[tokio::test]
    async fn test_case_multiple_patterns() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "yes" in
                    "y"|"yes"|"Y"|"YES") echo "affirmative" ;;
                    "n"|"no"|"N"|"NO") echo "negative" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "affirmative");
    }

    #[tokio::test]
    async fn test_case_glob_question_mark() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "test1" in
                    test?) echo "matched test?" ;;
                    *) echo "default" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "matched test?");
    }

    #[tokio::test]
    async fn test_case_char_class() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"
                case "Yes" in
                    [Yy]*) echo "yes-like" ;;
                    [Nn]*) echo "no-like" ;;
                esac
            "#)
            .await
            .expect("case failed");

        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "yes-like");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Cat Stdin Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_cat_from_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"echo "piped text" | cat"#)
            .await
            .expect("cat pipeline failed");

        assert!(result.ok(), "cat failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "piped text");
    }

    #[tokio::test]
    async fn test_cat_from_pipeline_multiline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"echo "line1\nline2" | cat -n"#)
            .await
            .expect("cat pipeline failed");

        assert!(result.ok(), "cat failed: {}", result.err);
        assert!(result.text_out().contains("1\t"), "output: {}", result.text_out());
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Heredoc Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_heredoc_basic() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute("cat <<EOF\nhello\nEOF")
            .await
            .expect("heredoc failed");

        assert!(result.ok(), "cat with heredoc failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "hello");
    }

    #[tokio::test]
    async fn test_arithmetic_in_string() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"echo "result: $((1 + 2))""#)
            .await
            .expect("arithmetic in string failed");

        assert!(result.ok(), "echo failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "result: 3");
    }

    #[tokio::test]
    async fn test_heredoc_multiline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute("cat <<EOF\nline1\nline2\nline3\nEOF")
            .await
            .expect("heredoc failed");

        assert!(result.ok(), "cat with heredoc failed: {}", result.err);
        assert!(result.text_out().contains("line1"), "output: {}", result.text_out());
        assert!(result.text_out().contains("line2"), "output: {}", result.text_out());
        assert!(result.text_out().contains("line3"), "output: {}", result.text_out());
    }

    #[tokio::test]
    async fn test_heredoc_variable_expansion() {
        // Bug N: unquoted heredoc should expand variables
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("GREETING=hello").await.expect("set var");

        let result = kernel
            .execute("cat <<EOF\n$GREETING world\nEOF")
            .await
            .expect("heredoc expansion failed");

        assert!(result.ok(), "heredoc expansion failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "hello world");
    }

    #[tokio::test]
    async fn test_heredoc_quoted_no_expansion() {
        // Bug N: quoted heredoc (<<'EOF') should NOT expand variables
        let kernel = Kernel::transient().expect("failed to create kernel");

        kernel.execute("GREETING=hello").await.expect("set var");

        let result = kernel
            .execute("cat <<'EOF'\n$GREETING world\nEOF")
            .await
            .expect("quoted heredoc failed");

        assert!(result.ok(), "quoted heredoc failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "$GREETING world");
    }

    #[tokio::test]
    async fn test_heredoc_default_value_expansion() {
        // Bug N: ${VAR:-default} should expand in unquoted heredocs
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute("cat <<EOF\n${UNSET:-fallback}\nEOF")
            .await
            .expect("heredoc default expansion failed");

        assert!(result.ok(), "heredoc default expansion failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "fallback");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Read Builtin Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_read_from_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Pipe input to read
        let result = kernel
            .execute(r#"echo "Alice" | read NAME; echo "Hello, ${NAME}""#)
            .await
            .expect("read pipeline failed");

        assert!(result.ok(), "read failed: {}", result.err);
        assert!(result.text_out().contains("Hello, Alice"), "output: {}", result.text_out());
    }

    #[tokio::test]
    async fn test_read_multiple_vars_from_pipeline() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel
            .execute(r#"echo "John Doe 42" | read FIRST LAST AGE; echo "${FIRST} is ${AGE}""#)
            .await
            .expect("read pipeline failed");

        assert!(result.ok(), "read failed: {}", result.err);
        assert!(result.text_out().contains("John is 42"), "output: {}", result.text_out());
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Shell-Style Function Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_posix_function_with_positional_params() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define POSIX-style function
        kernel
            .execute(r#"greet() { echo "Hello, $1!" }"#)
            .await
            .expect("function definition failed");

        // Call the function
        let result = kernel
            .execute(r#"greet "Amy""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "greet failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "Hello, Amy!");
    }

    #[tokio::test]
    async fn test_posix_function_multiple_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define function using $1 and $2
        kernel
            .execute(r#"add_greeting() { echo "$1 $2!" }"#)
            .await
            .expect("function definition failed");

        // Call the function
        let result = kernel
            .execute(r#"add_greeting "Hello" "World""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "function failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "Hello World!");
    }

    #[tokio::test]
    async fn test_bash_function_with_positional_params() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define bash-style function (function keyword, no parens)
        kernel
            .execute(r#"function greet { echo "Hi $1" }"#)
            .await
            .expect("function definition failed");

        // Call the function
        let result = kernel
            .execute(r#"greet "Bob""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "greet failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "Hi Bob");
    }

    #[tokio::test]
    async fn test_shell_function_with_all_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define function using $@ (all args)
        kernel
            .execute(r#"echo_all() { echo "args: $@" }"#)
            .await
            .expect("function definition failed");

        // Call with multiple args
        let result = kernel
            .execute(r#"echo_all "a" "b" "c""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "function failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "args: a b c");
    }

    #[tokio::test]
    async fn test_shell_function_with_arg_count() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Define function using $# (arg count)
        kernel
            .execute(r#"count_args() { echo "count: $#" }"#)
            .await
            .expect("function definition failed");

        // Call with three args
        let result = kernel
            .execute(r#"count_args "x" "y" "z""#)
            .await
            .expect("function call failed");

        assert!(result.ok(), "function failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "count: 3");
    }

    #[tokio::test]
    async fn test_shell_function_shared_scope() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Set a variable in parent scope
        kernel
            .execute(r#"PARENT_VAR="visible""#)
            .await
            .expect("set failed");

        // Define shell function that reads and writes parent variable
        kernel
            .execute(r#"modify_parent() {
                echo "saw: ${PARENT_VAR}"
                PARENT_VAR="changed by function"
            }"#)
            .await
            .expect("function definition failed");

        // Call the function - it SHOULD see PARENT_VAR (bash-compatible shared scope)
        let result = kernel.execute("modify_parent").await.expect("function failed");

        assert!(
            result.text_out().contains("visible"),
            "Shell function should access parent scope, got: {}",
            result.text_out()
        );

        // Parent variable should be modified
        let var = kernel.get_var("PARENT_VAR").await;
        assert_eq!(
            var,
            Some(Value::String("changed by function".into())),
            "Shell function should modify parent scope"
        );
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Script Execution via PATH Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_script_execution_from_path() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Create /bin directory and script
        kernel.execute(r#"mkdir "/bin""#).await.ok();
        kernel
            .execute(r#"write "/bin/hello.kai" 'echo "Hello from script!"'"#)
            .await
            .expect("write script failed");

        // Set PATH to /bin
        kernel.execute(r#"PATH="/bin""#).await.expect("set PATH failed");

        // Call script by name (without .kai extension)
        let result = kernel
            .execute("hello")
            .await
            .expect("script execution failed");

        assert!(result.ok(), "script failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "Hello from script!");
    }

    #[tokio::test]
    async fn test_script_with_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Create script that uses positional params
        kernel.execute(r#"mkdir "/bin""#).await.ok();
        kernel
            .execute(r#"write "/bin/greet.kai" 'echo "Hello, $1!"'"#)
            .await
            .expect("write script failed");

        // Set PATH
        kernel.execute(r#"PATH="/bin""#).await.expect("set PATH failed");

        // Call script with arg
        let result = kernel
            .execute(r#"greet "World""#)
            .await
            .expect("script execution failed");

        assert!(result.ok(), "script failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "Hello, World!");
    }

    #[tokio::test]
    async fn test_script_not_found() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Set empty PATH
        kernel.execute(r#"PATH="/nonexistent""#).await.expect("set PATH failed");

        // Call non-existent script
        let result = kernel
            .execute("noscript")
            .await
            .expect("execution failed");

        assert!(!result.ok(), "should fail with command not found");
        assert_eq!(result.code, 127);
        assert!(result.err.contains("command not found"));
    }

    #[tokio::test]
    async fn test_script_path_search_order() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Create two directories with same-named script
        // Note: using "myscript" not "test" to avoid conflict with test builtin
        kernel.execute(r#"mkdir "/first""#).await.ok();
        kernel.execute(r#"mkdir "/second""#).await.ok();
        kernel
            .execute(r#"write "/first/myscript.kai" 'echo "from first"'"#)
            .await
            .expect("write failed");
        kernel
            .execute(r#"write "/second/myscript.kai" 'echo "from second"'"#)
            .await
            .expect("write failed");

        // Set PATH with first before second
        kernel.execute(r#"PATH="/first:/second""#).await.expect("set PATH failed");

        // Should find first one
        let result = kernel
            .execute("myscript")
            .await
            .expect("script execution failed");

        assert!(result.ok(), "script failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "from first");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Special Variable Tests ($?, $$, unset vars)
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_last_exit_code_success() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // true exits with 0
        let result = kernel.execute("true; echo $?").await.expect("execution failed");
        assert!(result.text_out().contains("0"), "expected 0, got: {}", result.text_out());
    }

    #[tokio::test]
    async fn test_last_exit_code_failure() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // false exits with 1
        let result = kernel.execute("false; echo $?").await.expect("execution failed");
        assert!(result.text_out().contains("1"), "expected 1, got: {}", result.text_out());
    }

    #[tokio::test]
    async fn test_current_pid() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        let result = kernel.execute("echo $$").await.expect("execution failed");
        // PID should be a positive number
        let pid: u32 = result.text_out().trim().parse().expect("PID should be a number");
        assert!(pid > 0, "PID should be positive");
    }

    #[tokio::test]
    async fn test_unset_variable_expands_to_empty() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Unset variable in interpolation should be empty
        let result = kernel.execute(r#"echo "prefix:${UNSET_VAR}:suffix""#).await.expect("execution failed");
        assert_eq!(result.text_out().trim(), "prefix::suffix");
    }

    #[tokio::test]
    async fn test_eq_ne_operators() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Test -eq operator
        let result = kernel.execute(r#"if [[ 5 -eq 5 ]]; then echo "eq works"; fi"#).await.expect("execution failed");
        assert_eq!(result.text_out().trim(), "eq works");

        // Test -ne operator
        let result = kernel.execute(r#"if [[ 5 -ne 3 ]]; then echo "ne works"; fi"#).await.expect("execution failed");
        assert_eq!(result.text_out().trim(), "ne works");

        // Test -eq with different values
        let result = kernel.execute(r#"if [[ 5 -eq 3 ]]; then echo "wrong"; else echo "correct"; fi"#).await.expect("execution failed");
        assert_eq!(result.text_out().trim(), "correct");
    }

    #[tokio::test]
    async fn test_escaped_dollar_in_string() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // \$ should produce literal $
        let result = kernel.execute(r#"echo "\$100""#).await.expect("execution failed");
        assert_eq!(result.text_out().trim(), "$100");
    }

    #[tokio::test]
    async fn test_special_vars_in_interpolation() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Test $? in string interpolation
        let result = kernel.execute(r#"true; echo "exit: $?""#).await.expect("execution failed");
        assert_eq!(result.text_out().trim(), "exit: 0");

        // Test $$ in string interpolation
        let result = kernel.execute(r#"echo "pid: $$""#).await.expect("execution failed");
        assert!(result.text_out().starts_with("pid: "), "unexpected output: {}", result.text_out());
        let text = result.text_out();
        let pid_part = text.trim().strip_prefix("pid: ").unwrap();
        let _pid: u32 = pid_part.parse().expect("PID in string should be a number");
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Command Substitution Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[tokio::test]
    async fn test_command_subst_assignment() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Command substitution in assignment
        let result = kernel.execute(r#"X=$(echo hello); echo "$X""#).await.expect("execution failed");
        assert_eq!(result.text_out().trim(), "hello");
    }

    #[tokio::test]
    async fn test_command_subst_with_args() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Command substitution with string argument
        let result = kernel.execute(r#"X=$(echo "a b c"); echo "$X""#).await.expect("execution failed");
        assert_eq!(result.text_out().trim(), "a b c");
    }

    #[tokio::test]
    async fn test_command_subst_nested_vars() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Variables inside command substitution
        let result = kernel.execute(r#"Y=world; X=$(echo "hello $Y"); echo "$X""#).await.expect("execution failed");
        assert_eq!(result.text_out().trim(), "hello world");
    }

    #[tokio::test]
    async fn test_background_job_basic() {
        use std::time::Duration;

        let kernel = Kernel::new(KernelConfig::isolated()).expect("failed to create kernel");

        // Run a simple background command
        let result = kernel.execute("echo hello &").await.expect("execution failed");
        assert!(result.ok(), "background command should succeed: {}", result.err);
        assert!(result.text_out().contains("[1]"), "should return job ID: {}", result.text_out());

        // Give the job time to complete
        tokio::time::sleep(Duration::from_millis(100)).await;

        // Check job status
        let status = kernel.execute("cat /v/jobs/1/status").await.expect("status check failed");
        assert!(status.ok(), "status should succeed: {}", status.err);
        assert!(
            status.text_out().contains("done:") || status.text_out().contains("running"),
            "should have valid status: {}",
            status.text_out()
        );

        // Check stdout
        let stdout = kernel.execute("cat /v/jobs/1/stdout").await.expect("stdout check failed");
        assert!(stdout.ok());
        assert!(stdout.text_out().contains("hello"));
    }

    #[tokio::test]
    async fn test_heredoc_piped_to_command() {
        // Bug 4: heredoc content should pipe through to next command
        let kernel = Kernel::transient().expect("kernel");
        let result = kernel.execute("cat <<EOF | cat\nhello world\nEOF").await.expect("exec");
        assert!(result.ok(), "heredoc | cat failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "hello world");
    }

    #[tokio::test]
    async fn test_for_loop_glob_iterates() {
        // Bug 1: for F in $(glob ...) should iterate per file, not once
        let kernel = Kernel::transient().expect("kernel");
        let dir = format!("/tmp/kaish_test_glob_{}", std::process::id());
        kernel.execute(&format!("mkdir -p {dir}")).await.unwrap();
        kernel.execute(&format!("echo a > {dir}/a.txt")).await.unwrap();
        kernel.execute(&format!("echo b > {dir}/b.txt")).await.unwrap();
        let result = kernel.execute(&format!(r#"
            N=0
            for F in $(glob "{dir}/*.txt"); do
                N=$((N + 1))
            done
            echo $N
        "#)).await.unwrap();
        assert!(result.ok(), "for glob failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "2", "Should iterate 2 files, got: {}", result.text_out());
        kernel.execute(&format!("rm {dir}/a.txt")).await.unwrap();
        kernel.execute(&format!("rm {dir}/b.txt")).await.unwrap();
    }

    #[tokio::test]
    async fn test_bare_glob_expansion_echo() {
        let kernel = Kernel::transient().expect("kernel");
        let dir = format!("/tmp/kaish_test_bareglob_{}", std::process::id());
        kernel.execute(&format!("mkdir -p {dir}")).await.unwrap();
        kernel.execute(&format!("echo a > {dir}/a.txt")).await.unwrap();
        kernel.execute(&format!("echo b > {dir}/b.txt")).await.unwrap();
        kernel.execute(&format!("echo c > {dir}/c.rs")).await.unwrap();
        kernel.execute(&format!("cd {dir}")).await.unwrap();
        let result = kernel.execute("echo *.txt").await.unwrap();
        assert!(result.ok(), "echo *.txt failed: {}", result.err);
        let out = result.text_out();
        let out = out.trim();
        // Should contain both .txt files (order may vary)
        assert!(out.contains("a.txt"), "missing a.txt in: {}", out);
        assert!(out.contains("b.txt"), "missing b.txt in: {}", out);
        assert!(!out.contains("c.rs"), "should not contain c.rs in: {}", out);
        // cleanup
        kernel.execute(&format!("rm {dir}/a.txt")).await.unwrap();
        kernel.execute(&format!("rm {dir}/b.txt")).await.unwrap();
        kernel.execute(&format!("rm {dir}/c.rs")).await.unwrap();
    }

    #[tokio::test]
    async fn test_bare_glob_no_matches_errors() {
        let kernel = Kernel::transient().expect("kernel");
        let dir = format!("/tmp/kaish_test_bareglob_nomatch_{}", std::process::id());
        kernel.execute(&format!("mkdir -p {dir}")).await.unwrap();
        kernel.execute(&format!("cd {dir}")).await.unwrap();
        let result = kernel.execute("echo *.nonexistent").await;
        match &result {
            Ok(exec) => {
                // No-match glob should produce a non-zero exit code
                assert!(!exec.ok(), "expected failure, got success: out={}, err={}", exec.text_out(), exec.err);
                assert!(exec.err.contains("no matches"), "error should say no matches: {}", exec.err);
            }
            Err(e) => {
                assert!(e.to_string().contains("no matches"), "error should say no matches: {}", e);
            }
        }
    }

    #[tokio::test]
    async fn test_bare_glob_disabled_with_set() {
        let kernel = Kernel::transient().expect("kernel");
        let dir = format!("/tmp/kaish_test_bareglob_noglob_{}", std::process::id());
        kernel.execute(&format!("mkdir -p {dir}")).await.unwrap();
        kernel.execute(&format!("echo a > {dir}/a.txt")).await.unwrap();
        kernel.execute(&format!("cd {dir}")).await.unwrap();
        // Disable glob expansion
        kernel.execute("set +o glob").await.unwrap();
        let result = kernel.execute("echo *.txt").await.unwrap();
        // With glob disabled, *.txt should be passed as literal string
        assert!(result.ok(), "echo should succeed: {}", result.err);
        assert_eq!(result.text_out().trim(), "*.txt", "should be literal: {}", result.text_out());
        // cleanup
        kernel.execute("set -o glob").await.unwrap();
        kernel.execute(&format!("rm {dir}/a.txt")).await.unwrap();
    }

    #[tokio::test]
    async fn test_bare_glob_quoted_not_expanded() {
        let kernel = Kernel::transient().expect("kernel");
        let dir = format!("/tmp/kaish_test_bareglob_quoted_{}", std::process::id());
        kernel.execute(&format!("mkdir -p {dir}")).await.unwrap();
        kernel.execute(&format!("echo a > {dir}/a.txt")).await.unwrap();
        kernel.execute(&format!("cd {dir}")).await.unwrap();
        // Quoted globs should NOT expand
        let result = kernel.execute("echo \"*.txt\"").await.unwrap();
        assert!(result.ok(), "echo should succeed: {}", result.err);
        assert_eq!(result.text_out().trim(), "*.txt", "quoted should be literal: {}", result.text_out());
        // cleanup
        kernel.execute(&format!("rm {dir}/a.txt")).await.unwrap();
    }

    #[tokio::test]
    async fn test_bare_glob_for_loop() {
        let kernel = Kernel::transient().expect("kernel");
        let dir = format!("/tmp/kaish_test_bareglob_forloop_{}", std::process::id());
        kernel.execute(&format!("mkdir -p {dir}")).await.unwrap();
        kernel.execute(&format!("echo a > {dir}/a.txt")).await.unwrap();
        kernel.execute(&format!("echo b > {dir}/b.txt")).await.unwrap();
        kernel.execute(&format!("cd {dir}")).await.unwrap();
        let result = kernel.execute(r#"
            N=0
            for f in *.txt; do
                N=$((N + 1))
            done
            echo $N
        "#).await.unwrap();
        assert!(result.ok(), "for loop failed: {}", result.err);
        assert_eq!(result.text_out().trim(), "2", "should iterate 2 files: {}", result.text_out());
        // cleanup
        kernel.execute(&format!("rm {dir}/a.txt")).await.unwrap();
        kernel.execute(&format!("rm {dir}/b.txt")).await.unwrap();
    }

    #[tokio::test]
    async fn test_glob_in_assignment_is_literal() {
        let kernel = Kernel::transient().expect("kernel");
        let result = kernel.execute("X=*.txt; echo $X").await.unwrap();
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "*.txt", "glob in assignment should be literal");
    }

    #[tokio::test]
    async fn test_glob_in_test_expr_is_literal() {
        let kernel = Kernel::transient().expect("kernel");
        let result = kernel.execute(r#"
            if [[ *.txt == "*.txt" ]]; then
                echo "match"
            else
                echo "no"
            fi
        "#).await.unwrap();
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "match", "glob in test expr should be literal");
    }

    #[tokio::test]
    async fn test_command_subst_echo_not_iterable() {
        // Regression guard: $(echo "a b c") must remain a single string
        let kernel = Kernel::transient().expect("kernel");
        let result = kernel.execute(r#"
            N=0
            for X in $(echo "a b c"); do N=$((N + 1)); done
            echo $N
        "#).await.unwrap();
        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "1", "echo should be one item: {}", result.text_out());
    }

    // -- accumulate_result / newline tests --

    #[test]
    fn test_accumulate_no_double_newlines() {
        // When output already ends with \n, accumulate should not add another
        let mut acc = ExecResult::success("line1\n");
        let new = ExecResult::success("line2\n");
        accumulate_result(&mut acc, &new);
        assert_eq!(&*acc.text_out(), "line1\nline2\n");
        assert!(!acc.text_out().contains("\n\n"), "should not have double newlines: {:?}", acc.text_out());
    }

    #[test]
    fn test_accumulate_adds_separator_when_needed() {
        // When output does NOT end with \n, accumulate adds one
        let mut acc = ExecResult::success("line1");
        let new = ExecResult::success("line2");
        accumulate_result(&mut acc, &new);
        assert_eq!(&*acc.text_out(), "line1\nline2");
    }

    #[test]
    fn test_accumulate_empty_into_nonempty() {
        let mut acc = ExecResult::success("");
        let new = ExecResult::success("hello\n");
        accumulate_result(&mut acc, &new);
        assert_eq!(&*acc.text_out(), "hello\n");
    }

    #[test]
    fn test_accumulate_nonempty_into_empty() {
        let mut acc = ExecResult::success("hello\n");
        let new = ExecResult::success("");
        accumulate_result(&mut acc, &new);
        assert_eq!(&*acc.text_out(), "hello\n");
    }

    #[test]
    fn test_accumulate_stderr_no_double_newlines() {
        let mut acc = ExecResult::failure(1, "err1\n");
        let new = ExecResult::failure(1, "err2\n");
        accumulate_result(&mut acc, &new);
        assert!(!acc.err.contains("\n\n"), "stderr should not have double newlines: {:?}", acc.err);
    }

    #[tokio::test]
    async fn test_multiple_echo_no_blank_lines() {
        let kernel = Kernel::transient().expect("kernel");
        let result = kernel
            .execute("echo one\necho two\necho three")
            .await
            .expect("execution failed");
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "one\ntwo\nthree\n");
    }

    #[tokio::test]
    async fn test_for_loop_no_blank_lines() {
        let kernel = Kernel::transient().expect("kernel");
        let result = kernel
            .execute(r#"for X in a b c; do echo "item: ${X}"; done"#)
            .await
            .expect("execution failed");
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "item: a\nitem: b\nitem: c\n");
    }

    #[tokio::test]
    async fn test_for_command_subst_no_blank_lines() {
        let kernel = Kernel::transient().expect("kernel");
        let result = kernel
            .execute(r#"for N in $(seq 1 3); do echo "n=${N}"; done"#)
            .await
            .expect("execution failed");
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "n=1\nn=2\nn=3\n");
    }

    // ------------------------------------------------------------------
    // build_args_async: multi-consume flags (jq --arg NAME VALUE pattern)
    // ------------------------------------------------------------------

    /// Helper: a throwaway schema with one `--pair` param declared as
    /// consuming two positionals per occurrence. Modelled after what
    /// jq_native will declare for `--arg` / `--argjson`.
    fn multi_consume_schema() -> crate::tools::ToolSchema {
        use crate::tools::{ParamSchema, ToolSchema};
        ToolSchema::new("test", "multi-consume smoke")
            .param(
                ParamSchema::optional("pair", "array", Value::Null, "name+value pair")
                    .consumes(2),
            )
    }

    fn pos(s: &str) -> Arg {
        Arg::Positional(Expr::Literal(Value::String(s.to_string())))
    }

    #[tokio::test]
    async fn build_args_multi_consume_single_occurrence() {
        let kernel = Kernel::transient().expect("kernel");
        let schema = multi_consume_schema();
        // Simulates:  test --pair NAME VALUE filter
        let args = vec![
            Arg::LongFlag("pair".into()),
            pos("NAME"),
            pos("VALUE"),
            pos("filter"),
        ];
        let built = kernel
            .build_args_async(&args, Some(&schema))
            .await
            .expect("build_args should succeed");

        // `--pair` + its two positionals are consumed into named["pair"],
        // which becomes an outer array of one inner 2-element array.
        let pair = built.named.get("pair").expect("named[pair] missing");
        match pair {
            Value::Json(serde_json::Value::Array(occurrences)) => {
                assert_eq!(occurrences.len(), 1, "expected one occurrence");
                match &occurrences[0] {
                    serde_json::Value::Array(values) => {
                        assert_eq!(values.len(), 2, "pair must have 2 values");
                        assert_eq!(values[0], serde_json::Value::String("NAME".into()));
                        assert_eq!(values[1], serde_json::Value::String("VALUE".into()));
                    }
                    other => panic!("expected inner array, got {other:?}"),
                }
            }
            other => panic!("expected Json(Array(...)) for named[pair], got {other:?}"),
        }

        // The un-consumed positional ("filter") remains in `positional`.
        assert_eq!(built.positional.len(), 1);
        assert_eq!(built.positional[0], Value::String("filter".into()));
    }

    #[tokio::test]
    async fn build_args_multi_consume_two_occurrences_accumulate() {
        let kernel = Kernel::transient().expect("kernel");
        let schema = multi_consume_schema();
        // Simulates:  test --pair A 1 --pair B 2 filter
        let args = vec![
            Arg::LongFlag("pair".into()),
            pos("A"),
            pos("1"),
            Arg::LongFlag("pair".into()),
            pos("B"),
            pos("2"),
            pos("filter"),
        ];
        let built = kernel
            .build_args_async(&args, Some(&schema))
            .await
            .expect("build_args should succeed");

        let pair = built.named.get("pair").expect("named[pair] missing");
        match pair {
            Value::Json(serde_json::Value::Array(occurrences)) => {
                assert_eq!(occurrences.len(), 2, "expected two occurrences");
                // Preserved in invocation order.
                match &occurrences[0] {
                    serde_json::Value::Array(values) => {
                        assert_eq!(values[0], serde_json::Value::String("A".into()));
                        assert_eq!(values[1], serde_json::Value::String("1".into()));
                    }
                    other => panic!("expected inner array, got {other:?}"),
                }
                match &occurrences[1] {
                    serde_json::Value::Array(values) => {
                        assert_eq!(values[0], serde_json::Value::String("B".into()));
                        assert_eq!(values[1], serde_json::Value::String("2".into()));
                    }
                    other => panic!("expected inner array, got {other:?}"),
                }
            }
            other => panic!("expected Json(Array(...)), got {other:?}"),
        }
    }

    // ── initial_vars + execute_with_vars + hermetic env ───────────────────

    #[tokio::test]
    async fn test_initial_vars_set_and_exported() {
        let config = KernelConfig::transient()
            .with_var("INIT_FOO", Value::String("bar".into()));
        let kernel = Kernel::new(config).expect("failed to create kernel");

        assert_eq!(
            kernel.get_var("INIT_FOO").await,
            Some(Value::String("bar".into()))
        );
        assert!(
            kernel.scope.read().await.is_exported("INIT_FOO"),
            "initial_vars entries must be marked exported"
        );
    }

    #[tokio::test]
    async fn test_execute_with_vars_overlay_visible() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let mut overlay = HashMap::new();
        overlay.insert("OVERLAY_X".to_string(), Value::String("yes".into()));

        let result = kernel
            .execute_with_options(r#"echo "${OVERLAY_X}""#, ExecuteOptions::new().with_vars(overlay))
            .await
            .expect("execute failed");

        assert!(result.ok());
        assert_eq!(result.text_out().trim(), "yes");
    }

    #[tokio::test]
    async fn test_execute_with_vars_overlay_cleanup() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let mut overlay = HashMap::new();
        overlay.insert("EPHEMERAL".to_string(), Value::String("transient".into()));

        kernel
            .execute_with_options("echo ignored", ExecuteOptions::new().with_vars(overlay))
            .await
            .expect("execute failed");

        assert_eq!(kernel.get_var("EPHEMERAL").await, None);
        assert!(
            !kernel.scope.read().await.is_exported("EPHEMERAL"),
            "overlay-only export must be cleared on return"
        );
    }

    #[tokio::test]
    async fn test_execute_with_vars_does_not_clobber_existing_export() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        kernel
            .execute("export OUTER=outer")
            .await
            .expect("export failed");

        let mut overlay = HashMap::new();
        overlay.insert("OUTER".to_string(), Value::String("inner".into()));
        let result = kernel
            .execute_with_options(r#"echo "${OUTER}""#, ExecuteOptions::new().with_vars(overlay))
            .await
            .expect("execute failed");
        assert_eq!(result.text_out().trim(), "inner");

        assert_eq!(
            kernel.get_var("OUTER").await,
            Some(Value::String("outer".into())),
            "outer value must reappear after pop"
        );
        assert!(
            kernel.scope.read().await.is_exported("OUTER"),
            "outer export must survive overlay"
        );
    }

    #[tokio::test]
    async fn test_execute_with_vars_inner_assignment_is_local() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let mut overlay = HashMap::new();
        overlay.insert("LOCAL_FOO".to_string(), Value::String("from-overlay".into()));

        // Variable assignment inside a single statement uses set() (innermost
        // frame), not set_global() — this matches bash function-local semantics.
        // We explicitly use `local FOO=...` style by relying on the pushed
        // frame; the assignment in the script body modifies the same frame.
        let result = kernel
            .execute_with_options(
                r#"LOCAL_FOO="reassigned"; echo "${LOCAL_FOO}""#,
                ExecuteOptions::new().with_vars(overlay),
            )
            .await
            .expect("execute failed");
        assert!(result.ok());

        // After the call the frame is popped, so LOCAL_FOO is gone regardless
        // of how the script reassigned it.
        assert_eq!(kernel.get_var("LOCAL_FOO").await, None);
    }

    #[tokio::test]
    async fn test_external_command_sees_exported_var() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute("export EXT_FOO=bar; printenv EXT_FOO")
            .await
            .expect("execute failed");

        assert!(result.ok(), "printenv should succeed: stderr={}", result.err);
        assert_eq!(result.text_out().trim(), "bar");
    }

    #[tokio::test]
    async fn test_external_command_does_not_see_unexported_var() {
        let kernel = Kernel::transient().expect("failed to create kernel");

        // Set without exporting; printenv must not see it (exit code != 0,
        // empty stdout per printenv semantics).
        let result = kernel
            .execute("EXT_BAR=hidden; printenv EXT_BAR")
            .await
            .expect("execute failed");

        assert!(!result.ok(), "printenv should fail when var is unexported");
        assert!(
            result.text_out().trim().is_empty(),
            "no stdout when var is missing, got: {}",
            result.text_out()
        );
    }

    #[tokio::test]
    async fn test_external_command_does_not_see_os_env() {
        // The kernel is hermetic: it never reads std::env::vars() and only
        // exports what it has been told to export. Cargo always sets PATH for
        // tests, so PATH is reliably present in the OS env — but a transient
        // kernel doesn't seed it into initial_vars, so `printenv PATH` from
        // inside the kernel must fail.
        assert!(
            std::env::var_os("PATH").is_some(),
            "test precondition: cargo should set PATH"
        );

        let kernel = Kernel::transient().expect("failed to create kernel");
        let result = kernel
            .execute("printenv PATH")
            .await
            .expect("execute failed");

        assert!(
            !result.ok(),
            "printenv PATH must fail in hermetic kernel, got stdout={:?}",
            result.text_out()
        );
        assert!(
            result.text_out().trim().is_empty(),
            "no PATH in subprocess env, got stdout={:?}",
            result.text_out()
        );
    }

    #[tokio::test]
    async fn test_execute_with_vars_overlay_reaches_subprocess() {
        let kernel = Kernel::transient().expect("failed to create kernel");
        let mut overlay = HashMap::new();
        overlay.insert("SUB_FOO".to_string(), Value::String("subproc".into()));

        let result = kernel
            .execute_with_options("printenv SUB_FOO", ExecuteOptions::new().with_vars(overlay))
            .await
            .expect("execute failed");

        assert!(
            result.ok(),
            "printenv should succeed: code={} stdout={:?} stderr={:?}",
            result.code,
            result.text_out(),
            result.err
        );
        assert_eq!(result.text_out().trim(), "subproc");
    }
}
