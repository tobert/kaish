//! Kernel-level execution options.
//!
//! `ExecuteOptions` is the input to a single kernel `execute` call. It collects
//! the per-call knobs (variables, timeout, cancellation) so embedders don't need
//! to manage half a dozen execute-method overloads.

use std::collections::{BTreeMap, HashMap};
use std::path::PathBuf;
use std::time::Duration;

use tokio_util::sync::CancellationToken;

use crate::value::Value;

/// Per-call options for `Kernel::execute_with_options`.
///
/// Construct with `ExecuteOptions::new()` and the chainable `with_*` builders,
/// or via `Default`.
///
/// # Cancellation vs. timeout — embedder note
///
/// If a `cancel_token` is supplied, it is **raced** against the kernel's
/// internal token. The kernel does NOT cancel the embedder's token on its
/// own timeouts — it cancels its internal token and returns exit code 124.
/// So `your_token.is_cancelled()` after the call returns reflects only
/// whether *you* (or someone sharing your token) cancelled, not whether the
/// kernel timed out. Distinguish via the returned `ExecResult.code`:
/// `124` = kernel timeout, `130` = cancellation (Ctrl-C / `Kernel::cancel`).
#[derive(Default, Clone)]
pub struct ExecuteOptions {
    /// Variables exported into this call's environment (per-call overlay).
    pub vars: HashMap<String, Value>,
    /// Per-call timeout. Overrides `KernelConfig::request_timeout`.
    ///
    /// `None` means no timeout (or whatever the kernel-config default is).
    /// `Some(Duration::ZERO)` returns exit 124 immediately without spawning
    /// anything — useful for tests and dry-run paths.
    /// Any other `Some(d)` lets the kernel run for at most `d` before cancelling
    /// (which kills external children with the configured grace) and returning 124.
    pub timeout: Option<Duration>,
    /// Optional externally-owned cancellation token, *raced* against the kernel's
    /// internal token. Either firing cancels the request and kills any running
    /// external children. The kernel does not store this token in its own state —
    /// it's a per-call read-only input, so embedders are free to drop or reuse
    /// the original token after the call returns. CancellationToken is internally
    /// `Arc`-shared, so `clone()` it into the builder if you want to keep your
    /// original handle.
    pub cancel_token: Option<CancellationToken>,
    /// Per-call working directory override.
    ///
    /// When `Some(path)`, the kernel runs this call as if `cd path` happened
    /// first, then restores the prior cwd on return. Useful for embedders that
    /// run scripts in workspace contexts (notebook cells, per-tool dirs)
    /// without polluting the long-lived kernel's cwd.
    pub cwd: Option<PathBuf>,
    /// W3C `traceparent` of the embedder's active span, e.g.
    /// `"00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"`. When set,
    /// the kernel's execution span parents onto it, so kaish's spans appear as
    /// children of the embedder's trace rather than as orphan roots.
    pub traceparent: Option<String>,
    /// W3C `tracestate` (vendor-specific list) that rides alongside
    /// `traceparent`. Per the W3C spec, `tracestate` is meaningless without a
    /// `traceparent`, so the kernel ignores it unless `traceparent` is also set.
    pub tracestate: Option<String>,
    /// W3C baggage — cross-cutting identifiers (owner, connection, tenant, …)
    /// the embedder wants stamped onto the trace. Propagated to every child
    /// span. Independent of `traceparent`: baggage with no trace context starts
    /// a fresh root that still carries the identifiers.
    pub baggage: BTreeMap<String, String>,
    /// Standard input for this call, consumed by the first top-level command
    /// that reads stdin (shell semantics — a later reader sees nothing).
    ///
    /// Lets a non-interactive frontend feed piped input, e.g.
    /// `printf '…' | kaish -c 'sort'`. Without it a bare top-level builtin
    /// reading stdin has no input source and silently produces nothing.
    /// Bytes-typed (GH #176) to match `ExecContext::set_stdin`: a byte-aware
    /// builtin (`wc -c`, `cat`, `cmp`, `checksum`, …) sees binary content
    /// exactly, while a text-only builtin (`grep`, `sed`, …) still refuses it
    /// loudly at the point it asks for text (`read_stdin_to_text`), not here.
    /// Embedders that already hold a complete buffer use this; one that wants
    /// to avoid pre-draining an open process stdin should prefer
    /// `Kernel::execute_with_pipe_stdin` instead.
    pub stdin: Option<Vec<u8>>,
    /// Polled interrupt check, for embedders whose thread cannot fire
    /// `cancel_token` while execution runs — the motivating case is
    /// single-threaded wasm, where the browser's main thread flips a
    /// SharedArrayBuffer flag and this closure reads it. The kernel polls at
    /// its cancellation checkpoints (each loop iteration, among others) and,
    /// on `true`, fires its internal cancel — the same exit-130 path as
    /// `Kernel::cancel()`. Keep the closure cheap: it runs on the hot path.
    /// `None` (the default) polls nothing.
    pub interrupt: Option<std::sync::Arc<dyn Fn() -> bool + Send + Sync>>,
}

impl ExecuteOptions {
    pub fn new() -> Self {
        Self::default()
    }

    /// Replace the entire vars overlay with the given map.
    pub fn with_vars(mut self, vars: HashMap<String, Value>) -> Self {
        self.vars = vars;
        self
    }

    /// Add a single variable to the overlay (extending; last write wins).
    pub fn with_var(mut self, name: impl Into<String>, value: Value) -> Self {
        self.vars.insert(name.into(), value);
        self
    }

    /// Install a polled interrupt check (see the `interrupt` field).
    pub fn with_interrupt(
        mut self,
        check: std::sync::Arc<dyn Fn() -> bool + Send + Sync>,
    ) -> Self {
        self.interrupt = Some(check);
        self
    }

    pub fn with_timeout(mut self, timeout: Duration) -> Self {
        self.timeout = Some(timeout);
        self
    }

    pub fn with_cancel_token(mut self, token: CancellationToken) -> Self {
        self.cancel_token = Some(token);
        self
    }

    /// Run this call as if `cd path` had happened first; the prior cwd is
    /// restored on return.
    pub fn with_cwd(mut self, cwd: PathBuf) -> Self {
        self.cwd = Some(cwd);
        self
    }

    /// Set the W3C `traceparent` the kernel's execution span should parent onto.
    pub fn with_traceparent(mut self, traceparent: impl Into<String>) -> Self {
        self.traceparent = Some(traceparent.into());
        self
    }

    /// Set the W3C `tracestate` that rides alongside `traceparent`. Ignored by
    /// the kernel unless a `traceparent` is also present.
    pub fn with_tracestate(mut self, tracestate: impl Into<String>) -> Self {
        self.tracestate = Some(tracestate.into());
        self
    }

    /// Replace the entire baggage map with the given identifiers.
    pub fn with_baggage(mut self, baggage: BTreeMap<String, String>) -> Self {
        self.baggage = baggage;
        self
    }

    /// Add a single baggage identifier (extending; last write wins).
    pub fn with_baggage_entry(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.baggage.insert(key.into(), value.into());
        self
    }

    /// Set the standard input fed to this call's first stdin-reading command.
    ///
    /// Accepts anything `Into<Vec<u8>>` — a `&str`/`String` (the common text
    /// case) or a raw `Vec<u8>` (binary, GH #176) both work.
    pub fn with_stdin(mut self, stdin: impl Into<Vec<u8>>) -> Self {
        self.stdin = Some(stdin.into());
        self
    }
}
