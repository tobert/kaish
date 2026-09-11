//! The trimmed execution context exposed to tools.

use std::any::Any;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;

use async_trait::async_trait;
use kaish_types::{OutputFormat, Value};

use crate::backend::KernelBackend;

/// RAII guard returned by [`ToolCtx::patient`].
///
/// While held, the kernel's script-level timeout watchdog is suspended for
/// this execution: the script clock freezes and the guard's own budget
/// governs instead. Dropping the guard resumes the script clock with the
/// remaining time it had at acquire.
///
/// The inner box is the kernel's hold object; its `Drop` does the restore.
/// An *inert* guard (no watchdog running — e.g. the kernel has no script
/// timeout, or a non-kernel test context) holds nothing and drops as a no-op.
pub struct PatientGuard {
    hold: Option<Box<dyn Any + Send>>,
}

impl PatientGuard {
    /// A guard that does nothing — for contexts without a watchdog.
    pub fn inert() -> Self {
        Self { hold: None }
    }

    /// Wrap a kernel hold object whose `Drop` restores the watchdog.
    pub fn held(hold: Box<dyn Any + Send>) -> Self {
        Self { hold: Some(hold) }
    }

    /// Whether this guard actually suspended a watchdog.
    pub fn is_active(&self) -> bool {
        self.hold.is_some()
    }
}

/// The portable execution context a tool sees.
///
/// This is deliberately small: it carries only what a well-behaved,
/// out-of-tree tool needs. The kernel's full `ExecContext` implements this
/// trait; trusted in-tree builtins that need deeper state (job control,
/// streaming pipes, the dispatcher) downcast through [`ToolCtx::as_any_mut`].
///
/// `Send + Sync` are supertraits because tool execution is async: a `&dyn
/// ToolCtx` shared with an async helper is held across await points, and for
/// the resulting future to be `Send` the referent must be `Sync`. The kernel's
/// `ExecContext` already satisfies both.
///
/// `#[async_trait]` desugars the `async fn`s below into boxed futures; every
/// already-synchronous method is untouched.
/// Restricts `ToolCtx` to the kernel's own execution context.
///
/// A tool author *receives* a `ToolCtx`; nobody outside the kernel implements
/// one. Sealing says so in the type system, and the kernel's builtins depend
/// on it: each one narrows the `&mut dyn ToolCtx` it is handed back to the
/// concrete `ExecContext` it needs for pipes, jobs, and the dispatcher. With
/// exactly one implementor that narrowing cannot fail, which is what lets it
/// assert instead of inventing an exit code for a case that cannot happen.
///
/// `#[doc(hidden)]`: reachable for the kernel's own `impl`, kept off the
/// documented surface so it never reads as an extension point.
#[doc(hidden)]
pub mod sealed {
    /// Implemented only by the kernel's `ExecContext`.
    pub trait Sealed {}
}

#[async_trait]
pub trait ToolCtx: sealed::Sealed + Send + Sync {
    /// The backend for file I/O and tool dispatch.
    ///
    /// Tools reach the VFS (and re-dispatch other tools) through this handle.
    fn backend(&self) -> &Arc<dyn KernelBackend>;

    /// The current working directory, as a VFS path.
    fn cwd(&self) -> &Path;

    /// Resolve a (possibly relative) path against the cwd, normalizing `.`
    /// and `..` lexically. Never touches the real filesystem.
    fn resolve_path(&self, path: &str) -> PathBuf;

    /// Read a variable from the current scope, cloned.
    ///
    /// Returns `None` if the name is unset. Tools use this for configuration
    /// supplied by the frontend (e.g. `HOSTNAME`).
    fn var(&self, name: &str) -> Option<Value>;

    /// Set a variable in the current scope.
    fn set_var(&mut self, name: &str, value: Value);

    /// Set the per-execution output format override (e.g. from `--json`).
    ///
    /// The dispatcher reads this after `execute()` returns and applies the
    /// format to the result.
    fn set_output_format(&mut self, format: OutputFormat);

    /// Suspend the script-level timeout watchdog while the returned guard is
    /// held, bounding the patient operation by `budget` instead.
    ///
    /// For tools that legitimately outlive a script timeout (model/provider
    /// calls that run minutes): while the guard is held the script clock
    /// freezes and the watchdog fires only if the hold outlives `budget`.
    /// On drop the script clock resumes with the remaining time it had at
    /// acquire. Only Rust tool code can obtain the guard — script code has no
    /// path to it, so the script-level budget keeps its teeth.
    ///
    /// Cancellation stays live while suspended: `Kernel::cancel()` and the
    /// embedder token fire immediately — only the *timer* pauses. A patient
    /// tool must still `select!` its wait against the cancellation token.
    ///
    /// The explicit `timeout` builtin is **not** suspended: a user-requested
    /// bound on a command keeps its teeth regardless of patient holds.
    ///
    /// The default implementation returns an inert guard (no watchdog to
    /// suspend); the kernel's context overrides it.
    fn patient(&self, budget: Duration) -> PatientGuard {
        let _ = budget;
        PatientGuard::inert()
    }

    /// Escape hatch for trusted in-tree tools: recover the concrete context.
    ///
    /// Out-of-tree tools must not rely on this — downcasting to a kernel type
    /// is exactly the coupling this trait exists to avoid. It is here so
    /// in-tree builtins needing job control / pipes / the dispatcher can keep
    /// full access without those internals leaking into the public surface.
    ///
    /// `#[doc(hidden)]`: present for in-tree use but deliberately kept off the
    /// documented public surface so it doesn't advertise itself as a supported
    /// downcast hatch.
    #[doc(hidden)]
    fn as_any(&self) -> &dyn Any;

    /// Mutable counterpart to [`ToolCtx::as_any`].
    #[doc(hidden)]
    fn as_any_mut(&mut self) -> &mut dyn Any;
}
