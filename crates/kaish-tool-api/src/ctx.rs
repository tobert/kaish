//! The trimmed execution context exposed to tools.

use std::any::Any;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use kaish_types::{OutputFormat, Value};

use crate::backend::KernelBackend;

/// The portable execution context a tool sees.
///
/// This is deliberately small: it carries only what a well-behaved,
/// out-of-tree tool needs. The kernel's full `ExecContext` implements this
/// trait; trusted in-tree builtins that need deeper state (job control,
/// streaming pipes, the dispatcher) downcast through [`ToolCtx::as_any_mut`].
///
/// `Send` is a supertrait because tool execution is async and the context is
/// held across await points whose futures must be `Send`.
pub trait ToolCtx: Send {
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

    /// Escape hatch for trusted in-tree tools: recover the concrete context.
    ///
    /// Out-of-tree tools must not rely on this — downcasting to a kernel type
    /// is exactly the coupling this trait exists to avoid. It is here so
    /// in-tree builtins needing job control / pipes / the dispatcher can keep
    /// full access without those internals leaking into the public surface.
    fn as_any(&self) -> &dyn Any;

    /// Mutable counterpart to [`ToolCtx::as_any`].
    fn as_any_mut(&mut self) -> &mut dyn Any;
}
