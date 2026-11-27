//! Task-local embedder invocation identity.
//!
//! Carries a typed embedder value from `ExecuteOptions::invocation` to every
//! `KernelBackend` call made during that execution — including concurrent
//! pipeline stages, scatter workers, and `&` background jobs.
//!
//! This module does not depend on `tracing` or `opentelemetry`: propagation
//! works identically when tracing is disabled or no subscriber is installed.

use std::any::Any;
use std::future::Future;
use std::sync::Arc;

/// Type-erased embedder invocation identity stored on the active task.
pub type Invocation = Arc<dyn Any + Send + Sync>;

tokio::task_local! {
    static INVOCATION: Option<Invocation>;
}

/// Return the raw type-erased invocation identity active on the current task,
/// or `None` when none was supplied on `ExecuteOptions`.
pub fn current_invocation_raw() -> Option<Invocation> {
    INVOCATION.try_with(Clone::clone).ok().flatten()
}

/// Return the typed embedder invocation identity active on the current task.
///
/// Returns `None` if no invocation was supplied on `ExecuteOptions`, if called
/// outside a kernel execution, or if the stored value has a different concrete
/// type than `T`.
pub fn current_invocation<T: Send + Sync + 'static>() -> Option<Arc<T>> {
    current_invocation_raw().and_then(|arc| arc.downcast::<T>().ok())
}

/// Run `fut` with `invocation` bound as the active task's invocation identity.
///
/// Always installs a fresh scope — even when `invocation` is `None` — so a
/// nested `execute` call that omits an invocation never inherits a stale value
/// from an enclosing execution.
pub(crate) fn scope<F: Future>(
    invocation: Option<Invocation>,
    fut: F,
) -> impl Future<Output = F::Output> {
    INVOCATION.scope(invocation, fut)
}

/// Capture the current task's invocation identity synchronously and bind it
/// onto `fut` across a `tokio::spawn` boundary.
pub(crate) fn bind_current_invocation<F: Future>(fut: F) -> impl Future<Output = F::Output> {
    let invocation = current_invocation_raw();
    INVOCATION.scope(invocation, fut)
}
