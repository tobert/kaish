//! An embedder-supplied `JobManager` outlives the kernel that ran the job.
//!
//! `Kernel::new`/`Kernel::with_backend` each built their own `JobManager`, so
//! an embedder that materializes a fresh kernel per call (kaijutsu builds one
//! per tool call) lost every `cmd &` job the moment that kernel dropped — the
//! next call saw an empty job table. `KernelConfig::with_job_manager` lets the
//! embedder own the manager and hand the same one to every kernel.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;
use std::time::Duration;

use kaish_kernel::scheduler::{JobId, JobManager};
use kaish_kernel::{Kernel, KernelConfig};

fn config(jobs: Option<&Arc<JobManager>>) -> KernelConfig {
    let base = KernelConfig::isolated().with_kill_grace(Duration::from_millis(50));
    match jobs {
        Some(jobs) => base.with_job_manager(jobs.clone()),
        None => base,
    }
}

/// The whole point: start a job under one kernel, drop that kernel, and read
/// the job from the next one.
#[tokio::test]
async fn a_job_started_by_one_kernel_is_visible_to_the_next() {
    let jobs = Arc::new(JobManager::new());

    let first = Kernel::new(config(Some(&jobs))).unwrap();
    first.execute("sleep 3600 &").await.expect("spawn failed");
    assert!(jobs.exists(JobId(1)).await, "the shared manager must see the job immediately");
    drop(first);

    let second = Kernel::new(config(Some(&jobs))).unwrap();
    assert!(
        second.jobs().exists(JobId(1)).await,
        "the job must survive the kernel that started it — that is the injection point's reason to exist"
    );
    assert_eq!(
        second.jobs().get_command(JobId(1)).await.as_deref(),
        Some("sleep 3600"),
        "the second kernel reads the first kernel's command string, not a placeholder"
    );

    // Job ids come from the manager, so a shared manager must not restart at 1
    // and collide with the job the first kernel registered.
    second.execute("sleep 3600 &").await.expect("spawn failed");
    let ids: Vec<u64> = second.jobs().list_ids().await.into_iter().map(|id| id.0).collect();
    assert_eq!(ids, vec![1, 2], "a shared manager keeps one id space across kernels");

    second.cancel_all_jobs().await;
}

/// The injected manager is the same object, not a copy: `Kernel::jobs()`
/// hands back the very `Arc` the embedder supplied.
#[tokio::test]
async fn kernel_jobs_returns_the_injected_manager() {
    let jobs = Arc::new(JobManager::new());
    let kernel = Kernel::new(config(Some(&jobs))).unwrap();
    assert!(
        Arc::ptr_eq(&jobs, &kernel.jobs()),
        "the kernel must adopt the supplied manager, not clone its contents into a new one"
    );
}

/// Default stays default: with no manager supplied, two kernels are still
/// independent — this is the non-breaking half of the change.
#[tokio::test]
async fn without_injection_each_kernel_still_gets_its_own_manager() {
    let first = Kernel::new(config(None)).unwrap();
    first.execute("sleep 3600 &").await.expect("spawn failed");

    let second = Kernel::new(config(None)).unwrap();
    assert!(
        !second.jobs().exists(JobId(1)).await,
        "an un-injected kernel must not see another kernel's jobs"
    );
    assert!(!Arc::ptr_eq(&first.jobs(), &second.jobs()));

    first.cancel_all_jobs().await;
}

/// `with_backend` is the surface kaijutsu actually builds through, so it must
/// honor the injection too — not just `Kernel::new`.
#[tokio::test]
async fn with_backend_honors_the_injected_manager() {
    use kaish_kernel::backend::LocalBackend;
    use kaish_kernel::vfs::{MemoryFs, VfsRouter};

    let jobs = Arc::new(JobManager::new());
    let mut backend_vfs = VfsRouter::new();
    backend_vfs.mount("/", MemoryFs::new());
    let backend = Arc::new(LocalBackend::new(Arc::new(backend_vfs)));
    let kernel = Kernel::with_backend(backend, config(Some(&jobs)), |_| {}, |_| {}).unwrap();
    assert!(Arc::ptr_eq(&jobs, &kernel.jobs()));

    kernel.execute("sleep 3600 &").await.expect("spawn failed");
    assert!(jobs.exists(JobId(1)).await, "with_backend's jobs land in the embedder's manager");

    kernel.cancel_all_jobs().await;
}
