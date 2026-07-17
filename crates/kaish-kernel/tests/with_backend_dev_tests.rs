//! `Kernel::with_backend` must expose `/dev` (DevFs) regardless of what the
//! embedder's own backend does — this is the kaijutsu bug: a read-only
//! embedder backend made `cmd > /dev/null` fail as a filesystem error
//! instead of silently discarding the write, because `/dev` was never
//! kernel-owned in the `with_backend` path.

#![cfg(feature = "localfs")]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};

/// Build a `with_backend` kernel whose entire embedder-owned root is
/// read-only — the same shape as kaijutsu's `LocalBackend::read_only("/")`.
fn read_only_backend_kernel(dir: &std::path::Path) -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", LocalFs::read_only(dir));
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |_| {})
        .expect("with_backend kernel")
}

async fn run(kernel: &Kernel, script: &str) -> (String, i64) {
    let result = kernel.execute(script).await.expect("kernel execute");
    (result.text_out().trim().to_string(), result.code)
}

#[tokio::test]
async fn dev_null_write_succeeds_under_read_only_backend() {
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = read_only_backend_kernel(dir.path());

    // Writing to /dev/null must be discarded, not rejected — /dev is
    // kernel-owned and must not depend on the embedder's backend being
    // writable.
    let (out, code) = run(&kernel, "echo hello > /dev/null").await;
    assert_eq!(code, 0, "write to /dev/null must succeed under a read-only backend: out={out:?}");
}

#[tokio::test]
async fn dev_null_reads_empty_under_with_backend() {
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = read_only_backend_kernel(dir.path());

    let (out, code) = run(&kernel, "cat /dev/null").await;
    assert_eq!(code, 0);
    assert_eq!(out, "", "reading /dev/null must be empty");
}

#[tokio::test]
async fn dev_zero_readable_under_with_backend() {
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = read_only_backend_kernel(dir.path());

    let (out, code) = run(&kernel, "head -c 8 /dev/zero | wc -c").await;
    assert_eq!(code, 0, "reading /dev/zero must succeed: out={out:?}");
    assert_eq!(out, "8", "head -c 8 /dev/zero should yield exactly 8 bytes: out={out:?}");
}

#[tokio::test]
async fn ls_dev_lists_devices_under_with_backend() {
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = read_only_backend_kernel(dir.path());

    let (out, code) = run(&kernel, "ls /dev").await;
    assert_eq!(code, 0, "ls /dev must succeed: out={out:?}");
    for name in ["null", "zero", "random", "urandom"] {
        assert!(out.contains(name), "ls /dev should list {name}: out={out:?}");
    }
}

/// A non-virtual, non-`/dev` write must still be rejected by a read-only
/// inner backend — mount-aware routing must not accidentally widen what
/// counts as "handled by the internal VFS".
#[tokio::test]
async fn non_virtual_write_still_delegates_to_read_only_backend() {
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = read_only_backend_kernel(dir.path());

    let (out, code) = run(&kernel, "echo hello > /probe.txt").await;
    assert_ne!(code, 0, "write to a real path under a read-only backend must fail: out={out:?}");
}

/// `configure_vfs` can still override the default `/dev` mount, same as it
/// can already override `/v/jobs`/`/v/blobs` — the built-in mount is set up
/// before the caller's closure runs.
#[tokio::test]
async fn configure_vfs_can_override_dev_mount() {
    let dir = tempfile::tempdir().expect("tempdir");
    let mut vfs = VfsRouter::new();
    vfs.mount("/", LocalFs::read_only(dir.path()));
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let kernel = Kernel::with_backend(
        backend,
        KernelConfig::isolated(),
        |vfs| vfs.mount("/dev", MemoryFs::new()),
        |_| {},
    )
    .expect("with_backend kernel");

    // The empty MemoryFs has no "null" entry, unlike DevFs — proves the
    // override actually took effect rather than DevFs staying mounted.
    let (out, code) = run(&kernel, "cat /dev/null").await;
    assert_ne!(code, 0, "overridden /dev should no longer be DevFs: out={out:?}");
}
