//! `Kernel::with_backend` must let an embedder mount its own content under `/v`
//! without kaish shadowing it. An *unclaimed* `/v/*` path falls through to the
//! embedder's backend (no more blanket `/v` reservation), and a shared parent
//! like `/v` lists the *union* of kaish's own mounts and the embedder's. This
//! is the `/v` overlay-tuning fix — the prerequisite for kaijutsu consolidating
//! its virtual filesystems under `/v`. It also fixes the pre-existing papercuts
//! where `ls /v` returned nothing and `ls /` dropped `dev`.

#![cfg(feature = "localfs")]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::vfs::{LocalFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};

/// Build a `with_backend` kernel over a read-only embedder root at `/` —
/// kaijutsu's shape (`LocalBackend::read_only("/")`), the embedder owning the
/// whole non-`/v/*` namespace and, now, its own leaves under `/v`.
fn kernel_over(dir: &std::path::Path) -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", LocalFs::read_only(dir));
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |_| {})
        .expect("with_backend kernel")
}

/// An embedder root that serves a CAS blob at `/v/cas/blob.bin` — the embedder
/// putting its own storage under `/v`, which kaish used to shadow to NotFound.
fn seed_v_cas() -> tempfile::TempDir {
    let dir = tempfile::tempdir().expect("tempdir");
    let cas = dir.path().join("v").join("cas");
    std::fs::create_dir_all(&cas).expect("create v/cas");
    std::fs::write(cas.join("blob.bin"), b"cas data").expect("write blob");
    dir
}

async fn run(kernel: &Kernel, script: &str) -> (String, i64) {
    let result = kernel.execute(script).await.expect("kernel execute");
    (result.text_out().trim().to_string(), result.code)
}

fn has_token(out: &str, name: &str) -> bool {
    out.split_whitespace().any(|t| t == name)
}

#[tokio::test]
async fn unclaimed_v_path_reaches_embedder_backend() {
    let dir = seed_v_cas();
    let kernel = kernel_over(dir.path());

    // Nothing is mounted at /v/cas on kaish's side, so this must reach the
    // embedder's read-only root instead of the old NotFound reservation.
    let (out, code) = run(&kernel, "cat /v/cas/blob.bin").await;
    assert_eq!(code, 0, "cat /v/cas/blob.bin must reach the embedder: out={out:?}");
    assert_eq!(out, "cas data");
}

#[tokio::test]
async fn ls_v_unions_kaish_and_embedder_mounts() {
    let dir = seed_v_cas();
    let kernel = kernel_over(dir.path());

    let (out, code) = run(&kernel, "ls /v").await;
    assert_eq!(code, 0, "ls /v must succeed: out={out:?}");
    // kaish's own mounts...
    assert!(has_token(&out, "jobs"), "ls /v should list kaish jobs: out={out:?}");
    assert!(has_token(&out, "blobs"), "ls /v should list kaish blobs: out={out:?}");
    // ...unioned with the embedder's /v/cas.
    assert!(has_token(&out, "cas"), "ls /v should list embedder cas: out={out:?}");
}

#[tokio::test]
async fn v_synthesized_when_embedder_has_no_v() {
    // Embedder root is empty — nothing under /v at all. `/v` must still be a
    // navigable directory listing kaish's own mounts (synthesized ancestor).
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = kernel_over(dir.path());

    let (out, code) = run(&kernel, "test -d /v && ls /v").await;
    assert_eq!(code, 0, "`/v` must exist as a directory: out={out:?}");
    assert!(has_token(&out, "jobs"), "ls /v should list kaish jobs: out={out:?}");
    assert!(!has_token(&out, "cas"), "embedder contributes nothing here: out={out:?}");
}

#[tokio::test]
async fn ls_root_shows_v_and_dev() {
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = kernel_over(dir.path());

    // The generalized shared-parent union at `/` shows every kaish top-level
    // mount root, not just a hardcoded `v` (the old code dropped `dev`).
    let (out, code) = run(&kernel, "ls /").await;
    assert_eq!(code, 0, "ls / must succeed: out={out:?}");
    assert!(has_token(&out, "v"), "ls / should show v: out={out:?}");
    assert!(has_token(&out, "dev"), "ls / should show dev: out={out:?}");
}

/// The fall-through must not widen writability: an unclaimed `/v/*` path routes
/// to the *embedder's* backend, so a read-only embedder still rejects the write
/// (its gate applies, not a kaish scratch overlay).
#[tokio::test]
async fn unclaimed_v_write_respects_read_only_embedder() {
    let dir = seed_v_cas();
    let kernel = kernel_over(dir.path());

    let (out, code) = run(&kernel, "echo hi > /v/cas/new.bin").await;
    assert_ne!(code, 0, "write under a read-only embedder /v must fail: out={out:?}");
}

/// `rm -rf /v` must *refuse* — `/v` is a synthesized directory holding kaish's
/// own mounts, which don't live on the embedder's backend. Before the polish it
/// delegated straight to the embedder and could destroy the embedder's real
/// `/v` content while leaving the kaish mounts (a misleading half-delete). Now
/// it fails with a clear error and `/v` still lists the kaish mounts.
#[tokio::test]
async fn rm_rf_of_synthesized_v_is_refused() {
    let dir = seed_v_cas();
    let kernel = kernel_over(dir.path());

    let (out, code) = run(&kernel, "rm -rf /v").await;
    assert_ne!(code, 0, "rm -rf /v must fail: out={out:?}");
    assert!(
        !out.to_lowercase().contains("no such file"),
        "must be a clear refusal, not a misleading not-found: out={out:?}"
    );

    // The kaish mounts under /v are untouched and still listable.
    let (ls, code) = run(&kernel, "ls /v").await;
    assert_eq!(code, 0, "ls /v still works after the refusal: out={ls:?}");
    assert!(ls.split_whitespace().any(|t| t == "jobs"), "kaish mount survived: out={ls:?}");
}

/// `mkdir /v` reports the directory already exists, not a confusing `NotFound`
/// from an inner delegation.
#[tokio::test]
async fn mkdir_of_synthesized_v_reports_exists() {
    let dir = tempfile::tempdir().expect("tempdir");
    let kernel = kernel_over(dir.path());

    let (out, code) = run(&kernel, "mkdir /v").await;
    assert_ne!(code, 0, "mkdir /v must fail (it exists): out={out:?}");
    assert!(
        !out.to_lowercase().contains("no such file"),
        "must not be a misleading not-found: out={out:?}"
    );
}
