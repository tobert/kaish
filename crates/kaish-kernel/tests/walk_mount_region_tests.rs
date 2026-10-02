//! Recursive walks stay in the mount region they start in.
//!
//! `grep -r`, `find`, `ls -R`, `tree`, the `glob` builtin, and bare-glob
//! expansion all walk through the kernel backend. A walk from `/` does not
//! descend into `/v`, `/tmp`, or `/dev`: it reaches those mount points and
//! stops. Naming a mount walks it, including the mounts nested under it.
//! `--cross-mounts` on a walking builtin, or `set -o crossmounts`, crosses.
//!
//! The isolated kernel mounts `/`, `/tmp`, `/v`, and `/dev`, with `/v/jobs`
//! and `/v/bin` nested in `/v`. The `with_backend` tests use the embedder
//! shape: the embedder's mounts (`/`, `/r`, `/v/cas`) behind one backend, and
//! kaish's own mounts under a synthesized `/v`.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;
use std::sync::Arc;

use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};

const SEED: &str = "mkdir -p /data /v/notes; \
    echo needle > /data/a.txt; \
    echo needle > /v/notes/b.txt; \
    echo needle > /tmp/c.txt";

async fn isolated() -> Kernel {
    let kernel = Kernel::new(KernelConfig::isolated()).expect("isolated kernel");
    let seeded = kernel.execute(SEED).await.expect("seed");
    assert_eq!(seeded.code, 0, "seed failed: {}", seeded.text_out());
    kernel
}

async fn run(kernel: &Kernel, script: &str) -> (Vec<String>, i64) {
    let result = kernel.execute(script).await.expect("kernel execute");
    let lines = result
        .text_out()
        .lines()
        .map(|line| line.trim().to_string())
        .filter(|line| !line.is_empty())
        .collect();
    (lines, result.code)
}

fn has(lines: &[String], text: &str) -> bool {
    lines.iter().any(|line| line == text)
}

fn mentions(lines: &[String], text: &str) -> bool {
    lines.iter().any(|line| line.contains(text))
}

#[tokio::test]
async fn grep_r_from_root_stays_in_the_root_region() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "grep -rl needle /").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(has(&out, "/data/a.txt"), "{out:?}");
    assert!(!mentions(&out, "b.txt"), "walked into /v: {out:?}");
    assert!(!mentions(&out, "c.txt"), "walked into /tmp: {out:?}");
}

#[tokio::test]
async fn grep_r_naming_a_mount_walks_it() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "grep -rl needle /v").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(has(&out, "/v/notes/b.txt"), "{out:?}");
}

#[tokio::test]
async fn grep_cross_mounts_flag_walks_every_region() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "grep -rl --cross-mounts needle /").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(has(&out, "/data/a.txt"), "{out:?}");
    assert!(has(&out, "/v/notes/b.txt"), "{out:?}");
    assert!(has(&out, "/tmp/c.txt"), "{out:?}");
}

#[tokio::test]
async fn set_o_crossmounts_walks_every_region() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "set -o crossmounts; grep -rl needle /").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(has(&out, "/v/notes/b.txt"), "{out:?}");
    assert!(has(&out, "/tmp/c.txt"), "{out:?}");

    let (out, _) = run(&kernel, "set +o crossmounts; grep -rl needle /").await;
    assert!(!mentions(&out, "b.txt"), "+o crossmounts did not restore: {out:?}");
}

#[tokio::test]
async fn set_o_lists_crossmounts_off_by_default() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "set -o").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(
        out.iter().any(|line| {
            let cells: Vec<&str> = line.split_whitespace().collect();
            cells == ["crossmounts", "off"]
        }),
        "{out:?}"
    );
}

#[tokio::test]
async fn find_from_root_prints_mount_points_without_entering_them() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "find /").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(has(&out, "/data/a.txt"), "{out:?}");
    assert!(has(&out, "/v"), "the mount point itself is listed: {out:?}");
    assert!(has(&out, "/tmp"), "the mount point itself is listed: {out:?}");
    assert!(!mentions(&out, "/v/"), "walked into /v: {out:?}");
    assert!(!mentions(&out, "/tmp/"), "walked into /tmp: {out:?}");

    let (out, _) = run(&kernel, "find / --cross-mounts -name '*.txt'").await;
    assert!(has(&out, "/v/notes/b.txt"), "{out:?}");
}

#[tokio::test]
async fn ls_r_from_root_does_not_enter_mounts() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "ls -R /").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(mentions(&out, "a.txt"), "{out:?}");
    assert!(!mentions(&out, "b.txt"), "walked into /v: {out:?}");
    assert!(!mentions(&out, "c.txt"), "walked into /tmp: {out:?}");

    let (out, _) = run(&kernel, "ls -R --cross-mounts /").await;
    assert!(mentions(&out, "b.txt"), "{out:?}");
}

#[tokio::test]
async fn tree_from_root_does_not_enter_mounts() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "tree /").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(mentions(&out, "a.txt"), "{out:?}");
    assert!(!mentions(&out, "b.txt"), "walked into /v: {out:?}");

    let (out, _) = run(&kernel, "tree --cross-mounts /").await;
    assert!(mentions(&out, "b.txt"), "{out:?}");
}

#[tokio::test]
async fn glob_builtin_globstar_stays_in_the_root_region() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "glob '/**/*.txt'").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(mentions(&out, "a.txt"), "{out:?}");
    assert!(!mentions(&out, "b.txt"), "walked into /v: {out:?}");

    let (out, _) = run(&kernel, "glob '/v/**/*.txt'").await;
    assert!(mentions(&out, "b.txt"), "{out:?}");

    let (out, _) = run(&kernel, "glob --cross-mounts '/**/*.txt'").await;
    assert!(mentions(&out, "b.txt"), "{out:?}");
}

#[tokio::test]
async fn bare_glob_expansion_stays_in_the_root_region() {
    let kernel = isolated().await;
    let (out, code) = run(&kernel, "echo /**/*.txt").await;
    assert_eq!(code, 0, "{out:?}");
    let words: Vec<&str> = out.iter().flat_map(|l| l.split_whitespace()).collect();
    assert_eq!(words, ["/data/a.txt"], "{out:?}");

    let (out, _) = run(&kernel, "echo /v/**/*.txt").await;
    let words: Vec<&str> = out.iter().flat_map(|l| l.split_whitespace()).collect();
    assert_eq!(words, ["/v/notes/b.txt"], "{out:?}");

    let (out, _) = run(&kernel, "set -o crossmounts; echo /**/*.txt").await;
    let words: Vec<&str> = out.iter().flat_map(|l| l.split_whitespace()).collect();
    assert!(words.contains(&"/v/notes/b.txt"), "{out:?}");
}

/// The embedder shape: one backend holding the embedder's mounts `/`, `/r`,
/// and `/v/cas`, under kaish's overlay with `/v/jobs`, `/v/blobs`, `/v/bin`,
/// and `/dev`.
async fn embedded() -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    vfs.mount("/r", MemoryFs::new());
    vfs.mount("/v/cas", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let kernel = Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |_| {})
        .expect("with_backend kernel");
    let seeded = kernel
        .execute(
            "mkdir -p /data /r/laptop; \
             echo needle > /data/a.txt; \
             echo needle > /r/laptop/share.txt; \
             echo needle > /v/cas/blob.txt",
        )
        .await
        .expect("seed");
    assert_eq!(seeded.code, 0, "seed failed: {}", seeded.text_out());
    kernel
}

#[tokio::test]
async fn embedded_walk_from_root_skips_embedder_and_kaish_mounts() {
    let kernel = embedded().await;
    let (out, code) = run(&kernel, "grep -rl needle /").await;
    assert_eq!(code, 0, "{out:?}");
    assert_eq!(out, ["/data/a.txt"]);

    // kaish's own `/dev` is a region too.
    let (out, code) = run(&kernel, "find /").await;
    assert_eq!(code, 0, "{out:?}");
    assert!(has(&out, "/dev"), "{out:?}");
    assert!(!mentions(&out, "/dev/"), "walked into /dev: {out:?}");
}

#[tokio::test]
async fn embedded_walk_naming_v_walks_every_mount_under_it() {
    let kernel = embedded().await;
    // `/v` is synthesized; the embedder's `/v/cas` and kaish's `/v/bin` are
    // both mounts under it, and both are in the `/v` region.
    let (cas, code) = run(&kernel, "grep -rl needle /v").await;
    assert_eq!(code, 0, "{cas:?}");
    assert_eq!(cas, ["/v/cas/blob.txt"]);
    let (bin, _) = run(&kernel, "find /v -name echo").await;
    assert!(has(&bin, "/v/bin/echo"), "{bin:?}");
}

#[tokio::test]
async fn embedded_walk_naming_r_walks_the_share() {
    let kernel = embedded().await;
    let (out, code) = run(&kernel, "grep -rl needle /r").await;
    assert_eq!(code, 0, "{out:?}");
    assert_eq!(out, ["/r/laptop/share.txt"]);
}

#[test]
fn walk_boundaries_default_reports_mount_points() {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    vfs.mount("/r", MemoryFs::new());
    let backend = LocalBackend::new(Arc::new(vfs));
    let points = backend.walk_boundaries();
    assert!(points.iter().any(|p| p == Path::new("/r")), "{points:?}");
}
