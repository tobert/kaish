//! A mount point's ancestors must be navigable.
//!
//! `VfsRouter::mount_of` picks the longest matching mount, and `/` matches
//! everything. When a backend is mounted several components below `/` — the
//! common embedder shape (`kaibo`, `kaijutsu`) — the components ABOVE that
//! mount point are owned by whichever filesystem covers `/`, which has no
//! entry for them. The router answers for a path it does not own, and the
//! answer is "No such file or directory".
//!
//! In a real filesystem a mount point's ancestors necessarily exist: you
//! cannot mount at `/a/b/c` unless `/a/b` is a directory. The router
//! synthesizes the mount point itself; it must synthesize the path to it too.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(feature = "localfs", unix))]

use std::path::{Path, PathBuf};
use std::sync::Arc;

use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("router-ancestor-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// The mount root, several components below `/`, mirroring its own host path.
fn fixture_root(base: &tempfile::TempDir) -> PathBuf {
    let root = base.path().join("project").join("fixture");
    std::fs::create_dir_all(&root).expect("mkdir project/fixture");
    std::fs::write(root.join("top.txt"), "top\n").expect("write top.txt");
    root
}

fn rooted_kernel(root: &Path) -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount(root.to_path_buf(), LocalFs::read_only(root.to_path_buf()));
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let config = KernelConfig::isolated().with_cwd(root.to_path_buf());
    Kernel::with_backend(backend, config, |_| {}, |_| {}).expect("with_backend kernel")
}

async fn run(kernel: &Kernel, line: &str) -> (String, String, i64) {
    let result = kernel.execute(line).await.expect("execute");
    (
        result.text_out().trim_end().to_string(),
        result.err.trim_end().to_string(),
        result.code,
    )
}

/// Every strict ancestor of the mount point, from `/` down to its parent.
fn ancestors_of(root: &Path) -> Vec<String> {
    let mut out = Vec::new();
    let mut current = root.parent();
    while let Some(path) = current {
        out.push(path.to_string_lossy().into_owned());
        current = path.parent();
    }
    out.reverse();
    out
}

#[tokio::test]
async fn stat_answers_for_every_ancestor_of_a_mount_point() {
    let base = tempdir();
    let root = fixture_root(&base);
    let kernel = rooted_kernel(&root);

    for ancestor in ancestors_of(&root) {
        let (_out, err, code) = run(&kernel, &format!("stat {ancestor}")).await;
        assert_eq!(
            code, 0,
            "stat must answer for the mount ancestor {ancestor}: {err}"
        );
    }
}

#[tokio::test]
async fn ls_lists_an_ancestor_of_a_mount_point() {
    let base = tempdir();
    let root = fixture_root(&base);
    let kernel = rooted_kernel(&root);

    let parent = root.parent().expect("mount root has a parent");
    let (out, err, code) = run(&kernel, &format!("ls {}", parent.display())).await;
    assert_eq!(code, 0, "ls must list a mount ancestor: {err}");
    let leaf = root.file_name().expect("mount root has a name").to_string_lossy();
    assert!(
        out.contains(leaf.as_ref()),
        "listing a mount's parent must show the mount itself, got: {out:?}"
    );
}

#[tokio::test]
async fn cd_into_an_ancestor_of_a_mount_point_succeeds() {
    let base = tempdir();
    let root = fixture_root(&base);
    let kernel = rooted_kernel(&root);

    let parent = root.parent().expect("mount root has a parent");
    let (_out, err, code) = run(&kernel, &format!("cd {}", parent.display())).await;
    assert_eq!(code, 0, "cd into a mount ancestor must succeed: {err}");
}

#[tokio::test]
async fn a_file_test_sees_an_ancestor_as_a_directory() {
    let base = tempdir();
    let root = fixture_root(&base);
    let kernel = rooted_kernel(&root);

    let parent = root.parent().expect("mount root has a parent");
    let (out, err, code) = run(
        &kernel,
        &format!("if [[ -d {} ]]; then echo IS_DIR; fi", parent.display()),
    )
    .await;
    assert_eq!(code, 0, "the file test must not error: {err}");
    assert_eq!(out, "IS_DIR", "a mount ancestor must test as a directory");
}

/// Control: a path that is NOT an ancestor of any mount is still absent.
/// Synthesizing ancestors must not make the whole namespace exist.
#[tokio::test]
async fn an_unrelated_missing_path_is_still_missing() {
    let base = tempdir();
    let root = fixture_root(&base);
    let kernel = rooted_kernel(&root);

    let (_out, _err, code) = run(&kernel, "stat /definitely/not/here").await;
    assert_ne!(code, 0, "an unrelated missing path must still be missing");

    let (out, _err, code) = run(
        &kernel,
        "if [[ -d /definitely/not/here ]]; then echo IS_DIR; else echo ABSENT; fi",
    )
    .await;
    assert_eq!(code, 0);
    assert_eq!(out, "ABSENT", "a non-ancestor must not be synthesized");
}
