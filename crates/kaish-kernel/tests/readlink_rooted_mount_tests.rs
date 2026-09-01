//! `readlink -f` / `realpath` on a LocalFs mount rooted below `/`.
//!
//! Reproduces a bug reported against v0.17.0 by the kaibo project: a kernel
//! shape with `LocalFs` mounted read-only at a deep VFS path (mirroring its
//! own host directory, several path components below `/`) and `MemoryFs` at
//! `/` — the common embedder pattern (`kaijutsu`, `kaibo`). `readlink -f`
//! failed on every operand, reporting the FIRST PATH COMPONENT of the mount
//! root as "No such file or directory", because `canonicalize_path_allow_missing_final`
//! walks every component of the VFS path through `lstat`, including the
//! components ABOVE the mount point that no single backend owns.
//!
//! Bare `readlink` (no `-f`) was unaffected — it does one `lstat` on the
//! full, already-mount-relative path, never walking ancestors.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
// Symlinks are unix-only; real FS via localfs feature.
#![cfg(all(feature = "localfs", unix))]

use std::os::unix::fs::symlink;
use std::path::Path;
use std::sync::Arc;

use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};

fn tempdir() -> tempfile::TempDir {
    // Several path components deep under CARGO_TARGET_TMPDIR (itself deep),
    // so the mount root is not adjacent to `/` — the shape that reproduces
    // the bug. `fixture_root` joins on two more components below this.
    tempfile::Builder::new()
        .prefix("readlink-rooted-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// The mount's VFS path AND host root: several components below `/`, and,
/// per the report, the mount path mirrors the host path (the same string
/// used as both the VFS prefix and the real directory) — the common
/// embedder pattern of projecting a project's own absolute host path
/// straight into the VFS namespace.
fn fixture_root(base: &tempfile::TempDir) -> std::path::PathBuf {
    let root = base.path().join("project").join("fixture");
    std::fs::create_dir_all(&root).expect("mkdir project/fixture");
    root
}

/// LocalFs read-only at `fixture_root`, mounted at that SAME path in VFS
/// space; MemoryFs at `/` — the kaibo-reported shape.
fn rooted_kernel(root: &Path) -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount(root.to_path_buf(), LocalFs::read_only(root.to_path_buf()));
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let config = KernelConfig::isolated().with_cwd(root.to_path_buf());
    Kernel::with_backend(backend, config, |_| {}, |_| {}).expect("with_backend kernel")
}

/// Control: the same fixture layout with LocalFs mounted at VFS root `/`
/// (the ordinary, unrooted shape existing tests already cover). Must keep
/// passing — proves the fix does not regress the common case.
fn unrooted_kernel(root: &Path) -> Kernel {
    let config = KernelConfig::repl()
        .with_cwd(root.to_path_buf())
        .with_trash(false);
    Kernel::new(config).expect("kernel")
}

async fn run(kernel: &Kernel, script: &str) -> (String, String, i64) {
    let r = kernel.execute(script).await.expect("kernel execute");
    (r.text_out().trim().to_string(), r.err.clone(), r.code)
}

fn seed(root: &Path) {
    std::fs::create_dir_all(root.join("d")).unwrap();
    std::fs::write(root.join("d/a.txt"), "content").unwrap();
    std::fs::write(root.join("top.txt"), "top-level").unwrap();
    symlink("d/a.txt", root.join("link.txt")).unwrap();
    symlink("nosuchtarget", root.join("dangling")).unwrap();
}

// ---------------------------------------------------------------------------
// Rooted mount: all five reported cases
// ---------------------------------------------------------------------------

#[tokio::test]
async fn rooted_bare_readlink_on_symlink_works() {
    let base = tempdir();
    let root = fixture_root(&base);
    seed(&root);
    let k = rooted_kernel(&root);

    let (out, err, code) = run(&k, "readlink link.txt").await;
    assert_eq!(code, 0, "bare readlink should succeed: err={err}");
    assert_eq!(out, "d/a.txt");
}

#[tokio::test]
async fn rooted_readlink_f_on_symlink_resolves() {
    let base = tempdir();
    let root = fixture_root(&base);
    seed(&root);
    let k = rooted_kernel(&root);

    let (out, err, code) = run(&k, "readlink -f link.txt").await;
    assert_eq!(code, 0, "readlink -f on a symlink should succeed: err={err}");
    let expected = root.join("d/a.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

#[tokio::test]
async fn rooted_readlink_f_on_regular_file_resolves() {
    let base = tempdir();
    let root = fixture_root(&base);
    seed(&root);
    let k = rooted_kernel(&root);

    let (out, err, code) = run(&k, "readlink -f top.txt").await;
    assert_eq!(code, 0, "readlink -f on a plain regular file should succeed: err={err}");
    let expected = root.join("top.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

#[tokio::test]
async fn rooted_readlink_f_on_dangling_link_resolves() {
    let base = tempdir();
    let root = fixture_root(&base);
    seed(&root);
    let k = rooted_kernel(&root);

    let (out, err, code) = run(&k, "readlink -f dangling").await;
    assert_eq!(code, 0, "readlink -f on a dangling link should succeed (GNU allows a missing final target): err={err}");
    let expected = root.join("nosuchtarget").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

#[tokio::test]
async fn rooted_readlink_f_on_missing_file_resolves() {
    let base = tempdir();
    let root = fixture_root(&base);
    seed(&root);
    let k = rooted_kernel(&root);

    let (out, err, code) = run(&k, "readlink -f nosuchfile").await;
    assert_eq!(code, 0, "readlink -f on a missing final component should succeed: err={err}");
    let expected = root.join("nosuchfile").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

#[tokio::test]
async fn rooted_readlink_f_link_escaping_root_is_refused() {
    // A symlink inside the root whose target is an absolute host path
    // outside the mount's own root. Containment must still be refused —
    // fixing the ancestor-walk bug must not open this hole.
    let base = tempdir();
    let root = fixture_root(&base);
    seed(&root);
    let outside = tempfile::tempdir().expect("outside tempdir");
    std::fs::write(outside.path().join("secret.txt"), "s").unwrap();
    symlink(outside.path().join("secret.txt"), root.join("escape.txt")).unwrap();

    let k = rooted_kernel(&root);
    let (out, err, code) = run(&k, "readlink -f escape.txt").await;
    assert_ne!(
        code, 0,
        "readlink -f through a link escaping the mount root must be refused, got out={out:?}"
    );
    // Specifically "escape" — not the ancestor-walk bug's "No such file or
    // directory" wearing a different path, which would pass here for the
    // wrong reason.
    assert!(err.contains("escape"), "error should name the escape, got: {err}");
}

#[tokio::test]
async fn rooted_realpath_on_symlink_resolves() {
    let base = tempdir();
    let root = fixture_root(&base);
    seed(&root);
    let k = rooted_kernel(&root);

    let (out, err, code) = run(&k, "realpath link.txt").await;
    assert_eq!(code, 0, "realpath on a symlink should succeed: err={err}");
    let expected = root.join("d/a.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}

// ---------------------------------------------------------------------------
// Control: unrooted (LocalFs at VFS `/`) must keep working
// ---------------------------------------------------------------------------

#[tokio::test]
async fn unrooted_readlink_f_on_symlink_still_resolves() {
    let base = tempdir();
    let root = fixture_root(&base);
    seed(&root);
    let k = unrooted_kernel(&root);

    let (out, err, code) = run(&k, "readlink -f link.txt").await;
    assert_eq!(code, 0, "control (unrooted) readlink -f must still pass: err={err}");
    let expected = root.join("d/a.txt").to_string_lossy().into_owned();
    assert_eq!(out, expected);
}
