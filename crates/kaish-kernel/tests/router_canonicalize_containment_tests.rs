//! A symlink inside a rooted mount must not canonicalize past that mount's
//! root, even when another mount (or the mount covering `/`) happens to
//! have something at the escaped path.
//!
//! `readlink -f`/`realpath` used to walk a path hop by hop through
//! `ctx.backend` (the router), recomputing the owning mount via
//! `find_mount` on every hop. A symlink target that walked far enough above
//! its own mount's root via `..` got re-routed through the mount table from
//! scratch instead of being refused — the escape resolved against whatever
//! mount happened to cover the folded VFS-absolute path, not the mount the
//! symlink actually lives on. `VfsRouter::canonicalize` picks the owning
//! mount once and hands the whole walk to that mount's own
//! containment-checked resolver, so the escape is refused before it ever
//! reaches the mount table again.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(feature = "localfs", unix))]

use std::path::{Path, PathBuf};
use std::sync::Arc;

use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("router-canonicalize-containment-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

/// A rooted `LocalFs` mount several components below `/`, mirroring its own
/// host path (matching `router_mount_ancestor_tests.rs`'s fixture shape) —
/// plus a `/` mount with a "secret" file at the path the escape would land
/// on if the router ever re-routed a symlink hop through the mount table.
async fn kernel_with_escape_target(base: &tempfile::TempDir) -> (Kernel, PathBuf) {
    use kaish_kernel::vfs::Filesystem;

    let root = base.path().join("project").join("fixture");
    std::fs::create_dir_all(&root).expect("mkdir project/fixture");

    // 20 levels of `..` clamps to `/` lexically however deep `root` is —
    // no need to count `root`'s own depth exactly.
    let escape_target: PathBuf = std::iter::repeat_n("..", 20).collect::<PathBuf>().join("secret");
    std::os::unix::fs::symlink(&escape_target, root.join("escape")).expect("symlink escape");

    let mut vfs = VfsRouter::new();
    vfs.mount(root.to_path_buf(), LocalFs::new(root.to_path_buf()));
    let outside = MemoryFs::new();
    // The "secret" a mount-table re-route would leak: readlink -f must
    // never print this path for a symlink that lives inside the rooted
    // mount above.
    outside
        .write(Path::new("secret"), b"leaked")
        .await
        .expect("write /secret in the root mount");
    vfs.mount("/", outside);

    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let config = KernelConfig::isolated().with_cwd(root.to_path_buf());
    let kernel = Kernel::with_backend(backend, config, |_| {}, |_| {}).expect("with_backend kernel");
    (kernel, root)
}

async fn run(kernel: &Kernel, line: &str) -> (String, String, i64) {
    let result = kernel.execute(line).await.expect("execute");
    (
        result.text_out().trim_end().to_string(),
        result.err.trim_end().to_string(),
        result.code,
    )
}

#[tokio::test]
async fn readlink_f_refuses_a_symlink_that_escapes_its_mount_root() {
    let base = tempdir();
    let (kernel, root) = kernel_with_escape_target(&base).await;

    let (out, err, code) = run(&kernel, &format!("readlink -f {}/escape", root.display())).await;
    assert_ne!(code, 0, "readlink -f must refuse an escaping symlink, got: {out:?}");
    assert!(
        !out.contains("secret"),
        "readlink -f must never print the path a mount-table re-route would leak: {out:?}"
    );
    assert!(
        err.contains("escapes root") || err.contains("No such file"),
        "expected a containment or not-found refusal, got: {err}"
    );
}

#[tokio::test]
async fn realpath_refuses_a_symlink_that_escapes_its_mount_root() {
    let base = tempdir();
    let (kernel, root) = kernel_with_escape_target(&base).await;

    let (out, _err, code) = run(&kernel, &format!("realpath {}/escape", root.display())).await;
    assert_ne!(code, 0, "realpath must refuse an escaping symlink, got: {out:?}");
    assert!(
        !out.contains("secret"),
        "realpath must never print the path a mount-table re-route would leak: {out:?}"
    );
}
