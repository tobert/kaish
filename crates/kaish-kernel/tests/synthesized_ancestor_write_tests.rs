//! A directory the VFS lists but no backend holds (an ancestor of a deeper
//! mount, under a `MemoryFs` at `/`) and what each write path says about it.

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::vfs::{MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};

/// MemoryFs at `/`, a second MemoryFs at `/home/amy/project`: `/home` and
/// `/home/amy` exist only as synthesized ancestors.
fn embedder_kernel() -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount("/home/amy/project", MemoryFs::new());
    vfs.mount("/", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    let config = KernelConfig::isolated().with_cwd("/".into());
    Kernel::with_backend(backend, config, |_| {}, |_| {}).expect("with_backend kernel")
}

#[tokio::test]
async fn redirect_into_synthesized_ancestor_names_the_fix() {
    let kernel = embedder_kernel();
    let r = kernel.execute("echo x > /home/f").await.expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert_eq!(
        r.err.trim_end(),
        "redirect: /home/f: no such file or directory; create the directory first: mkdir -p /home",
    );
    let r = kernel.execute("mkdir -p /home; echo x > /home/f; cat /home/f").await.expect("execute");
    assert_eq!((r.code, r.text_out().trim()), (0, "x"), "{r:?}");
}

// Census probe: prints what each observation says; asserts nothing.
#[tokio::test]
async fn census() {
    for script in [
        "ls /", "ls /home", "test -d /home; echo rc=$?", "[[ -d /home ]]; echo rc=$?",
        "stat /home", "cd /home; pwd", "mkdir /home", "touch /home/f", "write /home/f hi",
        "echo x > /home/f", "echo a > /a; cp /a /home/f", "echo y | tee /home/f",
        "echo x >> /home/f", "mkdir /home/d",
    ] {
        let kernel = embedder_kernel();
        let r = kernel.execute(script).await.expect("execute");
        eprintln!("CENSUS {script:?} => rc={} out={:?} err={:?}", r.code, r.text_out(), r.err);
    }
}
