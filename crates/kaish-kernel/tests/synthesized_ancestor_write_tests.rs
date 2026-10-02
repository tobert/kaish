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
    let r = kernel
        .execute("echo x > /home/f")
        .await
        .expect("execute");
    assert_eq!(r.code, 1, "{r:?}");
    assert_eq!(
        r.err.trim_end(),
        "redirect: /home/f: no such file or directory; create the directory first: mkdir -p /home",
    );
    let r = kernel
        .execute("mkdir -p /home; echo x > /home/f; cat /home/f")
        .await
        .expect("execute");
    assert_eq!((r.code, r.text_out().trim()), (0, "x"), "{r:?}");
}

// Keep the original observation census and enforce its exit-code contract.
#[tokio::test]
async fn census() {
    for (script, expected_code) in [
        ("ls /", 0),
        ("ls /home", 0),
        ("test -d /home; echo rc=$?", 0),
        ("[[ -d /home ]]; echo rc=$?", 0),
        ("stat /home", 0),
        ("cd /home; pwd", 0),
        ("mkdir /home", 0),
        ("touch /home/f", 0),
        ("write /home/f hi", 0),
        ("echo x > /home/f", 1),
        ("echo a > /a; cp /a /home/f", 0),
        ("echo y | tee /home/f", 0),
        ("echo x >> /home/f", 1),
        ("mkdir /home/d", 0),
    ] {
        let kernel = embedder_kernel();
        let r = kernel.execute(script).await.expect("execute");
        assert_eq!(r.code, expected_code, "{script}: {r:?}");
        eprintln!(
            "CENSUS {script:?} => rc={} out={:?} err={:?}",
            r.code,
            r.text_out(),
            r.err
        );
    }
}

fn mounted_kernel(mount: &str, root: bool) -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount(mount, MemoryFs::new());
    if root {
        vfs.mount("/", MemoryFs::new());
    }
    Kernel::with_backend(
        Arc::new(LocalBackend::new(Arc::new(vfs))),
        KernelConfig::isolated().with_cwd("/".into()),
        |_| {},
        |_| {},
    )
    .unwrap()
}

#[rstest::rstest]
#[case("echo x > /home/f")]
#[case("echo x >> /home/f")]
#[case("cd /home; echo x > f")]
#[case("echo x > /home/projectish/f")]
#[case("echo x > /home")]
#[case("echo x >> /home")]
#[tokio::test]
async fn uncovered_write_names_an_actual_mount(#[case] script: &str) {
    let kernel = mounted_kernel("/home/project", false);
    let result = kernel.execute(script).await.unwrap();
    assert_eq!(result.code, 1, "{result:?}");
    assert!(
        result
            .err
            .contains("outside a mounted filesystem; write under /home/project"),
        "{result:?}"
    );
    assert!(!result.err.contains("mkdir"), "{result:?}");
    let result = kernel
        .execute("echo yes > /home/project/f; cat /home/project/f")
        .await
        .unwrap();
    assert_eq!((result.code, result.text_out().trim()), (0, "yes"));
}

#[rstest::rstest]
#[case("echo x > /home/f")]
#[case("echo x >> /home/f")]
#[case("cd /home; echo x > f")]
#[tokio::test]
async fn covered_missing_parent_names_mkdir(#[case] script: &str) {
    let kernel = mounted_kernel("/home/project", true);
    let result = kernel.execute(script).await.unwrap();
    assert_eq!(result.code, 1, "{result:?}");
    assert!(
        result.err.contains("create the directory first: mkdir -p"),
        "{result:?}"
    );
    assert!(!result.err.contains("outside a mounted"), "{result:?}");
    assert_eq!(
        kernel
            .execute("mkdir -p /home; echo yes > /home/f")
            .await
            .unwrap()
            .code,
        0
    );
}

#[tokio::test]
async fn mounted_hint_quotes_spaces() {
    let kernel = mounted_kernel("/home/my project", false);
    let result = kernel.execute("echo x > /home/f").await.unwrap();
    assert_eq!(result.code, 1);
    assert!(
        result.err.contains("write under \"/home/my project\""),
        "{result:?}"
    );
    assert_eq!(
        kernel
            .execute("echo yes > \"/home/my project/f\"")
            .await
            .unwrap()
            .code,
        0
    );
}

#[tokio::test]
async fn existing_real_ancestor_is_writable() {
    let kernel = embedder_kernel();
    assert_eq!(kernel.execute("mkdir /home").await.unwrap().code, 0);
    let result = kernel
        .execute("echo yes > /home/f; cat /home/f")
        .await
        .unwrap();
    assert_eq!((result.code, result.text_out().trim()), (0, "yes"));
}

#[tokio::test]
async fn mkdir_hint_is_a_working_command_for_spaces_and_dollars() {
    let kernel = mounted_kernel("/home $dir's/project", true);
    let script = r#"echo yes > "/home \$dir's/f""#;
    let result = kernel.execute(script).await.unwrap();
    assert_eq!(result.code, 1, "{result:?}");
    let command = result
        .err
        .trim_end()
        .split_once("create the directory first: ")
        .unwrap()
        .1;
    assert_eq!(kernel.execute(command).await.unwrap().code, 0, "{command}");
    assert_eq!(kernel.execute(script).await.unwrap().code, 0);
}

#[cfg(feature = "localfs")]
#[tokio::test]
async fn read_only_mount_is_not_recommended() {
    let directory = tempfile::tempdir().unwrap();
    let mut vfs = VfsRouter::new();
    vfs.mount(
        "/home/project",
        kaish_kernel::vfs::LocalFs::read_only(directory.path()),
    );
    vfs.mount("/home/writable", MemoryFs::new());
    let kernel = Kernel::with_backend(
        Arc::new(LocalBackend::new(Arc::new(vfs))),
        KernelConfig::isolated().with_cwd("/".into()),
        |_| {},
        |_| {},
    )
    .unwrap();
    let result = kernel.execute("echo x > /home/f").await.unwrap();
    assert!(
        result.err.contains("write under /home/writable"),
        "{result:?}"
    );
    let result = kernel.execute("echo x > /home/project/f").await.unwrap();
    assert_eq!(result.code, 1);
    assert!(result.err.contains("read-only filesystem"), "{result:?}");
    assert!(!result.err.contains("mkdir"));
}

#[cfg(feature = "localfs")]
#[tokio::test]
async fn no_writable_mount_does_not_invent_a_hint() {
    let directory = tempfile::tempdir().unwrap();
    let mut vfs = VfsRouter::new();
    vfs.mount(
        "/home/project",
        kaish_kernel::vfs::LocalFs::read_only(directory.path()),
    );
    let kernel = Kernel::with_backend(
        Arc::new(LocalBackend::new(Arc::new(vfs))),
        KernelConfig::isolated().with_cwd("/".into()),
        |vfs| {
            vfs.unmount("/v/blobs");
        },
        |_| {},
    )
    .unwrap();
    let result = kernel.execute("echo x > /home/f").await.unwrap();
    assert!(
        result.err.contains("no writable mounted path is available"),
        "{result:?}"
    );
    assert!(!result.err.contains("mkdir"));
}

#[tokio::test]
async fn root_keeps_its_specific_directory_refusal() {
    let kernel = mounted_kernel("/home/project", false);
    let result = kernel.execute("echo x > /").await.unwrap();
    assert_eq!(result.code, 1);
    assert_eq!(result.err.trim_end(), "redirect: /: is a directory");
}

#[tokio::test]
async fn overlay_ancestor_names_a_fix_its_policy_permits() {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let kernel = Kernel::with_backend(Arc::new(LocalBackend::new(Arc::new(vfs))),
        KernelConfig::isolated().with_cwd("/".into()),
        |vfs| vfs.mount("/home/project", MemoryFs::new()), |_| {}).unwrap();
    let result = kernel.execute("echo x > /home/f").await.unwrap();
    assert_eq!(result.code, 1, "{result:?}");
    assert!(result.err.contains("write under /home/project"), "{result:?}");
    assert!(!result.err.contains("mkdir"), "{result:?}");
    let result = kernel.execute("echo yes > /home/project/f; cat /home/project/f").await.unwrap();
    assert_eq!((result.code, result.text_out().trim()), (0, "yes"));
}

#[cfg(feature = "localfs")]
#[tokio::test]
async fn missing_read_only_parent_never_suggests_mkdir() {
    let directory = tempfile::tempdir().unwrap();
    let mut vfs = VfsRouter::new();
    vfs.mount("/home/project", kaish_kernel::vfs::LocalFs::read_only(directory.path()));
    let kernel = Kernel::with_backend(Arc::new(LocalBackend::new(Arc::new(vfs))),
        KernelConfig::isolated().with_cwd("/".into()), |_| {}, |_| {}).unwrap();
    let result = kernel.execute("echo x > /home/project/missing/f").await.unwrap();
    assert_eq!(result.code, 1);
    assert!(result.err.contains("read-only filesystem"), "{result:?}");
    assert!(!result.err.contains("mkdir"), "{result:?}");
}

#[tokio::test]
async fn overlay_keeps_existing_real_parent_writable() {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", MemoryFs::new());
    let kernel = Kernel::with_backend(Arc::new(LocalBackend::new(Arc::new(vfs))),
        KernelConfig::isolated().with_cwd("/".into()),
        |vfs| vfs.mount("/home/project", MemoryFs::new()), |_| {}).unwrap();
    assert_eq!(kernel.execute("write /home/existing yes").await.unwrap().code, 0);
    let result = kernel.execute("echo yes > /home/f; cat /home/f").await.unwrap();
    assert_eq!((result.code, result.text_out().trim()), (0, "yes"));
}
