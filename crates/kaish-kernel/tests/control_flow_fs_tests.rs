//! Regression tests for Batch 5 (exit/control flow, filesystem, and find):
//! - Dropped `$(...)` futures restore kernel `scope` and `user_tools`.
//! - Function definitions in the last pipeline stage write back to the caller.
//! - `find` emits the root directory at depth 0 and does not treat a future
//!   mtime as age 0.
//! - `LocalFs::replace` on a directory does not double "is a directory".
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;
use std::time::{Duration, SystemTime};

#[cfg(feature = "localfs")]
use kaish_kernel::vfs::LocalFs;
use kaish_kernel::vfs::Filesystem;
use kaish_kernel::{Kernel, KernelConfig};

#[tokio::test]
async fn dropped_cmdsubst_future_restores_scope_and_functions() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap().into_arc();
    let r = kernel.execute("OUTER=before; f() { echo old; }").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);

    // Start a command substitution that mutates OUTER and redefines `f`, then
    // sleeps so we can drop the future mid-await (simulating REPL Ctrl-C).
    {
        let fut = kernel.execute("X=$(OUTER=leaked; f() { echo leaked; }; sleep 5)");
        tokio::pin!(fut);
        // Poll briefly so the substitution enters `sleep 5`, then drop `fut`.
        let _ = tokio::time::timeout(Duration::from_millis(40), &mut fut).await;
    }

    let r = kernel.execute("echo \"$OUTER\" $(f)").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert_eq!(
        r.text_out(),
        "before old\n",
        "dropped $(...) future must restore scope and user_tools"
    );
}

#[tokio::test]
async fn function_defined_in_last_pipeline_stage_writes_back() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap().into_arc();
    let r = kernel
        .execute("echo ok | { greet() { echo hello-from-pipe; }; }; greet")
        .await
        .unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert_eq!(r.text_out(), "hello-from-pipe\n");
}

#[tokio::test]
async fn find_emits_root_directory_and_handles_future_mtime() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    kernel.vfs().mkdir(Path::new("/tree")).await.unwrap();
    kernel
        .vfs()
        .write(Path::new("/tree/child.txt"), b"hi\n")
        .await
        .unwrap();

    // `find /tree` must include `/tree` itself at depth 0, like GNU find.
    let r = kernel.execute("find /tree").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    let out = r.text_out();
    let lines: Vec<&str> = out.lines().collect();
    assert!(
        lines.contains(&"/tree") && lines.contains(&"/tree/child.txt"),
        "find /tree must emit both root and child, got: {lines:?}"
    );

    // `-mindepth 1` still excludes `/tree`.
    let r = kernel.execute("find /tree -mindepth 1").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    let out = r.text_out();
    let lines: Vec<&str> = out.lines().collect();
    assert!(!lines.contains(&"/tree"), "-mindepth 1 must exclude root: {lines:?}");
    assert!(lines.contains(&"/tree/child.txt"));

    // Future mtime must NOT match `-mtime 0` or `-mtime +0`, and MUST match `-mtime -1`.
    let future = SystemTime::now() + Duration::from_secs(86400 * 2);
    kernel
        .vfs()
        .set_mtime(Path::new("/tree/child.txt"), future)
        .await
        .unwrap();

    let r0 = kernel.execute("find /tree -type f -mtime 0").await.unwrap();
    assert!(r0.ok(), "err: {}", r0.err);
    assert!(
        r0.text_out().trim().is_empty(),
        "future mtime must not match -mtime 0, got: {:?}",
        r0.text_out()
    );

    let r_neg = kernel.execute("find /tree -type f -mtime -1").await.unwrap();
    assert!(r_neg.ok(), "err: {}", r_neg.err);
    assert!(
        r_neg.text_out().contains("/tree/child.txt"),
        "future mtime must match -mtime -1, got: {:?}",
        r_neg.text_out()
    );

    let r_neg0 = kernel.execute("find /tree -type f -mtime -0").await.unwrap();
    assert!(r_neg0.ok(), "err: {}", r_neg0.err);
    assert!(
        r_neg0.text_out().contains("/tree/child.txt"),
        "future mtime must match -mtime -0 (GNU find), got: {:?}",
        r_neg0.text_out()
    );

    let r_plus0 = kernel.execute("find /tree -type f -mtime +0").await.unwrap();
    assert!(r_plus0.ok(), "err: {}", r_plus0.err);
    assert!(
        r_plus0.text_out().trim().is_empty(),
        "future mtime must not match -mtime +0, got: {:?}",
        r_plus0.text_out()
    );
}

#[cfg(feature = "localfs")]
#[tokio::test]
async fn local_fs_replace_on_directory_does_not_double_reason() {
    let dir = tempfile::tempdir().unwrap();
    let sub = dir.path().join("subdir");
    std::fs::create_dir(&sub).unwrap();
    let fs = LocalFs::new(dir.path().to_path_buf());

    let err = fs.replace(Path::new("subdir"), b"data").await.unwrap_err();
    let backend_err: kaish_types::backend::BackendError = err.into();
    let rendered = backend_err.to_string();
    assert_eq!(
        rendered.matches("is a directory").count(),
        1,
        "expected 'is a directory' once, got: {rendered:?}"
    );
}
