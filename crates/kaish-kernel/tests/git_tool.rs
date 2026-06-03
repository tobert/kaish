//! Kernel-routed integration tests for the `git` builtin.
//!
//! The builtin now lives in the `kaish-tools-git` crate and runs against the
//! portable `ToolCtx` surface; these drive it through the full kernel dispatch
//! (registration → ToolCtx → GitVfs over a real `LocalFs` worktree), which is
//! exactly the path that matters now that the tool is out-of-tree. Requires the
//! `git` capability.

#![cfg(feature = "git")]

use kaish_kernel::{Kernel, KernelConfig};

/// A REPL-mode kernel can reach real filesystem paths (passthrough), so
/// `resolve_real_path` maps the cwd to a real worktree for libgit2.
fn repl_kernel() -> Kernel {
    Kernel::new(KernelConfig::repl()).expect("failed to create kernel")
}

#[tokio::test]
async fn git_init_then_status_is_clean() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = repl_kernel();

    kernel
        .execute(&format!("cd {}", tmp.path().display()))
        .await
        .unwrap();

    let init = kernel.execute("git init").await.unwrap();
    assert!(init.ok(), "git init should succeed: {:?}", init);

    let status = kernel.execute("git status").await.unwrap();
    assert!(status.ok(), "git status should succeed: {:?}", status);
}

#[tokio::test]
async fn git_add_and_commit_roundtrip() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = repl_kernel();
    let cd = format!("cd {}", tmp.path().display());

    kernel.execute(&cd).await.unwrap();
    assert!(kernel.execute("git init").await.unwrap().ok());

    // Materialize a file in the worktree, then stage + commit it. (Written
    // directly so the test exercises git, not redirect-write semantics.)
    std::fs::write(tmp.path().join("greeting.txt"), "hello\n").unwrap();
    let add = kernel.execute("git add greeting.txt").await.unwrap();
    assert!(add.ok(), "git add should succeed: {:?}", add);

    let commit = kernel
        .execute("git commit -m 'add greeting'")
        .await
        .unwrap();
    assert!(commit.ok(), "git commit should succeed: {:?}", commit);

    let log = kernel.execute("git log").await.unwrap();
    assert!(log.ok(), "git log should succeed: {:?}", log);
    assert!(
        log.text_out().contains("add greeting"),
        "log should show the commit message: {}",
        log.text_out()
    );
}

#[tokio::test]
async fn git_status_outside_repo_errors() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = repl_kernel();

    kernel
        .execute(&format!("cd {}", tmp.path().display()))
        .await
        .unwrap();

    // No `git init` — status must fail loudly rather than pretend success.
    let status = kernel.execute("git status").await.unwrap();
    assert!(!status.ok(), "git status outside a repo should fail: {:?}", status);
}
