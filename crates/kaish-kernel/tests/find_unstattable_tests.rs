//! `find` tests that need stat data are false for an entry whose stat
//! failed. The failure is reported on stderr and exits 1; the walk goes on.
//! A directory with mode 0o444 lists its entries but refuses `lstat` on them.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(feature = "localfs", unix))]

use std::os::unix::fs::PermissionsExt;
use std::path::Path;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};

fn tempdir() -> tempfile::TempDir {
    tempfile::Builder::new()
        .prefix("find-unstattable-")
        .tempdir_in(env!("CARGO_TARGET_TMPDIR"))
        .expect("tempdir under CARGO_TARGET_TMPDIR")
}

fn kernel_at(dir: &Path) -> Kernel {
    let config = KernelConfig::repl()
        .with_cwd(dir.to_path_buf())
        .with_trash(false);
    Kernel::new(config).expect("kernel")
}

async fn run(kernel: &Kernel, script: &str) -> ExecResult {
    kernel.execute(script).await.expect("kernel execute")
}

/// `ok.txt` is readable; `locked/` cannot be listed or stat'ed into.
/// Returns None when permissions do not restrict this user (root).
fn fixture(root: &Path) -> Option<std::path::PathBuf> {
    std::fs::write(root.join("ok.txt"), "data").unwrap();
    let locked = root.join("locked");
    std::fs::create_dir(&locked).unwrap();
    std::fs::write(locked.join("inner.txt"), "data").unwrap();
    std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o444)).unwrap();
    if std::fs::symlink_metadata(locked.join("inner.txt")).is_ok() {
        std::fs::set_permissions(&locked, std::fs::Permissions::from_mode(0o755)).unwrap();
        return None;
    }
    Some(locked)
}

fn restore(locked: &Path) {
    std::fs::set_permissions(locked, std::fs::Permissions::from_mode(0o755)).unwrap();
}

#[tokio::test]
async fn unstattable_entry_fails_mtime_and_size_tests() {
    let dir = tempdir();
    let Some(locked) = fixture(dir.path()) else { return };
    let kernel = kernel_at(dir.path());

    for test in ["-mtime -1", "-size -1M", "-mtime +9999", "-size +1M"] {
        let r = run(&kernel, &format!("find . {test} -name '*.txt'")).await;
        let out = r.text_out();
        assert!(!out.contains("inner.txt"), "{test}: unstattable entry printed: {out}");
        assert_eq!(r.code, 1, "{test}: exit code; stderr: {}", r.err);
        assert!(r.err.contains("'./locked'"), "{test}: stderr must name the unreadable directory: {}", r.err);
        assert!(r.err.starts_with("find: "), "{test}: stderr style: {}", r.err);
    }

    // Other entries still print, and the walk continues past the error.
    let r = run(&kernel, "find . -mtime -1 -name '*.txt'").await;
    assert!(r.text_out().contains("ok.txt"), "stat-able entry lost: {}", r.text_out());
    assert_eq!(r.code, 1);
    restore(&locked);
}

#[tokio::test]
async fn missing_operand_is_reported_and_later_operands_still_run() {
    let dir = tempdir();
    std::fs::create_dir(dir.path().join("ok_dir")).unwrap();
    std::fs::write(dir.path().join("ok_dir/a.txt"), "data").unwrap();
    let kernel = kernel_at(dir.path());

    let r = run(&kernel, "find missing ok_dir").await;
    let out = r.text_out();
    assert!(out.contains("ok_dir/a.txt"), "later operand skipped: {out}");
    assert!(!out.contains("missing"), "missing operand printed: {out}");
    assert_eq!(r.code, 1, "stderr: {}", r.err);
    assert!(r.err.starts_with("find: 'missing': "), "stderr style: {}", r.err);
    assert!(r.err.contains("No such file or directory"), "stderr reason: {}", r.err);
}

#[tokio::test]
async fn unreadable_gitignore_is_reported_naming_the_file() {
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "data").unwrap();
    std::fs::write(dir.path().join(".gitignore"), "*.log\n").unwrap();
    std::fs::set_permissions(
        dir.path().join(".gitignore"),
        std::fs::Permissions::from_mode(0o000),
    )
    .unwrap();
    if std::fs::read(dir.path().join(".gitignore")).is_ok() {
        return; // permissions do not restrict this user (root)
    }
    let kernel = kernel_at(dir.path());
    // find reads .gitignore only when the ignore config is enforced.
    run(&kernel, "kaish-ignore scope enforced; kaish-ignore auto on").await;
    let r = run(&kernel, "find . -name a.txt").await;
    std::fs::set_permissions(
        dir.path().join(".gitignore"),
        std::fs::Permissions::from_mode(0o644),
    )
    .unwrap();
    assert!(r.text_out().contains("a.txt"), "walk must go on: {}", r.text_out());
    assert_eq!(r.code, 1, "stderr: {}", r.err);
    assert!(r.err.starts_with("find: './.gitignore': "), "stderr: {}", r.err);
}

#[tokio::test]
async fn missing_gitignore_is_not_an_error() {
    let dir = tempdir();
    std::fs::write(dir.path().join("a.txt"), "data").unwrap();
    let kernel = kernel_at(dir.path());
    run(&kernel, "kaish-ignore scope enforced; kaish-ignore auto on").await;
    let r = run(&kernel, "find . -name a.txt").await;
    assert!(r.text_out().contains("a.txt"));
    assert_eq!(r.code, 0, "stderr: {}", r.err);
    assert_eq!(r.err, "");
}
