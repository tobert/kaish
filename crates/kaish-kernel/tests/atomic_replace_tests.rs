//! Which writes replace a file atomically and which rewrite it in place.
//!
//! A read-modify-write (`sed -i`, `patch`) writes a new file beside the
//! target and renames it over, so a crash leaves the old file or the new one,
//! never a partial file. `>` and `tee` truncate in place and keep the file's
//! inode, as bash does. The inode is the observable difference.

// Test-fixture code: unwrap/expect on known-good setup is the idiom.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(unix, feature = "localfs"))]

mod common;

use common::kernel_at;
use std::fs;
use std::io::Read;
use std::os::unix::fs::{MetadataExt, PermissionsExt};
use std::path::Path;
use tempfile::tempdir;

fn inode(path: &Path) -> u64 {
    fs::metadata(path).unwrap().ino()
}

async fn run_ok(dir: &Path, script: &str) {
    let result = kernel_at(dir).execute(script).await.unwrap();
    assert_eq!(result.code, 0, "{script}: {}", result.err);
}

#[tokio::test]
async fn sed_in_place_replaces_the_file() {
    let dir = tempdir().unwrap();
    let path = dir.path().join("f.txt");
    fs::write(&path, "alpha\nbravo\n").unwrap();
    fs::set_permissions(&path, fs::Permissions::from_mode(0o640)).unwrap();
    let before = inode(&path);
    let mut old_handle = fs::File::open(&path).unwrap();

    run_ok(dir.path(), "sed -i 's/bravo/BRAVO/' f.txt").await;

    assert_eq!(fs::read_to_string(&path).unwrap(), "alpha\nBRAVO\n");
    assert_ne!(inode(&path), before, "sed -i must rename a new file over the old one");
    assert_eq!(fs::metadata(&path).unwrap().permissions().mode() & 0o7777, 0o640);
    let mut seen = String::new();
    old_handle.read_to_string(&mut seen).unwrap();
    assert_eq!(seen, "alpha\nbravo\n", "the old file must never be truncated");
}

#[tokio::test]
async fn patch_replaces_the_file() {
    let dir = tempdir().unwrap();
    let path = dir.path().join("f.txt");
    fs::write(&path, "alpha\nbravo\n").unwrap();
    fs::write(
        dir.path().join("change.diff"),
        "--- f.txt\n+++ f.txt\n@@ -1,2 +1,2 @@\n alpha\n-bravo\n+BRAVO\n",
    )
    .unwrap();
    let before = inode(&path);

    run_ok(dir.path(), "patch f.txt < change.diff").await;

    assert_eq!(fs::read_to_string(&path).unwrap(), "alpha\nBRAVO\n");
    assert_ne!(inode(&path), before, "patch must rename a new file over the old one");
}

#[tokio::test]
async fn redirect_rewrites_in_place() {
    let dir = tempdir().unwrap();
    let path = dir.path().join("f.txt");
    fs::write(&path, "old\n").unwrap();
    let before = inode(&path);

    run_ok(dir.path(), "echo new > f.txt").await;

    assert_eq!(fs::read_to_string(&path).unwrap(), "new\n");
    assert_eq!(inode(&path), before, "> keeps the inode, like bash");
}

#[tokio::test]
async fn tee_rewrites_in_place() {
    let dir = tempdir().unwrap();
    let path = dir.path().join("f.txt");
    fs::write(&path, "old\n").unwrap();
    let before = inode(&path);

    run_ok(dir.path(), "echo new | tee f.txt").await;

    assert_eq!(fs::read_to_string(&path).unwrap(), "new\n");
    assert_eq!(inode(&path), before, "tee keeps the inode, like GNU tee");
}
