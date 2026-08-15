//! `touch <dir>` must update the directory's timestamps and exit 0, matching
//! GNU `touch`. It used to fail: `LocalFs::set_mtime` opened the target with
//! `OpenOptions::new().write(true)`, which cannot open a directory at all
//! (EISDIR) — so `touch somedir` on an existing directory returned "is a
//! directory: Is a directory (os error 21)" and exit 1 instead of succeeding.
//!
//! No single open mode covers every target, so `LocalFs::set_mtime` tries
//! read, then falls back to write: a directory can only be opened read-only,
//! while a write-only file (mode 0200) rejects a read-only open (EACCES) and
//! needs the write fallback. `futimens` with explicit times only checks file
//! ownership/CAP_FOWNER, not the fd's access mode, so either open mode is
//! sufficient once it succeeds at all. This file covers both shapes; a fix
//! that satisfies only one (as an earlier revision of this fix did — see the
//! write-only test below) is not a fix.
//!
//! These tests are kernel-routed (`kernel.execute(...)`), not calls to the
//! `Touch` builtin's `.execute()` directly, per the repo convention: direct
//! calls skip arg binding and the rest of the dispatch chain.

#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use std::time::{Duration, SystemTime};

#[tokio::test]
async fn touch_on_existing_directory_succeeds_and_advances_mtime() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let subdir = tmp.path().join("subdir");
    std::fs::create_dir(&subdir).unwrap();

    // Pin the mtime to a known past instant so a real timestamp change is
    // provable, not just "didn't error".
    let past = SystemTime::now() - Duration::from_secs(3600);
    let past_file = std::fs::File::open(&subdir).unwrap();
    past_file.set_modified(past).unwrap();
    drop(past_file);
    let before = std::fs::metadata(&subdir).unwrap().modified().unwrap();
    assert_eq!(before, past, "test setup: mtime pin didn't take");

    let (out, code) = run(&kernel, "touch subdir").await;
    assert_eq!(code, 0, "touch on an existing directory must succeed: {out}");

    let after = std::fs::metadata(&subdir).unwrap().modified().unwrap();
    assert!(
        after > before,
        "touch did not advance the directory's mtime: before={before:?} after={after:?}"
    );
}

#[tokio::test]
async fn touch_still_creates_a_missing_file() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "touch newfile.txt").await;
    assert_eq!(code, 0, "touch on a missing path must create it: {out}");
    assert!(tmp.path().join("newfile.txt").is_file());
}

#[tokio::test]
async fn touch_still_updates_an_existing_file() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let file = tmp.path().join("existing.txt");
    std::fs::write(&file, b"content").unwrap();

    let past = SystemTime::now() - Duration::from_secs(3600);
    let past_file = std::fs::File::open(&file).unwrap();
    past_file.set_modified(past).unwrap();
    drop(past_file);
    let before = std::fs::metadata(&file).unwrap().modified().unwrap();

    let (out, code) = run(&kernel, "touch existing.txt").await;
    assert_eq!(code, 0, "touch on an existing file must succeed: {out}");

    let after = std::fs::metadata(&file).unwrap().modified().unwrap();
    assert!(
        after > before,
        "touch did not advance the file's mtime: before={before:?} after={after:?}"
    );
    // Content must be untouched — touch never truncates an existing file.
    assert_eq!(std::fs::read(&file).unwrap(), b"content");
}

/// A write-only (mode 0200) file rejects a read-only open with EACCES —
/// GNU `touch` still succeeds on it (it owns the file), so the fix must fall
/// back to a write-only open rather than stopping at the first failure.
/// Permission-mode dependent: Unix only, on top of the crate's `localfs` gate.
#[cfg(unix)]
#[tokio::test]
async fn touch_still_updates_a_write_only_file() {
    use std::os::unix::fs::PermissionsExt;

    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let file = tmp.path().join("wonly.txt");
    std::fs::write(&file, b"").unwrap();

    // Pin the mtime while the file is still readable (default 0644), then
    // drop to write-only — the shape GNU touch must still handle.
    let past = SystemTime::now() - Duration::from_secs(3600);
    let past_file = std::fs::File::open(&file).unwrap();
    past_file.set_modified(past).unwrap();
    drop(past_file);
    let before = std::fs::metadata(&file).unwrap().modified().unwrap();
    assert_eq!(before, past, "test setup: mtime pin didn't take");

    std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o200)).unwrap();

    let (out, code) = run(&kernel, "touch wonly.txt").await;
    assert_eq!(code, 0, "touch on a write-only file must succeed: {out}");

    let after = std::fs::metadata(&file).unwrap().modified().unwrap();
    assert!(
        after > before,
        "touch did not advance the write-only file's mtime: before={before:?} after={after:?}"
    );
}
