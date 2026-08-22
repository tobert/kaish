//! `test -r FILE` and `[[ -r FILE ]]` must answer "is it readable", not "does
//! it exist" — before this fix both sites (`tools/builtin/test.rs::file_test`
//! and `kernel.rs::eval_test_async`) implemented `-r` as `entry.is_some()`,
//! the same check as `-e`. A mode-000 file exists, so `-r` said TRUE, and
//! kaish's own `cat` refused it one line later with EACCES — the shell
//! contradicting itself about the same file.
//!
//! `-w`/`-x` already check mode bits (`0o222`/`0o111`) at both sites; `-r`
//! now checks `0o444` the same way. These tests are kernel-routed
//! (`kernel.execute(...)`), covering BOTH the `test` builtin and `[[ ]]`
//! separately, so a fix landing in only one site (the exact failure mode
//! that produced the original bug — see `test_compound_tests.rs`'s
//! `double_bracket_answers_the_empty_path_like_test` for the same class of
//! mirror-drift) is still caught.
//!
//! Permission-mode dependent: Unix only, on top of the crate's `localfs`
//! gate (see `touch_dir_tests.rs` for the same pairing).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(all(feature = "localfs", unix))]

mod common;

use std::os::unix::fs::PermissionsExt;
use std::path::Path;

use common::{kernel_at, run};

/// Restores a file's mode on drop — including when an assertion panics — so
/// a mode-000 fixture never leaves the tempdir unable to clean itself up.
/// (Unlinking a file only needs write access to its *parent* directory, so
/// this is belt-and-suspenders rather than strictly required, but the task
/// asked for it explicitly and it costs nothing.)
struct RestoreMode<'a> {
    path: &'a Path,
    mode: u32,
}

impl Drop for RestoreMode<'_> {
    fn drop(&mut self) {
        let _ = std::fs::set_permissions(self.path, std::fs::Permissions::from_mode(self.mode));
    }
}

/// True when the environment ignores the mode-000 restriction — running as
/// root, or under a filesystem/container that otherwise bypasses DAC checks.
/// Detected empirically (try to open the file) rather than by uid, so it
/// covers both cases without a new dependency.
fn dac_is_bypassed(path: &Path) -> bool {
    std::fs::File::open(path).is_ok()
}

/// Builds a tempdir with a normal readable file and a mode-000 unreadable
/// one, chmod'd inside the test's own temp dir per the task's fixture
/// requirement. Returns `None` (skip) if the environment bypasses DAC (e.g.
/// running as root), since mode-000 does not mean "unreadable" there.
fn setup() -> Option<(tempfile::TempDir, kaish_kernel::Kernel)> {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("readable.txt"), b"hi\n").unwrap();
    let noread = tmp.path().join("noread.txt");
    std::fs::write(&noread, b"secret\n").unwrap();
    std::fs::set_permissions(&noread, std::fs::Permissions::from_mode(0o000)).unwrap();

    if dac_is_bypassed(&noread) {
        std::fs::set_permissions(&noread, std::fs::Permissions::from_mode(0o644)).unwrap();
        eprintln!(
            "skipping test_readable_permission_tests: environment bypasses \
             mode-000 (running as root, or a filesystem that ignores DAC)"
        );
        return None;
    }

    let kernel = kernel_at(tmp.path());
    Some((tmp, kernel))
}

// --- the `test` builtin -----------------------------------------------------

#[tokio::test]
async fn test_builtin_dash_r_false_on_a_mode_000_file() {
    let Some((tmp, kernel)) = setup() else { return };
    let noread = tmp.path().join("noread.txt");
    let _restore = RestoreMode { path: &noread, mode: 0o644 };

    let (out, code) = run(&kernel, "test -r noread.txt").await;
    assert_eq!(code, 1, "mode-000 file must not be -r: {out}");
}

#[tokio::test]
async fn test_builtin_dash_r_true_on_a_normal_file() {
    let Some((_tmp, kernel)) = setup() else { return };
    let (out, code) = run(&kernel, "test -r readable.txt").await;
    assert_eq!(code, 0, "a normally-readable file must be -r: {out}");
}

// --- `[[ ]]` ----------------------------------------------------------------

#[tokio::test]
async fn double_bracket_dash_r_false_on_a_mode_000_file() {
    let Some((tmp, kernel)) = setup() else { return };
    let noread = tmp.path().join("noread.txt");
    let _restore = RestoreMode { path: &noread, mode: 0o644 };

    let (out, code) = run(&kernel, "[[ -r noread.txt ]]").await;
    assert_eq!(code, 1, "mode-000 file must not be -r: {out}");
}

#[tokio::test]
async fn double_bracket_dash_r_true_on_a_normal_file() {
    let Some((_tmp, kernel)) = setup() else { return };
    let (out, code) = run(&kernel, "[[ -r readable.txt ]]").await;
    assert_eq!(code, 0, "a normally-readable file must be -r: {out}");
}

/// `-r` must not diverge from what `cat` can actually do — the original bug
/// in one sentence: kaish vouched for a file's readability and then failed
/// to read it. Pinned as a same-script contradiction check, both spellings.
#[tokio::test]
async fn dash_r_agrees_with_cat_on_a_mode_000_file() {
    let Some((tmp, kernel)) = setup() else { return };
    let noread = tmp.path().join("noread.txt");
    let _restore = RestoreMode { path: &noread, mode: 0o644 };

    let (out, code) = run(&kernel, "cat noread.txt").await;
    assert_eq!(code, 1, "cat on a mode-000 file must fail: {out}");

    let (out, code) = run(&kernel, "test -r noread.txt").await;
    assert_eq!(code, 1, "`test -r` must agree with `cat`'s failure: {out}");

    let (out, code) = run(&kernel, "[[ -r noread.txt ]]").await;
    assert_eq!(code, 1, "`[[ -r ]]` must agree with `cat`'s failure: {out}");
}
