//! `ls -R` and `tree` used to silently swallow a directory they could not
//! read: the recursive walk in each hit `Err(_) => continue` with no
//! comment, so an unreadable subdirectory just vanished from the output —
//! `ls -R` exited 0 with no mention of it, and `tree` rendered it as a
//! childless node, indistinguishable from a genuinely empty directory.
//!
//! Fixed behavior (matching GNU `ls -R`, which continues the walk and
//! reports every unreadable directory rather than stopping at the first):
//! - `ls -R`: every unreadable directory is named on stderr, the exit code
//!   moves to 1, and the readable parts of the tree still print.
//! - `tree`: GNU `tree`'s convention — the node is marked inline
//!   (`name [error opening dir]`) so a reader looking only at stdout can
//!   still tell it wasn't descended into, plus a stderr line and exit 1.
//! - A fully readable tree still exits 0 with no diagnostics, for both.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::kernel_at;

/// Restores a directory's mode to 0o755 on drop, even if the test body
/// panics on a failed assertion. Without this, a mode-000 directory left
/// behind by a failing test blocks `tempdir()`'s own recursive cleanup
/// (removing a directory's entries requires being able to list it first).
#[cfg(unix)]
struct RestorePerms(std::path::PathBuf);

#[cfg(unix)]
impl Drop for RestorePerms {
    fn drop(&mut self) {
        use std::os::unix::fs::PermissionsExt;
        let _ = fs::set_permissions(&self.0, fs::Permissions::from_mode(0o755));
    }
}

/// Build `<root>/parent/{good/visible.txt, secret/hidden.txt}` and chmod
/// `secret` to 000.
///
/// Returns `None` (after restoring `secret`'s permissions) when this
/// environment can still read a mode-000 directory — running as root, or a
/// filesystem/mount that ignores the permission bits. In that case the
/// fixture does not exercise the bug at all, and asserting on it would pass
/// for the wrong reason; callers must skip instead.
#[cfg(unix)]
fn make_fixture(root: &std::path::Path) -> Option<(std::path::PathBuf, RestorePerms)> {
    use std::os::unix::fs::PermissionsExt;

    let parent = root.join("parent");
    fs::create_dir_all(parent.join("good")).expect("mkdir good");
    fs::write(parent.join("good/visible.txt"), "hi").expect("write visible.txt");

    let secret = parent.join("secret");
    fs::create_dir(&secret).expect("mkdir secret");
    fs::write(secret.join("hidden.txt"), "nope").expect("write hidden.txt");

    fs::set_permissions(&secret, fs::Permissions::from_mode(0o000)).expect("chmod 000");

    // Prove the chmod actually blocks reads in this environment before
    // trusting the rest of the test to it.
    if fs::read_dir(&secret).is_ok() {
        fs::set_permissions(&secret, fs::Permissions::from_mode(0o755)).expect("restore perms");
        return None;
    }

    let guard = RestorePerms(secret);
    Some((parent, guard))
}

// ─── ls -R ───────────────────────────────────────────────────────────────

#[cfg(unix)]
#[tokio::test]
async fn ls_recursive_reports_unreadable_dir_and_keeps_readable_entries() {
    let tmp = tempfile::tempdir().unwrap();
    let Some((_parent, _guard)) = make_fixture(tmp.path()) else {
        eprintln!("skipping: mode-000 directory is still readable in this environment (root?)");
        return;
    };

    let kernel = kernel_at(tmp.path());
    let result = kernel.execute("ls -R parent").await.expect("kernel execute");

    assert_ne!(result.code, 0, "unreadable dir in the walk must be a nonzero exit: {result:?}");
    assert!(
        result.err.contains("secret"),
        "stderr should name the unreadable directory: {:?}",
        result.err
    );
    let out = result.text_out();
    assert!(out.contains("visible.txt"), "readable sibling entries must still list: {out:?}");
    assert!(out.contains("good"), "readable sibling directory must still list: {out:?}");
}

#[cfg(unix)]
#[tokio::test]
async fn ls_recursive_fully_readable_tree_exits_zero_no_diagnostics() {
    let tmp = tempfile::tempdir().unwrap();
    fs::create_dir_all(tmp.path().join("parent/good")).unwrap();
    fs::write(tmp.path().join("parent/good/visible.txt"), "hi").unwrap();

    let kernel = kernel_at(tmp.path());
    let result = kernel.execute("ls -R parent").await.expect("kernel execute");

    assert_eq!(result.code, 0, "fully readable tree must exit 0: {result:?}");
    assert!(result.err.is_empty(), "no diagnostics expected: {:?}", result.err);
    assert!(result.text_out().contains("visible.txt"));
}

// ─── tree ────────────────────────────────────────────────────────────────

#[cfg(unix)]
#[tokio::test]
async fn tree_marks_unreadable_dir_inline_and_exits_nonzero() {
    let tmp = tempfile::tempdir().unwrap();
    let Some((_parent, _guard)) = make_fixture(tmp.path()) else {
        eprintln!("skipping: mode-000 directory is still readable in this environment (root?)");
        return;
    };

    let kernel = kernel_at(tmp.path());
    let result = kernel.execute("tree parent").await.expect("kernel execute");

    assert_ne!(result.code, 0, "unreadable dir in the walk must be a nonzero exit: {result:?}");
    assert!(
        result.err.contains("secret"),
        "stderr should name the unreadable directory: {:?}",
        result.err
    );
    let out = result.text_out();
    assert!(
        out.contains("secret") && out.contains("[error opening dir]"),
        "the node must be marked inline, not rendered as a childless (i.e. \
         indistinguishable from empty) directory: {out:?}"
    );
    assert!(out.contains("visible.txt"), "readable sibling entries must still render: {out:?}");
}

#[cfg(unix)]
#[tokio::test]
async fn tree_traditional_marks_unreadable_dir_inline() {
    let tmp = tempfile::tempdir().unwrap();
    let Some((_parent, _guard)) = make_fixture(tmp.path()) else {
        eprintln!("skipping: mode-000 directory is still readable in this environment (root?)");
        return;
    };

    let kernel = kernel_at(tmp.path());
    let result = kernel.execute("tree --traditional parent").await.expect("kernel execute");

    assert_ne!(result.code, 0, "unreadable dir in the walk must be a nonzero exit: {result:?}");
    let out = result.text_out();
    assert!(
        out.contains("[error opening dir]"),
        "traditional format must also mark the unreadable node: {out:?}"
    );
    assert!(out.contains("visible.txt"), "readable sibling entries must still render: {out:?}");
}

#[cfg(unix)]
#[tokio::test]
async fn tree_fully_readable_tree_exits_zero_no_diagnostics() {
    let tmp = tempfile::tempdir().unwrap();
    fs::create_dir_all(tmp.path().join("parent/good")).unwrap();
    fs::write(tmp.path().join("parent/good/visible.txt"), "hi").unwrap();

    let kernel = kernel_at(tmp.path());
    let result = kernel.execute("tree parent").await.expect("kernel execute");

    assert_eq!(result.code, 0, "fully readable tree must exit 0: {result:?}");
    assert!(result.err.is_empty(), "no diagnostics expected: {:?}", result.err);
    assert!(!result.text_out().contains("[error opening dir]"));
}
