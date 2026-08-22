//! `tee -a` must never truncate an existing file when the pre-append read
//! fails for a reason other than "the file doesn't exist yet".
//!
//! Regression coverage for the bug where `.unwrap_or_default()` on the
//! read-before-append turned any read failure (permission denied, I/O
//! error, ...) into a silent truncation: `tee -a` exited 0 and the file's
//! prior content was gone, overwritten by the new input alone.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};

#[cfg(unix)]
#[tokio::test]
async fn tee_append_does_not_truncate_when_read_is_denied() {
    use std::os::unix::fs::PermissionsExt;

    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let file = tmp.path().join("existing.txt");
    std::fs::write(&file, b"original content\n").unwrap();

    // Write-only (0200), not mode 000: mode 000 blocks the append's WRITE
    // too (truncating open needs write permission on the file), so the
    // neutralized bug's write would silently no-op and the test would pass
    // for the wrong reason — it wouldn't be exercising the truncation at
    // all. 0200 denies the read while still letting the write through,
    // which is exactly the shape that turns "read failed" into "the file
    // got overwritten with just the new input."
    std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o200)).unwrap();

    // Sanity-check the fixture actually makes the read fail before relying
    // on it to prove anything. Root (and CI running as root) can read past
    // a write-only file, which would silently no-op this whole test.
    let read_still_works = std::fs::read(&file).is_ok();

    let result = kernel.execute("echo appended | tee -a existing.txt").await;

    // Always restore permissions before any assertion can panic/return, so
    // the tempdir can be removed on drop regardless of outcome.
    std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o644)).unwrap();

    if read_still_works {
        eprintln!(
            "skipping: mode-000 did not deny read (likely running as root); \
             fixture cannot exercise the read-failure path"
        );
        return;
    }

    let result = result.expect("kernel execute");

    // The heart of this test: the ORIGINAL BYTES must still be on disk.
    // A bug that silently truncates on read failure leaves only "appended\n".
    let on_disk = std::fs::read(&file).unwrap();
    assert_eq!(
        on_disk, b"original content\n",
        "tee -a must not destroy prior content when the pre-append read fails; \
         got {on_disk:?}"
    );

    assert_ne!(
        result.code, 0,
        "tee -a must report a non-zero exit when the pre-append read fails: {}",
        result.text_out()
    );
    assert!(
        !result.err.is_empty(),
        "tee -a must report the read failure, not swallow it silently"
    );
}

#[tokio::test]
async fn tee_append_still_creates_a_missing_file() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());

    let (out, code) = run(&kernel, "echo hello | tee -a newfile.txt").await;
    assert_eq!(code, 0, "tee -a on a missing file must succeed: {out}");

    let on_disk = std::fs::read(tmp.path().join("newfile.txt")).unwrap();
    assert_eq!(on_disk, b"hello\n");
}

#[tokio::test]
async fn tee_append_still_appends_to_an_existing_file() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let file = tmp.path().join("existing.txt");
    std::fs::write(&file, b"original content\n").unwrap();

    let (out, code) = run(&kernel, "echo appended | tee -a existing.txt").await;
    assert_eq!(code, 0, "tee -a on an existing, readable file must succeed: {out}");

    let on_disk = std::fs::read(&file).unwrap();
    assert_eq!(on_disk, b"original content\nappended\n");
}

#[tokio::test]
async fn tee_without_append_still_overwrites() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let file = tmp.path().join("existing.txt");
    std::fs::write(&file, b"original content\n").unwrap();

    let (out, code) = run(&kernel, "echo replaced | tee existing.txt").await;
    assert_eq!(code, 0, "tee (no -a) on an existing file must succeed: {out}");

    let on_disk = std::fs::read(&file).unwrap();
    assert_eq!(on_disk, b"replaced\n");
}
