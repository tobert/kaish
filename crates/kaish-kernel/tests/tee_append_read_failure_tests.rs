//! `tee -a` must never truncate an existing file when it lacks read
//! permission on it.
//!
//! Regression coverage for the bug where `.unwrap_or_default()` on the
//! read-before-append turned any read failure (permission denied, I/O
//! error, ...) into a silent truncation: `tee -a` exited 0 and the file's
//! prior content was gone, overwritten by the new input alone.
//!
//! `tee -a` now appends through a real VFS `append` (`O_APPEND`, no read),
//! matching POSIX `tee -a`: it needs no read permission on the target, so
//! it succeeds on a write-only file where the old read-then-write
//! composition failed loudly (correctly, but short of what real `tee -a`
//! does).
//!
//! `>>` shares the same `KernelBackend::append` primitive
//! (`scheduler/pipeline.rs`'s `redirect_append`), so this file also covers
//! it on the same write-only fixture.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use common::{kernel_at, run};
use kaish_kernel::ExecuteOptions;

#[cfg(unix)]
#[tokio::test]
async fn tee_append_succeeds_on_a_write_only_file() {
    use std::os::unix::fs::PermissionsExt;

    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let file = tmp.path().join("existing.txt");
    std::fs::write(&file, b"original content\n").unwrap();

    // Write-only (0200), not mode 000: mode 000 blocks the append's WRITE
    // too, which would make this test pass for the wrong reason (nothing
    // happening at all). 0200 denies read while still allowing write —
    // exactly the shape a real `O_APPEND` append needs no read for.
    std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o200)).unwrap();

    // Sanity-check the fixture actually denies the read before relying on
    // it to prove anything. Root (and CI running as root) can read past a
    // write-only file, which would silently no-op this whole test.
    let read_still_works = std::fs::read(&file).is_ok();

    let result = kernel.execute("echo appended | tee -a existing.txt").await;

    // Always restore permissions before any assertion can panic/return, so
    // the tempdir can be removed on drop regardless of outcome.
    std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o644)).unwrap();

    if read_still_works {
        eprintln!(
            "skipping: mode-000 did not deny read (likely running as root); \
             fixture cannot exercise the write-only-file path"
        );
        return;
    }

    let result = result.expect("kernel execute");

    // The heart of this test: `tee -a` now appends via a real VFS append
    // (O_APPEND, no read), so it succeeds on a write-only file — matching
    // POSIX `tee -a` — and the file holds the original bytes plus the new
    // input, not just one or the other.
    assert_eq!(
        result.code, 0,
        "tee -a on a write-only file must succeed (no read permission needed \
         for a real append): {}",
        result.text_out()
    );

    let on_disk = std::fs::read(&file).unwrap();
    assert_eq!(
        on_disk, b"original content\nappended\n",
        "tee -a must append to, not replace, a write-only file's content; got {on_disk:?}"
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

#[tokio::test]
async fn tee_append_passes_binary_content_byte_exact() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let file = tmp.path().join("existing.bin");
    std::fs::write(&file, [0xffu8, 0xfe, 0x00, 0x01]).unwrap();

    // Non-UTF-8 bytes, fed as raw stdin (not through a shell string, which
    // would force valid UTF-8) so the append path is proven byte-exact, not
    // just ASCII-exact.
    let stdin: Vec<u8> = vec![0xdeu8, 0xad, 0xbe, 0xef];
    let result = kernel
        .execute_with_options("tee -a existing.bin", ExecuteOptions::new().with_stdin(stdin))
        .await
        .expect("kernel execute");
    assert_eq!(result.code, 0, "tee -a on binary content must succeed: {}", result.text_out());

    let on_disk = std::fs::read(&file).unwrap();
    assert_eq!(on_disk, [0xff, 0xfe, 0x00, 0x01, 0xde, 0xad, 0xbe, 0xef]);
}

#[cfg(unix)]
#[tokio::test]
async fn redirect_append_succeeds_on_a_write_only_file() {
    use std::os::unix::fs::PermissionsExt;

    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let file = tmp.path().join("existing.txt");
    std::fs::write(&file, b"original content\n").unwrap();

    std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o200)).unwrap();
    let read_still_works = std::fs::read(&file).is_ok();

    let result = kernel.execute("echo appended >> existing.txt").await;

    std::fs::set_permissions(&file, std::fs::Permissions::from_mode(0o644)).unwrap();

    if read_still_works {
        eprintln!(
            "skipping: mode-000 did not deny read (likely running as root); \
             fixture cannot exercise the write-only-file path"
        );
        return;
    }

    let result = result.expect("kernel execute");

    // `>>` shares `KernelBackend::append` with `tee -a` (both call through
    // `ctx.backend.append`), so it gets the same real-append fix for free —
    // succeeding on a write-only file, not just leaving it intact.
    assert_eq!(
        result.code, 0,
        ">> on a write-only file must succeed (same append primitive as tee -a): {}",
        result.text_out()
    );

    let on_disk = std::fs::read(&file).unwrap();
    assert_eq!(
        on_disk, b"original content\nappended\n",
        ">> must append to, not replace, a write-only file's content; got {on_disk:?}"
    );
}
