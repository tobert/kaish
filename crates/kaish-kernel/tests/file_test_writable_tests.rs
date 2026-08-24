//! `-w` must answer "can kaish write this path", and an absent mode is not
//! that answer.
//!
//! `DirEntry.permissions` is `None` on MemoryFs (writable), on DevFs
//! (writable — writes discard), on BuiltinFs (read-only) and on JobFs
//! (read-only). Both defaults are therefore wrong for half the backends:
//! shipped 0.16 opened it (`is_none_or`) and said `/v/bin/echo` and
//! `/v/jobs/1/status` were writable; closing it said `/v/probe.txt` was not,
//! about a path the very next line writes. The bit being defaulted is not
//! the bit that answers the question.
//!
//! The answer needs both facts: the owning mount's read-only state AND the
//! mode bits that mount reports. `PathAccess::resolve` is the only place
//! they combine, and `KernelBackend::path_access` is the only thing the two
//! file-test sites call.
//!
//! Every case runs twice — once through the `test` builtin
//! (`tools/builtin/test.rs::file_test`) and once through `[[ ]]`
//! (`kernel.rs::eval_test_async`) — because a fix landing in one site and
//! not the other is the bug class this repo keeps getting caught by.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::sync::Arc;

use kaish_kernel::vfs::{LocalFs, MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelBackend, KernelConfig, LocalBackend};

use common::{kernel_at, run};

/// Assert that both spellings of a file test agree, and on what.
///
/// `setup` runs first (same kernel), so a case can create its fixture in the
/// VFS. Asserting on both spellings in one helper is the point: it is not
/// possible to add a case here that covers only `test` or only `[[ ]]`.
async fn both_spellings(kernel: &Kernel, setup: &str, op: &str, path: &str, expected: bool) {
    if !setup.is_empty() {
        let (out, code) = run(kernel, setup).await;
        assert_eq!(code, 0, "setup failed: {setup}: out={out:?}");
    }
    let want = if expected { 0 } else { 1 };

    let script = format!("if test {op} {path}; then echo YES; else echo NO; fi");
    let (out, _) = run(kernel, &script).await;
    assert_eq!(
        out,
        if expected { "YES" } else { "NO" },
        "`test {op} {path}` disagrees (want exit {want})",
    );

    let script = format!("if [[ {op} {path} ]]; then echo YES; else echo NO; fi");
    let (out, _) = run(kernel, &script).await;
    assert_eq!(
        out,
        if expected { "YES" } else { "NO" },
        "`[[ {op} {path} ]]` disagrees (want exit {want})",
    );
}

// ── MemoryFs: writable, reports no mode ────────────────────────────────────

/// The regression that killed the close-the-default attempt: `/v` is
/// MemoryFs, MemoryFs reports `permissions: None` everywhere, and MemoryFs
/// is writable. Any implementation that reads an absent mode as "not
/// writable" fails here.
#[tokio::test]
async fn memoryfs_file_is_writable() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    both_spellings(&kernel, "echo hi > /v/probe.txt", "-w", "/v/probe.txt", true).await;
}

/// The proof that the answer above is not a guess: the same path really does
/// take a second write. If this fails, the fixture is wrong, not the fix.
#[tokio::test]
async fn memoryfs_file_really_takes_a_second_write() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (_, code) = run(&kernel, "echo one > /v/p2.txt; echo two >> /v/p2.txt").await;
    assert_eq!(code, 0, "MemoryFs path must accept an append");
    let (out, _) = run(&kernel, "cat /v/p2.txt").await;
    assert_eq!(out, "one\ntwo");
}

/// A MemoryFs *directory* is writable too — `mkdir`/`touch` land there.
#[tokio::test]
async fn memoryfs_directory_is_writable() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    both_spellings(&kernel, "mkdir -p /v/sub", "-w", "/v/sub", true).await;
}

// ── BuiltinFs: read-only, reports no mode ──────────────────────────────────

/// `/v/bin` is BuiltinFs, whose `read_only()` is `true` and whose entries
/// report `permissions: None`. Shipped 0.16 answered "writable" here.
#[tokio::test]
async fn builtinfs_entry_is_not_writable() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    both_spellings(&kernel, "", "-w", "/v/bin/echo", false).await;
}

/// Read-only is about writes. A BuiltinFs entry reads fine, so `-r` must
/// stay true — an implementation that routed the mount's read-only state
/// into `readable` as well fails here.
#[tokio::test]
async fn builtinfs_entry_is_still_readable() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    both_spellings(&kernel, "", "-r", "/v/bin/echo", true).await;
}

/// The mount really does refuse the write the test predicts.
#[tokio::test]
async fn builtinfs_entry_really_refuses_a_write() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (_, code) = run(&kernel, "echo nope > /v/bin/echo").await;
    assert_ne!(code, 0, "BuiltinFs must refuse a write to /v/bin/echo");
}

// ── JobFs: read-only, reports no mode ──────────────────────────────────────

/// `/v/jobs/{id}/status` is JobFs — read-only, `permissions: None`. Same
/// wrong answer as BuiltinFs in shipped 0.16, from a different mount, so a
/// fix that special-cased `/v/bin` would still fail here.
#[tokio::test]
async fn jobfs_node_is_not_writable() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    let (out, code) = run(&kernel, "sleep 30 &").await;
    assert_eq!(code, 0, "backgrounding a job failed: {out:?}");
    both_spellings(&kernel, "", "-w", "/v/jobs/1/status", false).await;
    // and it is readable, for the same reason BuiltinFs is
    both_spellings(&kernel, "", "-r", "/v/jobs/1/status", true).await;
}

// ── DevFs: writable on purpose, reports no mode ────────────────────────────

/// `DevFs::read_only()` is deliberately `false` — refusing the write would
/// break `> /dev/null`. `-w /dev/null` must agree with that, so an
/// implementation that read "no mode bits" as "not writable" fails here.
#[tokio::test]
async fn devfs_null_is_writable() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    both_spellings(&kernel, "", "-w", "/dev/null", true).await;
    let (_, code) = run(&kernel, "echo discard > /dev/null").await;
    assert_eq!(code, 0, "/dev/null must accept the write it says it accepts");
}

// ── LocalFs: real OS mode bits ─────────────────────────────────────────────

/// A writable LocalFs mount still has to honour the mode bits — an
/// implementation that answered from the mount alone and ignored the stat
/// would call a mode-444 file writable.
#[cfg(unix)]
#[tokio::test]
async fn localfs_mode_bits_still_decide() {
    use std::os::unix::fs::PermissionsExt;

    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("rw.txt"), b"hi\n").unwrap();
    std::fs::write(tmp.path().join("ro.txt"), b"hi\n").unwrap();
    std::fs::set_permissions(
        tmp.path().join("ro.txt"),
        std::fs::Permissions::from_mode(0o444),
    )
    .unwrap();

    let kernel = kernel_at(tmp.path());
    let rw = tmp.path().join("rw.txt");
    let ro = tmp.path().join("ro.txt");
    both_spellings(&kernel, "", "-w", &rw.display().to_string(), true).await;
    both_spellings(&kernel, "", "-w", &ro.display().to_string(), false).await;
}

/// `-x` on a real path keeps answering from the mode bits. Pinned so a later
/// symmetry argument cannot quietly move it.
#[cfg(unix)]
#[tokio::test]
async fn localfs_executable_bit_still_decides() {
    use std::os::unix::fs::PermissionsExt;

    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("run.sh"), b"#!/bin/sh\n").unwrap();
    std::fs::write(tmp.path().join("plain.txt"), b"hi\n").unwrap();
    std::fs::set_permissions(
        tmp.path().join("run.sh"),
        std::fs::Permissions::from_mode(0o755),
    )
    .unwrap();

    let kernel = kernel_at(tmp.path());
    let run_sh = tmp.path().join("run.sh");
    let plain = tmp.path().join("plain.txt");
    both_spellings(&kernel, "", "-x", &run_sh.display().to_string(), true).await;
    both_spellings(&kernel, "", "-x", &plain.display().to_string(), false).await;
}

/// A memory-backed path has no executable to run — no mode bits, and
/// `real_path` is `None`, so there is nothing for exec(2). `-x` says false,
/// and read-only-ness has nothing to do with it. This pins the deliberate
/// answer to the `-x` question rather than leaving it to inference.
#[tokio::test]
async fn memory_backed_paths_are_not_executable() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    both_spellings(&kernel, "echo hi > /v/probe.txt", "-x", "/v/probe.txt", false).await;
    // read-only mount, same answer, for the same reason
    both_spellings(&kernel, "", "-x", "/v/bin/echo", false).await;
    // writable mount, same answer
    both_spellings(&kernel, "", "-x", "/dev/null", false).await;
}

// ── The hazard: read-only wrapper over an OS-writable directory ────────────

/// Build a kernel whose `/` is a `LocalFs::read_only` wrapper over a real,
/// OS-writable tempdir — the shape kaijutsu embeds.
fn read_only_wrapper_kernel(dir: &std::path::Path) -> Kernel {
    let mut vfs = VfsRouter::new();
    vfs.mount("/", LocalFs::read_only(dir));
    vfs.mount("/v", MemoryFs::new());
    let backend: Arc<dyn KernelBackend> = Arc::new(LocalBackend::new(Arc::new(vfs)));
    Kernel::with_backend(backend, KernelConfig::isolated(), |_| {}, |_| {})
        .expect("with_backend kernel")
}

/// The hazard from the design note: `LocalFs::stat` reports the real OS mode
/// bits and knows nothing about the read-only wrapper around it, so a raw
/// stat check calls a mode-644 file writable while every write to it fails.
/// An implementation that consults only the stat fails here; one that
/// consults only the mount passes here and fails `localfs_mode_bits_still_decide`.
#[cfg(unix)]
#[tokio::test]
async fn read_only_wrapper_over_writable_os_dir_is_not_writable() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("file.txt"), b"hi\n").unwrap();

    // The OS says 0o644. The wrapper says no.
    let mode = {
        use std::os::unix::fs::PermissionsExt;
        std::fs::metadata(tmp.path().join("file.txt"))
            .unwrap()
            .permissions()
            .mode()
    };
    assert_ne!(mode & 0o222, 0, "fixture must be OS-writable to be the hazard");

    let kernel = read_only_wrapper_kernel(tmp.path());
    both_spellings(&kernel, "", "-w", "/file.txt", false).await;
}

/// The wrapper really does refuse the write — the assertion above is about
/// the same file this one fails to write.
#[tokio::test]
async fn read_only_wrapper_really_refuses_a_write() {
    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("file.txt"), b"hi\n").unwrap();
    let kernel = read_only_wrapper_kernel(tmp.path());
    let (_, code) = run(&kernel, "echo nope > /file.txt").await;
    assert_ne!(code, 0, "a read-only LocalFs must refuse the write");
}

/// The read-only wrapper is still readable, and its `-x` still comes from
/// the OS mode bits — the wrapper is about writes only.
#[cfg(unix)]
#[tokio::test]
async fn read_only_wrapper_keeps_read_and_execute_answers() {
    use std::os::unix::fs::PermissionsExt;

    let tmp = tempfile::tempdir().unwrap();
    std::fs::write(tmp.path().join("run.sh"), b"#!/bin/sh\n").unwrap();
    std::fs::set_permissions(
        tmp.path().join("run.sh"),
        std::fs::Permissions::from_mode(0o555),
    )
    .unwrap();

    let kernel = read_only_wrapper_kernel(tmp.path());
    both_spellings(&kernel, "", "-r", "/run.sh", true).await;
    both_spellings(&kernel, "", "-x", "/run.sh", true).await;
    both_spellings(&kernel, "", "-w", "/run.sh", false).await;
}

// ── Absent paths ───────────────────────────────────────────────────────────

/// A path that does not exist is not readable, writable, or executable —
/// `path_access` reports `stat`'s error and the file test reads that as
/// false, on a writable mount and a read-only one alike.
#[tokio::test]
async fn missing_paths_answer_false_everywhere() {
    let tmp = tempfile::tempdir().unwrap();
    let kernel = kernel_at(tmp.path());
    for op in ["-r", "-w", "-x"] {
        both_spellings(&kernel, "", op, "/v/nope.txt", false).await;
        both_spellings(&kernel, "", op, "/v/bin/definitely-not-a-builtin", false).await;
    }
}
