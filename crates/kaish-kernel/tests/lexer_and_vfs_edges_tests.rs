//! Regression tests for Batch 8: empty assignments (#193) and VFS/PatchOp nits (#195).

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;
use std::sync::Arc;

use kaish_kernel::backend::{KernelBackend, LocalBackend, PatchOp};
use kaish_kernel::vfs::{Filesystem, MemoryFs, VfsRouter};
use kaish_kernel::{Kernel, KernelConfig};

#[tokio::test]
async fn empty_assignments_work_for_var_local_and_export() {
    let kernel = Kernel::new(KernelConfig::named("empty-assign")).unwrap();

    // Bare `VAR=` sets empty string.
    let res = kernel.execute("X=1; X=; echo \"[$X]\"").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "[]\n");

    // `VAR=` at EOF works too.
    let res = kernel.execute("Y=").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");

    // `local VAR=` inside a function sets an empty local variable.
    let res = kernel
        .execute("X=outer; f() { local X=; echo \"in:[$X]\"; }; f; echo \"out:[$X]\"")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "in:[]\nout:[outer]\n");

    // `export VAR=` exports an empty string, and `export A= B=1` handles both.
    let res = kernel
        .execute("export A= B=1; echo \"A=[$A] B=[$B]\"")
        .await
        .unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "A=[] B=[1]\n");

    // `echo foo= bar` keeps `foo=` as a literal word.
    let res = kernel.execute("echo foo= bar").await.unwrap();
    assert_eq!(res.code, 0, "{res:?}");
    assert_eq!(res.text_out(), "foo= bar\n");

    // Glued non-expression token after `=` (like `foo=-n` or `CFLAGS=-O2`)
    // must NOT silently split into `foo=""` + flag `-n`.
    assert!(
        kernel.execute("echo foo=-n").await.is_err(),
        "echo foo=-n must not silently split into empty assign + -n flag"
    );
    assert!(
        kernel.execute("export CFLAGS=-O2").await.is_err(),
        "export CFLAGS=-O2 must not silently split into empty assign + -O2 flag"
    );
}

#[tokio::test]
async fn patch_line_ops_preserve_crlf_and_missing_trailing_newline() {
    let mem = MemoryFs::new();
    mem.write(Path::new("crlf.txt"), b"one\r\ntwo\r\nthree\r\n")
        .await
        .unwrap();
    mem.write(Path::new("no_nl.txt"), b"alpha\nbeta")
        .await
        .unwrap();

    let mut vfs = VfsRouter::new();
    vfs.mount("/", mem);
    let backend = LocalBackend::new(Arc::new(vfs));

    // CRLF file keeps `\r\n` across ReplaceLine, InsertLine, and DeleteLine.
    backend
        .patch(
            Path::new("/crlf.txt"),
            &[
                PatchOp::ReplaceLine {
                    line: 2,
                    content: "TWO".to_string(),
                    expected: Some("two".to_string()),
                },
                PatchOp::InsertLine {
                    line: 3,
                    content: "mid".to_string(),
                },
                PatchOp::DeleteLine {
                    line: 1,
                    expected: Some("one".to_string()),
                },
            ],
        )
        .await
        .unwrap();
    let crlf_bytes = backend.read(Path::new("/crlf.txt"), None).await.unwrap();
    assert_eq!(crlf_bytes, b"TWO\r\nmid\r\nthree\r\n");

    // File without a trailing newline does not gain one.
    backend
        .patch(
            Path::new("/no_nl.txt"),
            &[PatchOp::ReplaceLine {
                line: 2,
                content: "BETA".to_string(),
                expected: Some("beta".to_string()),
            }],
        )
        .await
        .unwrap();
    let no_nl_bytes = backend.read(Path::new("/no_nl.txt"), None).await.unwrap();
    assert_eq!(no_nl_bytes, b"alpha\nBETA");

    // Deleting line 1 from `"a\n\n"` leaves the remaining blank line `"\n"`.
    backend
        .write(
            Path::new("/blank.txt"),
            b"a\n\n",
            kaish_kernel::backend::WriteMode::Overwrite,
        )
        .await
        .unwrap();
    backend
        .patch(
            Path::new("/blank.txt"),
            &[PatchOp::DeleteLine {
                line: 1,
                expected: Some("a".to_string()),
            }],
        )
        .await
        .unwrap();
    assert_eq!(
        backend.read(Path::new("/blank.txt"), None).await.unwrap(),
        b"\n"
    );
}

#[cfg(all(unix, feature = "localfs"))]
#[tokio::test]
async fn vfs_router_preserves_non_utf8_paths_losslessly() {
    use std::ffi::OsStr;
    use std::os::unix::ffi::OsStrExt;

    let dir = tempfile::tempdir().unwrap();
    let bad_name = OsStr::from_bytes(b"bad\xffname.txt");
    std::fs::write(dir.path().join(bad_name), b"raw-bytes").unwrap();

    let mut vfs = VfsRouter::new();
    vfs.mount("/mnt", kaish_kernel::vfs::LocalFs::new(dir.path().to_path_buf()));

    let mut vfs_path = std::ffi::OsString::from("/mnt/");
    vfs_path.push(bad_name);
    let data = vfs.read(Path::new(&vfs_path)).await.unwrap();
    assert_eq!(data, b"raw-bytes");
}
