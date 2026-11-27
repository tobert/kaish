//! Regression tests for ordered stream merges (#485) and JobFs range reads (#195).

#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use std::path::Path;

use kaish_kernel::vfs::{Filesystem, JobFs};
use kaish_kernel::{Kernel, KernelConfig};
use kaish_vfs::ReadRange;

const BOTH: &str = "both() { echo out; echo err >&2; echo out2; echo err2 >&2; }\n";

#[tokio::test]
async fn merge_both_to_file_preserves_write_order() {
    let kernel = Kernel::new(KernelConfig::named("ordered-merge-file")).unwrap();
    let res = kernel
        .execute(&format!("{BOTH}both &> /merged.txt; cat /merged.txt"))
        .await
        .unwrap();
    assert_eq!(res.code, 0);
    assert_eq!(res.text_out(), "out\nerr\nout2\nerr2\n");
    assert_eq!(res.err, "");
}

#[tokio::test]
async fn background_job_2_to_1_interleaved_publishes_all_bytes_once() {
    let kernel = Kernel::new(KernelConfig::named("ordered-merge-job")).unwrap();
    kernel
        .execute(&format!("{BOTH}both 2>&1 &"))
        .await
        .unwrap();
    let id = kaish_kernel::scheduler::JobId(1);
    let result = kernel.jobs().wait(id).await.unwrap();
    assert_eq!(result.code, 0);
    assert_eq!(result.text_out(), "out\nerr\nout2\nerr2\n");
    assert_eq!(result.err, "");
    let stdout = String::from_utf8(kernel.jobs().read_stdout(id).await.unwrap()).unwrap();
    let stderr = String::from_utf8(kernel.jobs().read_stderr(id).await.unwrap()).unwrap();
    // Live job streaming already pushed stdout lines as `both` ran, then
    // appended the merged stderr suffix once without duplicating stdout.
    assert_eq!(stdout, "out\nout2\nerr\nerr2\n");
    assert_eq!(stderr, "");
}

#[cfg(all(unix, feature = "subprocess", feature = "localfs"))]
#[tokio::test]
async fn external_command_2_to_1_preserves_write_order() {
    let dir = tempfile::tempdir().unwrap();
    let variables = std::collections::HashMap::from([(
        "PATH".to_string(),
        kaish_kernel::ast::Value::String(std::env::var("PATH").expect("PATH")),
    )]);
    let kernel = Kernel::new(
        KernelConfig::repl()
            .with_cwd(dir.path().to_path_buf())
            .with_initial_vars(variables),
    )
    .unwrap();
    // Sleep briefly between writes so the pipe reader observes distinct chunks.
    let res = kernel
        .execute("sh -c 'echo out; sleep 0.05; echo err >&2; sleep 0.05; echo out2; sleep 0.05; echo err2 >&2' 2>&1")
        .await
        .unwrap();
    assert_eq!(res.code, 0);
    assert_eq!(res.text_out(), "out\nerr\nout2\nerr2\n");
    assert_eq!(res.err, "");
}

#[tokio::test]
async fn merge_2_to_1_preserves_structured_data_sideband() {
    let kernel = Kernel::new(KernelConfig::named("ordered-merge-data")).unwrap();
    kernel.execute("mkdir -p /dir; write /dir/a.txt hello").await.unwrap();
    // `find` populates `.data` with matched paths and emits an error on stderr
    // for the missing operand; `2>&1` must not drop `.data`.
    let res = kernel.execute("find /dir /nosuch 2>&1").await.unwrap();
    assert!(
        res.data.is_some(),
        "2>&1 must preserve .data sideband when stderr is merged: {res:?}"
    );
    assert!(res.text_out().contains("/dir/a.txt"));
    assert!(res.text_out().contains("/nosuch"));
    assert_eq!(res.err, "");
}

#[tokio::test]
async fn jobfs_read_range_slices_stdout_and_stderr() {
    let kernel = Kernel::new(KernelConfig::named("jobfs-range")).unwrap();
    kernel
        .execute("{ printf '0123456789'; printf 'abcdefghij' >&2; } &")
        .await
        .unwrap();
    let id = kaish_kernel::scheduler::JobId(1);
    kernel.jobs().wait(id).await.unwrap();

    let jobfs = JobFs::new(kernel.jobs().clone());
    let stdout_path = Path::new("1/stdout");
    let stderr_path = Path::new("1/stderr");

    assert_eq!(
        jobfs
            .read_range(
                stdout_path,
                Some(ReadRange {
                    offset: Some(0),
                    limit: Some(4),
                    ..ReadRange::default()
                })
            )
            .await
            .unwrap(),
        b"0123"
    );
    assert_eq!(
        jobfs
            .read_range(
                stdout_path,
                Some(ReadRange {
                    offset: Some(4),
                    limit: Some(3),
                    ..ReadRange::default()
                })
            )
            .await
            .unwrap(),
        b"456"
    );
    assert_eq!(
        jobfs
            .read_range(
                stdout_path,
                Some(ReadRange {
                    offset: Some(7),
                    limit: None,
                    ..ReadRange::default()
                })
            )
            .await
            .unwrap(),
        b"789"
    );
    assert_eq!(
        jobfs
            .read_range(
                stdout_path,
                Some(ReadRange {
                    offset: Some(100),
                    limit: Some(5),
                    ..ReadRange::default()
                })
            )
            .await
            .unwrap(),
        b""
    );
    assert_eq!(
        jobfs
            .read_range(
                stdout_path,
                Some(ReadRange {
                    offset: Some(2),
                    limit: Some(0),
                    ..ReadRange::default()
                })
            )
            .await
            .unwrap(),
        b""
    );
    assert_eq!(
        jobfs
            .read_range(
                stderr_path,
                Some(ReadRange {
                    offset: Some(3),
                    limit: Some(4),
                    ..ReadRange::default()
                })
            )
            .await
            .unwrap(),
        b"defg"
    );
}
