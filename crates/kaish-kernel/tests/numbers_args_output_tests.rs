//! Regression tests for Batch 4 (numbers, arguments, and output correctness):
//! - `cat 0.10` preserves raw numeric filename; `printf %s -0` prints `-0`.
//! - Space-form flag values (`--flag 0.10`, `-v a=0.10`) preserve numeric fidelity.
//! - `timeout` rejects leading-zero durations (`010`, `010ms`) and immediately
//!   exits 124 on zero duration without racing the child.
//! - Ten read-only builtins do not re-glob quoted operands (`"*.txt"`).
//! - `OutputNode::estimated_byte_size` matches `OutputNode::write_canonical`.
//! - `cut` does not merge unterminated lines across multiple files.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;

use kaish_kernel::vfs::Filesystem;
use kaish_kernel::{Kernel, KernelConfig};
use kaish_types::OutputNode;

#[tokio::test]
async fn numeric_literal_fidelity_in_cat_printf_and_space_form_flags() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    kernel.vfs().write(Path::new("/0.10"), b"exact-ten\n").await.unwrap();
    kernel.vfs().write(Path::new("/0.1"), b"wrong-one\n").await.unwrap();

    // `cat 0.10`, `head -3 0.10`, `tail -3 0.10`, `ls -1 0.10` must read `/0.10`, not `/0.1`.
    let r = kernel.execute("cat /0.10").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert_eq!(r.text_out(), "exact-ten\n");

    for cmd in ["cd /; cat 0.10", "cd /; head -3 0.10", "cd /; tail -3 0.10"] {
        let r = kernel.execute(cmd).await.unwrap();
        assert!(r.ok(), "{cmd} failed: {}", r.err);
        assert_eq!(r.text_out(), "exact-ten\n", "{cmd} read wrong file");
    }

    let r = kernel.execute("cd /; ls -1 0.10").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert_eq!(r.text_out().trim(), "0.10");

    // Duplicate single-value flags must invalidate stale `named_raw` entries.
    let r = kernel.execute("head -n 0.10 -n 1 /0.10").await.unwrap();
    assert!(r.ok(), "head duplicate -n failed: {}", r.err);
    assert_eq!(r.text_out(), "exact-ten\n");

    let r = kernel.execute("printf 'a,b\\n' | awk -F 0.10 -F , '{ print $2 }'").await.unwrap();
    assert!(r.ok(), "awk duplicate -F failed: {}", r.err);
    assert_eq!(r.text_out(), "b\n");

    // `printf %s -0` prints `-0`, `printf %s 0.10` prints `0.10`, `%d` still 0.
    let r = kernel.execute("printf '%s|%s|%d' -0 0.10 -0").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert_eq!(r.text_out(), "-0|0.10|0");

    // Space-form `-v a=0.10` in awk preserves exact numeric spelling.
    let r = kernel.execute("awk -v a=0.10 'BEGIN { print a }'").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert_eq!(r.text_out(), "0.10\n");
}

#[tokio::test]
async fn timeout_rejects_leading_zeros_and_zero_duration_exits_124_without_race() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap().into_arc();

    for bad in ["010", "00", "010ms", "010s"] {
        let r = kernel.execute(&format!("timeout {bad} true")).await.unwrap();
        assert_eq!(r.code, 2, "timeout {bad} should exit 2, got {} ({})", r.code, r.err);
    }

    // Zero duration must reliably exit 124 without running the inner command.
    for _ in 0..10 {
        let r = kernel.execute("timeout 0 echo should-not-run").await.unwrap();
        assert_eq!(r.code, 124, "timeout 0 must exit 124, got {} (out={:?})", r.code, r.text_out());
        assert!(r.text_out().is_empty(), "timeout 0 must not run child");
    }
}

#[tokio::test]
async fn read_only_builtins_do_not_reglob_quoted_operands() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    kernel.vfs().write(Path::new("/a.txt"), b"alpha\n").await.unwrap();
    kernel.vfs().write(Path::new("/b.txt"), b"beta\n").await.unwrap();

    // Unquoted glob still expands via the kernel.
    let r = kernel.execute("cd /; cat *.txt").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert_eq!(r.text_out(), "alpha\nbeta\n");

    // Quoted `"*.txt"` must NOT be re-globbed by any of the 10 builtins.
    for cmd in [
        r#"cat "*.txt""#,
        r#"head "*.txt""#,
        r#"tail "*.txt""#,
        r#"wc "*.txt""#,
        r#"ls "*.txt""#,
        r#"ls -d "*.txt""#,
        r#"file "*.txt""#,
        r#"tac "*.txt""#,
        r#"xxd "*.txt""#,
        r#"base64 "*.txt""#,
        r#"checksum "*.txt""#,
    ] {
        let r = kernel.execute(&format!("cd /; {cmd}")).await.unwrap();
        assert_ne!(
            r.code, 0,
            "{cmd} should fail (no literal file '*.txt'), but exited 0 with out={:?}",
            r.text_out()
        );
    }
}

#[test]
fn output_node_estimated_byte_size_matches_write_canonical() {
    let cases = vec![
        OutputNode::text("hello world"),
        OutputNode::new("file.txt"),
        OutputNode::new("file.txt").with_text("ignored-in-brace-notation"),
        OutputNode::new("dir").with_children(vec![
            OutputNode::new("a.rs").with_text("extra"),
            OutputNode::new("b.rs"),
        ]),
    ];

    for node in cases {
        let mut buf = Vec::new();
        let written = node.write_canonical(&mut buf, usize::MAX).unwrap();
        assert_eq!(
            node.estimated_byte_size(),
            written,
            "mismatch for node {node:?}: buf={:?}",
            String::from_utf8_lossy(&buf)
        );
        assert_eq!(written, buf.len());
    }
}

#[tokio::test]
async fn cut_does_not_merge_unterminated_lines_across_files() {
    let kernel = Kernel::new(KernelConfig::isolated()).unwrap();
    kernel.vfs().write(Path::new("/f1.txt"), b"a:b").await.unwrap();
    kernel.vfs().write(Path::new("/f2.txt"), b"c:d\n").await.unwrap();

    let r = kernel.execute("cut -d: -f2 /f1.txt /f2.txt").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert_eq!(r.text_out(), "b\nd\n");

    // `sort` must also separate unterminated lines across files.
    kernel.vfs().write(Path::new("/s1.txt"), b"b").await.unwrap();
    kernel.vfs().write(Path::new("/s2.txt"), b"a\n").await.unwrap();
    let r = kernel.execute("sort /s1.txt /s2.txt").await.unwrap();
    assert!(r.ok(), "err: {}", r.err);
    assert_eq!(r.text_out(), "a\nb\n");
}

#[cfg(all(unix, feature = "subprocess"))]
#[tokio::test]
async fn external_stdout_redirect_still_tees_stderr_to_job_stream() {
    let kernel = Kernel::new(KernelConfig::repl()).unwrap().into_arc();
    let res = kernel
        .execute("/bin/sh -c 'echo out-line; echo err-line >&2' > /tmp/kaish_stderr_tee_test.txt &")
        .await
        .unwrap();
    assert!(res.ok(), "err: {}", res.err);
    assert_eq!(res.background_jobs.len(), 1);
    let job_id = res.background_jobs[0];

    let job_res = kernel.jobs().wait(job_id).await.unwrap();
    assert!(job_res.ok(), "job failed: {}", job_res.err);

    let stderr_bytes = kernel.jobs().read_stderr(job_id).await.unwrap();
    let stderr_str = String::from_utf8_lossy(&stderr_bytes);
    assert!(
        stderr_str.contains("err-line"),
        "stderr stream must receive external stderr even when stdout is redirected, got: {stderr_str:?}"
    );
    let _ = std::fs::remove_file("/tmp/kaish_stderr_tee_test.txt");
}
