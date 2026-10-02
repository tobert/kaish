//! An unknown flag on a builtin is refused with a short statement that kaish
//! does not support it: no clap usage block, no `--` tip, and no pointer to a
//! program outside kaish.

#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::kernel_at;
use rstest::rstest;
use tempfile::tempdir;

#[rstest]
#[case::ls_short("ls -Z", "ls: -Z is not supported")]
#[case::ls_long("ls --bogus", "ls: --bogus is not supported")]
#[case::grep_short("grep -Z pattern f", "grep: -Z is not supported")]
#[case::stat_filesystem("stat -f f", "stat: -f is not supported")]
#[case::cat_short("cat -Z f", "cat: -Z is not supported")]
#[case::find_predicate("find . -frobnicate", "find: -frobnicate is not supported")]
#[tokio::test]
async fn unknown_flag_is_not_supported(#[case] script: &str, #[case] expected: &str) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f"), "x\n").unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(script).await.unwrap();
    assert_eq!(result.code, 2, "{script}: {}", result.err);
    assert!(result.err.starts_with(expected), "{script}: {}", result.err);
    for leak in ["tip:", "Usage", "unexpected argument", "error:", "/usr", "bin/"] {
        assert!(!result.err.contains(leak), "{script} leaks {leak:?}: {}", result.err);
    }
    assert_eq!(result.err.trim_end().lines().count(), 1, "{script}: {}", result.err);
}
