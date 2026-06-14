//! Glob expansion can't inject flags into a builtin (the classic `rm *` with a
//! `-rf.txt` file in the directory).
//!
//! In bash, `rm *` expands `-rf.txt` into the argv where `rm` re-parses it as
//! `-r -f`. kaish expands a glob into *positional* values that `to_argv()` emits
//! after `--`, so a flag-shaped match is data, never a flag. This test locks
//! that in: `rm *` over a dir holding `-rf.txt` and a non-empty `sub/` deletes
//! the files as operands but refuses `sub/` (no `-r` was injected), so the
//! subtree survives.

mod common;

use common::kernel_at;
use std::fs;

#[tokio::test]
async fn rm_star_does_not_let_a_dash_rf_file_inject_recursive_force() {
    let tmp = tempfile::tempdir().unwrap();
    let dir = tmp.path();
    fs::create_dir(dir.join("sub")).unwrap();
    fs::write(dir.join("sub/inside.txt"), b"keep").unwrap();
    fs::write(dir.join("a.txt"), b"a").unwrap();
    fs::write(dir.join("-rf.txt"), b"x").unwrap();

    let kernel = kernel_at(dir);
    let result = kernel.execute("rm *").await.expect("execute");

    // `sub/` must survive: if `-rf.txt` had been read as `-r -f`, the whole
    // subtree would have been force-deleted.
    assert!(dir.join("sub").is_dir(), "sub/ must survive: flags were injected!");
    assert!(dir.join("sub/inside.txt").is_file(), "subtree content must survive");
    // The flag-shaped name was treated as a deletable operand, not a flag.
    assert!(!dir.join("-rf.txt").exists(), "-rf.txt should be removed as an operand");
    assert!(!dir.join("a.txt").exists(), "a.txt should be removed");
    // rm refused the directory without -r — exit 1 (the deletion-error code),
    // and stderr names `sub`. This proves sub/ survived because rm *tried and
    // was refused*, not because it was silently skipped for some other reason.
    assert_eq!(result.code, 1, "rm of a dir without -r should be exit 1: {:?}", result.err);
    assert!(result.err.contains("sub"), "stderr should name the refused dir: {:?}", result.err);
}
