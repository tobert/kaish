//! A `cd` or an `alias` takes effect for the rest of its own statement.
//!
//! Session state — cwd, aliases, ignore config, output limit — travels on the
//! threaded `ExecContext` during a run and is published to the kernel's
//! `exec_ctx` slot once each top-level statement finishes (GH #369). The slot
//! is therefore between-statements state. A site that reads a session field
//! from it while a statement is still running sees the value from *before*
//! that statement, and `cd d && ls *.txt` globs the wrong directory.
//!
//! Every row here is a program whose second half must observe what its first
//! half changed. They are cheap to break: routing any one of these reads back
//! through `self.exec_ctx.read()` turns exactly one row red.

#![cfg(feature = "localfs")]
// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use common::kernel_at;
use kaish_kernel::ast::Value;

/// Glob expansion resolves against the cwd the same statement just set.
#[tokio::test]
async fn a_glob_expands_in_the_directory_the_statement_cd_ed_into() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    std::fs::write(dir.path().join("sub/a.txt"), "one").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("cd sub && ls *.txt").await.expect("execute");
    assert_eq!(r.code, 0, "the glob did not see the new cwd: {}", r.err);
    assert_eq!(r.text_out().trim(), "a.txt");
}

/// A glob in a `for` list resolves the same way. It is a separate expansion
/// site from the one command arguments use, so it needs its own row.
#[tokio::test]
async fn a_for_list_glob_expands_in_the_new_directory() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    std::fs::write(dir.path().join("sub/a.txt"), "one").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute("cd sub && for f in *.txt; do echo \"got $f\"; done")
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "the for-list glob did not see the new cwd: {}", r.err);
    assert_eq!(r.text_out().trim(), "got a.txt");
}

/// An alias defined earlier in the statement resolves later in it.
#[tokio::test]
async fn an_alias_resolves_in_the_statement_that_defined_it() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel
        .execute("alias greet='echo hi' && greet")
        .await
        .expect("execute");
    assert_eq!(r.code, 0, "the alias was not found in its own statement: {}", r.err);
    assert_eq!(r.text_out().trim(), "hi");
}

/// `[[ -f rel ]]` and `test -f rel` must give the same answer. They read the
/// cwd from different places, so a stale read shows up as the two spellings
/// disagreeing — which is the failure GH #101 already ruled out.
#[tokio::test]
async fn the_two_file_test_spellings_agree_after_a_cd() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    std::fs::write(dir.path().join("sub/a.txt"), "one").unwrap();
    // A kernel each: `cd` persists between statements, so a second call on the
    // same kernel would be resolving `sub/sub`.
    let bracket = kernel_at(dir.path())
        .execute("cd sub && [[ -f a.txt ]] && echo FOUND")
        .await
        .expect("execute");
    let builtin = kernel_at(dir.path())
        .execute("cd sub && test -f a.txt && echo FOUND")
        .await
        .expect("execute");

    assert_eq!(
        (bracket.text_out().trim(), bracket.code),
        (builtin.text_out().trim(), builtin.code),
        "`[[ -f ]]` and `test -f` disagree after a cd",
    );
    assert_eq!(bracket.text_out().trim(), "FOUND");
}

/// `source` resolves a relative path against the cwd the statement just set.
#[tokio::test]
async fn source_resolves_a_relative_path_after_a_cd() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    std::fs::write(dir.path().join("sub/s.kai"), "echo SOURCED\n").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("cd sub && source ./s.kai").await.expect("execute");
    assert_eq!(r.code, 0, "source did not see the new cwd: {}", r.err);
    assert_eq!(r.text_out().trim(), "SOURCED");
}

/// An external command spawns in the cwd the statement just set, and a
/// relative `./name` resolves against it. This is the row that reaches
/// `try_execute_external_on_path`, which takes its argv and cancel token from
/// the threaded context and so must take its cwd from there too.
#[cfg(feature = "subprocess")]
#[tokio::test]
async fn a_relative_external_command_runs_after_a_cd() {
    use std::os::unix::fs::PermissionsExt;

    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    let script = dir.path().join("sub/run.sh");
    std::fs::write(&script, "#!/bin/sh\necho RAN\n").unwrap();
    std::fs::set_permissions(&script, std::fs::Permissions::from_mode(0o755)).unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("cd sub && ./run.sh").await.expect("execute");
    assert_eq!(r.code, 0, "the external did not resolve against the new cwd: {}", r.err);
    assert_eq!(r.text_out().trim(), "RAN");
}

/// The argv door runs one command the way a statement does, so a `cd`
/// through it outlives the call. It builds its own root context and is the
/// one execute entry that is not a statement loop, so it needs its own
/// publish back to the slot the public accessors read.
#[tokio::test]
async fn the_argv_door_publishes_its_session_back() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    let kernel = kernel_at(dir.path());

    let before = kernel.cwd().await;
    kernel
        .execute_argv("cd", &[Value::String("sub".into())])
        .await
        .expect("cd through the argv door");

    assert_eq!(kernel.cwd().await, before.join("sub"), "the cd did not persist");
    let r = kernel.execute("pwd").await.expect("pwd");
    assert_eq!(r.text_out().trim(), before.join("sub").display().to_string());
}

/// A `&` job inherits the session of the statement that spawned it. The fork
/// takes its session from the slot, which holds the state from before this
/// statement, so `cd d && ls &` would run the job in the old directory.
#[tokio::test]
async fn a_background_job_inherits_the_statements_cd() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::create_dir(dir.path().join("sub")).unwrap();
    std::fs::write(dir.path().join("sub/a.txt"), "one").unwrap();
    let kernel = kernel_at(dir.path());

    let r = kernel.execute("cd sub && ls *.txt > seen.txt &").await.expect("execute");
    assert_eq!(r.code, 0, "backgrounding failed: {}", r.err);

    // The job runs on its own task; poll rather than sleep a fixed span.
    let inner = dir.path().join("sub/seen.txt");
    for _ in 0..100 {
        if inner.exists() {
            break;
        }
        tokio::time::sleep(std::time::Duration::from_millis(20)).await;
    }
    assert!(inner.exists(), "the job ran in the pre-cd directory");
    assert!(!dir.path().join("seen.txt").exists(), "the job wrote outside the cd");
}
