//! `$PWD` and `$OLDPWD` follow `cd`.
//!
//! They did not. Both were whatever the process inherited from the
//! environment, and `cd` never touched either:
//!
//! ```text
//! cd /tmp; echo $PWD      # the directory the shell STARTED in
//! cd /tmp; pwd            # /tmp — correct
//! ```
//!
//! The wrong value was silent, and the validator vouched for it: `PWD` and
//! `OLDPWD` are in `scope_tracker`'s known-variable list, so `cd x; foo "$PWD"`
//! passed validation and then handed `foo` a stale path. `$OLDPWD` was worse
//! than stale — it held a directory from the invoking shell's history, which
//! has nothing to do with this session.
//!
//! Maintained in `ExecContext::set_cwd` rather than in the `cd` builtin, so
//! there is one place a working directory changes and one place the variables
//! follow it. A second writer would drift.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "localfs")]

use kaish_kernel::{Kernel, KernelConfig};

async fn out(script: &str) -> String {
    let k = Kernel::new(KernelConfig::repl()).expect("kernel");
    let r = k
        .execute(script)
        .await
        .unwrap_or_else(|e| panic!("`{script}` must run: {e}"));
    r.text_out().trim_end().to_string()
}

/// `$PWD` agrees with `pwd`, which was already right.
#[tokio::test]
async fn pwd_variable_agrees_with_the_pwd_builtin() {
    assert_eq!(out("cd /tmp; echo $PWD").await, "/tmp");
    assert_eq!(out("cd /tmp; echo \"$PWD|$(pwd)\"").await, "/tmp|/tmp");
}

/// It is set before any `cd`, or a script that reads it first sees whatever
/// the environment happened to hold.
#[tokio::test]
async fn pwd_is_set_before_any_cd() {
    let here = out("pwd").await;
    assert_eq!(out("echo $PWD").await, here, "PWD must be seeded at startup");
}

/// `$OLDPWD` is this session's previous directory — not the invoking shell's.
#[tokio::test]
async fn oldpwd_is_this_sessions_previous_directory() {
    assert_eq!(out("cd /tmp; cd /usr; echo $OLDPWD").await, "/tmp");
    assert_eq!(out("cd /tmp; cd /usr; echo \"$PWD|$OLDPWD\"").await, "/usr|/tmp");
}

/// `cd -` and the variables agree — it reads the same previous directory the
/// variable reports.
#[tokio::test]
async fn cd_dash_agrees_with_oldpwd() {
    // `cd -` prints the new directory (as bash does) and with no trailing
    // newline, so its output would run into the next one — redirect it away
    // and ask the variable, which is what this is about.
    assert_eq!(out("cd /tmp; cd /usr; cd - > /dev/null; echo $PWD").await, "/tmp");
}

/// Before any `cd` there is no previous directory, and `$OLDPWD` must not
/// claim otherwise — the invoking shell's `OLDPWD` describes its history, not
/// this session's, and `cd -` refuses with "OLDPWD not set".
#[tokio::test]
async fn oldpwd_is_absent_until_the_first_cd() {
    assert_eq!(out("echo \"[${OLDPWD:-unset}]\"").await, "[unset]");
}

/// A failed `cd` moves nothing, so it must not move the variables either.
#[tokio::test]
async fn a_failed_cd_leaves_them_alone() {
    let script = "cd /tmp; cd /definitely-not-a-directory-here; echo \"$PWD\"";
    assert_eq!(out(script).await, "/tmp", "a refused cd changes nothing");
}

/// `cd` inside a substitution is scoped to it — the parent's directory and its
/// variables are unchanged, and they stay consistent with each other.
#[tokio::test]
async fn a_substitution_does_not_leak_its_cd() {
    let here = out("pwd").await;
    let got = out("x=$(cd /tmp; pwd); echo \"$x|$PWD\"").await;
    assert_eq!(got, format!("/tmp|{here}"), "the cd is scoped, and PWD tracks");
}

/// `reset()` puts the session back at `/`, and `$PWD` must say so. It sets
/// `ctx.cwd` directly rather than going through `set_cwd`, so it needs its own
/// write — and `initial_vars` can carry an inherited `PWD` that would
/// otherwise survive and name a directory the session is no longer in.
#[tokio::test]
async fn reset_puts_pwd_back_too() {
    let k = Kernel::new(KernelConfig::repl()).expect("kernel");
    k.execute("cd /tmp").await.expect("cd");
    k.reset().await.expect("reset");
    let r = k.execute("echo \"$PWD|$(pwd)\"").await.expect("execute");
    assert_eq!(r.text_out().trim_end(), "/|/", "reset returns to / and says so");

    let r = k.execute("echo \"[${OLDPWD:-unset}]\"").await.expect("execute");
    assert_eq!(r.text_out().trim_end(), "[unset]", "no previous directory after reset");
}
