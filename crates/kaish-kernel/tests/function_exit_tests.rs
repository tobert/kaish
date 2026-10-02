//! State a function call leaves behind when `exit` ends it.
//!
//! The call must still pop its frame and restore the caller's positional
//! parameters, or a later `execute` on the same kernel sees the function's
//! locals.

#![allow(clippy::unwrap_used, clippy::expect_used)]

mod common;

use kaish_kernel::Kernel;

#[tokio::test]
async fn exit_in_function_pops_locals_and_restores_positionals() {
    let kernel = Kernel::transient().expect("kernel");
    let (out, code) = common::run(
        &kernel,
        "f(){ local v=inner; echo \"in:$1\"; exit 3; }; f arg; echo after",
    )
    .await;
    assert_eq!((out.as_str(), code), ("in:arg", 3));

    let (out, code) = common::run(&kernel, "echo \"[${v:-}][${1:-}]\"").await;
    assert_eq!((out.as_str(), code), ("[][]", 0), "locals and $1 must not outlive the call");
}

// `timeout` re-dispatches its command through the kernel. It is not a
// subshell, so an `exit` in a function it runs ends the script. Kaish-only:
// bash's `timeout` runs a program, never a function.
async fn run_arc(script: &str) -> (String, i64) {
    let kernel = Kernel::new(kaish_kernel::KernelConfig::isolated().with_skip_validation(true))
        .expect("kernel")
        .into_arc();
    common::run(&kernel, script).await
}

#[tokio::test]
async fn exit_in_function_run_by_timeout_ends_script() {
    let (out, code) = run_arc("f(){ echo hi; exit 3; }; timeout 5 f; echo after").await;
    assert_eq!((out.as_str(), code), ("hi", 3));
}

#[tokio::test]
async fn exit_in_function_run_by_timeout_in_a_stage_ends_only_the_stage() {
    let (out, code) = run_arc("f(){ exit 3; }; timeout 5 f | cat; echo after").await;
    assert_eq!((out.as_str(), code), ("after", 0));
}

#[tokio::test]
async fn function_exit_under_errexit_ends_script_with_its_code() {
    let (out, code) = run_arc("set -e; f(){ echo hi; exit 3; }; f; echo after").await;
    assert_eq!((out.as_str(), code), ("hi", 3));
}

// ---- Boundary pins ---------------------------------------------------------

#[tokio::test]
async fn function_exit_through_a_stdout_redirect_ends_script() {
    let (out, code) = run_arc("f(){ exit 3; }; f > /dev/null; echo after").await;
    assert_eq!((out.as_str(), code), ("", 3));
}

#[tokio::test]
async fn function_exit_in_case_arm_ends_script() {
    let (out, code) = run_arc("f(){ echo hi; exit 3; }; case a in a) f; echo in-arm;; esac; echo after").await;
    assert_eq!((out.as_str(), code), ("hi", 3));
}

// A redirect that fails when it is written (a full device) does not change
// the status an `exit` chose, for a command and for a compound body alike.
// The redirect's own error is still reported on stderr.
#[cfg(all(target_os = "linux", feature = "localfs"))]
#[tokio::test]
async fn exit_code_survives_a_redirect_that_fails_to_write_for_command_and_group() {
    let dir = tempfile::tempdir().unwrap();
    let kernel = common::kernel_at(dir.path());
    let command = kernel.execute("f(){ echo hi; exit 3; }; f > /dev/full").await.unwrap();
    let group = kernel.execute("{ echo hi; exit 3; } > /dev/full").await.unwrap();
    assert_eq!((command.code, group.code), (3, 3), "{command:?} {group:?}");
    assert!(!command.err.is_empty() && !group.err.is_empty(), "the write error is reported");
}

#[tokio::test]
async fn function_exit_in_scatter_worker_is_absorbed() {
    let (out, code) = run_arc(
        "w(){ echo \"w:$1\"; exit 9; }; seq 1 3 | scatter | w | gather --lines; echo after",
    )
    .await;
    assert_eq!(code, 0, "{out}");
    assert!(out.ends_with("after"), "script must continue past the scatter: {out}");
}

#[tokio::test]
async fn function_exit_in_scatter_pre_stage_is_absorbed() {
    let (out, code) = run_arc("p(){ seq 1 2; exit 9; }; p | scatter | cat | gather --lines; echo after").await;
    assert_eq!(code, 0, "{out}");
    assert!(out.ends_with("after"), "script must continue past the scatter: {out}");
}

#[cfg(feature = "localfs")]
#[tokio::test]
async fn kai_script_exit_is_absorbed() {
    let dir = tempfile::tempdir().unwrap();
    std::fs::write(dir.path().join("quitter.kai"), "echo in-script\nexit 4\n").unwrap();
    let kernel = common::kernel_at(dir.path());
    let path = dir.path().display().to_string();
    let (out, code) = common::run(&kernel, &format!("PATH=\"{path}\"; quitter; echo \"after $?\"")).await;
    assert_eq!((out.as_str(), code), ("in-script\nafter 4", 0));
}
