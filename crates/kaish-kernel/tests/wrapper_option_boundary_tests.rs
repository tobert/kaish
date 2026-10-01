//! A builtin that runs another command stops reading its own options where
//! the wrapped command begins, as POSIX `timeout`, `env`, and `exec` do:
//! `timeout 5 python3 -c "..."` hands `-c` to python3.
//!
//! Every case runs through `Kernel::execute`, so the validator, the argument
//! binder, and the builtin's own parse all see the script. The binder cases
//! at the bottom check the two binders (runtime and validation) agree.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use kaish_kernel::{Kernel, KernelConfig};

/// A hermetic kernel: only builtins, validation on. `timeout` re-dispatches
/// its command, which needs the shared (`Arc`) kernel.
fn isolated() -> std::sync::Arc<Kernel> {
    Kernel::new(KernelConfig::isolated()).expect("kernel").into_arc()
}

/// Stdout (untrimmed, so a missing newline is visible) and exit code.
async fn run(kernel: &std::sync::Arc<Kernel>, script: &str) -> (String, i64, String) {
    let result = kernel.execute(script).await.expect("kernel execute");
    (result.text_out().to_string(), result.code, result.err.clone())
}

#[tokio::test]
async fn timeout_passes_a_flag_to_a_builtin() {
    let kernel = isolated();
    let (out, code, err) = run(&kernel, "timeout 5 echo -n hi").await;
    assert_eq!((out.as_str(), code), ("hi", 0), "stderr: {err}");
}

#[tokio::test]
async fn timeout_passes_a_value_flag_to_a_builtin() {
    let kernel = isolated();
    let (out, code, err) = run(&kernel, "timeout 5 seq -s , 1 3").await;
    assert_eq!((out.trim_end(), code), ("1,2,3", 0), "stderr: {err}");
}

#[tokio::test]
async fn timeout_passes_a_flag_to_a_builtin_reading_the_pipe() {
    let kernel = isolated();
    let (out, code, err) = run(&kernel, "printf 'a\nb\na\n' | timeout 5 grep -c a").await;
    assert_eq!((out.trim(), code), ("2", 0), "stderr: {err}");
}

#[tokio::test]
async fn timeout_double_dash_before_the_duration_still_works() {
    let kernel = isolated();
    let (out, code, err) = run(&kernel, "timeout -- 5 echo -n x").await;
    assert_eq!((out.as_str(), code), ("x", 0), "stderr: {err}");
}

/// GNU reads this `--` as the command's name and fails; kaish has always
/// skipped it, and keeps skipping it.
#[tokio::test]
async fn timeout_double_dash_after_the_duration_is_skipped() {
    let kernel = isolated();
    let (out, code, err) = run(&kernel, "timeout 5 -- echo -n x").await;
    assert_eq!((out.as_str(), code), ("x", 0), "stderr: {err}");
}

/// The inner builtin receives exactly what it receives when called directly:
/// same stdout, same exit code.
async fn assert_same_as_direct(kernel: &std::sync::Arc<Kernel>, setup: &str, command: &str) {
    let direct = run(kernel, &format!("{setup}{command}")).await;
    let wrapped = run(kernel, &format!("{setup}timeout 5 {command}")).await;
    assert_eq!(
        (&wrapped.0, wrapped.1),
        (&direct.0, direct.1),
        "`timeout 5 {command}` differs from the direct call (stderr: {})",
        wrapped.2
    );
}

#[tokio::test]
async fn timeout_keeps_numerals_as_written() {
    let kernel = isolated();
    for numeral in ["-0", "0.10", "1.0", "-0.0", "007"] {
        assert_same_as_direct(&kernel, "", &format!("echo {numeral}")).await;
    }
    let (out, _, _) = run(&kernel, "timeout 5 echo -0").await;
    assert_eq!(out.trim(), "-0");
}

#[tokio::test]
async fn timeout_keeps_a_quoted_dash_word_as_data() {
    let kernel = isolated();
    for command in [
        "echo \"-n\" hi",
        "echo \"--key=value\" hi",
        "echo \"--json\" hi",
        "echo -- -n",
        "echo --key=value hi",
        "echo --k=5 hi",
    ] {
        assert_same_as_direct(&kernel, "", command).await;
    }
    let (out, _, _) = run(&kernel, "timeout 5 echo \"--key=value\" hi").await;
    assert_eq!(out.trim(), "--key=value hi");
}

#[tokio::test]
async fn timeout_keeps_a_variable_that_holds_a_dash_word() {
    let kernel = isolated();
    assert_same_as_direct(&kernel, "x=\"--\"; ", "echo $x hi").await;
    assert_same_as_direct(&kernel, "x=\"-n\"; ", "echo $x hi").await;
}

#[tokio::test]
async fn timeout_json_before_the_duration_is_the_kernels() {
    let kernel = isolated();
    let (json, code, err) = run(&kernel, "timeout --json 5 echo hi").await;
    let (expected, _, _) = run(&kernel, "echo hi --json").await;
    assert_eq!(code, 0, "stderr: {err}");
    assert_eq!(json, expected);
    assert!(json.starts_with('"'), "expected JSON rendering, got {json:?}");
}

#[tokio::test]
async fn timeout_unknown_option_before_the_duration_is_refused_by_name() {
    let kernel = isolated();
    let (out, code, err) = run(&kernel, "timeout -s KILL 5 echo ran").await;
    assert_ne!(code, 0);
    assert!(out.is_empty(), "the command must not run: {out:?}");
    assert!(err.contains("-s"), "the refusal should name the option: {err}");
}

#[tokio::test]
async fn timeout_still_kills_a_builtin_that_outlasts_it() {
    let kernel = isolated();
    let (_, code, err) = run(&kernel, "timeout 100ms sleep 10").await;
    assert_eq!(code, 124, "stderr: {err}");
}

#[cfg(feature = "subprocess")]
mod external {
    use super::*;
    use kaish_kernel::ast::Value;
    use std::collections::HashMap;

    /// The kernel never reads the OS environment; PATH comes from here, as the
    /// REPL seeds it.
    fn repl_kernel() -> std::sync::Arc<Kernel> {
        let mut vars = HashMap::new();
        vars.insert("PATH".to_string(), Value::String(std::env::var("PATH").unwrap_or_default()));
        let config = KernelConfig::repl().with_initial_vars(vars);
        Kernel::new(config).expect("kernel").into_arc()
    }

    #[tokio::test]
    async fn timeout_passes_dash_c_to_an_external_shell() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "timeout 5 sh -c 'echo hi; exit 3'").await;
        assert_eq!((out.trim(), code), ("hi", 3), "stderr: {err}");
    }

    #[tokio::test]
    async fn timeout_passes_a_dash_word_that_looks_like_its_own_option() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "timeout 5 sh -c 'echo \"$0\"' --json").await;
        assert_eq!((out.trim(), code), ("--json", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn env_passes_a_flag_to_the_command() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "env XDG=1 sh -c 'echo \"$XDG\"'").await;
        assert_eq!((out.trim(), code), ("1", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn env_passes_dash_words_that_look_like_its_own_options() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "env X=1 sh -c 'echo \"$0 $1 $2\"' -i -u -0").await;
        assert_eq!((out.trim(), code), ("-i -u -0", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn env_options_before_the_command_still_work() {
        let kernel = repl_kernel();
        let (out, _, err) = run(&kernel, "env -i ONLY=1 /bin/sh -c 'echo \"$ONLY-${HOME:-none}\"'").await;
        assert_eq!(out.trim(), "1-none", "stderr: {err}");

        let (out, _, err) = run(&kernel, "export GONE=1; env -u GONE /bin/sh -c 'echo \"[${GONE}]\"'").await;
        assert_eq!(out.trim(), "[]", "stderr: {err}");
    }

    #[tokio::test]
    async fn env_unset_takes_a_flag_shaped_word_as_its_name() {
        // GNU: `env -u -x cmd` unsets a variable named `-x`, then runs cmd.
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "env -u -x sh -c 'echo ok'").await;
        assert_eq!((out.trim(), code), ("ok", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn env_unset_takes_a_double_dash_as_its_name() {
        // GNU: `env -u -- cmd` unsets a variable named `--`, then runs cmd.
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "env -u -- sh -c 'echo ok'").await;
        assert_eq!((out.trim(), code), ("ok", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn env_double_dash_ends_options() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "env -- A=1 sh -c 'echo \"$A\"'").await;
        assert_eq!((out.trim(), code), ("1", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn env_double_dash_after_the_assignments_is_skipped() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "env A=1 -- sh -c 'echo \"$A\"'").await;
        assert_eq!((out.trim(), code), ("1", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn env_option_after_an_assignment_is_the_command() {
        // GNU: `env FOO=1 -i echo hi` runs a command named `-i`.
        let kernel = repl_kernel();
        let (out, code, _) = run(&kernel, "env FOO=1 -i echo hi").await;
        assert_ne!(code, 0);
        assert!(out.is_empty(), "nothing should run: {out:?}");
    }

    #[tokio::test]
    async fn spawn_passes_a_flag_to_the_command() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "spawn echo -n hi").await;
        assert_eq!((out.as_str(), code), ("hi", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn spawn_options_before_the_command_still_work() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "spawn --timeout 5000 sh -c 'echo \"$0\"' -i").await;
        assert_eq!((out.trim(), code), ("-i", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn spawn_passes_a_numeral_as_written() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "spawn sh -c 'echo \"$0\"' -0").await;
        assert_eq!((out.trim(), code), ("-0", 0), "stderr: {err}");
    }

    #[tokio::test]
    async fn spawn_command_flag_then_double_dash_passes_dash_words() {
        let kernel = repl_kernel();
        let (out, code, err) = run(&kernel, "spawn --command sh -- -c 'echo \"$0\"' -i").await;
        assert_eq!((out.trim(), code), ("-i", 0), "stderr: {err}");
    }

    /// `exec` replaces the process, so run it on the binder only: the words
    /// after the command must land in `positional`, never in `flags`.
    #[tokio::test]
    async fn binders_agree_for_every_wrapper() {
        use kaish_kernel::ast::{Program, Stmt};
        use kaish_kernel::tools::ExecContext;
        use kaish_kernel::vfs::{MemoryFs, VfsRouter};
        use std::sync::Arc;

        let kernel = repl_kernel();
        let schemas = kernel.tool_schemas();
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        let ctx = ExecContext::new(Arc::new(vfs));

        for script in [
            "timeout 5 sh -c body",
            "env A=1 sh -c body",
            "exec sh -c body",
            "spawn sh -c body",
        ] {
            let program: Program = kaish_kernel::parser::parse(script).expect("parse");
            let Some(Stmt::Command(cmd)) = program.statements.first() else {
                panic!("{script}: not a command");
            };
            let schema = schemas.iter().find(|s| s.name == cmd.name).expect("schema");

            let runtime = kaish_kernel::scheduler::build_tool_args(&cmd.args, &ctx, Some(schema))
                .await
                .expect("runtime bind");
            let validation = kaish_kernel::validator::build_tool_args_for_validation(&cmd.args, Some(schema));

            for (which, bound) in [("runtime", &runtime), ("validation", &validation)] {
                let tail: Vec<String> = bound
                    .positional
                    .iter()
                    .rev()
                    .take(3)
                    .rev()
                    .map(kaish_kernel::interpreter::value_to_string)
                    .collect();
                assert_eq!(tail, ["sh", "-c", "body"], "{script}: {which} positional {:?}", bound.positional);
                assert!(!bound.flags.contains("c"), "{script}: {which} flags {:?}", bound.flags);
            }
        }
    }
}
