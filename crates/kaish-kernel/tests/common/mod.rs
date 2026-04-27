//! Shared helpers for integration tests.
//!
//! Lives at `tests/common/mod.rs` (rather than `tests/common.rs`) because
//! Cargo treats every top-level `tests/*.rs` file as its own test binary.
//! The `common/mod.rs` form is the documented escape hatch for shared
//! test code, and is the only place in the workspace where we use a
//! `mod.rs` (the project otherwise prefers `module_name.rs`).

use std::process::Command;

/// What `bash -c` produced when we ran a script under it.
pub struct BashOutput {
    pub stdout: String,
    pub code: i64,
}

/// Run a script via `bash -c` if KAISH_BASH_COMPAT is set; otherwise return
/// `None` and let the caller short-circuit. Panics if the user opted in but
/// `bash` is missing or errored — they explicitly asked us to compare.
pub fn run_bash_if_enabled(script: &str) -> Option<BashOutput> {
    if std::env::var_os("KAISH_BASH_COMPAT").is_none() {
        return None;
    }
    let output = Command::new("bash")
        .arg("-c")
        .arg(script)
        .output()
        .expect("KAISH_BASH_COMPAT set but failed to run bash; install bash or unset the var");
    Some(BashOutput {
        stdout: String::from_utf8_lossy(&output.stdout).into_owned(),
        // signaled processes (no exit code) collapse to -1; bash itself exits
        // 128+sig in those cases, so this branch is rare for synthetic tests.
        code: output.status.code().map(i64::from).unwrap_or(-1),
    })
}

/// TT-muncher invoked by `shell_compat!` to expand the assertion clauses on
/// the kaish side. Recognized clauses:
///
/// - `eq: "..."`        — stdout (after `.trim()`) equals the literal
/// - `kaish_eq: "..."`  — same, kaish-only (paired with `bash_eq:` for known
///                         divergences)
/// - `bash_eq: "..."`   — ignored on the kaish side
/// - `contains: "..."`  — stdout contains the substring
/// - `absent: "..."`    — stdout does not contain the substring
/// - `exit: N`          — exit code equals N
#[macro_export]
macro_rules! shell_compat_kaish_assert {
    ($out:expr, $code:expr,) => {};
    ($out:expr, $code:expr, eq: $e:expr $(, $($r:tt)*)?) => {
        assert_eq!(
            $out.trim(),
            $e,
            "kaish stdout mismatch:\n--- got ---\n{}\n-----------",
            $out,
        );
        $( $crate::shell_compat_kaish_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, kaish_eq: $e:expr $(, $($r:tt)*)?) => {
        assert_eq!(
            $out.trim(),
            $e,
            "kaish stdout mismatch:\n--- got ---\n{}\n-----------",
            $out,
        );
        $( $crate::shell_compat_kaish_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, bash_eq: $e:expr $(, $($r:tt)*)?) => {
        $( $crate::shell_compat_kaish_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, contains: $n:expr $(, $($r:tt)*)?) => {
        assert!(
            $out.contains($n),
            "kaish missing substring {:?}:\n--- got ---\n{}\n-----------",
            $n,
            $out,
        );
        $( $crate::shell_compat_kaish_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, absent: $m:expr $(, $($r:tt)*)?) => {
        assert!(
            !$out.contains($m),
            "kaish unexpected substring {:?}:\n--- got ---\n{}\n-----------",
            $m,
            $out,
        );
        $( $crate::shell_compat_kaish_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, exit: $c:expr $(, $($r:tt)*)?) => {
        assert_eq!($code, $c as i64, "kaish exit code mismatch");
        $( $crate::shell_compat_kaish_assert!($out, $code, $($r)*); )?
    };
}

/// Bash-side mirror of `shell_compat_kaish_assert!`. Ignores `kaish_eq:`,
/// honors `bash_eq:`, and otherwise applies the same shared clauses.
#[macro_export]
macro_rules! shell_compat_bash_assert {
    ($out:expr, $code:expr,) => {};
    ($out:expr, $code:expr, eq: $e:expr $(, $($r:tt)*)?) => {
        assert_eq!(
            $out.trim(),
            $e,
            "bash stdout mismatch:\n--- got ---\n{}\n-----------",
            $out,
        );
        $( $crate::shell_compat_bash_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, kaish_eq: $e:expr $(, $($r:tt)*)?) => {
        $( $crate::shell_compat_bash_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, bash_eq: $e:expr $(, $($r:tt)*)?) => {
        assert_eq!(
            $out.trim(),
            $e,
            "bash stdout mismatch:\n--- got ---\n{}\n-----------",
            $out,
        );
        $( $crate::shell_compat_bash_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, contains: $n:expr $(, $($r:tt)*)?) => {
        assert!(
            $out.contains($n),
            "bash missing substring {:?}:\n--- got ---\n{}\n-----------",
            $n,
            $out,
        );
        $( $crate::shell_compat_bash_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, absent: $m:expr $(, $($r:tt)*)?) => {
        assert!(
            !$out.contains($m),
            "bash unexpected substring {:?}:\n--- got ---\n{}\n-----------",
            $m,
            $out,
        );
        $( $crate::shell_compat_bash_assert!($out, $code, $($r)*); )?
    };
    ($out:expr, $code:expr, exit: $c:expr $(, $($r:tt)*)?) => {
        assert_eq!($code, $c as i64, "bash exit code mismatch");
        $( $crate::shell_compat_bash_assert!($out, $code, $($r)*); )?
    };
}

/// Generate a pair of tests for a single shell scenario.
///
/// Layout:
/// ```ignore
/// shell_compat! {
///     name: my_scenario,
///     script: "echo hi",
///     eq: "hi",
/// }
/// ```
///
/// Generates `my_scenario::kaish` (always runs) and `my_scenario::bash`
/// (gated on `KAISH_BASH_COMPAT=1`). Both apply the same clauses to the
/// stdout/exit-code their side produced.
#[macro_export]
macro_rules! shell_compat {
    (
        name: $name:ident,
        script: $script:expr,
        $($body:tt)*
    ) => {
        mod $name {
            use ::kaish_kernel::Kernel;

            #[tokio::test]
            #[allow(unused_variables)]
            async fn kaish() {
                let kernel = Kernel::transient().expect("transient kernel");
                let result = kernel.execute($script).await.expect("kaish execute");
                let out: String = result.text_out().into_owned();
                let code: i64 = result.code;
                $crate::shell_compat_kaish_assert!(out, code, $($body)*);
            }

            #[test]
            #[allow(unused_variables)]
            fn bash() {
                let Some(b) = $crate::common::run_bash_if_enabled($script) else {
                    return;
                };
                $crate::shell_compat_bash_assert!(b.stdout, b.code, $($body)*);
            }
        }
    };
}
