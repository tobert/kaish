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

#[rstest]
#[case::ls_short("ls -Z")]
#[case::ls_similar("ls --lon")]
#[case::cp_preserve("cp -p f g")]
#[tokio::test]
async fn unknown_flag_refusal_ends_its_line(#[case] script: &str) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f"), "x\n").unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(script).await.unwrap();
    assert_eq!(result.code, 2, "{script}: {}", result.err);
    assert!(result.err.ends_with('\n'), "{script}: no trailing newline: {:?}", result.err);
    assert!(!result.err.ends_with("\n\n"), "{script}: blank line: {:?}", result.err);
}

#[tokio::test]
async fn refused_cp_preserve_names_the_reason_and_the_fix() {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("a.txt"), "x\n").unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute("cp -p a.txt b.txt").await.unwrap();
    assert_eq!(result.code, 2, "{}", result.err);
    assert_eq!(
        result.err,
        "cp: -p is not supported: kaish cannot copy a file's mode, owner, or times; \
         no builtin sets them from another file. Run `cp SRC DST`; the copy gets the current time.\n"
    );
    assert!(!dir.path().join("b.txt").exists(), "a refused cp must not copy");
}

#[rstest]
#[case::cp_preserve_clustered("cp -rp f g", "cp: -p is not supported: ", "`cp SRC DST`")]
#[case::cp_preserve_long("cp --preserve=all f g", "cp: --preserve is not supported: ", "`cp SRC DST`")]
#[case::cp_archive("cp -a f g", "cp: -a is not supported: ", "`cp -rP SRC DST`")]
#[case::cp_force("cp -f f g", "cp: -f is not supported: ", "`cp SRC DST`")]
#[case::cp_interactive("cp -i f g", "cp: -i is not supported: ", "`cp -n SRC DST`")]
#[case::mv_force("mv -f f g", "mv: -f is not supported: ", "`mv SRC DST`")]
#[case::mv_interactive("mv -i f g", "mv: -i is not supported: ", "`mv -n SRC DST`")]
#[case::mkdir_mode("mkdir -m 755 d", "mkdir: -m is not supported: ", "`mkdir DIR`")]
#[case::echo_escapes("echo -e 'a\\tb'", "echo: -e is not supported: ", "`printf 'a\\tb\\n'`")]
#[case::printf_variable("printf -v x hi", "printf: -v is not supported: ", "`NAME=$(printf FORMAT ARGS)`")]
#[case::grep_perl("grep -P x f", "grep: -P is not supported: ", "`grep -E`")]
#[case::find_delete("find . -delete", "find: -delete is not supported: ", "do rm \"$f\"; done`")]
#[case::find_exec("find . -exec cat", "find: -exec is not supported: ", "$(find . -name")]
#[tokio::test]
async fn refused_flag_names_the_reason_and_the_fix(
    #[case] script: &str,
    #[case] prefix: &str,
    #[case] fix: &str,
) {
    let dir = tempdir().unwrap();
    fs::write(dir.path().join("f"), "x\n").unwrap();
    let kernel = kernel_at(dir.path());
    let result = kernel.execute(script).await.unwrap();
    assert_eq!(result.code, 2, "{script}: {}", result.err);
    assert!(result.err.starts_with(prefix), "{script}: {}", result.err);
    assert!(result.err.contains(fix), "{script} does not name {fix}: {}", result.err);
    assert!(!result.err.contains("see `help"), "{script}: the fix replaces the help pointer: {}", result.err);
    assert!(!result.err.contains("similar:"), "{script}: {}", result.err);
    assert_eq!(result.err.trim_end().lines().count(), 1, "{script}: {}", result.err);
}

/// Every refused flag a builtin declares is still refused, prints its hint,
/// and is listed in `help limits`. A flag that gains an implementation must
/// drop its hint, or this fails.
#[tokio::test]
async fn every_declared_refusal_is_live_and_documented() {
    use kaish_kernel::tools::{register_builtins, ToolRegistry};

    let limits = include_str!("../../kaish-help/content/en/limits.md");
    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);
    let mut declared = 0;
    for name in registry.names() {
        let tool = registry.get(name).unwrap();
        for refused in tool.refused_flags() {
            let hint = refused.hint;
            assert!(hint.contains("Run `"), "{name}: hint names no command: {hint}");
            assert!(hint.ends_with('.'), "{name}: hint is not a sentence: {hint}");
            assert!(!hint.contains('\n'), "{name}: hint spans lines: {hint}");
            let first = refused.spellings[0];
            assert!(
                limits.contains(&format!("`{name} {first}`")),
                "help limits does not list `{name} {first}`"
            );
            for spelling in refused.spellings {
                declared += 1;
                let dir = tempdir().unwrap();
                fs::write(dir.path().join("f"), "x\n").unwrap();
                let kernel = kernel_at(dir.path());
                let script = format!("{name} {spelling} f g");
                let result = kernel.execute(&script).await.unwrap();
                assert_eq!(result.code, 2, "{script}: {}", result.err);
                assert_eq!(
                    result.err,
                    format!("{name}: {spelling} is not supported: {hint}\n"),
                    "{script}"
                );
            }
        }
    }
    assert!(declared > 0, "no builtin declares a refused flag");
}
