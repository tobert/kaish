//! `--json` sweep across the builtin registry.
//!
//! `--json` is the headline MCP feature, but until 2026-06-11 only ~4 of the
//! ~89 builtins had kernel-routed shape assertions. This sweep runs one
//! representative invocation per registered builtin with `--json` through
//! `kernel.execute()` and pins the top-level JSON shape (array / object /
//! string / number / empty / error envelope).
//!
//! Drift guard: `sweep_covers_every_registered_builtin` fails when a builtin
//! is registered without a sweep entry — adding a builtin means adding a
//! case (or an explicit `Skip` with a reason) here.
//!
//! Each case gets a fresh tempdir fixture and kernel, so cases are
//! order-independent and mutating commands (rm, mv, unset) can't poison
//! neighbors. The first sweep run found and fixed `checksum --json`'s
//! scrambled columns (full text line in node.name shifted every column by
//! one and dropped the algorithm).

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
// KernelConfig::repl() mounts the real filesystem.
#![cfg(feature = "localfs")]

mod common;

use std::fs;

use common::kernel_at;
use kaish_kernel::tools::{register_builtins, ToolRegistry};
use kaish_kernel::Kernel;
use tempfile::TempDir;

/// Expected outcome of running a case's `cmd` (which carries `--json`).
#[derive(Debug, Clone, Copy)]
enum Expect {
    /// exit 0; stdout parses as a JSON array
    Array,
    /// exit 0; stdout parses as a JSON object
    Object,
    /// exit 0; stdout parses as a JSON string
    String,
    /// exit 0; stdout parses as a JSON number (structured `.data` preserved)
    Number,
    /// exit 0; stdout empty (`--json` leaves an empty success untouched)
    Empty,
    /// given nonzero exit; stdout empty (clean failure stays unwrapped)
    FailsClean(i64),
    /// given nonzero exit; stdout is the `{"error":…,"code":…}` envelope
    FailsEnvelope(i64),
}

struct Case {
    name: &'static str,
    /// Commands run first, each via its own `kernel.execute` (state setup).
    setup: &'static [&'static str],
    /// The swept command; must exercise `--json` (or the builtin's own
    /// structured format where the builtin owns its output, e.g. gather).
    cmd: &'static str,
    expect: Expect,
}

/// A builtin deliberately not swept, with the reason on record.
struct Skip {
    name: &'static str,
    #[allow(dead_code)] // documentation; read by humans, not asserts
    reason: &'static str,
}

const SKIPS: &[Skip] = &[
    Skip { name: "bg", reason: "requires a stopped job (PTY job control)" },
    Skip { name: "fg", reason: "requires a stopped job (PTY job control)" },
    Skip { name: "exec", reason: "replaces the calling process" },
    Skip { name: "kaish-trash", reason: "reads the user's real OS trash — non-hermetic" },
];

const CASES: &[Case] = &[
    Case { name: "alias", setup: &["alias g=grep"], cmd: "alias --json", expect: Expect::Array },
    Case { name: "assert", setup: &[], cmd: "assert 1 --json", expect: Expect::Empty },
    Case { name: "awk", setup: &[], cmd: r#"printf 'a b\nc d\n' | awk '{print $1}' --json"#, expect: Expect::String },
    Case { name: "base64", setup: &[], cmd: "echo hi | base64 --json", expect: Expect::String },
    Case { name: "basename", setup: &[], cmd: "basename /a/b.txt --json", expect: Expect::String },
    Case { name: "cat", setup: &[], cmd: "cat tmp/data.json --json", expect: Expect::String },
    // Pins the error-envelope contract: a failure carrying a diagnostic still
    // honors --json, emitting {"error","code"} rather than leaking plain text.
    Case { name: "cat", setup: &[], cmd: "cat tmp/nope.json --json", expect: Expect::FailsEnvelope(1) },
    Case { name: "cd", setup: &[], cmd: "cd src --json", expect: Expect::Empty },
    Case { name: "checksum", setup: &[], cmd: "checksum tmp/data.json --json", expect: Expect::Array },
    // Identical files → exit 0, no output (clean success untouched by --json).
    Case { name: "cmp", setup: &[], cmd: "cmp tmp/data.json tmp/data.json --json", expect: Expect::Empty },
    Case { name: "cp", setup: &[], cmd: "cp tmp/data.json tmp/copy.json --json", expect: Expect::Empty },
    // cut populates `.data` with a per-line array (the same structure that
    // drives `for v in $(cut …)`), so `--json` surfaces that array, not the
    // text-flattened scalar.
    Case { name: "cut", setup: &[], cmd: r#"printf 'a,b\n' | cut -d ',' -f 1 --json"#, expect: Expect::Array },
    Case { name: "date", setup: &[], cmd: "date --json", expect: Expect::Object },
    // dd with no of= emits a Bytes result; --json renders the base64 envelope
    // (an object). Reads a finite fixture — never a real /dev device, which
    // would hang here in passthrough mode.
    Case { name: "dd", setup: &[], cmd: "dd if=tmp/data.json bs=4 count=1 --json", expect: Expect::Object },
    // Identical files → exit 0 with empty *text*, but `--json` still emits a
    // consistent object (`{old_file, new_file, differ:false, hunks:[]}`) so a
    // consumer iterating file pairs always parses an object, never "".
    Case { name: "diff", setup: &[], cmd: "diff tmp/data.json tmp/data.json --json", expect: Expect::Object },
    Case { name: "dirname", setup: &[], cmd: "dirname /a/b.txt --json", expect: Expect::String },
    Case { name: "echo", setup: &[], cmd: "echo hi --json", expect: Expect::String },
    Case { name: "env", setup: &["export FOO=bar"], cmd: "env --json", expect: Expect::String },
    Case { name: "export", setup: &[], cmd: "export FOO=bar --json", expect: Expect::Empty },
    Case { name: "false", setup: &[], cmd: "false --json", expect: Expect::FailsClean(1) },
    Case { name: "file", setup: &[], cmd: "file tmp/data.json --json", expect: Expect::Array },
    Case { name: "find", setup: &[], cmd: "find src -name '*.rs' --json", expect: Expect::Array },
    Case { name: "fromjson", setup: &[], cmd: r#"fromjson '{"a":1}' --json"#, expect: Expect::Object },
    // scatter/gather own their output (`owns_output`): `--json` passes
    // through untouched and `--format json` is the structured form.
    Case { name: "gather", setup: &[], cmd: "seq 1 2 | scatter --as N | echo $N | gather --json", expect: Expect::Array },
    Case { name: "scatter", setup: &[], cmd: "seq 1 2 | scatter --as N | echo $N | gather --json", expect: Expect::Array },
    Case { name: "git", setup: &["git init ."], cmd: "git status --json", expect: Expect::String },
    Case { name: "glob", setup: &[], cmd: "glob 'tmp/*.json' --json", expect: Expect::Array },
    Case { name: "grep", setup: &[], cmd: "grep INFO tmp/app.log --json", expect: Expect::Array },
    Case { name: "head", setup: &[], cmd: "head -n 1 tmp/app.log --json", expect: Expect::Array },
    Case { name: "help", setup: &[], cmd: "help cat --json", expect: Expect::String },
    Case { name: "hostname", setup: &[], cmd: "hostname --json", expect: Expect::String },
    Case { name: "jobs", setup: &["sleep 0.2 &"], cmd: "jobs --json", expect: Expect::Array },
    // jq's `.data` already carries the structured value, so `--json` serializes
    // that (the number 1), not the rendered text re-wrapped as a string.
    // apply_output_format prefers `.data` over text when both are present.
    Case { name: "jq", setup: &[], cmd: r#"echo '{"a":1}' | jq '.a' --json"#, expect: Expect::Number },
    Case { name: "kaish-ast", setup: &[], cmd: "kaish-ast 'echo hi' --json", expect: Expect::String },
    Case { name: "kaish-clear", setup: &[], cmd: "kaish-clear --json", expect: Expect::String },
    Case { name: "kaish-ignore", setup: &[], cmd: "kaish-ignore --json", expect: Expect::Array },
    Case { name: "kaish-last", setup: &["echo hi"], cmd: "kaish-last --json", expect: Expect::String },
    // Breaking shape change in 0.8.1: {mounts, budget?} object, not a bare array.
    Case { name: "kaish-mounts", setup: &[], cmd: "kaish-mounts --json", expect: Expect::Object },
    Case { name: "kaish-output-limit", setup: &[], cmd: "kaish-output-limit --json", expect: Expect::Array },
    Case { name: "kaish-status", setup: &[], cmd: "kaish-status --json", expect: Expect::Array },
    Case { name: "kaish-tools", setup: &[], cmd: "kaish-tools --json", expect: Expect::Array },
    Case { name: "kaish-validate", setup: &[], cmd: "kaish-validate -e 'echo hi' --json", expect: Expect::String },
    Case { name: "kaish-vars", setup: &["X=1"], cmd: "kaish-vars --json", expect: Expect::Array },
    Case { name: "kaish-version", setup: &[], cmd: "kaish-version --json", expect: Expect::String },
    Case { name: "kaish-vfs", setup: &[], cmd: "kaish-vfs status --json", expect: Expect::Array },
    Case { name: "keys", setup: &["u=$(fromjson '{\"a\":1,\"b\":2}')"], cmd: "keys $u --json", expect: Expect::Array },
    Case { name: "kill", setup: &["sleep 5 &"], cmd: "kill %1 --json", expect: Expect::Empty },
    Case { name: "ln", setup: &[], cmd: "ln -s tmp/data.json link2.json --json", expect: Expect::Empty },
    Case { name: "ls", setup: &[], cmd: "ls src --json", expect: Expect::Array },
    Case { name: "mkdir", setup: &[], cmd: "mkdir newdir --json", expect: Expect::Empty },
    Case { name: "mktemp", setup: &[], cmd: "mktemp -p tmp --json", expect: Expect::String },
    Case { name: "mv", setup: &[], cmd: "mv tmp/data.json tmp/moved.json --json", expect: Expect::Empty },
    Case {
        name: "patch",
        setup: &["cat > fix.patch << 'EOF'\n--- a/tmp/app.log\n+++ b/tmp/app.log\n@@ -1,2 +1,2 @@\n-INFO one\n+INFO 1\n ERROR two\nEOF"],
        cmd: "patch tmp/app.log --dry-run --json < fix.patch",
        expect: Expect::String,
    },
    Case { name: "printf", setup: &[], cmd: "printf 'x' --json", expect: Expect::String },
    Case { name: "ps", setup: &[], cmd: "ps --json", expect: Expect::Array },
    // push mutates in place and is silent on success, like unset.
    Case { name: "push", setup: &["xs=[a b]"], cmd: "push xs c --json", expect: Expect::Empty },
    Case { name: "pwd", setup: &[], cmd: "pwd --json", expect: Expect::String },
    Case { name: "read", setup: &[], cmd: "echo hi | read X --json", expect: Expect::Empty },
    Case { name: "readlink", setup: &["ln -s tmp/data.json link.json"], cmd: "readlink link.json --json", expect: Expect::String },
    Case { name: "realpath", setup: &[], cmd: "realpath tmp/data.json --json", expect: Expect::String },
    Case { name: "rm", setup: &["touch del.txt"], cmd: "rm del.txt --json", expect: Expect::Empty },
    Case { name: "sed", setup: &[], cmd: r#"printf 'a\n' | sed 's/a/b/' --json"#, expect: Expect::String },
    Case { name: "seq", setup: &[], cmd: "seq 1 3 --json", expect: Expect::Array },
    Case { name: "set", setup: &[], cmd: "set --json", expect: Expect::Empty },
    Case { name: "sleep", setup: &[], cmd: "sleep 0 --json", expect: Expect::Empty },
    Case { name: "sort", setup: &[], cmd: r#"printf 'b\na\n' | sort --json"#, expect: Expect::String },
    Case { name: "spawn", setup: &[], cmd: "spawn --command /usr/bin/true --json", expect: Expect::Empty },
    Case { name: "split", setup: &[], cmd: "split 'a,b' ',' --json", expect: Expect::Array },
    Case { name: "stat", setup: &[], cmd: "stat tmp/data.json --json", expect: Expect::Array },
    Case { name: "tac", setup: &[], cmd: r#"printf 'a\nb\n' | tac --json"#, expect: Expect::String },
    Case { name: "tail", setup: &[], cmd: "tail -n 1 tmp/app.log --json", expect: Expect::Array },
    Case { name: "tee", setup: &[], cmd: "echo hi | tee out.txt --json", expect: Expect::String },
    Case { name: "timeout", setup: &[], cmd: "timeout 5 echo hi --json", expect: Expect::String },
    Case { name: "tojson", setup: &[], cmd: "tojson hello --json", expect: Expect::String },
    Case { name: "tokens", setup: &[], cmd: "echo hello | tokens --json", expect: Expect::Array },
    Case { name: "touch", setup: &[], cmd: "touch new.txt --json", expect: Expect::Empty },
    Case { name: "tr", setup: &[], cmd: "printf 'abc' | tr a x --json", expect: Expect::String },
    Case { name: "tree", setup: &[], cmd: "tree src --json", expect: Expect::Object },
    Case { name: "true", setup: &[], cmd: "true --json", expect: Expect::Empty },
    Case { name: "typeof", setup: &["x=$(fromjson '[1,2,3]')"], cmd: "typeof $x --json", expect: Expect::String },
    Case { name: "unalias", setup: &["alias g=grep"], cmd: "unalias g --json", expect: Expect::Empty },
    Case { name: "uname", setup: &[], cmd: "uname --json", expect: Expect::String },
    Case { name: "uniq", setup: &[], cmd: r#"printf 'a\na\nb\n' | uniq --json"#, expect: Expect::String },
    Case { name: "unset", setup: &["X=1"], cmd: "unset X --json", expect: Expect::Empty },
    Case { name: "values", setup: &["u=$(fromjson '{\"a\":1,\"b\":2}')"], cmd: "values $u --json", expect: Expect::Array },
    Case { name: "wait", setup: &[], cmd: "wait --json", expect: Expect::String },
    Case { name: "wc", setup: &[], cmd: "wc -l tmp/app.log --json", expect: Expect::Array },
    Case { name: "which", setup: &["export PATH=/usr/bin:/bin"], cmd: "which sh --json", expect: Expect::String },
    Case { name: "write", setup: &[], cmd: "write out2.txt hello --json", expect: Expect::String },
    Case { name: "xxd", setup: &[], cmd: "printf 'a' | xxd --json", expect: Expect::String },
];

fn touch(dir: &std::path::Path, name: &str, contents: &str) {
    let path = dir.join(name);
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).expect("create parent dirs");
    }
    fs::write(path, contents).expect("write file");
}

fn fixture() -> (TempDir, std::sync::Arc<Kernel>) {
    let dir = tempfile::tempdir().expect("tempdir");
    touch(dir.path(), "src/main.rs", "fn main() {}\n// TODO x\n");
    touch(dir.path(), "tmp/data.json", r#"{"a": 1}"#);
    touch(dir.path(), "tmp/app.log", "INFO one\nERROR two\n");
    // into_arc: redispatching builtins (timeout) need the kernel's self_weak
    // dispatcher, which only exists on an Arc'd kernel.
    let kernel = kernel_at(dir.path()).into_arc();
    (dir, kernel)
}

/// Run one case against a fresh kernel; Err describes the mismatch.
async fn check_case(case: &Case) -> Result<(), String> {
    let (_dir, kernel) = fixture();
    for setup in case.setup {
        let r = kernel
            .execute(setup)
            .await
            .map_err(|e| format!("{}: setup {setup:?} failed to execute: {e}", case.name))?;
        if !r.ok() {
            return Err(format!(
                "{}: setup {setup:?} exited {}: {}",
                case.name, r.code, r.err
            ));
        }
    }
    let result = kernel
        .execute(case.cmd)
        .await
        .map_err(|e| format!("{}: {:?} failed to execute: {e}", case.name, case.cmd))?;
    let out = result.text_out().to_string();

    let parse = || -> Result<serde_json::Value, String> {
        serde_json::from_str(&out)
            .map_err(|e| format!("{}: output is not JSON ({e}): {out:?}", case.name))
    };
    let expect_code = |want: i64| -> Result<(), String> {
        if result.code == want {
            Ok(())
        } else {
            Err(format!(
                "{}: expected exit {want}, got {} (err: {})",
                case.name, result.code, result.err
            ))
        }
    };

    match case.expect {
        Expect::Array => {
            expect_code(0)?;
            match parse()? {
                serde_json::Value::Array(_) => Ok(()),
                other => Err(format!("{}: expected JSON array, got: {other}", case.name)),
            }
        }
        Expect::Object => {
            expect_code(0)?;
            match parse()? {
                serde_json::Value::Object(_) => Ok(()),
                other => Err(format!("{}: expected JSON object, got: {other}", case.name)),
            }
        }
        Expect::String => {
            expect_code(0)?;
            match parse()? {
                serde_json::Value::String(_) => Ok(()),
                other => Err(format!("{}: expected JSON string, got: {other}", case.name)),
            }
        }
        Expect::Number => {
            expect_code(0)?;
            match parse()? {
                serde_json::Value::Number(_) => Ok(()),
                other => Err(format!("{}: expected JSON number, got: {other}", case.name)),
            }
        }
        Expect::Empty => {
            expect_code(0)?;
            if out.is_empty() {
                Ok(())
            } else {
                Err(format!("{}: expected empty stdout, got: {out:?}", case.name))
            }
        }
        Expect::FailsClean(code) => {
            expect_code(code)?;
            if out.is_empty() {
                Ok(())
            } else {
                Err(format!(
                    "{}: clean failure should leave stdout empty, got: {out:?}",
                    case.name
                ))
            }
        }
        Expect::FailsEnvelope(code) => {
            expect_code(code)?;
            match parse()? {
                serde_json::Value::Object(map)
                    if map.contains_key("error") && map.contains_key("code") =>
                {
                    Ok(())
                }
                other => Err(format!(
                    "{}: expected {{\"error\",\"code\"}} envelope, got: {other}",
                    case.name
                )),
            }
        }
    }
}

/// Drift guard: every builtin registered in this build has a sweep case or
/// an explicit skip. (Plan entries for feature-gated builtins absent from
/// this build are fine — they're exercised in fuller builds.)
#[tokio::test]
async fn sweep_covers_every_registered_builtin() {
    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);
    let planned: std::collections::BTreeSet<&str> = CASES
        .iter()
        .map(|c| c.name)
        .chain(SKIPS.iter().map(|s| s.name))
        .collect();
    let missing: Vec<String> = registry
        .schemas()
        .iter()
        .map(|s| s.name.clone())
        .filter(|name| !planned.contains(name.as_str()))
        .collect();
    assert!(
        missing.is_empty(),
        "registered builtins without a --json sweep case (add a Case or a Skip \
         with a reason in json_sweep_tests.rs): {missing:?}"
    );
}

/// Run every sweep case whose builtin is registered in this build.
#[tokio::test]
async fn json_output_shapes_hold_across_the_registry() {
    let mut registry = ToolRegistry::new();
    register_builtins(&mut registry);
    let registered: std::collections::BTreeSet<String> =
        registry.schemas().iter().map(|s| s.name.clone()).collect();

    let mut failures = Vec::new();
    let mut ran = 0usize;
    for case in CASES {
        if !registered.contains(case.name) {
            continue; // feature-gated out of this build
        }
        ran += 1;
        if let Err(msg) = check_case(case).await {
            failures.push(msg);
        }
    }
    assert!(ran > 0, "no sweep cases ran — registry empty?");
    assert!(
        failures.is_empty(),
        "{} of {ran} --json sweep cases failed:\n{}",
        failures.len(),
        failures.join("\n")
    );
}
