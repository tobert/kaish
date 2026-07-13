//! Tests for external command execution via PATH lookup.
//!
//! These tests verify that kaish correctly falls back to PATH resolution
//! when no builtin tool matches a command name. They spawn real processes, so
//! the whole suite requires the `subprocess` capability.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]
#![cfg(feature = "subprocess")]

use std::collections::HashMap;
use std::time::Duration;

use kaish_kernel::ast::Value;
use kaish_kernel::{Kernel, KernelConfig};

/// Helper to create a kernel with passthrough filesystem and PATH access.
///
/// The kernel execution core is hermetic — it never reads OS env — so PATH must
/// be supplied via `initial_vars`, exactly as the real REPL frontend does with
/// `os_env_vars()`. (Reading OS env *here*, in test fixture code, is fine.)
fn repl_kernel() -> Kernel {
    let mut vars = HashMap::new();
    vars.insert(
        "PATH".to_string(),
        Value::String(std::env::var("PATH").unwrap_or_default()),
    );
    let config = KernelConfig::repl().with_initial_vars(vars);
    Kernel::new(config).expect("Failed to create kernel")
}

// ============================================================================
// Basic External Command Tests
// ============================================================================

#[tokio::test]
async fn external_command_basic() {
    let kernel = repl_kernel();
    // /bin/true always exists and returns 0
    let result = kernel.execute("true").await.unwrap();
    assert!(result.ok(), "true should succeed: {:?}", result);
}

#[tokio::test]
async fn external_command_with_args() {
    let kernel = repl_kernel();
    // Test that args are passed correctly
    let result = kernel.execute("echo hello world").await.unwrap();
    // Note: we have a builtin echo, so this tests builtin echo
    // Let's use a command that's definitely external
    assert!(result.ok());
}

#[tokio::test]
async fn large_buffered_stdin_does_not_deadlock() {
    // A buffered String stdin used to be write_all'd INLINE, before the
    // stdout/stderr drain tasks spawned. With >64 KiB of input AND a child that
    // emits >64 KiB before draining its input, both pipes fill and neither side
    // can progress → deadlock. `cat` echoes stdin to stdout, so a 256 KiB
    // payload through external `cat` (via bash, since `cat` is a kaish builtin)
    // fills both 64 KiB pipe buffers. The fix writes stdin from a detached task
    // that runs concurrently with the output drain.
    let kernel = repl_kernel();
    let payload = "x".repeat(8 * 1024 * 1024);
    let fut = kernel.execute_with_options(
        "bash -c \"cat\"",
        kaish_kernel::ExecuteOptions::new().with_stdin(payload.clone()),
    );
    let result = tokio::time::timeout(Duration::from_secs(10), fut)
        .await
        .expect("large buffered stdin must not deadlock")
        .expect("execute");
    assert_eq!(result.code, 0, "cat should succeed: {}", result.err);
    assert_eq!(
        result.text_out().len(),
        payload.len(),
        "all stdin bytes should round-trip through cat"
    );
}

#[tokio::test]
async fn external_resolution_is_hermetic_no_os_path_fallback() {
    // A kernel with no PATH in scope must NOT reach into the OS PATH to resolve
    // external commands — the execution core never reads OS env. `printenv` is a
    // real external (not a kaish builtin, unlike `true`) present on every Linux
    // PATH, so resolving it here would prove a hermeticity leak.
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel"); // initial_vars empty → no PATH
    let result = kernel.execute("printenv").await.unwrap();
    assert_eq!(
        result.code, 127,
        "with no PATH in scope, external resolution must report command-not-found, \
         not fall back to the OS PATH: {result:?}"
    );
}

#[tokio::test]
async fn exporting_a_structured_value_to_a_subprocess_is_a_loud_error() {
    // A list/record can't cross the process boundary; the external spawn refuses
    // rather than silently JSON-serializing it into the child's environment.
    // `printenv` is a real external (see the hermetic test above), so this
    // exercises the production spawn-site guard in try_execute_external.
    let kernel = repl_kernel();
    let result = kernel
        .execute(r#"export CFG=$(fromjson '{"port":8080}'); printenv CFG"#)
        .await;
    // The guard surfaces as an Err from execute (or a failed ExecResult); either
    // way it must be loud and hint at serializing with `tojson` first.
    let msg = match result {
        Ok(r) => {
            assert_ne!(r.code, 0, "exporting a record to a subprocess must fail: {r:?}");
            r.err
        }
        Err(e) => format!("{e:#}"),
    };
    assert!(msg.contains("tojson"), "should hint at serializing with tojson: {msg}");
    assert!(msg.contains("CFG"), "should name the offending variable: {msg}");
}

#[tokio::test]
async fn bare_collection_in_external_argv_is_a_loud_error() {
    // A live (un-interpolated) collection reaching an external command's argv
    // is a loud error, never a silent JSON-serialize — the argv-side twin of
    // the OS-env-export guard above. `printenv` is a real external (no kaish
    // builtin by that name), so this exercises the production spawn-site
    // guard in `build_args_flat`.
    let kernel = repl_kernel();
    let result = kernel
        .execute(r#"xs=$(fromjson '[1,2]'); printenv $xs"#)
        .await;
    let msg = match result {
        Ok(r) => {
            assert_ne!(r.code, 0, "a bare collection argv element must fail: {r:?}");
            r.err
        }
        Err(e) => format!("{e:#}"),
    };
    assert!(msg.contains("tojson"), "should hint at serializing with tojson: {msg}");
}

// Linux-gated + absolute path so the external spawn is unconditionally taken
// (bypasses PATH lookup entirely), mirroring
// `external_argv_does_not_split_space_containing_var` above — the exact
// argv content is what's under test, so we pin it via `printf`.
#[cfg(target_os = "linux")]
#[tokio::test]
async fn interpolated_collection_is_a_string_arg() {
    // `"$xs"` interpolates to compact JSON text BEFORE reaching argv (an
    // ordinary `Value::String`), so it is not a boundary violation — only a
    // bare, un-interpolated collection value trips the guard above.
    let kernel = repl_kernel();
    let result = kernel
        .execute(r#"xs=$(fromjson '[1,2]'); /usr/bin/printf "[%s]" "$xs""#)
        .await
        .unwrap();
    assert!(result.ok(), "interpolated collection arg must not error: {:?}", result);
    assert_eq!(result.text_out(), "[[1,2]]");
}

// ── Decision D completeness: the three subprocess builtins with their own
// argv/env stringify paths (spawn/exec/env) bypass `build_args_flat`, so each
// carries the same boundary guard at its own edge. Linux-gated + absolute
// command paths (`/bin/echo`, `/bin/true`) so the spawn is unconditional. ──

#[cfg(target_os = "linux")]
#[tokio::test]
async fn spawn_nested_collection_argv_element_is_a_loud_error() {
    // spawn's argv is legitimately a list of strings, so the top-level list
    // passes — but a *nested* collection element can't be a process argument.
    // Previously `extract_string_array` silently JSON-stringified it.
    let kernel = repl_kernel();
    let result = kernel
        .execute(r#"xs=$(fromjson '["ok",[1,2]]'); spawn --command /bin/echo --argv $xs"#)
        .await;
    let msg = match result {
        Ok(r) => {
            assert_ne!(r.code, 0, "a nested collection argv element must fail: {r:?}");
            r.err
        }
        Err(e) => format!("{e:#}"),
    };
    assert!(msg.contains("tojson"), "should hint at serializing with tojson: {msg}");
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn spawn_record_as_whole_argv_is_a_loud_error() {
    // A record used as the whole argv is not a list of strings — previously a
    // silent empty argv (data loss), now loud.
    let kernel = repl_kernel();
    let result = kernel
        .execute(r#"r=$(fromjson '{"a":1}'); spawn --command /bin/echo --argv $r"#)
        .await;
    let ok = match result {
        Ok(r) => r.code != 0,
        Err(_) => true,
    };
    assert!(ok, "a record as the whole argv must be a loud error, not a silent empty argv");
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn exec_bare_collection_argv_is_a_loud_error() {
    // exec consumes typed Values from `args.positional`, not `build_args_flat`,
    // so its own edge carries the guard. The guard fires BEFORE `execvp`, so
    // the test process is never actually replaced.
    let kernel = repl_kernel();
    let result = kernel
        .execute(r#"xs=$(fromjson '[1,2]'); exec /bin/echo $xs"#)
        .await;
    let msg = match result {
        Ok(r) => {
            assert_ne!(r.code, 0, "a bare collection exec argv element must fail: {r:?}");
            r.err
        }
        Err(e) => format!("{e:#}"),
    };
    assert!(msg.contains("tojson"), "should hint at serializing with tojson: {msg}");
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn env_bare_collection_argv_is_a_loud_error() {
    // env also builds argv from typed positionals with its own private
    // `value_to_string`; a bare collection argv element is now loud.
    let kernel = repl_kernel();
    let result = kernel
        .execute(r#"xs=$(fromjson '[1,2]'); env /bin/echo $xs"#)
        .await;
    let msg = match result {
        Ok(r) => {
            assert_ne!(r.code, 0, "a bare collection env argv element must fail: {r:?}");
            r.err
        }
        Err(e) => format!("{e:#}"),
    };
    assert!(msg.contains("tojson"), "should hint at serializing with tojson: {msg}");
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn env_command_exporting_a_collection_var_is_a_loud_error() {
    // `env <command>` spawns via its OWN path (`execute_with_env`), which
    // populates the child env directly — bypassing `try_execute_external`'s
    // export guard. It now runs the same `structured_export_error` check, so an
    // exported collection variable is refused rather than silently serialized.
    let kernel = repl_kernel();
    let result = kernel
        .execute(r#"export CFG=$(fromjson '{"port":8080}'); env /bin/true"#)
        .await;
    let msg = match result {
        Ok(r) => {
            assert_ne!(r.code, 0, "exporting a record through `env cmd` must fail: {r:?}");
            r.err
        }
        Err(e) => format!("{e:#}"),
    };
    assert!(msg.contains("tojson"), "should hint at serializing with tojson: {msg}");
    assert!(msg.contains("CFG"), "should name the offending variable: {msg}");
}

#[tokio::test]
async fn external_command_not_found() {
    let kernel = repl_kernel();
    let result = kernel
        .execute("definitely_not_a_real_command_12345")
        .await
        .unwrap();
    assert_eq!(result.code, 127, "Should return 127 for command not found");
    assert!(
        result.err.contains("command not found"),
        "Error should mention 'command not found': {}",
        result.err
    );
}

// ============================================================================
// Date Format String Tests (requires lexer +bare handling)
// ============================================================================

#[tokio::test]
async fn external_command_date_format() {
    let kernel = repl_kernel();
    // Test that +%s is passed correctly to date
    let result = kernel.execute("date +%s").await.unwrap();
    assert!(result.ok(), "date +%s should succeed: {:?}", result);
    // Output should be a unix timestamp (all digits)
    let text = result.text_out();
    let out = text.trim();
    assert!(
        out.chars().all(|c| c.is_ascii_digit()),
        "date +%s should output digits: '{}'",
        out
    );
}

#[tokio::test]
async fn external_command_date_complex_format() {
    let kernel = repl_kernel();
    // Test more complex format string
    let result = kernel.execute("date +%Y-%m-%d").await.unwrap();
    assert!(result.ok(), "date +%Y-%m-%d should succeed: {:?}", result);
    // Output should match YYYY-MM-DD pattern
    let text = result.text_out();
    let out = text.trim();
    assert_eq!(out.len(), 10, "Date should be 10 chars: '{}'", out);
    assert!(out.contains('-'), "Date should have dashes: '{}'", out);
}

// ============================================================================
// Flag Preservation Tests
// ============================================================================

#[tokio::test]
async fn external_command_short_flags() {
    let kernel = repl_kernel();
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().display();
    // ls -la should work (testing short flag preservation)
    let result = kernel.execute(&format!("ls -la {path}")).await.unwrap();
    // We have builtin ls, but this tests that flags are handled
    assert!(result.ok(), "ls -la should succeed: {:?}", result);
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn external_command_long_flags() {
    let kernel = repl_kernel();
    // Test long flags with external command via spawn (uname is now a builtin)
    let result = kernel.execute("spawn --command uname --argv '--kernel-name'").await.unwrap();
    assert!(result.ok(), "spawn uname --kernel-name should succeed: {:?}", result);
    assert!(
        result.text_out().contains("Linux"),
        "Should show Linux: {}",
        result.text_out()
    );
}

// ============================================================================
// Exit Code Tests
// ============================================================================

#[tokio::test]
async fn external_command_exit_code_success() {
    let kernel = repl_kernel();
    let result = kernel.execute("true").await.unwrap();
    assert_eq!(result.code, 0, "true should exit with 0");
}

#[tokio::test]
async fn external_command_exit_code_failure() {
    let kernel = repl_kernel();
    let result = kernel.execute("false").await.unwrap();
    assert_eq!(result.code, 1, "false should exit with 1");
}

#[tokio::test]
async fn external_command_exit_code_specific() {
    let kernel = repl_kernel();
    // sh -c "exit N" is a reliable way to test specific exit codes
    let result = kernel.execute("sh -c 'exit 42'").await.unwrap();
    assert_eq!(result.code, 42, "Should preserve exit code 42");
}

// ============================================================================
// Stdin/Stdout Piping Tests
// ============================================================================

#[tokio::test]
async fn external_command_stdin_piping() {
    let kernel = repl_kernel();
    // Test that stdin flows correctly to external commands
    // Using our builtin echo piped to external wc
    let result = kernel.execute("echo 'hello world' | wc -c").await.unwrap();
    assert!(result.ok(), "pipe should succeed: {:?}", result);
    // "hello world\n" is 12 chars
    let count: i64 = result.text_out().trim().parse().unwrap_or(-1);
    assert_eq!(count, 12, "wc -c should count 12 chars: {}", result.text_out());
}

// ============================================================================
// Working Directory Tests
// ============================================================================

#[tokio::test]
async fn external_command_respects_cwd() {
    let kernel = repl_kernel();
    let tmp = tempfile::tempdir().unwrap();
    let path = tmp.path().to_string_lossy().to_string();
    // cd to a known directory, then run pwd
    kernel.execute(&format!("cd {path}")).await.unwrap();
    let result = kernel.execute("pwd").await.unwrap();
    assert!(result.ok(), "pwd should succeed: {:?}", result);
    assert!(
        result.text_out().contains(&path),
        "Should be in {}: {}",
        path,
        result.text_out()
    );
}

// ============================================================================
// Mixed Builtin and External Tests
// ============================================================================

#[tokio::test]
async fn pipeline_builtin_to_external() {
    let kernel = repl_kernel();
    // builtin echo | external sort
    let result = kernel
        .execute("echo 'c\nb\na' | sort")
        .await
        .unwrap();
    assert!(result.ok(), "pipeline should succeed: {:?}", result);
    // Sort should alphabetize
    let text = result.text_out();
    let lines: Vec<&str> = text.trim().lines().collect();
    assert_eq!(lines, vec!["a", "b", "c"], "Should be sorted: {:?}", lines);
}

#[tokio::test]
async fn pipeline_builtin_to_builtin() {
    let kernel = repl_kernel();
    // seq | head (both builtins, tests pipeline)
    // Using lines=3 named arg since -n 3 requires schema-aware parsing
    let result = kernel.execute("seq 1 10 | head -n 3").await.unwrap();
    assert!(result.ok(), "pipeline should succeed: {:?}", result);
    let text = result.text_out();
    let lines: Vec<&str> = text.trim().lines().collect();
    assert_eq!(lines, vec!["1", "2", "3"], "Should have first 3: {:?}", lines);
}

// ============================================================================
// Environment Variable Tests
// ============================================================================

#[tokio::test]
async fn external_command_is_hermetic_by_default() {
    // The kernel does not inherit OS env — `KernelConfig::repl()` alone does
    // not seed PATH. Frontends (the REPL binary, the MCP server) populate
    // `initial_vars` from `std::env::vars()`; embedders that don't populate
    // get a hermetic kernel that can't even resolve an external command,
    // because resolution reads PATH from scope and never from OS env.
    let kernel = Kernel::new(KernelConfig::repl()).expect("kernel"); // no initial_vars → no PATH
    assert!(
        std::env::var_os("PATH").is_some(),
        "test precondition: PATH must be set for cargo test"
    );
    let result = kernel.execute("printenv PATH").await.unwrap();
    assert!(
        !result.ok(),
        "printenv PATH must fail in hermetic kernel: {:?}",
        result
    );
}

#[tokio::test]
async fn external_command_sees_initial_vars() {
    // When a frontend populates `initial_vars`, those names are exported and
    // reach subprocesses. This is the path REPL/MCP take to mirror the host
    // env to children.
    use kaish_kernel::ast::Value;
    use std::collections::HashMap;

    let mut vars = HashMap::new();
    vars.insert("PATH".to_string(), Value::String("/usr/bin:/bin".into()));
    vars.insert("MY_PROBE".to_string(), Value::String("seeded".into()));

    let kernel = Kernel::new(KernelConfig::repl().with_initial_vars(vars))
        .expect("Failed to create kernel");

    let result = kernel.execute("printenv MY_PROBE").await.unwrap();
    assert!(result.ok(), "printenv MY_PROBE should succeed: {:?}", result);
    assert_eq!(result.text_out().trim(), "seeded");
}

#[tokio::test]
async fn env_prefix_reaches_subprocess_then_does_not_leak() {
    // `NAME=value cmd` exports the assignment into the command's environment
    // (so the child sees it), but it must not persist: a later `printenv NAME`
    // finds nothing. Regression test for docs/issues.md #1.
    use kaish_kernel::ast::Value;
    use std::collections::HashMap;

    let mut vars = HashMap::new();
    vars.insert("PATH".to_string(), Value::String("/usr/bin:/bin".into()));
    let kernel = Kernel::new(KernelConfig::repl().with_initial_vars(vars))
        .expect("Failed to create kernel");

    let scoped = kernel
        .execute("MY_PROBE=fromprefix printenv MY_PROBE")
        .await
        .unwrap();
    assert!(scoped.ok(), "prefixed printenv should see MY_PROBE: {scoped:?}");
    assert_eq!(scoped.text_out().trim(), "fromprefix");

    // Not leaked: a fresh execute in the same kernel no longer has MY_PROBE.
    let after = kernel.execute("printenv MY_PROBE").await.unwrap();
    assert!(
        !after.ok(),
        "MY_PROBE must not persist past the prefixed command: {after:?}"
    );
}

// Linux-gated + absolute path so the external spawn is unconditionally taken.
// The Decision-D export guard fires at spawn time, so it needs a real binary —
// a nonexistent path errors on resolution before the guard is reached.
#[cfg(target_os = "linux")]
#[tokio::test]
async fn env_prefix_collection_to_external_is_a_loud_error() {
    // `X=[1 2] cmd` parses (env-prefix RHS is a value position), and the
    // scoped collection is fine for BUILTINS (a kaish var, no boundary). But
    // an external subprocess would need X serialized into its OS environment —
    // that's the Decision-D boundary, and it must refuse with the tojson hint
    // rather than silently JSON-stringifying into the child env.
    // (2026-07-03 coverage review, gemini #3.)
    let kernel = repl_kernel();
    let result = kernel.execute("X=[1 2] /bin/true").await;
    let msg = match result {
        Ok(r) => {
            assert_ne!(r.code, 0, "must not spawn with a collection env: {r:?}");
            r.err
        }
        Err(e) => format!("{e:#}"),
    };
    assert!(msg.contains("tojson"), "should hint at tojson: {msg}");
    assert!(msg.contains("list"), "should name the shape: {msg}");
}

// ============================================================================
// Interactive Stdin Inheritance Tests
// ============================================================================

/// Helper to create a kernel with interactive mode enabled.
fn interactive_kernel() -> Kernel {
    Kernel::new(KernelConfig::repl().with_interactive(true)).expect("Failed to create kernel")
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn non_interactive_stdin_is_dev_null() {
    let kernel = repl_kernel();
    // Use /bin/readlink to bypass the builtin — we need an external process
    // to introspect its own fd/0, since the builtin reads kaish's fd/0.
    // A bare `readlink` resolves to the builtin and only ever "passed" when
    // the test runner itself had stdin=/dev/null (CI gave it a pipe: PR #169).
    // Linux-specific: requires /proc/self/fd/0.
    let result = kernel
        .execute("/bin/readlink /proc/self/fd/0")
        .await
        .unwrap();
    assert!(result.ok(), "readlink should succeed: {:?}", result);
    assert_eq!(
        result.text_out().trim(),
        "/dev/null",
        "Non-interactive external command stdin should be /dev/null: {}",
        result.text_out()
    );
}

#[cfg(target_os = "linux")]
#[tokio::test]
#[ignore = "requires TTY stdin — fails when cargo test runs with stdin=/dev/null"]
async fn interactive_stdin_is_not_dev_null() {
    let kernel = interactive_kernel();
    // Standalone interactive commands inherit stdout (real-time streaming),
    // so we pipe through cat to capture output. Readlink is First in
    // the pipeline: stdout is captured for the pipe, but stdin still inherits
    // from the terminal (no piped input for the first command).
    // /bin/readlink, not the builtin — same reason as the test above.
    // Linux-specific: requires /proc/self/fd/0.
    let result = kernel
        .execute("/bin/readlink /proc/self/fd/0 | cat")
        .await
        .unwrap();
    assert!(result.ok(), "readlink should succeed: {:?}", result);
    assert_ne!(
        result.text_out().trim(),
        "/dev/null",
        "Interactive external command stdin should NOT be /dev/null: {}",
        result.text_out()
    );
}

#[tokio::test]
async fn interactive_piped_stdin_still_works() {
    let kernel = interactive_kernel();
    // grep exits 0 only if it finds a match, so this verifies data flows
    // through the pipe. In interactive mode the last command (grep) inherits
    // stdout to the terminal, so we assert on exit code rather than output.
    let result = kernel
        .execute("echo hello | grep hello")
        .await
        .unwrap();
    assert_eq!(
        result.code, 0,
        "grep should find 'hello' in piped input (exit 0): {:?}",
        result
    );
}

// ============================================================================
// Argv No-Split Guarantee
// ============================================================================

/// The no-word-splitting guarantee on the EXTERNAL argv path
/// (`build_args_flat` → `try_execute_external`): a `$VAR` holding spaces must
/// arrive as ONE argv element in the spawned process, even unquoted. printf
/// cycles its format over operands, so a split would render `[a][b][c]`
/// instead of one bracket group. The builtin path is covered elsewhere; this
/// pins the external spawn site, which the hermetic-env discipline requires
/// to stay in sync with its test-only twin.
#[cfg(target_os = "linux")]
#[tokio::test]
async fn external_argv_does_not_split_space_containing_var() {
    let kernel = repl_kernel();
    kernel.execute(r#"X="a b  c""#).await.unwrap();
    let result = kernel
        .execute(r#"/usr/bin/printf "[%s]" $X"#)
        .await
        .unwrap();
    assert!(result.ok(), "printf should succeed: {:?}", result);
    assert_eq!(
        result.text_out(),
        "[a b  c]",
        "external argv split a space-containing $VAR"
    );
}

// ============================================================================
// Minus Alone (stdin indicator) Tests
// ============================================================================

#[tokio::test]
async fn minus_alone_lexes_correctly() {
    let kernel = repl_kernel();
    // Test that "-" is recognized as a positional argument
    // Using echo to verify "-" passes through correctly
    let result = kernel.execute("echo - foo -").await.unwrap();
    assert!(result.ok(), "echo should succeed: {:?}", result);
    assert!(result.text_out().contains("- foo -"), "Should include dashes: {}", result.text_out());
}

/// Phase 2: a background *external* job records its child's process group, so
/// `kill -<sig> %N` can deliver an arbitrary signal (STOP/CONT), not just
/// terminate. If the PGID weren't recorded, `kill --signal STOP %1` would be
/// refused as an "in-process task" — so its exit 0 proves the killpg path.
#[tokio::test]
async fn kill_signals_external_background_job_process_group() {
    let kernel = repl_kernel();
    kernel
        .execute("/usr/bin/sleep 30 &")
        .await
        .expect("background");

    // Wait until the backgrounded external's process group is actually
    // registered before signalling. A fixed `sleep` here raced the child's
    // fork/exec + PGID registration under load (flaked the suite). `CONT` is a
    // harmless probe: it's refused as an "in-process task" (exit 1) until the
    // PGID lands, then delivered via killpg (exit 0) — so its success is the
    // readiness gate. Bounded so a job that never registers still fails loudly.
    let mut ready = false;
    for _ in 0..150 {
        if kernel
            .execute("kill --signal CONT %1")
            .await
            .expect("execute")
            .code
            == 0
        {
            ready = true;
            break;
        }
        tokio::time::sleep(Duration::from_millis(20)).await;
    }
    assert!(ready, "background external job never registered its PGID (~3s)");

    // The real verification: STOP/CONT/TERM all delivered via the process group.
    let result = kernel
        .execute("kill --signal STOP %1; kill --signal CONT %1; kill %1")
        .await
        .expect("execute");
    assert_eq!(
        result.code, 0,
        "STOP/CONT/TERM via process group should all succeed: {}",
        result.err
    );

    // The job is gone after the terminating kill.
    let again = kernel.execute("kill %1").await.expect("execute");
    assert!(again.err.contains("not found"), "job should be gone: {}", again.err);
}

// ── External-command binary I/O (binary-data Phase C) ───────────────────────

#[cfg(target_os = "linux")]
#[tokio::test]
async fn external_binary_output_is_captured_as_bytes() {
    // A standalone external command producing non-UTF-8 bytes is captured as a
    // Bytes result, not lossy-decoded. 0xFF 0xFE 0xFD is invalid UTF-8.
    let kernel = repl_kernel();
    let r = kernel
        .execute(r#"sh -c 'printf "\377\376\375"'"#)
        .await
        .unwrap();
    assert!(r.is_bytes(), "binary external output should be a Bytes result");
    assert_eq!(r.out_bytes(), Some(&[0xffu8, 0xfe, 0xfd][..]));
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn external_binary_output_redirects_raw() {
    // `cmd > file` writes the raw bytes (the capture is byte-clean, so the
    // redirect isn't fed a lossy string). Verify the round-trip size via dd.
    let kernel = repl_kernel();
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("b.bin");
    let p = path.to_string_lossy();
    let r = kernel
        .execute(&format!(r#"sh -c 'printf "\377\376\375\374"' > {p}; dd if={p} of=/dev/null"#))
        .await
        .unwrap();
    assert!(r.err.contains("4 bytes copied"), "raw redirect size: {}", r.err);
}

#[cfg(target_os = "linux")]
#[tokio::test]
async fn spawn_forwards_and_captures_binary() {
    // Binary into an external command's stdin (forwarded raw) and back out
    // (captured as bytes): xxd -r -p makes the 0xFF byte, cat echoes it.
    let kernel = repl_kernel();
    let r = kernel
        .execute("echo ff | xxd -r -p | spawn --command cat")
        .await
        .unwrap();
    assert!(r.is_bytes(), "binary round-trip through cat should be Bytes");
    assert_eq!(r.out_bytes(), Some(&[0xffu8][..]));
}
