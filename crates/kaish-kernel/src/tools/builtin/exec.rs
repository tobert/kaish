//! exec — Replace the current process (POSIX exec).
//!
//! Replaces the shell process with the specified command via `execvp`.
//! This never returns on success — the process image is replaced entirely.
//!
//! For subprocess spawning with output capture, env/cwd/timeout control,
//! use `spawn` instead.
//!
//! # Examples
//!
//! ```kaish
//! exec cargo build --release    # replace shell with cargo
//! exec /usr/bin/python3 app.py  # replace shell with python
//! exec bash                     # drop into bash, replacing kaish
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::builtin::get_path_string;
use crate::tools::{exec_context,
    external_commands_unavailable_error, schema_from_clap, ExternalCommandsUnavailable,
    GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema,
};

use super::spawn::resolve_in_path;

/// Exec tool: replaces the current process (POSIX `exec`).
pub struct Exec;

/// clap-derived argv layer for exec.
///
/// `trailing_var_arg` + `allow_hyphen_values` because exec is a passthrough —
/// everything after the command name is the child's argv, including flags.
#[derive(Parser, Debug)]
#[command(name = "exec", about = "Replace the current process with a command (POSIX exec)")]
struct ExecArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// Command to exec into, followed by its arguments.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    command_argv: Vec<String>,
}

#[async_trait]
impl Tool for Exec {
    fn name(&self) -> &str {
        "exec"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &ExecArgs::command(),
            "exec",
            "Replace the current process with a command (POSIX exec)",
            [
                ("Replace shell with bash", "exec bash"),
                ("Replace shell with a command", "exec cargo build --release"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("exec: {e}")),
        };
        let parsed = match ExecArgs::try_parse_from(
            std::iter::once("exec".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("exec: {e}")),
        };
        parsed.global.apply(ctx);

        if !ctx.allow_unwrapped_commands {
            // `exec` is only registered when the `subprocess` capability is
            // compiled in (tools/builtin/mod.rs), so reaching here always
            // means the runtime config turned it off, never that the
            // capability is missing — `ConfiguredOff` is the only reachable
            // reason. Exit 127, same as PATH lookup and `env CMD` — one
            // refusal shape for all four gated sites.
            return external_commands_unavailable_error("exec", ExternalCommandsUnavailable::ConfiguredOff);
        }

        // First positional is the command, rest are argv. A binary command
        // name goes loud rather than `get_string`'s silent `None` (which would
        // misreport it as "exec: missing command"); exec's argv loop below is
        // already guarded, but the command word itself slipped through.
        let command_name = match get_path_string(&args, "command", 0) {
            Ok(Some(cmd)) => cmd,
            Ok(None) => return ExecResult::failure(1, "exec: missing command"),
            Err(e) => return ExecResult::failure(1, format!("exec: {e}")),
        };

        // Resolve command path. The kernel never reads the OS env — a
        // frontend that wants host PATH seeds it via `initial_vars`. No PATH
        // in scope means nothing resolves, same as the external-command path.
        let command = if command_name.starts_with('/') || command_name.starts_with("./") {
            command_name.clone()
        } else {
            let path_var = ctx.scope.get("PATH").map(value_to_string).unwrap_or_default();

            match resolve_in_path(&command_name, &path_var) {
                Some(resolved) => resolved,
                None => {
                    return ExecResult::failure(
                        127,
                        format!("exec: {}: command not found", command_name),
                    )
                }
            }
        };

        // Remaining positionals become argv. Decision D: a bare collection
        // can't cross the process boundary as an argv element — refuse rather
        // than the previous silent JSON stringify (via `value_to_string`).
        // exec consumes typed Values directly (no `build_args_flat`), so the
        // guard lives here at exec's own edge. Binary gets the same treatment
        // as `build_args_flat`'s argv (text sink, loud rather than the
        // `[binary: N bytes]` placeholder) — exec's argv crosses the same
        // process boundary as any other external command's.
        let mut argv: Vec<String> = Vec::with_capacity(args.positional.len().saturating_sub(1));
        for (i, v) in args.positional.iter().enumerate().skip(1) {
            if let Some(msg) = crate::interpreter::structured_boundary_error("a command argument", v) {
                return ExecResult::failure(1, format!("exec: {msg}"));
            }
            // `exec /bin/echo -0` must reach the process as the same argv
            // `/bin/echo -0` does, so the source text wins here too.
            if let Some(raw) = args.positional_raw.get(&i) {
                argv.push(raw.clone());
                continue;
            }
            match crate::interpreter::value_to_text_sink(v) {
                Ok(s) => argv.push(s),
                Err(e) => return ExecResult::failure(1, format!("exec: {e}")),
            }
        }

        // Platform-specific: Unix replaces the process, others error
        #[cfg(unix)]
        {
            use std::os::unix::process::CommandExt;
            let mut cmd = std::process::Command::new(&command);
            cmd.args(&argv);

            // `Command::exec()` resets SIGPIPE to its default disposition
            // right before the exec syscall — correct for the child a real
            // `fork`+`exec` would produce, but `exec()` doesn't fork: this
            // reset runs directly on the CURRENT process, and only returns
            // (leaving the reset in place) when exec fails. Rust's runtime
            // sets SIGPIPE to `SIG_IGN` at startup precisely so a write to
            // a closed pipe returns `EPIPE` instead of killing the process;
            // a failed `exec /nonexistent` would otherwise leave this
            // process — the REPL, or an embedder's host process calling
            // through the kernel — with SIGPIPE defaulted for the rest of
            // its life. The next write to a closed pipe then kills it
            // instead of erroring.
            //
            // SAFETY: `SigDfl` is a well-defined, safe signal disposition
            // (no custom handler code runs). `nix::sigaction` always both
            // sets AND returns the PRIOR disposition — there is no
            // "query-only" variant in its safe API — so the value set here
            // is a deliberate throwaway: `exec()` is about to force SIGPIPE
            // to default anyway on its way to the exec syscall, so setting
            // it to `SigDfl` ourselves first changes nothing observable,
            // and `old_sigpipe` is what we actually came here for.
            #[allow(unsafe_code)]
            let old_sigpipe = unsafe {
                nix::sys::signal::sigaction(
                    nix::sys::signal::Signal::SIGPIPE,
                    &nix::sys::signal::SigAction::new(
                        nix::sys::signal::SigHandler::SigDfl,
                        nix::sys::signal::SaFlags::empty(),
                        nix::sys::signal::SigSet::empty(),
                    ),
                )
            };

            // exec() replaces the process — on success it never returns
            let err = cmd.exec();

            // If we get here, exec failed: restore whatever SIGPIPE
            // disposition this process actually had before, rather than
            // leaving it defaulted.
            //
            // SAFETY: restoring a disposition this same process already
            // held is always safe — no new handler code, no fd or
            // allocator assumptions (we are back in normal process
            // context, not inside a fork/pre_exec window).
            #[allow(unsafe_code)]
            if let Ok(old) = old_sigpipe {
                let _ = unsafe { nix::sys::signal::sigaction(nix::sys::signal::Signal::SIGPIPE, &old) };
            }

            ExecResult::failure(126, format!("exec: {}: {}", command, err))
        }

        #[cfg(not(unix))]
        {
            let _ = (command, argv);
            ExecResult::failure(1, "exec: process replacement not supported on this platform")
        }
    }
}

/// Convert a Value to a string.
fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => String::new(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
        Value::Json(json) => json.to_string(),
        Value::Bytes(b) => format!("[binary: {} bytes]", b.len()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tools::ExecContext;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_exec_missing_command() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing command"));
    }

    #[tokio::test]
    async fn test_exec_command_not_found() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("definitely_not_a_real_command_xyz".into()));

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 127);
        assert!(result.err.contains("command not found"));
    }

    #[tokio::test]
    async fn test_exec_absolute_path_not_found() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/nonexistent/binary".into()));

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok());
        // exec of nonexistent absolute path fails with 126 (exec error)
        assert_eq!(result.code, 126);
    }

    /// kaibo round-5 finding: `Command::exec()` resets SIGPIPE to its
    /// default disposition right before the exec syscall, and — because
    /// `exec()` doesn't fork — that reset runs on the CURRENT process, not
    /// a disposable child. On success the process image is replaced anyway
    /// (moot), but on FAILURE (this test's `/nonexistent/...` path) the
    /// reset was never undone: this test process (and, in production, a
    /// REPL or an embedder's host process) was left with SIGPIPE defaulted
    /// for the rest of its life, so the next write to a closed pipe kills
    /// it instead of returning EPIPE — exactly the crash that took down the
    /// whole `cargo test --lib` run once `exec`'s own tests (which exercise
    /// this failure path) ran before any test writing to a broken pipe.
    ///
    /// The query is a round-trip: setting SIGPIPE to `SigIgn` and reading
    /// back the PRIOR disposition nix's `sigaction` returns, since nix's
    /// safe API has no "query only" form. This is non-destructive for the
    /// property under test — Rust's own runtime sets SIGPIPE to `SigIgn` at
    /// startup, so restoring it to `SigIgn` here is a no-op if the fix
    /// worked, and leaves the test process in the same healthy state either
    /// way.
    #[cfg(unix)]
    #[tokio::test]
    async fn test_exec_failure_does_not_leave_sigpipe_defaulted() {
        use nix::sys::signal::{sigaction, SaFlags, SigAction, SigHandler, SigSet, Signal};

        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("/nonexistent/kaish-exec-sigpipe-probe".into()));

        let result = Exec.execute(args, &mut ctx).await;
        assert!(!result.ok(), "exec of a nonexistent command must fail: {result:?}");

        // SAFETY: SigIgn is a well-defined, safe signal disposition (no
        // custom handler code runs); this is a read via round-trip, not a
        // handler installation.
        #[allow(unsafe_code)]
        let prior = unsafe {
            sigaction(
                Signal::SIGPIPE,
                &SigAction::new(SigHandler::SigIgn, SaFlags::empty(), SigSet::empty()),
            )
        }
        .expect("sigaction query");

        assert_eq!(
            prior.handler(),
            SigHandler::SigIgn,
            "a failed exec() must not leave SIGPIPE defaulted on this process: {:?}",
            prior.handler()
        );
    }
}
