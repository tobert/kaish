//! kill — Send signals to processes or jobs.
//!
//! Job control over kaish's own jobs (`kill %N`) is a kernel-level concern and
//! works in **every** build, hermetic ones included: a background job is a kaish
//! task with a cancellation token, not necessarily an OS process, so terminating
//! it needs no platform signal support. The `subprocess` capability only adds
//! real OS-signal *fidelity* — delivering an arbitrary signal (`STOP`/`CONT`/…)
//! to an external child's process group, and signalling a bare PID. When
//! external commands are disabled there are no such processes to signal anyway
//! (and when they're enabled, `/bin/kill` is on PATH for raw PIDs).
//!
//! ## Signal shorthand (GH #198)
//!
//! Besides `--signal NAME`/`-s NAME`, kill accepts the bash/POSIX shorthand as
//! a leading dash token: `kill -9 %1`, `kill -STOP %1`. Getting there needed
//! `raw_argv` (see [`ToolSchema::with_raw_argv`]) — the *default* argv binder
//! would otherwise mangle these tokens before `execute()` ever sees them:
//!
//! - `-9`/`-15`/… lexes as a plain `Int` positional (fine on its own), but
//!   `execute()` used to always read positional 0 as the target — so `-9`
//!   silently became "the target" (tried as a PID) while the real target
//!   (`%1`) was dropped with no error at all.
//! - `-STOP`/`-KILL`/… lexes as a multi-character `ShortFlag`. The default
//!   binder splits any *undeclared* multi-char short flag into one boolean
//!   flag per letter (`-S -T -O -P`), which then fails clap with a confusing
//!   `unexpected argument '-O'` — nothing about signals at all. Worse: since
//!   `-s` (`--signal`) IS a declared value-taking short flag, a
//!   case-insensitive lowercase spelling (`-stop`, `-sigkill`) would collide
//!   with the binder's glued-short-value rule (`-s<rest>` → `--signal <rest>`)
//!   and silently resolve to the wrong thing (`-stop` → `--signal top`) —
//!   not even loud, just wrong. `raw_argv` sidesteps the entire flag/positional
//!   split so kill can hand-roll its own tiny grammar over the untouched,
//!   source-ordered tokens (à la `set.rs`).
//!
//! `raw_argv` has two costs, both accepted deliberately: the kernel's generic
//! `--help`/`-h` interception (which reads `args.flags`, always empty under
//! raw_argv) no longer fires for `kill` — `help kill` is the fully equivalent
//! replacement, verified to produce identical output. `--json` needed a
//! companion fix in `GlobalFlags::apply_from_args` (kaish-tool-api) since it
//! has the same problem; fixed there once, for every raw_argv builtin.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::scheduler::{JobId, JobManager};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// Kill tool: send signals to processes or jobs.
pub struct Kill;

/// clap-derived argv layer for kill — schema/help generation only. Real
/// binding happens by hand over `args.positional` in `execute()` (`raw_argv`,
/// see the module docs above): `signal`/`discard` are never populated by a
/// real parse (raw_argv keeps `flags`/`named` empty, so clap only ever sees
/// the declared defaults), and `targets` is a validation-only sink — per
/// CLAUDE.md's clap-builtin convention, never read directly.
#[derive(Parser, Debug)]
#[command(name = "kill", about = "Send a signal to a process or job")]
struct KillArgs {
    /// Signal name or number: TERM, KILL, INT, HUP, STOP, CONT, QUIT, USR1,
    /// USR2 (--signal or -s; case-insensitive, optional SIG prefix). The
    /// bash/POSIX shorthand is also accepted as a leading token: `-9`/`-15`/
    /// `-<N>` (numeric) or `-KILL`/`-STOP`/... (name, e.g. `-SIGKILL`).
    #[arg(short = 's', long, default_value_t = String::from("TERM"))]
    signal: String,

    /// Abandon a gated (approval-pending) job. Without this flag, kill
    /// refuses to destroy a job's pending approval request. Conflicts with
    /// a signal (--signal/-s, or a shorthand): discarding a gate delivers
    /// nothing to anyone.
    #[arg(long, conflicts_with = "signal")]
    discard: bool,

    /// Return as soon as the termination is dispatched instead of waiting
    /// for the job to exit. By default kill waits, bounded by the kernel's
    /// kill grace plus 3 seconds per target, so exit 0 means the job is
    /// dead; with --no-wait exit 0 only means the signal went out, and the
    /// job stays visible in jobs until it unwinds.
    #[arg(long)]
    no_wait: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// One or more targets: job specifiers (`%1`) or PIDs. Every target is
    /// signalled; the exit code is 1 if any of them failed.
    targets: Vec<String>,
}

/// Signal names kaish recognizes, for both `--signal NAME` and the `-NAME`
/// shorthand — the fixed 80/20 set Amy named for job control in kaijutsu
/// (GH #198), not the full POSIX/BSD table. Deliberately excludes
/// ABRT/TSTP/WINCH/etc.: an unsupported name fails loudly below, naming
/// exactly what IS supported, rather than a silent guess or a per-platform
/// host lookup (the latter is exactly why this shorthand was demoted to P3 —
/// see the issue).
const KNOWN_SIGNAL_NAMES: &[&str] =
    &["TERM", "KILL", "INT", "HUP", "STOP", "CONT", "QUIT", "USR1", "USR2"];

/// Normalize a bare signal-name token (no leading `-`) to kaish's fixed
/// canonical spelling. Case-insensitive (bash accepts `-kill` as well as
/// `-KILL`) and tolerant of an optional `SIG` prefix (`SIGKILL`/`KILL` both
/// work, matching bash). `None` means the name isn't in
/// [`KNOWN_SIGNAL_NAMES`] — every caller must fail loud on `None`, never fall
/// back to a default signal.
fn normalize_signal_name(raw: &str) -> Option<&'static str> {
    let upper = raw.to_ascii_uppercase();
    let stripped = upper.strip_prefix("SIG").unwrap_or(&upper);
    KNOWN_SIGNAL_NAMES.iter().copied().find(|&n| n == stripped)
}

#[async_trait]
impl Tool for Kill {
    fn name(&self) -> &str {
        "kill"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &KillArgs::command(),
            "kill",
            "Send a signal to a process or job",
            [
                ("Terminate a job", "kill %1"),
                ("Kill a process by PID", "kill --signal KILL 1234"),
                ("Send signal 9 by number", "kill -9 %1"),
                ("Send STOP by name", "kill -STOP %1"),
            ],
        )
        .with_raw_argv()
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kill: {e}")),
        };
        let parsed = match KillArgs::try_parse_from(
            std::iter::once("kill".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kill: {e}")),
        };
        parsed.global.apply(ctx);

        // Hand-rolled grammar over the raw, source-ordered `args.positional`
        // (see the module docs for why this can't be a normal clap/binder
        // parse). `explicit_signal` is anything spelled via --signal/-s (any
        // form); `shorthand_signal` is a leading `-9`/`-STOP`-style token.
        // Kept separate so mixing the two forms can be flagged as ambiguous
        // instead of silently preferring one.
        let mut discard = false;
        let mut no_wait = false;
        let mut explicit_signal: Option<String> = None;
        let mut shorthand_signal: Option<String> = None;
        let mut targets: Vec<Value> = Vec::new();
        // POSIX end-of-options: once a literal `--` is seen, every following
        // token is an operand (a target), never a flag or shorthand — so
        // `kill -- -9 %1` tries to signal a PID literally named `-9`, not
        // send signal 9 (found via kaibo review: without this, `--` was
        // silently a no-op instead of actually ending option parsing).
        let mut past_dash_dash = false;

        let mut i = 0;
        while i < args.positional.len() {
            let value = &args.positional[i];
            if past_dash_dash {
                targets.push(value.clone());
                i += 1;
                continue;
            }
            match value {
                // Numeric shorthand: -9, -15, -2, -1, -<N>. Only a NEGATIVE
                // Int can be this — a real PID target is always positive, so
                // there's no ambiguity. (`-0` is not reachable this way: i64
                // has no negative zero, so it lexes identically to a bare
                // `0`; out of scope for this 80/20 pass.)
                Value::Int(n) if *n < 0 => {
                    shorthand_signal = Some((-n).to_string());
                }
                Value::String(s) => match s.as_str() {
                    "--" => past_dash_dash = true,
                    // --json is a kernel-level concern (GlobalFlags's
                    // raw_argv-aware pre-apply already handled it); nothing
                    // to do here but recognize and skip it, not error.
                    "--json" => {}
                    "--discard" => discard = true,
                    "--no-wait" => no_wait = true,
                    "-s" | "--signal" => {
                        i += 1;
                        let Some(next) = args.positional.get(i) else {
                            return ExecResult::failure(2, "kill: --signal requires a value".to_string());
                        };
                        explicit_signal = match crate::interpreter::value_to_text_sink_named(
                            next,
                            "a --signal value",
                        ) {
                            Ok(v) => Some(v),
                            Err(e) => return ExecResult::failure(2, format!("kill: {e}")),
                        };
                    }
                    _ if s.starts_with("--signal=") => {
                        explicit_signal = Some(s["--signal=".len()..].to_string());
                    }
                    _ if s.starts_with("--json=") => {}
                    _ if s.starts_with('-') && s.len() > 1 => {
                        let bare = s.trim_start_matches('-');
                        if let Some(canonical) = normalize_signal_name(bare) {
                            // Named shorthand: -STOP, -KILL, -SIGKILL, -kill, ...
                            shorthand_signal = Some(canonical.to_string());
                        } else if let Some(rest) = s.strip_prefix("-s").filter(|r| !r.is_empty()) {
                            // Glued explicit form: -sKILL (clap parity for
                            // the old `-s`-as-clap-short-flag behavior).
                            // Checked AFTER the shorthand-name match above so
                            // a lowercase name-shorthand starting with 's'
                            // (-stop, -sigkill, -sigstop, ...) resolves as
                            // the intended signal name, not as `-s` + glued
                            // garbage (the exact collision `raw_argv` exists
                            // to dodge — see the module docs).
                            explicit_signal = Some(rest.to_string());
                        } else {
                            return ExecResult::failure(
                                2,
                                format!(
                                    "kill: unrecognized option {s} — kill accepts --signal/-s \
                                     NAME, --discard, or a signal shorthand (-TERM/-KILL/-INT/\
                                     -HUP/-STOP/-CONT/-QUIT/-USR1/-USR2, case-insensitive, \
                                     optional SIG prefix) or -<N> (e.g. -9); see `help kill`"
                                ),
                            );
                        }
                    }
                    _ => targets.push(value.clone()),
                },
                _ => targets.push(value.clone()),
            }
            i += 1;
        }

        if discard && (explicit_signal.is_some() || shorthand_signal.is_some()) {
            return ExecResult::failure(
                2,
                "kill: --discard cannot be combined with a signal (--signal/-s or a \
                 shorthand) — discarding a gate delivers nothing to anyone"
                    .to_string(),
            );
        }
        if discard && no_wait {
            return ExecResult::failure(
                2,
                "kill: --discard cannot be combined with --no-wait — discarding a gate \
                 is immediate; there is nothing to wait for"
                    .to_string(),
            );
        }
        if explicit_signal.is_some() && shorthand_signal.is_some() {
            return ExecResult::failure(
                2,
                "kill: signal specified twice (--signal/-s and a shorthand) — use one form"
                    .to_string(),
            );
        }
        // `parsed.signal` is always clap's declared default ("TERM") here —
        // raw_argv means the real value never reaches clap's own parse — so
        // using it as the fallback keeps `#[arg(default_value_t = ...)]` the
        // single source of truth for what "no signal given" means.
        let signal_name = explicit_signal.or(shorthand_signal).unwrap_or(parsed.signal);

        if targets.is_empty() {
            return ExecResult::failure(
                1,
                "kill: usage: kill [--signal SIG | -SIG | -N] target...".to_string(),
            );
        }

        // Signal every target, like bash: one target's failure does not stop the
        // rest, and the exit code is 0 only if all of them succeeded. Reading
        // just `targets.first()` used to drop the remainder silently, so
        // `kill %1 %2` reported success having signalled only `%1`.
        let mut out = String::new();
        let mut any_failed = false;

        for target in &targets {
            let target_str = match target {
                Value::String(s) => s.clone(),
                Value::Int(i) => i.to_string(),
                other => {
                    any_failed = true;
                    out.push_str(&format!("kill: invalid target: {other:?}\n"));
                    continue;
                }
            };

            let result = kill_one(ctx, &target_str, &signal_name, discard, no_wait).await;
            if result.code != 0 {
                any_failed = true;
            }
            let text = result.text_out();
            let text = text.trim_end();
            if !text.is_empty() {
                out.push_str(text);
                out.push('\n');
            }
            if !result.err.is_empty() {
                out.push_str(result.err.trim_end());
                out.push('\n');
            }
        }

        let out = out.trim_end().to_string();
        if any_failed {
            ExecResult::failure(1, out)
        } else {
            ExecResult::success(out)
        }
    }
}

/// Signal one target: a `%N` job reference or a bare PID.
///
/// Split out of `execute()` so the multi-target loop can run every target and
/// aggregate the outcome. Each early return here ends one target, not the
/// command — that distinction is the whole fix.
async fn kill_one(
    ctx: &ExecContext,
    target_str: &str,
    signal_name: &str,
    discard: bool,
    no_wait: bool,
) -> ExecResult {
    // Job reference `%N` — kaish-level job control, available in every build.
    if let Some(job_num) = target_str.strip_prefix('%') {
        let job_id = match job_num.parse::<u64>() {
            Ok(i) => JobId(i),
            Err(_) => {
                return ExecResult::failure(1, format!("kill: invalid job reference: {target_str}"))
            }
        };
        let manager = match &ctx.job_manager {
            Some(m) => m.clone(),
            None => return ExecResult::failure(1, "kill: no job manager"),
        };
        // A gated job's cached result is the only reference to its pending
        // approval request — killing it would silently destroy the gate
        // (GH #96). Refuse unless the caller explicitly discards.
        //
        // TOCTOU note: a Running job can race into Gated between this
        // check and kill_job's cancel+remove. That's acceptable — the
        // guard protects an already-visible approval request from
        // accidental destruction; a job killed while still running is the
        // caller's stated intent, whatever it was about to become.
        if manager.is_gated(job_id).await {
            if !discard {
                return ExecResult::failure(
                    1,
                    format!(
                        "kill: job {job_id} is gated awaiting approval — \
                         fulfill it (see /v/jobs/{job_id}/approval) or abandon \
                         it with: kill --discard %{job_id}"
                    ),
                );
            }
            // Close the held request before the job entry — and with it the
            // only reference to that request — is dropped
            // (`docs/approval-ledger.md` §B.5, "a job is discarded").
            // Nothing times a request out, so a discard that skipped this
            // would leave a live ledger slot nobody can reach.
            if let Some(access) = ctx.ledger_access.as_ref() {
                crate::ledger::cancel_job_request(
                    &access.requester,
                    &access.principal,
                    &manager,
                    job_id,
                    kaish_types::approval::CancelReason::Withdrawn,
                )
                .await;
            }
            manager.cancel(job_id).await;
            manager.remove(job_id).await;
            return ExecResult::success(format!(
                "kill: discarded the pending approval request for job {job_id}"
            ));
        }
        // A job that already finished keeps its entry (and result) until
        // reaped — a second kill is a clean no-op that says so, keeping the
        // MCP surface idempotent: before GH #244, "I killed job 1" and
        // "job 1 never existed" both answered `job 1 not found`.
        if let Some(status) = already_finished(&manager, job_id).await {
            return ExecResult::success(format!(
                "kill: job {job_id} already finished ({status})"
            ));
        }
        return kill_job(&manager, job_id, signal_name, no_wait).await;
    }

    // Bare PID — signalling an OS process needs the subprocess capability.
    kill_pid(target_str, signal_name)
}

/// The job's status string if it has already unwound (its result is cached),
/// `None` while it is still running/stopped or does not exist.
async fn already_finished(manager: &JobManager, job_id: JobId) -> Option<String> {
    if manager.try_result(job_id).await.is_some() {
        manager.get_status_string(job_id).await
    } else {
        None
    }
}

/// Terminate a running job and confirm the death: flag it killed (so its
/// terminal status reads `killed:{code}`, not `failed:{code}`), trip the
/// cancellation cascade, and — unless `--no-wait` — wait for the job to
/// actually unwind, so exit 0 means "dead", never "signal dispatched"
/// (GH #244). The job entry, its result, and its output all stay tracked
/// until reaped (`jobs --cleanup`, the REPL prompt, or retention).
///
/// `delivered` says whether a real OS signal already went out via `killpg` —
/// in that case a missing cancellation token is not an error (the wrapper
/// task unwinds when its children die); with no delivery and no token there
/// is nothing to kill with, and that fails loud.
async fn terminate_and_confirm(
    manager: &JobManager,
    job_id: JobId,
    no_wait: bool,
    delivery: &str,
    delivered: bool,
) -> ExecResult {
    // Flag + cancel are one atomic manager call: the flag turns the terminal
    // status into Killed, so it must never be set when there is no lever to
    // kill with (no token, nothing delivered) — a later organic failure
    // would read as a kill that never happened.
    if !manager.mark_killed_and_cancel(job_id, delivered).await {
        return ExecResult::failure(
            1,
            format!(
                "kill: job {job_id} has no cancellation token and no live process \
                 group — nothing to deliver {delivery} to"
            ),
        );
    }
    if no_wait {
        return ExecResult::success(format!(
            "kill: job {job_id}: {delivery} dispatched; not awaited (--no-wait) — the \
             job stays in jobs until it unwinds"
        ));
    }
    // Bound the wait by the cascade's own SIGTERM→SIGKILL grace plus margin:
    // past that, something is genuinely stuck and kill says so with exit 1
    // rather than parking forever or guessing.
    let bound = manager.kill_grace() + std::time::Duration::from_secs(3);
    match tokio::time::timeout(bound, manager.wait(job_id)).await {
        Ok(Some(_)) => {
            let status = manager
                .get_status_string(job_id)
                .await
                .unwrap_or_else(|| "killed".to_string());
            ExecResult::success(format!("kill: job {job_id} exited ({status})"))
        }
        Ok(None) => ExecResult::failure(
            1,
            format!(
                "kill: job {job_id}: {delivery} dispatched, but the job is stopped or no \
                 longer tracked — resume it (bg %{job_id}) or check jobs"
            ),
        ),
        Err(_) => ExecResult::failure(
            1,
            format!(
                "kill: job {job_id}: {delivery} dispatched, but the job has not exited \
                 after {bound:?} — check jobs; wait %{job_id} returns its result when it dies"
            ),
        ),
    }
}

/// Classify a signal as terminating (unwinds the job) vs. non-terminating,
/// without depending on platform signal types. Used by the hermetic job path to
/// decide between cancelling the job and refusing a signal it cannot deliver.
/// `None` = unrecognised signal.
///
/// Only the portable POSIX signal numbers (identical across Linux/macOS/BSD) are
/// classified numerically; job-control numerics that vary by platform
/// (`STOP`/`CONT`/`USR*`) are matched by name. An unknown number is treated as
/// non-terminating — safe, since the hermetic path can only ever *cancel*.
#[cfg(not(all(unix, feature = "subprocess")))]
fn signal_is_terminating(name: &str) -> Option<bool> {
    if let Ok(num) = name.parse::<i32>() {
        return match num {
            // Portable terminating signals: HUP, INT, QUIT, ABRT, KILL, TERM.
            1 | 2 | 3 | 6 | 9 | 15 => Some(true),
            n if n > 0 => Some(false),
            _ => None,
        };
    }
    match normalize_signal_name(name)? {
        "TERM" | "KILL" | "INT" | "HUP" | "QUIT" => Some(true),
        "STOP" | "CONT" | "USR1" | "USR2" => Some(false),
        other => unreachable!(
            "normalize_signal_name returned {other:?}, outside its own KNOWN_SIGNAL_NAMES table"
        ),
    }
}

// ─── Hermetic build: job control via cancellation tokens only ───────────────

/// Terminate a kaish job by its cancellation token. No OS-signal support, so
/// only terminating signals are honoured; anything else is refused loudly.
#[cfg(not(all(unix, feature = "subprocess")))]
async fn kill_job(
    manager: &JobManager,
    job_id: JobId,
    signal_name: &str,
    no_wait: bool,
) -> ExecResult {
    match signal_is_terminating(signal_name) {
        Some(true) => {
            if !manager.exists(job_id).await {
                return ExecResult::failure(1, format!("kill: job {job_id} not found"));
            }
            terminate_and_confirm(manager, job_id, no_wait, "termination", false).await
        }
        Some(false) => ExecResult::failure(
            1,
            format!(
                "kill: job {job_id} is an in-process task; only termination signals \
                 (TERM/KILL/INT/HUP/QUIT) can be delivered, not {signal_name} \
                 (arbitrary-signal delivery needs the subprocess capability)"
            ),
        ),
        None => ExecResult::failure(1, format!("kill: unknown signal: {signal_name}")),
    }
}

/// Without the subprocess capability there are no OS processes kaish spawned to
/// signal, so a bare-PID target cannot be honoured.
#[cfg(not(all(unix, feature = "subprocess")))]
fn kill_pid(target: &str, _signal_name: &str) -> ExecResult {
    ExecResult::failure(
        1,
        format!("kill: {target}: signalling a PID requires the subprocess capability"),
    )
}

// ─── subprocess build: full OS-signal fidelity ──────────────────────────────

/// Signal a kaish job. Prefers the real process group(s) the job spawned (so any
/// signal — STOP/CONT/USR1/… — is delivered faithfully); falls back to the
/// cancellation token for pure in-process jobs (which can only be terminated).
#[cfg(all(unix, feature = "subprocess"))]
async fn kill_job(
    manager: &JobManager,
    job_id: JobId,
    signal_name: &str,
    no_wait: bool,
) -> ExecResult {
    use nix::sys::signal::Signal;

    let signal = match parse_signal(signal_name) {
        Some(s) => s,
        None => return ExecResult::failure(1, format!("kill: unknown signal: {signal_name}")),
    };
    let terminating = matches!(
        signal,
        Signal::SIGTERM | Signal::SIGKILL | Signal::SIGINT | Signal::SIGHUP | Signal::SIGQUIT
    );
    if !manager.exists(job_id).await {
        return ExecResult::failure(1, format!("kill: job {job_id} not found"));
    }
    let stopped = matches!(
        manager.get(job_id).await.map(|info| info.status),
        Some(crate::scheduler::JobStatus::Stopped)
    );

    // Real process group(s) recorded for the job — deliver the signal directly.
    let pgids = manager.job_pgids(job_id).await;
    if !pgids.is_empty() {
        // A recorded group whose processes have all exited (`ESRCH`) is
        // stale, not a failure: a pipeline job records one group per external
        // child and finished children are never unrecorded, so `true |
        // sleep 30 &` has a dead group next to the live one (review finding —
        // the old aggregate-error return aborted the kill after successfully
        // signalling the live group, and skipped the lifecycle entirely).
        let mut delivered_any = false;
        let mut hard_err = None;
        for pg in &pgids {
            let pgid = nix::unistd::Pid::from_raw(*pg as i32);
            match nix::sys::signal::killpg(pgid, signal) {
                Ok(()) => delivered_any = true,
                Err(nix::errno::Errno::ESRCH) => {}
                Err(e) => hard_err = Some(e),
            }
        }
        if let Some(e) = hard_err {
            return ExecResult::failure(1, format!("kill: {e}"));
        }
        let groups = pgids
            .iter()
            .map(u32::to_string)
            .collect::<Vec<_>>()
            .join(", ");
        // A stopped (Ctrl-Z) job cannot act on a termination while it is
        // suspended — the signal sits pending forever — so, like bash, kill
        // follows it with SIGCONT on every group. There is no result to
        // persist either (a stopped foreground job has no result channel;
        // its entry would read "stopped" forever), so this is the one kill
        // that still untracks the job (GH #244 scoped stopped/TTY jobs out
        // of the persist rule).
        if terminating && stopped {
            for pg in &pgids {
                let pgid = nix::unistd::Pid::from_raw(*pg as i32);
                // Error intentionally ignored: the group may already have
                // died on the first signal, and CONT is only the wake-up
                // nudge.
                let _ = nix::sys::signal::killpg(pgid, Signal::SIGCONT);
            }
            manager.remove(job_id).await;
            return ExecResult::success(if delivered_any {
                format!(
                    "kill: job {job_id} (stopped): delivered {signal} and CONT to \
                     process group(s) {groups}; job untracked"
                )
            } else {
                format!(
                    "kill: job {job_id} (stopped): process group(s) {groups} already \
                     gone; job untracked"
                )
            });
        }
        // A terminating signal also unwinds the wrapping task; the job entry
        // stays, with its result and output, until reaped (GH #244). With
        // every group stale, the cancellation token is the remaining lever —
        // terminate_and_confirm fails loud if there is neither.
        if terminating {
            let delivery = format!("{signal} to process group(s) {groups}");
            return terminate_and_confirm(manager, job_id, no_wait, &delivery, delivered_any)
                .await;
        }
        if delivered_any {
            return ExecResult::success(format!(
                "kill: job {job_id}: delivered {signal} to process group(s) {groups}"
            ));
        }
        return ExecResult::failure(
            1,
            format!(
                "kill: job {job_id}: recorded process group(s) {groups} have already \
                 exited — nothing to deliver {signal} to"
            ),
        );
    }

    // No process group — a pure in-process job (e.g. `sleep &`, a kaish builtin)
    // or an external whose PGID hasn't registered yet. The cancellation token is
    // the only lever; it can stop the job but not deliver an arbitrary signal.
    if terminating {
        terminate_and_confirm(manager, job_id, no_wait, "termination", false).await
    } else {
        ExecResult::failure(
            1,
            format!(
                "kill: job {job_id} is an in-process task with no process group; \
                 only termination signals (TERM/KILL/INT/HUP/QUIT) can be delivered, not {signal_name}"
            ),
        )
    }
}

/// Send a signal to a bare PID via the OS.
#[cfg(all(unix, feature = "subprocess"))]
fn kill_pid(target: &str, signal_name: &str) -> ExecResult {
    let signal = match parse_signal(signal_name) {
        Some(s) => s,
        None => return ExecResult::failure(1, format!("kill: unknown signal: {signal_name}")),
    };
    let pid_num: i32 = match target.parse() {
        Ok(p) => p,
        Err(_) => return ExecResult::failure(1, format!("kill: invalid pid: {target}")),
    };
    let pid = nix::unistd::Pid::from_raw(pid_num);
    if let Err(e) = nix::sys::signal::kill(pid, signal) {
        return ExecResult::failure(1, format!("kill: ({pid_num}): {e}"));
    }
    ExecResult::success("")
}

/// Parse a signal name or number to a `nix` Signal value.
#[cfg(all(unix, feature = "subprocess"))]
fn parse_signal(name: &str) -> Option<nix::sys::signal::Signal> {
    use nix::sys::signal::Signal;

    // Try as number first
    if let Ok(num) = name.parse::<i32>() {
        return Signal::try_from(num).ok();
    }

    match normalize_signal_name(name)? {
        "TERM" => Some(Signal::SIGTERM),
        "KILL" => Some(Signal::SIGKILL),
        "STOP" => Some(Signal::SIGSTOP),
        "CONT" => Some(Signal::SIGCONT),
        "INT" => Some(Signal::SIGINT),
        "HUP" => Some(Signal::SIGHUP),
        "USR1" => Some(Signal::SIGUSR1),
        "USR2" => Some(Signal::SIGUSR2),
        "QUIT" => Some(Signal::SIGQUIT),
        other => unreachable!(
            "normalize_signal_name returned {other:?}, outside its own KNOWN_SIGNAL_NAMES table"
        ),
    }
}

#[cfg(test)]
mod tests {
    use super::normalize_signal_name;

    #[test]
    fn normalize_signal_name_is_case_insensitive() {
        assert_eq!(normalize_signal_name("kill"), Some("KILL"));
        assert_eq!(normalize_signal_name("Kill"), Some("KILL"));
        assert_eq!(normalize_signal_name("KILL"), Some("KILL"));
    }

    #[test]
    fn normalize_signal_name_accepts_optional_sig_prefix() {
        assert_eq!(normalize_signal_name("SIGKILL"), Some("KILL"));
        assert_eq!(normalize_signal_name("sigstop"), Some("STOP"));
        assert_eq!(normalize_signal_name("STOP"), Some("STOP"));
    }

    #[test]
    fn normalize_signal_name_rejects_unknown_names() {
        assert_eq!(normalize_signal_name("ABRT"), None);
        assert_eq!(normalize_signal_name("TSTP"), None);
        assert_eq!(normalize_signal_name("WINCH"), None);
        assert_eq!(normalize_signal_name("FOOBAR"), None);
    }
}
