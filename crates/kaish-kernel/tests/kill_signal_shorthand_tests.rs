//! TDD for GH #198: `kill` bash/POSIX signal shorthand (`kill -9 %1`,
//! `kill -STOP %1`) alongside the existing `--signal NAME`/`-s NAME` form.
//!
//! Before this fix, `kill -9 %1` silently mis-bound: `-9` lexes as a plain
//! `Int(-9)` positional (untouched by the argv binder), and `kill.rs` always
//! read *positional 0* as the target — so `-9` became the "target" (tried as
//! a PID) while `%1`, the real target, was silently dropped with no error at
//! all. `kill -STOP %1` was worse: `-STOP` lexes as a multi-character
//! `ShortFlag`, and the *default* (non-raw_argv) arg binder splits any
//! undeclared multi-char short flag into one boolean flag per letter
//! (`-S -T -O -P`) before `kill.rs` ever runs — surfacing a confusing
//! `unexpected argument '-O'` clap error instead of anything about signals.
//!
//! These tests exercise the real path end-to-end via `kernel.execute(...)`
//! (never `Kill.execute()` directly — see CLAUDE.md), because the bug lived
//! in argv binding, not in the tool body.

// Test-fixture code: unwrap/expect on known-good setup is the idiom here.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::sync::Arc;

use kaish_kernel::{Kernel, KernelConfig};

async fn setup() -> Arc<Kernel> {
    Kernel::new(KernelConfig::isolated()).expect("failed to create kernel").into_arc()
}

/// Prove a job was killed earlier: a second `kill %N` on it is an idempotent
/// no-op naming the terminal status (GH #244). The old proof — `kill %N`
/// failing with "not found" — pinned delete-on-kill, which #244 retired: a
/// killed job stays tracked with its result until reaped.
async fn assert_already_killed(kernel: &Kernel, spec: &str) {
    let again = kernel.execute(&format!("kill {spec}")).await.expect("execute");
    assert_eq!(again.code, 0, "re-kill of a killed job is a clean no-op: {}", again.err);
    assert!(
        again.text_out().contains("already finished (killed:"),
        "job {spec} must be tracked as killed, got: {}",
        again.text_out()
    );
}

/// `kill -9 %1` must terminate job 1 — not silently try to signal a PID named
/// "-9" while dropping `%1` on the floor. A follow-up `kill %1` reporting the
/// job gone is the only way to distinguish "job 1 was really killed" from the
/// old bug (which errored on a bogus PID target and left job 1 running).
#[tokio::test]
async fn dash9_numeric_shorthand_kills_the_job_not_a_pid_named_dash9() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -9 %1").await.expect("execute");
    assert_eq!(r.code, 0, "kill -9 %1 should terminate job 1: out={} err={}", r.text_out(), r.err);

    assert_already_killed(&kernel, "%1").await;
}

/// `-15` (SIGTERM) is the other numeric shorthand explicitly called out —
/// same story as `-9`.
#[tokio::test]
async fn dash15_numeric_shorthand_kills_the_job() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -15 %1").await.expect("execute");
    assert_eq!(r.code, 0, "kill -15 %1 should terminate job 1: {}", r.err);
}

/// `-2` (SIGINT) — the general `-<N>` numeric form, not just the two signals
/// named explicitly in the issue.
#[tokio::test]
async fn dash2_numeric_shorthand_kills_the_job() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -2 %1").await.expect("execute");
    assert_eq!(r.code, 0, "kill -2 %1 should terminate job 1: {}", r.err);
}

/// `-STOP` is a *recognized* signal name (unlike the old shredded-into-`-O`
/// failure) that just can't be delivered to a pure in-process job — same
/// refusal shape as the existing `--signal USR1` coverage. Proves `-STOP`
/// reached signal resolution as "STOP", not as four stray boolean flags.
#[tokio::test]
async fn dash_stop_name_shorthand_is_recognized_and_refused_on_builtin_job() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -STOP %1").await.expect("execute");
    assert!(!r.ok(), "STOP can't reach a pure in-process job: {:?}", r);
    assert!(
        r.err.contains("in-process task") && r.err.contains("STOP"),
        "expected an in-process-task/STOP refusal, got: {}",
        r.err
    );
    let _ = kernel.execute("kill %1").await;
}

/// Case-insensitivity (bash accepts `-kill` as well as `-KILL`) — costs
/// nothing at the *shorthand* layer since kaish owns the fixed name table.
#[tokio::test]
async fn dash_name_shorthand_is_case_insensitive() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -kill %1").await.expect("execute");
    assert_eq!(r.code, 0, "lowercase -kill should work like -KILL: {}", r.err);

    let kernel2 = setup().await;
    let r2 = kernel2.execute("sleep 30 & kill -Kill %1").await.expect("execute");
    assert_eq!(r2.code, 0, "mixed-case -Kill should work like -KILL: {}", r2.err);
}

/// Both `-KILL` and `-SIGKILL` spellings are accepted, matching bash.
#[tokio::test]
async fn dash_sig_prefixed_shorthand_is_accepted() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -SIGKILL %1").await.expect("execute");
    assert_eq!(r.code, 0, "kill -SIGKILL %1 should terminate job 1: {}", r.err);
}

/// `-SIGSTOP` behaves exactly like `-STOP` (refused on a builtin job, not
/// silently dropped) — the `SIG` prefix is optional, not a different signal.
#[tokio::test]
async fn dash_sigstop_behaves_like_dash_stop() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -SIGSTOP %1").await.expect("execute");
    assert!(!r.ok());
    assert!(r.err.contains("STOP"), "got: {}", r.err);
    let _ = kernel.execute("kill %1").await;
}

/// A name outside kaish's fixed 80/20 table must fail LOUDLY, naming what IS
/// supported — never silently fall back to TERM or get silently absorbed as
/// an inert flag (the pre-fix behavior for any undeclared single-char flag).
#[tokio::test]
async fn unsupported_shorthand_name_fails_loudly_naming_supported_set() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -FOOBAR %1").await.expect("execute");
    assert_eq!(r.code, 2, "unsupported shorthand is a usage error: {:?}", r);
    assert!(r.err.contains("FOOBAR"), "names the bad token: {}", r.err);
    assert!(
        r.err.contains("KILL") && r.err.contains("TERM"),
        "names the supported set: {}",
        r.err
    );
    let _ = kernel.execute("kill %1").await;
}

/// `ABRT` is deliberately OUT of kaish's fixed shorthand/name table (Amy's
/// 80/20 scope for kaijutsu job control) even though the old hermetic
/// `signal_is_terminating` used to recognize it by coincidence — pinned so
/// the two build configurations (hermetic vs `subprocess`) can't silently
/// diverge on which names are "known" again.
#[tokio::test]
async fn abrt_is_outside_the_fixed_table() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill --signal ABRT %1").await.expect("execute");
    assert_eq!(r.code, 1, "ABRT is unknown to kaish's table: {:?}", r);
    assert!(r.err.contains("unknown signal"), "got: {}", r.err);
    let _ = kernel.execute("kill %1").await;
}

/// Explicit `--signal NAME` / `-s NAME` must keep working exactly as before —
/// the shorthand is additive, not a replacement.
#[tokio::test]
async fn explicit_signal_forms_still_work() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill --signal KILL %1").await.expect("execute");
    assert_eq!(r.code, 0, "kill --signal KILL %1: {}", r.err);

    let kernel2 = setup().await;
    let r2 = kernel2.execute("sleep 30 & kill -s KILL %1").await.expect("execute");
    assert_eq!(r2.code, 0, "kill -s KILL %1: {}", r2.err);
}

/// Adjacent bug found while fixing #198: `--signal`'s name matching was
/// case-sensitive (`kill --signal kill` failed as "unknown signal: kill")
/// even though the shorthand form is meant to be case-insensitive. Both
/// forms now share one normalizer, so both are case-insensitive together.
#[tokio::test]
async fn explicit_signal_is_now_case_insensitive_too() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill --signal kill %1").await.expect("execute");
    assert_eq!(r.code, 0, "lowercase --signal kill should work: {}", r.err);
}

/// The glued short form (`-sKILL`, no space) is real clap-parity for the
/// explicit path — still works once `-s` is no longer clap's own argv layer.
#[tokio::test]
async fn glued_short_signal_value_still_works() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -sKILL %1").await.expect("execute");
    assert_eq!(r.code, 0, "kill -sKILL %1 should terminate job 1: {}", r.err);
}

/// `--discard` still conflicts with a signal — now checked against *either*
/// the explicit form or a shorthand, not just the old declared `signal` field.
#[tokio::test]
async fn discard_conflicts_with_shorthand_signal() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill --discard -9 %1").await.expect("execute");
    assert_eq!(r.code, 2, "--discard + shorthand is a usage error: {:?}", r);
    let _ = kernel.execute("kill %1").await;
}

/// Giving both an explicit `--signal` and a shorthand on the same invocation
/// is ambiguous — fail loud rather than silently picking one.
#[tokio::test]
async fn explicit_and_shorthand_signal_together_is_a_usage_error() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill --signal TERM -9 %1").await.expect("execute");
    assert_eq!(r.code, 2, "specifying a signal twice is a usage error: {:?}", r);
    let _ = kernel.execute("kill %1").await;
}

/// Companion fix discovered while adopting `raw_argv` for the shorthand
/// (GH #198): a raw_argv tool's binder never lifts `--json` into
/// `args.flags` (that's the whole point of raw_argv), so `GlobalFlags`'s
/// kernel-side pre-apply used to miss it entirely — `kill --json ...` would
/// have silently stopped honoring `--json` the moment `kill` went raw_argv.
/// Fixed once in `GlobalFlags::apply_from_args` (kaish-tool-api) for every
/// raw_argv builtin; this pins it for `kill` specifically via a usage error
/// (has real `err` text either way, so the JSON envelope is unambiguous).
#[tokio::test]
async fn json_flag_still_applies_under_raw_argv() {
    let kernel = setup().await;
    let r = kernel.execute("kill --json --discard -9 %1").await.expect("execute");
    assert_eq!(r.code, 2, "usage error as usual: {:?}", r);
    let out = r.text_out();
    assert!(
        out.trim_start().starts_with('{') && out.contains("\"error\""),
        "expected a JSON error envelope, got: {out}"
    );
}

/// `kill %1` with no signal at all still defaults to TERM (unchanged).
#[tokio::test]
async fn no_signal_still_defaults_to_term() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill %1").await.expect("execute");
    assert_eq!(r.code, 0, "bare kill %1 should still terminate the job: {}", r.err);
}

/// Known, accepted trade-off of `raw_argv`: the kernel's generic `--help`/
/// `-h` interception reads `args.flags`, which is always empty under
/// raw_argv, so it no longer intercepts `kill --help`/`kill -h` (same gap
/// `test` already had — this isn't new to raw_argv, just newly relevant to
/// `kill`). Pinned here rather than silently drifting: `--help`/`-h` now
/// fall into the "unrecognized option" usage error, which explicitly points
/// at `help kill` — the fully equivalent replacement (verified byte-for-byte
/// identical to the old `kill --help` output before this change).
#[tokio::test]
async fn help_flag_no_longer_auto_intercepted_but_points_at_help_kill() {
    let kernel = setup().await;
    let r = kernel.execute("kill --help").await.expect("execute");
    assert_eq!(r.code, 2, "no longer auto-intercepted: {:?}", r);
    assert!(r.err.contains("help kill"), "points at the replacement: {}", r.err);

    let help_kill = kernel.execute("help kill").await.expect("execute");
    assert_eq!(help_kill.code, 0);
    assert!(help_kill.text_out().contains("--signal"), "help kill documents --signal: {}", help_kill.text_out());
}

/// `-h` gets the same treatment as `--help` (both fall into the
/// "unrecognized option" branch) — pinned separately since it's a distinct
/// token shape.
#[tokio::test]
async fn dash_h_also_points_at_help_kill() {
    let kernel = setup().await;
    let r = kernel.execute("kill -h").await.expect("execute");
    assert_eq!(r.code, 2, "no longer auto-intercepted: {:?}", r);
    assert!(r.err.contains("help kill"), "points at the replacement: {}", r.err);
}

/// Found via kaibo review: a literal `--` end-of-options marker must
/// actually end option parsing, not be a no-op. `kill -- -9 %1` must treat
/// `-9` as a target literally named `-9` (which fails loudly — no such
/// capability/process), never quietly resolve it as the numeric shorthand
/// once `--` has been seen.
///
/// Note the operand count: after `--` there are **two** targets, `-9` and
/// `%1`, and kill signals every target. This test used to also assert "job 1
/// must be untouched", which was true only because kill read `targets.first()`
/// and silently dropped the rest — the test was pinning a consequence of that
/// bug. With bash-parity multi-target, `%1` is signalled as its own operand
/// (with the default TERM, not the `-9` that `--` disarmed), and the run exits
/// non-zero because `-9` failed.
#[tokio::test]
async fn double_dash_ends_option_parsing() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -- -9 %1").await.expect("execute");
    assert!(!r.ok(), "post-- `-9` must be an operand, not a signal: {:?}", r);
    assert!(
        r.err.contains("-9"),
        "the literal PID target -9 is named in the error: {}",
        r.err
    );
    // `%1` was a target in its own right — the failing `-9` did not stop
    // kill from getting to it (with the default TERM, since `--` disarmed -9).
    assert_already_killed(&kernel, "%1").await;
}

/// A literal `--` also ends option parsing for flag-shaped tokens, not just
/// the signal shorthand — `kill -- --discard %1` must try to signal a target
/// literally named `--discard`, not treat it as an option.
///
/// Same operand-count note as the test above: `--discard` and `%1` are two
/// targets, so job 1 is signalled on its own account. The old "job 1 must be
/// untouched" assertion was pinning the drop-after-first bug.
#[tokio::test]
async fn double_dash_ends_option_parsing_for_discard_too() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill -- --discard %1").await.expect("execute");
    assert!(!r.ok(), "post-- `--discard` must be an operand, not a flag: {:?}", r);
    assert!(
        r.err.contains("--discard"),
        "the literal target --discard is named in the error: {}",
        r.err
    );
    // The point that still matters: `--discard` was NOT honoured as a flag, so
    // job 1's gate (had it any) was never discarded — it was just signalled.
    assert_already_killed(&kernel, "%1").await;
}

// ─── Multi-target (bash parity) ────────────────────────────────────────────
//
// `kill` collected every positional target but only ever read
// `targets.first()`, so `kill %1 %2` signalled job 1, dropped job 2 without a
// word, and exited 0. A destructive command reporting success for work it did
// not do is the failure class this project treats as unacceptable — and the
// published arg doc said "Target PID(s) or job specifier(s)", plural, so the
// schema promised what the code did not deliver.

/// Every target gets signalled, not just the first.
#[tokio::test]
async fn kill_signals_every_target_not_just_the_first() {
    let kernel = setup().await;
    let r = kernel
        .execute("sleep 30 & sleep 30 & kill %1 %2")
        .await
        .expect("execute");
    assert_eq!(
        r.code, 0,
        "kill %1 %2 should terminate both jobs: out={} err={}",
        r.text_out(),
        r.err
    );

    for spec in ["%1", "%2"] {
        assert_already_killed(&kernel, spec).await;
    }
}

/// A bad target does not abort the run: the later good target is still
/// signalled, and the exit code still reports the failure. This is the half
/// that a naive "return on first error" rewrite would get wrong — `%99` comes
/// first deliberately.
#[tokio::test]
async fn kill_continues_past_a_bad_target_and_still_reports_failure() {
    let kernel = setup().await;
    let r = kernel.execute("sleep 30 & kill %99 %1").await.expect("execute");
    assert!(
        !r.ok(),
        "a bad target must make kill exit non-zero: out={} err={}",
        r.text_out(),
        r.err
    );

    assert_already_killed(&kernel, "%1").await;
}

/// The mixed run, pinned end-to-end: targets before AND after a failing middle
/// one are both signalled, and the aggregate exit code reports the failure.
/// (`%99` first and `%99` last are each covered above; this closes the
/// remaining shape.)
#[tokio::test]
async fn kill_mixed_run_signals_good_targets_around_a_bad_one_and_exits_nonzero() {
    let kernel = setup().await;
    let r = kernel
        .execute("sleep 30 & sleep 30 & kill %1 %99 %2")
        .await
        .expect("execute");
    assert!(
        !r.ok(),
        "one failed target must make the whole kill exit non-zero: out={} err={}",
        r.text_out(),
        r.err
    );
    assert!(
        r.err.contains("not found"),
        "the error must name the missing job: {}",
        r.err
    );

    for spec in ["%1", "%2"] {
        assert_already_killed(&kernel, spec).await;
    }
}
