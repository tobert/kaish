# kaish devlog

Narrative history of landed work and the decisions behind it — the "how we got
here" color that used to clutter [issues.md](issues.md) and tax every read of it.
This is *not* the authoritative record: `CHANGELOG.md` (per-version, user-facing)
and `git log` (commits, SHAs) are canonical. This is the story.

Newest themes first within each area; dates are when the work landed.

Write entries **late** — just before signoff or opening a PR — so they carry with
the PR and describe the work as it actually landed. Not early, not mid-flight: the
decisions aren't settled yet, and an entry written ahead of its change goes stale
before it ships.

---

## The lexer becomes one machine (2026-07-16)

GH #95 sat locked since 2026-07-04: two cross-model batch reviews (gemini-pro,
fable) had independently concluded that `lexer.rs`'s four-stage pipeline — two
string-rewriting preprocessors → logos → marker re-threading → three fusion
passes — was architecturally overdue, its stages communicating through in-band
marker strings, span adjacency, and pass ordering that nothing checked. Amy's
call was aggressive: full rewrite, single PR, trust the ~4,100-test suite as
the spec. It held up — the entire workspace suite passed the new engine after
exactly one fix (bare-CR heredoc terminators, a Mac-classic edge the old code
normalized as a side effect of its rewriting).

The census phase did the heavy lifting. Tracing the seams before writing code
settled the two questions #95 left open and overturned one of its assumptions:
multiline list/record literals are legal (the parser eats interior newlines),
so the context stack must NOT reset at newline inside an open literal — and
nested `$((..))` inside `$( )` was never "the subcommand's problem" because
substitution bodies are lexed inline; the old skip just left inner arithmetic
raw and unparseable. The census also found two corruption bugs #95 didn't
list: `${X:-$((1+2))}` leaked raw `__KAISH_ARITH_…__` marker text into the
AST, and the `#` of `$#` opened comment state in the preprocessor. And it
found that the grammar accepts *spaced* assignment (`x = [a b]`), which
reshaped the bug-5 fix from "require span adjacency" to a small statement-head
DFA that models the real assignment shapes (glued, spaced, `local`,
subscripted lvalues, env-prefix chains) — an argv `=` opens nothing.

The engine: one composed scanner with explicit quote/escape/comment state
extracts heredocs and arithmetic together and records a complete replacement
table in both coordinate systems — heredocs included, which kills the span
drift the old pipeline documented in its own comments (`body_start_offset` is
now exact). Markers resolve back to tokens positionally, keyed by table
ranges; a word glued onto a marker splits into `Arithmetic` plus re-lexed
fragments so the parser's no-pasting guard fires loudly where marker garbage
used to leak (`echo $((1+2))abc`; bash's `3abc` interpolation stays rejected
as scope creep, but `echo "$((1+2))abc"` works). Fusion slices its text
verbatim from the source, so `a:007` and `007*` finally keep their zeros. The
context pass is an explicit frame stack — `$( )` pushes a fresh scope, which
is what fixes `[[ -n $(x=[a]) ]]` leaking test depth into the substitution.
The logos vocabulary and `Token` enum were not touched: ~60 shipped idiom
decisions live in those regexes and none of the bugs did.

One deviation from #95's locked sketch: the three fusion passes stayed three
passes (sharing the one new context walker) instead of collapsing into one.
Their alphabets differ deliberately — `Float` colon-fuses but doesn't
glob-fuse (`1.5:x*` fuses; `1.5*` stays split) — and a union-alphabet single
pass would have changed tokenization in corners the suite pins. Composition
preserved the interactions; the bugs weren't in the pass structure anyway.

Review pair per house style, pointed at the worktree: the deepseek consult's
one HIGH finding (line continuations corrupting heredoc collection) didn't
reproduce — the scanner's behavior is bash's (continuation joins the
introducer line; `cat <<EOF \` + `| tr a-z A-Z` uppercases the body),
verified end-to-end and pinned as a test — but its four smaller cleanups were
real and landed. The gemini-pro batch leg came back truncated mid-thought;
its one visible finding was the documented spaced-lvalue trade-off. 25 new
characterization tests in `lexer_pipeline_tests.rs` keep all eight corruption
classes dead.

---

## kaish gets CI, and CI immediately earns its keep (2026-07-13)

Three years of shell projects say the first CI run never passes; kaish kept the
streak. PR #169 added the repo's first GitHub Actions workflow — modeled on
kaibo's (pinned-SHA actions, minimal permissions, per-ref concurrency) but
encoding kaish's own CLAUDE.md gates: `cargo test --all --locked` + clippy
`-D warnings` with `--all-targets`, the kernel's no-default-features leg, the
`kaish-wasi` wasm32-wasip1 build, and a tripwire for committed `.snap.new`
files. No release workflow on purpose: kaish ships crates, publishing stays the
manual `/release` runbook, and kaibo's TLS-invariant guard didn't come along
because kaish has no TLS surface.

The first live run failed two of three jobs, and every failure was a
pre-existing repo bug, not a workflow bug:

- `non_interactive_stdin_is_dev_null` had been silently testing the wrong
  process for who-knows-how-long. Its comment said "/bin/readlink to bypass the
  builtin"; its code ran bare `readlink` — the kaish builtin — which resolves
  the *test process's* fd/0. It only ever passed where the test runner itself
  had stdin=/dev/null; GitHub's runner hands the step a pipe. Reproduced red
  locally with `echo poke | cargo test …`, fixed by making code match comment.
- The no-default-features leg was written as `cargo test`, which has **never
  compiled** — `KernelConfig::repl()` is localfs-gated and ~25 test files call
  it. The embarrassing part: local "verification" had piped cargo through
  `tail`, reading tail's exit code. The documented invariant is "sandbox-mode
  *compiles*", so the leg became `cargo check`; the test upgrade is GH #170.
- Run two found a third class: CI's stable (1.97) out-lints local (1.96) —
  `clippy::question_mark` got smarter and flagged a strip_prefix match in
  ignore_config.rs. Fixed the code, didn't pin the toolchain; catching
  toolchain drift is a feature, not a flake.

A kaibo (deepseek) review of the workflow pre-push contributed the fourth fix:
`.gitignore` had no `*.snap.new` entry, so the exact mistake the CI tripwire
catches could still be staged locally by `git add .`.

The doc sweep that followed (this PR) wired CI into the contributor story:
README badge + gates in "Building from Source"/"Contributing" (and the stale
"Status: 0.11" became badge-driven instead of hand-maintained), CLAUDE.md's
gates section now names ci.yml as the enforcement point and warns about
runner-vs-local clippy drift.

---

## The REPL learns to read .gitignore — and kaish-ignore learns to persist (2026-07-06)

Amy's call on #134: default to ignore-aware. The reference REPL had always run
`IgnoreConfig::none()` — zero filtering — so every interactive `glob '**/*'`
or `grep -r` walked `target/`, `.git/`, and `node_modules/` in full (it was
half of the "glob walked forever" repro behind #122). The new
`IgnoreConfig::interactive()` preset is `agent()`'s filters at **Advisory**
scope: `.gitignore` plus the default ignore list for the polite walkers,
`find` still POSIX-unrestricted, and two escape hatches — `--no-ignore` per
call, `kaish-ignore clear` per session. Bare embedded kernels keep `none()`;
the choice is per-frontend, not global.

The trap discovered while testing the opt-out: `kaish-ignore clear` printed a
cleared config and then the very next statement saw the old one. The
per-command context sync in `execute_command` copied back cwd, aliases, and
output-limit — with a comment documenting exactly this bug class for
output-limit — but never `ignore_config`, so every runtime ignore mutation
died at the end of its own statement. The documented `.kaishrc` recipe
(`kaish-ignore add .gitignore`) had been a no-op the whole time. One line in
the sync closes it, and the missing-field-in-a-manual-sync class gets another
tally mark for the "extract the ctx-sync helper" backlog entry.

---

## The latch survives its consumers (2026-07-06)

A pre-release "fishing expedition" — Amy's cross-model combo, a gemini-pro
batch over the whole scheduler/jobs/REPL surface as whole files plus a
deepseek consult over the same waters, no diff so the models read the code
cold — converged independently on the same finding: the backgrounded latch
that #96 had just made *reachable* was still *destructible*, silently, by
both of its housekeeping consumers. `jobs --cleanup` reaped a latched job
because `is_done()` counts Latched as done; `kill %N` ran cancel+remove
unconditionally. Both verified against the binary before believing the
models: "Cleaned up 1 completed job(s)", gate gone, the destructive op
permanently unconfirmable. The stored LatchRequest is the *only* handle to a
backgrounded gate — dropping the job drops the contract.

The decisions: cleanup keeps latched jobs and says so ("Kept 1 latched
job(s)…"), because a silently-retained job is just a differently-shaped
surprise. `kill %N` refuses with a pointer to `/v/jobs/N/latch` — kill is
not repurposed as the discard path, since fat-fingering kill at a gate you
meant to confirm was precisely the found failure mode. The explicit path is
`kill --discard %N`: loud about what it abandoned, conflicts with `--signal`
at the clap layer (discarding delivers nothing to anyone). Review (deepseek,
worktree checkout) mapped every other job-dropping path — fg/bg structurally
can't reach a latched job (no pid/pgid, not stopped), shutdown waits but
never removes — and flagged the one unguarded seam, `JobManager::remove`,
now documented as latch-bypassing with `cleanup()` named as the safe bulk
path. Rider from the same sweep: `Job::try_poll`'s "shouldn't happen"
Pending branch had already taken the JoinHandle and would have dropped it,
stranding the job as Running forever with its result silently lost; it puts
the handle back now.

The latch-visibility residuals the sweep also surfaced (wait --json renders
`"[1] Latched\n"` with no nonce; jobs --json rows omit the latch; scatter
rows can't carry one; mid-pipeline gates lose their exit code) were filed as
#124/#125 rather than stretched into this PR.
## `--include` learns to filter; the family goes loud (2026-07-06)

The same fishing sweep grep-trawled for siblings of #122's bug classes —
flags read off the raw ToolArgs map instead of the parsed clap struct,
`Option<String>` numerics with `parse().ok()` fallbacks — and the walk-filter
family turned out worse than the deferred nits suggested. The headline:
`glob --include` had never filtered anything. `IncludeExclude` pushed Include
rules into its list, but `should_exclude` only ever acted on Exclude matches;
the "strict mode" its own doc comment promised was never written, and the
walker only asked the exclude question. grep's `--include` merely *looked*
functional because it separately baked the single include into its walk
pattern as `**/{inc}`. On top of that, repeating `--include`/`--exclude`
silently kept only the last value (glob's help said "can be repeated"), so
`grep -r TODO . --include='*.rs' --include='*.toml'` answered with a silent
false negative — the worst shape of wrong for an agent. And the numeric
cousins all failed toward danger: `--depth=abc`/`-L abc`/`-maxdepth xyz`
walked unlimited at exit 0, `spawn timeout=abc` silently *disabled* the
timeout.

The include semantics decision: rg-like. When include rules exist a file must
match one — checked against the relative path, then the basename, first
Include/Exclude verdict wins — but a directory is never excluded by
include-miss, so traversal still reaches included files below it; excludes
keep pruning subtrees. That distinction (files strict, dirs traversable) is
the part the old two-call walker check could never express: the basename call
would have rescued `src/lib.rs` against `*.rs` only *after* the relative-path
call had already dropped it. One entry-aware `excludes_entry()` replaced the
pair, `should_exclude` was deleted rather than kept as a shim, and grep's
pattern hack went with it. Repeatables became `Vec<String>` +
`read_repeatable_strings` — the ftype pattern that was ten lines away all
along — and every numeric in the family now refuses bad and negative values
loudly. Eleven kernel-routed tests pin it, including the
include-doesn't-block-recursion case.

---

## `glob **/*.rs` stops eating its own pattern (2026-07-06)

Amy hit it live at the REPL: `glob **/*.rs` at the kaish repo root took a long
time, then printed exactly one file — `crates/kaish-client/src/embedded.rs`.
Contributing factors, in order: (1) an unquoted pattern in argv position is a
`GlobPattern` token, and the kernel's argv binder pre-expands those into
matching paths before the tool runs — correct shell semantics for `cat *.rs`,
fatal for a tool whose *input is the pattern*; (2) the `glob` builtin read
positional 0 as its pattern and silently dropped the rest, so the first
pre-expanded path (alphabetically first — kaish-client sorts first) became the
"pattern", an all-literal glob that matches exactly itself; (3) the REPL runs
`IgnoreConfig::none()`, so the bind-time walk descended all of `target/`
unfiltered — that was the "long time", paid twice because the builtin then
walked again to match the literal path. The builtin's own schema examples
teach the unquoted spelling, so every agent following the help walks into it.

The fix is a first-class seam, not a special case: `ToolSchema` grows a
`glob_passthrough` flag (sibling of `raw_argv`/`owns_output`) telling the
binder to hand bare patterns through as written; the eval fallback already
binds `Expr::GlobPattern` to its literal text (it's what `set +o glob` used).
`glob` opts in and now consumes *all* positionals as patterns — deduped union
in pattern order, strict per-pattern no-match errors that name the missing
pattern — instead of silently ignoring everything past the first. This is
also consistent with the builtin-side expansion family: cat/head/tail/ls/…
already do their own glob eval on string args via `ctx.expand_paths`, so the
tool owning pattern semantics was the established pattern; the binder just
had to stop pre-chewing glob's input. Embedder tools with pattern-shaped
inputs get the same opt-in. The REPL's unfiltered `target/` walk (slow even
when correct) stays open as a follow-up.

## Escaped quotes in `${VAR:-default}` (2026-07-06, GH #93 item 5)

Item 5 of #93's punch list: `unquote_default_word` (the function that strips
the syntactic quotes off a `${VAR:-"default"}` word before it's parsed for
interpolation) toggled its `in_single`/`in_double` state on *every* `"`/`'` it
saw, with no notion of a preceding backslash. So `${UNSET:-"hello
\"world\""}` — a double-quoted default containing an escaped inner quote —
had its second `"` prematurely close the quoted region, and the value came
out as `hello \world\` instead of `hello "world"`.

The fix needed to answer a harder question than "skip escaped quotes": what
does a *run* of backslashes immediately before a quote mean? Bash's actual
rule (verified empirically against real bash, not from memory) pairs
backslashes left-to-right — an even run collapses to half as many literal
backslashes and leaves the quote as a real, state-toggling delimiter; an odd
run does the same collapse and additionally escapes the quote (literal
character, no toggle). A naive one-token lookahead gets this wrong on 2+
backslash runs (`"a\\"` → `a\` in real bash; a lookahead that treats each
backslash independently would misjudge the second backslash as escaping the
closing quote instead of pairing with the first). The fix buffers the
contiguous backslash run and decides once it hits the terminating character.

Backslashes *not* adjacent to a quote were left untouched on purpose — real
bash also collapses `\\` inside double quotes when it's followed by an
ordinary character (verified: `"foo\\bar"` → `foo\bar` in bash), but
`unquote_default_word` never did general backslash-escape processing before
this fix (confirmed by reading `parse_interpolated_string`, its downstream
consumer, which has no backslash handling at all), and this PR is scoped to
the reported quote-toggle bug, not a full escape-processing rewrite. Recorded
as a known gap in the PR body rather than filed as a separate issue — it's a
narrow, pre-existing limitation, not a regression.

The single-quote case forced a judgment call, and the *first* answer was
wrong. The initial pass extended the same backslash-escape rule *into*
single-quoted default words "for symmetry," reasoning that real bash's
behavior there (`'hello \'world\''` is a syntax error — unterminated quote —
not an embedded apostrophe) wasn't a coherent target to match. Amy overruled
it on review, and correctly: single quotes are a *literal* region in shell,
full stop. A model relying on shell muscle memory must get **zero** surprises
inside `'…'` — zero interpolation, zero escape processing. A backslash there
is a literal byte and a `'` always closes the span; it is never escaped. The
"symmetry" argument was inventing a dialect where the whole point is fidelity
to shell's literal-region contract.

So the escape logic is gated to fire only *outside* single quotes (`if ch ==
'\\' && !in_single`). Nothing is actually lost: the shell-correct way to embed
a single quote is the `'…'\''…'` idiom — close the span, emit an **unquoted**
escaped `\'`, reopen — and that unquoted escape still works (it's the same
code path as the double-quote fix, just outside any quote). `${X:-'it'\''s'}`
→ `it's`, matching bash exactly. The one behavior that stays inside single
quotes is the pre-existing `$` → `__KAISH_ESCAPED_DOLLAR__` marking, which is
what *implements* "zero interpolation" — it isn't escape processing, it's
suppression.

The delimiter-stripping itself (`${X:-'x'}` → `x`) was double-checked and is
correct — that's the function's whole purpose, the quotes are syntax not data.
Only the escape-processing-inside-single-quotes overreach was the bug.

A third round, from a kaibo review of the revision, caught a subtler
context bug: the escape predicate treated `'` and `"` identically, gated only
on `!in_single`. So inside a *double*-quoted region a `\'` still escaped —
`${X:-"a\'b"}` came out `a'b`. Bash disagrees: inside `"…"` a `'` is an
ordinary character, and only `\"`, `\$`, `\\`, `` \` `` are escapes, so the
backslash before `'` is literal and bash yields `a\'b` (verified against real
bash). The fix is a one-line narrowing of the predicate — a `'` only counts as
escapable when *not* `in_double` — so unquoted `\'` still powers the
`'it'\''s'` idiom, double-quoted `\'` stays literal, and single-quoted regions
(already fully literal) are untouched.

Tests in `shell_compat_tests.rs`: the double-quote fix is pinned by
`default_word_double_quoted_escaped_quotes_literal`,
`default_word_escaped_backslash_before_quote` (the `"a\\"` → `a\` parity
case), `default_word_mixed_single_and_escaped_double_quotes`, and
`default_word_double_quoted_backslash_before_squote_literal` (the kaibo-caught
`"a\'b"` → `a\'b` case, confirmed red as `a'b` before the predicate fix). The
shell-literal single-quote contract is pinned by four cases:
`…_strips_delimiters` (`'x'` → `x`), `…_no_interpolation` (`'$HOME'` →
`$HOME`), `…_backslash_literal` (`'a\b'` → `a\b`, backslash NOT collapsed),
and `…_embed_idiom` (`'it'\''s'` → `it's`). All the newly-fixed cases were
confirmed failing against the pre-fix code; every case — single- and
double-quoted alike — now passes under `KAISH_BASH_COMPAT=1` against real
bash, with no recorded divergence, because the shell rule *is* bash's rule.

## Binary at the remaining text sinks, and `&>` streaming (2026-07-06, GH #93)

0.11.0 made `Value::Bytes` go loud at the three primary text sinks — string
interpolation, bare-word external argv, `echo` — via `value_to_text_sink()`.
#93 item 1 asked for the rest of the sinks that still fell through to
`value_to_string`'s infallible `[binary: N bytes]` placeholder. Rather than a
bespoke fix per sink, added `value_to_text_sink_named(value, sink)` (and a
`values_to_text_sink_named` for a whole positional list) — same guard,
parameterized so the error names the actual boundary ("a path", "an exported
environment variable value", "a redirect target") instead of the generic
"text", mirroring the `sink` parameter `structured_boundary_error` already
uses for the collection-vs-process-boundary guard.

**The five named sinks, and one the sweep turned up.** Builtin
path-positional coercion touched ~17 files (`mkdir`/`cp`/`mv`/`rm`/`touch`/
`dirname`/`cut`/`stat`/`readlink`/`realpath`/`tee`/`sort`/`find`/`grep`/`sed
-i`/`ls`) — all the same shape, so `cp`/`mv`/`tee` also got a small cleanup:
they were stringifying the same source value twice (once for a
gate-overwrite preview, again in the write loop) — computed once now. Widened
"path-positional" to cover `[[ -f $x ]]`/`test -f $x` too (kernel.rs's
`eval_test_async` and the `test` builtin's own separate file-test impl,
matching in comments) since a binary operand there silently stats a file
literally named `[binary: N bytes]` — same bug, different entry point.
Env-var export needed three call sites in sync (kernel.rs's production spawn,
its `dispatch.rs` test-only twin, and `env`'s own `execute_with_env`), all
already following the collection guard's precedent of a separate binary
check after `structured_export_error`. The redirect target
(`pipeline.rs::eval_redirect_target`) had a private, single-caller
`value_to_string` shim living in the same file — deleted once its call site
converted, rather than leaving it as dead code. Sweeping the codebase for the
"bare-word external argv" pattern (already fixed in `build_args_flat`) turned
up a fourth spawn site nobody had touched: `exec`'s own argv loop, built
straight from typed positionals with no shared helper — same class of bug,
now fixed the same way.

**The semantic ops split three ways on inspection**, not the four the issue
guessed. `${#…}` on binary was *already* correct (`value_length` special-cases
`Bytes` to the byte count before ever reaching `value_to_string` — locked in
with a regression test, no fix needed). `==`/`!=` needed a new guard arm in
`values_equal` ahead of the generic mixed-scalar fallback: `Value::Bytes`
against anything but another `Value::Bytes` is now a loud type error, not a
silent compare-the-placeholder-text. `in` needed the same treatment only for
the record-key branch of `eval_membership` (a `Value::Bytes` needle stringified
into a lookup key); the list-membership branch (`element_matches`) keeps
treating a shape mismatch as "not a match, not an abort" — consistent with how
it already treats a nested-collection element, and it's what keeps `x in
$heterogeneous_list` from dying partway through the scan. `case`-glob needed
one line in `kernel.rs`'s `Stmt::Case` handler.

**Heredoc body and record-key interpolation turned out to be a non-finding
worth writing down anyway.** The sync `Evaluator`'s `HereDocBody` and
`RecordKey::Interpolated` arms (`interpreter/eval.rs`) are the ones the issue
named, but tracing every real call site of the sync `eval_expr` showed
heredocs and record literals in an actual script always resolve through the
kernel's *async* evaluator, which already composes through the guarded
`eval_string_part_async` — the sync arms are unreachable with a live
`Value::Bytes` in production (only reachable by an embedder driving the sync
`Evaluator` directly, or by a unit test). Swapped `value_to_string` for
`value_to_text_sink` there anyway — cheap, correct, and stops the code from
leaning on a non-local invariant — but it's a defense-in-depth edit with no
behavior change, not a bug fix; said so plainly rather than overclaiming a
fourth "fixed" sink. Confirmed by literally reverting just those two lines
and rerunning the new eval.rs unit tests: both still passed.

**A test-helper trap almost hid a false pass.** The shared `assert_loud_binary`
helper (from 0.11.0) checked `err.contains("binary")` — true of a real
"cannot be used as a path" message, but *also* true of `rm`'s ordinary "No such
file or directory" once the path itself is the literal string `[binary: N
bytes]` (the placeholder text contains the word "binary"). `rm`'s new test
silently passed against the *unfixed* code for exactly that reason. Caught it
by deliberately reverting the whole PR's `src/` diff (keeping the new tests)
and rerunning — 11 of the 21 new/touched tests failed as expected, but `rm`'s
didn't. Tightened the helper to `contains("cannot be used as")` — the
consistent wording all these guards now share (reworded `values_equal`'s
message to fit) — and added a stderr placeholder-leak check alongside the
existing stdout one. Reran the full revert to confirm: all 11 fail pre-fix,
all pass post-fix.

**Item 6 (`&>` streaming) is an honest equivalence test, not a failing one.**
`RedirectKind::Both` now uses `take_output_for_stream`/`write_canonical`
like `>`/`>>`, instead of forcing structured output through a full
`to_canonical_string()` `String` first — a memory-copy optimization, not an
output-correctness fix, so the file bytes are identical either way and a
test can't observe the difference by content alone. Wrote it up front as
that: byte-for-byte equivalence + a large-table completeness check, in
`pipeline.rs`'s own `apply_redirects` test module (unit-level, not
`kernel.execute()` — there's no builtin under test, just the redirect
machinery, and the existing merge-redirect tests already use this pattern).
Verified honestly by reverting just that hunk: same tests, same pass. Applied
the identical streaming swap to the inter-stage pipe write next to it
(`run_pipeline`) since it shares the exact `out_bytes()`/
`text_out().into_owned().into_bytes()` shape — not `&>`-specific, but the
same anti-pattern sitting right next to the one the issue named.

**Deferred, tracked in #116, not fixed here:** a `WordAssign`→positional/named
reconstruction cluster (`dd if=$BIN`, `awk -v a=$BIN`, `cat foo=$BIN`) across
four call sites in `kernel.rs`/`pipeline.rs` that embeds a value into a
`key=value` string the same lossy way — real, but a distinct code shape from
the five named sinks, and out of scope for one PR. Also verified in passing
that #93's two SUSPECTED findings (S3/S4: binary crossing into `fromjson`/
`fromjsonl`) are already handled — both already reject a binary positional
loudly and route stdin through the shared `read_stdin_to_text`, which already
errors on invalid UTF-8.

**A kaibo review of the diff (deepseek, whole-file, not just the patch) found
a second wave the grep-for-`value_to_string` sweep missed entirely**, because
it's a different anti-pattern: `ExecContext::expand_paths` (the shared path
list builder behind `cat`/`head`/`tail`/`wc`/`checksum`/`file`/`base64_tool`/
`tac`/`xxd`) matched non-string positionals with `_ => continue` — a
`Value::Bytes` operand didn't get stringified into a placeholder, it just
*vanished* from the list. Worse than the placeholder bug: every one of those
callers falls back to reading stdin when the path list comes back empty, so
`head $BIN` (say) silently read whatever was piped in instead of erroring on
the binary path — confirmed with a test that pipes distinguishable stdin
content through and asserts it never appears in the output. Same shape at
`cd` (`get_string`'s silent `None` → falls back to `$HOME`) and `awk`'s input
operand (→ falls back to stdin); `basename`/`diff` already failed loudly on
`None`, just with a generic "missing" message, fixed for consistency.
`get_string` itself lives in the `kaish-types` leaf crate with no
`EvalError` machinery to explain *why* it saw nothing, so the fix is a
`get_path_string` helper in `tools/builtin/mod.rs` that checks for
`Value::Bytes` before falling through to `get_string`, used at each of the
four call sites — not a change to `get_string`'s own signature, which is used
far too broadly (for non-path args too) to safely touch here. Same
revert-and-confirm discipline as the rest of this PR: all 7 new tests fail
against the pre-fix code, pass after.

**A second kaibo pass over the same `get_path_string`/`expand_paths` fix
found 10 more builtins with the identical `get_string` shape** — `sed`
(streaming-mode input) and `uniq` silently fell back to stdin exactly like
`awk`; `jq`'s `path` positional the same; `tree` silently used `.`; `write`/
`ln`/`patch`/`validate`/`checksum`/`spawn` already failed loudly on `None`
(just with a generic "missing" message), converted for consistency. All
mechanical once `get_path_string` existed: swap `args.get_string` for it, add
the `Err` arm.

**Two of those ten (`checksum --check=$BIN`, `patch --file=$BIN`) exposed a
second, deeper bug while writing their tests — the tests kept passing against
unfixed code, for the wrong reason.** Both builtins check their own
clap-parsed field (`parsed.check`/`parsed.file`) *before* falling back to
`args.get_string`, and clap's field is populated from `ToolArgs::to_argv()` —
which stringifies `Value::Bytes` into the *exact same* `[binary: N bytes]`
placeholder via its own `value_to_argv_token`, a separate, pre-existing,
explicitly-commented-as-deferred gap in the `kaish-types` leaf crate. So
`parsed.check` was never `None` for a binary `--check=$BIN` — it was
`Some("[binary: N bytes]")`, and the `get_path_string` fallback (only reached
on `None`) never ran. Fixed by reordering: check the untouched `ToolArgs`
value first (via `get_path_string`), fall back to the clap field only when
genuinely absent — correct for both the binary case and the ordinary one
(the raw `ToolArgs` map already has whatever clap would have parsed, from
before `to_argv()` ever ran). Audited the rest of the `parsed.foo.clone()
.or_else(|| args.get_string(...))` sites in the codebase for the same
ordering hazard — none of the others (`algo`, `template`, `encoding`,
`separator`, `field_separator`, …) are path-typed, so out of *this* PR's
reach, but the root cause (`to_argv()`/`value_to_argv_token`) is filed as
#120 rather than silently left for the next person to rediscover the hard
way. Every genuinely-fixed builtin (11 new tests this round) verified
fail-first the same way as the rest of the PR.

**A third pass (gemini, asked specifically to sweep for the same ordering
hazard in every builtin touched so far) found one more: `cmp.rs`** read its
two file operands off `parsed.paths` (the clap-parsed field) instead of
`args.positional` — the exact CLAUDE.md gotcha ("read `Value`-typed
positionals off `args.positional`, not the clap struct") that the other
path-positional builtins already followed correctly. Fixed the same way as
`mkdir`/`cp`/etc.: `values_to_text_sink_named(&args.positional, "a path")`.
Three review passes, three distinct bug shapes (`_ => continue` drop,
clap-field-checked-before-raw-value ordering, clap-field-read-directly) —
each caught because the review asked "is this ACTUALLY the shape you think
it is" against the live code rather than trusting the pattern to have been
applied uniformly. Stopped the loop here rather than keep sweeping
indefinitely; #116/#120 carry the remaining known-adjacent gaps forward.

## The README learns to face the front door (2026-07-06)

Amy asked for a first-time-visitor evaluation of README.md ahead of a rewrite.
Two stateless outside reads (deepseek and gemini-flash via kaibo `oneshot`,
given only the README — exactly what a visitor sees) converged with our own
read almost point-for-point: the project thesis ("Why a shell for agents?")
was buried as an `####` heading inside Components → Embedding at 85% document
depth; the doc oscillated between agent-runtime and daily-driver-shell
identities section by section; and reference-grade material (exit-code table,
latch/trash mechanics, embedder lifecycle) clogged the middle. The accuracy
pass found worse: the very first instruction was broken — `cargo install
kaish` names a crate that doesn't exist (the binary ships in `kaish-repl`) —
and "still experimental" contradicted the settled-language stance.

Decisions:

- **Sequence, don't interleave.** The rewrite front-loads the universal pitch
  (hero with the "skills transfer" line both reviewers independently named the
  strongest sentence in the file, thesis, differences-from-bash), keeps the
  Quick Tour, then branches Getting Started into the two real journeys: REPL
  and embedding. Six crates ship this README as their crates.io page, so the
  embedder content stays — sequenced, not braided.
- **Reference material moved to its homes.** Exit-code table + output contract
  + fresh-kernel-per-request lifecycle → EMBEDDING.md (new "result contract"
  section; its stale "0.8.x" fixed too). Trash thresholds/exclusions →
  LANGUAGE.md — they had no home outside the README.
- **Everything shown is verified.** Every tour example ran against the current
  binary (including the quote-to-join parse error); the embed example was
  compiled and run from a scratch crate against the worktree kernel. The tour
  now shows 0.11 typed collections (`fromjson` + `${r[k]}`) instead of jq —
  Amy may spin jq out to its own crate, so the README stops showcasing it
  (it stays in the builtin inventory table, which tracks code).

## `jq -s` stops being a no-op on the `.data` path (2026-07-06, GH #93 item 2)

Real `jq -s`/`--slurp` has one law: wrap the inputs in an array, always —
even a single document (`printf '{"a":1}' | jq -s length` is `1`, the array
length, not the object's key count). kaish's `jq` already got that right on
the text path (GH #80 landed real slurp framing there). But the structured
`.data` shortcut — the fast path used when an upstream stage like `fromjson`
or scatter/gather already handed over a parsed value instead of raw stdin
text — treated `-s` as a no-op, reasoning that "the pipeline is already
slurped." That reasoning doesn't hold: `.data` carries exactly *one*
document, the same as reading one document off stdin, and real jq wraps a
single document too. The divergence was silent and easy to miss because it
only shows up on scalar/record `.data` (an array `.data` piped through
`-s length` gives a plausible-looking wrong answer instead of an error).

The fix is a three-line change in `resolve_stdin_json`: wrap the `.data`
value in a one-element JSON array when `slurp` is set, same as the text path
already does. `docs/LANGUAGE.md`'s slurp section and the `JqArgs::slurp` doc
comment both asserted the old no-op behavior as intentional — both corrected
to describe the wrap. TDD: the existing `jq_slurp_is_a_noop_on_the_data_path`
test encoded the old (wrong) behavior as a passing assertion; it now fails
red against the fix and was rewritten in place as
`jq_slurp_wraps_the_data_path_value_in_an_array_of_one` plus two new
guard tests — one confirming an array-shaped `.data` value still gets one
more layer of wrapping (not passed through as "already an array"), and one
confirming plain `jq` (no `-s`) on the `.data` path is untouched. Full suite
and `clippy --all-targets` both clean.

## Hardening the job/tool-result seam (2026-07-06, GH #93 items 3, 4)

`kaish-types` had adopted `#[non_exhaustive]` broadly — `ExecResult`,
`OutputData`, `ToolSchema`, `ToolArgs`, `WriteMode`, `BackendError`,
`CommandKind`, `DirEntryKind` — but `job.rs` was missed. That gap had just bitten
an embedder for real: #96 added `JobStatus::Latched` and `JobInfo.latch` and had
to call it out as **BREAKING (embedders)** in the changelog, because a bare
`JobStatus`/`JobInfo` gave downstream `match`es and struct literals no forward
compatibility. Closing the gap now (`JobStatus`, `JobInfo`, `ToolResult`) means
the *next* variant/field addition is additive, not breaking — and softens that
existing #96 bullet from `**BREAKING (embedders):**` to a plain `Embedders:`
heads-up, matching the calmer framing Amy asked for (bug-fix-shaped changes
toward correct behavior, first-party/coordinated dependents, don't cry wolf).

`JobInfo` got a `new(id, command, status)` constructor plus
`.with_output_file()`/`.with_pid()`/`.with_latch()`, mirroring the existing
kaish-types builder idiom. `ToolResult` already had `success`/`failure`/
`with_data`; it picked up `.with_output()`/`.with_content_type()`/
`.with_baggage()`/`.with_latch()`/`.with_did_spill()`/`.with_original_code()` so
every field has a construction path across the crate boundary. `JobStatus`
needed no call-site changes at all — its variants are all unit variants, so
external construction was never blocked by `#[non_exhaustive]`; only its
(nonexistent, it turns out) external exhaustive matches would have needed a `_`
arm. Letting the compiler drive turned up exactly three struct-literal sites to
fix: two `JobInfo { .. }` in `scheduler/job.rs` and one test-only
`ToolResult { .. }` in `kernel.rs` — `cargo build --all-targets` was clean
immediately after, which is as much confirmation as this kind of attribute
change gets.

**Item 3** was a quieter bug found by reading `ExecResult` and `ToolResult`
side by side: `ExecResult.did_spill`/`.original_code` (the output-limiter's
"this got capped, and here's the code before the remap" signal) had no
`ToolResult` counterpart, so a backend-registered tool's (kaijutsu, an MCP
engine) capped result silently looked uncapped by the time it crossed back into
the kernel — in *both* `From` directions. Added both fields, propagated them
in both impls, and pinned it with round-trip tests in each direction.

**Item 4** turned out bigger than "align a test double." The test-only
`BackendDispatcher::dispatch` (used by `scheduler/pipeline.rs`'s unit tests, not
by any real embedder path) hand-rolled the `ToolResult → ExecResult` conversion
instead of calling `From<ToolResult> for ExecResult` — the exact function the
production dispatch path in `kernel.rs` uses. The hand-rolled version wrapped
`data` unconditionally as `Value::Json(json_data)`, which happens to coincide
with the real conversion for object/array-shaped data (both stay `Value::Json`)
but silently diverges for *scalar* `data` — the production path unwraps a plain
JSON number/bool/string into the matching native `Value` variant via
`json_to_value_no_envelope`; the old test-dispatcher path never did. It also
never touched `did_spill`/`original_code` at all, so item 3's new fields would
have been silently dropped on this path even after being added to `ToolResult`.
TDD caught both: wrote a scalar-unwrap test and a did_spill/original_code test,
confirmed each genuinely failed by temporarily reverting the dispatch.rs fix
and rerunning (red), then swapped the hand-rolled block for
`ExecResult::from(tool_result)` (green). A third test pins that envelope-shaped
`data` stays structured (`Value::Json`, never auto-decoded to `Value::Bytes`) —
it passes either way today, since neither path decoded envelopes, but it's
worth keeping now that both paths share one conversion function: any future
regression that adds envelope-decoding to the shared `From` impl trips it on
both the production and test-dispatcher side at once. No `CHANGELOG.md` entry
for item 4 — it's test-only internal churn with no embedder-visible effect.

## Interpreter allocation/stack pass (2026-07-05, GH #48)

#46/#47 landed a recursion depth guard sized against a measured ~380 KB of
native stack *per statement-engine re-entry level* — the figure that forced
`RECOMMENDED_STACK_SIZE` to 16 MiB. #48 is the pass to make that cheaper. A
model panel (gemini-pro + fable, whole hot files, no diff) had converged on a
ranked burndown, posted on the issue: profile fix first, then a batch of
mechanical boxing, then two wider items to measure-and-decide.

**The measurement had a trap.** The obvious tool, `-Zprint-type-sizes`, reports
the *coroutine layout* (future struct size), which is computed at MIR level and
is invariant to optimization. So it's a fine proxy for the boxing items (they
shrink the struct) but completely blind to the profile change and to
codegen-level native-stack cost — the thing #46/#47 actually cares about. Proof:
the debug and `opt-level=1` type-size logs were byte-identical. So the first real
deliverable was a *runtime* probe: a `stackprobe` builtin that steps into a
`#[inline(never)]` sync frame (the async body is heap-boxed by `async_trait`, so
a local there isn't the native stack) to read the true stack pointer at each
`$(…)` nesting level. Pure builtins never yield, so a nest runs in one
synchronous poll and adjacent probes differ by exactly one re-entry level. It
reads dead-consistent (min=max=median across 25 samples) — the recursion is
perfectly self-similar — and it became the metric every item was measured
against, plus a durable regression guard.

**The batch, each step measured (median per-`$()`-level, `opt-level=1`):**
- *Item 0* — `opt-level = 1` on the two interpreter crates: 414 → 106 KB debug
  (**3.9×**), nearly closing the debug↔release gap. fable's lead insight —
  unoptimized async poll frames are ~proportional to future size with redundant
  memcpys — landed exactly.
- *Item 1* (box the cold dispatch branches + the two `execute_pipeline` calls):
  106 → 96.
- *Item 3* (drop the per-command tracing spans off the ring): 96 → 76 — the
  `Instrumented<Span>` wrapper was heavy on the big ring futures.
- *Item 4* (box the command-subst scope snapshot): 78.7 → 77.8 KB.
- *Item 2* (box the two per-command `ExecContext` snapshots via a sync helper,
  collapsing two near-duplicate 30-field blocks into one): 77.8 → 74.6.
- *Item 7* (drop the `ToolSchema` before the execute await): no-op under
  optimization (the compiler already narrows it), kept for debug + clarity — and
  reported honestly as such.
- *Item 8* (`tool_schemas: Vec → Arc<[…]>`): an *allocation* win (the ~70-schema
  catalog was deep-cloned per dispatch → refcount bump), invisible to the stack
  probe, so validated by construction.
- *Item 5* (box `ExecResult.output` and `Scope.last_result`) — **the headline:**
  74.6 → 56.8. `ExecResult` is the most-replicated type in the recursion frame
  (every `ControlFlow`, every return, the accumulator), so boxing its 120-byte
  `output` cascaded ~10× per level for a single 14 KB/level drop.

**Result: ~46% off the per-level native stack** — release 92 → 50 KB, debug
opt=1 106 → 57 KB, debug no-opt 414 → 193 KB.

**Two decisions were Amy's.** The wider items 6 (box `Value::Json`) and 9
(Arc-split `ExecContext`) turned out poor value once re-measured: `Value` is only
72 B and no longer dominant, and item 9's benefit largely evaporated — item 2
already boxed `ExecContext` off the stack and item 8 already Arc'd its expensive
clone, leaving a large invasive refactor for a small allocation gain. Deferred
both (measure-first follow-ups on #48). And with the frames this much smaller,
the #46/#47 pair was relaxed: `MAX_RECURSION_DEPTH` 32 → 48 and
`RECOMMENDED_STACK_SIZE` 16 → 12 MiB, adjusted *together* with a comment making
the relationship explicit — the cap must trip before `cap × (worst-case
per-level) < floor`, and the worst case is deliberately the ~193 KB *unoptimized*
figure because the new `opt-level=1` dev profile is local to this workspace and
does **not** propagate to embedders, whose own debug builds pay the full cost.
`48 × 193 KB ≈ 9.3 MB` under 12 MiB keeps the same ~1.3× margin the old pair had.

kaibo (deepseek, holistic, pointed at the worktree) reviewed all six change
classes clean — no drop-order/lock-lifetime issue from the `Box::pin`s, faithful
field parity in the snapshot helper, correct accessor boxing, identical serde
wire format, sound `Arc` sharing, and adequate margin math. One flagged
"output_limit sync" was pre-existing code it noticed while reading, not part of
the change.

## Surfacing the backgrounded confirmation latch (2026-07-05, GH #96)

The confirmation latch (`set -o latch`) went first-class in #92, but a gap
survived: background a gated op — `rm precious.txt &` — and the latch is stored
in the job's result, but *every* consumer was blind to it. `wait` reported
`Failed`; `jobs`, `JobInfo`, and `/v/jobs/{id}` had no latch anywhere;
`Job::status()` folded the exit-2-latch into `Failed`, indistinguishable from a
real error. The op stayed safely *blocked* (no data loss), but the nonce was
unreachable, so a backgrounded gate could never be *fulfilled*. Low-stakes, but
a real dead-end in the safety story.

The fix threads the stored `LatchRequest` out to all four surfaces named in the
issue, each with one job: `JobStatus::Latched` (the state is *held*, not
errored, so `jobs` and `/v/jobs/{id}/status` say so distinctly);
`JobInfo.latch: Option<LatchRequest>` (the programmatic surface an embedder
reads from `JobManager::list`/`get`); `wait` surfacing the request on its
result's control-plane `.latch` field with exit 2, mirroring a foreground gate
so `latch_request()`/`Kernel::confirm` work identically; and a new
`/v/jobs/{id}/latch` VFS node rendering the request as JSON (nonce, command,
paths, hint) — empty body when the job isn't gated, so a reader reads-then-parses.

TDD, and the capstone test earns its keep: background a gated `rm`, `wait 1` to
surface the latch, `Kernel::confirm(&latch)`, assert the file is gone — the
whole loop, from backgrounded gate to fulfillment, in one test. Two design
touches worth noting: `wait` on several gated jobs keeps the *first* latch
(`.latch` holds one; waiting on multiple gated jobs is an odd pattern), and the
VFS node stays plain-text-empty rather than error when not latched, matching how
`status`/`command` always read. Three kernel-routed tests red→green, 4377 suite
+ clippy `--all-targets` clean, verified end-to-end through the REPL.

## The recursion guard, and why #46 and #47 are one PR (2026-07-05, GH #46/#47)

The plan was a tidy correctness fix: thread a depth counter through the three
dynamic re-entry points (command substitution, shell functions, `.kai`
scripts), return a loud error past a cap, done. The counter + RAII guard part
*was* tidy (an `AtomicUsize` on the Kernel, fresh per fork, decremented on drop
so cancellation stays balanced). Picking the **cap** is where physics showed up.

We measured the native stack cost per recursion level by disabling the cap and
probing depth-at-overflow: **~380 KB/level in debug, ~80 KB/level in release**
(the fat boxed async futures #48 is about). That means:

| thread | debug | release |
|---|---|---|
| 8 MB main | ~21 levels | ~100 |
| 2 MB tokio worker | ~5 | ~25 |

The plan had been "defer #47 (stack size) to a doc note, ship #46 alone." The
measurement killed it: with the default stacks, **no single cap works** — one
safe on a 2 MB worker (~4) is uselessly shallow, and a useful cap (~16) doesn't
protect workers *or the test itself* (a `#[tokio::test]` thread is ~2 MB, so a
recursion test would SIGSEGV at ~5 before it could assert the loud error). #46
needs #47's controlled stack to be effective *and* testable. Amy called it:
ship them together.

So the guard is tuned for a **documented floor**: `RECOMMENDED_STACK_SIZE`
(16 MiB) and `MAX_RECURSION_DEPTH` (32), both `pub` so embedders can size their
runtime against them. kaish can't set the stack itself (it doesn't own the
runtime), so the REPL walks the talk — `thread_stack_size` on its tokio workers,
and a `std::thread` with the recommended stack driving `block_on` (the OS main
thread's ~8 MB overflows a deep debug recursion before the cap). The tests run
each recursion on a `RECOMMENDED_STACK_SIZE` thread with a fresh current-thread
runtime — the only honest way to reach the cap without the overflow-under-test
taking down the binary. Verified end-to-end: `f(){f;};f`, mutual recursion, and
`$()`-nested recursion all go loud (debug and release), foreground and inside a
pipeline stage; `countdown 20` still runs.

One consistency note banked: a recursion error *inside* `$(...)` follows kaish's
existing command-substitution semantics — a failed `$()` yields an empty
expansion and doesn't fail the enclosing command (same as `echo $(false)`), so
that path is *bounded* (no crash) but not exit-code-loud. Direct recursion
(functions, scripts) is exit-1-loud. Not a new silent path — it matches how
`$()` already behaves; making `$()` failures louder is a separate concern.

Meanwhile fired gemini-pro + fable batches at the kernel/types source to hunt
per-level stack reductions (#48 territory) — shrink the ~380 KB and the floor
can drop. That's a follow-up; this PR ships the guard + the floor it needs.

## `$()` in a redirect target — the bug that only bit embedders (2026-07-05, GH #90)

Picked this up expecting a quick "attach the dispatcher for bare commands" fix.
The first surprise: the exact repro (`echo x > $(echo f.txt)`) **worked** through
the REPL. The `Stmt::Command` fast path was removed months ago ("This is the
single execution path — no fast path for single commands"), so bare commands run
through `execute_pipeline`, which attaches `ctx.dispatcher`. The GH #90 comment
in `redirect_in_cmdsubst_tests.rs` describing it as blocked was stale — or so it
looked. I nearly closed it as already-fixed with a pinning test.

The pinning test is what saved it. Written through `kernel.execute()` (the
embedder API, not the REPL), all three cases went **red**: `eval_redirect_target`
fell back to the sync evaluator and failed with "could not evaluate redirect
target". The REPL worked; `kernel.execute()` didn't. The tell: `ctx.dispatcher`
is only populated by `self.dispatcher()`, which upgrades a `self_weak` that's set
**only in `into_arc`**. The REPL Arc-attaches its kernel; a bare `Kernel` from
`Kernel::new()` — every embedder holding a `Kernel` by value, and the entire
4000-test harness — never sets `self_weak`, so `dispatcher()` returns `None` and
a `$()` redirect target silently degrades. #90 was real, and it bit *exactly the
surface that matters* (kaibo/kaijutsu drive `kernel.execute`), while hiding from
the interactive shell we test by hand.

The fix follows the constraint: without an `Arc<Kernel>` there is no
`Arc<dyn CommandDispatcher>` to put in `ctx.dispatcher`, so the owned-handle
approach can't work for a bare kernel. But the *runner* always holds a real
`&dyn CommandDispatcher` (the kernel itself, Arc'd or not). So thread it: through
`apply_redirects` and `setup_stdin_redirects` into `eval_redirect_target`, which
now calls `dispatcher.eval_expr` directly and no longer reads `ctx.dispatcher` or
falls back to the sync evaluator. The silent fallback — the thing that turned a
missing dispatcher into a wrong answer instead of a loud error — is deleted
outright. Bonus coverage: a pipeline-stage target (`echo x | cat > $(echo g)`)
had the same gap (the stage context copies `dispatcher: None` too) and is now
fixed and pinned. Four kernel-routed tests, red→green; 4374 suite + clippy clean.

Lesson banked: **test the embedder path, not the REPL.** A convenience wrapper
that Arc-attaches can paper over a defect on the API every real consumer uses.

## `grep -r` searches a file operand instead of silently missing (2026-07-05, GH #105)

The reflex `grep -r PATTERN FILE` — `-r` is muscle memory, and everyone assumes
handing a file to a recursive search searches *that file* — returned zero matches,
exit 1, and **no error**. The recursive branch unconditionally treated the operand
as a walk root and enumerated it with a files-only `**/*`; a directory yields its
files, a plain file has nothing "under" it, so the walk collected nothing. The
worst failure mode: silent. A model reads the empty result as "not found" and
quietly corrupts whatever reasoning follows — bad enough that kaibo carries a note
in its shell preamble steering models around it, spending instruction tokens every
session.

The fix reframes what `-r` *means*: it governs how **directories** expand, nothing
more. The recursive branch now partitions its operands by `stat` — files are
searched directly (falling through to the ordinary file-operand path, so a lone
file prints unprefixed like plain `grep -c`), directories are walked, and a mixed
`grep -r p file dir` does both. A stat-unresolvable operand still falls to the
walker, preserving pre-fix behavior for a bad root. Display is conservative: the
sole-directory walk strips its root exactly as before (byte-for-byte unchanged
output for the overwhelmingly common `grep -r p dir`); only the genuinely-new
mixed/multi-source shapes switch to cwd-relative prefixes so each source shows
under its own subpath. TDD: four kernel-routed regression tests (single file, `-c`
count, `-R` alias, mixed file+dir) red first, then green; 4370-test suite and
clippy `--all-targets` clean. Once this ships, kaibo's preamble note can go.

## The `test` builtin — `[[` semantics as a command (2026-07-04)

`test` had been quietly shelling out to the host `/usr/bin/test`: OS-dependent,
not VFS-aware, and command-not-found in a no-subprocess build. Amy asked whether
we could add a `test` that follows kaish's own `[[` semantics and whether that
would be safe for muscle memory. A 4-model kaibo panel (deepseek/gemini/or-gpt/
or-kimi, stateless) converged hard: yes — but a flag-form-only shim is
*not credible*, because `test a = b` is the canonical idiom, and without it the
thing would parse-error on the one form everyone reaches for. Full scope it was.

**Two parts, and the second was the surprise.** Part one was grammar relief: kaish
lexes `=`/`==`/`!=`/`!` as shell-significant tokens, so `test a = b` /
`test ! -f x` parse-errored before reaching a command. The fix is a small,
name-agnostic arg-parser production making those four operators literal positional
words (angle brackets `< > <= >=` deliberately excluded — they stay redirection).
A side effect, bash-consistent: spaced `cmd key = value` now parses as a three-arg
command instead of an error (glued `key=value` stays an assignment).

Part two — the builtin — turned out to be *not* about the predicate logic (that
reuses `[[`'s engine verbatim), but about **argv shape**. POSIX `test` is
position-sensitive (`test $x = -n`, `test 0 -gt -5`), while kaish's `ToolArgs`
splits flags into an unordered set — so an operand that merely *looks* like a flag
gets silently mis-routed (proof: `echo a -n b` prints `a b`). Amy pushed on
whether the lexer could just preserve order always; the honest trace showed order
dies in the *binder*, not the lexer, and that faithful source order is a distinct
binding mode incompatible with the flag-value-consumption logic (`grep -A 3`
consumes its `3`). So: a first-class opt-in `ToolSchema.raw_argv` — the binder
binds every arg to `positional` in source order with `Value` types preserved.
`test` opts in; nothing else pays. (A first-class field, not a reconstruct-and-
shape-check — the same principle as the latch below.)

**One decision reversed on the merits.** The panel wanted integer-strict numerics
(POSIX errors on `1.5 -eq 1`). Amy pointed out that kaish numbers *are* JSON
numbers and `[[` already compares floats via this exact `numeric_compare` —
rejecting `1.5` in `test` but not `[[` would be the odd one out. She was right,
and it *simplified* the code: `test` numeric == `[[` numeric, verbatim, no
integer-strict wrapper. Still loud on non-numeric/collection/NaN.

Predictable-by-design divergences from POSIX `test`, all deliberate: no word
splitting; `-a`/`-o`/`( )` rejected loudly (chain with shell `&&`/`||` or use
`[[ ]]`); no arg-count magic (an operator missing its operand is loud, not a
surprise-true); negation is a single parity-collapsing leading `!`. Adding the
builtin retired the `PosixTestCommand` validator advisory (W006) that used to
steer `test` toward `[[` — it's a real command now.

**Sequencing.** Built on a worktree parked while #94 (hardening) and #92/#97
(latch) landed — the `raw_argv` work touches the same dispatch seam #92 reworks,
so it went *after*, beside `current_invocation`, not in conflict. Then rebased
across #99 (chumsky alpha.8 → 0.13); the parser change compiled clean on the new
combinator surface.

## The confirmation latch becomes a first-class, fulfillable API (2026-07-04)

Fell out of the redirect-in-`$()` work above. That change had to special-case
`clear_stdout` so a stdout redirect wouldn't clear the `rm` latch nonce — because
the nonce rode inside `ExecResult.data` as serialized JSON, overloading the
data-plane `.data` (structured stdout) with a control-plane signal. Amy's call:
don't slime the latch through another feature's field with a runtime shape-check
— give it a first-class typed home.

**The field.** `ExecResult.latch: Option<Box<LatchRequest>>`. `latch_result`
sets it typed (no serialize round-trip); `latch_request()` reads it; `clear_stdout`
drops the discriminator and clears `.data` unconditionally. Threaded through the
`ExecResult`↔`ToolResult` backend roundtrip (which #94 had just converted to
symmetric `From` impls, fixing the old #84 field-drop) and through
`accumulate_result` — a single `rm x` statement flows through accumulation, so
forgetting `.latch` there lost every gate (a test caught it). `--json` surfaces
it under a dedicated `latch` key, never folded into `data`.

**Then Amy pushed on fulfillment.** Inspection was first-class (`latch_request()`),
but *fulfilling* a latch meant re-running with `--confirm=<nonce>` — via the
`hint` string, which is `format!`-built and doesn't quote paths (a space breaks
it), or by manually rebuilding argv. She wanted the latch to hold the exact state
for a precise replay. So: capture the exact argv at the dispatch seam
(`(dispatch_name, ToolArgs::to_argv())`) into `LatchRequest.tool`/`.argv`, and add
`Kernel::confirm(&LatchRequest)` that replays `execute_argv(tool, argv)` with
`--confirm` **prepended** (appending would let `to_argv`'s trailing `--`
terminator swallow the flag). A path with a space now round-trips exactly where
the hint can't — the payoff, pinned by a test. The seam capture is gated on
`latch_enabled` so it's free when the latch is off.

**Two stack-size lessons, the hard way.** `ExecContext` and `ExecResult` are both
rebuilt/returned at every level of deep `$()` recursion. Adding an inline
`(String, Vec<String>)` to `ExecContext`, and growing the inline `LatchRequest`
inside `ExecResult` by two fields, fattened every frame enough to overflow the
stack on `deeply_nested_command_substitution` — but only under `cargo test --all`
(parallel binaries, tighter thread stacks; it passed in isolation). Boxing both
(`Box<(String, Vec<String>)>`, `Option<Box<LatchRequest>>`) kept the hot structs
lean and fixed it. Lesson: a ~150-byte field on a stack-hot, deeply-recursive
struct wants a box (see GH #46/#47). Docs (EMBEDDING.md, LANGUAGE.md, README,
the syntax fragment) reworked to teach the typed field + `confirm`.

## Redirects inside `$()`, and what that taught us about `.data` (2026-07-04)

Started from a curiosity: a test comment said "kaish's grammar doesn't accept a
redirect inside `$(...)`." It turned out to be a one-line gap — `cmd_subst_parser`
hardcoded `redirects: vec![]` and never called `redirect_parser` — but wiring it
up surfaced two deeper things.

**The parser cycle.** Naively calling `redirect_parser()` from the cmd-subst body
stack-overflowed at *parse* time: `redirect_parser` built `primary_expr_parser()`
for its target, `primary_expr_parser` builds a `cmd_subst_parser`, which now calls
`redirect_parser` again — an unbounded *construction* cycle. Fix: parameterize
`redirect_parser` on its target parser. The top level passes a fresh
`primary_expr_parser()`; the cmd-subst body passes its already-recursive `expr`
handle, so the nested target flows through chumsky's existing `recursive` wrapper
instead of constructing a new parser at each depth. `$(cmd > $(subst))` now parses.

**What a stdout redirect means for `.data`.** The interesting part. `$()` capture
prefers a result's structured `.data` over its text, so `x=$(seq 1 3 > file)` was
capturing `[1,2,3]` instead of bash's `""` — the redirect sent the text to the
file but left `.data` intact. We decided `.data` is *the structured view of
stdout* (every builtin that sets it — seq/jq/fromjson/keys/find — does so as the
typed form of what it wrote to stdout), so a stdout redirect must take `.data`
with it. New `ExecResult::clear_stdout()` clears `.out`/`.output`/`.data` together;
the three file-redirect arms and (caught in review) the `1>&2` merge arm all route
through it. This also kills a pipe-sideband leak (`cmd > file | consumer`) and
makes `for x in $(cmd > file)` correctly iterate zero times.

**Two things this disturbed.** First, four `kaish-last` tests broke — they used
`producer > /dev/null; kaish-last` where the `> /dev/null` was a *harness trick*
to silence the producer's stdout in the shared capture stream (the line-22 comment
said so), and leaned on `.data` surviving that redirect. That reliance was an
accident of the original `.data` rollout, not a contract; rewrote them to isolate
kaish-last's output by its last line, no redirect. Second, and more important:
`.data` is *overloaded* — for `rm` under `set -o latch` it carries the
confirmation nonce, a control-plane signal, not stdout. Blanket-clearing `.data`
silently disabled the safety gate on `rm precious > log` (a TDD guard caught it).
The scoped fix: `clear_stdout` preserves `.data` when `latch_request()` matches
(exit-2 + the exact `LatchRequest` envelope). Amy's call: that overloading is a
real smell, but the latch deserves a first-class typed public-API field of its
own rather than being refactored as a rider on this change — deferred to its own
worktree so neither feature's boundary gets weakened.

Cross-family review (DeepSeek + Gemini via kaibo, whole files, no diff) converged
independently on the one bug this text glosses: the `1>&2` arm cleared only
`.out`, leaking `.data`. Fixed, with a guard test. Review also surfaced GH #90 —
`$()` in a redirect *target* fails for a bare single command (dispatcher not
attached) — a pre-existing bug, orthogonal, filed not fixed here.

## scatter/gather's own flag values could silently drop a bad subscript (2026-07-03)

A 0.11.0 pre-release punch-list item: `scatter`/`gather` bind their OWN flag
values (`--as`, `--limit`, `--timeout`) through a *sync* twin of the real
argument binder — `build_tool_args`/`eval_simple_expr` in
`scheduler/pipeline.rs` — because `run_scatter_gather` parses those flags once,
before any worker forks, and can't recurse back through the async
`PipelineRunner::run` → `Kernel::dispatch` chain to get there. Every real
command's arguments bind through the async `build_args_async` (`kernel.rs`),
which already surfaces a `PathError` (missing key, shape mismatch, undefined
subscripted root) loudly — matching the other three primary eval sites
(assignment, `$(( ))`, `"${…}"`). The sync twin never got that treatment: every
arm discarded the error via `.ok()` / `if let Ok(..)`, so `scatter --as
${u[nope]}` silently fell back to treating `--as` as a bare boolean flag
(dropping the intended value and stranding the bad expression as an unused
positional) instead of failing, and `${#u[tags]}` on a bad subscript inside an
interpolated flag value silently omitted the length instead of erroring.

Confirmed `build_tool_args` has exactly one production call site —
`run_scatter_gather`'s two calls parsing the `scatter`/`gather` commands'
own args — everywhere else it's `#[cfg(test)]` (`BackendDispatcher`) or the
~19 unit tests inside `pipeline.rs` itself. Gave `eval_simple_expr` and
`eval_string_parts_sync` a real `Result` error channel: `Ok(Some(value))` on
success, `Ok(None)` unchanged for "not representable in this reduced sync
context" (binary ops, command substitution — still the documented, deferred
"eliminate the sync twin" gap), and `Err(msg)` for a genuine `PathError`
(absence/shape), propagated with the same message text the async path uses.
Also noticed `eval_simple_expr`'s top-level match had no arm at all for a
*bare* (unquoted, whole-token) `Expr::VarLength`/`Expr::VarWithDefault` —
`scatter --limit ${#tags}` fell to the catch-all `_ => None` — so added those
two arms routing through the same `resolve_length`/`resolve_default` the async
path and the in-string arms already call. `build_tool_args` is now fallible;
`run_scatter_gather` converts an `Err` into `ExecResult::failure(1, …)` at the
same architectural boundary `run_single` already uses for a dispatch error, so
the failure surfaces as a normal loud pipeline error, not a panic. Along the
way, deduped `eval_simple_expr`'s `Expr::Interpolated` arm (it had its own
~60-line copy of `eval_string_parts_sync`'s loop) down to a single call.

TDD: wrote the new tests first against the unmodified source (verified two of
them failed as expected — a bad subscript and a shape error in `--as` both
silently exit 0), then applied the fix and confirmed all pass. One test needed
a second pass — its worker read `$N` while the buggy fallback actually bound
`$n` (a mangled but working var name), so it was accidentally passing "for
free" on the old code for an unrelated reason (the worker's *own* command args
already error loud via the async path); fixed to read `$n`, matching the
silent-fallback name, so it's a real red/green pair now. Left the parallel
`StringPart::Arithmetic` swallow in the same two functions alone — same
symptom class, but a distinct `anyhow` error type (not `PathError`), so out of
this fix's scope; recorded in issues.md for the next pass.

kaibo's post-PR review caught a hole in the first cut: `eval_simple_expr`'s
`Expr::VarRef` arm coalesced `PathError::UndefinedRoot` to `Ok(None)`
*unconditionally*, so `scatter --as ${x[key]}` with a typo'd (entirely
undefined) root still silently dropped the flag — UndefinedRoot isn't
`Absence`. Follow-up commit restricts the coalesce to bare paths
(`path.segments.len() <= 1`), erroring on subscripted paths with the same
`"${x[key]}: undefined variable"` shape `resolve_length` uses (exported
`format_path` as `pub(crate)` for it). Deliberately did NOT apply the same
restriction to `eval_string_parts_sync`'s `StringPart::Var`: verified
empirically that both primary string-interpolation sites (async
`eval_string_part_async`, sync `eval_interpolated`) expand an undefined root
to empty even when subscripted (`echo "a${nope[k]}b"` → `ab`, bash-compatible)
— restricting only the sync twin would have made it *stricter* than the
primaries, diverging the other way. The whole-token/string-context split is
the shipped contract; the sync path now matches it on both sides. Also
confirmed the bare `Expr::VarLength`/`VarWithDefault` arms added in the first
cut are genuinely reachable (parser emits both variants in expression
position, `parser.rs:2307`/`:48`) and pinned them with observable tests —
the `VarWithDefault` one exploits the fact that a dropped `--as` leaves the
ITEM binding in place, so the workers' `$W` fails loud if the arm regresses.

---

## `/dev` was a no-op under `with_backend` (2026-07-03)

Amy asked a throwaway question — "did we ever add `/dev/null`?" — which turned
into finding kaijutsu's kernel never had it. kaijutsu builds its kaish kernel
via `Kernel::with_backend` (custom-storage embedders), which never ran the
`setup_vfs()` path that mounts `DevFs` at `/dev` for `Kernel::new`/`transient`.
Checking whether kaijutsu's own read-only host-root mount shadowed `/dev/null`
surfaced a second, deeper bug: it does, for reads (the real host `/dev/null` is
empty too), but writes go through `LocalBackend::read_only`'s guard and error
as read-only instead of discarding — so `cmd > /dev/null` was actively broken,
not just missing.

Worse: mounting `DevFs` at `/dev` inside `with_backend` alone would have been a
no-op. `VirtualOverlayBackend::is_virtual_path` hardcoded routing to `/v`/`/v/*`
only; every other path, including a freshly-mounted `/dev`, fell straight
through to the embedder's own backend regardless of what the internal
`VfsRouter` had mounted. Fixed both: `VfsRouter::has_mount` exposes a
mount-table lookup, `is_virtual_path` became an `&self` method that also
checks it (keeping the `/v` reservation as an explicit fast path, since that
whole namespace is reserved even where nothing is mounted), and `with_backend`
now mounts `/dev` alongside `/v/jobs`/`/v/blobs`. Verified the regression test
actually catches the bug by reverting the two source files and rerunning it —
all three cases failed as expected before the fix, passed after.

kaijutsu itself stays broken until it bumps its `kaish-kernel = "0.10"`
crates.io pin to whatever ships this — tracked as a follow-up, not fixed in
this PR.

## `help regex` — waking the ERE weights (2026-07-03)

PR #65's last follow-up: the BRE-superset story lived in four places (grep
schema, sed schema, awk help, LANGUAGE.md) with no single teachable surface.
Amy's framing set the design: token-efficient, and written to *wake up the
model's ERE weights* — so the fragment leads with working ERE idioms
(alternation, capture groups, quantifiers) before mentioning BRE compat at all,
then covers the two escape hatches and the one hard limit. One screen, every
line verified against the binary. Rides the #69 `syntax_section` mechanism, so
`help regex` worked the moment the fragment was named.

## The two review bugs: silent-zero length + literal-`$k` record keys (2026-07-03)

The 2026-07-03 coverage review verified two live silent-wrongs; both fixed here.
`${#nope[items]}` returned 0 — `resolve_length`'s bash-parity arm (`${#unset}` →
0) didn't distinguish bare roots from subscripted paths, so a typo'd name in a
length-guarded loop spun zero times with no diagnostic. Subscripted paths now
error like bare `${nope[items]}` does; bare `${#unset}` stays 0, pinned
separately so the forms can't drift together.

`{"$k": 8080}` created a literal `"$k"` key — the record-literal parser took
`Token::String` raw. Double-quoted keys now interpolate like every other
double-quoted string (new `RecordKey::Interpolated(Vec<StringPart>)` riding the
existing StringPart machinery in both eval sites; a pure-literal parse folds
back to `Quoted` so the common case is free). Single quotes remain the
literal-`$` escape hatch, and an unset var in a key expands to `""` — the
ratified string-interpolation rule applied consistently, pinned with a test.

---


## Collections panel gate + docs delivery — sign-off (2026-07-03)

The last item on the collections milestone: Teaching note #8's pre-sign-off cross-model
panel re-test against the *final* bracket surface (literals, lvalues, `push`, `$()`-only
iteration — all merged to main this session via #66/#67), plus closing the docs/help gaps
that closing out that milestone exposed.

**The composable-help surface had drifted from `LANGUAGE.md`.** Each collections PR kept
`docs/LANGUAGE.md` in sync (per the CLAUDE.md convention), but the `collections`
`syntax_section` in `crates/kaish-help/src/fragments.rs` — the single source for `help
syntax`, `syntax.md`, and (new) `help collections` — was never updated when native literal
construction and spread landed (#66/#64). It still taught `fromjson`-only construction.
Fixed: the syntax_section now leads with the native literal forms (list/record/nesting) and
the `...` spread nest-vs-flatten contrast, matching LANGUAGE.md; a new ranked Foundations
Rule (`collection-literals`, rank 10) and Contrast fragment mention the literal forms and
the dot-leakage error in the always-on onboarding block too.

**`help collections` is now a fragment query, not a file** (the design's explicit decision,
finally implemented) — `compose::render_syntax_section(key)` renders a single `Syntax`
fragment by its key, single-sourced with `syntax.md`, and `HelpTopic::parse_topic` falls
back to it for any key that matches a registered syntax_section before falling through to
`Tool(name)`. The mechanism is generic (any future subsystem-sized syntax feature gets
`help <key>` for free by naming its section), not collections-specific plumbing.

**A real regression surfaced while assembling the panel's cheat sheet, independent of the
panel run itself.** Teaching note #1 — "teach an operator inside its full control
structure, never bare" — is the hard-won rule from the *original* 2026-06-05 experiments
(a standalone `[[ k in $r ]]` line reads as a complete statement to a model). Both the
`collections` and `test-expressions` syntax_sections had shipped membership as a bare
standalone `[[ ]]` line anyway — introduced when membership landed (#58) and never caught,
because no panel re-test had exercised the *actual composed artifact* since then. Fixed in
both fragments and in LANGUAGE.md's matching examples before running the panel, so the
tested artifact reflects the corrected teaching copy rather than the regression it would
otherwise have quietly re-validated.

**The panel gate: 18/18 clean, zero correction rounds.** Ran the actual shipped
`Recipe::agent_onboarding()` output plus the `collections` syntax_section — the real
delivery artifact, not an ad-hoc cheat sheet — as a stateless one-shot prompt (no repo
access) against DeepSeek V4, Gemini 3.5-flash, and Claude Haiku 4.5, on a 6-task script
covering every item Teaching note #8 called out: nested record construction, bracket-path
lvalues + `push`, the literal-vs-variable subscript distinction (`${user[name]}` vs
`${user[$field]}`), membership inside a full `if/then/else`, a bare dot-leakage probe, and a
slice. All three models converged immediately on `for k in $(keys $servers); do echo "$k:
${servers[$k][port]}"; done` — the exact form the 2026-06-05 panel most commonly got wrong
(it required explicit correction to stop reaching for the bare-builtin for-head). Zero
dot-leakage on the bare field-access task, despite no "don't use dots" warning in the
prompt — the taught contrast (`${u.name}` shown as the wrong form, with its error) held
unprompted. All 18 generated scripts were then run against the real
`./target/debug/kaish` binary and produced correct output. **The v2
bare-collection-iterates relaxation is not adopted** — there's no evidence the `$()`-only
form is a tax being paid forever; it converged in one round across the whole tested range.
`docs/arrays-and-hashes.md` carries the full result inline at Teaching note #8 and the
Resolved-decisions "Access form" entry, rather than a separate write-up, since the doc is
already the design's evidence record.


## BRE follow-ups + the stale-`$?` bug (2026-07-03)

Working the PR #65 follow-up comments: awk's invalid-FS/`split()` errors now name
the separator as the user wrote it (not the rewritten form the engine saw) and
carry the dialect hint when the rewrite changed it.

The bigger catch was the loose end from PR-D testing: a standalone `[[ ]]` never
wrote `$?` — `[[ 1 = 2 ]]; echo $?` printed 0, so `[[ -f x ]]; ok=$?` silently
read the *previous* command's status. Amy called it P1 on sight and it was
cheaper to fix than file: `Stmt::Test` now mirrors `Stmt::Command` (write the
result, honor suppressible errexit). The chain arms already suppress errexit
around their left side and `if`/`while` conditions evaluate as expressions, so
`[[ … ]] && cmd` and loop conditions are unaffected — pinned with five
`shell_compat!` tests verified against real bash, including the `set -e` trip.

## GNU BRE superset for grep/sed/awk (2026-07-03)

Issue #60 measured `grep 'a\|b'` as the single largest source of wasted agent
tool calls (12 of 41 explorer greps in one sweep, each followed by a single-term
retry) — commercial models write GNU BRE reflexively and don't unlearn it from
preamble text. kaish had three different wrong answers: grep silently no-matched
(literal `|`), sed loud-rejected (the E006 teaching error from the ergonomics
pass), awk silently no-matched. The fix reverses the sed-pass decision that
erroring beats guessing: a shared rewriter (`regex_dialect.rs::bre_metas_to_ere`)
turns the seven backslash-metas (`\| \+ \? \( \) \{ \}`) into their ERE forms, so
both spellings are the same operation and it's no longer a guess. `-E`/`-r`
(grep/sed) now mean *strict ERE* — the escape hatch where `\|` is a literal pipe;
awk always rewrites (no flag). The narrow casualty: a backslashed meta is always
the operator; literals move to bracket classes (`[|]`, `[)]`).

The review round (self-review + kaibo deepseek on whole files, no diff) earned
its keep: awk's FS/`split()` path applied the rewrite *inside* the multi-char
regex branch, so the gawk literal-pipe idiom `FS="\\|"` became the
empty-alternation regex `|` and split every character (NF=7 on `a|b|c` vs gawk's
3) — silent-wrong, the exact class the branch exists to kill. gawk's actual
order is demote-then-single-char-literal, so the fix is rewrite *before* the
single-char check. Also caught: the `-E` flag doc comments still said "no-op"
(and they're schema-visible via `schema_from_clap`), and the changelog claimed
grep takes `-r` (that's recursive). Since a formerly-literal escape like `:\)`
now fails compile with an error describing a pattern the author never wrote
(`unopened group` on `:)`), compile errors append a dialect hint (bracket-class
spelling + the strict-ERE flag where one exists) whenever the rewrite actually
changed the pattern.

## Collection lvalue writes + `push` land (2026-07-02)

`xs[0]=9`, `user[email]=x`, deep paths (`services[web][port]=9000`), and a
bareword `push` — the write half of the collections effort, on top of the
shared per-hop path resolver (`resolve_step`, from the read-side #6 work) and
the collection-literals grammar.

**`walk_write` mirrors the read descent, sharing `resolve_step` unchanged, but
diverges at the leaf.** The read side (`resolve_path`) walks the tree with
`Cow`-borrowed lookups; the write side clones the root once, then walks
`&mut serde_json::Value` chains so mutating the deepest hop mutates the whole
tree in place (no per-hop clone). Every hop still calls the SAME `resolve_step`
for classification (bounds, shape) — read and write can never classify a
subscript differently. The two walks diverge only in *leaf policy*: an
intermediate hop requires the child to already exist (`descend_mut` — no
autovivification, mirroring the read's `descend`), while the FINAL hop may
insert a new record key (`apply_leaf_write`) — the one thing a path-set may
create. A list index write is in-bounds only for free: `resolve_step`'s
`classify_index` already turns an out-of-bounds index into a loud `Absence`
before the write ever sees it. A slice step (`xs[0:2]=x`) is always a `Shape`
error, at any hop — mutating through a detached slice copy would silently not
write back to the real list, so it's rejected outright rather than special-cased
only at the final position.

**The lexer needed a SECOND, distinct suppression trigger.** The
collection-literals grammar already taught the lexer to stop fusing a
`[`-leading run into a `GlobWord` at *value position* (RHS of `=`/`in`). An
assignment LHS (`fruits[0]=kiwi`) sits at argv position, not value position —
a different shape entirely. `flush_glob_run` gained `followed_by_eq`: computed
by peeking at whatever token triggers the run's flush (in token-stream order,
regardless of whitespace, since `local xs[0] = 9` and `fruits[0]=kiwi` must
both suppress), true only when that token is `Token::Eq`. A bracket-pair run
with no `*`/`?` immediately before `=` is a subscripted assignment target, not
a glob, and gets suppressed the same way the value-position case does — the
two triggers are ORed together in `flush_glob_run`, each protecting a
different shape of run.

**`Assignment.name: String` widened to `Assignment.path: VarPath`** — no dual
representation, every construction/read site updated in the same change
(parser, kernel, sexpr formatter, validator, tests). `Assignment::name()`
extracts the root `Field` for the still-common bare case; a subscripted write
never honors `local` (it always mutates the existing root wherever it lives —
`local` is meaningless once you're inside an existing collection).

**Env-prefix assignment stays bare-ident-only, by construction, not by a
runtime check.** `env_prefix_assign` still calls `ident_parser()` directly
(never `lvalue_path_parser()`), so a subscripted target can't even reach the
`EnvScoped` grammar arm — structured values can't cross the process boundary
anyway. Grounding this surfaced a pre-existing, unrelated quirk: kaish's
statement `terminator` is `.repeated()`, not `.at_least(1)`, so `X=1 Y=2` (no
separator at all) already parsed as two independent assignment statements
before this change. `user[email]=x echo hi` falls through the same way —
NOT into `EnvScoped` (verified with a parser test), but into two ordinary
statements. That's the existing grammar's behavior extended consistently, not
a new hazard this work introduced.

**Validator additions (E016, E017).** A subscripted assignment whose root
isn't bound (`z[0]=x`) is now a static error with a "create it first" hint,
the same treatment `push`'s undefined-target rule gets at runtime. A dotted
assignment target (`user.email=x`) is now also a static error — the lexer's
shared `Ident` token admits `.` for other legitimate uses (filenames,
`source foo.kai`), so the restriction lives in the validator, not a regex
change, with the fix (`user[email]=x`) in the message.

**`push` ships bareword-target only; bracket-path `push` is a real, tracked
gap, not a rejection.** `push services[web][tags] item` doesn't work: the
target isn't followed by `=`, so the lvalue lexer's `followed_by_eq` trigger
never fires, and the whole subscripted target fuses into a `GlobWord` that
glob-expands (failing loudly as "no matches") before `push` ever runs. Loud,
not silently wrong — but the feature the design doc originally scoped for
`push` isn't there yet. Filed in `docs/issues.md` P3 rather than attempted
here; it needs its own lexer/parser pass since the lexer has no notion of
"this bareword run is `push`'s target."

## Collection literals + spread land (2026-07-02)

`xs=[a b c]`, `{port: 8080}`/`{port:8080}`, multi-line records with a trailing
comma, nesting, and `[...$xs date]` spread — the construction half of the
collections effort, on top of the read-side resolver and the value/argv
grammar seam (both already landed). Value model unchanged: a literal just
evaluates to `Value::Json(Array|Object)`, so every existing access/`keys`/
`values`/membership/shape-guard/`${#…}` mechanism works on a literal for free.

The load-bearing decision was **context-aware lexer suppression over a global
rule**. The obvious fix — stop the glob-merge pass from ever fusing a
`[`-leading run — was rejected up front: kaish's argv glob path (`ls [dog]`,
`foo[0-9]`) and scalar assignment (`x=foo:bar`) both lean on the *existing*
fusion behavior, so a global change would either break those or need a second,
parallel "argv assembles from primitives" grammar that doesn't exist. Instead
`lexer::compute_value_context` walks the token stream once and marks a
per-token flag — inside/opening a value-position `[`/`{` literal — and the
glob-merge/colon-merge passes skip fusion *only* where that flag is set. Value
position starts right after `Token::Eq` (assignment) or a genuine membership
`Token::In`; everything else is untouched.

The load-bearing subtlety is that the two tokens the suppression keys on are
each **reused** for a non-value grammatical role, and getting the split wrong
breaks real syntax. This took three iterations to get right (two intermediate
heuristics landed and regressed before the grammar-exact rule) — the story is
worth keeping because it's a clean case of "reason from the grammar, not the
token":

- **`Token::In` is both membership and a statement head; `Token::Eq` is both
  assignment and `[[ ]]` comparison.** Membership `in` (`[[ e in $c ]]`) must
  open value position for its RHS literal; a `for`/`case` head `in`
  (`for f in *.txt`, `case 5 in [0-9]*)`) must NOT (the following word is an
  argv glob / char-class pattern). Symmetrically, an assignment `=`
  (`x=[a b]`) must open value position; a comparison `=` (`[[ $x = [0-9]* ]]`)
  must NOT. The **grammar-exact discriminator is `[[ ]]` test depth**:
  membership `in` occurs only *inside* `[[ ]]`, a head `in` only *outside*;
  assignment `=` occurs *outside*, comparison `=` only *inside*. So
  `compute_value_context` tracks `test_depth` in its forward pass and opens
  value position on `Token::In` iff `test_depth > 0` and on `Token::Eq` iff
  `test_depth == 0`. (`==` is a distinct `Token::EqEq` and never opens value.)
  Two rejected intermediate heuristics that each shipped and regressed:
  (1) a fixed three-token lookback `For Ident In` — covered `for` but missed
  `case`'s variable-length head (`case EXPR in`), so `case 5 in [0-9]*)`
  parse-errored; (2) a general pending-`for`/`case` counter — fixed the `in`
  half but the `=` half was still wrong (`[[ $x = [0-9]* ]]` regressed), and a
  bareword `for`/`case`/`in` inside `$(…)` could leak the counter/flag past
  `RParen`. The `test_depth` rule subsumes all of it: no per-keyword casing, no
  state to leak across `$()` (a bareword `in` inside `$(echo in)` is at
  `test_depth 0` → never membership; a real sub-shell assignment `$(x=…)` is
  also `test_depth 0` → still an assignment), and the `[[` detection is guarded
  by `!currently_value` so a glued nested value list `x=[[a] [b]]` reads as
  literal brackets, not a bogus test.
- **A pure `Star`/`Question` glob at value position must NOT suppress.**
  The first pass suppressed *any* run that opened right after `Eq`, which
  broke the existing `X=*.txt` → literal-string-`"*.txt"` invariant (caught by
  the pre-existing `kernel::tests::test_glob_in_assignment_is_literal`, not a
  new test — the full suite is the safety net here). Narrowed to: suppress
  only when the run actually contains an `LBracket`/`RBracket` pair — a glob
  with no brackets at value position is unaffected and keeps evaluating to a
  literal string exactly as before literals existed.

The colon-fusion exemption for `{port:8080}` needed the same care in the other
direction: it must NOT fire for a plain scalar assignment like `x=foo:bar`
(which must keep fusing into one `Ident`), so the suppression flag there is
narrower than the bracket one — specifically "inside an open value-position
`{`", not just "at value position".

AST: `Expr::ListLiteral(Vec<ListElem>)` / `Expr::RecordLiteral(Vec<RecordEntry>)`,
`ListElem::{Item,Spread}`, `RecordKey::{Bare,Quoted}`. Eval landed twice (async
`kernel.rs::eval_expr_async` for real execution, sync `interpreter/eval.rs`
`Evaluator::eval` for the reduced sync path) with a shared `spread_non_list_message`
helper so the two paths can't diverge on wording for a non-list spread — the
same dual-eval-site pattern as `${#…}` length and `${…:-default}` before it.

Deferred (`docs/issues.md` P3): deeply-nested *glued* list literals
(`x=[[a] [b]]` — spaced nesting works fine, only the glued form isn't
unfused), and the multi-word-bareword-record-value error
(`{msg: hello world}`) is loud but carries chumsky's generic message instead
of the hand-crafted "quote it" wording the design doc sketched.

## Importance-ranked onboarding tiers (2026-07-01)

Step 3 of the collections effort splits in two: the tier *mechanism* (pure infra,
this entry) and the collections *fragments* (which describe not-yet-built syntax,
so they wait on a ship-vs-panel scoping call). The mechanism landed first because
issues.md wanted it "before the collections fragments land — they're the feature
that will test it."

The always-on instruction block an embedder ships was an undifferentiated blob in
registry order; nothing capped its size and nothing guaranteed the load-bearing
rules came first. Now `Fragment` carries an importance `rank` (0 = most important;
`UNRANKED` default keeps registry order), and `select_for_concept` stable-sorts by
it — a deliberate no-op for any all-`UNRANKED` concept, so `syntax.md`
(`render_syntax_reference`, which doesn't even go through that path) and the REPL
welcome stay byte-identical. The ~10 Foundations Summary fragments got explicit
ranks 0..9, ordered by what an agent needs to write *correct* kaish first
(no-word-splitting, quote-to-join, then substitution/iteration, then capability
and safety guarantees, with the verbose overlay-mode trailing). A char-budget test
now keeps the spine lean.

Two implementation notes worth keeping: `const fn ranked(self, rank) -> Fragment {
Fragment { rank, ..self } }` relies on functional record update in const context
(stable since Rust 1.83; MSRV is 1.85), which let the rank attach in the static
registry without a builder-signature change touching every call site — only the
ranked fragments changed. And the sort key must stay `rank` alone: adding `key`
would alphabetize equal-rank fragments and reorder the Model concept the REPL
welcome depends on. Deepseek review confirmed the no-op-for-unranked claim and
flagged one latent case (Reference-depth Foundations would separate a Contrast
from its Rule — inert today, filed P4) and the public-field break (loud compile
error, marked BREAKING).

## Native collection read access: the bracket path resolver (2026-07-01)

Step 2 of the collections effort — reading into a value with `${xs[0]}`,
`${r[key]}`, `${r[$k]}`, `${r["weird-key"]}`, `${xs[-1]}`, `${xs[0:2]}`, chained
`${a[b][c]}`, and `${#…}` list/record length. Deliberately *before* the
literal-grammar gate: it only consumes `${…}` segments the lexer already splits,
so no value/argv bifurcation and no glob-merge change — the read side was
"half-built" and this finishes it.

A reconnaissance pass (Explore agent) charted the machinery and surfaced the
shape of the work: the lexer already hands out `["VAR","[0]","[k]"]`, but
`parse_varpath` was *filtering every bracket segment out* (so `${x[0]}` silently
returned the whole value — a latent silent bug the red test baseline caught
immediately), and `resolve_path` rejected anything multi-segment. The clean
insight that shaped the design: dynamic keys are just `$var`, resolved through
`scope.get`, and `resolve_path` is a `Scope` method — so the **entire traversal,
dynamic keys included, lives in `resolve_path`**, and all four var-ref eval sites
(sync + async, expression + string) get collection access for free with no
evaluator plumbing.

The load-bearing decision was the error channel. `resolve_path` went from
`Option<Value>` to `Result<Value, PathError>` with a two-variant split that the
whole no-silent-fallback directive rides on: `UndefinedRoot` is **soft** (an
error in expression position, empty inside a string — bash-compatible, preserving
every existing `"${UNSET}"` → empty), while `Invalid` (subscript on a scalar,
out-of-bounds, missing key, dotted access, string-key-on-list, integer-index-on-
record) is **loud everywhere, including inside double-quoted strings** — never
swallowed to empty. That distinction is exactly why the four primary eval sites
each match both arms explicitly; the three reduced *sync* pipeline sites keep
their pre-existing best-effort coalescing (a known narrow gap, filed P3).

Access unwraps at the boundary through `json_to_value_no_envelope` (the same
envelope-free law `fromjson` uses — an envelope-shaped sub-object stays a record,
never becomes bytes), so `[[ ${cfg[healthy]} == false ]]` is a typed bool compare
and `$(( ${cfg[port]} + 1 ))` just works — the latter needed teaching the
arithmetic mini-parser's `${…}` branch to collect bracket runs and reuse
`parse_varpath` + `resolve_path` rather than choking at the first `[`.

Brackets-only is enforced by making a dotted `${u.name}` a loud error suggesting
the bracket form (the lexer still splits `.field`, so a non-root `Field` segment
*is* the dotted case). Semantic calls, all consistent with the loud-errors ethos:
missing key errors (not bash's silent empty — `[[ k in $r ]]` is the presence
test), integer index on a record errors ("integers index lists"), a dynamic
string key works as a list index if it parses as one. Deepseek review confirmed
the core (envelope-free at every level, panic-free bounds math, correct arithmetic
bracket balancing) and turned up only the two low-priority follow-ups now in
issues.md. Twenty-four kernel-routed tests, built on `fromjson` to construct the
values the literal grammar can't yet.

## JSON bridge: fromjson / tojson land ahead of the grammar (2026-07-01)

First implementation step of the collections effort, and deliberately the one
that touches no lexer or parser. `fromjson`/`tojson` are the value model's text
boundary — the pair the arrays-and-hashes doc sketched as "prototype early." They
exercise the whole value-model plumbing (structured `$()`, `.data`, typed
positionals, assignment capture) end to end, so building them first de-risks the
boundary semantics and pins the error copy before the big grammar PR.

Two integration points were verified against HEAD before writing a line, because
they decide whether the pair is useful *before* native access exists: (1)
command-substitution already prefers a result's `.data` (`kernel.rs:3716`), so
`cfg=$(… | fromjson)` captures the structured `Value`; (2) a variable holding a
`Value::Json` expands straight into `args.positional` as that typed value
(`kernel.rs:3285`), so `tojson $cfg` reads a real value off the positional, not a
stringified copy. Both held — the pair works today, with access arriving later.

The load-bearing decision is **envelope-free conversion**. The internal
`json_to_value` auto-decodes any object matching the base64 byte-envelope shape
into `Value::Bytes` — correct for internal round-tripping, a silent-corruption
trap for *external* JSON that happens to match. Added
`json_to_value_no_envelope` (kaish-types) and routed `fromjson` through it; the
hazard is pinned by a test that feeds an envelope-shaped document through
`fromjson | tojson` and asserts it re-serializes as a record, not as refused
binary. `fromjson` is one-doc-one-value (serde's trailing-garbage rejection does
the work; empty input is a separate loud error), and its parse errors carry
serde's line:column. `tojson` is text-out-only — setting `.data` would make
`$(tojson $x)` round-trip straight back to a value and defeat the export escape
hatch — and refuses `Value::Bytes` loudly rather than emit an envelope. The
roundtrip law (`fromjson "$(tojson $x)"` ≡ `$x`) is test-pinned. Both are pure
data: registered unconditionally, verified under `--no-default-features`, and
tested through `KernelConfig::isolated()` (no localfs).

## Arrays & hashes design revision: the brackets are ours (2026-07-01)

The 2026-06-05 collections proposal came back for review a month later and left
substantially changed — not because the method was wrong (the weak-model stress
tests held up), but because grounding it against the current code surfaced two
facts that flipped decisions, and a design conversation with Amy pushed one
principle — consistency over convenience — further than the original draft dared.

Three Sonnet exploration passes re-grounded every file:line claim (several had
drifted; two issues.md citations pointed at entries that no longer exist). Two
discoveries did real work. First: `for x in a b c` word lists already parse, so
the proposal's bare-builtin for-head (`for k in keys $r`) wouldn't have filled a
grammar hole — it would have changed the meaning of valid syntax, with the
quasi-reserved word set growing every time a builtin ships. Replaced by
`for k in $record` iterating keys: Python's exact prior, zero grammar cost.
Second: the lexer's `Ident` regex admits `.`, so `user.email=x` parses *today* as
a flat, unreadable variable — dotted path-writes would have silently diverged
from that. That discovery sealed Amy's instinct to drop dotted access entirely:
**brackets only** (`${user[name]}`, `${r[$k]}`, `${r["weird key"]}`), bareword
subscripts are literal keys, jq keeps the dot-shaped language so the two surfaces
never blur. The doc keeps the superseded evidence annotated rather than erased —
the record of *why* is part of the method.

The review also filled gaps the original never addressed: scalar unwrap at the
access boundary (so `[[ ${cfg[healthy]} == false ]]` is a typed comparison, not a
stringify accident), structural equality collection↔collection with a loud error
for collection↔scalar, and a full lvalue ruleset — bracket paths, no
autovivification, in-bounds index set, POSIX-name LHS tightening. A fresh lexer
collision fell out of writing the implementation notes: glued `[]`/`[dog]`/`[1]`
merge into GlobWords today (`x=[dog]` is currently a *glob assignment*), so a
`[`-leading token at value position is now ALWAYS a list literal — a small
breaking change Amy ratified with "we own the []". limits.md's "kaish will never
have `[]` array syntax" line got the falsehood treatment the same session.

Cross-model review, house style (no diff, whole files): a gemini-pro batch with
the lexer and parser attached found what the doc's impl notes underestimated —
kaish's pre-parser token-fusion passes are hostile to the new literals.
`merge_colon_adjacent` eats unspaced record colons (`{port:8080}`), the
double-quote regex can't survive quoted keys inside interpolated strings, and the
shared expression parser must bifurcate into value/argv grammars or `ls [dog]`
grows a JSON argument. Every lexer claim was verified locally before folding.
Gemini also pushed back on `push` being top-level-only — the nested-append
workaround (`a[b]=[...${a[b]} new]`) was hostile enough that Amy reversed it:
push takes bracket paths under the lvalue rules. Unspaced colons: accepted.
Quoted keys in strings: loud error suggesting assign-first. A deepseek second
pass on the post-fold doc was still in flight when this PR opened.

Two forward commitments landed in the doc. `fromjson`/`tojson` (jq-named, pure
data, envelope-FREE conversion — `json_to_value`'s bytes-envelope sniffing must
never run on external JSON) get prototyped *before* the grammar: they exercise
the whole value-model plumbing without touching the parser. And the help system
grows importance-ranked ~200–300-char onboarding tiers (Amy's design), with
`help collections` as a fragment query rather than a file, and the panel re-test
gated on testing the *composed* help artifact — the thing we ship, not an ad-hoc
cheat sheet. Collections grammar work does not start until that re-test passes.

## `classify_command`: a supported command classifier for embedder preflight (2026-06-30)

Follow-on to the typed latch accessor (#45). kaijutsu wants to gate kaish's
destructive/external operations for consent — walk a script, find the command
nodes that escape to `PATH`, and block until they're approved. It can already do
most of that with the public surface (`parser::parse`, the `ast`, `tool_schemas`,
`has_function`). The one thing it *can't* do without forking kaish's truth is
classify a command name the way the interpreter resolves it — the two rules that
decide that (`is_static_command_name`, the special-form set) were private helpers
in the validator. If kaijutsu hardcodes copies, the day kaish refines name
resolution its consent gate silently disagrees with what kaish actually runs.
That's the silent-divergence failure class, and here it's security-relevant.

So we added `Kernel::classify_command(name) -> CommandKind` (`Builtin` /
`UserTool` / `Special` / `Dynamic` / `External`, plus `escapes_kernel()` for the
two buckets a gate scrutinizes). `CommandKind` lives in `kaish-types` (pure data,
`#[non_exhaustive]` so a future variant doesn't break embedders — and the safe
default for an unknown kind is to gate it). The classification core is one shared
`classify_command_name` in `validator/walker.rs`; the kernel computes the two
booleans and delegates, so there's a single source of truth.

The sharp bit was discovered empirically: the validator's `is_special_command`
set (`true`/`false`/`:`/`readonly`/`local`) is **not** what the interpreter
actually short-circuits. At runtime `readonly` resolves to an *external* command
(exit 127), `:` is a parse error, `local` is parser-level. The validator uses that
broad set only to suppress a "command not found" warning. `classify_command`
deliberately mirrors the **interpreter's** real special-forms
(`true`/`false`/`source`/`.`) instead — a consent gate must see `readonly` as
`External`, not be told it's internal. The validator↔runtime mismatch is now
filed as a P3 (the validator probably shouldn't suppress those warnings).

A deepseek review (kaibo) caught the one real under-reporting hole: execution
expands **aliases** before the registry/PATH lookup, so `alias cat=/bin/evil`
would have classified as `Builtin` while running external — the dangerous
direction for a consent gate, and there was no public alias API for the embedder
to compensate. `classify_command` now expands aliases internally, mirroring
`execute_command_depth`'s bounded recursion (special-forms re-checked each step),
so it reports `External` there too. `/v/bin/cat` and `.kai`/backend tools still
over-report as `External` (the safe direction — over-gate, never leak a `PATH`
escape). Added a `${VAR}` → `Dynamic` guard for the string-API surface.

**Staying in sync with the executor.** The classifier duplicates the
interpreter's resolution rules, which is the same silent-divergence risk the
feature exists to kill — just moved inside the kernel. Three guards keep it
honest: (1) *membership* never drifts because classify reads the live registry
and user-tool tables, not a copy; (2) the *special-form set* is a single
`SpecialForm` enum whose `from_name` is the only place a name becomes "special" —
classify reports it via `is_runtime_special_form`, and `execute_command_depth`
matches the enum **exhaustively**, so adding a form is a *compile error* until
both the name mapping and the behavior are updated (the first cut used a const +
`unreachable!`, but a deepseek round-2 review flagged that as a runtime panic the
drift test didn't actually exercise — the enum makes it compile-enforced and
drops the panic); (3) a `classify_command_matches_executor` drift test pins the
rules that *can't* be compile-enforced (user-vs-builtin precedence, alias
expansion) by classifying *and* observing the real resolution for each kind, and
now executes every special form (incl. `source`/`.`), not just `true`/`false`.
Add a resolution step to the executor without teaching classify, and either it
won't compile or that test fails.

Deferred, same reasoning as the `kaish-edit` crate: the `PreflightReport`, the AST
walk, and the consent loop are embedder policy (kaijutsu owns them), and a
`Kernel::preflight(src)` convenience waits for a second consumer. Docs: a
"Preflighting a script for external commands" recipe in EMBEDDING.md.

## Interpreter stack-depth analysis → first GitHub Issues (2026-06-30)

A question — "what pushes up the interpreter's need for stack space over time?" —
turned into a map of the async statement engine and three filed issues. The shape
of the answer is worth keeping: stack cost here is **call-chain depth × fat async
frames**, not data.

- The real statement engine is the async kernel in `kernel.rs` (not the sync
  `Evaluator` in `interpreter/eval.rs`, which only does arithmetic/test
  sub-eval). Its core is a set of mutually-recursive `Box::pin`-returning
  functions — `execute_stmt_flow`, `eval_expr_async`, `eval_string_part(s)_async`,
  `eval_test_async`. Rust async recursion *requires* boxing, and each boxed level
  keeps every live local across every `.await`, so frames are heavy.
- The deepest cycle is command substitution: `Expr::CommandSubst(Vec<Stmt>)`
  re-enters `execute_block_capturing` → `execute_stmt_flow`, stacking two boxed
  futures per `$( … )` nesting level. Recursive shell functions
  (`execute_user_tool`) and `.kai`→`.kai` sourcing (`try_execute_script`) deepen
  the *same* thread stack the same way. None of these is depth-guarded — only
  alias re-entry (`<10`) and the lexer (`MAX_PAREN_DEPTH = 256`) are.
- Per command, the dispatch chain (`execute_pipeline` → runner → `dispatch_command`
  → `execute_command_depth`) carries a ~30-field `ExecContext` that's copied twice
  in `dispatch_command`, plus `#[tracing::instrument]` future bloat on the hot
  frames.
- Runtimes are plain `Runtime::new()` (no custom stack). The foreground root future
  runs on the `block_on` thread; forked work (pipeline stages, scatter, background)
  hops to tokio worker threads with the default ~2 MB stack — so the same script
  has less headroom inside a pipe than in the foreground.

Filed as [#46](https://github.com/tobert/kaish/issues/46) (depth guard — a loud
error beats a silent `SIGSEGV`), [#47](https://github.com/tobert/kaish/issues/47)
(explicit, uniform worker-thread stack size), and
[#48](https://github.com/tobert/kaish/issues/48) (a profile-first memory/allocation
pass — suspected low cost with occasional peaks, not urgent). These are the first
issues we've put on GitHub instead of [issues.md](issues.md): an experiment ahead of
announcing kaibo, where outside agents and people will want a public tracker.
`devlog.md` stays in-repo and ships with the code — these repos also teach how a
project gets sculpted.

## `execute_argv` — argv-native kernel entry point (landed 2026-06-29)

`execute(&str)` is *string-native*: it lexes and parses its input. A caller that
already holds tokenized argv — a structured embedder (kaijutsu), a future
busybox-style multicall binary — had to re-quote argv into a string just so the
lexer could split it apart again, a round-trip that's *lossy* for typed values
(`to_argv()` stringifies `Bytes`/`Json`). `Kernel::execute_argv(name, &[Value])`
is the peer door that skips it. Full design: [multicall.md](multicall.md).

**The implementation deviates from the design doc's letter, deliberately.** The
doc proposed a new `build_args_from_argv` that builds `ToolArgs` directly,
"mirroring `build_args_async` minus the `Expr` eval." Writing a *second* binder is
exactly the drift hazard the validation-builder unification (2026-06-23) was about.
Instead the only new logic is `argv_to_args(&[Value]) -> Vec<Arg>`: a classifier
that maps each token to the AST `Arg` the lexer would produce for the equivalent
minimally-quoted word (`--` → `DoubleDash`, `-x…` with an alpha lead → `ShortFlag`,
`--k=v` → `Named`, identifier `k=v` → `WordAssign`, else literal `Positional`; a
**non-string** `Value` is always a literal positional — the typed-passthrough win).
`execute_argv` then runs the resulting `Command` through `execute_pipeline`,
*reusing the entire string-door dispatch chain verbatim* — command resolution,
arg binding, `--json`, the latch. Two binders can't drift because there's still
only one. Typed values survive because `Expr::Literal(Value)` carries the value
and evaluates by identity (it never becomes a `GlobPattern`/`VarRef`, so no
glob/`$VAR`/split can occur).

**Semantics:** tokens are literal — the "single-quoted word" rule taken to its end.
`execute_argv("echo", &["*.txt"])` emits `*.txt`. The kernel's pre-execution
*syntax* validator doesn't run (argv has no shell syntax; a tool's own
`validate()`/clap parse still does, at dispatch).

**Tests** (the design's four surfaces): an in-crate classifier suite incl. the
workspace's **first proptest** — `classifier_matches_parser_on_clean_tokens`
asserts `argv_to_args` agrees with the *real parser* on metachar-free tokens. It
earned its keep immediately, shrinking to `:A=0`: the lexer colon-merges `:A` into
one `Ident` and parses it as a `WordAssign`, where the classifier (bash-correctly)
makes a positional. They converge observably anyway — a `WordAssign` for any
non-`export`/`alias` command stringifies straight back to a `"key=value"`
positional — so the property compares *logical arguments*, not AST tags, and the
case is pinned as a regression seed. Plus kernel-routed `execute_argv_tests.rs`:
argv door ≡ string door over a corpus, literal-not-globbed, no `$VAR` interp,
typed `Int`/`Json` passthrough, `--json` transform, exit-127, and a full latch
round-trip (gate → `--confirm=<nonce>`) all through the argv door.

**Deferred:** the `kaish-multicall` binary (the cheap frontend half) and the
`&[String]` convenience door — both in issues.md.

## rg-features port — `--ftype` filtering on grep + glob (landed 2026-06-27, #38)

`rg` was dropped under the 80% rule, but its still-useful filtering re-homed onto
kaish's two *modern* search builtins. Driver: kaibo's hot path (type-scoped greps,
early-stop on match caps). The walker engine was already done — `WalkOptions.types`
had been live but dormant — so this was surface wiring plus a shared registry.

Landed scope (the design lived in the transient `search-features-port.md`, deleted
on ship):
- **grep + glob only; `find` stays POSIX** (no `--ftype` on find — it keeps
  traditional behavior; the `--no-ignore` recovery question for both search builtins
  and for find-under-`Enforced` is the live deferral, now in issues.md P3).
- **`--ftype` is the kaish-wide file-type-filter standard**, deliberately *not* rg's
  `-t`. Both grep and glob get `--ftype` / `--ftype-not` / `--ftype-list`, sharing
  one `kaish-glob::build_file_types` registry so they can't drift. An unknown type
  name is a loud exit-2, never a silent empty match.
- **All new flags are long-only, no shorts** — sidesteps GNU-grep `-T`/`-m` muscle
  memory landing on different semantics.
- grep also got **`--hidden`** (include dotfiles, bash no-dotglob default off) and
  GNU-semantics **`--max-count <N>`** (per-file cap, streaming early-stop so it
  bounds work on large/piped inputs; a truncated-at-cap UTF-8 carry is no longer
  misflagged as binary). glob keeps its fd-style `-t`/`--type` (entry *kind*:
  file/dir), which composes with the new extension-based `--ftype`.

DeepSeek-reviewed (via kaibo) on the branch.

## Correctness one-offs — grep -c exit, `$()` trim, jq /0 (landed 2026-06-24)

Three small independent silent/surprise fixes (`correctness_oneoffs_tests.rs`):

- **`grep -c` exits 1 on zero matches** (GNU). Was always exit 0 — the count is
  printed but the status must still signal "no match." Both the single-buffer and
  multi-file count paths now set `code = 1` when the total is 0 (multi-file: only
  when *no* file matched; a read-error still overrides to 2).
- **`$()` strips only trailing newlines**, not all trailing whitespace. The bare
  `Expr::CommandSubst` arms used `.trim_end()` (ate spaces/tabs); now
  `trim_end_matches('\n')` — the *exact* trim the quoted `"$(…)"` interpolation path
  (`StringPart::CommandSubst`) uses, so bare and quoted command substitution agree
  (they're the same operation; the for-loop split path's extra `\r` strip is its own
  line-splitting concern). Significant trailing spaces survive (`x=$(printf 'a  ')`).
- **jq `. / 0` fails loudly** instead of silently returning `null`. The mechanism:
  jaq evaluates `n/0` to a non-finite `Val::Float` (inf, or NaN for `0/0`), and
  `val_to_json` did `from_f64(inf) → None → unwrap_or(Null)` — a silent-wrong null.
  New `has_nonfinite_float` (recurses arrays/objects) gates the result loop and
  errors. **Decision:** this also errors on jq's `infinite`/`nan` *literals*, where
  real jq clamps `infinite` to max-f64 and renders `nan` as null. We can't tell a
  division-by-zero inf from an `infinite`-literal inf at serialization time (jaq
  doesn't error at the division like real jq does), and a loud error beats a silent
  null (the no-silent-fallbacks / crash-over-corruption directive). `infinite`/`nan`
  literals are rare; accepted divergence. The separate jaq float-formatting quirk (`3.0` for `6/2`,
  `1e10`→`10000000000.0`) is untouched — still in issues.md P3.

## Common-idiom lexer gaps — `@`, hyphenated numbers, `-1k` (landed 2026-06-24)

Three everyday agent inputs used to fragment into adjacent tokens and trip the
no-token-pasting guard — a *loud but wrong* parse error on ubiquitous text. All
fixed at the lexer with raw-slice tokens (so leading zeros survive), wired
through the parser exactly where `NumberIdent` already flows (argv/case/merge):

- **Bare `@`** — `@` is now an ordinary word char. Mid-word (`user@host`,
  `a@b.com`) joined the `Ident` trailing class; leading-`@` (`@scope/pkg`, `@0`,
  bare `@`) got a new `AtWord` token. `user@host:8080` still colon-merges into
  one word for free (Ident is colon-mergeable).
- **Digit-leading hyphenated words** (`2024-01-02`, `10-20`, `1.5-2`,
  `cut -f 1-3`) and **minus-led numeric predicate values** (`find -size -1k`,
  `-30d`) got one `DashNumWord` token (two regexes). A plain `2024`/`1.5`/`-1`
  stays `Int`/`Float` — the hyphen form requires a `-segment`, the minus form an
  alpha after the digits, so the two never overlap with `Int`/`Float`/`NumberIdent`.

**A deliberate decision reversed, intentionally.** `tr -d 0-9` was previously
made a *loud error* (and pinned by `bareword_comma_tests::numeric_range_is_loud_not_silent`)
because `0-9` could only fragment into `Int(0)`+`Int(-9)` and silently delete just
`0`. That was the best available then, not a desired end state. A contiguous `0-9`
the user typed is *one word*, not two pasted tokens — so it now reaches `tr`
verbatim and the range applies, matching bash/GNU. The test was rewritten to assert
the new behavior; the comma run (`echo 1,2,3`) stays a loud no-pasting error
(commas aren't part of the word). Strictly additive otherwise: every newly-valid
input was a parse error before, so no previously-valid program changes meaning.

## diff/patch improvements — `diff --json` + GNU-fuzz `patch` (landed 2026-06-22)

The diff/patch half of the [editing-for-agents](editing-for-agents.md) design
pass (the `edit` builtin was declined for kaish — embedder concern). Two changes,
both on branch `feat/diff-patch-improvements`:

- **`diff --json`** attaches structured hunks via `OutputData::with_rich_json`
  (same mechanism as `grep --json`) while the text node keeps serving pipes/humans.
  Built from `similar`'s `iter_hunks()` ranges + `iter_changes()` tags. The kernel
  only serializes the rich_json when `--json` is requested, so plain `diff` is
  byte-for-byte unchanged.
- **`patch` gained GNU-style offset search + fuzz**, replacing the
  stricter-than-GNU exact-context matcher that hard-failed on any drift (an
  "attractive nuisance" — agents pipe a near-miss diff, it rejects, they loop). The
  new pure `apply_hunks` locates each hunk by matching its context near the
  header position (searching outward → *offset*) and, failing exact, trimming up to
  2 context lines per end (*fuzz*); only the verified span is rewritten, so fuzz
  never overwrites unverified lines. Offset/fuzz are reported loudly (`Hunk #N
  succeeded at L (offset O lines)`); no-match still fails loud (`Hunk #N FAILED`),
  file untouched. Applied via a whole-file `PatchOp::Replace { offset:0, expected:
  Some(original) }` — TOCTOU-safe CAS, uniform across local/overlay backends.

**Decisions worth keeping:** the strict→fuzzy relaxation is *within one algorithm,
reported* — explicitly NOT the rejected "hijack patch → auto content-anchor on
failure" (a silent algorithm switch, the footgun kaish forbids). Reflex survey
showed agents reach for `patch`/`sed -i` by reflex; research showed line-numbered
diffs are the format LLMs generate worst — so `patch` stays a faithful workalike
and the reliable content-anchored path lives in the embedders (kaijutsu's hashline
`edit`), not here. The `apply_hunks` core is pure and unit-tested (clean/offset/
fuzz/no-match/reverse); execute-level tests confirm loud non-destructive failure.

## OverlayFs — copy-on-write overlay (landed 2026-06-10)

A CoW overlay filesystem composing with `LocalFs`/`MemoryFs`, for kaibo coder
workspaces, kaijutsu context forking, and kaish `--overlay` session mode. Landed
across `69c42e3`+`2a62a72` (MemoryFs lift + overlay core), `dddc85d` (accounting:
`resident_bytes`, `ByteBudget`), `6fe2225` (inspection API), and `d0e0deb` (kernel
surface: 64 MiB agent budget "vfs-memory", `MountInfo.resident_bytes` +
`kaish-mounts`, `--overlay` opt-in across REPL/embedder, `kaish-vfs` builtin
`status`/`diff`/`commit`/`reset`). Full design: [kaish-overlayfs.md](kaish-overlayfs.md).
Deliberately punted in core (per-file whiteout only, layer-local symlink
resolution, no mtime propagation in `commit_into`) — the live residuals are in
issues.md.

## Binary data & strict UTF-8 (landed 2026-06-13)

kaish was UTF-8 text end to end (`ExecResult.out: String`, string-shaped
`OutputData`, pipes consumed as text), so raw bytes couldn't transit and
`/dev/urandom` couldn't exist. Two intertwined efforts fixed that.

**Strict decodes.** The in-process text builtins used to `from_utf8_lossy`
stdin/files — silently replacing invalid bytes with `U+FFFD`, corruption that
looks like success. Replaced with a strict `ExecContext::read_stdin_to_text()`
(errors on non-UTF-8) wired through every text builtin; text tools now loud-error
on binary while byte-aware tools consume it. A same-pass bug: `accumulate_result`
folded every top-level statement via `push_out(text_out())`, lossy-decoding *any*
`Bytes` final result (so even a standalone `cat blob.bin` came back mangled) — now
concatenates raw bytes when binary is involved. A round-2 DeepSeek+Gemini review
caught more silent-corruption sites (cat as a pipeline's last stage, `$()`/block
capture, command-subst-in-string, `kaish-last`, background-job output, `for` over
`Bytes`, `dd skip*bs` overflow), all fixed with regression tests in
`sandbox_mode_tests`.

**Typed `Bytes`.** Design (nushell-style, full plan in
[binary-data.md](binary-data.md)): a `Value::Bytes`/`OutputData::Bytes` that flows
through pipes, coerces to text iff valid UTF-8 (else loud error), and renders at
the boundary (REPL hex dump, `--json`/MCP base64 envelope). All phases landed
(`e612d69` → `818a22e`): the value + boundary type (dead `Value::Blob`/`BlobRef`
deleted), byte-clean transit + redirects, `dd` + `/dev/urandom`/`/dev/random`,
byte-aware movers (cat/head -c/tail -c/base64/xxd/checksum/wc -c/tee/cmp), and
external-command capture → `Bytes` with raw stdin forwarding. North-star test
(`dd if=/dev/urandom of=/dev/null bs=1024 count=10` copies exactly 10240 bytes;
two draws have differing checksums) lives in `sandbox_mode_tests.rs`; DevFs in
`crates/kaish-vfs/src/dev.rs`. **Decisions:** no generic `encode`/`decode`
(`base64`+`xxd` already bridge; a generic pair invites a basenc format×flag
matrix); no `random` builtin (`dd if=/dev/urandom` covers it).

## Streaming file reads (landed 2026-06-14)

Scan-oriented builtins stopped reading whole files into memory. Mechanism respects
kaish-vfs's runtime-free trait (no `AsyncRead`/tokio in `Filesystem`, so WASI stays
clean): `LocalFs::read_range` does a true positional `seek + take`, and
`ExecContext::read_file_chunked` pulls a file forward in 256 KiB windows. Landed:
`wc` (`a610044`), `checksum` (`770766c`), `grep` single-file simple path
(`0fce8c4`), `cmp` lockstep two-file (`dcb806c`), `cat` single-file piped
(`e220767`). Each has a parity test against the *production* whole-buffer path —
that lesson came from a grep draft that hardcoded `byte_offset=0`/`path=null` in
`--json` and passed a text-only self-comparison. Deliberately NOT streamed
(`sort`/`uniq`/`diff`/`jq` and anything emitting structured `.data` — the
`.data`/`OutputData` channel is whole-value by design).

## Output disk-spill — read-only safety (core fix 2026-06-06)

`OutputLimitConfig` carries a runtime `SpillMode` (`Disk` | `Memory`); `Memory`
truncates in memory with no disk I/O. `Kernel::assemble` auto-forces `Memory` for
`NoLocal` mounts, and (fixed 2026-06-08) for any `with_backend` kernel — a
custom-backend kernel owns no host mounts, so a host spill is always a VFS bypass.
This closed the kaibo gap (kaibo uses `with_backend` and didn't opt into `Memory`).

## Composable help (Phases 1–3, 2026-06-06)

The `kaish-help` crate (concept fragments + `compose`/recipes + byte-stable
`get_help`) became the single source for help content. `syntax.md` is generated +
drift-tested; the REPL welcome, the `execute` tool description, and the embedder
prompt set all compose from it (no hand-rolled prose left). Phase 4 publish-half
done 2026-06-08 (`kaish-help` 0.8.0 on crates.io). Full design + resolved
decisions: [composable-help.md](composable-help.md).

## Test fortification (through 2026-06-14)

The 2026-06-09 Fable 5 systemic review (61-agent fleet) drove a fortification pass.
Closed: destructive rails, the realworld port (48 tests through `kernel.execute`,
which immediately caught the grep/rg value-flag binding bug), the `--json` sweep
with a drift guard, snapshot-isolation races, tautological asserts, external-argv
no-split, kill/wait e2e. Lexer negative tests tightened to assert exact
`LexerError` variants (`2026-06-14`), which surfaced a real diagnostic gap (an
unterminated `"string` emits the generic `UnexpectedCharacter`, not the curated
`UnterminatedString`). Seven never-emitted `IssueCode` variants were removed as
silent aspiration; E011/E006/E007 later re-added *with* real `validate` emitters,
honoring the no-variant-without-an-emitter rule.

## The Feb-2026 P0 bugs (validated fixed 2026-04-16)

The original P0 from the Feb-2026 integration-test pass — unknown-command socket
error, missing `$0`, alias concatenation, subprocess capture, arithmetic token
leak — all validated as fixed and were retired from the punch list.

---

## Standing decisions (don't re-litigate)

- **No rustfmt (2026-06-14, Amy).** The audience for this code is Claude and other
  agents, not human reviewers, so rustfmt's payoff is mostly cosmetic; the compiler
  + `cargo clippy --all --all-targets` are the real gate. A first `cargo fmt --all`
  rewrites ~202 files / ~2127 hunks and no small config shrinks it meaningfully
  (`use_small_heuristics` Off/Max both make it worse; the knobs that would preserve
  the hand-style are nightly-only). No `rustfmt.toml` is added (a dormant one makes
  ad-hoc `cargo fmt --check` fail confusingly). If ever revisited: its own commit,
  pinning `edition = "2024"`.
- **No unquoted token-pasting (2026-06-08).** A run of adjacent *unquoted* lexemes
  is never concatenated into one word — `/tmp/$(echo x).txt` lexes as three tokens.
  The quoted form (`"/tmp/$(echo x).txt"`) is the supported idiom and aligns with
  the `shellcheck --enable=all` north star (SC2086). kaish guides agents to write
  reliable scripts; "always quote interpolated words" is simpler and lint-aligned
  than bash's implicit pasting. (Live polish residuals — redirect-target hint,
  post-`--` glue — are in issues.md.)
- **No generic `encode`/`decode`; no `random` builtin (2026-06-13).** See the
  binary-data section above.
- **No `test` builtin and no `[` command (2026-06-25, Amy).** Conditionals go
  through `[[ … ]]` only. The old `test`/`[` builtins were removed: `[`
  (`Bracket`) was effectively dead (it can't be a command name — `[` lexes as
  `LBracket`, absent from `command_parser`'s `command_name`), and `test`'s
  binary-operator half (`test a = b`, `-eq`, `<`, `>`) never reached the builtin
  because `=`/`!=`/`<`/`>` lex as operator tokens the argv parser rejects in
  argument position — only unary `test -z`/`test -f` actually routed. Rather than
  do the delicate, broad-blast-radius parser change to accept those operator
  tokens as barewords (without breaking assignment `x=y`, glob char-classes
  `ls [ab]*`, or `[[ ]]`), we keep `[[ … ]]` as the single test form. **Why
  `[[ ]]` wins:** it's real grammar the validator sees, so a malformed test /
  unknown operator / unquoted expansion is caught *before* runtime; `test`/`[`
  hide their operators as runtime string args (the late-failure footgun kaish
  exists to remove). The fully-written `test`/`[` builtin (with passing
  *direct-`.execute()`* unit tests — the gotcha CLAUDE.md warns about: they
  bypassed the real lex/parse path) was the tell that this never worked end-to-end.
  **Guard (gemini-batch review of PR #29):** a bare `test` in a subprocess build
  falls through to an external `/usr/bin/test` that evaluates against the *real*
  host FS, bypassing the VFS/overlay — a silent wrong boolean into `if`/`&&`. We
  added validator advisory **W006** (`IssueCode::PosixTestCommand`) that steers to
  `[[ … ]]` and *still runs* (Amy chose warn-don't-reject over a poison stub). It's
  the first agent-surfaced validation warning: warnings were trace-only because
  every external command fires `UndefinedCommand`, so a code now opts into
  surfacing via `IssueCode::surfaces_to_agent` — the seam for the broader P4
  "did-you-mean" pass. Surfacing is dual-path: the streaming frontend gets it via a
  pre-loop `on_output` emission; `kernel.execute` reads it off the aggregate
  `result.err` (the two consumers are disjoint, so it prints exactly once each).

## Accepted risks & waived items (decided, not open work)

These were on the issues.md punch list but reached a verdict — recorded so the
deferral stays a decision, not drift. Reopen only if a real failure surfaces.

- **Non-Linux `kill` keeps PID-based signalling.** `pidfd` is Linux-only;
  elsewhere we `kill(pid, sig)` and accept the PID-reuse race for the direct
  child. Acceptable — kaish runs predominantly on Linux.
- **Process-group kill PID-reuse window.** The PG-wide kill that catches
  grandchildren goes through `killpg(pgid, sig)` — no PGID equivalent of pidfd.
  If a leader is reaped and its PGID reused before `killpg` fires, an unrelated
  group could be signalled. Mitigations (cgroup v2 `cgroup.kill`,
  `PR_SET_CHILD_SUBREAPER`) are significant complexity; deferred until a real
  failure.
- **`JobManager::spawn` busy-waits** with `std::hint::spin_loop()` for immediate
  visibility — works, wastes CPU under contention. Channel coordination would be
  cleaner; no trigger.
- **`head -n -0` / signed-zero line counts (waived).** `-0` lexes as `Int(0)`
  (sign lost at the lexer) and `line_spec` only treats a *negative* Int as
  all-but-last, so `head -n -0` prints nothing instead of the whole file. Not
  fixable without lexer changes to preserve signed zero. Obscure; waived.
- **`mktemp` random-suffix modulo bias (recorded by choice).** `byte % 36` skews
  the first four alphabet chars (~3.1% vs ~2.7%, since `256 % 36 = 4`). Negligible
  for temp suffixes; the rejection-sampling fix complicates the
  fail-loud-on-no-entropy contract.
- **`uname -v` discloses build provenance unconditionally.** Formats
  `kaish {version} ({git_hash} {build_date})` from compile-time `option_env!`; an
  embedder that sets `KAISH_GIT_HASH`/`KAISH_BUILD_DATE` fingerprints the exact
  commit even in a minimal build. Gate behind a `verbose-identity` feature only if
  a threat model cares.
- **`mktemp` entropy-failure message is unhelpful on wasm.** A
  `getrandom::fill` failure on `wasm32-wasip1` surfaces a near-empty `Display`.
  Add a `cfg!(target_arch = "wasm32")` hint if it ever matters.
- **`to_argv()` flattens a repeatable scalar array to one JSON token (no live
  trigger).** `ParamSchema.repeatable` stores repeated single-value flags as
  `named[key] = Json(Array([scalar, …]))`; `to_argv()` only splits the
  *array-of-arrays* (`consumes > 1`) shape, so a flat scalar array becomes one
  JSON-text token. `sed` is unaffected (it reads the raw `ToolArgs`); a future
  repeatable-flag builtin that trusts its clap struct after a `to_argv()`
  round-trip would see one mangled value. The real fix needs schema context in
  `to_argv` (it has none). Record-then-defer until a builtin hits it.
