# issues.md

Living working document for the kaish repo: what is in flight, what is next,
and who owns what. GH issue numbers refer to github.com/tobert/kaish.
Ephemeral per-session state lives in `signoff.md` (gitignored); durable
decisions land in `CHANGELOG.md` and PR bodies.

## In flight

### Bare-assignment exit status (kaijutsu trap #1) — fix on `fix/assignment-subst-status`

`x="$(cmd)" || x="FALLBACK"` never fired: an assignment with no command name
returned success unconditionally, so a failed command substitution could not
trip `||`, `&&`, or `set -e`. Fix: each substitution notes its exit code on
the scope as it completes; the assignment takes the last note (or 0), writes
`$?`, and honors errexit — the `Stmt::Test` arm is the template it mirrors.
Rule (re-probed against bash): for a command with **no command name**, the
exit status is that of the **last command substitution performed**, or 0 if
there were none. A command with a name takes that command's status. Last
wins, not first, not "any failed":

```
x=$(false)                 rc=1     x=5                        rc=0
false; x=5                 rc=0     x="$(false)$(true)"        rc=0
x="$(true)$(false)"        rc=1     x=$(false) true            rc=0
```

Deliberate divergence kept: kaish's `local x=$(false)` propagates the status
where bash's `local` masks it (the SC2155 footgun).

Found on the way: **assignments do not parse inside an unquoted `$()` body**
(`echo $(y=1; echo $y)` errors at the `=`; the quoted form re-parses with the
full grammar and works). Same grammar story as #194 — folded into the 0.15
train, not this fix. Also unparseable there: `set -e` and adjacent unquoted
`$(a)$(b)` words.

## Next up

- **kaijutsu trap #2 is a docs task** — document the numeric-coercion rule
  (most contexts coerce: `03`→3 number, `"03"` string) and the inconsistency
  alongside it: `1e2` and `0x10` stay strings. Verified good:
  `[[ "08" -lt 6 ]]` is correctly false, no octal trap.
- **`-0` silently loses its dash on render** — the lexer reads `-<digit>` as
  Int, so `-0` renders `0` and `xargs -0` plans with args `["0", ...]`.
  Adjacent notes (HEAD~3 tilde, bare leading `+`) belong to GH #193;
  cross-check there rather than filing new.

## The 0.15 train

- **Compound statements in pipelines** — plan approved
  (`~/.claude/plans/encapsulated-wibbling-dahl.md`): compound-in-pipeline +
  #194 + #187 as one piece; variables persist (no subshell); streaming kept
  with scope merged back in stage order; `exit` is always an immediate exit.
  Steps 1, 2, 2b shipped in 0.14.1 (#325 #326 #327 #329).
- **#194 step 3: compounds in unquoted `$()`** — parser-only, runtime ready.
  The constraint is build-time: the stmt→…→cmd_subst→stmt cycle must use one
  threaded `recursive()` handle. Three routes analyzed (thread the handle,
  re-parse from source, capture tokens and parse at parse time); decide
  before coding. Tension: GH #255 — the parser is rebuilt per `parse()` and
  is 62% of allocations; measure with `alloc_profile`.
- **Adjacent gap, same day:** `if`/`while` conditions cannot hold pipelines
  either, and there are two independent pipeline grammars; fixing one leaves
  the other broken. Confirmed 2026-08-15 while probing trap #1: assignments,
  `set -e`, and adjacent unquoted `$(a)$(b)` words also do not parse inside
  an unquoted `$()` body (the quoted path re-parses the full grammar).

## Fleet lane (道場) — check before starting

- **Perf trio: #255 grammar dedupe, #256 clap rebuilds, #257 `Value::Json`
  clone.** Their "before" heap profiles are archived off-repo and irreplaceable
  once the branches land; they need a real home when this work starts.
- **`ps` silently swallows operands** — `crates/kaish-tools-host/src/ps.rs`
  declares a hidden clap sink and never reads `args.positional`, so
  `ps 1234` lists every process, exit 0. Sink-without-read is the bug; the
  kernel builtins pair the sink with a real read and are fine. kaish-extras
  PR #29 is the worked example. Reported by the kaish-extras lane; in the
  道場 queue.

## Needs Amy's word

1. extras#4 facade/plumbing fork (moltar `kaish-extras-git-skel`).
2. Scatter `--json` gap (recorded in #283's body) — ask before filing.
3. #293's capability reduction — say the word if a live surface should return
   sooner than the /proc idea.
4. CLAUDE.md deferred-work ladder text (queue: `~/exomemory/queue.md`).
5. The test-only tempfile PR.
6. Irreversible + outward-facing steps (tag push, `cargo publish`, release
   pages) need their own word each time; a merge word does not cover them.

## Backlog pointers (GH)

- Fidelity/edge bugs: #192 builtin punch list, #193 lexer edges, #195
  VFS/walker edges, #196 patch residuals, #233 binary-boundary stragglers,
  #223 jq binary envelopes, #181 OverlayFs residuals, #180 overwrite
  residuals, #190 scheduler/job residuals.
- Structure/refactors: #186 kernel.rs split, #187 parser cleanup, #182
  resolver O(n²), #255/#256/#257 perf trio.
- Features: #194 compounds in `$()`, #246 scatter jobs, #242 KernelClient job
  methods, #200 embedder-API polish, #203 did-you-mean, #202 REPL polish,
  #197 --no-ignore, #178 preflight, #175 multicall, #228 host /proc reads,
  #225 date wasm timezones, #185 help i18n, #184 test coverage.
- Housekeeping: #119 crates.io yanks, #276 VFS seam.
