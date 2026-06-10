# Known Issues & Open Work

Actionable punch list. Narrative context lives in [reviews.md](reviews.md).
Last validation pass: 2026-06-09 (Fable 5 systemic review — 61-agent fleet with
adversarial verification + DeepSeek/Gemini resonance panel; narrative in
[reviews.md](reviews.md), panel report in [resonance-2026-06.md](resonance-2026-06.md)).
Path note: the 0.8.0 crate split moved some cited files — `vfs/local.rs` →
`kaish-vfs`, host `ps` → `kaish-tools-host/src/ps.rs`, Tool/ToolCtx/KernelBackend
→ `kaish-tool-api`.

Priorities follow the convention from `reviews.md`:

- **P1** — high-leverage features and diagnostics
- **P2** — focused refactors
- **P3** — scheduler and infra
- **P4** — eventually

The old P0 from the Feb-2026 integration-test pass (5 concrete bugs:
unknown-command socket error, missing `$0`, alias concatenation,
subprocess capture, arithmetic token leak) **all validate as fixed** on
2026-04-16 and have been removed from this list.

---

## P1 — High-leverage features and diagnostics

### Documentation accuracy sweep — LANGUAGE.md + help content (2026-06-09)
Every claim in LANGUAGE.md was executed against the v0.8.0 binary; help content
checked against code. Verified falsehoods to fix (each needs a "fix doc vs fix
code" decision; bugs with their own entries are cross-referenced):
- `LANGUAGE.md:206`/`:659` vs `:674` — three mutually contradictory statements
  about `[ ]`. Binary truth: `[ expr ]` does **not** parse (`found '-f' expected
  '['`); `test` builtin works; only `[[ ]]` is test syntax. The `:661`
  intentionally-missing row also lists "Aliases" while `:96-100` documents them.
- `LANGUAGE.md:643-648`/`:686` — VFS table lists a `/git/` mount that does not
  exist anywhere (`ls /git` → not found; no GitFs mount in `setup_vfs`); git
  ships as the `git` *builtin* via kaish-tools-git. Table also says `/` is
  "kernel root (cwd)" while `ls /` lists the host root, and omits `/v/`
  entirely. Same stale `/git` story in `vfs.md:21` + `:42-49` (whole section of
  `cat /git/status` examples) and in repo CLAUDE.md's "VFS Router (local,
  memory, git backends)".
- `LANGUAGE.md:707` + `limits.md:33` — claim `head`/`tail -c` counts UTF-8
  characters; implementation is deliberate POSIX **bytes** (`head.rs:163`
  comment says exactly that). Doc records the opposite of a code decision.
- `LANGUAGE.md:720` — "parser combinator library is sourced from git" is now
  false; chumsky is crates.io `1.0.0-alpha.8` (see P4 chumsky entry). The
  resonance panel independently cited this line as adoption risk.
- `limits.md:41` — claims user functions/.kai scripts cannot run in pipeline
  stages, scatter workers, or background jobs. All three work since the fork
  refactor; `scatter.md:33` documents the opposite. Two help topics from one
  corpus disagree.
- `limits.md:64` — printf row documents `a\nb` for `printf "a"; printf "b"`;
  CLI emits `ab`. Mechanism subtlety: `accumulate_result` *does* insert newline
  separators between statement outputs but the `;`-sequence path bypasses it —
  see the P2 entry before editing either side.
- `limits.md:20` — keyword-bareword row's own example (`echo done`) works now.
- `limits.md:28` — `set` options row omits `-o output-limit`.
- `rg.md:12` — `rg -trust` glued short-flag example fails under clap
  ("unexpected argument '-r'"); spaced `-t rust` works. `:48` references
  nonexistent `kaish-jq` (builtin is `jq`).
- `README.md:253-258` — documents a `help` **MCP tool**; the server exposes
  exactly one tool, `execute` (`handler.rs` tool_router); help is a builtin +
  MCP prompts. Topic list also stale (missing ignore, output-limit). The
  resonance panel's #1 convergent finding (MCP JSON contradiction) lives in
  this same section — rewrite it once, fixing both.
- `README.md:189-192` — embedding snippet reads `result.out`, a private field
  (`pub` accessor is `text_out()`); first Rust sample an embedder sees fails to
  compile. Same bug in EMBEDDING.md Quick Start (see next entry).
- `LANGUAGE.md:411-414` — latch message format drifted (no filename on first
  line; nonce now quoted; new `Authorized:` line). Cosmetic but agents
  pattern-match it.
- Stale test-file header comments that misdescribe what's covered:
  `heredoc_compat_tests.rs:10-14` (claims unterminated-heredoc tolerance and
  ignored $()-tests; both now wrong) and `heredoc_tests.rs:10-11` (claims CR/LF
  normalization; tests assert `\r` preserved).
Verified-accurate (no action): README builtin table matches all 86 registered
builtins exactly; all Quick Tour examples run; syntax.md drift test passes and
every spot-checked syntax.md claim verified; timeout/ignore/scatter help claims
verified.

### EMBEDDING.md predates the 0.8.0 split — 4 of 8 samples don't compile
Last touched 2026-05-02 (fb49110). Verified against HEAD:
- Quick Start (`:19`) and README both read private `result.out` → `text_out()`.
- Custom Tools (`:144`) implements removed `Tool::execute(&mut ExecContext)`;
  trait is now `execute(&mut dyn ToolCtx)` (`kaish-tool-api/src/tool.rs:27`)
  with the downcast pattern every builtin uses.
- KaijutsuBackend example (`:75`) calls `LocalBackend::new()` with no args;
  signature requires `Arc<VfsRouter>` (`backend/local.rs:31`).
- Programmatic VFS read (`:473`) passes `&str` where `&Path` is required and
  never imports the `Filesystem` trait.
- Promotes `Kernel::execute_with_vars`, now `#[deprecated]` (`kernel.rs:1080`).
- Silent on everything an embedder now needs: **capability features** (default
  is `localfs` only; every GitVfs example needs non-default `git`, external
  commands need `subprocess` — no mention anywhere), **ExecuteOptions**
  (timeout incl. ZERO dry-run, cancel_token, cwd, W3C trace context — the
  canonical per-call surface, undocumented), `with_backend` hermeticity from
  037aa63 (spill forced in-memory, job files off — embedders relying on disk
  spill or `/v/jobs` persistence must know), `owns_output` from cd23012, and
  the kaish-client `KernelClient`/`EmbeddedClient` surface.
- `JobFs` status documented as `"completed:0"`; actual is `"done:0"`
  (`job.rs:180-186`) — string-matching embedders never match.
- Resonance panel (Gemini): GitVfs/kaijutsu material is ~60% of the doc and
  reads as an app tutorial, not a library guide — move to its own doc when
  rewriting. Panel also wants a stability/semver/MSRV statement and the
  panic-safety line contextualized. See [resonance-2026-06.md](resonance-2026-06.md).
Repo CLAUDE.md/GEMINI.md (byte-identical) share the staleness: crate tree lists
8 of 12 crates — missing exactly kaish-tool-api, kaish-vfs, kaish-tools-git,
kaish-tools-host, the ones an agent needs to find Tool/GitVfs. Also the
"Avoid mod.rs" style rule is contradicted by 7 mod.rs files in kaish-kernel —
scope it ("new modules") or drop it.

### Resonance follow-ups — MCP contract docs (2026-06-09)
Both panel models independently ranked the same gaps top-3; full report in
[resonance-2026-06.md](resonance-2026-06.md). The doc work, in leverage order:
1. README MCP section rewrite: the JSON-either-way vs clean-text-by-default
   contradiction, plus an explicit `execute` **return contract** (envelope
   shape, exit-code table 0/1/2/3, where `.data` lives, truncation recovery).
2. Per-call **lifecycle table**: what resets each `execute()` (vars, functions,
   cwd, aliases) vs what persists (nonce store, trash, init script) — "the
   contract the agent codes against."
3. Surface the existing security model: `allow_external_commands`, capability
   features, with_backend hermeticity are invisible in user-facing docs; Gemini
   read the external-command path as an undisclosed sandbox escape.
4. Agent "Do Not" callouts: inline env vars (`FOO=bar cmd` → `$1`), one
   concrete quote-to-join error example, latch exit-2 → parse nonce recipe.

### `rg` parallel walking
The 2026-04-29 rg builtin uses `ignore::WalkBuilder::build()`, which
yields a sequential iterator. `WalkParallel::run()` is a few lines'
diff plus careful synchronization (work-stealing crossbeam deques,
results merged across workers). High value on large trees — rg's
real-world advantage. Skipped this round to ship the feature; revisit
when single-thread perf bites.

### `rg` async stdin streaming
`rg.rs::run_with_matcher` reads stdin via `ctx.read_stdin_to_string()`
into a `String` and uses `Searcher::search_slice` on the bytes. Fine
for small/medium pipes; blocks the runtime worker on large pipes.
A `SyncPipeReader` adapter (sync `std::io::Read` over async
`PipeReader` via `Handle::block_on`) plus `Searcher::search_reader`
would stream chunks. Expected modest payoff; defer until a real
workload pushes against the buffer.

### Output disk-spill — runtime read-only residuals (core fix done 2026-06-06)
**Core fix landed 2026-06-06** (full narrative now in Resolved). `OutputLimitConfig`
carries a runtime `SpillMode` (`Disk` | `Memory`); `Memory` truncates in memory with
no disk I/O, and `Kernel::assemble` auto-forces `Memory` for `NoLocal` mounts.
Remaining open work:
- `Disk` is still the default for *mode-based* `localfs` kernels built via
  `Kernel::new` (`Sandboxed`/`Passthrough`) — those genuinely own their host
  mounts, so disk spill is legitimate there. The auto-force to `Memory` now also
  fires for **any `with_backend` kernel** (not just `NoLocal`): a custom-backend
  kernel owns no host mounts, so a host spill — or a background-job output file —
  is always a VFS bypass (fixed 2026-06-08, see Resolved; this closed the kaibo
  gap — kaibo uses `with_backend` and did *not* opt into `Memory`, contrary to a
  prior note here). Residual: a mode-based `Sandboxed` kernel an embedder runs
  read-only via `Kernel::new` without `.in_memory()` still spills; none known in
  the wild, and `with_backend` is the read-only embedder path.
- No runtime switch via the `kaish-output-limit` builtin / `set -o` yet — mode is
  config-only. Add a `memory`/`disk` subcommand if interactive control is wanted.
- `host`'s `/proc` and `/etc` reads still bypass the VFS by design — if
  "read-only" is ever marketed as "no host observation," those need a runtime
  gate too.

---

## P2 — Focused refactors & real bugs

### `break 2` silently discards output accumulated before the break
Verified 2026-06-09: a nested loop printing before `break 2` prints **nothing**
(exit 0); single-level `break` keeps its output. `ControlFlow::break_n`
(`control_flow.rs:41-46`) carries a fresh empty `ExecResult`; when the Break
propagates past the inner loop the accumulated output is replaced rather than
merged. Bash prints the pre-break output. `LANGUAGE.md:240` documents `break 2`
with no caveat. Fix: merge the loop's accumulated output into the propagating
Break result; kernel-routed test asserting pre-break output survives.

### `--` does not protect dash-words with internal hyphens; snapshot blesses the bug
`echo -- -not-a-flag` prints `-not -a -flag` (three args; external argv gets
four: `[--][-not][-a][-flag]`). Mechanism: `ShortFlag` regex
(`lexer.rs:438`, `-[a-zA-Z][a-zA-Z0-9]*`) disallows internal hyphens so the
word lexes as three flag tokens, and the post-`--` arm of `args_list_parser`
(`parser.rs:1440-1448`) maps each to its own positional with no rejoin.
**Scope (verified):** only short-dash words with internal hyphens split —
`git checkout -- -file` and `-- --weird-name` survive as one arg each; quoting
is a clean workaround. The snapshot test `parser_double_dash_ends_flags`
(`parser_tests.rs:694-699` + its .snap) pins the *broken* behavior — fixing the
lexer will fail the test rather than the bug failing it. Same tokenization-gap
family as the bare-`,`/digit-range P4 entry. Fix: rejoin dash-bearing words
after DoubleDash (or document the divergence), update the snapshot, and add an
end-to-end `echo -- -not-a-flag` assertion so the contract is explicit.

### `;` sequences bypass the newline output separator — doc's own example is false
`LANGUAGE.md:170` claims kaish separates statement outputs by newlines, with
example `printf "a"; printf "b"` → `a\nb`. Observed: `ab` via the CLI — exactly
the bash behavior the doc disclaims. `&&` chains DO separate (`a\nb`).
Subtlety from verification: `accumulate_result` (`kernel.rs:~4216`) *does*
insert newline separators — the `;`-sequence path evidently bypasses it.
Decide intent (make `;` match `&&`, or scope the doc claim and fix
`limits.md:64`'s matching row). Related undocumented limit found by the same
probe: `$(printf a; printf b)` is a parse error — `;` is not accepted inside
`$()` at all; multi-line `$( )` and `#` comments inside `$()` are also parse
errors with no test pinning either behavior (`skip_command_substitution`,
`lexer.rs:1096`, skips quotes but not comments — same family as the
comment-arithmetic preprocessor gotcha).

### `wait %1` / `kill %N` — documented jobspec syntax is a lexer error
`LANGUAGE.md:481` (`wait %1 %2`) and `:635` (`kill %N`), plus generated
`syntax.md:233`, document syntax that cannot be typed: `%` has no token, so
both are "lexer error: unexpected character". Inconsistent workarounds:
`wait 1` works, `wait "%1"` fails (wait doesn't strip `%`), `kill "%1"` parses
(kill.rs strips `%`) but the `-c` probe couldn't find the job. Fix: lex `%N`
as a jobspec token (preferred — it's what agents' bash priors write) or align
wait/kill on quoted-`%` handling and rewrite the three doc sites to verified
recipes.

### scatter does not fan out plain-text stdin — the docs' canonical examples run one worker
`cat items.txt | scatter --as ITEM ...` (`LANGUAGE.md:607`, `:611-613`) binds
the **entire** stdin to one item — one worker, exit 0, silent. Structured input
fans out correctly (`split ... | scatter` works as documented). The behavior is
deliberate (`extract_items`, `scatter.rs:327-328`: "Raw text without structured
data — one item (no implicit split)") but it contradicts the for-loop
newline-split philosophy adopted later, and the doc's canonical examples
silently degrade. Decide: newline-split plain text in scatter (consistent with
`for`), or fix the examples to `... | split --lines | scatter` and state the
structured-data requirement loudly.

### Destructive-op safety rails have zero kernel-routed test coverage
The latch and trash systems are only tested below the arg-binding layer that
c463f42/ffe0f44 just reworked — verified 2026-06-09 (the flows *work* at HEAD;
this is a missing regression net, not a bug):
- `tests/common/mod.rs:28-32` builds every harness kernel with
  `.with_latch(false).with_trash(false)`; no test anywhere drives
  `set -o latch` → `rm` exit 2 → `rm --confirm=<nonce>` through
  `kernel.execute`. All inline latch tests (`rm.rs:443-589`,
  `kaish_trash.rs:281-307`) inject `confirm` directly into `ToolArgs.named`,
  bypassing lex → parse → clap binding entirely.
- The `RmAction::Trash` **execution** arm (`rm.rs:212-227`) is never run by any
  test: no TrashBackend mock exists, and the real-backend tests are
  `#[ignore]`d as CI-flaky. The "trash failure = error, never fall through to
  permanent delete" invariant is honored by the code but pinned by nothing.
Fix: `tests/latch_trash_tests.rs` with latch-on kernel (exit-2 → parse nonce →
confirm → deleted; bogus nonce → exit 1, file survives), plus a test-only
TrashBackend mock (recording + always-failing variants) covering the
trash-success, trash-failure, and backend-absent paths.

### `realworld_builtin_tests.rs` (43 tests) bypasses the kernel it's named for
The file hand-builds `ToolArgs` and calls `tool.execute()` directly
(`realworld_builtin_tests.rs:56-66`, e.g. `:464`) — the documented anti-pattern
from the kernel-routed-tests convention. It carries the bulk of grep's flag
coverage plus cat/head/tail/wc/ls/jq, none of it exercising lex → parse →
validate → glob pre-expansion → `build_args_async` — the exact layer changed by
ffe0f44/cd23012/c463f42. The intended commands are already in comments above
each test; porting to the `common::kernel_at`/`run` harness is mechanical and
converts 43 vacuous-at-the-binding-layer tests into real ones. Highest-risk
sibling: `rg` — the largest flag surface in the registry (27 params), 15
inline-only tests, zero kernel-routed coverage, post-canonicalization.
Also port-priority: `read` (pipeline/scope semantics are pure kernel
interaction), `env`/`exec` (hermetic-env overlay, two-spawn-site sync).
Phases 1–3 done 2026-06-06: the `kaish-help` crate (concept fragments + `compose`/
recipes + byte-stable `get_help`) is the single source for help content; `syntax.md`
is generated + drift-tested; and MCP `instructions:`, the REPL welcome, the `execute`
tool description, and the MCP prompt set all compose from it (no hand-rolled prose
left in the MCP frontend). Remaining:
- **Phase 4:** publish half **done 2026-06-08** (`kaish-help` 0.8.0 on
  crates.io); remaining: kaijutsu/kaibo adoption (tracked in those repos).
- **Phase 5:** i18n scaffolding + first `ja` fragments.

Full design + resolved decisions: [composable-help.md](composable-help.md).

### Minimal build (`--no-default-features`) — integration test binaries don't compile
**REGRESSED 2026-06-09:** lib unit tests no longer compile minimally either —
`test_touch_existing_readonly_rejects` (`touch.rs:199-214`) does
`use crate::vfs::LocalFs;` at `touch.rs:200` with no `localfs` gate (E0432).
The documented gate `cargo check -p kaish-kernel --no-default-features` still
passes because check doesn't build `#[cfg(test)]` code — add
`cargo test -p kaish-kernel --lib --no-default-features --no-run` to the gate
list so this can't slip again. Consequence flagged by the safety-test audit:
`sandbox_no_native_builtins` (`sandbox_mode_tests.rs:217-229`) only compiles in
the exact configuration that doesn't build, so it is dead code, and the
sandbox configuration is certified by check/clippy only (and there is no CI).
Pre-regression state: lib tests passed minimally as of 2026-06-07
(`cargo test -p kaish-kernel --lib --no-default-features`, 1320 tests).
Still open: the *integration* test binaries
(`tests/*.rs`) don't compile minimally. 8 of 23 files use `KernelConfig::repl()` /
the `kernel_at` harness, both `#[cfg(feature = "localfs")]` (real-FS) — so `cargo
test -p kaish-kernel --no-default-features` (which builds the integration binaries
too) fails to compile those. Options: (a) file-level `#![cfg(feature = "localfs")]`
on the inherently real-FS binaries so they're skipped minimally; (b) convert the
ones that don't actually need real FS (e.g. `validation_tests`) to
`KernelConfig::isolated()` (memory VFS) so they *run* minimally — strictly better
coverage. `hermetic_home_tests.rs` already uses `isolated()` and runs in both modes
as the pattern to follow. Surfaced 2026-06-03; partially resolved 2026-06-07. Folds
naturally into the planned `native`→capability-feature split.

### Split `kernel.rs::execute_stmt_flow`
`kernel.rs:1463`–~1913 (kernel.rs is now 6,838 lines) is a 16-arm async match.
Each arm reaches into `scope`,
`exec_ctx`, and `user_tools` RwLocks; `For`/`While`/`Case` are 100+
lines apiece. Natural refactor: `mod kernel/exec/{assignment, command,
pipeline, control, …}` with `execute_stmt_flow` reduced to a dispatch
arm-per-module.

### Extract `dispatch_command` ctx-sync helper
The six-field `ExecContext` ↔ kernel-state sync appears near every
call site that fields a fork (`kernel.rs:~4100-4140` and duplicates;
citation refreshed 2026-06-09). One helper, one truth.

### No unquoted token-pasting — adjacent bare lexemes are not one word
kaish's lexer/parser does **no token pasting**: a run of adjacent *unquoted*
lexemes is never concatenated into a single word. `/tmp/$(echo x).txt` lexes as
three tokens (`RelativePath` `/tmp/`, `CommandSubst $(…)`, `DottedIdent .txt`)
and `$dir/out.txt` as two (`SimpleVarRef`, path) — there is no word-assembly
step that joins them. The **quoted** form is the supported idiom and works
everywhere because a double-quoted string lexes as one `Interpolated` token:
`"/tmp/$(echo x).txt"`, `"$dir/out.txt"`. This aligns with the
`shellcheck --enable=all` north star, which already requires `"$var"`/`"$(cmd)"`
quoting (SC2086). **Decision (2026-06-08): keep the quoting requirement; do not
add bare word-pasting.** kaish is not aiming to be a human REPL (bash/zsh/fish
fill that niche) — the goal is to guide agents to write reliable scripts, and
"always quote interpolated words" is a simpler, lint-aligned rule than bash's
implicit pasting. Two surfaces expose the no-pasting behavior differently:

- **Redirect target** (`crates/kaish-kernel/src/parser.rs:1540`, `redirect_parser`)
  binds with a single `primary_expr_parser()`, so `> /tmp/$(echo x).txt` is a
  hard parse error (`found '$(' expected redirect, …`). Bare-subst
  (`> $(echo /tmp/x)`) and quoted (`> "/tmp/$(echo x).txt"`) both work.
  *Polish (P4, deferred):* turn the parse error into a "quote the redirect
  target" hint instead of the generic expected-token list.
- **Argv** (next entry) silently splats into multiple args instead of erroring —
  the more dangerous of the two.

Surfaced 2026-06-08 while adding command-substitution support to redirect
targets.

### Unquoted interpolated argv word silently splats into multiple args
Because there is no token-pasting (see previous entry), `echo /tmp/$(echo gen).txt`
parses as **three** positional args and prints `/tmp/ gen .txt` (space-joined),
and `echo $dir/out.txt` prints `/tmp /out.txt` — two args. `command_parser` uses
`primary_expr_parser().repeated()`, so adjacent exprs are each accepted as their
own `Arg::Positional` with no diagnostic. This is worse than the redirect case
because it *silently* produces wrong argv rather than failing. Given the
keep-quoting decision, the right fix is a **validator diagnostic**: flag adjacent
argv exprs with no intervening whitespace (lexer would need to preserve adjacency
spans) and tell the agent to quote — e.g. `E0xx: '/tmp/' and '$(…)' are adjacent;
quote the word as "/tmp/$(…)"`. Until then, the quoting rule in the docs is the
mitigation. Deferred (needs adjacency tracking in the lexer).

---

## P3 — Scheduler and infra

### Process-group kill still has a (small) PID-reuse window
The direct-child kill path now uses `pidfd_send_signal` on Linux (see
`crates/kaish-kernel/src/pidfd.rs`), so the bound generation immunises
us from PID reuse for the spawned child itself. **But** the PG-wide kill
that catches grandchildren still goes through `killpg(pgid, sig)` —
there is no PGID equivalent of pidfd. If the leader is reaped and its
PID/PGID is reused before our `killpg` fires, grandchildren of an
unrelated process group could be signalled. Mitigations: cgroup v2
`cgroup.kill` (Linux ≥ 5.14) for atomic tree kill, or
`PR_SET_CHILD_SUBREAPER` to enumerate descendants and pidfd each. Both
are significant complexity; defer until we see a real failure.

### Non-Linux unix targets (macOS, BSD) keep PID-based kill
`pidfd` is Linux-only. On other unix targets we still send `kill(pid,
sig)` and accept the PID-reuse race for the direct child. macOS has no
direct equivalent (FreeBSD has `procctl`/`pdfork` but the model differs).
Acceptable today since kaish runs predominantly on Linux.

### Sync `build_tool_args` lacks the undeclared-space-flag guard
The async `build_args_async` (kernel.rs) fails loud when an undeclared flag under
a `map_positionals` schema would silently divorce a space-form value (fixed
2026-06-08, see Resolved). The sync twin `build_tool_args`
(`scheduler/pipeline.rs:705`, `unwrap_or(true)` at `:822`; citation refreshed
2026-06-09) shares the same `unwrap_or(true)` default-to-bool
and has **no** equivalent guard. It can't trigger the production bug today — its
only production callers are scatter/gather option parsing (builtin schemas,
`map_positionals=false`); the backend-tool caller is the `#[cfg(test)]`
`BackendDispatcher`. Applying the guard would require changing the return type to
`Result` across ~22 call sites, so it was deferred. If the sync path ever fields a
`map_positionals` backend tool in production, add the same guard (lift a shared
helper) to keep the two builders in sync.

### Eliminate the sync `build_tool_args` twin entirely
Bigger than the guard above: retire the sync arg builder so there's one path to
reason about. Real commands already bind through the async `build_args_async`
(foreground via `execute_command`; in pipelines via `Kernel::dispatch` →
`dispatch_command`). The sync `build_tool_args` survives only in (a) scatter/gather
*option* parsing in `run_scatter_gather` (`scheduler/pipeline.rs`, an `async fn`
that already holds a `&dyn CommandDispatcher`, so it could `await
dispatcher.eval_expr`) and (b) the `#[cfg(test)]` `BackendDispatcher`, plus ~27
in-module test call sites. Eliminating it means giving the scheduler an async
arg-builder (e.g. a `CommandDispatcher::build_args` trait method, or a free
`build_tool_args_async` that routes `$(...)` through `eval_expr`) and converting
those test sites to async. Deferred 2026-06-08 while landing per-subcommand tool
schemas — `select_leaf` lives only in the async builder, so the twin never sees a
subcommand tool and routing isn't at risk; this is purely a "fewer paths to
reason about" cleanup. Doing it would also subsume the guard-parity item above.

### Undeclared space-flag guard covers long flags only (`-t val` still divorces)
The 2026-06-08 fix errors on undeclared `--type value` but not single-char
`-t value`, because short flags followed by a positional (`-r path`, `-f file`)
are overwhelmingly legitimate bool+positional and erroring there would be too
aggressive. Net: an undeclared short flag that *should* take a value still
silently drops it under a `map_positionals` schema. Lower-risk than the long-flag
case (backend/MCP tools rarely expose single-char value flags) and the real fix
is the same — declare the flag in the schema. Revisit if a backend tool surfaces a
`-x VALUE`-style flag.

### `dispatch_command` cancel sync is one-way (in only)
`ec.cancel = ctx.cancel` is synced INTO `self.exec_ctx` at the start of
each dispatch, but there's no `ctx.cancel = ec.cancel` reverse sync at
the end. Today this is fine because the only known mutators of
`ctx.cancel` (the `timeout` builtin) save and restore the original token
themselves. If we add another builtin that wants to propagate a token
swap *outward* through dispatch_command, it would need either explicit
restoration or a reverse sync added.

### Test-effectiveness residuals (2026-06-09 fleet review)
Smaller verified gaps from the systemic review, grouped; the headline test
items got their own P2 entries (destructive rails, realworld port). Each
bullet is independently actionable:
- **`--json` is kernel-route-tested for ~4 of 89 commands** despite being the
  headline MCP feature (`shell_compat_tests.rs:27` explicitly skips it; only
  ps + three kaish-* tools assert shape through the full stack). One
  rstest-parameterized sweep file — run `<cmd> --json`, assert it parses and
  the top-level shape — would cover the registry.
- **`builtin_kernel_tests.rs` is 100% happy-path:** 38 tests, zero nonzero-exit
  assertions. Add 1–2 negative cases per builtin (missing file, bad flag)
  asserting the specific code + err substring — agents branch on exit codes.
- **External-command argv never tested with a space-containing `$VAR`**
  (`external_command_tests.rs` uses only literals/builtin echo). The no-split
  guarantee on the *external* path (`build_args_flat`) is unpinned, and the
  hermetic-env memory note already warns the two spawn sites must stay in sync.
  One Linux-gated `printf "[%s]\n" $X` test closes it.
- **`background_job_snapshot_isolation` doesn't exercise its race**
  (`concurrency_tests.rs:203`): `sleep 0.2; echo $VAR &` backgrounds only the
  echo, so the sleep is foreground and the test passes even under a
  shared-scope regression. Fix: move the delay inside the backgrounded unit
  (`snap() { sleep 0.2; echo $VAR; }; snap &` then mutate immediately).
- **Tautological assertions:** `sandbox_external_commands_blocked`
  (`sandbox_mode_tests.rs:187`, second assert always true — pin exit 127 +
  message so policy-block is distinguishable from accidental failure) and
  `validation_quoted_glob_in_mv` (`validation_tests.rs:468`,
  `is_ok() || is_err()`). The bare-glob validation tests at `:321-391` are
  environment-dependent (transient-kernel cwd) — backstopped by the
  deterministic inline `test_bare_glob_no_matches_errors` (`kernel.rs:6135`),
  but that test only covers the builtin-argv site; the `execute_stmt_flow` and
  `build_args_flat` "no matches" sites lack their own, and the inline module
  is feature-gated such that `cargo test -p kaish-kernel --lib` alone skips it.
- **Lexer negative tests assert only `is_err`** (`lexer_tests.rs:160`
  `run_lexer_error_test`): the float/ambiguous-boolean/unterminated suites
  would still pass if the curated diagnostics regressed to a generic error.
  Thread an expected variant/substring through the rstest cases (the inline
  `backtick_in_source_is_rejected` shows the pattern).
- **7 of 20 `IssueCode` variants are never emitted** (E006 sed, E007 jq, E010,
  E011 diff, W003, W004, W005 — `kaish-tool-api/src/issue.rs`): the advertised
  validations don't exist, so deleting them fails zero tests. Only grep and
  seq implement `Tool::validate`. Either implement + test per code, or remove
  the dead variants (fail-loud over silent aspiration).
- **kill/bg/fg coverage is PTY-only** (unix-gated, timing-sensitive,
  `pty_job_control.rs`) and **`wait` never appears as a command in any
  integration test** (6 inline tests only). `kill %1` + `wait` exit-code
  propagation are doable in `background_execution_tests.rs` without a PTY.
- **Timing margins:** `background_job_does_not_block_foreground`
  (`concurrency_tests.rs:159`, 300ms headroom — the file's only hard
  wall-clock bound) and bare 5-10ms registration sleeps in `job.rs` unit
  tests (`:719/:738/:759`) — poll instead.
- **Bash-compat leg never runs automatically:** with `KAISH_BASH_COMPAT=1`,
  71+27 bash-side tests pass (verified) — but nothing sets it and there is no
  CI workflow at all, so the divergence detector depends on someone
  remembering. (CI is deliberately deferred per project memory; recorded here
  so the deferral stays a decision, not a drift.)

### Test gaps around kill discipline
- No test for `kill_grace = Duration::ZERO` (immediate SIGKILL).
- No test that a user-defined tool (`tool name { ... }`) inside a
  scatter worker dispatches correctly under cancellation — current
  scatter test uses an external `bash -c "sleep 60"`.
- No test for the JC inherit-output path being killed via cancel —
  that path requires a real TTY which is awkward in test infra.

### `ExecuteOptions` callback type is awkward
`Option<&mut (dyn FnMut(&ExecResult) + Send)>` is hard to call. A
trait-object alias or a dedicated `Callback` trait would smooth the
ergonomics. Not blocking; do it once we see real downstream pain.

### Long-blocking builtins other than `sleep` may not honor cancellation
`sleep` is done (see Resolved 2026-06-07). The guidance stands for any *other*
builtin that holds a long time/IO future without yielding through `ctx.cancel`:
audit and apply the same `tokio::select!` pattern where one is found (none
currently known besides the now-fixed `sleep`).

### Job output files in `/tmp/kaish/jobs/` persist indefinitely
`JobManager` only cleans up on explicit `cleanup()` / `remove()`. No
automatic GC of stale files from crashed sessions or old jobs.
Long-running embedders accumulate. Prune on kernel start and on a
configurable interval.

### `JobManager::spawn` busy-waits
Uses `std::hint::spin_loop()` to guarantee immediate visibility. Works,
wastes CPU on contention. Channel-based coordination would be cleaner.

### MCP per-request OS thread + 16MB stack
`crates/kaish-mcp/src/server/execute.rs:192` documents it. Fine at
agent-call rates, will be a problem in a hot loop. Replace with a pool
of LocalSet workers + mpsc channel. Benchmark first to confirm the win.

### MCP resource list always re-traversed
`notify_resource_list_changed` fires unconditionally. Cheap fix;
diff against previous snapshot before notifying.

### MCP resource watcher channel fixed at 256
`subscriptions.rs:33` bounds the file-watch channel; high-churn
environments drop events silently.

### `ToolCtx::backend()` forces a full `KernelBackend` mock for out-of-tree tests
`crates/kaish-tool-api/src/ctx.rs`. `backend()` returns a non-optional
`&Arc<dyn KernelBackend>`, so a third-party tool author who wants to unit-test
their tool must construct a complete `KernelBackend` mock (~16 async methods)
even if the tool never touches I/O. We sidestepped this in-tree by relocating
the git/host tool tests to kernel-routed integration tests — which doesn't help
external authors. Surfaced by the dpal review 2026-06-03. Options: (a) ship a
`#[cfg(feature = "test-util")]` no-op `KernelBackend` + `ToolCtx` harness from
`kaish-tool-api` so any tool crate can spin up a context in one line; (b) make
`backend()` return `Option<&Arc<dyn KernelBackend>>` (kernel always `Some`,
pure-compute/test contexts `None`). (a) is less invasive and keeps the common
path honest. Revisit when the first external tool bundle wants unit tests.

### `ToolCtx::as_any`/`as_any_mut` are a public downcast hatch
`crates/kaish-tool-api/src/ctx.rs`. The escape hatch that lets in-tree builtins
recover the concrete `ExecContext` is exposed on the public trait, so an
out-of-tree tool could in principle downcast to a kernel type — though only if
it deliberately takes a dependency on `kaish-kernel` to name `ExecContext`
(impossible from the leaf API alone), so the practical risk is low. Cheap
hardening flagged by dpal 2026-06-03: mark both methods `#[doc(hidden)]` so they
don't advertise themselves as part of the supported surface. A heavier option (a
kernel-internal extension trait carrying the downcast, keeping `ToolCtx` itself
hatch-free) is more churn than the current need justifies.

---

## P4 — Eventually

### Soften the "sh subset that passes shellcheck" framing
CLAUDE.md (and the README) describe kaish as "a `sh` subset that passes
`shellcheck --enable=all`." That framing is now more aspirational than accurate
and undersells what kaish actually is. The skeleton is sh-shaped — `if/then/fi`,
`for…in…do/done`, `case…esac`, `NAME=value`, `$()`, `$(())`, `${VAR:-}`, pipes,
heredocs — but:

- The dominant test form is `[[ ]]` (LANGUAGE.md:173), which is a bash-ism, not
  POSIX: shellcheck flags it **SC3010** under `sh` dialect. kaish then extends it
  past bash too (quoted-regex `=~ "\.rs$"`, `!~`). Here-strings `<<<` are bash
  (SC3011).
- Core shell *semantics* are deliberately dropped: word splitting, `eval`,
  backticks, process substitution.
- Typed data (floats, strict booleans, JSON), structured `$()`, the `split`
  builtin, E-code diagnostics, and the proposed collections
  ([arrays-and-hashes.md](arrays-and-hashes.md)) are modelled by **no** shellcheck
  dialect at all.

Net: kaish is neither a strict POSIX-`sh` subset nor a bash subset. "Passes
`shellcheck --enable=all`" holds only for the genuinely-shared core and only under
`bash` dialect. Recommend reframing the docs to something like *"inspired by POSIX
sh and bash, and informed by shellcheck's lints"* rather than implying compliance.
Corollary worth stating in the same place: shellcheck offers **zero** coverage for
kaish's extensions by construction, so the kaish validator is the sole safety net
for them (e.g. discarded-`append`, `len`/`in` type mismatches). One nice point in
kaish's favour — bare `$arr` = whole value is exactly the bash footgun shellcheck
warns about with SC2128 ("expanding an array without an index"), designed out.

### `mktemp` random suffix has slight modulo bias
`random_suffix` (`tools/builtin/mktemp.rs`) maps random bytes onto a 36-char
alphabet with `byte % 36`. Since `256 % 36 = 4`, bytes 252–255 land on the first
four chars, giving them ~3.1% vs ~2.7% probability. Negligible for temp-file
suffixes — the 36^N search space is dominated by N, not the per-char skew — so
not worth the rejection-sampling loop (which also complicates the
fail-loud-on-no-entropy contract). Flagged by dpal review 2026-06-03; recorded
rather than fixed by deliberate choice.

### `uname -v` discloses build provenance unconditionally
`tools/builtin/uname.rs` formats the version field as
`kaish {version} ({git_hash} {build_date})` from compile-time `option_env!`
values. If an embedder sets `KAISH_GIT_HASH`/`KAISH_BUILD_DATE`, `uname -v`
fingerprints the exact commit and build time even in a minimal build. Not host
info, but a build fingerprint; gate behind a `verbose-identity`-style feature if
a threat model cares. Flagged by dpal review 2026-06-03.

### `mktemp` entropy-failure message is unhelpful on wasm
On `wasm32-wasip1`, a `getrandom::fill` failure surfaces an opaque
`getrandom::Error` whose `Display` may be near-empty, so the
`mktemp: could not obtain system entropy: {e}` message loses its detail. Minor;
add a `cfg!(target_arch = "wasm32")` hint to the message if it ever matters.
Flagged by dpal review 2026-06-03.

### `touch <dir>` on a local mount fails (EISDIR); memory mounts succeed
Hardening on 2026-06-07 routed `touch`-on-existing through the VFS via a new
`set_mtime` op (Filesystem + KernelBackend), removing the old
`resolve_real_path` escape to the host and its silent no-op on virtual mounts
(read-only mounts now reject loudly; `MemoryFs` updates its per-entry mtime).
Residual: `LocalFs::set_mtime` opens the path with `write(true)` (preserving the
old `update_mtime` semantics), so `touch existing_directory/` fails with EISDIR
on a local mount, while `MemoryFs::set_mtime` updates a directory's mtime fine.
POSIX `touch` works on directories. Fix is to bump the time via `utimensat`/an
O_RDONLY fd (works on dir fds; on Linux futimens needs only ownership/write
perm, not a write-mode handle) instead of opening for write — deferred to keep
the cross-platform `File::set_modified` contract unchanged for now.

### Bare `,` / numeric ranges parse oddly — `cut -d,`, `tr -d 0-9` need quoting
Surfaced 2026-05-28 by the kernel-routed builtin tests. Two related
tokenization gaps in the same family as the dot-prefixed-filename entry
above:
- A standalone comma isn't a bareword, so `cut -d, -f2` and `cut -d , -f2`
  both raise a parse error; only `cut -d ',' -f2` works.
- A numeric range like `0-9` lexes as `Int(0)` + `Int(-9)` (the `-` starts
  a negative int), so `tr -d 0-9` is a silent no-op — only `tr -d '0-9'`
  works. Letter ranges (`tr a-z A-Z`) work because `-` is inside the Ident
  char class. (The `tr` help example was fixed 2026-05-28 to quote `'0-9'`.)
Low priority — quoting is a clean workaround and kaish is a POSIX *subset* —
but a "did-you-mean: quote it" diagnostic would smooth bash porting. Decide
whether lone punctuation / digit-ranges that agents reach for as set/delim
arguments should tokenize as barewords.

### Flag injection via glob expansion in `rm`
`rm *` in a directory containing `-rf.txt` expands to `rm -rf.txt …`
and kaish's structured flag parsing then flips `rm` behaviour.
Mitigations already in place: `set -o latch`, `set -o trash`, `rm --
*`. Future fix: the kernel knows which args are `GlobPattern`-sourced
— `rm` could reject flag-shaped positional args from that path.

### Extract `skip_quoted_content()` shared by arithmetic/heredoc preprocessing
`preprocess_arithmetic()` (~150 lines) and `preprocess_heredocs()`
(~150 lines) both implement quote/escape tracking. Also a dependency
of the heredoc span-tracking work under P1.

### `parse_interpolated_string` is ~200 lines
Handles `$VAR`, `${VAR}`, `$(…)` with nested paren tracking. Could
split into smaller helpers for maintainability.

### chumsky alpha → 1.0 before kaish 1.0
Updated 2026-06-09: chumsky moved to crates.io `1.0.0-alpha.8` in `229f363`
(release prep) — the old "pin a git commit" option is moot. Residual: upgrade
to a stable chumsky 1.0 when it ships, before kaish 1.0. `LANGUAGE.md:720`
still claims the parser "is sourced from git rather than crates.io" — now
factually wrong; fix with the doc sweep (the resonance panel cited that exact
line as adoption risk the project has already paid down).

### REPL polish
Syntax highlighting (chumsky already produces structured tokens),
abbreviation expansion, multi-line paste detection. `kaish-repl`
highlighter and hinter are currently no-ops; multi-line paste
re-validates on each keystroke.

### Bash-prior "did-you-mean" validator pass
Agents will write `[ -z "$VAR" ]` (single bracket) when `[[ ]]` is
preferred. The biggest historical bash-prior trap — `for i in $LIST`
silently iterating once — is now caught: bare `$VAR` in for-position
is E012 hard error, and `$(cmd)` in for-position splits on `\n`. What
remains is "soft" guidance for less-common ports; a shellcheck-style
actionable warning without rejection would steer without frustrating.
Optional but moderate leverage now that the loud cases are handled.

### `EmbeddedClient::shutdown()` is a no-op
Intentional (embedder owns lifecycle) but worth one doc line in
`EMBEDDING.md`.

### Multi-file `grep` silently skips unreadable explicit operands
`grep_multiple_files` does `Err(_) => continue` per file — right for the
recursive walk it was written for, but for explicit operands
(`grep p real.txt typo.txt`) a missing/unreadable file should report
`grep: typo.txt: …` and affect the exit code, like POSIX grep. Same
silent-skip shape as the multi-arg `ls` entry below. Low priority.

### Multi-argument `ls` silently skips inaccessible paths
`Ls::render_names` (`ls.rs`) skips any path that fails to `stat` — correct
for the glob race it was written for (a match removed between walk and
stat), but for an explicit multi-arg call like `ls real.txt gone.txt` it
drops `gone.txt` with no error, where bash prints `ls: cannot access`.
Single-arg `ls gone.txt` still fails loudly (separate code path). Low
priority; decide whether multi-arg should accumulate per-path errors.

---

## Resolved since previous lists

Captured here so context from `cleanups-todo.md` / old `issues.md`
isn't lost when those files are deleted.

- **`${VAR:-"default"}` default-word quote removal — fixed 2026-06-10.** The
  default word was passed verbatim to `parse_interpolated_string`, so quote
  characters survived into the value (`${NAME:-"default"}` → `"default"`). The
  quotes are shell syntax, not data. Added `unquote_default_word` in `parser.rs`
  (drops quote delimiters; double quotes keep interpolation, single quotes
  suppress it via the `__KAISH_ESCAPED_DOLLAR__` marker) applied at all three
  default-word sites (`parse_var_expr` + the two interpolated-string parsers).
  Bash-cross-checked compat tests in `shell_compat_tests.rs`
  (`default_word_*`, 7 cases incl. single-quote interpolation suppression and
  value-wins). `LANGUAGE.md:32`'s example now reads true.

- **`[[ ! A || B ]]` precedence — fixed 2026-06-10.** `!` was parsed by
  `ignore_then(compound)` where `compound` is the full or-level parser, so it
  negated the entire rest of the expression (`!(A || B)`) instead of just the
  next term. Empirically `[[ ! -f /nonexistent || -d /etc ]]` returned false
  (should be true). Fix: a nested `recursive` makes the bang arm recurse at the
  unary level only (`(!A) || B`), matching the grammar comment and
  `LANGUAGE.md`. Two parser snapshots (`test_not_with_and`,
  `test_complex_compound`) had blessed the buggy AST and were re-blessed to the
  correct precedence; behavioural regressions in `shell_bugs_tests.rs`
  (`test_bang_binds_tighter_than_or/_and`, `test_bang_single_term_unaffected`).

- **Output-limit runtime control is now reachable from the shell — fixed 2026-06-10.**
  The four-part bug cluster the 2026-06-09 doc-accuracy pass surfaced is closed,
  each with kernel-routed coverage in `tests/output_limit_control_tests.rs`:
  - **`set -o output-limit=SIZE` parses.** The set grammar
    (`parser.rs`, `set_with_flags`) only accepted flag tokens and bare
    identifiers, so the `name`/`=`/`value` token triple was a parse error. Added
    a `set_option_assign` arm that folds the three tokens back into one
    `name=value` positional (the form `set.rs` already parses), plus a
    `set_quoted_arg` arm so `set -o "output-limit=8K"` works too.
  - **`kaish-output-limit set N` parses.** `set` is a keyword token and was
    absent from `keyword_as_bareword` (`parser.rs:primary_expr_parser`), so it
    couldn't be an argument — the `set` subcommand (and `echo set`) were parse
    errors / silently mis-parsed. Added `Token::Set => "set"`; the `set` *builtin*
    is still matched only when the token leads a statement, so nothing is shadowed.
  - **Runtime mutations persist.** `execute_command`'s sync-back (`kernel.rs`)
    synced scope/cwd/aliases/pipes back into `self.exec_ctx` but **not**
    `output_limit`, so a builtin's change was dropped before `dispatch_command`
    could read it back. Added the one missing `ec.output_limit = ctx.output_limit`.
  - **Docs re-trued to the real 8K default** (was 8K in code, 64K in docs):
    `output-limit.md` MCP-limit row + `on` default, `limits.md` set-options row,
    and `LANGUAGE.md`'s set-builtins note (which now lists `-o output-limit`).

- **`touch .hidden.txt` / dot-prefixed filenames — fixed (validated 2026-06-09).**
  The old P4 entry is closed: dot-prefixed names now lex as a single
  `DottedIdent` bareword (`lexer.rs:378-384`, the argv-barewords work,
  `074ad98`). Verified at HEAD: `touch .hidden.txt && ls .hidden.txt &&
  echo .parent` all behave; POSIX `. file` source alias still works (pinned by
  the bare-`.` Resolved entry below). Quoting is no longer needed.

- **grep `-E` no-op — closed as decided 2026-06-09.**
  The old P2 entry's preferred end state (option a + c: keep the accept-and-
  ignore for muscle-memory compatibility, note it in help text) is implemented:
  `grep.rs:74-76` doc comment ("No-op: Rust's regex crate is always extended …
  accepted for POSIX/muscle-memory compatibility") surfaces in `help grep` via
  the clap-derived schema. Nothing left to do unless someone reports surprise.

- **`with_backend` kernels now refuse host side channels — output spill + job-output leaks closed 2026-06-08.**
  Two paths reached the real host filesystem via `std::fs`, bypassing the VFS (and
  any read-only mount): output disk-spill (`paths::spill_dir()`) and background-job
  output files (`Job::write_output_file` → `std::env::temp_dir()/kaish/jobs`). The
  auto-force to `SpillMode::Memory` only triggered on `VfsMountMode::NoLocal`, but the
  read-only embedder path is `Kernel::with_backend` (kaibo, kaijutsu) with a
  `Sandboxed`-flavored config — so neither guard fired and a >limit `cat`/`rg` (8 KB
  in kaibo) spilled project bytes to host `/tmp`, and `cmd & ; wait` wrote job output
  there too. Both bypass the read-only `LocalFs` mount kaibo relies on. Realized the
  "dedicated runtime read-only flag" the `assemble` comment anticipated, structurally:
  `assemble` takes a `no_host_filesystem` bool (`true` from `with_backend`, since a
  custom-backend kernel owns no host mounts so *any* host write is a bypass; `false`
  from `Kernel::new`, where `NoLocal` still triggers via `vfs_mode`). When set it
  forces `SpillMode::Memory` **and** `JobManager::set_persist_output_files(false)`.
  Jobs stamp the flag at registration (`spawn`/`register`/`register_with_streams`);
  `Job::wait` skips the host write when off — live output stays in-memory via the VFS
  streams (`/v/jobs/{id}/stdout`) and the cached `ExecResult`, so nothing is lost
  in-process. Tests: `scheduler::job::tests::test_no_host_output_file_when_persistence_disabled`
  (mechanism) and `background_execution_tests::test_job_output_persistence_disabled_for_hostless_kernels`
  (wiring: `NoLocal` + `with_backend` → off, `Sandboxed` → on). The `host` `/proc`/`/etc`
  *reads* remain a separate, by-design, `host`-gated disclosure (P1).

- **Stdin redirect (`< file`) now reads through the VFS, honors `$PWD`, and fails loud — fixed 2026-06-08.**
  `setup_stdin_redirects` (`scheduler/pipeline.rs`) read the target with
  `std::fs::read_to_string` — bypassing the VFS entirely (host-fs leak in sandbox
  mode, ignored VFS mounts), not resolving against cwd, and silently swallowing a
  missing/unreadable file (`if let Ok(content)`) so the command ran with empty
  stdin instead of erroring. Now the helper is `async`, resolves the target via
  `ctx.resolve_path` and reads via `ctx.backend.read` (same path as `cat`), and a
  missing file or non-UTF-8 content is a hard error (exit 1, `redirect: …`
  message). Both call sites updated: `run_single` returns the failure directly;
  the pipeline-stage path surfaces it from inside the spawned task. Two regression
  tests (`sandbox_stdin_redirect_reads_via_vfs_and_cwd`,
  `sandbox_stdin_redirect_missing_file_fails_loud`). Command substitution in
  redirect targets was a follow-on fix (see next entry).

- **Command substitution now runs in redirect targets and heredoc bodies — fixed 2026-06-08.**
  Redirect-target and heredoc-body evaluation went through the synchronous
  `eval_simple_expr`/`eval_string_parts_sync`, whose `CommandSubst` arm was a
  no-op (no pipeline execution available), so `cat < $(echo x)`,
  `echo x > $(echo f)`, and `$(...)` inside a heredoc body silently dropped the
  substitution. Fix (design chosen with the maintainer): added
  `async fn eval_expr(&self, expr, ctx)` to the `CommandDispatcher` trait — the
  Kernel delegates to the existing `eval_expr_async` (snapshots scope/cwd, runs
  the pipeline, restores; only output escapes), the test `BackendDispatcher`
  keeps its sync-only behavior. `eval_redirect_target` is now async, returns
  `Result`, and routes through `ctx.dispatcher.eval_expr` (already populated on
  every `ExecContext`), falling back to the sync evaluator only when no
  dispatcher is attached. All four output-redirect arms in `apply_redirects` and
  the stdin/heredoc/here-string arms in `setup_stdin_redirects` were converted to
  the async fail-loud path. Un-ignored the two `heredoc_tests` command-subst
  tests; added `sandbox_redirect_target_command_substitution`. **Remaining:**
  command substitution *embedded in* an interpolated redirect path
  (`> /tmp/$(echo x).txt`) still fails to parse — see P2 below; the bare-subst
  form (`> $(echo /tmp/x.txt)`) works.

- **Output redirects now resolve relative targets against `$PWD` — fixed 2026-06-08.**
  `redirect_write`/`redirect_append` (`scheduler/pipeline.rs`) handed the raw
  redirect target straight to `ctx.backend.write` with no cwd resolution, unlike
  every other path operand. The router normalizes a bare relative path by
  prepending `/` (`vfs/router.rs`), so `echo x > f` wrote `/f`, not `$PWD/f`. Two
  symptoms: redirects ignored `cd` (`cd /sub; echo x > f` landed at `/f`), and
  write/read disagreed — `echo x > f` then `cat f` failed because `cat` resolves
  against cwd while the redirect resolved against `/`. Surfaced from kaibo, where
  it made a sandbox test vacuous: a relative `>` into a read-only project landed
  in the ephemeral `MemoryFs` `/` mount instead of being refused. Fix: both
  helpers now call `ctx.resolve_path(path)` (the same cwd-relative, normalized
  resolution `cat`/`cp`/etc. use) before handing the target to the backend;
  absolute targets are unaffected. Two regression tests pin it
  (`sandbox_relative_redirect_honors_cwd`, `sandbox_relative_append_honors_cwd`):
  relative write+read agree at `$PWD/f`, and `cat /f` still fails.

- **Space-form flag values to backend tools no longer silently dropped — fixed 2026-06-08.**
  `kj context create exp --type explorer` (space form) silently lost the value
  while `--type=explorer` (equals form) worked — a **privilege-escalation-by-typo**
  once kaijutsu went deny-by-default (asking for read-only `explorer`, silently
  getting permissive `default`). Root-caused to kaish, not kaijutsu: in
  `build_args_async` (kernel.rs) an *undeclared* flag — one the tool's schema
  doesn't list as a non-bool param — defaults to `is_bool = true`
  (`unwrap_or(true)`), so `--type` became a value-less boolean flag and `explorer`
  was orphaned, then misrouted by `map_positionals` to a different param slot. The
  `=` form survives because `--type=explorer` lexes as `Arg::Named` and bypasses
  schema lookup entirely. Confirmed empirically: `{named:{name:"context"},
  flags:{type}, positional:[create,exp,explorer]}`. Since kaish genuinely cannot
  distinguish `--type explorer` (flag wants value) from `--force file.txt` (bool +
  positional) without a complete schema, the fix **fails loud rather than guessing**
  (crash > corruption): an undeclared long flag under a `map_positionals` schema
  that is immediately followed by an unconsumed positional now errors with a
  copy-pasteable fix — `kj: --type is not a declared flag … Use --type=explorer, or
  have kj declare --type in its schema`. Surfaces like a validation failure (Err →
  exit 1, message to stderr). Six guard tests pin the safe paths (declared flag
  still binds, `=` form binds, bool-at-end ok, flag-before-flag ok, builtins
  unaffected); one red-first test pins the loud error. The real binding fix remains
  for kaijutsu to declare its `kj` flags in the schema, after which the existing
  `consume_flag_positionals` machinery handles space form. Two residuals tracked
  below (sync `build_tool_args`; single-char short flags).

- **Output disk-spill no longer bypasses the VFS — core fix done 2026-06-06.**
  `output_limit.rs` used to write spill files via `paths::spill_dir()` + `tokio::fs`
  directly (not through `ctx.backend`), ignoring the VFS mount mode — exploitable for
  exfil staging, disk-fill DoS, or persistent artifacts with no feature beyond the
  default `localfs`, and invisible to the compile-time capability split (runtime path).
  Surfaced by dpal design review 2026-06-03; live-confirmed from kaibo's read-only
  `run_kaish` 2026-06-06. Fix: `OutputLimitConfig` gained a runtime `SpillMode`
  (`Disk` | `Memory`). `Memory` (builder `OutputLimitConfig::mcp().in_memory()`)
  truncates overflow in memory to a head+tail preview with **no disk I/O** — the
  streaming external path (`drain_in_memory`) keeps only a head + a `tail_bytes` ring
  buffer while draining to EOF, and oversized structured `OutputData` is rendered
  through a `write_canonical` byte budget rather than materialized into a full
  `String`, so memory stays bounded regardless of output size. Memory mode still
  remaps the exit to `3` (`did_spill = true`, real code in `original_code`); callers
  treat `3` as "output capped, not error". Disk vs memory distinguished by the `out`
  message ("truncated in memory … no spill file" vs "full output at <path>").
  `Kernel::assemble` auto-forces `Memory` whenever `vfs_mode == NoLocal` (overriding
  an explicit `Disk`), so a memory-only kernel can never write a host spill file;
  documented on `VfsMountMode::NoLocal`, `SpillMode::Disk`, `OutputLimitConfig::in_memory`.
  Residual follow-ups (localfs default, runtime switch, host /proc/etc) tracked in P1.

- **Dispatcher re-entrancy deadlock resolved — verified/pinned 2026-06-07.**
  `timeout 5 echo works` (and any builtin that re-dispatches through
  `ctx.dispatcher`) no longer deadlocks. The fix — drop the `exec_ctx.write()`
  guard before `tool.execute` via a snapshot-and-release in
  `execute_command_depth` (`kernel.rs` ~L2252) — already landed with the
  cancellation/concurrency rework (`719896f`); the issue text was stale. Audit
  confirms there is exactly one `tool.execute` site and it runs with no
  `exec_ctx` lock held. Hardened coverage:
  - Un-ignored the three `timeout::tests::*` that were quarantined for "lexer
    rejects `5s`/`100ms`" — that's also stale (`5s`/`100ms`/`500ms` lex fine
    now via the NumberIdent/bareword work), so all 7 timeout tests run.
  - Added `test_redispatch_does_not_deadlock`, which wraps the re-dispatch in
    `tokio::time::timeout(10s)` so a *future* regression (holding the guard
    across `tool.execute`) fails cleanly in 10s instead of hanging the suite —
    verified by temporarily re-introducing the guard (failed at exactly 10.00s)
    then reverting.
  - Fixed an adjacent bug surfaced by un-ignoring `test_timeout_builtin_times_out`:
    when the timer fired on a cancellation-aware *builtin* (`sleep` returns
    "sleep: interrupted"), timeout's `if err.is_empty()` guard masked the
    "timed out" reason. Now the authoritative "timed out after N" note is
    always surfaced, *appended* to any inner detail (no data loss).

- **Job output filenames now include the OS pid — flake + cross-process collision fixed 2026-06-07.**
  `write_output_file` named files `session_{session}_job_{id}.txt` in the shared
  `/tmp/kaish/jobs/`. `session_id` is a *process-local* atomic that restarts at
  0, so two kaish processes on one host — or two `cargo test --all` binaries —
  both wrote `session_0_job_1.txt` and clobbered each other. That was a real
  production cross-process collision *and* the source of the
  `test_cleanup_removes_temp_files` flake. Filenames now mix in
  `std::process::id()` (`session_{s}_job_{i}.{pid}.txt`), mirroring
  `output_limit`'s spill convention — unique across processes. Chosen over the
  issue's suggested per-test `tempfile::tempdir()` because pid namespacing fixes
  the production bug too, with a smaller change than threading an injected dir
  through the three `Job` constructors. (The separate "job files persist
  indefinitely" GC item in P3 is unchanged.) `--all` tests green.

- **`MemoryFs` `ensure_parents` TOCTOU closed — fixed 2026-06-07.**
  `ensure_parents` took and released its own write lock, then the caller
  re-locked for the mutation — a window where a concurrent task could remove or
  replace a parent dir between setup and mutation (`write`, `mkdir`, `symlink`,
  `rename`). Replaced it with a sync `ensure_parents_locked(&mut entries, path)`
  that operates on the caller's already-held guard, so each op does
  parent-creation + mutation atomically under one lock. `rename` keeps its
  intentional error-ignore (`let _ =`). The fix is structural (single guard) —
  a deterministic race test isn't feasible without flakiness, which this project
  avoids; the 37 existing `vfs::memory` tests cover the parent-creation
  behavior. clippy + `--all` tests green.

- **Builtin `sleep` honors cancellation — verified/pinned 2026-06-07.**
  `tools/builtin/sleep.rs` already raced `tokio::time::sleep(d)` against
  `ctx.cancel.cancelled()` (returns 130 on cancel) — the issue description was
  stale. Added `sleep::tests::test_sleep_honors_cancellation` (an
  already-cancelled token makes a 1h sleep return 130 near-instantly) so a
  regression to a bare `.await` fails the suite. The general "audit other
  long-blocking builtins" guidance is retained in P3.

- **Kernel no longer reads host `HOME` — hermetic by default — fixed 2026-06-07.**
  The construction-time `std::env::var("HOME")` read in `kernel.rs` is gone:
  `HOME` is owned entirely by the frontend via `initial_vars` (REPL/MCP seed it
  from `std::env::vars()`; a hermetic embedder leaves it empty and gets no
  HOME). Tilde expansion no longer reads the host env either — `expand_tilde`
  and `apply_tilde_expansion` now take `home: Option<&str>`, sourced from the
  kernel scope via a new `Kernel::scope_home()` and threaded through
  `build_args_async` / `build_args_flat` / `consume_flag_positionals`. With no
  HOME in scope, `~`/`~/path` stay literal instead of leaking the host home
  dir. `cd` with no args dropped its `std::env::var("HOME")` fallback (scope
  only, else `/`). `~user` → `/etc/passwd` is unchanged (already gated behind
  `host`). Tests: `interpreter::eval::tests::expand_tilde_*` (incl.
  `expand_tilde_hermetic_no_home_does_not_leak_host`) and the new
  `tests/hermetic_home_tests.rs` (e2e: seeded vs hermetic kernel through
  `kernel.execute`). Gates green in default + `--no-default-features` + WASI.

- **`kaish-tool-api` public types are now `#[non_exhaustive]` — fixed 2026-06-07.**
  Added `#[non_exhaustive]` to the data types out-of-tree tools construct and
  pattern-match: `ExecResult`, `ToolArgs`, `ToolSchema`, `ParamSchema`,
  `OutputData` (all `kaish-types`), `BackendError` (errors should be
  non-exhaustive), and `ValidationIssue` (`kaish-tool-api`). `OutputFormat`
  already carried it. Adding a field/variant in a minor release no longer
  breaks downstream exhaustive matches or struct literals. Blast radius was
  small — the types already routed through constructors; only `ParamSchema`
  needed new builders (`new` + `with_required`/`with_default`/
  `with_description`/`with_positional`), and the clap reflector + a few test
  helpers moved off struct literals onto them.

  **Deliberately not annotated:** the enums kaish matches heavily *in-tree*
  (`EntryType`, `IssueCode`, `Severity`). `#[non_exhaustive]` on those would
  force `_` arms in the kernel's cross-crate matches and *lose* the compiler's
  exhaustiveness check — a worse trade than the external-stability gain for
  enums external tools read but don't extend. Gates green: `--all` tests,
  clippy, `--no-default-features`, WASI.

- **Here-string `<<<` ambiguity now surfaces an actionable parse error — fixed 2026-06-07.**
  Two stdin sources on one command (`cat < a <<< b`, `cat <<< a <<< b`) used to
  fail with the generic "expected '=', or '('" — the error from the competing
  statement-level *assignment* alternative. Empirically confirmed that a
  `try_map` rejection inside `command_parser` cannot win this: chumsky's
  `choice` merge keeps the assignment alternative's error regardless of the
  span our custom error carries (even a valid `cat foo`, forced to error in
  `try_map`, surfaced the assignment message at 4..7, not ours at the command
  span). So the rule moved to a **post-parse structural scan** in
  `parse()` (`first_ambiguous_stdin`): `command_parser` now always builds the
  command, and `parse()` rejects any command with >1 stdin source with a clear
  message. Kept at parse time (not the validator) because validation is
  skippable (`skip_validation`) while `setup_stdin_redirects` is silently
  last-wins — parsing is the non-bypassable gate. Span is best-effort
  (start-of-source; redirects carry no AST span — precise columns would need
  spanning `Redirect`, deferred). Test:
  `parser_tests::ambiguous_stdin_surfaces_actionable_message`.

- **`gather` line-format no longer silently drops failures — fixed 2026-06-07.**
  `scheduler/scatter.rs` line format dropped failed workers via
  `.filter(|r| r.result.ok())` while JSON kept them as rows with an `"ok"`
  field — same data, fewer rows depending on `--json`, and a caller iterating
  `for x in $(gather)` saw fewer items than it scattered (silent data
  corruption). `gather_results` now returns a `GatherOutput { text,
  dropped_failures }`: the line format still renders only successful stdout
  rows (clean iteration data) but reports the failed item names, and `run`
  turns a non-empty `dropped_failures` into a loud non-zero exit (`code 1`)
  with an err naming the failed items, short-circuiting before post-gather so
  the truncated set isn't propagated downstream. JSON is unchanged (it already
  carries failures as rows, so `dropped_failures` stays empty). Tests:
  `scheduler::scatter::tests::{test_gather_results_lines_reports_dropped_failures,
  test_gather_results_json_keeps_failures_as_rows}`.

- **Non-finite `Value::Float` no longer collapses to null — fixed 2026-06-04.**
  `value_to_json` mapped NaN/Infinity to `null` (JSON has no representation for
  them), silently losing data on roundtrip. It now serializes the non-finite
  value to its string form (`"NaN"`, `"inf"`, `"-inf"`) so the information
  survives — favoring visibility over a silent fallback. Finite floats are
  unchanged. Tests: `result.rs` `value_to_json_finite_float_is_number`,
  `value_to_json_non_finite_float_serializes_to_string`.

- **`--json` now holds on the error path — fixed 2026-06-04.**
  `apply_output_format` early-returned when stdout was empty, so a failure with
  a populated `err` (e.g. `grep --json --bogus-flag`) leaked the message as
  plain text despite `--json`. The empty-stdout branch now emits a JSON error
  object `{"error": <err>, "code": <code>}` (and mirrors it onto `.data`) when
  the result is a failure with a non-empty `err`. A clean non-zero exit with no
  message (grep no-match, exit 1) and an empty success both stay empty — only
  diagnostic-bearing failures are wrapped. Tests in `output.rs`:
  `apply_output_format_emits_json_error_object_on_failure`,
  `apply_output_format_leaves_clean_no_match_empty`,
  `apply_output_format_empty_success_stays_empty`.

- **`printf` now cycles its format over extra operands — fixed 2026-06-04.**
  POSIX `printf` reuses the format string until all operands are consumed
  (`printf '%s\n' a b c` → `a\nb\nc\n`); kaish made a single pass and dropped
  the rest. `format_string.rs` now factors the per-pass loop into `format_pass`
  (returns the conversion count) and adds `format_string_cycling`, which repeats
  the format in operand-count chunks. A zero-conversion format prints exactly
  once (matches bash, guards the infinite loop); the final pass may run short
  and defaults missing operands. `printf.rs` calls the cycling variant; awk's
  `sprintf` keeps the single-pass `format_string`. Tests: `format_string`
  `test_cycling_*` (six cases) + `printf::test_printf_cycles_format_over_extra_args`.

- **Trace-context egress + the two minor OTel leftovers — done 2026-06-01.**
  The remaining OTel work landed in one batch:
  1. **Egress baggage merge (kernel).** `Kernel::run_inner` now echoes the
     embedder's incoming `ExecuteOptions.baggage` back onto the returned
     `ExecResult.baggage` via `telemetry::merge_egress_baggage`. Merge
     semantics: **tool-emitted entries win** on key collision (freshest
     execution-time value; the embedder already holds its own copy), and
     non-colliding embedder keys are added so the result is a single complete
     view. Unit tests in `telemetry.rs` + an egress assertion in
     `trace_context_tests.rs`.
  2. **MCP round-trip.** `ExecuteResult` gained a `baggage` field
     (`from_exec_result` carries it, omitted from the wire when empty), and the
     `execute` handler mirrors it onto the response `_meta` as a `baggage`
     object — symmetric with `trace_from_meta`'s ingress lift. Tests:
     `from_exec_result_carries_baggage`,
     `execute_echoes_baggage_onto_response_meta`,
     `execute_without_baggage_leaves_meta_unset`.
  3. **REPL env parity.** `kaish-repl::trace_options_from_env` reads
     `TRACEPARENT` / `TRACESTATE` / `BAGGAGE` (W3C baggage-header format) and
     the non-interactive entry points (`kaish script.kai`, `kaish -c '…'`) now
     route through `execute_with_options_streaming`, so `otel-cli exec -- kaish
     …` traces across the boundary. Interactive REPL still doesn't (no upstream
     trace in interactive use). Pure parser unit-tested via an injected env
     getter. (Exposing these in Kaijutsu's embedding is tracked there, not in
     the kernel.)
  4. **Forked spans nest under the execute span.** `bind_current_context` now
     captures the active execute span's OTel context (via the
     `tracing-opentelemetry` bridge, now a normal kernel dep) instead of the
     ambient `Context::current()`, so forked work (background jobs, scatter
     workers, concurrent pipeline stages) nests under the foreground span
     rather than becoming a sibling under the embedder's remote parent. Baggage
     propagation is preserved (the bridge stores `parent_cx.with_span(span)`);
     falls back to `Context::current()` when no bridge layer is installed.
     `trace_context_tests.rs` asserts workers parent onto a kaish-local span,
     not the remote parent.

- **`grep PATTERN f1 f2 …` searched only the first file — fixed 2026-05-28.**
  Same multi-positional class as `ls`: grep read only `get_string("path", 1)`,
  so `grep match a.txt b.txt` (and `grep p *.rs` after kernel glob
  pre-expansion) silently ignored every file after the first — matches in
  later files vanished from both text and `--json`. Now collects all
  `positional[1..]` operands and routes ≥2 files through the existing
  `grep_multiple_files` renderer (filename-prefixed, `show_filename: true`).
  Single-file and stdin paths unchanged (no prefix, POSIX). Regression tests:
  `builtin_kernel_tests::{grep_searches_all_files_and_prefixes_filenames,
  grep_glob_searches_all_matched_files}`.

- **Bare `.` argument parsed as `source` — fixed 2026-05-28 (parser).**
  Surfaced by the kernel-routed builtin breadth tests: `find .`, `ls .`,
  `echo .`, `wc -l .` all misbehaved — `echo .` printed `source: missing
  filename` instead of `.`. `primary_expr_parser` (every argument position)
  omitted bare `Token::Dot`, so a `.` argument was never consumed; the
  statement parser then started a fresh `source` (`.`) command. `find .`
  became two statements: `find` (defaulting to cwd → an unfiltered recursive
  listing) plus `.` (source, no file). Fix: accept `Token::Dot` as the
  literal `"."` in expression/argument position. The `source` alias is
  unaffected — `command_parser` consumes a *leading* `.` as the command name
  before args are parsed. Regression tests:
  `parser_tests::{bare_dot_argument_is_one_command, leading_dot_is_still_source}`
  and `builtin_kernel_tests::{dot_argument_is_literal_not_source,
  ls_dot_lists_current_directory, find_name_filters_to_matches_recursively,
  source_alias_still_works_in_command_position}`.

- **`cp SRC... DST/` trailing-slash failure — fixed 2026-05-28 (lexer).**
  Surfaced by the new kernel-routed builtin tests: `cp a.txt b.txt dest/`
  failed with `cp: dest: is a directory (use -r to copy)` though `... dest`
  worked. Root cause was the *lexer*, not `cp`: `RelativePath`'s bare form
  (`lexer.rs:367`) required ≥1 char after the slash (`...]+`), so `dest/`
  lexed as `Ident("dest")` + `Path("/")` — turning `cp a b dest/` into a
  4-operand command where `cp` took `/` as the destination and `dest` as a
  source. Changed the quantifier to `*` so a trailing slash stays attached
  (absolute `Path` already allowed it). Same tokenization-gap family as the
  dot-prefixed-filename and bare-`,` P4 entries. Regression tests:
  `lexer_tests::lexer_navigation_tokens` (`dest/`, `src/kaish/`) and
  `builtin_kernel_tests::cp_multiple_sources_into_directory_trailing_slash`.

- **`ls` multi-positional / glob-drops-all-but-first — fixed 2026-05-28.**
  The kernel pre-expands a bare glob into one `positional` per match, but
  `Ls::execute` read only `positional[0]` via `get_string("path", 0)`, so
  `ls crates/*/Cargo.toml` and `ls a.txt b.txt` listed only the first item.
  `execute` now collects all positionals and branches: a single target keeps
  the rich behavior (glob / file-as-name / dir contents / recursion) via a
  new `list_one`; multiple targets render one node per argument (directories
  shown by name, not expanded — the predictable structured-output choice)
  via a new `render_names` shared with `list_glob`. The old direct-`.execute()`
  unit tests never caught this because they passed raw glob strings straight
  to the builtin, bypassing kernel pre-expansion. Fixed alongside a new
  kernel-routed test harness (`tests/common::kernel_at`/`run`) and
  `tests/ls_tests.rs` (13 tests through the full kernel pipeline over a
  tempdir; 3 reproduced the bug red-first).

- **Concurrency — done (2026-04-10).** `execute_lock` + `Kernel::fork`
  land; `BackendDispatcher` demoted to `#[cfg(test)]`. See
  `reviews.md` Finding 1.
- **Scatter workers can run user tools / `.kai` scripts** — resolved
  by the fork refactor. Help text (`scatter.md:33`) updated in
  `7fb3d18`.
- **`ls` `too_many_arguments`** — resolved; schema-driven approach
  avoids the parameter count.
- **Old Feb-2026 integration-test bugs** — all five (socket-bind on
  unknown command, `$0` in script mode, alias-in-script concatenation,
  `$(kaish -c …)` capture, arithmetic token leak in subprocesses)
  validate as fixed on 2026-04-16.
- **Drop redundant `with_output_and_text(text(x), x)` in `tail`/`head`**
  — 2026-04-16. Both call sites (`tail.rs:175`, `head.rs:210`)
  switched to `with_output(OutputData::text(x))`.
- **`Token::is_keyword()` / `Token::starts_statement()` gaps** —
  2026-04-16. `is_keyword` now covers `While`/`Return`/`Break`/
  `Continue`/`Exit`; `starts_statement` now covers `While`. Unit
  tests added in `lexer.rs`.
- **`VfsRouter::read_only()` aggregation** — 2026-04-16. Returns
  `true` iff every mount is read-only; empty router stays `false`.
  Tests added in `vfs::router::tests`.
- **Scatter `limit=N` has no upper bound** — 2026-04-16. Clamped to
  `1..=10_000` (`SCATTER_LIMIT_MAX`) with `tracing::warn` on
  clamp-down. Help doc updated.
- **Adopt `ExecContext::expand_paths` in `ls`** — 2026-04-16.
  `list_glob` now uses the shared helper. Recursive `grep` was
  misidentified in prior reviews — it uses `FileWalker` for recursion,
  not the glob-expansion pattern, so no change is warranted there.
- **`#[ignore]` on pre-existing failing tests** — 2026-04-16.
  `test_exec_builtin` (exec replaces libtest binary) and three
  `timeout::tests::*` (`5s`/`100ms` lexer rejection) now carry
  `#[ignore = "..."]` with the reason. `test_timeout_numeric_duration`
  (distinct failure, P2) and `test_symlink_absolute_target_escape_blocked`
  (security-relevant, P2) were intentionally not silenced.
- **`test_symlink_absolute_target_escape_blocked` — fixed 2026-04-18.**
  `vfs/local.rs::symlink` was calling `self.resolve(target)` to validate
  absolute targets, but `resolve` strips the leading `/` and joins with
  root, so `/etc/passwd` always resolved to `<root>/etc/passwd` and the
  containment check trivially passed. Replaced with a direct
  canonical-target vs canonical-root comparison; non-existent absolute
  targets fall back to the literal path (rejected when outside root).
- **Heredoc unit-test expectations — fixed 2026-04-18.** `098f3fe` fixed
  the heredoc lexer to preserve the trailing newline and updated the
  integration tests in `heredoc_tests.rs`, but missed the seven unit
  tests in `lexer.rs` (`heredoc_simple`, `heredoc_multiline`,
  `heredoc_with_special_chars`, `heredoc_strip_tabs`, `heredoc_in_command`,
  `test_heredoc_preserves_leading_empty_lines`,
  `test_heredoc_quoted_delimiter_sets_literal`). Expected `content` strings
  now include the trailing `\n`.
- **`test_timeout_numeric_duration` — root cause identified, fix deferred
  2026-04-18.** The test was `#[ignore]`'d with a pointer to the P2
  "Dispatcher re-entrancy deadlock" entry, which captured the full topology
  and fix direction. *(Superseded — the deadlock is resolved as of 2026-06-07;
  see the entry at the top of this section.)*
- **Here-string `<<<`** — 2026-04-16. New `Token::HereString`,
  `RedirectKind::HereString`, parser arm in `redirect_parser()`, and
  stdin wiring in `setup_stdin_redirects` (append `\n`, match bash).
  Parser rejects any combination of `<`, `<<`, `<<<` on one command
  rather than "last wins". Snapshot and E2E coverage; see
  `docs/plan-here-string.md`.
- **`${VAR.field}` for JSON variables — decided-against 2026-04-16.**
  Discussion landed on keeping jq as the one field-access path.
  `jq -r '.key' <<< "$R"` is bash-idiomatic, shellcheck-clean, and
  kaish's jq is built in (native jaq, no subprocess). Pointer:
  `docs/plan-here-string.md`.
- **Clap-migration underscore-id leak + multi-positional sweep** —
  2026-05-28. Two clap-migration follow-ups found by post-fix audit.

  (a) **Underscore-id routing bug.** Builtins with `_field` (Rust's
  unused-field convention) leaked `_field` as the clap id, so the
  kernel's flag canonicalization emitted `--_field=value` and clap
  rejected it. Broke `kaish-validate -e EXPR`, `kaish-validate --expr
  VALUE` (space form), `read -p PROMPT`, `kaish-trash --confirm VALUE`
  (space form). Bool flags worked because the short-flag branch inserts
  the short char directly. Fix: added `#[arg(id = "name", ...)]` to ~21
  fields across diff/grep/kaish-trash/mkdir/read/tee/uname/validate.
  Side benefit: `help mkdir` / `help uname` no longer show `_parents`
  / `_s` etc. — schema names are clean. Regression test in
  `clap_schema::tests::id_override_strips_leading_underscore_from_schema_name`
  + integration tests for `kaish-validate -e` / `--expr` round-trip.

  (b) **Multi-positional sweep.** The pre-existing
  `args.get_string("path", 0)` pattern only read positional[0]; `mkdir
  a b c` silently dropped `b` and `c`. Converted mkdir/touch/rm/tee/cut
  /sort/stat/dirname/realpath/readlink to iterate `args.positional` via
  `crate::interpreter::value_to_string`. rm batches multi-path latch
  into a single nonce (NonceScope.paths is a set; one nonce validates
  any subset). cp/mv adopted POSIX `SRC... DST/` semantics: when more
  than one source is given, dest must be an existing directory. basename
  / uniq / validate kept single-positional per POSIX. Integration tests
  cover mkdir / touch / rm / dirname multi-arg behavior.

  Note: `ls <glob>` still has the same `positional[0]`-only shape and
  needs a paired fix that also decides how to render multiple roots —
  see the surviving P2 entry above.

  Workspace 1554 kernel unit + 129 REPL integration tests green; clippy
  clean (--all and --no-default-features); WASI build OK.

- **Schema fidelity + positional-index mismatch (clap migration)** —
  2026-05-28. The paired P2 from 2026-05-27 landed in a single batch:
  1. `ParamSchema` gained a `positional: bool` field (serde-skipped
     when false). New `ParamSchema::positional()` builder for hand-
     written schemas; jq_native's `filter` re-insert now uses it.
  2. `params_from_clap` now exposes hidden *positional* sinks while
     still skipping hidden *flag* args (clap's `is_positional()` is the
     oracle — `get_index()` returns None for derived `Vec<String>`
     positionals). Positional params are tagged accordingly.
  3. `validate_against_schema` (`traits.rs:42-150`) was refactored to
     split params into positional and flag groups and match positionals
     by their order *among positionals only*, never by the struct-field
     index. Required-positional and required-flag checks were split so
     each goes through its own matching path.
  4. `schema_param_lookup` (`scheduler/pipeline.rs:514`) now excludes
     positional params from the flag-name lookup so the kernel doesn't
     mis-route `cat --paths foo.txt` into named[paths]=foo.txt and
     starve `cat`'s positional[0] read.
  5. ~40 builtins' positional sinks were un-hidden with descriptive
     names + user-facing doc comments (`paths`, `pattern`, `command`,
     etc.). `help cat` now shows `paths : string` with description.
     A handful that conflict with same-named flags (mktemp `--template`,
     patch `--file`) or accept no real positionals (scatter, gather,
     pwd, kaish-status, …) stay hidden — documented inline.

  Unit-test coverage: clap_schema tests verify positional/flag
  detection; traits validator tests pin the index-mismatch regression
  (`required_positional_satisfied_when_positional_sits_after_flags`).
  Full workspace 1543 unit + integration suites green; clippy clean
  (--all and --no-default-features); WASI build OK.

- **Clap-migration review fixes** — 2026-05-27. Five findings from a
  code-review pass on commits 800a55b … 93fbd12 (clap sweep + parser
  split) landed in a single batch:
  1. `grep -F` no longer silently routes through regex matching;
     pattern is now `regex::escape`'d when -F is set (was: `grep -F
     "192.168.1.1"` matched `1X168Y1Z1`).
  2. `--json` is honored before `tool.execute()` runs via
     `GlobalFlags::apply_from_args(&tool_args, ctx)` in kernel.rs and
     dispatch.rs, so a clap parse-failure path doesn't drop the format
     on the floor. Per-builtin `parsed.global.apply(ctx)` is now
     idempotent backup.
  3. `validate_user_tool_args` in `validator/walker.rs` now counts
     `Arg::WordAssign` as positional (user tools aren't on the
     WordAssign allowlist, so runtime stringifies WA to positional —
     validator must agree).
  4. `flagify_bool_named()` hoisted from 12 byte-identical inline
     copies to a `ToolArgs` method in kaish-types. Documented behavior:
     `Bool(true)` → flag presence, `Bool(false)` dropped (matches
     clap's "absent ≡ false" model).
  5. Migrated builtins' long-flag names converted from snake to
     kebab (`--ignore_case` → `--ignore-case`); both forms accepted
     via `visible_alias` for back-compat. `params_from_clap` now
     exposes the long-flag name as an alias so the validator and
     kernel `schema_param_lookup` recognise the kebab form.

  Two paired findings (schema fidelity + positional index mismatch)
  and one limitation (`apply_output_format` empty-stdout early-return
  suppressing --json on errors) recorded as new P2 entries.

- **Heredoc span tracking** — 2026-04-16 (commits `c21e09c`
  source, `2490127` tests). Interpolated heredoc bodies now carry
  per-part byte offsets via `SpannedPart` and a new `Expr::HereDocBody`
  AST variant; validator attaches the span to any issue raised inside
  the body. `ParseError::format(source)` renders `line:col` with a
  source snippet. Literal heredocs and double-quoted strings keep the
  spanless paths for now — universal spanning is a separate refactor.
  The `skip_quoted_content()` extraction called out as a dependency
  was not needed for this fix; it remains a standalone P4 cleanup.
