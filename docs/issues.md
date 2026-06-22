# Known Issues & Open Work

Actionable punch list — **open work only**. Landed-work narrative and standing
decisions live in [devlog.md](devlog.md); per-version history is in `CHANGELOG.md`.
Path note: the 0.8.0 crate split moved some cited files — `vfs/local.rs` →
`kaish-vfs`, host `ps` → `kaish-tools-host/src/ps.rs`, Tool/ToolCtx/KernelBackend
→ `kaish-tool-api`.

Priorities: **P1** high-leverage features/diagnostics · **P2** focused refactors
& real bugs · **P3** scheduler and infra · **P4** eventually.

---

## P1 — High-leverage features and diagnostics

### Port useful `rg`-only features into `kaish-glob` / `grep` (Amy, 2026-06-17)
`rg` was removed (80%-rule: one search builtin). Some dropped flags are worth
re-homing in `kaish-glob` (which already wraps `ignore::types::Types`) and
surfacing through `grep`/`glob`/`find` rather than a second search builtin:
- **`--type`/`-t`/`-T`** (file-type filters) — strongest candidate; the `Types`
  machinery already exists, so it's mostly surface wiring on the walkers.
- **`--hidden`** — kaish-glob already has dotglob-style hidden handling; thread it
  through a `grep --hidden`.
- **`--no-ignore`** (bypass gitignore), **`--max-count`** (early-stop).
Design first: decide whether these live on `grep` flags or on the file-walking
layer so search and listing share one type/hidden/ignore model. Not urgent.

### OverlayFs residuals
(Core landed — see devlog.) Open:
- **External commands under overlay** fail with exit 127 (real_path=None guard);
  a friendlier in-band error naming the overlay would help. (P3)
- **`kaish-mounts --json` shape changed** (breaking): bare array → `{mounts,
  budget?}`. Flag in release notes. (P2)
- **Punted in core** (revisit if a consumer hits them): cross-layer symlink
  resolution is layer-local; directory whiteout is per-file only; `commit_into`
  doesn't propagate mtimes. JobFs resident-counting skipped deliberately (its
  ring buffers are already bounded). (P4)

### Binary-data residuals (NOT silent corruption — visible/loud)
(Typed `Bytes` landed — see devlog.) Open:
- **Buffered-`String` stdin** (`ExecContext::stdin: Option<String>`): `< binfile`
  and binary here-strings/heredocs error or stringify to `[binary: N bytes]`.
  Making `ctx.stdin` hold bytes is the real fix; rare in practice.
- **Var interpolation** `"$x"` where `x` is `Value::Bytes` → `[binary: N bytes]`
  placeholder (visible marker, not lossy). Could loud-error if `value_to_string`
  grew a fallible path.
- **Output-limit spill preview** still `from_utf8_lossy`s the head/tail preview
  text (the spilled file itself is correct raw bytes). Cosmetic — switch the
  preview to a hex dump.
- **`wc -l`** counts `str::lines()` not `\n` — overcounts by 1 with no trailing
  newline; **`wc -m`** on binary over-counts via U+FFFD. Whole-file reads
  (`cmp`/`cat`/…) can OOM on huge inputs (general kaish model); add a size cap if
  it bites.

### Output disk-spill residuals
(Core fix landed — see devlog.) Open:
- A mode-based `Sandboxed` kernel an embedder runs read-only via `Kernel::new`
  without `.in_memory()` still spills to `Disk`; none known in the wild
  (`with_backend` is the read-only embedder path, already auto-forced to `Memory`).
- No runtime switch via `kaish-output-limit` / `set -o` — mode is config-only. Add
  a `memory`/`disk` subcommand if interactive control is wanted.
- `host`'s `/proc` and `/etc` reads bypass the VFS by design; if "read-only" is
  ever marketed as "no host observation," gate those at runtime too.

---

## P2 — Focused refactors & real bugs

### Repeatable flags: glued short-flag form still overwrites (`-es/a/b/`)
The `-e A -e B` and `--expression=A --expression=B` forms accumulate correctly,
but the *glued* short-flag value path in `kernel.rs` (parses `cut -f1`, `head -c5`)
still does an unconditional `named.insert` of a single `Value::String`. So
`sed -es/a/b/` then `-e X` overwrites the accumulated array. Nobody glues `-e`'s
value, but it's the same silent-drop class. Fix: route the glued path through
`push_repeatable_value` when the matched param is repeatable.

### `sed -i` (in-place edit) — design decided, ready to build with tee/patch
Both lite models in the sed usability panel reached for `sed -i 's/…/…/' file` —
the strongest remaining ergonomic gap. In-place editing is a file-mutating side
effect that must route through kaish's write machinery (VFS resolution, overlay
transactions, latch/trash rails) so sandbox/`--overlay`/confirm modes all hold.
`sed -i` is *always* a truncating overwrite of an existing file, so it inherits
the tee/patch "truncating overwrite gates" rule below. Today `sed` loud-errors on
`-i`. **Amy decisions (2026-06-21):**
- **Confirm flag:** `--confirm=<nonce>` across the whole mutation family
  (`tee`/`patch`/`sed -i`) — one mental model, matching `rm`.
- **`-i.bak`:** support the explicit backup suffix to match convention. `sed
  -i.bak` is the user's deliberate, named backup; trash is the *transparent*
  safety net for the gating, not a substitute for the explicit `.bak`.
- **Multi-file:** one nonce scoping the whole path set (like `rm`'s
  `NonceScope.paths`), not per-file.
- **Overlay prior-content capture:** trash snapshots the *overlay's current
  view* (what would actually be lost on overwrite in-transaction), not the
  real-FS baseline.
- **No file operands:** loud error (`sed -i requires file operands`) — in-place
  editing of a stream is meaningless; do not fall back to stdin.
- **Atomicity:** write-temp-then-atomic-rename through the VFS (crash-safe);
  this surfaces an atomic-replace capability question on the `Filesystem` trait.

### `tee` / `patch` bypass the latch + trash machinery (write-model design)
`tee`/`patch` mutate files through the VFS (overlay-safe) but don't honor
`set -o latch` or trash-on-overwrite the way `rm` does — an agent can silently
overwrite a file with no confirm and no recoverable prior copy. NOT a reuse of
`rm`'s `decide_rm_action` ("trash IS the op"); tee/patch need a *pre-write safety
copy then overwrite* — a new `decide_mutation_action → {TrashFirst(path), Latch,
Proceed}` (same priority chain + `/tmp`,`/v` excludes; tee can create a nonexistent
file with no trash). **Amy decisions (2026-06-17):** latch+trash stay ON even in
overlay mode (the protections are about agent-operation safety, not just real-FS
data); truncating overwrite gates, `tee -a` append does NOT (for now); new file →
just write. **Resolved (2026-06-21, with the `sed -i` decisions above):** the
family-wide confirm flag is `--confirm=<nonce>`; in overlay mode trash captures
the overlay's current view of the prior content. Do it with the `sed -i` work.

### Streaming file reads — remaining
(wc/checksum/grep/cmp/cat landed — see devlog.) Open:
- **`base64`** — deferred; only a pipe-case win, the 3-byte/4-char carry is
  fiddly, and big-file base64 is unlikely in kaish.
- **`read_range` re-opens the file per chunk** on LocalFs (open+seek per 256 KiB).
  Cheap relative to the read; a stateful streaming handle would cut syscalls but
  reintroduces the tokio-in-trait problem the chunk approach avoided. Revisit only
  if a profile shows it matters.

### Kernel-routed test depth: `read`, `env`, `exec`
The realworld port runs 48 tests through `kernel.execute`, but `read`
(pipeline/scope semantics), `env`, and `exec` (hermetic-env overlay, two-spawn-site
sync) each have only a single smoke case in the `--json` sweep — no flag-surface
depth. Worth dedicated kernel-routed suites.

### Composable help — remaining phases
(Phases 1–3 landed — see devlog.) **Phase 4:** kaijutsu/kaibo adoption (tracked in
those repos). **Phase 5:** i18n scaffolding + first `ja` fragments. Full design:
[composable-help.md](composable-help.md).

### Split `kernel.rs::execute_stmt_flow`
`kernel.rs:1463`–~1913 is a 16-arm async match (kernel.rs is ~6,838 lines); each
arm reaches into `scope`/`exec_ctx`/`user_tools` RwLocks, and `For`/`While`/`Case`
are 100+ lines apiece. Natural refactor: `mod kernel/exec/{assignment, command,
pipeline, control, …}` with `execute_stmt_flow` reduced to dispatch-arm-per-module.

### Extract `dispatch_command` ctx-sync helper
The six-field `ExecContext` ↔ kernel-state sync appears near every fork call site
(`kernel.rs:~4100-4140` and duplicates). One helper, one truth.

### No unquoted token-pasting — residual polish
(Decision recorded in devlog — keep the quoting requirement.) Live residuals (P4):
- **Redirect target** (`parser.rs`, `redirect_parser`): `> /tmp/$(echo x).txt` is a
  hard parse error; turn it into a "quote the redirect target" hint.
- **Argv**: the glued-positional "quote the whole word" check covers pre-`--`
  positionals only; post-`--` and flag-adjacent-to-positional glue aren't flagged.

### v0.8.4 review residuals (Gemini Pro, 2026-06-14)
- **`diff -C 3 -C 4` miscounts arity** (P4-trivial — `context_steals_positional`
  subtracts 1 for a deduped `-C`). Redundant usage; the execute-time clap backstop
  (`parsed.files.len() > 2`) already catches the real surplus-operand case, so the
  validator heuristic is cosmetic. Fix only if it bites.

---

## P3 — Scheduler and infra

### Pre-0.9 punch-list residuals — low-frequency fidelity gaps (2026-06-17)
Low-frequency, record-then-defer:
- **sed `p` / `s///p` in non-quiet mode** suppress the auto-print → one print where
  GNU/POSIX print twice (invisible in tests — all `p` tests use `-n`).
- **sed `s///0`** silently treated as first-match (GNU errors); **empty `//`**
  compiles an always-match regex instead of reusing the last pattern.
- **awk `"1e"` → 0** (partial-exponent parse + `unwrap_or(0.0)`); **`FILENAME`
  always empty**; **`RS=""` paragraph mode** splits on exactly `"\n\n"`; **`OFMT`
  ignored** (hardcoded `%.6g`).
- **multi-file `head`/`tail` strip the trailing newline** while single-file emits
  one (inconsistent `*_files`).
- **stdin bridge-thread leak / `block_on`-after-shutdown race**: the REPL detaches
  an OS thread blocked in `read(2)` that can't observe `PipeReader::drop` while
  parked (CLI bounded; a long-lived embedder with a never-closing producer would
  leak it). Harden with `select!`/cancellation.
- **`PipeStdinGuard::try_write` silent skip** (`kernel.rs`) and the test-only
  `BackendDispatcher::try_external` stdin-task leak on unreachable error returns
  (missing the `AbortStdinCopyOnDrop` guard the production path has). Make loud.
- **awk multi-char `FS` regex recompiles per record** (`split_record` —
  `Regex::new(&fs)` each record). Cache it keyed on the FS string. Single-char FS
  already avoids it.
- **awk array-element type inconsistency**: `split()` stores `StrNum`, `sub`/`gsub`
  on an element stores `String`, so a mutated element loses its numeric-string
  attribute. Low impact; POSIX is ambiguous.
- **head streaming-vs-buffered binary-stdin divergence**: the streaming path emits
  earlier valid lines downstream before erroring on a non-UTF-8 line (buffered
  emits nothing) and uses a different message. Both loud; cosmetic.
- **sed `-e <numeric>` is dropped**: `collect_expressions` only matches string
  values, so `sed -e 5 -e 's/a/b/'` silently ignores the `5`. Loud when it's the
  only `-e` (empty expr list), silent when mixed. `-e 5` isn't valid sed anyway;
  coerce non-string values to string (or reject loudly). (0.9.0 review, 2026-06-18.)

### `GlobPath::walk_match` globstar recursion has no work bound
`walk_match`/`match_segments` backtrack on globstar with no `MAX_MATCH_CALLS` guard
(unlike `match_bounded` in `glob.rs`, capped at 100_000). Calls are bounded by
`C(n+g-1, g)` — polynomial in real FS depth, but an adversarial pattern
(`a/**/b/**/c/**/…`) against a deep path could blow up. Low risk today (walker only
matches real FS paths; not a regression). If kaish ever matches user patterns
against user-supplied path *strings*, add a call counter.

### `kill -<sig>` bash shorthand (`kill -9 %1`, `kill -STOP %1`) isn't accepted
`kill` takes the signal via `--signal NAME` / `-s NAME` only; `-9`/`-STOP` fail at
clap arg parsing. Fix: bespoke argv handling (à la `set.rs`) stripping a leading
`-<signum>`/`-<SIGNAME>` before clap. Delivery is already in place. **Demoted
P2→P3 (Amy):** the shorthand leans POSIX-y and touches OS signal-number variance;
the loud `--signal NAME %N` form covers the need. Defer.

### Validator's schema-less arg-builder misparses glued value flags (false E-codes)
`build_tool_args_for_validation` (`validator/walker.rs`) is a *third* arg-builder
(beside `build_args_async` and the sync `build_tool_args`). It is schema-blind:
`Arg::ShortFlag("e1d")` becomes `flags={"e1d"}` with no glued-value split, so a
tool whose `validate()` reads positionals semantically misclassifies them. Concrete:
`sed -e1d -e2d file` fails validation with `E006 unknown command: f` — with no `-e`
bound, `collect_expressions` falls back to the *filename* as the expression. (The
piped form `seq 1 3 | sed -e1d -e2d` validates fine — no file operand to misparse —
and the execute path now accumulates correctly.) Pre-existing; surfaced by the
2026-06-21 combined-short-flag work. Right fix is schema-aware validation arg
binding, which converges with the twin-elimination below (there are really *two*
non-async builders to retire). Until then, glued domain-value flags on sed/awk with
a trailing file operand false-error at validation. Affects sed/awk; `grep -ivC`,
`cut -f1` etc. validate fine (their `validate()` doesn't parse positionals).

### Eliminate the sync `build_tool_args` twin entirely
Retire the sync arg builder so there's one path. Real commands already bind through
`build_args_async`; the sync twin (`scheduler/pipeline.rs`) survives only in (a)
scatter/gather *option* parsing in `run_scatter_gather` (an `async fn` that already
holds a `&dyn CommandDispatcher`) and (b) the `#[cfg(test)]` `BackendDispatcher` +
~27 test sites. Eliminating it means an async scheduler arg-builder and converting
those test sites. Doing so closes three divergences the sync path carries today
(all currently un-triggerable because scatter/gather only expose scalar flags with
`map_positionals=false`): it lacks the async path's undeclared-space-flag guard, its
glued short-flag handling (`cut -f1`), and its repeatable/`consumes>1` accumulation
(the sync builder overwrites where the async one accumulates into `Json(Array)`).
Fold the validation arg-builder (above) into the same effort — schema-aware binding
in one place would fix all three surfaces at once.

### Undeclared space-flag guard covers long flags only (`-t val` still divorces)
The guard errors on undeclared `--type value` but not single-char `-t value`
(short-flag+positional is overwhelmingly legit bool+positional). Net: an undeclared
short flag that should take a value still silently drops it under a `map_positionals`
schema. Lower risk (backend/MCP tools rarely expose single-char value flags); real
fix is declaring the flag.

### `dispatch_command` cancel sync is one-way (in only)
`ec.cancel = ctx.cancel` is synced INTO `exec_ctx` per dispatch, but there's no
reverse sync at the end. Fine today (only `timeout` mutates `ctx.cancel`, and it
saves/restores itself). A future builtin propagating a token swap *outward* would
need explicit restoration or a reverse sync.

### Test-effectiveness residuals (2026-06-09 fleet review)
(Headline items closed — see devlog.) Still open:
- **Bare-glob validation backstop is single-site:** `validation_tests.rs:321-391`
  are environment-dependent (transient-kernel cwd), backstopped only by the inline
  `test_bare_glob_no_matches_errors` (`kernel.rs:6135`) — which covers the
  builtin-argv site only; `execute_stmt_flow` and `build_args_flat` "no matches"
  sites lack their own, and the inline module is feature-gated such that `cargo test
  -p kaish-kernel --lib` skips it.
- **`UnterminatedString` diagnostic gap:** an unterminated double-quoted string
  emits the generic `UnexpectedCharacter` (only the complete/interpolated-string
  helper reaches the curated variant); needs a logos fallback rule. (P4)
- **bg/fg coverage is PTY-only** (`pty_job_control.rs`, unix-gated,
  timing-sensitive); no non-PTY coverage.
- **Timing margins:** `background_job_does_not_block_foreground`
  (`concurrency_tests.rs:159`, 300ms headroom) and bare 5-10ms registration sleeps
  in `job.rs` unit tests — poll instead.
- **Bash-compat leg never runs automatically:** `KAISH_BASH_COMPAT=1` passes 71+27
  bash-side tests, but nothing sets it and there's no CI. (CI deliberately deferred
  per project memory; recorded so the deferral stays a decision, not drift.)

### Test gaps around kill discipline
No test for `kill_grace = Duration::ZERO` (immediate SIGKILL); no test that a
user-defined tool inside a scatter worker dispatches correctly under cancellation
(current test uses external `bash -c`); no test for the JC inherit-output path
killed via cancel (needs a real TTY).

### `ExecuteOptions` callback type is awkward
`Option<&mut (dyn FnMut(&ExecResult) + Send)>` is hard to call. A trait-object alias
or a dedicated `Callback` trait would smooth it. Not blocking.

### Long-blocking builtins other than `sleep` may not honor cancellation
`sleep` honors `ctx.cancel`. The guidance stands for any *other* builtin holding a
long time/IO future without yielding — audit and apply the same `tokio::select!`
pattern. None currently known.

### Piped stdin isn't shared across statements in one `kaish -c 'a; b'` call
The consume-once logic in `execute_pipeline` moves the seeded `pipe_stdin` into the
FIRST top-level statement's pipeline; if that statement doesn't read stdin, the
reader is dropped, so a later reader gets nothing — `printf hi | kaish -c 'echo x;
cat'` prints only `x`. A real shell leaves fd 0 shared. Fix: keep the source on the
persistent `exec_ctx` and let whichever command first reads it take it. Niche; defer.

### `PipelineRunner::run_single`'s `stdin` parameter is vestigial
`scheduler/pipeline.rs:362` — `run_single(cmd, ctx, stdin)` is always called with
`stdin: None`; the real stdin travels through `ctx.stdin`. The override branch is
dead. Drop the parameter or note it's forward-looking.

### `ToolCtx::backend()` forces a full `KernelBackend` mock for out-of-tree tests
`kaish-tool-api/src/ctx.rs`: `backend()` returns a non-optional `&Arc<dyn
KernelBackend>`, so an external tool author must construct a complete mock (~16
async methods) even for an I/O-free tool. Options: (a) ship a `#[cfg(feature =
"test-util")]` no-op `KernelBackend`+`ToolCtx` harness from `kaish-tool-api`; (b)
make `backend()` return `Option<…>` (kernel always `Some`). (a) is less invasive.
Revisit when the first external tool bundle wants unit tests.

---

## P4 — Eventually

### `patch` residuals — final-newline intent and file creation (2026-06-22)
Surfaced by the diff/patch code review; both pre-existing, both low-frequency:
- **Final-newline intent isn't honored.** `parse_unified_diff` discards the
  `\ No newline at end of file` marker, and `apply_hunks` derives the result's
  trailing newline solely from the input file — so a patch whose intent is to add
  or remove the file's terminal newline can't. Fixing means threading
  no-newline-at-EOF state through `DiffLine`/hunk parsing. Rare (final-newline-only
  changes); the applied file just doesn't match patch intent (no data loss).
- **No file-creation path.** `patch` reads the target then issues a whole-file
  `Replace`, so a `--- /dev/null` / `+++ b/new.txt` creation diff errors
  (`cannot read 'new.txt'`) instead of creating the file. The whole-file-Replace
  design cements the pre-existing limitation. Add an "old side empty → create"
  branch if agents start emitting creation diffs.

### Control structures inside `$()` are not supported
The `$()` body accepts the full *statement* grammar (pipelines, `&&`/`||`,
`;`/newline, `#` comments) but not `if`/`for`/`while`/`case` — wiring those in would
thread the recursive `stmt` parser through ~17 expression call sites, a large parser
refactor. Exotic in command-substitution position; workaround: compute at statement
level and capture the variable.

### Soften the "sh subset that passes shellcheck" framing
CLAUDE.md and the README describe kaish as "a `sh` subset that passes `shellcheck
--enable=all`." More aspirational than accurate: the dominant test form `[[ ]]` is a
bash-ism (SC3010 under `sh`), here-strings `<<<` are bash (SC3011), and typed data /
structured `$()` / `split` / E-codes / the proposed collections are modelled by no
shellcheck dialect at all. Net: kaish is neither a strict POSIX-`sh` nor a bash
subset. Reframe to something like *"inspired by POSIX sh and bash, informed by
shellcheck's lints."* Corollary worth stating: shellcheck gives zero coverage for
kaish's extensions, so the kaish validator is their sole safety net.

### `touch <dir>` on a local mount fails (EISDIR); memory mounts succeed
`LocalFs::set_mtime` opens the path with `write(true)`, so `touch existing_dir/`
fails EISDIR on a local mount while `MemoryFs::set_mtime` works. POSIX `touch` works
on directories. Fix: bump the time via `utimensat`/an O_RDONLY fd instead of opening
for write — deferred to keep the cross-platform `File::set_modified` contract
unchanged.

### Recursive/tree `--json` carries structure but no size/type metadata
`tree --json DIR` and `ls -R --json DIR` (without `-l`) serialize the directory
*shape* but every leaf is `null` — the tree-JSON shape (`name → children`) has
nowhere to hang per-node metadata, and `tree` never collects sizes. `ls -lR --json`
does carry sizes (table-with-children path). Fix is a shape decision: teach
`tree`/recursive-`ls` to emit table-with-children, or extend the tree-JSON branch
with a per-node metadata slot. (P3)

### `--flag=true` on bool flags rejected for builtins that skip flagify
The explicit `=true` form on a bool flag binds into `named` as `Bool(true)` (the
`Arg::Named` branch skips the `LongFlag` bool check), and the ~20 builtins that
never call `flagify_bool_named` (seq, find, stat, cp, mkdir, …) hand clap a
`--json=true` their plain-`bool` field rejects (`seq --json=true` → exit 2). The
bare `seq --json` works. Better fix: flagify **once in the kernel** before dispatch
and drop the per-builtin calls. The global `--json` stripper also misses the `=true`
form (it scans `flags`, not `named`). (P3)

### Smaller refactors
- **Extract `skip_quoted_content()`** shared by `preprocess_arithmetic()` and
  `preprocess_heredocs()` (~150 lines each of duplicated quote/escape tracking).
- **`parse_interpolated_string` is ~200 lines** (`$VAR`/`${VAR}`/`$(…)` with nested
  paren tracking) — split into helpers.
- **chumsky alpha → 1.0 before kaish 1.0**: on `1.0.0-alpha.8`; upgrade to stable
  chumsky 1.0 when it ships.

### REPL polish
Syntax highlighting (chumsky already produces structured tokens), abbreviation
expansion, multi-line paste detection. The highlighter and hinter are currently
no-ops; multi-line paste re-validates on each keystroke.

### Bash-prior "did-you-mean" validator pass
Agents write `[ -z "$VAR" ]` (single bracket) when `[[ ]]` is preferred. The biggest
historical trap — `for i in $LIST` iterating once — is already caught (bare `$VAR`
in for-position is E012; `$(cmd)` splits on `\n`). What remains is soft guidance for
less-common ports: a shellcheck-style actionable warning without rejection.
