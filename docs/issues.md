# Known Issues & Open Work

Actionable punch list — **open work only**. When an entry ships, delete it: the
landed-work narrative and standing decisions live in [devlog.md](devlog.md);
per-version history is in `CHANGELOG.md`.

> **New deferrals go to GitHub Issues.** As we get ready to announce kaibo, work
> deferred *outside* an active PR should be filed at
> [github.com/tobert/kaish/issues](https://github.com/tobert/kaish/issues), not
> added here — GH is the transparent surface contributors can see. This file is
> being phased out in favor of GH Issues; the entries below are the remaining
> backlog we haven't migrated yet. Don't double-track: interpreter stack/memory
> work already lives on GH —
> [#46](https://github.com/tobert/kaish/issues/46) (recursion depth guard),
> [#47](https://github.com/tobert/kaish/issues/47) (worker-thread stack size),
> [#48](https://github.com/tobert/kaish/issues/48) (allocation/memory pass).
>
> Scoped deferrals discovered *while building a PR* may still be recorded here
> (or in the PR) so they carry with the change; migrate them to GH when convenient.

Priorities: **P1** high-leverage features/diagnostics · **P2** focused refactors
& real bugs · **P3** scheduler and infra · **P4** eventually.

Path note: the 0.8.0 crate split moved some cited files — `vfs/local.rs` →
`kaish-vfs`, host `ps` → `kaish-tools-host/src/ps.rs`, Tool/ToolCtx/KernelBackend
→ `kaish-tool-api`.

---

## Before 0.11.0 (the collections release)

0.11.0's headline is the collections milestone (native literals, read access,
lvalue writes, `push`). These open items sit *on that surface* — fix or
consciously ship-as-known-limitation before the release:

- **[SILENT DATA LOSS] `"$(cmd)"` drops a `.data`-only collection to `""`** — the
  clearest "crash-beats-corrupt" violation on the new surface. Detail under P3.
- **[SILENT] reduced sync path coalesces bad/subscripted collection access** in
  scatter/gather workers (`${#u[tags]}` → silent-0, bad subscript → dropped arg).
  Detail under P2 (collection read-access follow-ups) and the #6 resolver entry.
- **[LOUD gap — documented 2026-07-03]** bracket-path `push` target
  (`push a[b] x`) fails loudly (glob-expands, "no matches") and doesn't work.
  Documented as a known limitation with a working alternative in `help
  collections` and `arrays-and-hashes.md`; the real fix stays open under P3
  ("Bracket-path `push` target").
  **Re-verified 2026-07-03 and dropped from this list, already fixed:**
  deeply-nested glued list literals (`x=[[a] [b]]` — fixed as a side effect of
  the `[[ ]]` test-depth lexer rewrite, `09c1a89`; regression test added) and
  nested `${#path}` / `${path:-default}` (shipped in `28ec480`, before this
  list was written). Neither was actually a live gap by the time this section
  called for a decision — corrected in `arrays-and-hashes.md` ("Known
  limitations (0.11.0)").

Recommendation: land the two silent items above; the one remaining loud gap
(`push` bracket-path target) is documented, not fixed.

---

## P1 — High-leverage features and diagnostics

### `kaish-multicall` binary (deferred from `execute_argv`)
`execute_argv` (the argv-native peer of `execute(&str)`) landed. Remaining: a
busybox-style binary dispatching by `argv[0]` (`ln -s kaish-multicall ~/bin/ln`).
~80 lines, no kernel changes — a second `[[bin]]` in `kaish-repl`, promoted to its
own crate only if it grows. Build when a consumer wants it. Writeup:
[multicall.md](multicall.md) (records the open `&[String]`-vs-`&[Value]` door
question).

### Binary-data residuals (visible/loud, not silent corruption)
- **Buffered-`String` stdin** (`ExecContext::stdin: Option<String>`): `< binfile`
  and binary here-strings/heredocs error or stringify to `[binary: N bytes]`.
  Real fix is bytes-typed `ctx.stdin`; rare.
- **Var interpolation** `"$x"` where `x` is `Value::Bytes` → `[binary: N bytes]`
  placeholder (visible marker, not lossy).
- **Output-limit spill preview** `from_utf8_lossy`s the head/tail preview (the
  spilled file is correct raw bytes). Cosmetic — switch to a hex dump.
- **`wc -l`** counts `str::lines()` not `\n` (overcounts by 1 with no trailing
  newline); **`wc -m`** over-counts binary via U+FFFD. Whole-file reads can OOM on
  huge inputs; add a size cap if it bites.

### Output disk-spill residuals
- A mode-based `Sandboxed` kernel run read-only via `Kernel::new` without
  `.in_memory()` still spills to `Disk` (none known in the wild; `with_backend` is
  the read-only path and auto-forces `Memory`).
- No runtime switch via `kaish-output-limit` / `set -o` — mode is config-only.
- `host`'s `/proc`, `/etc` reads bypass the VFS by design; gate at runtime too if
  "read-only" is ever marketed as "no host observation."

### Script preflight follow-ups
`Kernel::classify_command(name) -> CommandKind` landed (gives embedders the
consent-gate half). Deferred until a 2nd embedder wants them:
- **`Kernel::preflight(src) -> Report`** convenience — a kernel-side walk bundling
  classification per command node. (P3)
- **Validator/runtime special-form divergence.** The validator's
  `is_special_command` set (`true`/`false`/`:`/`readonly`/`local`) suppresses the
  "command not found" warning for names that don't resolve in-process (`readonly`
  is external → exit 127; `:` is a parse error; `local` is parser-level).
  `classify_command` uses the narrower runtime set on purpose. The validator
  warning should warn on `readonly`/`:` or route through `classify_command_name`.
  Warning-only. (P3)

---

## P2 — Focused refactors & real bugs

### Optional latch around subprocess spawn (allowlist + future model review)
The confirmation latch covers the in-process write-model, but an external
`/bin/rm` on `PATH` bypasses it — the gate lives in the builtin. For an embedder
using the latch as a safety hook (kaijutsu's preapproval/model review via
`ExecResult::latch_request()`), the subprocess door is the hole. Proposal: opt-in
latch around *every* `subprocess` spawn, gated by an embedder-supplied allowlist;
anything off-list returns exit-2 + nonce for the same `latch_request()` path.
Later: a hook for static checks + argv model review. Mechanism in the kernel,
judgment in the embedder. Airtight config until then: `subprocess` off.

### Write-model gate residuals
The full family is gated (`rm` + `tee`/`patch`/`sed -i`/`write`/`cp`/`mv`/`dd of=`).
Open edges:
- **cp/mv recursive & into-directory overwrites are ungated.** The gate covers the
  named destination when it's an existing file. A recursive `cp -r`/`mv` merging
  into an existing dir, and `SRC DIR/` overwriting `DIR/SRC`, are NOT gated
  (neither truncates the named target). Real fix: up-front enumeration of every
  clobbered child so one nonce scopes them.
- **`mv` overwrite has no CAS** (gate-snapshot + latch only). Same-mount is atomic
  `rename`; cross-mount is unlink+write with the trash snapshot as recovery. Fine —
  recorded for symmetry.
- **`sed -i.bak` backup suffix** unsupported: the lexer splits `-i.bak` at the dot
  and the value-flag binder consumes the next token greedily, so GNU's
  glued-optional-suffix isn't modelable post-lexing. Trash snapshot already gives a
  recoverable copy. Needs lexer/binder work (or a bespoke pre-clap argv pass).
- **Atomicity (whole family).** Overwrites use `backend.write(Overwrite)` (not
  write-temp-then-atomic-rename), so a crash mid-write can truncate. `cas_overwrite`
  detects a *concurrent change* but is not crash-atomic. Fix surfaces an
  atomic-replace capability on the `Filesystem` trait — do it once for the family.
- **TOCTOU window B — snapshot/CAS double-read (P3).** The byte-oriented writers CAS
  against the returned snapshot (one read), but `patch`/`sed -i` still read twice, so
  a concurrent A→B change in the gap leaves the trash holding stale A. Migrate them
  to transform the *returned* snapshot bytes to close it.
- **TOCTOU window A — existence-check race (P3).** `decide_mutation_action` keys on
  `exists` at gate time; a path absent then returns `Proceed`. If created before the
  builtin's `write`, it's clobbered ungated. Needs a create-exclusive/write-if-absent
  `WriteMode` across LocalFs/MemoryFs/OverlayFs. Low risk (foreground `execute` is
  serialized by `execute_lock`).

### OverlayFs residuals
- **External commands under overlay** fail with exit 127 (real_path=None guard); a
  friendlier in-band error naming the overlay would help. (P3)
- **`kaish-mounts --json` shape changed** (breaking): bare array → `{mounts,
  budget?}`. Flag in release notes.
- **Punted in core** (revisit if a consumer hits them): cross-layer symlink
  resolution is layer-local; directory whiteout is per-file only; `commit_into`
  doesn't propagate mtimes. (P4)

### STRUCTURAL — two-phase bind/walk resolver (#6, before further path work)
Read access, membership, and lvalue writes shipped, but they lean on
`Scope::apply_segment`, which fuses classify + walk + per-hop `clone()`+unwrap
(O(n²) for `for k in $(keys $u)` with per-element `${u[$k]}`); the write side needs
`&mut` serde navigation and can't reuse it. Extract **Bind**
`(VarPath, &Scope) → Result<Vec<ConcreteStep>, PathError>` + **Walk** over
`&serde_json::Value`. **Correction 2026-07-03:** the "folds in" list this entry
originally carried — `VarLength`/`VarWithDefault` → `VarPath`, the lexer `VarLength`
regex widening, arithmetic's own path building, and full-path error messages — all
shipped in `28ec480`, before this entry's decisions were even recorded below; nested
`${#path}` and `${path:-default}` are working forms today, not blocked on this
extraction. **This entry is now purely the `apply_segment` O(n²)-clone perf
extraction** (killing the per-hop `clone()` by walking a borrowed root instead), which
is genuinely still open. **Decisions settled (2026-07-02, already reflected in the
shipped read-side resolver):** `PathError` splits into `UndefinedRoot`/`Absence`/
`Shape`; `${path:-default}` is lenient on absence (unset root, missing key, OOB,
empty → default) but loud on shape errors. Full writeup:
[arrays-and-hashes.md](arrays-and-hashes.md) ("#6 — the shared per-hop resolver").
The `for k in $record` / E012 question is out of scope (iteration stays `$()`-only).

Riders on #6 (currently loud, become nicer once the perf extraction lands):
- **Reduced sync path still coalesces** (also the P3 entry below): the subscripted-name
  loud guard lands in `eval.rs` (sync) + `kernel.rs` (async), but
  `scheduler/pipeline.rs`'s `eval_string_parts_sync` has no error channel, so
  `${#u[tags]}` there is silent-0. **[SILENT — 0.11.0 candidate]**
- **P3/P4 tail** (loud/cosmetic): `fromjson` on an already-typed value stringifies-
  then-reparses and loses `Value::Bytes` (short-circuit typed values); async `VarRef`
  "undefined variable" omits the name while sync includes it; missing-key errors could
  list available keys; `${.a}`/`${a.}` empty dot segments resolve as `${a}` silently.
  Separately, `${#var}` length syntax (nested or not) isn't recognized at all inside
  `$(( ))` arithmetic — `arithmetic.rs` never hand-collects a `#`-prefixed subscript;
  `$(( xs[i] ))` variable-subscript reads work, only the `${#…}` spelling doesn't.
  (P4 — pre-existing, general arithmetic gap, unrelated to path-nesting.)

### Collection read-access follow-ups
- **P3 — reduced sync arg path coalesces a loud path error to skip.** The four primary
  eval sites (`echo`, assignment, `$(( ))`, `"${…}"`) surface `PathError` loudly, but
  the reduced sync pipeline path (`eval_simple_expr`/`eval_string_parts_sync`,
  `pipeline.rs:943/950/1051`) coalesces resolution failure to None/skip — a bad
  subscript in a scatter/gather worker's arg silently drops the arg. Give those sync
  helpers an error channel (or route through the async resolver) when the
  scatter/gather arg path is next touched. **[SILENT — 0.11.0 candidate]**
- **P4 — a `]` inside a quoted subscript key is mishandled.** The lexer's bracket
  collector (`lexer.rs` `parse_var_ref`) scans to the first `]` with no quote
  awareness, so `${r["weird]key"]}` doesn't resolve the intended key. It errors (not
  silent), but cryptically. Fix: quote-aware bracket collector, or a clear diagnostic.
  (Same root cause bounds the empty-subscript `${r[]}` → `Key("")` oddity.)

### Streaming file reads — remaining
- **`base64`** — deferred; only a pipe-case win, the 3-byte/4-char carry is fiddly.
- **`read_range` re-opens the file per chunk** on LocalFs (open+seek per 256 KiB).
  Cheap relative to the read; a stateful handle would cut syscalls but reintroduces
  the tokio-in-trait problem the chunk approach avoided. Revisit only if a profile
  shows it matters.

### Kernel-routed test depth: `read`, `env`, `exec`
The realworld port runs 48 tests through `kernel.execute`, but `read`
(pipeline/scope semantics), `env`, and `exec` (hermetic-env overlay, two-spawn-site
sync) each have only a single smoke case in the `--json` sweep — no flag-surface
depth. Worth dedicated kernel-routed suites.

### Composable help — remaining phases
**Phase 4:** kaijutsu/kaibo adoption (tracked in those repos). **Phase 5:** i18n
scaffolding + first `ja` fragments. Design: [composable-help.md](composable-help.md).
- **P4 — rank the Reference/Contrast fragments before a Reference-depth Foundations
  consumer ships.** `select_for_concept` stable-sorts by `rank`; `UNRANKED` (default
  on Contrast/Example/Reference fragments) sorts last. Inert today (no recipe renders
  Foundations at Reference depth), but if one ever does, an UNRANKED Contrast renders
  after its ranked Rule sibling instead of beside it. The sort key can't include
  `key`/`variant` (would reorder the REPL Model concept), so the fix is to give each
  Contrast/Reference fragment its Rule's rank when such a consumer is built. (Also:
  rank is an *instance* property but documented as a *slot* property — a locale
  forgetting `.ranked(n)` silently reorders that locale's onboarding; add a
  cross-locale rank-parity test.)

### Split `kernel.rs::execute_stmt_flow`
`kernel.rs:1463`–~1913 is a 16-arm async match (kernel.rs is ~6,838 lines); each arm
reaches into `scope`/`exec_ctx`/`user_tools` RwLocks, `For`/`While`/`Case` are 100+
lines apiece. Refactor: `mod kernel/exec/{assignment, command, pipeline, control, …}`
with `execute_stmt_flow` reduced to dispatch-arm-per-module.

### Extract `dispatch_command` ctx-sync helper
The six-field `ExecContext` ↔ kernel-state sync appears near every fork call site
(`kernel.rs:~4100-4140` and duplicates). One helper, one truth.

### Focused session: clean up & streamline the parser
`parser.rs` has accreted special-casing and the lexer/parser boundary does more than
it should (context-free glob/colon merge the parser fights, hand-rolled bracket
collection in `arithmetic.rs`, the value-vs-argv grammar split still pending for
collection literals). Consolidate: unify subscript/path machinery, push the value/argv
bifurcation through cleanly, thin the merge-pass workarounds. This is the write-side
half of the #6 resolver work — streamlining it first de-risks future lvalue grammar.

### `build_args_async` WordAssign arm ignores `past_double_dash` (P4)
`DoubleDash` sets `past_double_dash` (demoting a following flag to positional), but the
`WordAssign` arm doesn't check it, so `export -- A=1` still binds `A=1` as a named
assignment. Pre-existing in the shared binder; only observable on the `export`/`alias`
word-assign allowlist. Fix: degrade to a stringified positional when `past_double_dash`.

### No unquoted token-pasting — residual polish (P4)
- **Redirect target** (`parser.rs`, `redirect_parser`): `> /tmp/$(echo x).txt` is a
  hard parse error; turn it into a "quote the redirect target" hint.
- **Argv**: the glued-positional "quote the whole word" check covers pre-`--`
  positionals only; post-`--` and flag-adjacent-to-positional glue aren't flagged.

### `diff -C 3 -C 4` miscounts arity (P4-trivial)
`context_steals_positional` subtracts 1 for a deduped `-C`. Redundant usage; the
execute-time clap backstop already catches the real surplus-operand case, so the
validator heuristic is cosmetic. Fix only if it bites.

---

## P3 — Scheduler and infra

### `"$(cmd)"` interpolation drops a `.data`-only collection to `""` (SILENT DATA LOSS)
Quoted command-substitution reads a command's `.out` via `try_text_out()`
(`kernel.rs` `StringPart::CommandSubst`, ~line 4035), so a builtin that sets only
structured `.data` (a collection) with an empty `.out` interpolates to `""` inside
`"...$(cmd)..."`. Distinct from the Decision-D stringify boundary (now loud): the
collection evaporates before reaching a process edge. Fix: when `.out` is empty but
`.data` is a collection, render it as compact JSON (consistent with bare `$c`
display) or fail loud — decide which; do NOT leave the silent `""`. **[0.11.0
candidate — clearest crash-beats-corrupt item on the collections surface.]**

### Collection literals: generic record-value error text
- **Unquoted multi-word record value** (`{msg: hello world}`) is already a loud parse
  error (never silently split), but the message is chumsky's generic "expected `:`/`}`"
  rather than the crafted "quote it" wording. Fix: a `try_map` guard in
  `record_literal_parser` peeking for a stray bareword not followed by `:`.
  (Deeply-nested *glued* list literals, `x=[[a] [b]]`, used to be listed here too —
  fixed as a side effect of the `[[ ]]` test-depth lexer rewrite, `09c1a89`;
  see `arrays-and-hashes.md` "Known limitations (0.11.0)".)

### Bracket-path `push` target (`push services[web][tags] item`) — deferred
Lvalue *assignment* (`xs[0]=9`, deep paths) and `push` shipped, but `push` only
accepts a top-level bareword target. `push`'s target isn't followed by `=`, so the
lvalue lexer's `=`-triggered suppression never fires — `services[web][tags]` fuses
into a `GlobWord` and glob-expands (loud "no matches") before `push` runs. Loud, not
silent, but the feature doesn't work. Needs its own lexer/parser pass: a `push`-aware
lexer trigger, or routing `push`'s target through the argv-position bracket-path
grammar. The design intended `push` to accept bracket paths, so this is a real gap.
**[0.11.0 loud gap — documented 2026-07-03 as a known limitation** in `help
collections` and `arrays-and-hashes.md`, with the read/push/assign-back
workaround; this entry stays open for the actual fix.**]**

### `JobManager` output-stream reads hold the jobs lock across `await`
`read_stdout`/`read_stderr` (`scheduler/job.rs`, the `/v/jobs/{id}/stdout|stderr`
reads) `return Some(stream.read().await)` while holding `self.jobs.lock()`. Same class
as the fixed `wait` deadlock: a blocked stream read stalls every other job op. The
async `spawn` fix keeps it from hard-deadlocking. Clone the `Arc<BoundedStream>` out
under the lock, drop, then `read().await`. Audit all `*.lock().await` sites in job.rs.

### `JobManager::wait` can lose a result if `cleanup` reaps the job mid-poll
`wait` polls `jobs.get_mut(&id)?` under the lock between sleeps; `cleanup` (the `jobs`
builtin) `retain`s out done jobs. If `cleanup` removes a finished job between polls,
the next poll's `?` returns `None` and `wait` reports no result — silent loss. Narrow
(foreground `execute()` is serialized by `execute_lock`); needs a background/forked
caller. Fix: distinguish "never existed" from "reaped after completion".
(`scheduler/job.rs::wait`/`cleanup`)

### Builtin residuals from the 2026-06-24 correctness batch
- **`sort`**: only one `-k` accepted (clap `Option<String>`) — GNU chains `-k`; `.C`
  char-within-field offsets accepted but ignored; per-key `b`/`d`/`f`/`i` accepted but
  not implemented; `-k2,1` (stop < start) sorts by field 2 where GNU treats the lower
  field as the stop boundary. (`tools/builtin/sort.rs`)
- **`ls -1`**: recovered by stripping a `Value::Int(-1)` positional, so `ls -- -1`
  (a file literally named `-1`) misfires and lists cwd. Real fix: lex `-1` as a flag.
  (`tools/builtin/ls.rs`)
- **`printf`**: `%b`'s `\c` doesn't stop the whole format string (only its own arg);
  `%c` ignores field width. Low-frequency. (`tools/builtin/printf.rs`)

### `mv` cross-mount copy of a symlink *child* follows it (fidelity, not data loss)
Top-level cross-mount move preserves a symlink source, but `move_dir_recursive` reads+
writes each child by content, so a symlink child inside a moved dir becomes a regular
file (read through to target). Not data loss (source removed via symlink-safe
`backend.remove`); the copy loses link-ness. Fix: `lstat` children and
`read_link`+`symlink` the symlinks. Cross-mount only.

### `find -mtime`/`-size` emits an un-stattable entry instead of dropping it
In the `find` loop (`tools/builtin/find.rs:314-342`) the per-entry stat is best-effort
and each filter is a chained `if let Some(..) = filter && let Some(ref info) = info`.
When a filter is set but `info` is `None` (entry vanished, or backend doesn't populate
the field), no `continue` fires and the entry falls through unfiltered — so
`find . -mtime +30` can print a file it can't confirm. The leniency is deliberate for
"field unavailable" but conflates it with "file is gone." Fix: drop the entry when a
filter is requested but `info`/the field is `None`. (Inverse of the PR-#41 bug.)

### Pre-release sweep — lower-frequency fidelity gaps (verified local)
- **`rm <empty-dir>` without `-r` silently removes it** (coreutils needs `-d`/`-r`);
  **`mkdir` is always `-p`-like** (`mkdir existing` → exit 0; `mkdir a/b/c` creates the
  chain). (`tools/builtin/{rm,mkdir}.rs`)
- **awk fidelity:** `FNR` uninitialized; setting `NF` doesn't truncate fields/`$0`;
  `split(s,a,/re/)` evaluates the regex as a `$0`-match; `sub`/`gsub` replacements lack
  `\1`..`\9` backrefs. (`tools/builtin/awk.rs`)
- **jq residuals** (post jaq-3 upgrade, both low-severity): `1e100` renders as `1e100`
  where jq prints `1e+100` (cosmetic); big integers >2^63 are exact on stdout but lossy
  in `.data` (`val_to_json` round-trips through non-`arbitrary_precision` serde_json) —
  clean fix is `serde_json/arbitrary_precision`, a broad workspace change; defer until a
  consumer needs big-int `.data`.
- **External output via `BoundedStream` keeps only the tail and sets no spill signal**
  in the `output_limit::none()` path (repl/transient/default) — silent truncation, head
  lost. (Enabled output-limit presets spill + exit-3 correctly.) (`kernel.rs`)
- **Embedder-facing VFS:** `PatchOp::*Line` ops strip CRLF / add a trailing newline (the
  `patch` builtin uses whole-file Replace, so end-user `patch` is unaffected); `stat` on
  an intermediate mount path fails though `list_root` shows it; `JobFs` doesn't override
  `read_range` (O(n²) on chunked reads); router `find_mount` uses `to_string_lossy`
  (corrupts non-UTF-8 paths). (`backend/local.rs`, `vfs/router.rs`, `vfs/jobfs.rs`)
- **`date`:** `%Z` renders the numeric offset not the zone name; compound
  `-d '2025-01-15 - 1 day'` (spaced operator) fails (`-1 day` works).
- **rm latch can't be confirmed across separate `kaish -c` processes** (NonceStore is
  per-process; by design) — worth a CLI doc note.
- **Multiple heredocs on one line** (`cat <<A | cmd <<B`): in the REPL the second body
  can execute as commands (data-as-code); in `-c`/script mode it's a loud parse error.
  REPL-only. (`lexer.rs` `preprocess_heredocs`)
- **Scheduler:** concurrent stages/workers use `Vec<JoinHandle>` (not `JoinSet`) with no
  abort — bounded leak, not unbounded (join-all + `fork_attached` cancellation cascade);
  the redirect "stream-to-disk" comment overstates (materializes in memory first); the
  sync `build_tool_args` drops a `$()`-valued scatter/gather *option* (folds into the
  "eliminate the sync twin" item). (`scheduler/{pipeline,scatter}.rs`)
- **Scatter/gather structured-data pre-read:** the scatter runner
  (`scheduler/scatter.rs:136`) still uses the old one-shot `take_stdin_data` rather than
  the `resolve_stdin()` drain-then-await the main consumers adopted. Latent — fold it in
  if a `… | scatter … | gather` structured-data race surfaces.

### Pre-0.9 punch-list residuals — low-frequency fidelity gaps
- **sed `p` / `s///p` in non-quiet mode** suppress auto-print → one print where
  GNU/POSIX print twice (all `p` tests use `-n`, so invisible).
- **sed empty `//`** compiles an always-match regex instead of reusing the last pattern
  (needs runtime last-regex state).
- **awk `"1e"` → 0** (partial-exponent parse); **`FILENAME` always empty**; **`RS=""`
  paragraph mode** splits on exactly `"\n\n"`; **`OFMT` ignored** (hardcoded `%.6g`).
- **multi-file `head`/`tail` strip the trailing newline** while single-file emits one.
- **stdin bridge-thread leak / `block_on`-after-shutdown race**: the REPL detaches an OS
  thread blocked in `read(2)` that can't observe `PipeReader::drop` while parked (CLI
  bounded; a long-lived embedder with a never-closing producer leaks it). Harden with
  `select!`/cancellation.
- **`PipeStdinGuard::try_write` silent skip** (`kernel.rs`) and the test-only
  `BackendDispatcher::try_external` stdin-task leak on unreachable error returns (missing
  the `AbortStdinCopyOnDrop` guard the production path has). Make loud.
- **awk multi-char `FS` regex recompiles per record** (`split_record` — `Regex::new(&fs)`
  each record). Cache it keyed on the FS string.
- **awk array-element type inconsistency**: `split()` stores `StrNum`, `sub`/`gsub`
  stores `String`, so a mutated element loses its numeric-string attribute. POSIX
  ambiguous.
- **head streaming-vs-buffered binary-stdin divergence**: the streaming path emits
  earlier valid lines before erroring on a non-UTF-8 line (buffered emits nothing) and
  uses a different message. Both loud; cosmetic.

### `GlobPath::walk_match` globstar recursion has no work bound
`walk_match`/`match_segments` backtrack on globstar with no `MAX_MATCH_CALLS` guard
(unlike `match_bounded`, capped at 100_000). Bounded by `C(n+g-1, g)` — polynomial in
real FS depth, but an adversarial pattern against a deep path could blow up. Low risk
(walker only matches real FS paths). Add a call counter if kaish ever matches user
patterns against user-supplied path *strings*.

### `--no-ignore` for the search builtins (deferred from the rg-features port)
grep/glob got `--ftype`/`--hidden`/`--max-count` but not `--no-ignore`: the honest
semantics of a per-call ignore-bypass under an embedder's `Enforced` scope (kaibo's
preset) aren't designed — should a user flag override the embedder's context-flood
protection? Decide once, then add `--no-ignore` across the search builtins
consistently. **Audit:** `glob`'s existing `--no-ignore` already bypasses the filter
unconditionally regardless of scope — confirm that's intended. (Ignore is
context-control, not a security boundary — the VFS mount is — so this is
predictability, not a sandbox hole.)

### `find --no-ignore` escape under `Enforced` ignore scope
`find` stays POSIX in the rg-features port, but under `IgnoreScope::Enforced` it *does*
respect the ignore config, diverging from POSIX find. An agent stuck in Enforced may
want `find --no-ignore` to recover traditional behavior. Only meaningful under Enforced;
adds a non-POSIX flag. Add if an Enforced-mode consumer hits it.

### `kill -<sig>` bash shorthand (`kill -9 %1`, `kill -STOP %1`) isn't accepted
`kill` takes the signal via `--signal NAME` / `-s NAME` only; `-9`/`-STOP` fail at clap
parsing. Fix: bespoke argv handling (à la `set.rs`) stripping a leading
`-<signum>`/`-<SIGNAME>` before clap. **Demoted P2→P3 (Amy):** the shorthand leans
POSIX-y and touches OS signal-number variance; the loud `--signal NAME %N` form covers
the need.

### Eliminate the sync `build_tool_args` twin entirely
Retire the sync arg builder so there's one path. Real commands bind through
`build_args_async`; the sync twin (`scheduler/pipeline.rs`) survives only in (a)
scatter/gather *option* parsing in `run_scatter_gather` and (b) the `#[cfg(test)]`
`BackendDispatcher` + ~27 test sites. Eliminating it closes three divergences the sync
path carries (all currently un-triggerable — scatter/gather only expose scalar flags):
it lacks the async path's undeclared-space-flag guard, glued short-flag handling
(`cut -f1`), and repeatable/`consumes>1` accumulation. Fold the schema-aware
validation arg-builder into the same effort.

### Undeclared space-flag guard covers long flags only (`-t val` still divorces)
The guard errors on undeclared `--type value` but not single-char `-t value`
(short-flag+positional is overwhelmingly legit bool+positional). An undeclared short
flag that should take a value silently drops it under a `map_positionals` schema. Low
risk (backend/MCP tools rarely expose single-char value flags); real fix is declaring
the flag.

### `dispatch_command` cancel sync is one-way (in only)
`ec.cancel = ctx.cancel` is synced INTO `exec_ctx` per dispatch, no reverse sync at the
end. Fine today (only `timeout` mutates `ctx.cancel`, and it saves/restores itself). A
future builtin propagating a token swap outward would need explicit restoration.

### Test-effectiveness residuals
- **Bare-glob validation backstop is single-site:** `validation_tests.rs:321-391` are
  environment-dependent (transient-kernel cwd), backstopped only by the inline
  `test_bare_glob_no_matches_errors` (`kernel.rs:6135`, builtin-argv site only);
  `execute_stmt_flow` and `build_args_flat` "no matches" sites lack their own, and the
  inline module is feature-gated such that `cargo test -p kaish-kernel --lib` skips it.
- **`UnterminatedString` diagnostic gap** (P4): an unterminated double-quoted string
  emits generic `UnexpectedCharacter`; needs a logos fallback rule.
- **bg/fg coverage is PTY-only** (`pty_job_control.rs`, unix-gated, timing-sensitive).
- **Timing margins:** `background_job_does_not_block_foreground`
  (`concurrency_tests.rs:159`, 300ms headroom) and bare 5-10ms registration sleeps in
  `job.rs` unit tests — poll instead.
- **Bash-compat leg never runs automatically:** `KAISH_BASH_COMPAT=1` passes 71+27
  bash-side tests, but nothing sets it and there's no CI. (CI deliberately deferred per
  project memory; recorded so the deferral stays a decision, not drift.)

### Test gaps around kill discipline
No test for `kill_grace = Duration::ZERO` (immediate SIGKILL); no test that a
user-defined tool inside a scatter worker dispatches correctly under cancellation
(current test uses external `bash -c`); no test for the JC inherit-output path killed
via cancel (needs a real TTY).

### `ExecuteOptions` callback type is awkward
`Option<&mut (dyn FnMut(&ExecResult) + Send)>` is hard to call. A trait-object alias or
dedicated `Callback` trait would smooth it. Not blocking.

### Long-blocking builtins other than `sleep` may not honor cancellation
`sleep` honors `ctx.cancel`. The guidance stands for any other builtin holding a long
time/IO future without yielding — audit and apply the same `tokio::select!` pattern.
None currently known.

### Piped stdin isn't shared across statements in one `kaish -c 'a; b'` call
`execute_pipeline` moves the seeded `pipe_stdin` into the FIRST top-level statement's
pipeline; if that statement doesn't read stdin, the reader is dropped, so a later reader
gets nothing — `printf hi | kaish -c 'echo x; cat'` prints only `x`. A real shell leaves
fd 0 shared. Fix: keep the source on the persistent `exec_ctx` and let whichever command
first reads it take it. Niche.

### `PipelineRunner::run_single`'s `stdin` parameter is vestigial
`scheduler/pipeline.rs:362` — `run_single(cmd, ctx, stdin)` is always called with
`stdin: None`; the real stdin travels through `ctx.stdin`. Drop the parameter or note
it's forward-looking.

### `ToolCtx::backend()` forces a full `KernelBackend` mock for out-of-tree tests
`kaish-tool-api/src/ctx.rs`: `backend()` returns a non-optional `&Arc<dyn
KernelBackend>`, so an external tool author must construct a complete mock (~16 async
methods) even for an I/O-free tool. Options: (a) ship a `#[cfg(feature = "test-util")]`
no-op harness from `kaish-tool-api`; (b) make `backend()` return `Option<…>` (kernel
always `Some`). (a) is less invasive. Revisit when the first external tool bundle wants
unit tests.

### Recursive/tree `--json` carries structure but no size/type metadata
`tree --json DIR` and `ls -R --json DIR` (without `-l`) serialize the directory *shape*
but every leaf is `null` — the tree-JSON shape has nowhere to hang per-node metadata,
and `tree` never collects sizes. `ls -lR --json` does carry sizes. Fix is a shape
decision: table-with-children, or a per-node metadata slot on the tree-JSON branch.

### `--flag=true` on bool flags rejected for builtins that skip flagify
The explicit `=true` form binds into `named` as `Bool(true)` (the `Arg::Named` branch
skips the `LongFlag` bool check), and the ~20 builtins that never call
`flagify_bool_named` (seq, find, stat, cp, mkdir, …) hand clap a `--json=true` their
plain-`bool` field rejects (`seq --json=true` → exit 2). Bare `seq --json` works. Better
fix: flagify once in the kernel before dispatch and drop the per-builtin calls. The
global `--json` stripper also misses `=true` (scans `flags`, not `named`).

---

## P4 — Eventually

### `compute_value_context`: `[[`/`]]` detection vs `]`-containing glob char-classes
The value-position suppression (`lexer.rs::compute_value_context`) discriminates
assignment `=` from `[[ ]]` comparison `=` by tracking `[[ ]]` test depth in a forward
pass, deducing `[[`/`]]` from adjacent single brackets *before* glob-fusion — fragile: a
glob char-class containing `]` (`[]]`, lexed `[ ] ]`) has an inner `]` at
`bracket_depth==0` that can prematurely decrement `test_depth`. Trigger: a `]`-char-class
glob AND a later single-`=` bracket-glob comparison inside the SAME `[[ ]]`
(`[[ $a == []] && $b = [0-9] ]]`). **Failure mode is LOUD** (a parse error, never wrong
result) — acceptable per crash-beats-corrupt; the common case `[[ $x == []] ]]` parses
identically to bash. Pinned by `collection_literals_tests.rs` (common case works +
`#[ignore]`'d exotic-failure doc test). Robust fix (only if it matters): a context-aware
test-region pass, or matching glob char-classes as single units before
`compute_value_context` runs.

### Pre-release sweep — minor / edge (verified)
- **Backticks inside double-quotes and heredoc bodies are silently literal** — bare
  backticks are a loud lexer error, but quoted/heredoc ones slip through
  `parse_string_literal`. Reject with the `$(cmd)` hint. (`lexer.rs`)
- **Comment `#` mid-word truncates** — `echo http://x/#frag` → `http://x/`; any `#`
  outside double-quotes starts a comment with no word-boundary check. (`lexer.rs`)
- **Malformed `$()` inside a `${VAR:-default}` default word silently falls back to
  literal** — unquoted `$()` and quoted-string `$()` both loud-error on a syntax error
  inside them, but the default-word path has two infallible Expr-returning call sites, so
  a broken `$()` there stringifies instead of erroring. Rare edge; a silent fallback to
  close when those sites grow a fallible path. (`parser.rs`)
- **Empty assignment `VAR=` / `export VAR=` rejected** — the parser requires a RHS; bash
  accepts it as an empty/cleared value. (`parser.rs`)
- **`xxd -l -1` / `-s -1` and `base64 -w -1`** accept negative counts via `i64 as usize`
  wrap (full output, exit 0). (`tools/builtin/{xxd,base64_tool}.rs`)
- **`<`/`>` in `[[ ]]` compare numerically when both operands are `Value::Int`** (bare
  numeric literals), where bash compares lexicographically; quoted strings match bash.
  (`interpreter/eval.rs`)
- **`kill_with_grace` has no else-fallback** when the pidfd target is `None` (only
  reachable for an already-reaped child) — robustness gap, not a live bug. (`kernel.rs`)

### `patch` residuals — final-newline intent and file creation
- **Final-newline intent isn't honored.** `parse_unified_diff` discards the `\ No newline
  at end of file` marker; `apply_hunks` derives the trailing newline from the input file
  — so a patch whose intent is to add/remove the terminal newline can't. Rare; applied
  file just doesn't match intent (no data loss).
- **No file-creation path.** `patch` reads the target then whole-file-`Replace`s, so a
  `--- /dev/null` creation diff errors instead of creating the file. Add an "old side
  empty → create" branch if agents emit creation diffs.
- **`search_block` integer overflow on a crafted header.** A hunk header with a huge
  `old_start` makes `center + dist` overflow (debug panic; release wraps and can match at
  the wrong offset), and the `0..=max_dist` loop is unbounded when the header points past
  EOF (watchdog-bounded). Clamp `center`/`max_dist` to buffer length; use checked/
  saturating add. (`tools/builtin/patch.rs`)

### Control structures inside `$()` are not supported
The `$()` body accepts the full *statement* grammar (pipelines, `&&`/`||`, `;`/newline,
`#` comments) but not `if`/`for`/`while`/`case` — wiring those in threads the recursive
`stmt` parser through ~17 expression call sites, a large parser refactor. Workaround:
compute at statement level and capture the variable.

### Soften the "sh subset that passes shellcheck" framing
CLAUDE.md and the README describe kaish as "a `sh` subset that passes `shellcheck
--enable=all`." More aspirational than accurate: `[[ ]]` is bash (SC3010 under `sh`),
here-strings `<<<` are bash (SC3011), and typed data / structured `$()` / `split` /
E-codes / collections are modelled by no shellcheck dialect. Reframe to *"inspired by
POSIX sh and bash, informed by shellcheck's lints."* Corollary: shellcheck gives zero
coverage for kaish's extensions, so the kaish validator is their sole safety net.

### `touch <dir>` on a local mount fails (EISDIR); memory mounts succeed
`LocalFs::set_mtime` opens the path with `write(true)`, so `touch existing_dir/` fails
EISDIR on a local mount while `MemoryFs::set_mtime` works. POSIX `touch` works on
directories. Fix: bump the time via `utimensat`/an O_RDONLY fd — deferred to keep the
cross-platform `File::set_modified` contract unchanged.

### `FileWalker` symlink-following is broken (dormant — no production caller)
Both bugs are latent: nothing in production sets `FileWalkOptions::follow_symlinks: true`,
so they bite only once a builtin exposes a `-L`/`--follow` flag.
- **Dir symlinks are never recursed.** The walker decides recursion on `entry.is_dir()`
  (`kaish-glob/src/walker.rs:248,308`), but `DirEntry::is_dir()` is `kind == Directory`
  and a symlink is the distinct `Symlink` kind (`kaish-types/src/dir_entry.rs:72`), so
  `is_dir()` is always false for a directory symlink. Fix: gate on
  `is_dir || (is_symlink && follow_symlinks)` and `stat`-through.
- **Cycle detection is a no-op in production.** It keys `visited_dirs` on
  `self.fs.canonicalize(&full_path)`, but `BackendWalkerFs`
  (`kaish-kernel/src/backend_walker_fs.rs:37`) doesn't override `canonicalize`, so a
  circular symlink grows unique paths forever. Fix: implement `canonicalize` on
  `BackendWalkerFs` via `KernelBackend` link resolution before any `-L` flag ships.

### Smaller refactors
- **Extract `skip_quoted_content()`** shared by `preprocess_arithmetic()` and
  `preprocess_heredocs()` (~150 lines each of duplicated quote/escape tracking).
- **`parse_interpolated_string` is ~200 lines** — split into helpers.
- **chumsky alpha → 1.0 before kaish 1.0**: on `1.0.0-alpha.8`; upgrade when stable
  ships.

### REPL polish
Syntax highlighting (chumsky already produces structured tokens), abbreviation
expansion, multi-line paste detection. The highlighter and hinter are currently no-ops;
multi-line paste re-validates on each keystroke.

### Bash-prior "did-you-mean" validator pass
Agents write `[ -z "$VAR" ]` (single bracket) when `[[ ]]` is preferred. The biggest
historical trap — `for i in $LIST` iterating once — is already caught (E012;
`$(cmd)` splits on `\n`). What remains is soft guidance for less-common ports: a
shellcheck-style actionable warning without rejection.
