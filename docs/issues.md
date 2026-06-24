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

### Release-blocking bugs — pre-release review sweep (2026-06-23)
Cross-model sweep (deepseek on builtins, gemini-pro on internals), **every item
reproduced against the local build** before filing; ~10 reviewer false positives
discarded (notably: jq `//`-on-`false` is correct; the watchdog `duration_since`
"panic" — tokio's `Instant` saturates; the "overlay write escapes to real FS" —
overlay CoW is correctly wired; multibyte/empty short-flag "panics" — the lexer
guarantees ASCII flags; the validator does *not* invent block scoping). Fix these
before release.

**Safety / security**
- ~~**`rm -r <symlink-to-dir>` deletes the link target's contents on the real FS.**~~
  FIXED 2026-06-24 (`fix/p1-safety-hangs`). Root cause was deeper than one site:
  `LocalFs::remove`/`rename` canonicalized the *whole* path (following the final
  symlink), and `rm`/`mv` each hand-rolled a recursive walk that `stat`'d through
  links. Consolidated onto a single symlink-safe `backend.remove(path, recursive)`
  (lstat recurse decision); added `LocalFs::resolve_for_unlink` (parent
  canonicalized, final component literal) for `remove`/`rename`; `rm` uses `lstat`
  for the trash/latch decision and symlinks bypass trash; `mv` recreates a symlink
  rather than copying through it. Tests: `rm_mv_symlink_safety_tests.rs` (13
  kernel-routed) + `LocalFs` unit tests. Residual filed P3 below (mv cross-mount
  child-symlink copy fidelity).
- ~~**Hermeticity leak: production reads OS `PATH`.**~~ FIXED 2026-06-24
  (`fix/p1-safety-hangs`). Dropped the `std::env::var("PATH")` fallback in both
  `kernel.rs::try_execute_external` and the test-only `dispatch.rs::try_external`;
  `PATH` now comes from scope only. Frontends seed it via `initial_vars`
  (REPL: `os_env_vars()`). Test `external_resolution_is_hermetic_no_os_path_fallback`
  + updated fixtures that were silently leaning on the fallback (they now seed PATH
  like the real frontend).

**Deadlocks / hangs**
- ~~**Large buffered stdin deadlocks.**~~ FIXED 2026-06-24 (`fix/p1-safety-hangs`).
  The buffered-`String` stdin write now runs in a detached task (matching the
  `pipe_stdin` path) instead of inline-before-drain, so a child emitting a lot
  before draining its input no longer wedges both pipes. Broken-pipe-on-stdin
  (child closes early, e.g. `head`) is no longer reported as a failure. Test:
  `large_buffered_stdin_does_not_deadlock` (8 MiB through external `cat`, 10s
  watchdog — reproduced the hang pre-fix).
- ~~**Job `wait`/`spawn` deadlock.**~~ FIXED 2026-06-24 (`fix/p1-safety-hangs`).
  `JobManager::wait` now takes the job's awaitable out under the lock, releases
  the lock before awaiting completion, and re-acquires only to finalize;
  `JobManager::spawn` is `async` and inserts via `lock().await` (no `try_lock`
  busy-spin). Test: `wait_does_not_block_other_job_ops` (verified red against a
  bug-simulating revert). Related latent holds noted P3 below
  (`/v/jobs/{id}/stdout` reads hold the lock across `stream.read().await`).

**Correctness — silent wrong output**

Builtin batch FIXED 2026-06-24 (`fix/p1-correctness`, kernel-routed regression
tests per fix): **`printf`** flags/precision/`%u%E%G%b`/`\0NNN` now applied
(`printf_format_tests`); **`tr`** interprets SET escapes (`tr_escape_tests`);
**`sort -k`** honors glued/separated keys + `-u` key-dedup (`sort_key_tests`);
**`cut`** unions/dedups/orders ranges (`cut_range_dedup_tests`); **`cat -n`**
preserves the trailing newline (`cat_number_newline_tests`); **`find`**
regular-file/`-maxdepth 0`/`-mindepth`/`-path`/`-ipath` (`find_predicate_tests`);
**`[[ =~ ]]`** loud on uncompilable regex (`regex_match_error_tests`). **awk
numeric comparison** was already fixed in `5abecdf` (strnum `compare_values`) —
this entry was stale; pinned now by `awk_numeric_compare_tests` (12, gawk-checked).
Residuals filed below/P3: sort multi-`-k` + `.C` offsets + `b/d/f` modifiers;
printf `%b \c` whole-format stop + `%c` width.

Still open:
- ~~**Structured-data pipeline race.**~~ FIXED 2026-06-24 (`fix/p1-correctness`).
  Consumers (`jq`, `scatter`) now `ctx.resolve_stdin()`: drain the pipe first
  (unblocks a streaming upstream), then await the structured-data sideband
  (`stdin_data_rx` threaded through `dispatch_command`), instead of a one-shot
  `try_recv` that lost the race on a multi-thread runtime. Test:
  `pipeline_structured_data_tests` (multi-thread, looped — pre-fix `seq|jq` failed
  ~197/200). NOTE: the scatter/gather *runner* (`scheduler/scatter.rs:136`) still
  uses the old `take_stdin_data` pre-read — fold it into resolve_stdin if a
  `… | scatter … | gather` structured-data race surfaces.
- ~~**`&&`/`||` precedence inverted.**~~ FIXED 2026-06-24 (`fix/p1-correctness`):
  single left-associative fold, equal precedence (POSIX), in BOTH
  `statement_parser` and `cmd_subst_parser` (the latter caught in the Gemini-Pro
  review — unquoted `$(a || b && c)` had the same bug). The `[[ ]]`
  `condition_parser` intentionally keeps `&&` tighter (test-expr precedence).
  Test: `and_or_precedence_tests` (incl. cmdsubst) + 3 updated parser snapshots.
- ~~**Syntax error inside a quoted `$()` silently becomes literal text**~~ FIXED
  2026-06-24 (`fix/p1-correctness`): `parse_interpolated_string` is now fallible
  and the double-quoted path `try_map`s the cmdsubst parse error into a loud Rich
  error (matching unquoted `$()`). Test: `quoted_cmdsubst_error_tests`. Residual:
  a malformed `$()` inside a `${VAR:-default}` *default word* still falls back to
  literal (two infallible Expr-returning call sites; rare edge).
- **`jq '. / 0'` returns `null`** silently while `%` errors (jaq-core).

### `execute_argv` — argv-native kernel entry point (+ multicall binary) (Amy, 2026-06-23)
Embeddable surface is string-native (`execute(&str)` → lex/parse). A structured
embedder (kaijutsu) or a busybox-style `kaish-multicall` binary arrives with argv
*already tokenized*, and today must re-quote it back into a string for the lexer —
lossy for typed `Value`s (`to_argv()` stringifies `Bytes`/`Json`). Add
`execute_argv(&self, name, &[Value]) -> ExecResult` as a **peer** of `execute`,
joining the dispatch chain at `ToolArgs` (new helper `build_args_from_argv`
mirroring `build_args_async` minus `Expr` eval; reuses validation/`--json`/latch/
dispatch unchanged). argv tokens are literal — no glob/`$VAR`/split. Test plan:
proptest binder-equivalence vs `build_args_async` (first proptest use — greenlit),
a differential harness over the single-command corpus (argv door ≡ string door),
`Bytes`/`Json` round-trip pins, latch/`--json` smoke. Wrinkle: the two-layer clap
model (`to_argv()` re-parse) caps typed passthrough to `args.positional`-reading
builtins. Full writeup: [multicall.md](multicall.md).

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

### Pre-release sweep — real bugs (2026-06-23, verified local)
Builtin batch FIXED 2026-06-24 (`fix/p2-verified-bugs`, kernel-routed regression
tests per fix): **`glob`** exit 1 on zero matches (`glob_no_match_tests`);
**`env -u`/`-i`** apply when listing (`env_filter_tests`); **`readlink -f`/`realpath`**
resolve symlinks via the VFS + "not a symbolic link" + nonexistent errors
(`readlink_realpath_tests`); **`ls -1`** is the flag (`ls1_basename_dirname_tests`);
**`basename //`/`dirname //`** → `/` (same); **`tail -c +N`** from byte N
(`tail_c_plus_tree_tests`); **`tree <nonexistent>`** errors (same); **`diff --json`**
consistent `{old_file,new_file,differ,hunks?}` shape (`diff_json_shape_tests`);
**`dd skip=`** without `count=` + **`seq -w`** pads negatives (`dd_xxd_seq_tests`);
**`scatter`** single JSON object/scalar = one item (`scatter_single_json_tests`).
NOTE: `xxd -r -p` trailing odd nibble was a **false positive** — kaish already
matches GNU xxd (silently drops it); pinned by a test, not a bug.

Still open (deferred — bigger than a builtin fix, each its own focused PR):
- **POSIX `test` / `[` are unreachable.** `test "a" = "a"`, `test a != b`,
  `[ -f x ]` fail to **parse**. Diagnosed 2026-06-24: NOT a routing miss — `=`/`!=`
  lex as `Token::Eq`/`NotEq` and `[`/`]` as `LBracket`/`RBracket`, and the argv
  parser rejects these operator tokens in command-arg position (`test -z ""` works;
  any operator arg fails). A correct fix must let the argv parser accept these as
  barewords in arg position WITHOUT breaking assignment (`x=y` → IDENT Eq EXPR),
  glob char-classes (`ls [ab]*`), or `[[ ]]`. Delicate lexer/parser change, broad
  regression surface — wants dedicated work + review, not a batch fix.
- **`write`/`tee` write binary-via-`$()` as the literal text `[binary: N bytes]`**
  (silent corruption; the stdin path is safe). Extends the binary-data
  var-interpolation placeholder (P1 binary-data residuals) — here the placeholder
  reaches a file. Best folded into the tee/patch write-model work below.
- **Interpreter trio:** `export` inside a function is dropped on return — needs
  scope-frame-model work (the single `Scope.exported` set isn't merged back when a
  function's forked scope returns), `interpreter/scope.rs`; file tests skip tilde —
  `[[ -f ~/x ]]` false (`eval.rs`); `<<-` tab-stripping runs *after* interpolation,
  eating tabs that came from a variable's value (`eval.rs`).

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

### `JobManager` output-stream reads hold the jobs lock across `await`
Surfaced fixing the `wait`/`spawn` deadlock (2026-06-24). `read_stdout`/`read_stderr`
(`scheduler/job.rs`, the `/v/jobs/{id}/stdout|stderr` reads) do
`return Some(stream.read().await)` *while still holding* `self.jobs.lock()`. Same
class as the (now-fixed) `wait` bug: if the stream read blocks, every other job op
stalls. Lower-frequency than `wait` and the `spawn`-async fix keeps it from
hard-deadlocking the executor, but it should clone the `Arc<BoundedStream>` out
under the lock, drop the lock, then `read().await`. Audit all `*.lock().await`
sites in job.rs for other across-await holds while there.

### Builtin residuals from the 2026-06-24 correctness batch
Low-frequency sub-cases left after the P1 builtin fixes:
- **`sort`**: only one `-k` accepted (clap `Option<String>`) — GNU chains `-k` for
  tiebreaking; `.C` char-within-field offsets are accepted but ignored; per-key
  `b`/`d`/`f`/`i` modifiers are accepted but not implemented; **`-k2,1` (stop <
  start)** sorts by field 2, but GNU treats the lower field as the stop boundary
  (sorts by field 1). (`tools/builtin/sort.rs`; spot-check 2026-06-24)
- **`printf`**: `%b` doesn't honor `\c` (stop *all* output) at the whole-format
  level; `%c` ignores width/flags. (`tools/builtin/format_string.rs`)
- **`ls -1`**: the fix recovers `-1` by stripping a `Value::Int(-1)` positional,
  so `ls -- -1` (listing a file literally named `-1`) misfires and lists cwd
  instead. Rare; the real fix is lexing `-1` as a flag, not an int. (`$VAR=-1`
  is safe — interpolation yields a `String`, not `Int(-1)`.) (`tools/builtin/ls.rs`)

### `mv` cross-mount copy of a symlink *child* follows it (fidelity, not data loss)
Surfaced consolidating the rm/mv symlink-safety fix (2026-06-24). The top-level
cross-mount move now preserves a symlink source (recreates the link). But
`move_dir_recursive` (the copy half of the cross-mount fallback) reads+writes
each child by content, so a *symlink child* inside a moved directory is copied as
a regular file/dir (read through to the target) rather than recreated as a link.
Not data loss — the source tree is removed via the symlink-safe
`backend.remove(src, true)`, and the external target survives; the moved copy just
loses link-ness. Fix: teach `move_dir_recursive` to `lstat` children and
`read_link`+`symlink` the symlinks (mirroring the top-level branch in
`move_path`). Cross-mount only (same-mount uses atomic `rename`, already correct).

### Pre-release sweep — lower-frequency fidelity gaps (2026-06-23, verified)
- **`cp -p`/`--preserve` parsed then discarded** (no mode/mtime preserve; VFS has no
  attributes) — silent. (`tools/builtin/cp.rs`)
- **`rm <empty-dir>` without `-r` silently removes it** (coreutils needs `-d`/`-r`);
  **`mkdir` is always `-p`-like** — `mkdir existing` → exit 0 (no "File exists"),
  `mkdir a/b/c` (no `-p`) creates the chain. (`tools/builtin/{rm,mkdir}.rs`)
- **awk fidelity (adjacent to the pre-0.9 awk gaps below):** `FNR` uninitialized
  (empty); setting `NF` doesn't truncate fields/`$0`; `split(s,a,/re/)` evaluates the
  regex as a `$0`-match (wrong count); `sub`/`gsub` replacements lack `\1`..`\9`
  backrefs. (`tools/builtin/awk.rs`)
- **jq:** `keys_unsorted` returns *sorted* keys; `1e10` serializes as
  `10000000000.0`; indexing `null` (`null | .a`) errors where real jq returns `null`.
  (jaq-core / `jq_native.rs`)
- **`grep -c` exits 0 on zero matches** (GNU exits 1); **`readlink` on a non-symlink**
  reports "Invalid argument" not "not a symbolic link" (`tools/builtin/readlink.rs`).
- **`$(cmd)` trims *all* trailing whitespace** (`.trim_end()`) not just trailing
  newlines — inconsistent with the interpolation/for-loop paths that trim only `\n`.
  (`kernel.rs`)
- **External output via `BoundedStream` keeps only the tail and sets no spill signal**
  in the `output_limit::none()` path (repl/transient/default) — silent truncation,
  head lost. (When output-limit is enabled — agent presets — spill + exit-3 work.)
  (`kernel.rs`)
- **Embedder-facing VFS:** `PatchOp::*Line` ops strip CRLF / add a trailing newline
  (the `patch` builtin uses whole-file Replace, so end-user `patch` is unaffected;
  embedder line-ops aren't); `stat` on an *intermediate* mount path fails though
  `list_root` shows it; `JobFs` doesn't override `read_range` (O(n²) on chunked reads,
  small files); router `find_mount` uses `to_string_lossy` (corrupts non-UTF-8 paths).
  (`backend/local.rs`, `vfs/router.rs`, `vfs/jobfs.rs`)
- **`date`:** `%Z` renders the numeric offset, not the zone name (UTC/EST); compound
  `-d '2025-01-15 - 1 day'` (spaced operator) fails (the no-space `-1 day` form works).
- **rm latch can't be confirmed across separate `kaish -c` processes** (NonceStore is
  per-process; by design for embedder/REPL sessions) — worth a CLI doc note.
- **Multiple heredocs on one line** (`cat <<A | cmd <<B`): in the REPL the second
  body can be executed as commands (data-as-code); in `-c`/script mode it's a loud
  parse error. REPL-only. (`lexer.rs` `preprocess_heredocs`)
- **Scheduler (lower than the toolless review claimed):** concurrent stages/workers
  use `Vec<JoinHandle>` (not `JoinSet`) with no abort, but the normal path joins all
  and cancellation cascades via `fork_attached` tokens, so a drop-mid-await leak is
  *bounded*, not unbounded (`scheduler/{pipeline,scatter}.rs`); the redirect
  "stream-to-disk" comment overstates — it materializes the buffer in memory first;
  the sync `build_tool_args` silently drops a `$()`-valued scatter/gather *option*
  (inserts a bool flag) — folds into the "eliminate the sync twin" item below.

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

### ~~Validator's schema-less arg-builder misparses glued value flags~~ — FIXED 2026-06-23
`build_tool_args_for_validation` (`validator/walker.rs`) was schema-blind:
`Arg::ShortFlag("e1d")` became `flags={"e1d"}` with no glued-value split, so a
tool whose `validate()` reads positionals semantically (sed/awk) misclassified
them — `sed -e1d -e2d FILE` fell back to parsing the *filename* as the program, a
path-dependent false `E006`. **Fixed**: the validation builder now takes the tool
schema and binds glued/combined/space-form and multi-consume (`jq --arg NAME VAL`)
short-flags exactly as `build_args_async` does, reusing the shared
`schema_param_lookup`/`bind_glued_short_value`/`push_repeatable_value` helpers
(now `pub(crate)`), so the two paths can't drift. jq's `validate()` skip-guard was
moved off the old unbound-`flags` representation onto the bound `named` pair
(still rejecting the illegal `--arg=NAME` equals form). Regression test:
`glued_value_flags_dont_false_error_at_validation`. The full twin-elimination
below is still open, but the false-E-code class this caused is closed.

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

### Pre-release sweep — minor / edge (2026-06-23, verified)
- **Backticks inside double-quotes and heredoc bodies are silently literal** — bare
  backticks are a loud lexer error, but quoted/heredoc ones slip through
  `parse_string_literal`. Should reject with the same `$(cmd)` hint. (`lexer.rs`)
- **Unterminated arithmetic `$(( 1 + 2`** reaches EOF and is silently evaluated as `3`
  instead of erroring. (`lexer.rs` `preprocess_arithmetic`)
- **Comment `#` mid-word truncates** — `echo http://x/#frag` → `http://x/`, `echo a#b`
  → `a`; any `#` outside double-quotes starts a comment with no word-boundary check.
  (`lexer.rs`)
- **Empty assignment `VAR=` / `export VAR=` rejected** — the parser requires a RHS
  expression; bash accepts it as setting an empty/cleared value. (`parser.rs`)
- **`xxd -l -1` / `-s -1` and `base64 -w -1`** accept negative counts via `i64 as
  usize` wrap (full output, exit 0). (`tools/builtin/{xxd,base64_tool}.rs`)
- **`<`/`>` in `[[ ]]` compare numerically when both operands are `Value::Int`** (bare
  numeric literals), where bash always compares lexicographically; quoted strings
  match bash. (`interpreter/eval.rs`)
- **`kill_with_grace` has no else-fallback** when the pidfd target is `None` (only
  reachable for an already-reaped child, so a live hang is implausible) — robustness
  gap, not a live bug. (`kernel.rs`)

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
- **`search_block` integer overflow on a crafted header (2026-06-23 sweep).** A hunk
  header with a huge `old_start` makes `center + dist` overflow (debug panic; release
  wraps and can match at the wrong offset), and the `0..=max_dist` search loop is
  unbounded when the header points far past EOF (watchdog-bounded in practice). Clamp
  `center`/`max_dist` to the buffer length and use checked/saturating add.
  (`tools/builtin/patch.rs`)

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
