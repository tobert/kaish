# Known Issues & Open Work

Actionable punch list — **open work only**. Landed-work narrative and standing
decisions live in [devlog.md](devlog.md); per-version history is in `CHANGELOG.md`.
Path note: the 0.8.0 crate split moved some cited files — `vfs/local.rs` →
`kaish-vfs`, host `ps` → `kaish-tools-host/src/ps.rs`, Tool/ToolCtx/KernelBackend
→ `kaish-tool-api`.

Priorities: **P1** high-leverage features/diagnostics · **P2** focused refactors
& real bugs · **P3** scheduler and infra · **P4** eventually.

> **Some work now lives on GitHub Issues** — an experiment as we get ready to
> announce kaibo. Don't double-track these here. Interpreter stack/memory work:
> [#46](https://github.com/tobert/kaish/issues/46) (recursion depth guard),
> [#47](https://github.com/tobert/kaish/issues/47) (explicit worker-thread stack
> size), [#48](https://github.com/tobert/kaish/issues/48) (allocation/memory pass).

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

### Optional latch around subprocess spawn (allowlist + future model review)
The confirmation latch covers the in-process write-model (`rm` + the truncating
overwrite family), but an external `/bin/rm` on `PATH` bypasses it entirely — the
gate lives in the builtin. For an embedder using the latch as a safety hook
(kaijutsu catching latches to apply preapproval policy / a model review — the
`ExecResult::latch_request()` seam, landed), the subprocess door is the hole.
Proposal: an opt-in latch around *every* `subprocess` spawn, gated by an
embedder-supplied **allowlist** (commands that run without confirmation); anything
off the list returns the standard exit-2 + nonce so the embedder can approve via
the same `latch_request()` path. Later: a hook for static checks + a model review
of the argv (mirrors Claude Code "auto" mode). Mechanism in the kernel, judgment
in the embedder — same split as the write-model latch. Until then, the airtight
config is `subprocess` off (read-only / no-spawn build). (P2)

### Script preflight: `classify_command` landed; report + `preflight()` deferred
`Kernel::classify_command(name) -> CommandKind` **landed** (mirrors the
interpreter's resolution order; the two private validator predicates are promoted
behind it via the shared `classify_command_name` in `validator/walker.rs`). That
gives an embedder (kaijutsu's consent gate) the kernel half: walk the public AST,
classify each command node, gate `External`/`Dynamic`. Two follow-ups stay
deferred until a second consumer wants them — same reasoning as the deferred
`kaish-edit` crate:
- **`PreflightReport` + the AST walk + the consent loop are embedder policy**, not
  kernel work. kaijutsu owns them. (advice, not an issue)
- **`Kernel::preflight(src) -> Report` convenience** — a kernel-side walk that
  bundles classification per command node. Defer until a 2nd embedder wants the
  shared version. (P3)
- **Validator/runtime special-form divergence (found building this).** The
  validator's `is_special_command` set (`true`/`false`/`:`/`readonly`/`local`)
  suppresses the "command not found" warning for names that don't actually resolve
  in-process: `readonly` runs as an *external* command (exit 127 at runtime), `:`
  is a parse error, `local` is parser-level. `classify_command` deliberately uses
  the **narrower** runtime set (`true`/`false`/`source`/`.`) so a consent gate
  isn't lied to. The validator's warning heuristic is now the odd one out — it
  should probably warn on `readonly`/`:` (they aren't kaish commands), or route
  through `classify_command_name`. Low urgency (warning-only), but it's a real
  validator↔runtime mismatch. (P3)

### `kaish-multicall` binary (deferred from the `execute_argv` work)
`execute_argv` — the argv-native peer of `execute(&str)` — **landed** (see
devlog); the load-bearing half is done. The cheap second half is still open: a
busybox-style `kaish-multicall` binary that dispatches by `argv[0]`
(`ln -s kaish-multicall ~/bin/ln` → kaish's `ln`). It's a third *frontend*, ~80
lines, no kernel changes — a second `[[bin]]` in `kaish-repl` first, promoted to
its own crate only if it grows. Now that `execute_argv` exists it can be
argv-native from day one (no quoting-reconstruct interim). Build it when a
consumer wants it. Full writeup: [multicall.md](multicall.md). Open design
question recorded there: whether to also expose a `&[String]` convenience door
over the `&[Value]` primitive (trivial `.map(Value::String)` wrapper).

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
- **Interpreter trio — ALL FIXED** (2026-06-26, `fix/file-test-tilde`):
  - ~~**`export NAME=VALUE` inside a function is dropped on return.**~~ FIXED: the
    builtin's assignment form now uses new `Scope::set_exported_global`
    (`set_global` + export mark) so the value lands in the shared scope and survives
    the function frame, matching plain assignments and bash. `set_exported`
    (innermost-frame) stays for the intentional frame-scoped overlays
    (`execute_with_vars`, `FOO=bar cmd`). Tests: `export_scope_tests.rs`.
  - ~~**`<<-` tab-stripping runs *after* interpolation**, eating tabs from a
    variable's value.~~ FIXED 2026-06-26 (`fix/file-test-tilde`): new
    `HeredocAssembler` (`interpreter/eval.rs`) strips leading tabs from the literal
    source part-by-part; interpolated values are appended verbatim and terminate the
    leading-tab run (bash strips source-line tabs before expansion). Wired into all
    three interpolated-heredoc paths (sync `eval`, async `kernel`, sync `pipeline`
    redirect-target). Tests in `heredoc_tests.rs`; bash-compat leg green.
  - ~~**File tests skip tilde** — `[[ -f ~/x ]]` always false.~~ FIXED 2026-06-26
    (`fix/file-test-tilde`): `eval_test_async` (the kernel path) now expands `~`
    against the session `HOME` via `apply_tilde_expansion` before stat'ing, matching
    the argv-positional path. Hermetic kernels (no `HOME`) keep `~` literal. Test:
    `file_test_tilde_tests.rs` (3, localfs). NOTE: the sync `Executor::eval_test`
    (`eval.rs`, non-kernel embedders only) still doesn't expand — it has no `HOME`
    source on the trait; left as-is until an embedder needs the sync `[[ ]]` path.

### Write-model gate — DONE (full family); residuals
**Gated: `rm` (delete) + `tee`/`patch`/`sed -i`/`write`/`cp`/`mv`/`dd of=` (truncating
overwrite)** (`feat/complete-write-model-gate` finished the last four, which had
bypassed the gate entirely). Pure `decide_mutation_action → {TrashFirst, Latch,
Proceed}` — now **size-aware** (a prior file over `trash_max_size` can't be snapshotted
so it falls through to latch/proceed, like `rm`) — plus `ExecContext::gate_overwrites`
(snapshots prior content via `TrashBackend::trash_bytes` **and returns those bytes**) and
the binary-safe `cas_overwrite`/`overwrite_checked` (re-read + compare prior bytes; no
`String` `PatchOp`) used by the byte-oriented writers. `rm` and the overwrite gate share
`is_trash_excluded` (the `/tmp`,`/v` list) so it can't drift. Family-wide
`--confirm=<nonce>`, one nonce per command (`dd` uses its `confirm=<nonce>` key=value
idiom). New file / append (`tee -a`) / `patch --dry-run` / copy-or-move *into* a
directory don't gate. Latch+trash stay ON in overlay mode.

Residuals:
- **cp/mv recursive & into-directory overwrites are ungated.** The gate covers the
  *named* destination when it already exists as a file (the direct `cp/mv SRC FILE`
  clobber). A recursive `cp -r`/`mv` merging into an existing directory (per-child
  writes in `copy_dir_recursive`/`move_dir_recursive`) and a `SRC DIR/` that overwrites
  `DIR/SRC` are NOT gated — neither truncates the named target. The real fix is up-front
  enumeration of every clobbered child so one nonce can scope them; deferred.
- **`mv` overwrite has no CAS** (gate-snapshot + latch only). The same-mount path is an
  atomic `rename` (no read→write window to guard) and the cross-mount path is
  unlink+write; the trash snapshot is the recovery. Fine as-is — recorded for symmetry.
- **`sed -i.bak` backup suffix.** Not supported: kaish's lexer splits `-i.bak` at
  the dot (dot-prefixed barewords), and the kernel value-flag binder consumes the
  *next* token greedily, so GNU's glued-optional-suffix isn't modelable post-lexing.
  The trash snapshot already gives a recoverable copy, so this is convenience, not
  safety. Needs lexer/binder work (or a bespoke pre-clap argv pass à la `set.rs`).
- **Atomicity (whole family).** Overwrites use `backend.write(Overwrite)` (`patch`/`sed
  -i` a CAS `Replace`; the byte writers `cas_overwrite`), not write-temp-then-atomic-
  rename, so a crash mid-write can truncate. `cas_overwrite` detects a *concurrent
  change* but is **not** crash-atomic. The fix surfaces an atomic-replace capability
  question on the `Filesystem` trait — do it once for the whole family, not per-builtin.
- **TOCTOU window B — snapshot/CAS double-read (P3).** `gate_overwrites` now returns the
  snapshotted bytes and the byte-oriented writers (`tee`/`write`/`dd`/`cp`) CAS against
  *those* (one read), so window B is closed for them. `patch`/`sed -i` still read twice
  (their transform re-reads and binds `expected` to the later read), so a concurrent
  change A→B in the snapshot→read#2 gap leaves the trash holding the *stale* A while
  B→C is written — recovery one version behind, CAS doesn't catch it. Migrating
  `patch`/`sed -i` to transform the *returned* snapshot bytes (now available) closes it.
- **TOCTOU window A — existence-check race (P3).** `decide_mutation_action` keys on
  `exists` at gate time; a path absent then returns `Proceed` (no snapshot, no latch).
  If it's created before the builtin's `write`, it's clobbered ungated. Inherent with
  today's primitives — `WriteMode` has no `O_EXCL`/create-exclusive or write-if-absent
  CAS. Closing it needs a new write mode across LocalFs/MemoryFs/OverlayFs. Practical
  risk is low (foreground `execute` is serialized by `execute_lock`; the window only
  opens against an external process, a background fork, or a second kernel on the same
  localfs).

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

**P4 — rank the Reference/Contrast fragments before a Reference-depth Foundations
consumer ships (2026-07-01, from the tier-mechanism review).** `select_for_concept`
stable-sorts by `rank`, and `UNRANKED` (the default on Contrast/Example/Reference
fragments) sorts last. No shipping recipe renders Foundations at Reference depth,
so this is inert today — but if a `help foundations`-style topic ever composes
Foundations at Reference depth, an UNRANKED Contrast would render *after* the
ranked Rules instead of beside its Rule sibling. The sort key can't include
`key`/`variant` (that would reorder the REPL's Model concept, which relies on
registry order for equal ranks), so the fix is to give each Contrast/Reference
fragment the same rank as its Rule counterpart when such a consumer is built.

**Importance-ranked onboarding tiers (2026-07-01, from the arrays-and-hashes
review):** restructure `agent_onboarding()` composition into ~200–300-char ranks in
descending importance — the first rank carries the most critical rules so the client
model gets them immediately even under skimming/truncation; successive ranks follow;
everything else moves to an easy-to-find, loadable resource (help topics / MCP
prompts) pointed at from the block's tail. Mechanically: fragments gain an importance
rank, recipes compose by rank, and a size assertion in kaish-help's tests caps the
ranked tier stack (today nothing caps composed size — no test, no ceiling; `Depth` is
the only lever, and the block is already ~9–10.5K chars with the builtin index
dominating at ~5.4K). Do this before the collections fragments land — they're the
feature that will test it. See [arrays-and-hashes.md](arrays-and-hashes.md)
"Help & teaching delivery".

**Collection read-access follow-ups (2026-07-01, from the read-access deepseek
review; verified locally):**
- **P3 — reduced sync arg path coalesces a loud path error to skip.** The four
  primary eval sites surface `PathError::Invalid` loudly (verified: `echo`,
  assignment, `$(( ))`, and `"${…}"` interpolation all error on a bad subscript),
  but the reduced *sync* pipeline path (`eval_simple_expr` /
  `eval_string_parts_sync`, `pipeline.rs:943/950/1051`) coalesces any resolution
  failure to None/skip — so a bad subscript inside a scatter/gather worker's arg
  could silently drop the arg instead of erroring. Narrow (the common paths are
  loud) and consistent with the pre-existing undefined-var behavior there, but a
  silent-fallback to close: give those sync helpers an error channel (or route
  them through the async resolver) when the scatter/gather arg path is next
  touched.
- **P4 — a `]` inside a quoted subscript key is mishandled.** The lexer's bracket
  collector (`lexer.rs` `parse_var_ref`) scans to the first `]` with no quote
  awareness, so `${r["weird]key"]}` doesn't resolve the intended key. It does NOT
  silently corrupt (it errors — "failed to evaluate assignment"), but the error
  is cryptic. Fix: make the bracket collector quote-aware, or emit a clear
  "quote-bearing key with `]`" diagnostic. (Same root cause bounds the empty
  subscript `${r[]}` → `Key("")` oddity — reject in the validator if we care.)

**~~Envelope-free must hold through downstream `.data` consumers~~ FIXED
2026-07-02** (`fix/collections-silent-traps`). The `for`-loop array-element spread
(`kernel.rs:2177`) and `jq`'s single-value output (`jq_native.rs:689`) both
re-sniffed the byte-envelope via the sniffing `json_to_value`, so `for i in
$(fromjson '[{"_type":"bytes",…}]')` silently decoded an envelope-shaped element
to `Value::Bytes`. Both now use `json_to_value_no_envelope` (jq's 2+-value path
already handed raw serde through, so this makes the single-value path match).
Regression test: `collection_access_tests::for_loop_over_envelope_shaped_elements_stays_a_record`.
The `fromjson | jq '.'` roundtrip case remains untested — add if it recurs.

**Overnight milestone review — collections read-access (2026-07-02, gemini-pro +
fable-5 batches; every finding verified against the local build).** Cross-model
consensus. **The silent-corruption cluster (#1–#5) FIXED 2026-07-02**
(`fix/collections-silent-traps`, failing-first tests each); the structural item
(#6) and the small tail remain open.

- **~~`${cfg[port]:-8080}` always returned the default; `${xs[0:-1]}` → `1]`~~ FIXED**
  (fable A1). Both `:-` scanners (`find_default_separator`/`_in_content`, `parser.rs`)
  are now bracket-aware, so a `:-` *inside* a subscript (negative slice end) is no
  longer read as a default separator — `${xs[0:-1]}` slices correctly. A genuine `:-`
  *after* a subscript (`${cfg[port]:-8080}`, name still carries `[port]`) now trips a
  **loud "bind it first" error** instead of silently returning the default. (Full
  path-aware default semantics defer to #6 — the `VarWithDefault.name`→`VarPath`
  conversion.)
- **~~`${#u[tags]}` silently `0`; `${#xs}` byte-length not element count in sync~~ FIXED**
  (fable A2 + gemini #3). `scheduler/pipeline.rs`'s two reduced-sync sites now use
  `value_length` (they were `value_to_string(value).len()` → `${#xs}` on `[1,2,3]`
  gave `7`); `value_length` counts `Value::Bytes` by byte length. `${#u[tags]}` (a
  subscripted name) is now a **loud "bind it first" error**, not silent `0` — full
  nested length defers to #6.
- **~~collection-vs-scalar `==`/`!=` silently `false`~~ FIXED** (gemini #1 / fable A4).
  `values_equal` now returns `EvalResult<bool>` and yields `EvalError::Unsupported`
  for the collection-vs-scalar case (surfaces as a loud `Err` from `execute`, same
  contract as the `=~` regex fix). Pulled forward ahead of `in`/membership. `!=`
  propagates the same error.
- **~~`export` of a structured value silently JSON-serialized~~ FIXED** (gemini #2).
  `structured_export_error` guards before `cmd.env(...)` at both spawn sites
  (`kernel.rs::try_execute_external` + test-only twin `dispatch.rs::try_external`),
  naming the var and pointing at `export CFG=$(tojson $CFG)`.
- **~~for-loop / jq envelope re-sniff~~ FIXED** — see the "Envelope-free" entry above
  (folded together; `kernel.rs:2177` + `jq_native.rs:689` now `_no_envelope`).

- **STRUCTURAL — #6: extract a two-phase bind/walk resolver before lvalues** (fable's
  "single most important thing before the grammar"). Design pass with Amy in progress
  (2026-07-02); full writeup in [arrays-and-hashes.md](arrays-and-hashes.md)
  Implementation notes ("Two-phase bind/walk resolver"). In short: `Scope::apply_segment`
  fuses classify + walk + per-hop `clone()`+unwrap (O(n²) for `for k in $(keys $u)` with
  per-element `${u[$k]}`); the write side needs `&mut` serde navigation and can't reuse
  it, so building lvalues on it duplicates the classification/message logic under `&mut`
  and read/write drift. Split into **Bind** `(VarPath, &Scope) → Result<Vec<ConcreteStep>,
  PathError>` + **Walk** over `&serde_json::Value`. Folds in: `VarLength`/`VarWithDefault`
  → `VarPath` (unblocks nested `${#path}` / default-on-path from #1/#2, plus the lexer
  `VarLength`-regex widening), arithmetic's hand-collected brackets (fixes fable A3), and
  full-path-prefix error messages. **Design decisions settled 2026-07-02:** (a) `PathError`
  becomes three variants — `UndefinedRoot` (soft) / `Absence` (missing key, OOB) / `Shape`
  (wrong-type access); `${path:-default}` catches `UndefinedRoot`+`Absence`, `Shape` stays
  loud. (b) The `for k in $record` / E012 question is **OUT of #6** — iteration is
  `$()`-only, E012 unchanged, `keys`/`values` (landing separately) cover it. So #6 is now
  purely the resolver extraction, no for-loop/validator work.
- **DECIDED (2026-07-02) — `${path:-default}` semantics for #6:** `:-` is **lenient on
  absence** (unset root, missing key, out-of-bounds, empty → the default) but **loud on
  shape errors** (string key on a list, int index on a record, subscript-on-scalar).
  Rationale: `:-` *is* the absence affordance; strictness stays available per-expression
  via the `:-`-free bare form (`${r[k]}`, already loud); `[[ k in $r ]]`/`jq has()` cover
  the boolean-branch case. No global `set -o strict` for now. Implementation dependency:
  #6's `Bind` must split today's single `PathError::Invalid` into **absence** vs
  **shape** so `:-` can catch only absence — full writeup in arrays-and-hashes.md
  ("Length/default on a subscripted path" + the resolver Implementation note). `${#path}`
  stays loud "bind first" until path-aware (no default-style leniency question there).
- **OPEN — reduced sync path still coalesces** (unchanged, see the separate P3 entry
  above): the subscripted-name loud guard lands in `eval.rs` (sync) + `kernel.rs`
  (async), but `scheduler/pipeline.rs`'s `eval_string_parts_sync` has no error channel,
  so a subscripted `${#u[tags]}` there is still silent-0. Rides the P3 error-channel
  work when the scatter/gather arg path is next touched.
- **OPEN — P3/P4 tail** (verified, non-blocking): `fromjson` on an already-typed value
  stringifies-then-reparses and loses `Value::Bytes` (gemini #4 — short-circuit typed
  values); async `VarRef` "undefined variable" omits the name (`kernel.rs:eval_expr_async`)
  while the sync path includes it; missing-key errors could list available keys;
  `${.a}`/`${a.}` empty dot segments resolve as `${a}` silently. Help-tier: rank is an
  *instance* property but documented as a *slot* property — a locale translation
  forgetting `.ranked(n)` silently reorders that locale's onboarding (add a cross-locale
  rank-parity test) — refines the Reference/Contrast P4 above.
- **OPEN — expression-position `${#u[tags]}` gives a poor message** (kaibo deepseek review of
  #57, 2026-07-02; verified). *In-string* `"${#u[tags]}"` gets the nice "bind it first" guard,
  but *expression-position* `echo ${#u[tags]}` never becomes a `VarLength` node: the lexer regex
  `Token::VarLength` (`\$\{#[a-zA-Z_][a-zA-Z0-9_]*\}`, `lexer.rs`) excludes `[`, so it falls to
  `VarRef` with root `#u` → loud, but the message is the bare "undefined variable" (also missing
  the name, per the async-VarRef item above), not the length hint. **Loud, not silent — no
  correctness bug.** Fix rides #6: when `VarLength`/`VarWithDefault` carry a `VarPath`, also widen
  the lexer `VarLength` regex (or route `#`-prefixed `VarRef` through the length path) so the
  subscripted form reaches the guard/resolver in expression position too.

### Split `kernel.rs::execute_stmt_flow`
`kernel.rs:1463`–~1913 is a 16-arm async match (kernel.rs is ~6,838 lines); each
arm reaches into `scope`/`exec_ctx`/`user_tools` RwLocks, and `For`/`While`/`Case`
are 100+ lines apiece. Natural refactor: `mod kernel/exec/{assignment, command,
pipeline, control, …}` with `execute_stmt_flow` reduced to dispatch-arm-per-module.

### Extract `dispatch_command` ctx-sync helper
The six-field `ExecContext` ↔ kernel-state sync appears near every fork call site
(`kernel.rs:~4100-4140` and duplicates). One helper, one truth.

### Focused session: clean up & streamline the parser
`parser.rs` has accreted a lot of special-casing and the lexer/parser boundary is
doing more than it should (context-free glob/colon merge passes the parser then has
to fight against, hand-rolled bracket collection in `arithmetic.rs`, the
value-vs-argv grammar split still pending for collection literals). Worth a dedicated
pass to consolidate: unify the subscript/path machinery, push the value/argv
bifurcation through cleanly, and thin the merge-pass workarounds. Surfaced while
grounding the #6 resolver work (2026-07-02) — the resolver refactor is the read-side
half; the parser is where the *write*-side literal/lvalue grammar will land, so
streamlining it first de-risks that phase. (P2)

### No unquoted token-pasting — residual polish
(Decision recorded in devlog — keep the quoting requirement.) Live residuals (P4):
- **Redirect target** (`parser.rs`, `redirect_parser`): `> /tmp/$(echo x).txt` is a
  hard parse error; turn it into a "quote the redirect target" hint.
- **Argv**: the glued-positional "quote the whole word" check covers pre-`--`
  positionals only; post-`--` and flag-adjacent-to-positional glue aren't flagged.

### `build_args_async` WordAssign arm ignores `past_double_dash` (P4)
Surfaced in the `execute_argv` review (Gemini Pro, 2026-06-29). `DoubleDash` sets
`past_double_dash`, which demotes a following `ShortFlag`/`LongFlag` to a
positional — but the `WordAssign` arm doesn't check it, so `export -- A=1` still
binds `A=1` as a named assignment instead of a literal positional. Pre-existing in
the **shared** binder (hits the string door too, not an `execute_argv` regression),
and only observable on the `export`/`alias` word-assign allowlist (every other
command already stringifies `WordAssign` to a `"key=value"` positional). Fix: have
the `WordAssign` arm degrade to a stringified positional when `past_double_dash`.
(`kernel.rs::build_args_async`)

### v0.8.4 review residuals (Gemini Pro, 2026-06-14)
- **`diff -C 3 -C 4` miscounts arity** (P4-trivial — `context_steals_positional`
  subtracts 1 for a deduped `-C`). Redundant usage; the execute-time clap backstop
  (`parsed.files.len() > 2`) already catches the real surplus-operand case, so the
  validator heuristic is cosmetic. Fix only if it bites.

---

## P3 — Scheduler and infra

### Collection literals: deeply-nested *glued* brackets, and generic multi-word-value error text
Two scoped deferrals from the collection-literals grammar landing (2026-07-02,
`feat/collections-literals`, see `docs/arrays-and-hashes.md` "Implementation
notes" for the shipped mechanism):
- **Deeply-nested glued list literals** like `x=[[a] [b]]` aren't unfused by the
  lexer's value-position suppression today — only a flat/glued-single/empty
  bracket run (`[dog]`, `[]`, `[1]`) is exempted from `GlobWord` fusion.
  Spaced nesting (`[ [a] [b] ]`) was never glued in the first place and works
  fine. Fix would extend `lexer::compute_value_context`'s depth tracking to
  also unfuse an inner glued bracket run once already inside an open value
  literal (the counter is there; the glob-merge run-detection loop would need
  to stop treating the WHOLE glued span as one run when depth re-enters 0 mid-run).
- **Unquoted multi-word record value** (`{msg: hello world}`) is already a loud
  parse error (never silently split/joined — the invariant that matters), but
  the message is chumsky's generic "expected `:`/`}`, found identifier" rather
  than the hand-crafted "quote it: `{msg: \"hello world\"}`" wording sketched
  in the design doc's Teaching note #10. Fix: a `try_map` guard in
  `record_literal_parser` (`parser.rs`) that peeks for a stray bareword not
  followed by `:` and raises a custom `Rich::custom` message.

### `"$(cmd)"` interpolation drops a `.data`-only collection to empty string (silent data LOSS)
Found alongside the collections boundary-ops work (2026-07-02, `feat/collections-boundary-ops`).
Quoted command-substitution interpolation reads a command's `.out` via
`try_text_out()` (kernel.rs `StringPart::CommandSubst`, ~line 4035), so a builtin
that sets only structured `.data` (a collection) with an empty `.out` interpolates
to `""` inside `"...$(cmd)..."`. This is a **silent data-loss** distinct from the
Decision-D stringify boundary (which is now loud): the collection never reaches a
process edge — it evaporates before it can. Repro shape: any builtin whose result
carries `.data` but no materialized `.out`, spliced into a double-quoted string.
Fix direction: when `.out` is empty but `.data` is a collection, either render the
collection as compact JSON (consistent with bare `$c` display) or fail loud —
decide which; do NOT leave the silent `""`. Different bug class from the
boundary guards, so deferred here rather than fixed in that branch.

### `JobManager` output-stream reads hold the jobs lock across `await`
Surfaced fixing the `wait`/`spawn` deadlock (2026-06-24). `read_stdout`/`read_stderr`
(`scheduler/job.rs`, the `/v/jobs/{id}/stdout|stderr` reads) do
`return Some(stream.read().await)` *while still holding* `self.jobs.lock()`. Same
class as the (now-fixed) `wait` bug: if the stream read blocks, every other job op
stalls. Lower-frequency than `wait` and the `spawn`-async fix keeps it from
hard-deadlocking the executor, but it should clone the `Arc<BoundedStream>` out
under the lock, drop the lock, then `read().await`. Audit all `*.lock().await`
sites in job.rs for other across-await holds while there.

### `JobManager::wait` can lose a result if `cleanup` reaps the job mid-poll
Surfaced in the v0.9.1 release review (gemini-pro, 2026-06-25). `wait` polls
`jobs.get_mut(&id)?` under the lock between sleeps; `cleanup` (called by the
`jobs` builtin) `retain`s out *done* jobs. If `cleanup` removes a finished job in
the gap between `wait`'s polls, the next poll's `?` returns `None` and `wait`
reports no result for a job that actually completed — a silent loss, not a hang.
Narrow today: foreground `execute()` is serialised by `execute_lock`, so a `wait`
and a `jobs`-triggered `cleanup` can't run concurrently from the same foreground
path; it needs a background/forked caller. Fix: have `wait` distinguish
"never existed" from "reaped after completion" (e.g. consult a short-lived
completed-results cache, or have `cleanup` not drop a job with an outstanding
waiter). (`scheduler/job.rs::wait`/`cleanup`)

### Builtin residuals from the 2026-06-24 correctness batch
Low-frequency sub-cases left after the P1 builtin fixes:
- **`sort`**: only one `-k` accepted (clap `Option<String>`) — GNU chains `-k` for
  tiebreaking; `.C` char-within-field offsets are accepted but ignored; per-key
  `b`/`d`/`f`/`i` modifiers are accepted but not implemented; **`-k2,1` (stop <
  start)** sorts by field 2, but GNU treats the lower field as the stop boundary
  (sorts by field 1). (`tools/builtin/sort.rs`; spot-check 2026-06-24)
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

### `find -mtime`/`-size` emits an un-stattable entry instead of dropping it
Surfaced by the cross-family review of the LocalFs list TOCTOU fix (PR #41,
2026-06-29; Gemini Pro caught it, DeepSeek missed it). In the `find` loop
(`tools/builtin/find.rs:314-342`) the per-entry stat is best-effort
(`ctx.backend.stat(&path).await.ok()`), and each filter is a chained
`if let Some(..) = mtime_filter && let Some(ref info) = info { … continue }`.
When a filter *is* set but `info` is `None` (the entry vanished between the walk
and the stat, or a backend doesn't populate the field), the whole block
short-circuits, **no `continue` fires**, and the entry falls through to output
unfiltered — so `find . -mtime +30` can print a file it can't confirm matches.
The leniency is deliberate for "stat field unavailable" (comment at
`passes_mtime_size`, ~`find.rs:425`), but it conflates that with "file is gone."
Fix: when a filter is requested but `info`/the field is `None`, drop the entry
(treat as non-match) rather than passing it through. Note this is the *inverse*
of the PR-#41 bug — a false inclusion, not an aborted walk — and is pre-existing.

### Pre-release sweep — lower-frequency fidelity gaps (2026-06-23, verified)
- **`rm <empty-dir>` without `-r` silently removes it** (coreutils needs `-d`/`-r`);
  **`mkdir` is always `-p`-like** — `mkdir existing` → exit 0 (no "File exists"),
  `mkdir a/b/c` (no `-p`) creates the chain. (`tools/builtin/{rm,mkdir}.rs`)
- **awk fidelity (adjacent to the pre-0.9 awk gaps below):** `FNR` uninitialized
  (empty); setting `NF` doesn't truncate fields/`$0`; `split(s,a,/re/)` evaluates the
  regex as a `$0`-match (wrong count); `sub`/`gsub` replacements lack `\1`..`\9`
  backrefs. (`tools/builtin/awk.rs`)
- **jq cluster — ALL FIXED** by the jaq-core 3 / jaq-json 2 upgrade
  (`chore/jaq-3-bump`, 2026-06-26):
  - ~~number formatting~~ integral results render without `.0` (`6/2`→`3`,
    `1e10`→`10000000000`). The `jq_float_to_json`/`num_str_to_json` serde-side
    helpers were replaced by `canonicalize_numbers` (integral `Num::Float`→`Num::Int`)
    in front of jaq's native writer.
  - ~~`keys_unsorted` returns *sorted* keys~~ FIXED: jaq-json 2's `Val::Obj` is an
    IndexMap and `serde_json` now has `preserve_order` (workspace-wide), so insertion
    order survives the input parse. *All* jq object output is now insertion-ordered
    (jq parity); `keys` (sorted) is unchanged.
  - ~~indexing `null` (`null | .a`) errors~~ FIXED: jaq-json 2's `index_opt` maps
    `null` → `None` → `null`.
  - bonus: large integers are now exact (bignum-backed `Num`), no `1e+22` degrade.
  Tests in `jq_tests.rs`. Residuals (both low-severity, not regressions):
  - `1e100` renders as `1e100` (jaq) where jq prints `1e+100` — cosmetic, out-of-range
    float, not worth special-casing.
  - **Big integers >2^63 are exact on stdout but lossy in `.data`.** jaq's writer
    prints them exactly, but `val_to_json` round-trips through `serde_json` (no
    `arbitrary_precision`), which parses an integer past ±2^63 as `f64`. So
    `jq -cn '9999999999999999999999'` prints 22 exact digits, but
    `for x in $(jq …)` / `kaish-last` see ~1e22. `.data` was always f64-limited (the
    pre-upgrade path too); only stdout improved, so this is a widened asymmetry, not a
    regression. The clean fix is `serde_json/arbitrary_precision` — a broad,
    independent workspace change; defer until a consumer actually needs big-int `.data`.
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
- **sed empty `//`** compiles an always-match regex instead of reusing the last
  pattern (needs runtime last-regex state, not a parse-time fix).
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

### `GlobPath::walk_match` globstar recursion has no work bound
`walk_match`/`match_segments` backtrack on globstar with no `MAX_MATCH_CALLS` guard
(unlike `match_bounded` in `glob.rs`, capped at 100_000). Calls are bounded by
`C(n+g-1, g)` — polynomial in real FS depth, but an adversarial pattern
(`a/**/b/**/c/**/…`) against a deep path could blow up. Low risk today (walker only
matches real FS paths; not a regression). If kaish ever matches user patterns
against user-supplied path *strings*, add a call counter.

### `--no-ignore` for the search builtins (deferred from the rg-features port)
The rg-features port (grep `--ftype`/`--hidden`/`--max-count`, glob `--ftype`)
landed without `--no-ignore` on grep — the honest semantics of a per-call
ignore-bypass under an embedder's `Enforced` scope (kaibo's preset) aren't
designed yet: should an explicit user flag override the embedder's context-flood
protection? That's an embedder-policy question, not a one-liner. Decide it once,
then add `--no-ignore` across the search builtins consistently. **Related audit:**
`glob`'s *existing* `--no-ignore` already bypasses the filter unconditionally,
regardless of scope (`tools/builtin/glob.rs`) — confirm that's intended (vs.
silently escaping `Enforced`) as part of the same design. Note: ignore is
context-control, not a security boundary (the VFS mount is) — so this is about
predictability, not a sandbox hole.

### `find --no-ignore` escape under `Enforced` ignore scope (deferred from the rg-features port)
`find` stays POSIX in the rg-features port (grep + glob got `--ftype`, find didn't). But under
`IgnoreScope::Enforced` (kaibo/agent preset) `find` *does* respect the ignore config,
diverging from POSIX find (which ignores `.gitignore` entirely). An agent stuck in
Enforced may want a per-call `find --no-ignore` to recover traditional find behavior.
Record-don't-build: only meaningful under Enforced, and adds a non-POSIX flag to the
"trained-in habits" builtin. Add if an Enforced-mode consumer actually hits it.

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

### `compute_value_context`: `[[`/`]]` detection vs `]`-containing glob char-classes
Surfaced in the collection-literals cross-model review (gemini-pro, 2026-07-02).
The value-position suppression (`lexer.rs::compute_value_context`) discriminates
assignment `=` from `[[ ]]` comparison `=` (and membership `in` from a for/case
head `in`) by tracking `[[ ]]` **test depth** in a forward pass over raw tokens.
`[[`/`]]` are deduced from *adjacent single brackets* (`LBracket LBracket` /
`RBracket RBracket`) **before glob-fusion**, which is inherently fragile: a glob
char-class that itself contains `]` (`[]]` matches a literal `]`, lexed as
`[ ] ]`) has an inner `]` at `bracket_depth==0` that can prematurely decrement
`test_depth`.
- **Trigger:** a `]`-char-class glob AND a *later* single-`=` bracket-glob
  comparison inside the SAME `[[ ]]` — e.g. `[[ $a == []] && $b = [0-9] ]]`.
  The desynced `test_depth` reads the later `=` as an assignment, suppresses the
  `[0-9]` glob, and the comparison RHS (parsed on `primary_expr_parser`, no
  list-literal arm) hits primitive brackets.
- **Failure mode: LOUD.** A parse error ("found '['"), never a wrong result —
  per the "crash beats corrupt" directive this is acceptable to ship. Confirmed:
  the *common* case `[[ $x == []] ]]` (a `]`-char-class alone in a test) parses
  and runs identically to bash — the internal desync suppresses nothing and the
  parser matches its own brackets.
- **Why low priority:** exotic construct (a `]`-char-class next to a single-`=`
  bracket-glob comparison in one test); every common form is unaffected. Pinned
  by `collection_literals_tests.rs::bracket_char_class_containing_rbracket_in_test_parses`
  (common case works) plus an `#[ignore]`'d
  `rbracket_char_class_plus_later_eq_comparison_currently_errors` documenting the
  exotic failure so a future fix is visible.
- **Robust fix if it ever matters:** deducing `[[`/`]]` from adjacent single
  brackets before glob-fusion can't be made reliable. The durable options are a
  context-aware test-region pass, or reworking the pipeline so glob char-classes
  are matched as single units before `compute_value_context` runs (so a `]`
  inside a class is never mistaken for a test delimiter).

### Pre-release sweep — minor / edge (2026-06-23, verified)
- **Backticks inside double-quotes and heredoc bodies are silently literal** — bare
  backticks are a loud lexer error, but quoted/heredoc ones slip through
  `parse_string_literal`. Should reject with the same `$(cmd)` hint. (`lexer.rs`)
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

### `FileWalker` symlink-following is broken (dormant — no production caller)
Surfaced by the cross-family review of the LocalFs list TOCTOU fix (PR #41,
2026-06-29, Gemini Pro). Both bugs are latent: nothing in production sets
`FileWalkOptions::follow_symlinks: true` (only the doc comment and tests
reference it), so they bite only once a builtin exposes a `-L`/`--follow` flag.
- **Dir symlinks are never recursed.** The walker decides recursion on
  `entry.is_dir()` (`kaish-glob/src/walker.rs:248,308`), but `DirEntry::is_dir()`
  is `kind == Directory` and a symlink is the distinct `Symlink` kind
  (`kaish-types/src/dir_entry.rs:72`), so `is_dir()` is always false for a
  directory symlink — even with `follow_symlinks` on, the link is yielded as a
  file, never descended. Fix: gate on `is_dir || (is_symlink && follow_symlinks)`
  and `stat`-through (`self.fs.is_dir(&full_path)`, which follows the link) to
  decide whether the target is a directory.
- **Cycle detection is a no-op in production.** Cycle detection keys
  `visited_dirs` on `self.fs.canonicalize(&full_path)`, but `BackendWalkerFs`
  (`kaish-kernel/src/backend_walker_fs.rs:37`) doesn't override `canonicalize`,
  so it returns the path unresolved — a circular symlink would grow unique paths
  forever (infinite loop / OOM). Fix: implement `canonicalize` on
  `BackendWalkerFs` via `KernelBackend` link resolution before any `-L` flag ships.

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
