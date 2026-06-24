# kaish devlog

Narrative history of landed work and the decisions behind it — the "how we got
here" color that used to clutter [issues.md](issues.md) and tax every read of it.
This is *not* the authoritative record: `CHANGELOG.md` (per-version, user-facing)
and `git log` (commits, SHAs) are canonical. This is the story.

Newest themes first within each area; dates are when the work landed.

---

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
