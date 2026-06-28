# Search features port — rg-style filtering for `grep` + `glob`

**Status:** design + punch-list, **transient** — delete this file when the work ships
(awk-overhaul.md precedent). Tracked from `docs/issues.md` P1.

Port the still-useful ripgrep-only flags onto kaish's two *modern* search builtins.
`rg` was removed under the 80%-rule (one search builtin); some of its filtering is
worth re-homing on the file-walking layer kaish-glob already owns. Driver: **kaibo**,
a read-only codebase-analysis MCP whose hot path is "grep this repo" — `grep --ftype rust`
and `--max-count N` early-stop are the ergonomics that make it feel fast and precise.

## What already exists (engine is done)

This is surface wiring, not engine work. The walker already supports everything:

- `WalkOptions.types: Option<Arc<ignore::types::Types>>` (`kaish-glob/src/walker.rs:86`),
  **applied at walker.rs:286** and tested (`test_walk_types_select_only_rust`,
  `test_walk_types_negate_excludes`). The directory-pruning hazard is already handled:
  `Types::matched` returns `Match::None` for directories, so a type filter never stops
  traversal (walker.rs:284-285, comment + behavior).
- `WalkOptions.include_hidden` — dotglob semantics (walker.rs:268-273).
- `IgnoreConfig` (`kaish-kernel/src/ignore_config.rs`) — the gitignore policy layer with
  `IgnoreScope::{Advisory, Enforced}`, `.ignore`/`.rgignore` precedence, ancestor
  walk-up, global-gitignore, and a runtime `ignore` builtin. `IgnoreConfig::agent()`
  (kaibo's preset) is `Enforced` + `auto_gitignore`.

No builtin populates `types` today — the machinery is live but dormant. No new
dependency (`ignore` is already in kaish-glob).

## Decisions (the design forks, resolved)

1. **`find` is not modified.** It stays POSIX (`--type f` = entry-kind, includes
   hidden, traditional). Users wanting modern filtering reach for `glob`. (Deferred
   note: an optional `find --no-ignore` escape *only* matters under `Enforced` scope —
   recorded in `docs/issues.md` P3, record-don't-build.)

2. **`--ftype` is the kaish-wide standard for file-type filtering.** Not rg-native
   `-t/--type`. One flag name, every search builtin, now and future:
   - select: `--ftype <NAME>` (repeatable)
   - negate: `--ftype-not <NAME>` (repeatable)
   - discover: `--ftype-list` (print the type→globs table)

   Rationale: `glob` already owns `-t/--type` for **entry-kind** (`f`/`d`/`a`, the fd
   convention) — file-type is a different axis and gets its own consistent name rather
   than colliding or splitting per-builtin. This becomes a kaish naming convention
   (record in `docs/NAMING.md` on ship).

3. **New flags are long-only — no short forms.** `--ftype`, `--ftype-not`,
   `--ftype-list`, `--hidden`, `--max-count`. No `-t`/`-T`/`-m`. Side benefit: sidesteps
   GNU grep's `-T`/`--initial-tab` and `-m` muscle-memory entirely — kaish isn't
   pretending to be GNU grep's short-flag surface, just borrowing the useful long names.
   (Existing shorts on existing flags — glob's `-t`/`-a` — are untouched; the rule is
   for *new* flags.)

4. **Unknown type name is loud.** `grep --ftype nonsense` → exit 2 (usage), never a
   silent empty match. (`TypesBuilder::build()` surfaces `UnrecognizedFileType`; map it
   to a builtin usage error — verify the exact error path during impl.) Per the
   no-silent-fallback directive.

5. **`--no-ignore` is DEFERRED — don't ship a flag that lies.** The honest semantics of
   a per-call ignore-bypass under an embedder's `Enforced` scope aren't designed yet
   (should an explicit user flag override an embedder's context-flood protection? that's
   an embedder-policy question, not a one-liner). So `grep` does **not** get `--no-ignore`
   this round; figure out the correct `Enforced` interaction first, then add it across
   the search builtins consistently. `glob`'s *existing* `--no-ignore` is left exactly
   as-is (pre-existing behavior, separate question — see the audit note below).

## Surface matrix

| flag (all long-only) | grep | glob | find |
|---|---|---|---|
| `--ftype <NAME>` (file-type select, repeatable) | ✅ new | ✅ new | ✗ |
| `--ftype-not <NAME>` (negate, repeatable) | ✅ new | ✅ new | ✗ |
| `--ftype-list` (print types→globs table) | ✅ new | ✅ new | ✗ |
| `--hidden` (include dotfiles in walk) | ✅ new (default off) | exists (`-a/--hidden`) | ✗ (always on) |
| `--max-count <N>` (stop after N matches/file) | ✅ new (GNU semantics) | — (no line matching) | — |
| `--no-ignore` | ✗ DEFERRED (decision 5) | exists (audit) | ✗ |

`--ftype*`/`--hidden` on grep apply to the **`-r` recursive walk** only; on explicit
file args they're inert (don't error).

## Shared helper (kaish-glob)

One place builds the `Types` so grep + glob can't drift. New module
`kaish-glob/src/file_types.rs`:

```rust
/// Build an rg-style file-type filter. `Ok(None)` when both lists are empty.
/// `Err` on an unrecognized type name (loud — never a silent empty match).
pub fn build_file_types(
    select: &[String],
    negate: &[String],
) -> Result<Option<Arc<ignore::types::Types>>, FileTypeError>;

/// All known type definitions as (name, globs) rows, for `--ftype-list`.
pub fn list_file_types() -> Vec<(String, Vec<String>)>;

#[derive(Debug, thiserror::Error)]
pub enum FileTypeError { /* UnrecognizedFileType(String), Build(..) */ }
```

Builtins call `build_file_types(...)`, stuff the result into `WalkOptions.types`, and
map `FileTypeError` → `failure(2, ...)`. `--ftype-list` emits
`OutputData::table(["TYPE","GLOBS"], rows)` (→ `--json` for free), valid with no
pattern, exit 0.

## `--max-count <N>` semantics (grep)

GNU/rg faithful, **per file**:
- Stop after N matching lines in a given file; under `-r`, each file gets its own N.
- **Early-terminates the chunk reader** — once N matches land, stop pulling that file's
  remaining 256 KiB windows (reuse the existing binary/quit-byte stop signal — a real
  streaming win on big files, which is the kaibo case).
- `--max-count 0` → match nothing, exit 1.
- with `-c/--count`: report `min(matches, N)`.
- with `-l`/`-q`: naturally satisfied at the first match (`--max-count 1`-equivalent).
- with `-v` (invert): N *non*-matching lines.

## Edge cases / loud behavior

- Unknown type → exit 2 (decision 4).
- `--ftype` + `--ftype-not` together: rg-defined; pass both to `TypesBuilder`.
- glob entry-kind `-t d` must remain intact alongside `--ftype` (regression test).
- **`--ftype` is a no-op on directories** — `Types::matched` returns `None` for a
  dir, so a file-type filter narrows *files* but never gates dirs (the same
  property that keeps recursive type-filtered walks traversable). Consequence:
  `glob -t d --ftype rust` still lists directories. Worth a line in help so it
  doesn't read as a bug.

## Punch list

**P1 — ship (grep, kaibo's hot path)**
- [x] `kaish-glob/src/file_types.rs`: `build_file_types`, `list_file_types`,
      `FileTypeError`; export from `lib.rs` + re-export in kernel `walker` mod.
      (5 unit tests.)
- [x] grep clap: `--ftype` (Vec), `--ftype-not` (Vec), `--ftype-list`, `--hidden`.
- [x] grep execute: populate `WalkOptions.types`/`include_hidden`; `--ftype-list`
      short-circuit; loud unknown-type. (`grep_search_features_tests.rs`, 7 kernel-routed.)
- [x] **grep `--max-count`** — per-file cap across all four match paths
      (streaming-stdin, single-file scanner, whole-buffer, multi-file). Streaming
      paths early-stop the chunk reader (`hit_limit`, distinct from the quit-byte
      fallback); whole-buffer caps in `render_events` (lets trailing after-context
      through). `--max-count 0` = exit 1, no output. (5 more kernel-routed tests.)

**P2 — glob parity**
- [x] glob clap: `--ftype` (Vec), `--ftype-not` (Vec), `--ftype-list`. Kept
      `-t`=entry-kind, `-a/--hidden`, existing `--no-ignore`.
- [x] glob execute: populate `WalkOptions.types` via shared helper; `--ftype-list`.
      `read_repeatable_strings` lifted to `builtin/mod.rs` so grep+glob share one
      reader (can't drift). (`glob_search_features_tests.rs`, 6 kernel-routed,
      incl. the `-t` entry-kind regression + the dirs-no-op interaction.)

**Deferred (record-don't-build)**
- grep `--no-ignore`: design the `Enforced`-scope interaction first (decision 5).
- **Audit glob's existing `--no-ignore` under `Enforced`** — it unconditionally skips
  the filter regardless of scope; confirm that's intended (vs. silently escaping an
  embedder's context-flood protection). Feeds the deferred `--no-ignore` design.
- `find --no-ignore` escape under `Enforced` → `docs/issues.md` P3.

## Test plan (kernel-routed)

- **kaish-glob unit:** `build_file_types` select/negate/unknown-error/empty;
  `list_file_types` nonempty + contains `rust→*.rs`. (Walk-level type filtering already
  covered by walker.rs tests.)
- **grep (`grep_search_features_tests.rs`):** `--ftype rust` over a temp tree of `.rs`/
  `.py`/`.md` → only `.rs`; `--ftype-not rust` excludes; unknown type → exit 2;
  `--hidden` reaches a dotfile under `-r`; `--max-count 2` caps per file + a >2-match
  file proves early stop; `--max-count` with `-c` reports capped; `--ftype-list` table +
  `--json` shape.
- **glob (`glob_search_features_tests.rs`):** `--ftype rust` → only `.rs`; `--ftype-not`;
  unknown → exit 2; **`-t d` entry-kind still works** (regression); `--ftype-list`.

## Docs to sync (on ship)

- [x] `docs/NAMING.md` — `--ftype` file-type-filter convention recorded (decision 2).
- [x] builtin help — flag descriptions live in the clap doc-comments and flow
      through `schema_from_clap` into `help grep`/`help glob` automatically; no
      hand-written per-builtin help file exists to edit.
- [x] `CHANGELOG.md` Added bullets (grep ×3, glob ×1). README describes builtins
      only at category level (no per-flag list), so nothing to sync there.
- [ ] **Delete this file** once the port lands (the work is complete; this is the
      last open box). Deferred `--no-ignore` design + the glob `--no-ignore`-under-
      Enforced audit live in `docs/issues.md`, so they survive deletion.
