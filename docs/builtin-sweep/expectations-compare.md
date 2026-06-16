# Builtin sweep — compare/patch expectations (cmp / diff / patch)

Banked 3-vendor consensus (Sonnet 4.6, DeepSeek-V4-Pro, Gemini-3.5-Flash),
2026-06-16. Reference = consensus, not GNU (`[[model-memory-over-gnu-oracle]]`).
The `patch`/`tee` write-model row is from **reading the source**, not the panel
(panel can't see kaish internals; the kaibo consult flaked empty). Routing:
CODE / DOC. Legend: ↵ / ∅↵. Fixtures: A=`apple⏎banana⏎cherry`, B=line-2
`BANANA`, C==A, P=unified diff A→B.

## cmp — exit-code contract is the point

| ID | program | expected | rc |
|----|---------|----------|----|
| CM1 | `cmp A C` | *(empty)* | 0 |
| CM2 | `cmp A B` | `A B differ: byte 7, line 2` | 1 |
| CM3 | `cmp -s A B` | *(empty)* | 1 |

- **Exit codes 0/1/2 (CODE, contract).** identical→0, differ→1, error→2; agents
  gate on this (`if cmp -s`). 3/3 on the codes. Assert rc precisely in Phase 2.
- **CM2 wording (DOC).** value `7`/`line 2` unanimous; "byte" (2/3) vs "char"
  (Sonnet) — pin kaish's exact message and document it.

## diff — DEFAULT FORMAT is the headline divergence

| ID | program | expected | rc | route |
|----|---------|----------|----|----|
| DF1 | `diff A C` | *(empty)* | 0 | — |
| DF2 | `diff A B` (no flags) | **`2c2`⏎`< banana`⏎`---`⏎`> BANANA`** (ed-style normal) | 1 | **CODE+DOC** |
| DF3 | `diff -u A B` | unified hunk (`--- / +++ / @@ -1,3 +1,3 @@ / apple / -banana / +BANANA / cherry`) | 1 | — |
| DF4 | `diff -q A B` | `Files A and B differ` | 1 | — |

- **DF2 — the wave-3 headline (CODE+DOC).** All 3 vendors predict plain `diff A B`
  emits the **ed-style "normal" format** (`2c2 …`) by muscle memory. kaish's `diff`
  **defaults to unified** (`-u`). So `diff a b` with no flags diverges from what
  every agent reflexively expects. Decision: keep the unified default (more useful
  for agents/patch — likely) but **document the divergence loudly**, or match the
  normal-format expectation. Not silent-wrong (output is valid), but surprising.
- **DF3** unified body unanimous (timestamps in the `---/+++` header vary; confirm
  kaish's header form — filenames only vs with mtime). Exit-code 0/1/2 contract as
  cmp.

## patch — value unanimous; dry-run wording split

| ID | program | expected | rc |
|----|---------|----------|----|
| PT1 | `patch A < P` | stdout `patching file A`; **A becomes** `apple⏎BANANA⏎cherry` | 0 |
| PT2 | `patch --dry-run A < P` | A **unmodified** (3/3); message `checking file A` (Gemini) vs `patching file A` (2/3) | 0 |

- **PT2 (DOC):** all agree the file is untouched under `--dry-run`; the printed
  message splits (GNU patch actually says `checking file A` in dry-run). Pin
  kaish's wording + document.

## WRITE MODEL — from source (tee.rs / patch.rs), not panel

The wave-3 safety question for the file-mutating builtins:

- **Both route through the VFS, NOT std::fs (✅ overlay-safe).** `tee` →
  `ctx.backend.write(path, …, WriteMode::Overwrite)`; `patch` →
  `ctx.backend.patch(path, &ops)`. Under `--overlay` their mutations land in the
  overlay transaction; sandbox/NoLocal modes hold. No direct-to-disk bypass.
- **Neither is gated by the confirmation latch (⚠️ DECISION OWED).** `rm` runs
  through `decide_rm_action` (latch + trash); `tee existing` truncates and `patch`
  rewrites **without a nonce**. Destructive *overwrite* isn't gated the way
  destructive *delete* is — an inconsistency. Decide: should `tee`/`patch` over a
  pre-existing file require the latch (or trash the prior content) under
  `set -o latch`? Same hazard class as sed's deferred `-i`
  (`[[sed_ergonomics_pass]]`); track with it.
- **tee `-a` is read-modify-write `Overwrite`** (kaish VFS has no native append) —
  not an atomic append; note for concurrency.

## Phase-2 watch list
1. **cmp/diff exit codes** 0/1/2 — assert rc, not just output.
2. **DF2 default = unified** (kaish) vs **normal** (model reflex) — confirm + DOC.
3. **patch/tee latch gating** — behavioral test under `--overlay` and `set -o latch`.
4. **CM2 / PT2 message wording** — pin + DOC.
