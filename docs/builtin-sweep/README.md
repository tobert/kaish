# Builtin sweep — breadth-first correction (burn-down)

**Temporary, delete-before-next-release** (issues.md-style, like
`docs/awk-overhaul.md`). Goal: shake out **dangerous edges** across the
oracle-... *model-backed* builtins before the next kaish release and kaibo's
larger release. Deep per-tool passes (awk, sed) come later; this is triage.

## Method (model-memory, NOT GNU)
Reference of correctness = **3-vendor model consensus** (Sonnet 4.6,
DeepSeek-V4-Pro, Gemini-3.5-Flash), generated independently — **not** a GNU run
(`[[model-memory-over-gnu-oracle]]`). Per cluster:
1. **Battery** — fixed `(program, input)` cells, identical for all vendors.
   Panel answers from memory only (no kaish, no real binary).
2. **Panel** — DeepSeek + Gemini via kaibo `oneshot`; Sonnet via subagent
   (tools forbidden). `battery-*.md` holds the cells + the verbatim prompt.
3. **Bank** — `expectations-*.md`: unanimous values + contested cells, each
   **routed**: **CODE** (kaish diverges from clear consensus → fix) or **DOC**
   (panel split on what's standard → kaish picks one behavior + documents it
   crystal-clear so agents don't guess). Many are both.
4. **Phase 2 (deferred** — awk session owns the tree): `run-*.sh` classifies
   kaish via `harness.sh` → `MATCH | SILENT-WRONG | LOUD | WRONG-RC`. The hunt
   target is **SILENT-WRONG** (exit 0, wrong bytes — the data hazard).

This pass is **catalog-only**: zero builtin code changed. Fixes are a later
deliberate pass.

## Cluster progress
| Cluster | Tools | Battery | Panel | Banked | Phase 2 |
|---------|-------|:-------:|:-----:|:------:|:-------:|
| pilot | cut tr sort | ✅ | ✅ | ✅ | ⏳ deferred |
| line-slicers | head tail tac uniq wc | ✅ | ✅ | ✅ | ⏳ deferred |
| search | grep ~~rg~~ | ✅ | ✅ | ✅ | ⏳ deferred |
| generate/transform | printf seq split jq | ✅ | ✅ | ✅ | ⏳ deferred |
| encode/binary | base64 xxd checksum dd | ✅ | ✅ | ✅ | ⏳ deferred |
| compare/patch | cmp diff patch | ⬜ | ⬜ | ⬜ | ⬜ |
| passthrough/io | cat tee | ⬜ | ⬜ | ⬜ | ⬜ |

(FS mutators cp/mv/rm/ln/mkdir/touch/write — separate behavioral pass, out of
scope here per the scope decision. `format_string` is the shared printf/awk
sprintf parser, NOT a builtin — excluded from the sweep.)

## Accumulated findings (fill as Phase 2 runs)
Cross-tool levers worth fixing/documenting regardless of any single cell:
- **No-match exit codes (CODE, contract).** `grep`/`rg` no-match → rc 1, 3/3
  unanimous. Agents gate on `if grep -q`. A rc-0 here silently inverts control
  flow → P1. Audit every search/test-shaped builtin's exit contract.
- **ERE-always dialect consistency (CODE+DOC).** grep/rg should accept ERE
  (`a{2}`, `(a|b)`) and **loud-error BRE idioms** (`a\{2\}`, `\(…\)`, `\|`,
  pattern backrefs) with an ERE hint — the same diagnostics sed got
  (`[[sed_ergonomics_pass]]`). Make the shell's regex story read consistently
  across grep/rg/sed/awk.
- **Numeric field-width / padding (DOC).** `wc`, `uniq -c` — panel agrees on
  shape, splits on exact width. Decide kaish's rule, pin a test, document the
  byte shape so it's contractual.
- **rg stdin defaults (DOC).** No line numbers / no color / case-sensitive on a
  pipe — models *hesitate* here, so agents will. Document explicitly. **NOTE: `rg`
  is likely to be DROPPED** (decided 2026-06-16) — it duplicates `grep`'s engine
  with a divergent default surface (the hesitation above is itself evidence the
  second tool earns its keep poorly). If dropped, the R* cells become a
  removal-safety check, not a fidelity target; the no-match-rc and ERE-dialect
  findings still apply to `grep`.
- **Trailing-newline under delete/complement (CODE).** `tr -cd` drops the final
  `\n` (3/3); base64 `-d` / `xxd -r` likewise. Producers add a newline, decoders/
  transformers don't. Harness is sentinel-guarded to catch a kaish that keeps it.
- **Namesake collision must be LOUD (CODE — wave-2 headline).** Where kaish reuses
  a familiar name with a narrower contract, the GNU reflex must error, never
  silently misbehave: `split -l 100` (string-splitter, no `-l`), `sha256sum` (it's
  `checksum`), `printf | dd` (dd requires `if=`, no stdin), `dd … bogus`. All 3/3
  unanimous they should be loud. Phase 2 asserts rc≠0 + a hint on each.
- **jq env boundary (CODE, P1-class).** kaish jq (jaq subset) is hermetic; `env`
  must fail loud, but real jq returns `null` silently — verify jaq doesn't inherit
  the silent-null and quietly diverge from kaish's own contract.
- **Format/layout decisions (DOC).** base64 wrap-76, `xxd` default-dump spacing,
  `checksum` line shape (`hash  -` vs bare hex; must round-trip with `-c`). Decide,
  pin a test, document — same class as the wc/uniq padding lever.

## Files
`battery-<cluster>.md` cells+prompt · `expectations-<cluster>.md` banked
consensus+routing · `run-<cluster>.sh` deferred Phase-2 runner · `harness.sh`
the no-GNU classifier.
