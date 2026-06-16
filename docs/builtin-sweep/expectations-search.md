# Builtin sweep — search expectations (grep / rg)

Banked 3-vendor consensus (Sonnet 4.6, DeepSeek-V4-Pro, Gemini-3.5-Flash),
2026-06-16. Reference = model consensus, not GNU (`[[model-memory-over-gnu-oracle]]`).
The regex-dialect cluster — kaish is ERE-always (`regex` crate, no backrefs),
same posture as sed/awk. Phase 2 runs kaish against these. Temporary.

> **`rg` likely DROPPED (2026-06-16).** It duplicates `grep`'s engine with a
> divergent, hesitation-inducing default surface (R1/R5 below). If removed, the
> R* cells are a removal-safety check, not a fidelity target — the no-match-rc
> and ERE-dialect findings carry over to `grep`. Decision pending.

**Routing:** DOC = panel split on what's standard → kaish picks + documents.
CODE = kaish diverges from clear consensus → fix. Legend: ↵ / ∅↵.

## values — unanimous (3/3), bank as-is

| ID | kaish program | stdin | expected stdout | rc |
|----|---------------|-------|-----------------|----|
| G1 | `grep foo` | `foo`⏎`Foobar`⏎`baz`⏎`foofoo` | `foo`⏎`foofoo` | 0 |
| G2 | `grep -v foo` | same | `Foobar`⏎`baz` | 0 |
| G3 | `grep -i foo` | same | `foo`⏎`Foobar`⏎`foofoo` | 0 |
| G4 | `grep -c foo` | same | `2` (matching **lines**, not occurrences) | 0 |
| G5 | `grep -o foo` | same | `foo`⏎`foo`⏎`foo` | 0 |
| G6 | `grep -n foo` | same | `1:foo`⏎`4:foofoo` | 0 |
| G7 | `grep -E 'baz\|Foo'` | same | `Foobar`⏎`baz` | 0 |
| G8 | `grep zzz` | same | *(empty)* | **1** |
| R2 | `rg -i foo` | same | `foo`⏎`Foobar`⏎`foofoo` | 0 |
| R3 | `rg -o foo` | same | `foo`⏎`foo`⏎`foo` | 0 |
| R4 | `rg -c foo` | same | `2` (matching lines) | 0 |
| R5 | `rg -n foo` | same | `1:foo`⏎`4:foofoo` | 0 |
| R6 | `rg zzz` | same | *(empty)* | **1** |

## the three levers

### 1. No-match exit code = 1  — CODE (contract)
**G8 / R6 unanimous, rc 1.** Agents gate on `if grep -q …` / `grep … && …`. If
kaish `grep`/`rg` exits 0 on no match, that's a **P1 silent contract break** —
control flow silently inverts. Phase 2 must assert rc precisely (the harness
`expected-rc` arg). Also confirm `grep -q` (quiet, rc-only) and `-c` with zero
matches → prints `0`, rc 1.

### 2. Brace interval dialect (G9) — CODE + DOC, cross-tool consistency
| vendor | cmd | predicted out |
|--------|-----|---------------|
| DeepSeek | `grep 'a\{2\}'` (BRE, escaped) | `aa` |
| Gemini | `grep -E 'a{2}'` (ERE) | `aa` |
| Sonnet | `grep -E 'a{2}'` (ERE) | `aa` |

stdin `aa`⏎`a` → output `aa` unanimous; the **command** splits on dialect. kaish
is ERE-always, so:
- `grep -E 'a{2}'` (and bare `grep 'a{2}'`) → `aa`, rc 0. **CODE: must match.**
- `grep 'a\{2\}'` (DeepSeek's BRE reflex) → kaish should **loud-error with a hint
  to the ERE form**, exactly as sed does for `\{N,M\}` (`[[sed_ergonomics_pass]]`).
  Silent literal-match or no-match here would be a P1. Audit grep/rg for the same
  BRE-idiom diagnostics sed got: `\(…\)`, `\|`, `\{…\}`, pattern backrefs.
- **DOC:** state grep/rg's ERE-always dialect + the loud BRE boundaries in the
  grep help fragment, cross-linked to sed/awk so the shell reads consistently.

### 3. rg stdin defaults (R1 / R5) — DOC (models hesitate)
**R1 `rg foo`** consensus → `foo`⏎`foofoo`, rc 0, **no line numbers, no color** on
a pipe. But the panel was *visibly unsure*: Sonnet flip-flopped mid-answer (first
emitted `1:foo`/`4:foofoo`, then corrected). When models hesitate, agents will too.
- **DOC:** document kaish `rg`'s stdin behavior explicitly — no line numbers
  unless `-n`, no color on non-tty, case-sensitive by default (smart-case is
  opt-in). **CODE:** verify kaish `rg foo` on a pipe emits bare lines, not
  `N:line` (a silent `-n`-on-by-default would diverge from consensus).

## Phase-2 watch list
1. **G8 / R6** no-match rc=1 — assert exit code, not just output.
2. **G9** ERE `a{2}` matches; BRE `a\{2\}` fails loud (sed-consistent).
3. **R1** rg stdin emits bare lines (no auto line numbers).
4. **G4 / R4** `-c` semantics = matching lines; zero-match `-c` → `0` + rc 1.
