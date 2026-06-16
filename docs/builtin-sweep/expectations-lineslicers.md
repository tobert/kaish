# Builtin sweep — line-slicers expectations (head / tail / tac / uniq / wc)

Banked 3-vendor consensus (Sonnet 4.6, DeepSeek-V4-Pro, Gemini-3.5-Flash),
2026-06-16. Reference = model consensus, not GNU (`[[model-memory-over-gnu-oracle]]`).
Phase 2 runs kaish against these once the tree is stable. Temporary.

**Routing of disagreements** (per Amy, 2026-06-16): a panel split is signal, not
noise. **DOC** = models disagree on what's standard → kaish picks one behavior and
documents it crystal-clear (agents will guess wrong otherwise). **CODE** = kaish
diverges from a *clear* consensus → fix. Many cells are both.

Legend: ↵ trailing newline · ∅↵ no trailing newline.

## values — unanimous (3/3), bank as-is

| ID | kaish program | stdin | expected stdout | nl |
|----|---------------|-------|-----------------|----|
| H1 | `head -n 2` | `apple`⏎`banana`⏎`cherry`⏎`date` | `apple`⏎`banana` | ↵ |
| H2 | `head -n -1` | same | `apple`⏎`banana`⏎`cherry` | ↵ |
| H3 | `head -c 5` | same | `apple` | **∅↵** |
| TL1 | `tail -n 2` | same | `cherry`⏎`date` | ↵ |
| TL2 | `tail -n +2` | same | `banana`⏎`cherry`⏎`date` | ↵ |
| TL3 | `tail -c 5` | same | `date` | ↵ |
| TC1 | `tac` | same | `date`⏎`cherry`⏎`banana`⏎`apple` | ↵ |
| U1 | `uniq` | `a`⏎`a`⏎`b`⏎`a`⏎`c`⏎`c` | `a`⏎`b`⏎`a`⏎`c` | ↵ |
| U3 | `uniq -d` | same | `a`⏎`c` | ↵ |
| W5 | `wc -l` | `a\nb` (no final ↵) | `1` (the value) | ↵ |

- **H2** all three produced `head -n -1`; GNU-vs-BSD support note only (coverage,
  not value). **H3** byte-boundary: 5th byte is `e`, so no trailing newline —
  Sonnet self-corrected to this; 3/3. **W5** the value `1` is unanimous (wc counts
  newlines): CODE — verify kaish reports 1, not 2.

## format — contested (DOC lever: decide kaish's rule, then document it)

| ID | kaish program | shape all agree on | the split |
|----|---------------|--------------------|-----------|
| W1 | `wc -l` | the count | **2/3 unpadded `2`**; DeepSeek BSD-padded `      2` |
| W2 | `wc -w` | the count | 2/3 unpadded `3`; DeepSeek padded |
| W3 | `wc -c` | the count | 2/3 unpadded `16`; DeepSeek padded |
| W4 | `wc` | `lines words bytes`, right-justified, 1-space sep | exact field **width** differs across all three |
| U2 | `uniq -c` | `<count> <line>`, count right-justified | field width: DeepSeek/Sonnet ~7, Gemini ~6 |

- **Decision owed:** kaish's numeric field-width rule for `wc`/`uniq -c`. Consensus
  leans GNU: single-count `wc -lN` **unpadded**, multi-column `wc` right-justified to
  a common width. Phase 2 observes kaish's actual format; whatever we keep, **document
  it** in the wc/uniq help fragment so the byte shape is contractual, not surprising.
- These are unlikely to be P1 *corruption* (agents usually `awk '{print $1}'` the
  count), but a silently-different shape still breaks byte-exact consumers — so:
  pick, pin with a test, document.

## Phase-2 watch list
1. **W5** newline-counting value (`1`) — CODE, semantic.
2. **W1–W4 / U2** numeric padding — DOC (decide + document + pin).
3. **H3 / TL3** byte boundaries incl. trailing-newline (sentinel-checked).
4. **H2 / TC1** `head -n -N` and `tac` coverage (must work or fail loud).
