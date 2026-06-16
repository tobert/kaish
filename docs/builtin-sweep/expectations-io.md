# Builtin sweep — passthrough/io expectations (cat / tee)

Banked 3-vendor consensus (Sonnet 4.6, DeepSeek-V4-Pro, Gemini-3.5-Flash),
2026-06-16. Reference = consensus, not GNU (`[[model-memory-over-gnu-oracle]]`).
`tee` write-model = see expectations-compare.md (routes through VFS, not
latch-gated). Routing: CODE / DOC. Legend: ↵ / ∅↵. Fixture A=`apple⏎banana⏎cherry`.

## cat

| ID | program | expected | route |
|----|---------|----------|-------|
| CT1 | `cat A` | `apple⏎banana⏎cherry` | — (3/3) |
| CT2 | `cat -n A` | `␠␠␠␠␠1<TAB>apple` … (6-wide right-justified number + literal tab) | DOC |
| CT3 | `cat A C` | A then C, 6 lines, no separator | — (3/3) |
| CT4 | `echo hi \| cat` | `hi` | — (3/3) |
| CT5 | `cat /dev/zero` | **loud error** (kaish) vs **infinite hang** (model reflex) | CODE+DOC |

- **CT2 field-width (DOC).** 3/3: GNU `cat -n` uses `%6d\t` (6-wide right-justified
  + tab). Same padding-decision class as wc/uniq — pin kaish's width + document.
- **CT5 — intentional safety divergence (CODE+DOC, the good kind).** All 3 expect
  `cat /dev/zero` to stream NUL forever / hang. kaish deliberately makes it a
  **loud error** (DevFs, `[[project_dev_fs]]`) rather than hang or silently
  truncate. Phase 2: confirm it's loud (not a hang, not a silent partial read).
  DOC the divergence — it's kaish being *safer* than muscle memory, which agents
  should know.

## tee

| ID | program | expected stdout | file after |
|----|---------|-----------------|------------|
| TE1 | `echo hi \| tee out.txt` | `hi` | `hi` |
| TE2 | `printf 'a\n' \| tee -a out.txt` (had `x`) | `a` | `x⏎a` |

3/3 on both; `-a` appends (kaish does read-modify-write, see compare doc). tee
passes input through to stdout unchanged (binary-safe — reads bytes).

## Phase-2 watch list
1. **CT5** `/dev/zero` is loud, not a hang — CODE (safety), and DOC the divergence.
2. **CT2** `cat -n` field-width — DOC (decide + pin).
3. **tee** write routing/latch — covered in expectations-compare.md.
