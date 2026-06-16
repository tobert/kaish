# Builtin sweep — generate/transform expectations (printf / seq / split / jq)

Banked 3-vendor consensus (Sonnet 4.6, DeepSeek-V4-Pro, Gemini-3.5-Flash),
2026-06-16. printf/seq = model-memory; split/jq = contract-first (panel given
kaish's contract). Reference = consensus, not GNU (`[[model-memory-over-gnu-oracle]]`).
Routing: **CODE** (kaish diverges from clear consensus → fix) / **DOC** (panel
split or surprising-by-design → document crisply). Legend: ↵ / ∅↵.

## printf — unanimous (3/3), CODE: verify kaish matches

| ID | program | expected | nl |
|----|---------|----------|----|
| P1 | `printf '%s\n' a b c` | `a`⏎`b`⏎`c` | ↵ |
| P2 | `printf '%s-%s\n' a b c d` | `a-b`⏎`c-d` | ↵ |
| P3 | `printf '%d-%d\n' 1 2 3` | `1-2`⏎`3-0` | ↵ |
| P4 | `printf '%05.2f\n' 3.14159` | `03.14` | ↵ |
| P5 | `printf '%x\n' 255` | `ff` | ↵ |
| P6 | `printf 'a\tb\n'` | `a<TAB>b` | ↵ |
| P7 | `printf '%s\n'` | *(empty line)* | ↵ |

- **P3 / P7 are the sharp ones:** format **cycling** with default-fill (missing
  `%d`→`0`, missing `%s`→empty) — 3/3. kaish's `format_string_cycling` matches by
  design; pin a differential test so a regression can't silence it.

## seq — unanimous (3/3)

`seq 3`→`1`⏎`2`⏎`3` · `seq 2 5`→`2..5` · `seq 1 2 7`→`1`⏎`3`⏎`5`⏎`7` ·
`seq -w 8 10`→`08`⏎`09`⏎`10` · `seq 3 -1 1`→`3`⏎`2`⏎`1` · `seq -s, 1 3`→`1,2,3`↵ ·
`seq 5 2` (start>end, +step)→ *empty*, rc 0.

## split — contract-first

| ID | program | expected | route |
|----|---------|----------|-------|
| SP1 | `echo "a:b:c" \| split ":"` | `a`⏎`b`⏎`c` | DOC |
| SP2 | `split "hello world"` | `hello`⏎`world` | — (3/3) |
| SP3 | `split "a:b:c:d" ":" --limit=2` | `a`⏎`b:c:d` | — (3/3) |
| SP4 | `split "a1b2c3" -r "[0-9]"` | `a`⏎`b`⏎`c`⏎*(empty)* | DOC/decision |
| SP5 | `split -l 100 access.log` | *(empty)*, **rc≠0 LOUD** | CODE (3/3) |

- **SP1 — stdin newline trim.** kaish `split` calls `trim_end_matches('\n')` on
  piped input, so `echo`'s `\n` does NOT leak a trailing blank line → `a`⏎`b`⏎`c`.
  2/3 agree; DeepSeek predicted the blank (naive behavior). The code is right;
  **DOC** the trim so agents aren't surprised either way.
- **SP4 — trailing empty.** `regex::split` keeps the trailing empty piece →
  `["a","b","c",""]` (blank last line). 2/3 (DeepSeek+Gemini) match the code;
  Sonnet argued trimming is more ergonomic. **Decision owed:** keep trailing
  empties (fidelity) or trim (ergonomics)? Whatever we pick, document + test.
- **SP5 — NAMESAKE COLLISION.** `split` is a string-splitter, not GNU's
  file-chunker; `-l` doesn't exist → clap rejects it, exit 2. **3/3 unanimous it
  MUST be loud.** CODE: confirm it errors (not silent no-op / mis-split). The
  whole-tool semantic mismatch is the surprise → DOC prominently.

## jq — contract-first (jaq subset)

| ID | program | expected | route |
|----|---------|----------|-------|
| J1 | `… \| jq '.a'` | `1` | — (3/3) |
| J2 | `… \| jq -r '.name'` | `kaish` (no quotes) | — (3/3) |
| J3 | `… \| jq '.[] \| select(. > 1)'` | `2`⏎`3` | — (3/3) |
| J4 | `… \| jq -c '.'` | `{"a":1}` | — (3/3) |
| J5 | `… \| jq 'env.HOME'` | **loud parse/compile error, rc≠0** | CODE+DOC |

- **J5 is the sharpest jq cell.** Contract promises a *loud* error for an
  unimplemented/hermetic feature. **But real jq returns `null` silently for a
  missing env var** (Sonnet flagged this) — so if jaq silently returns `null`
  here instead of erroring, that's a **P1 SILENT divergence from kaish's own
  contract.** Phase 2 MUST verify `env` is loud, not silent-null. DOC the
  hermetic boundary (no `env`, no clock) in the jq fragment.
