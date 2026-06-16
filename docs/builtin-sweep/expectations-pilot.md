# Builtin sweep — pilot expectations (cut / tr / sort)

Banked model-consensus expectations from the 3-vendor panel (Sonnet 4.6,
DeepSeek-V4-Pro, Gemini-3.5-Flash), 2026-06-16. **The reference is model
consensus, not a GNU run** (see `[[model-memory-over-gnu-oracle]]`). Phase 2
runs kaish against these via `harness.sh` once the tree is stable. Temporary;
delete with the sweep.

Legend: ⟨↵⟩ = output ends with a trailing newline · ⟨∅↵⟩ = **no** trailing
newline · **FLAG** = panel disagreed or a coverage decision is owed.

## cut

| ID | kaish program | stdin | expected stdout | nl | consensus |
|----|---------------|-------|-----------------|----|-----------|
| C1 | `cut -d: -f7` | `root:x:0:0:root:/root:/bin/bash` | `/bin/bash` | ↵ | 3/3 |
| C2 | `cut -d: -f1,3` | same | `root:0` | ↵ | 3/3 — output joined by **input** delim `:` |
| C3 | `cut -d, -f3` | `alice,30,nyc`⏎`bob,25,la` | `nyc`⏎`la` | ↵ | 3/3 |
| C4 | `cut -d, -f2-` | same | `30,nyc`⏎`25,la` | ↵ | 3/3 |
| C5 | `cut -c1-5` | `hello world` | `hello` | ↵ | 3/3 |
| C6 | `cut -d, -f2` | `nodelim` | `nodelim` | ↵ | **FLAG 2/3** |
| C7 | `cut -d, -f1,3 --output-delimiter='\|'` | csv | `alice\|nyc`⏎`bob\|la` | ↵ | **FLAG (cmd split)** |

- **C6 decision — pass-through whole line.** Gemini + Sonnet: a line with no
  delimiter prints whole (`nodelim`); DeepSeek: empty line. Consensus + muscle
  memory = pass-through (suppress only with `-s`). This is a **prime silent-
  divergence cell**: if kaish emits empty / errors / drops the line, that's a P1.
- **C7 decision — own `--output-delimiter`.** 2/3 reached for the GNU
  `--output-delimiter=` extension; DeepSeek used a `tr ',' '\|'` pipe fallback.
  Output agreed either way. Phase 2 checks whether kaish supports the flag; if
  not, that's a coverage gap (and the bare `cut -d, -f1,3` portable form must at
  least produce `alice,nyc` / `bob,la`).

## tr

| ID | kaish program | stdin | expected stdout | nl | consensus |
|----|---------------|-------|-----------------|----|-----------|
| T1 | `tr '[:lower:]' '[:upper:]'` | `Hello World` | `HELLO WORLD` | ↵ | 3/3 (Gemini used `a-z A-Z`) |
| T2 | `tr -d '[:lower:]'` | `Hello World` | `H W` | ↵ | 3/3 |
| T3 | `tr -s ' '` | `a   b    c` | `a b c` | ↵ | 2/3 (DeepSeek misread input) |
| T4 | `tr ' ' '_'` | `a b c` | `a_b_c` | ↵ | 3/3 |
| T5 | `tr -cd '[:digit:]'` | `abc123def456` | `123456` | **∅↵** | 3/3 |
| T6 | `tr '[:digit:]' '#'` | `id=42` | `id=##` | ↵ | 3/3 output (cmd split) |
| T7 | `tr -cd '[:alnum:]'` | `a-b_c!d` | `abcd` | **∅↵** | 3/3 |

- **T5 / T7 — trailing newline is deleted.** Unanimous: `-c` complements the
  set, so `\n` (not a digit/alnum) is deleted with everything else → output has
  **no** trailing newline. High-value fidelity cell; the harness sentinel fix
  exists specifically to catch a kaish that wrongly keeps the `\n`.
- **T6 — replacement set repeats its last char.** `[:digit:]` is 10 chars, `#`
  is 1; the single `#` fills all ten (POSIX last-char-repeat). Gemini wrote the
  explicit POSIX `[#*]`; output is `id=##` regardless. Check kaish does the
  repeat rather than erroring on length mismatch.

## sort — fully unanimous (3/3, every cell)

| ID | kaish program | stdin | expected stdout | nl |
|----|---------------|-------|-----------------|----|
| A1 | `sort` | `10`⏎`2`⏎`1`⏎`20` | `1`⏎`10`⏎`2`⏎`20` | ↵ |
| A2 | `sort -n` | same | `1`⏎`2`⏎`10`⏎`20` | ↵ |
| A3 | `sort -rn` | same | `20`⏎`10`⏎`2`⏎`1` | ↵ |
| A4 | `sort -t, -k2,2n` | `charlie,3`⏎`alice,1`⏎`bob,2` | `alice,1`⏎`bob,2`⏎`charlie,3` | ↵ |
| A5 | `sort -t, -k1,1` | same | `alice,1`⏎`bob,2`⏎`charlie,3` | ↵ |
| A6 | `sort -u` | `apple`⏎`apple`⏎`banana` | `apple`⏎`banana` | ↵ |
| A7 | `sort -V` | `v1.10`⏎`v1.2`⏎`v1.9` | `v1.2`⏎`v1.9`⏎`v1.10` | ↵ |

All three flag `-V` as a GNU (non-POSIX) extension — a coverage decision for
kaish, not a correctness one.

## Phase-2 watch list (what to classify when kaish runs)

1. **C6** pass-through of a non-delimited line — silent-divergence risk.
2. **T5 / T7** trailing-newline deletion under `-cd` — byte-exact (sentinel).
3. **T6** short-replacement last-char repeat.
4. **C7** `--output-delimiter` coverage.
5. **A7 / C7** GNU-extension coverage decisions (`-V`, `--output-delimiter`):
   if unsupported, must **fail loud**, never silently mis-handle.
