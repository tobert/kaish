# Builtin sweep — line-slicers battery (head / tail / tac / uniq / wc)

Fixed cells for the 3-vendor model-consensus panel. Identical inputs for every
vendor; panel answers from model memory only (no kaish, no real binary).
Temporary; delete with the sweep.

## inputs
- **L4** (4 lines + trailing `\n`): `apple` `banana` `cherry` `date`
- **LU** (uniq, adjacency matters, + `\n`): `a` `a` `b` `a` `c` `c`
- **LW** (+ `\n`): `hello world` then `foo`
- **LN** (**no** trailing newline): `a` then `b` (bytes `a\nb`)

| ID | Input | Task |
|----|-------|------|
| H1 | L4 | First 2 lines. |
| H2 | L4 | Every line except the last. |
| H3 | L4 | First 5 bytes. |
| TL1 | L4 | Last 2 lines. |
| TL2 | L4 | From line 2 through the end. |
| TL3 | L4 | Last 5 bytes. |
| TC1 | L4 | Reverse the line order. |
| U1 | LU | Collapse adjacent duplicate lines. |
| U2 | LU | Collapse adjacent dupes, prefix each with its count. |
| U3 | LU | Print only lines that were repeated (adjacent). |
| W1 | LW | Count lines. |
| W2 | LW | Count words. |
| W3 | LW | Count bytes. |
| W4 | LW | Default output (lines, words, bytes together). |
| W5 | LN | Count lines of an input with no trailing newline. |

Probes: U2 count-prefix spacing, W4 column spacing, W5 newline-counting
semantics, H3/TL3 byte boundaries — all classic silent-divergence cells.
