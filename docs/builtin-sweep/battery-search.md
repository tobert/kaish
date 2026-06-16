# Builtin sweep — search battery (grep / rg)

Fixed cells for the 3-vendor model-consensus panel. The regex-dialect cluster:
kaish is ERE-always (the `regex` crate; no backrefs) per the sed/awk work, so
the BRE-idiom and exit-code cells are the sharpest silent/loud probes here.
Temporary; delete with the sweep.

## inputs
- **SG** (4 lines + trailing `\n`): `foo` `Foobar` `baz` `foofoo`
- **SB** (2 lines + trailing `\n`): `aa` `a`

| ID | Input | Task |
|----|-------|------|
| G1 | SG | Lines containing `foo`. |
| G2 | SG | Lines NOT containing `foo`. |
| G3 | SG | Lines containing `foo`, case-insensitive. |
| G4 | SG | Count of matching lines for `foo`. |
| G5 | SG | Only the matched text `foo`, one per match. |
| G6 | SG | Matching lines for `foo`, prefixed with line number. |
| G7 | SG | Lines matching `baz` OR `Foo` (alternation). |
| G8 | SG | Search for `zzz` (no match) — what is printed and the exit code? |
| G9 | SB | Lines containing two consecutive `a`s, via a brace interval quantifier. |
| R1 | SG | (ripgrep) Lines containing `foo`. |
| R2 | SG | (ripgrep) Lines containing `foo`, case-insensitive. |
| R3 | SG | (ripgrep) Only the matched text `foo`. |
| R4 | SG | (ripgrep) Count of matches for `foo`. |
| R5 | SG | (ripgrep) Matching lines with line numbers. |
| R6 | SG | (ripgrep) Search for `zzz` (no match) — printed output and exit code? |

Probes: G8/R6 no-match **exit code** (agents rely on `if grep -q`), G7 ERE
alternation, G9 BRE-vs-ERE interval dialect, G5/R3 only-matching format, R1/R5
ripgrep's stdin defaults (line numbers? color?) vs grep.
