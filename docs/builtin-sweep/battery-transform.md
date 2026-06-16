# Builtin sweep — generate/transform battery (printf / seq / split / jq)

Fixed cells for the 3-vendor panel. `printf`/`seq` are universal (predict from
memory). `split`/`jq` are **kaish-specific** — the panel is given kaish's
contract and asked to predict output AND flag agent-surprise (a GNU-namesake
reflex must fail loud, never silently misbehave). `format_string` is NOT a
builtin (shared printf/sprintf parser) — excluded. Temporary; delete with sweep.

## universal — predict from memory

inputs arrive via pipe; printf/seq take operands directly.

| ID | Task |
|----|------|
| P1 | `printf '%s\n' a b c` |
| P2 | `printf '%s-%s\n' a b c d` |
| P3 | `printf '%d-%d\n' 1 2 3` (odd operand count) |
| P4 | `printf '%05.2f\n' 3.14159` |
| P5 | `printf '%x\n' 255` |
| P6 | `printf 'a\tb\n'` |
| P7 | `printf '%s\n'` (no operands) |
| S1 | `seq 3` |
| S2 | `seq 2 5` |
| S3 | `seq 1 2 7` |
| S4 | `seq -w 8 10` |
| S5 | `seq 3 -1 1` |
| S6 | `seq -s, 1 3` |
| S7 | `seq 5 2` (start > end, no negative step) |

## kaish-specific — predict given the contract, flag surprise

**`split` contract:** splits a string into an array (the replacement for shell
word-splitting). `split "<str>" "<delim>"`; no delim → whitespace; `-r <regex>`
splits on a regex; `--limit=N` caps the number of pieces. Reads the string from
stdin when piped (then the first positional is the delimiter). When printed/
iterated, one element per line. Flags are **only** `-r`/`--regex`, `--limit`.

| ID | Input | Task / question |
|----|-------|-----------------|
| SP1 | `echo "a:b:c" \| split ":"` | predicted lines? |
| SP2 | `split "hello world"` | default whitespace split — lines? |
| SP3 | `split "a:b:c:d" ":" --limit=2` | lines? |
| SP4 | `split "a1b2c3" -r "[0-9]"` | lines? (mind a trailing empty piece) |
| SP5 | `split -l 100 access.log` | **GNU file-chunk reflex.** kaish has no `-l`. What SHOULD happen, and is silent misbehavior acceptable? |

**`jq` contract:** native JSON engine (a jq *subset*, jaq-dialect), filter is the
first positional; `-r` raw, `-c` compact, `-n` null-input. A filter using a
feature the engine lacks should emit a loud `jq parse/compile error`, never
silently return null/wrong. Hermetic (no `env`, no clock).

| ID | Input | Task / question |
|----|-------|-----------------|
| J1 | `echo '{"a":1,"b":2}' \| jq '.a'` | output? |
| J2 | `echo '{"name":"kaish"}' \| jq -r '.name'` | output? |
| J3 | `echo '[1,2,3]' \| jq '.[] \| select(. > 1)'` | output? |
| J4 | `echo '{"a":1}' \| jq -c '.'` | output? |
| J5 | `echo '"x"' \| jq 'env.HOME'` | does this feature exist; if not, loud or silent? |
