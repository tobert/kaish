# Builtin sweep — passthrough/io battery (cat / tee)

Fixed cells for the 3-vendor panel. `cat` is universal; `tee` is a WRITER — its
write-model safety rides the same kaibo `consult` as `patch` (battery-compare.md).
Temporary; delete with the sweep.

## fixtures
- **A** = `apple`⏎`banana`⏎`cherry`⏎
- **C** = identical to A

| ID | program | question |
|----|---------|----------|
| CT1 | `cat A` | output? |
| CT2 | `cat -n A` | exact numbered-line format (spacing)? |
| CT3 | `cat A C` | output (concatenation)? |
| CT4 | `echo hi \| cat` | output (stdin passthrough)? |
| CT5 | `cat /dev/zero` | what SHOULD happen (infinite source)? loud or hang? |
| TE1 | `echo hi \| tee out.txt` | stdout output + what `out.txt` contains? |
| TE2 | `printf 'a\n' \| tee -a out.txt` (out.txt already has `x`) | file contents after? |

Probes: **CT2 `cat -n` field-width** (same padding-decision class as wc/uniq —
DOC); **CT5 `/dev/zero`** (kaish makes `cat /dev/zero` a loud error rather than
hang — `[[project_dev_fs]]`; confirm it's loud, not a hang/silent-truncate);
**TE* write model** (consult, with `patch`).
