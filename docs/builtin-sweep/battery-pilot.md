# Builtin sweep — pilot battery (cut / tr / sort)

Fixed cells for the model-consensus panel. Every vendor (Sonnet, DeepSeek,
Gemini) predicts on **identical** inputs/tasks so consensus is measurable.
The panel never sees kaish or any real binary — it answers from model memory
only. Temporary; delete with the sweep.

The exact prompt sent to each vendor is below the battery.

## cut — inputs

- **A** (one line + trailing `\n`): `root:x:0:0:root:/root:/bin/bash`
- **B** (CSV, trailing `\n`):
  ```
  alice,30,nyc
  bob,25,la
  ```
- **C** (one line + `\n`): `hello world`
- **D** (one line + `\n`, no delimiter present): `nodelim`

| ID | Input | Task |
|----|-------|------|
| C1 | A | Print the 7th colon-separated field (login shell). |
| C2 | A | Print fields 1 and 3 (the username and uid). |
| C3 | B | Print the 3rd comma-separated field (city) of every line. |
| C4 | B | Print fields 2 through end of every line. |
| C5 | C | Print characters 1–5. |
| C6 | D | Cut comma-field 2. What is printed for a line with no delimiter? |
| C7 | B | Print fields 1 and 3, joined by `|` instead of `,`. |

## tr — inputs

- **A**: `Hello World` · **B**: `a   b    c` (multiple spaces) · **C**: `a b c`
- **D**: `abc123def456` · **E**: `id=42` · **F**: `a-b_c!d`

| ID | Input | Task |
|----|-------|------|
| T1 | A | Uppercase every letter. |
| T2 | A | Delete every lowercase letter. |
| T3 | B | Squeeze each run of repeated spaces to one. |
| T4 | C | Replace spaces with underscores. |
| T5 | D | Delete everything that is NOT a digit. |
| T6 | E | Translate digits to `#` using a character class. |
| T7 | F | Keep only alphanumerics (delete the complement of `[:alnum:]`). |

## sort — inputs

- **A** (lines): `10` `2` `1` `20`
- **B** (CSV): `charlie,3` `alice,1` `bob,2`
- **C** (lines): `apple` `apple` `banana`
- **D** (lines): `v1.10` `v1.2` `v1.9`

| ID | Input | Task |
|----|-------|------|
| A1 | A | Default sort. |
| A2 | A | Numeric sort. |
| A3 | A | Reverse numeric sort. |
| A4 | B | Sort by the 2nd comma-field, numerically. |
| A5 | B | Sort by the 1st field (default/alphabetical). |
| A6 | C | Sorted, with duplicates removed. |
| A7 | D | Version sort. |

---

## Panel prompt (sent verbatim to each vendor)

> You are an expert at the standard Unix command-line text tools `cut`, `tr`,
> and `sort`. Answer entirely from your own knowledge — **do not run any
> commands or use any tools**; predict from memory, the way you would when
> writing a one-liner for a colleague.
>
> For each task below you are given a fixed input and an intent. Produce, per
> task:
> - **cmd** — the exact command you would reflexively write (assume input
>   arrives on stdin via a pipe).
> - **out** — the exact stdout you predict, rendered in a fenced code block.
>   Show every line. If a trailing newline is present say so; if you are
>   genuinely unsure of the exact bytes, say what you are unsure about rather
>   than guessing confidently.
> - **note** — one line: any ambiguity, dialect split (e.g. GNU vs BSD), or
>   place a reasonable person might expect different behavior.
>
> Keep it tight and scannable. Use the task IDs (C1, T1, A1, …).
>
> [battery inputs + task tables as above]
