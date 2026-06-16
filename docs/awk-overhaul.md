# awk Overhaul — burn-down punch list

**Scope:** make kaish's `awk` builtin correct and ergonomic for the agents that
drive it. This is a **temporary, burn-it-down-before-the-next-release** file
(issues.md-style), not a permanent doc. When every box here is checked, fold any
lasting notes into `CHANGELOG.md` + the awk help fragment and **delete this file**.

**Research backing:** [`awk-research.md`](awk-research.md) (design + pilot, 2026-06-16).
Findings below come from the pilot: a differential sweep vs `gawk 5.4.0` and an
emission panel (Anthropic Sonnet 4.6 / DeepSeek-V4-Pro / Gemini-3.5-Flash, all
unanimous).

**Working method (hold the line):**
- **Differential oracle.** `gawk` is installed. Every FIX lands with an rstest case
  asserting kaish-vs-gawk output parity for that program (route through
  `kernel.execute(...)`, not the builtin's `.execute()` — per CLAUDE.md).
- **Every boundary fails loud.** A feature we choose *not* to support must produce a
  hint-bearing awk error, never a silent no-op or a mis-parse. A silent divergence
  is itself a P1 bug.
- **TDD.** Write the failing differential test first; it must be able to fail.
- Tag fixes with the commit that closed them. `cargo test --all` +
  `cargo clippy --all --all-targets` clean before each commit.

> **STATUS: COMPLETE (2026-06-16)** — all items below landed on branch
> `feat/awk-overhaul`, each gawk-verified and DeepSeek-reviewed. Commits:
> `ae0357b`+`5faae15` (-F:), `31d1c70` (split/sub/gsub), `6e0ba5b`
> (match/length/int/sqrt/OFMT), `23759d6` (range patterns + loud boundaries),
> `7d36647` (docs/schema). **One found-and-fixed bonus bug:** a regex pattern
> starting a new rule after `}`/`;`/newline (`/a/{...} /b/{...}`) was mis-lexed
> as division — fixed in `23759d6`. **Deferred minors (out of scope, noted):**
> (1) `compare_values` treats `NaN` as equal (pre-existing; only reachable via
> `sqrt(-1)`); (2) `"cmd" | getline` hits the awk lexer's lone-`|` error before
> the getline hint; (3) `awk.md` topic-wiring waits for composable-help Phase 4
> (rg.md/timeout.md are unwired too). Once a release ships, **delete this file.**

Priorities: **P1** unanimous-80% breakage · **P2** fidelity bugs · **P3** coverage
adds · **P4** teach/document boundaries.

**80/20 boundary (decided 2026-06-16).** A respectable agent-facing awk is a
*crisp, loud, documented subset* — not a half-built gawk. Features no realistic
text-wrangling task reaches for, or whose cost dwarfs their value, are **declared
unsupported forever** (see the bottom section), not left half-done. Each must fail
loud with a hint and be documented in the help fragment. The pilot panel
(8 realistic tasks, 3 vendors) is the evidence for what's in vs. out.

---

## P1 — Unanimous-80% breakages (data-corruption / universal idiom)

All three panel models reached for these unprompted; all three are broken.

### [x] 1. `-F:` rejected by the kaish lexer (NOT awk.rs)
- **Symptom:** `awk -F: '{print $1}' /etc/passwd` → kaish shell parse error
  `found ':' expected …`. The single most common awk invocation.
- **Blast radius:** fails for `-F:`, `-F;`, `-F'\t'`, `-F|`; *works* for `-F,`,
  `-F.` (bareword-legal chars) and for the quoted/split form `-F ':'`. Plain
  `echo a:b:c` works — so `:` is fine mid-bareword; the bug is specific to a
  metachar glued onto a flag word.
- **Contributing factor:** kaish's tokenizer (logos lexer in `kaish-kernel`, not the
  awk builtin) emits `:` / `;` / `|` / tab as operator tokens immediately after the
  `-F` flag-like word, so they never coalesce into the `-F:` argument. Find where
  short-flag/option words are lexed; an argument token of the form `-X<metachar…>`
  should lex as one word.
- **Fix:** lex `-F:` (and `--field-separator=:`) as a single argument word.
  Cross-check against the existing bareword rules ([[lang_argv_barewords]]:
  `NumberIdent`/`DottedIdent`) — this is the same class of "glue metachars into the
  word" decision.
- **Test:** differential `awk -F: '{print $1}'` over a passwd fixture == gawk; plus
  `-F;`, `-F'\t'`. Lexer unit test that `-F:` is one token.

### [x] 2. `split()` never populates the array
- **Symptom:** `{n=split($0,a," "); print n, a[1], a[2]}` → kaish `3  ` (count right,
  array empty); gawk `3 alice 25`.
- **Contributing factor:** `awk.rs` `call_function` `"split"` arm computes the parts
  then returns only the count — the comment even says it "would need mutable access."
  It has `&mut self`; populate `self.arrays`.
- **Fix:** clear and fill `self.arrays[name]` with 1-indexed string keys (`"1"`,
  `"2"`, …) from the split parts; return the count. Honor FS-style separator rules
  (single-char vs regex vs default-whitespace) already used by `split_record`.
- **Test:** differential `split($0,a,","); print a[2]` and the whitespace/regex
  variants == gawk.

### [x] 3. `sub()` / `gsub()` never substitute
- **Symptom:** `{n=gsub(/o/,"0"); print n,$0}` → kaish `0 foo BAR baz` (no-op);
  gawk `2 f00 BAR baz`. `sub` likewise.
- **Contributing factor:** `awk.rs:~2194` `"sub" | "gsub"` arm returns `0` with a
  comment "need mutable access."
- **Fix:** implement against the target lvalue (default `$0`; optional 3rd arg is an
  lvalue — var/field/array elem). `sub` = first match, `gsub` = all; return the
  replacement count; rebuild `$0`/fields when the target is a field. Support the `&`
  backreference in the replacement (literal `\&` escapes it) — panel/gawk expect it.
  Mind the `regex` crate dialect (no backrefs in the *pattern*; that's a P4 boundary).
- **Test:** differential `sub`, `gsub`, `gsub` with `&`, and `gsub` into a named
  field/variable == gawk; assert returned count.

---

## P2 — Fidelity bugs worth fixing

### [x] 4. `match()` doesn't set `RSTART` / `RLENGTH`
- **Symptom:** `if(match("foobar",/bar/)) print RSTART, RLENGTH` → kaish empty;
  gawk `4 3`.
- **Why in-scope:** `match()` is *only* worth calling for the position — a plain
  boolean is what `~` is for. Without these vars the builtin is pointless, and the
  `match`+`substr` extraction idiom is real. Cheap to fix.
- **Fix:** on `match`, set `RSTART` (1-based start, 0 on no match) and `RLENGTH`
  (match length, -1 on no match) in `self.vars`.
- **Test:** differential match + RSTART/RLENGTH, incl. no-match (`0`, `-1`).

### [x] 5. `print $1 > "file"` silently parsed as comparison
- **Symptom:** `{print $1 > "/dev/stderr"}` → kaish prints `1` per line (evaluates
  `$1 > "/dev/stderr"` as a boolean); gawk redirects.
- **Decision:** output redirection is **unsupported forever** (see bottom). But
  parsing it as a comparison is a *silent* divergence — the dangerous kind — so this
  is a P2 safety-floor fix even though no model emits it.
- **Fix:** in `print`/`printf` parsing, recognize a trailing `>`/`>>`/`|` and emit
  the hint-bearing "unsupported" error from the boundary section. Do not treat it as
  a relational operator inside a print arg.
- **Test:** `{print $1 > "f"}` exits non-zero with the hint (not `1`s).

---

## P3 — Coverage adds (cheap, idiomatic, panel/POSIX-justified)

### [x] 6. Range patterns `/a/,/b/`
- **Symptom:** `awk '/bob/,/carol/'` → `unexpected token: Comma`. POSIX core,
  parallels sed's address ranges (already supported).
- **Fix:** parse `pattern , pattern` as a range pattern with per-rule active-state;
  fires inclusively from the line matching the first to the line matching the second.
- **Test:** differential range over a fixture == gawk, incl. re-trigger.

### [x] 7. Bare `length` (no parens)
- **Symptom:** `{print length}` → kaish empty (parsed as a variable); gawk =
  `length($0)`.
- **Fix:** treat bare `length` as `length($0)`.
- **Test:** differential `{print length}` == gawk.

### [x] 8. Numeric builtins `int` and `sqrt` (only)
- **Symptom:** `int(3.9)` → `not implemented (use dedicated tool)` (and the message
  is wrong — there is no dedicated tool). `int`/`sqrt` do show up in data work.
- **Fix:** implement `int` (truncate toward zero) and `sqrt`. The rest of the math
  set (`sin`/`cos`/`atan2`/`exp`/`log`/`rand`/`srand`) is **unsupported forever**
  (see bottom) — agents wrangling text never reach for trig, and `rand` fights the
  hermetic stance.
- **Test:** differential `int(3.9)`, `int(-3.9)`, `sqrt(2)` == gawk.

---

## P4 — Teach: loud boundaries + the docs

### [x] 9. Unsupported-forever constructs → loud, hinted errors
Every item in the **Unsupported forever** section below must fail with a clear
awk-level error that names the construct and the kaish alternative — never a silent
no-op, a mis-parse, or a misleading message. Audit each:
- `getline`, `print | "cmd"`, output redirection (`>`/`>>`): currently
  parse-error with unhelpful text (`expected || but found single |`) or the #5
  comparison mis-parse. Replace with e.g. *"awk: getline is not supported in kaish;
  pipe input in (`cat f | awk …`)"* / *"awk: output redirection is not supported;
  pipe kaish's output downstream instead."*
- user functions (`function`): currently `unexpected token: Function`. Make it
  *"awk: user-defined functions are not supported (kaish awk is a one-liner
  subset)."*
- multi-subscript `a[i,j]` / `SUBSEP`: currently `expected RBracket, found Comma`.
  Name it.
- math set `sin/cos/atan2/exp/log/rand/srand`: fix the message (drop "use dedicated
  tool").
- `ENVIRON`/`ARGV`/`ARGC`: currently a silently-empty array — document the hermetic
  reason; if a loud signal is feasible on access, prefer it.

### [x] 10. Write the awk help content (the biggest teaching lever)
awk has **zero** dedicated `kaish-help` content and one line in `docs/LANGUAGE.md`.
Add an awk fragment to `crates/kaish-help/content/en/` (and the fragment registry if
applicable) and an `docs/LANGUAGE.md` section covering:
- the **supported subset** (fields/records, patterns + ranges, `-F` forms,
  `print`/`printf`, arrays + `in`/`delete`/`for-in`, control flow, the string
  builtins, `split`/`sub`/`gsub`/`match`+RSTART/RLENGTH, `int`/`sqrt`);
- the **`-F` field-separator forms** that work, and the substitution/split semantics;
- the **Unsupported forever** list verbatim, each with its kaish alternative;
- the **regex dialect** boundary (kaish uses the `regex` crate: no backreferences,
  different escape set, vs gawk's engine — document, don't chase parity).
Per the research's dominant finding, completeness of teaching beats everything.

---

## Unsupported forever (80/20 boundary)

Declared out of scope by the 80/20 rule on 2026-06-16. **Do not implement** during
the burn-down; instead make each loud (P4 #9) and documented (P4 #10). Listed so the
boundary is explicit and reviewable.

| Construct | Why kicked | Required behavior |
|---|---|---|
| **strnum on string literals** (`"10" < "9"` compares numerically) | Common case (field-vs-value) already correct; broken case is contrived; fix needs an `AwkValue` type-tag touching the comparison core — cost ≫ value. | Documented known divergence. (Can't loud-error a comparison; the help fragment notes it. The field/strnum cases that agents actually use stay correct.) |
| **User-defined functions** (`function f(){…}`, `return`) | No panel model defined one in a one-liner; "awk programming," not shell wrangling. Largest single implementation cost (call-by-ref arrays, locals, recursion). | Loud error. |
| **Multi-subscript arrays** `a[i,j]` / `SUBSEP` | Niche; zero panel signal. | Loud error. |
| **Math set** `sin`/`cos`/`atan2`/`exp`/`log`/`rand`/`srand` | Text wrangling never needs trig; `rand`/`srand` fight the hermetic/deterministic stance. (`int`/`sqrt` are kept — P3 #8.) | Loud error, accurate message. |
| **`getline`** (all forms) | I/O sequencing belongs to the shell, not awk; kaish controls input. | Loud error; hint: pipe input in. |
| **Output redirection** `print > f`, `>>` | kaish owns the write surface (overlay/latch/trash); awk writing files bypasses it. | Loud error (P2 #5); hint: pipe output downstream. |
| **Pipes to/from commands** `print | "cmd"`, `"cmd" | getline` | Subprocess control is the shell's job, gated by capability features. | Loud error. |
| **`ENVIRON` / `ARGV` / `ARGC`** | kaish is hermetic (never reads OS env; frontends populate `initial_vars`); file-arg processing differs. | Documented; loud on access if feasible. |
| **Regex backreferences & gawk escape extensions** | kaish uses the `regex` crate, a different engine than gawk by design. | Documented dialect boundary. |

---

## Done log

*(move items here with commit SHAs as they land; delete the file when empty above)*
