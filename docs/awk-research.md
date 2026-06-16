# Researching kaish's awk: a fidelity-and-coverage study

*A research design for making kaish's `awk` builtin more **correct** and more
**ergonomic** for the language models that drive it — adapting the LLM-panel
method of [`designing-syntax-with-llms.md`](designing-syntax-with-llms.md) and
[`cross-model-eval.md`](cross-model-eval.md) to a language the models already
know.*

---

## Why awk is a different problem than arrays/hashes

The collection-syntax study answered a **novel-syntax** question: *can a model
reliably emit a syntax we just invented?* Divergence there meant "the spec has a
hole the model filled from its bash priors." The fix was almost always a spec/doc
change, and the grading was eyeball-per-cell.

awk inverts almost every premise:

- **The syntax is not novel.** POSIX awk (and the gawk dialect) is in every
  model's training set. Models emit fluent, *correct* awk on the first try, across
  every tier. There is little emission risk to measure.
- **The risk moved from emission to fidelity.** The question is no longer "can the
  model write it" but **"when the model writes correct awk, does kaish do what that
  awk means?"** A divergence is now usually a *kaish bug or missing feature*, not a
  spec hole.
- **There is a ground-truth oracle.** Real awk exists. `gawk 5.4.0` is installed in
  the dev environment. We can diff kaish's output against a reference implementation
  *mechanically*, instead of grading by hand.
- **kaish is deliberately a subset** (the 80% rule). So some divergence is *correct
  by design* — the research has to separate "bug" from "out-of-scope boundary,"
  and ensure every boundary **fails loud with a hint** rather than silently doing
  the wrong thing.

The methodology's deepest finding still governs, though, and points the lever:

> Completeness of how you teach it beats model strength. Even the flagship
> falls back to priors exactly where the docs go quiet.

awk currently has **zero** dedicated help content (`crates/kaish-help/content/en/`
has no awk fragment) and a single line in `docs/LANGUAGE.md`. That silence is the
largest single lever in this study.

---

## The motivating finding (already in hand)

A five-minute smoke test surfaced the exact hazard the method is built to catch —
a **silent data-corruption divergence**, live in the shipping builtin:

```
$ kaish: echo "a,b,c" | awk '{n=split($0,parts,","); print n, parts[1], parts[2]}'
3
```

`n` is correctly `3`, but `parts[1]`/`parts[2]` are **empty**: `split()` returns
the count and silently discards the array (`awk.rs` `call_function` → `"split"`).
Reading the source, the same class affects:

- **`sub()` / `gsub()`** — `return 0`, never substitute (`awk.rs:2194`). The most
  common awk text edit is a silent no-op.
- **strnum comparison** — `looks_numeric` (`awk.rs:2072`) treats *string literals*
  as numeric, so `"10" < "9"` compares numerically (real awk compares string
  literals lexically → true). A fidelity bug in the comparison core.

These are not ergonomic nits; under the project's "fail loud, not silent" standard
they are `latch`-grade. They motivate making **differential correctness** the
primary axis, not a footnote.

---

## Two axes

### Axis 1 — Differential correctness sweep (primary)

A reference oracle changes the loop. Instead of "grade divergences by eye," we run
a corpus of awk programs through **both** `kaish awk` and `gawk` (and, where it
matters, a POSIX-strict second oracle), and **diff stdout + exit code**. Every
diff is a finding; the diff *is* the grade.

**Corpus.** A few hundred small, self-contained programs over fixed input
fixtures, organized by feature so a failure localizes:

| Group | Probes |
|---|---|
| Fields & records | `$0`,`$1..$NF`, `NF`/`NR`, field assignment rebuilds `$0`, `$(expr)` |
| Separators | default whitespace, `-F:`, regex FS, multi-char FS, `OFS`/`ORS`, `RS` (single/regex/paragraph) |
| Patterns | `/re/`, expr patterns, range patterns `/a/,/b/`, `!`, compound `&&`/`||` |
| Arithmetic & strnum | numeric coercion, `"10"<"9"`, uninitialized-as-0/"", `%`/`^`, `int` |
| String builtins | `length` (and **bare** `length`), `substr`, `index`, `tolower`/`toupper`, `sprintf` |
| **Mutating builtins** | **`split` populates array**, **`sub`/`gsub` substitute + return count**, `&` backref, `match` sets `RSTART`/`RLENGTH` |
| Control flow | `if/else`, `while`, C-`for`, `for-in`, `break`/`continue`/`next`/`exit`, ternary |
| Arrays | assoc set/get, `in`, `delete`, `delete arr`, multi-subscript `SUBSEP` |
| Output | `print`/`printf` format spectrum, `%c`/`%d`/`%s`/`%g`/width/precision |
| Special vars | `FS/OFS/RS/ORS/NR/NF/FILENAME/SUBSEP/RSTART/RLENGTH/CONVFMT/OFMT/ENVIRON/ARGV/ARGC` |
| User functions | `function f(a){…}`, recursion, locals, `return` |
| I/O extensions | `getline`, redirection `print > f`, pipe `print | "cmd"` (expected out-of-scope — assert *loud*) |

**Auto-classification.** Each divergence lands in exactly one bucket:

1. **FIX** — kaish claims support and gets it wrong (split/gsub/strnum). A bug.
2. **ADD** — a feature kaish lacks that the coverage axis proves agents reach for.
3. **TEACH/BOUNDARY** — out-of-scope by the 80% rule. Requirement: kaish **errors
   loudly with a hint** ("awk: getline not supported; read the file with `cat`
   and pipe it in"), never silently no-ops or mis-parses. A *silent* divergence
   here is itself a FIX.

The corpus doubles as a **regression suite**: each program becomes an rstest case
asserting kaish-vs-gawk parity (for in-scope features) or a specific loud error
(for boundaries). Differential tests that *can and will fail* when we regress —
the TDD standard.

> **Oracle caveat.** gawk has gawk-isms (`gensub`, `BEGINFILE`, true regex RS,
> `\<` word boundaries) beyond POSIX. Where kaish intentionally tracks POSIX or its
> own ERE (the regex crate ≠ gawk's engine), the "reference" must be the documented
> kaish contract, not gawk's superset. Tag each corpus row with its authority
> (POSIX / gawk-extension / kaish-specific) so a diff against a gawk-ism isn't
> miscounted as a bug. Regex-dialect differences especially: kaish uses the `regex`
> crate (no backreferences, different escapes) — that's a documented boundary, not
> a defect.

### Axis 2 — Cross-model emission & coverage panel (secondary)

The panel method from `cross-model-eval.md`, repurposed. We are *not* validating a
syntax we chose — awk's syntax is fixed. We are measuring **two** things:

1. **Coverage / the 80% line.** Given realistic, awk-tempting data-wrangling tasks
   stated in prose ("sum column 3 grouped by column 1"; "print lines where field 2
   matches /err/ and renumber them"; "replace the 3rd comma-field and re-emit"),
   capture *which awk features each tier reaches for*. Features the whole panel
   uses unprompted are inside the agent-relevant 80% → must work (Axis-1 FIX) or
   fail loud well. Features no one reaches for are safe boundaries to document.
   This is how we decide **ADD vs. TEACH** with evidence, not taste.
2. **Recovery / error ergonomics.** When kaish loud-errors on a boundary
   (getline, a gawk-ism, a bad FS), does the *error message* steer the model to a
   working rewrite? A/B competing phrasings on identical tasks and grade whether
   the model's *next* attempt succeeds. (Mirrors the `has`→`[[ in ]]` lesson:
   the win was re-spelling so the model's instinct composes.)

**Panel.** Full tiered, per `cross-model-eval.md`: cross-vendor and deliberately
down into the fast/cheap tail, because that tail is where coverage assumptions
break. Via kaibo casts:

| Tier | Models (casts) |
|---|---|
| flagship | DeepSeek-Pro, Gemini-Pro, Claude (Opus/Sonnet) |
| fast | DeepSeek-Flash, Gemini-Flash-Lite, Claude Haiku |
| local/quantized | whatever loads (Gemma, GLM, Qwen) — coverage as VRAM allows |

**Conditions** (the `#5` scaffolding-variation discipline, adapted): for the
coverage axis, *no cheat-sheet* — we want the model's untaught awk instinct, since
that is precisely what hits kaish in production. For the error-ergonomics axis,
feed the real kaish error string and measure the next turn.

---

## The loop

1. Run the differential sweep → mechanical list of divergences.
2. Classify each FIX / ADD / TEACH (oracle + corpus authority tag).
3. Run the panel → which divergences are inside the agent-80% (raises priority),
   which are safe boundaries (downgrades to TEACH).
4. **Synthesize into three workstreams:**
   - **FIX**: split-populates-array, sub/gsub-substitute, strnum comparison, any
     parity bug in an in-scope feature.
   - **ADD**: features the panel reaches for that we lack (candidates from the
     source read: bare `length`, user `function`s, `match` setting RSTART/RLENGTH,
     `printf` redirection — pending panel evidence).
   - **TEACH**: write the missing `kaish-help` awk fragment + LANGUAGE.md section;
     convert every unsupported feature from silent/garbled to a loud, hint-bearing
     error; document the regex-dialect and POSIX-subset boundaries explicitly.
5. Re-test the changed forms against the oracle before commit (`#9`: the things
   adopted late were never validated in their final shape).

Each FIX ships with its differential test. Each TEACH boundary ships with a test
asserting the loud error. ADDs are scoped against parser/runtime cost the same way
the collection study weighed ergonomics against the lexer (`#7`).

---

## What this design deliberately does *not* do

- It does not try to make kaish awk a gawk clone. The 80% rule stands; the output
  is a *defensible, documented, loud-at-the-edges* subset.
- The oracle measures behavioral parity, not internals — a kaish that gets the
  right answer a different way is a pass.
- The panel measures generation and recovery, not long-horizon debugging
  (same blind spot called out in the methodology doc).
- Regex-engine differences (kaish `regex` crate vs gawk) are a boundary to
  *document*, not a parity bug to chase.

---

## Pilot scope (this round)

Before committing to the full corpus, a pilot validates the harness and confirms
the headline divergences:

- **Differential pilot**: ~30-40 programs across the highest-risk groups (mutating
  builtins, strnum, separators, special vars) through kaish + gawk; classify.
- **Panel pilot**: the full tiered panel on a handful of realistic wrangling tasks
  (no cheat-sheet) to read the coverage line and confirm split/sub/gsub are inside
  the 80%.

Findings from the pilot append below.

---

## Pilot findings

Ran 2026-06-16. Differential pilot: 38 programs through `kaish awk` vs `gawk 5.4.0`
(21 MATCH, 10 DIFF, 7 KAISH-ERR). Emission panel: 8 realistic wrangling tasks, no
cheat-sheet, across Anthropic (Sonnet 4.6), DeepSeek-V4-Pro, Gemini-3.5-Flash.
Both harnesses validated; the method works. (Local/quantized tier deferred to the
full run — the cloud unanimity below was already conclusive for the pilot.)

### Headline: the panel and the oracle point at the same two features

The emission panel was **unanimous across all three vendors** on the canonical
idioms — and two of the most-reached-for are exactly the ones the differential
sweep found broken:

- All 3 models wrote **`awk -F: '{print $1}'`** for colon-separated fields
  (passwd, tags) → **kaish rejects this at the shell-parse layer** (below).
- All 3 wrote **`gsub(/foo/,"bar")`** for substitution → **kaish silently
  no-ops** it.

That is the coverage line settled with evidence: `-F:` and `gsub` are inside the
agent-relevant 80%, not the tail. They are top-priority FIXes.

### FIX — claims support, silently wrong (data-corruption class)

| Finding | Evidence | Notes |
|---|---|---|
| **`-F:` / `-F;` / `-F'\t'` rejected by the *kaish lexer*** | `awk -F: '…'` → kaish `parse error … found ':'`. `-F,` and `-F.` work; `-F ':'` (quoted, separate) works. `echo a:b:c` works. | **Not in awk.rs.** kaish's tokenizer emits `:`/`;`/`\|`/tab as operator tokens immediately after the `-F` flag word, so they don't glue into the `-F:` argument. The single most common awk invocation. Highest impact. |
| **`split()` never populates the array** | `split($0,a," ")` returns 3 but `a[1]`/`a[2]` empty | `awk.rs` `"split"` arm returns count only |
| **`sub()`/`gsub()` never substitute** | `gsub(/o/,"0")` returns 0, text unchanged | `awk.rs:2194` returns 0 |
| **`match()` never sets `RSTART`/`RLENGTH`** | `match("foobar",/bar/)` then `print RSTART,RLENGTH` → empty | both special vars unset |
| **strnum: string literals compared numerically** | `"10" < "9"` → kaish "num", gawk "lex" | `looks_numeric` (`awk.rs:2072`) applies to literals; POSIX limits strnum to fields/input |
| **`print $1 > "file"` parsed as comparison** | prints `1` (boolean of `$1 > "file"`) instead of redirecting | silent mis-parse; redirection is out-of-scope but must be a *loud* error, not a comparison |

### ADD — candidate features the corpus/panel justify

| Finding | Evidence |
|---|---|
| **Range patterns `/a/,/b/`** | `awk '/bob/,/carol/'` → `unexpected token: Comma`. POSIX core. |
| **Bare `length`** | `{print length}` → empty (parsed as a var); gawk prints `length($0)` |
| **User functions** `function f(x){…}` | `function sq(x){…}` → `unexpected token: Function`; keyword lexed, never parsed |
| **Numeric builtins** `int`/`sqrt`/`sin`/… | `int(3.9)` → `not implemented (use dedicated tool)`; `int`/`sqrt` are arguably in-80% |
| **Multi-subscript arrays** `a[i,j]` / `SUBSEP` | `a[1,2]` → `expected RBracket, found Comma` |

### TEACH — out-of-scope boundaries (make loud + document)

- `getline`, `print | "cmd"`, redirection — currently parse-error with unhelpful
  messages (`expected || but found single |`). Should be a hint-bearing awk error.
- `ENVIRON`/`ARGV`/`ARGC` — kaish is hermetic; `ENVIRON` semantics differ by
  design. Document the boundary.
- Regex dialect: kaish uses the `regex` crate (no backreferences, different
  escapes) vs gawk's engine — a documented boundary, not a parity bug.

### Re-grade of the method for awk

The differential oracle did the heavy lifting — 17 of 38 programs flagged
mechanically, no eyeballing. The panel's value was **prioritization**: it proved
`-F:` and `gsub` are unanimous reflexes, lifting them above the rest. The two axes
agreed, which (per `#7` in the methodology) is the signal you can act on with both
hands. The `-F:` lexer finding is the standout — invisible to a pure awk-internals
review, surfaced only because the differential harness drove through the *real*
kaish shell front-end.

### Recommended next steps (out of scope for this research doc; → `docs/issues.md`)

1. **P1 FIX**: `-F:` lexer interaction, `split` populate, `sub`/`gsub` substitute —
   the three unanimous-80% breakages. Each lands with its differential test.
2. **P2 FIX**: `match` sets `RSTART`/`RLENGTH`; strnum literal-vs-field; redirection
   parsed-as-comparison → loud error.
3. **P2 ADD**: range patterns, bare `length`, `int`/`sqrt`.
4. **P3 ADD/TEACH**: user functions, multi-subscript/`SUBSEP`, getline/pipe loud
   errors, awk help fragment + LANGUAGE.md section.
5. Promote the pilot corpus to the full ~few-hundred-program differential suite and
   run the local/quantized panel tier.
