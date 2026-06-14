# Designing `date` from a survey of models

*kaish's `date` builtin was not designed against the `date(1)` man page. It was
designed against the **empirical** `date` — what language models actually type
when they reach for it cold, with no docs and no grounding. This document is the
full record: the experiment that produced the spec, the spec itself, the design
that fell out of it, and what finally shipped.*

It belongs to a small family of "design from what LLMs actually do" notes —
see [`designing-syntax-with-llms.md`](designing-syntax-with-llms.md) and
[`cross-model-eval.md`](cross-model-eval.md) for the same method applied
elsewhere in kaish.

## How we ran it — kaibo

The whole experiment ran through **kaibo** (解剖, "dissection") — a new agent MCP
server built *around* kaish. kaibo drives a **read-only kaish shell** to
investigate a codebase and answers with cited `file:line` evidence; it fields a
roster of model backends through named *casts* (Anthropic, DeepSeek, Gemini, and
local Gemma/GLM via an OpenAI-compatible endpoint). That made it the natural
instrument here, two ways:

- **The survey fleet** — kaibo's multi-backend roster let us send one cold prompt
  across nine different models without leaving the session.
- **Live probing** — kaibo's `run_kaish` tool ran the muscle-memory commands
  against the *actual* kaish 0.8.1 builtin, so the "what kaish does today" column
  below is observed behavior, not a guess.
- **Post-landing review** — after the rewrite, kaibo's `deepseek` cast
  (`deepseek-v4-pro`) reviewed the new code read-only and caught a real bug (see
  [Status](#status--as-built)).

The dog fooding is deliberate: kaish is a shell for agents, kaibo is an agent
built on that shell, and we used the agent-on-the-shell to design a shell builtin
for agents.

---

## The experiment

One prompt, sent cold — no docs, no man pages, no running anything. This is almost
verbatim the prompt @tobert provided to Opus at the start.

> Off the top of your head — no docs, no grounding — list a dozen `date` shell
> commands that come to mind right away. Your go-to invocations. End with one
> sentence on what your list reveals about your own habits.

**The flagship panel:** Opus 4.8 (also orchestrating), Sonnet 4.6, Haiku 4.5,
Gemini 3.1 Pro, DeepSeek V4-Pro. Five independent samples of "what `date` *is*"
in a model's weights. Then we went down-market and re-ran the *exact* prompt on
the lite tier: Gemini 3.1 flash-lite, Gemma 4 31B, Gemma 4 26B-A4B (the genuinely
small one — 26B MoE, ~4B active), DeepSeek V4-flash. Nine models total.

### The universal core — every single model

```bash
date            # what time is it
date -u         # UTC reflex
date +%s        # epoch seconds
```

These three are **invariant** across every model, flagship down to Haiku. ISO
output (`-I` / `+%Y-%m-%d` / `%F`) is a near-universal fourth. This is the
lizard-brain of `date`.

### Where the flagships diverge

| Example command | what it does | Opus | Haiku | Sonnet | Gemini | DeepSeek |
|---|---|:-:|:-:|:-:|:-:|:-:|
| `date -d @1700000000` | epoch → human-readable | ✓ | — | ✓ | ✓ | ✓ |
| `date +%s%N` | epoch with nanoseconds | ✓ | — | — | — | — |
| `date -r file.log` | a file's last-modified time | ✓ | — | — | ✓ | — |
| `date +%Y%m%d_%H%M%S` | sortable stamp for filenames | ✓ | — | ✓ | — | — |
| `date -d "next friday"` | natural-language date math | ✓ | — | — | ✓ | ✓ |
| `date +%A` / `date +%B` | weekday / month *name* | — | ✓ | — | — | ✓ |
| `date -Iseconds` | ISO 8601 down to the second | — | — | — | ✓ | ✓ |
| `date -R` | RFC 2822 (email/HTTP) | — | ✓ | — | — | ✓ |

A few personalities fall out:

- **Haiku** is the most cautious and least worldly — no epoch decoding, no nanos,
  no mtime. Its one tell: it listed `TZ=UTC date`, functionally redundant with
  the `date -u` it had *already* listed.
- **Sonnet** is the log-and-backup persona — filename stamps and `date↔epoch`
  round-trips dominate. Sharpest self-diagnosis of the GNU/BSD breakage.
- **Opus** had the widest spread — the only one to surface BSD `date -r file`
  *and* nanoseconds *and* `next friday`, and the only one whose self-note named
  the BSD command (`-v`) it *failed* to list.
- **Gemini** reads like an ops runbook: `-Iseconds`, `-r filename`,
  `next Friday 09:00`. Most "API timestamp" framed.
- **DeepSeek** is the only one leaning human/calendar — `%A`, `%B` for names —
  alongside the machine stuff, and its visible reasoning even *decided* to stick
  to GNU "because that's typical."

### The shared blind spot

Here's the kicker. Every model — unprompted — **confessed a GNU/Linux bias. And
not one corrected for it.** Not a single list *led* with BSD/macOS. The most-loved
trick across the fleet is GNU `-d` relative math (`date -d "2 weeks ago"`) —
which faceplants on a fresh macOS box, where `-d` means something else and date
math is `-v`. Five independently-sampled models, one shared muscle memory, one
shared place they'd all get bitten: a clean little demonstration of
**monoculture in the weights** — confident, convergent, and quietly
platform-wrong in exactly the same spot.

### Down-market: the monoculture *tightens*

| Example command | what it does | flash-lite | Gemma 26B-A4B | Gemma 31B | V4-flash |
|---|---|:-:|:-:|:-:|:-:|
| `date` · `date -u` · `date +%s` | the universal core | ✓ | ✓ | ✓ | ✓ |
| `date -d @1700000000` | epoch → human-readable | ✓ | ✓ | ✓ | ✓ |
| `date -d "2 weeks ago"` | natural-language date math | ✓ | ✓ | ✓ | ✓ |
| `date +%Y%m%d_%H%M%S` | sortable stamp for filenames | ✓ | ✓ | ✓ | ✓ |
| `date -I` / `--iso-8601=seconds` | ISO 8601 | ✓ | — | — | ✓ |
| `date +%A` / `date +%Z` | weekday / timezone *name* | ✓ | ✓ | — | — |
| `date -r file.log` | a file's last-modified time | — | — | ✓ | — |

Two things, both cutting against "smaller = worse":

**No quality cliff, and the monoculture gets *tighter* going down-market.**
Filename-stamping was 2/6 among flagships; it's **4/4** here. `-d @N` epoch decode
is 4/4. A ~4B-active Gemma produced a list indistinguishable in quality from
Gemini Pro. It's the well-trodden-task floor: capability tier barely predicts the
answer when the task is this common.

**The blind spot survives all the way down — one model caught red-handed.**
DeepSeek V4-flash's visible trace literally reads *"I'll focus on GNU date since
that's most common, but note BSD/macOS differences where they diverge"* — and
then, like every model before it, **it didn't.** It even nested
`date -d "$(date +%Y-%m-01) -1 day"` (last-day-of-previous-month, a real sysadmin
idiom): confident, fluent, and GNU-only. The monoculture isn't a flagship or a
small-model artifact. It's just *the weights*, top to bottom.

**Design consequence:** kaish gets to *choose* which `date` it is — and the
empirical answer is unambiguous. **9/9 models were GNU-shaped, the tiny ones
included.** There is zero measured operator demand for the BSD dialect, so kaish
picks GNU and says so. The audience matters: kaibo's own fast `explore` slot runs
a Gemma-class model — the exact class that reaches hardest for the forms that
were broken (below).

---

## From muscle memory to spec

We took the fleet's go-to commands and ran each against kaish 0.8.1 live (via
kaibo's `run_kaish`, 2026-06-14):

| Muscle-memory form | reached by | kaish 0.8.1 result | verdict |
|---|---|---|---|
| `date` | 6/6 | local `%Y-%m-%d %H:%M:%S` | ✅ works |
| `date -u` | 6/6 | UTC | ✅ works |
| `date +%s` | 6/6 | epoch (chrono `%s`) | ✅ works *by accident* |
| `date +%Y-%m-%d` / `+%F` | 5/6 | works via format path | ✅ works |
| `date -d "yesterday"` / `"2 weeks ago"` / `"next friday"` | 6/6 | clap reject, exit 2 | ❌ unsupported |
| `date -d "@1700000000"` (epoch → human) | 4/6 | **echoes `@1700000000`, exit 0** | ☠️ silent-wrong |
| `date -I` / `-Iseconds` | 3/6 | clap reject, exit 2 | ❌ unsupported |
| `date -R` (RFC 2822) | 3/6 | clap reject, exit 2 | ❌ unsupported |
| `date +%s%N` (nanos) | 2/6 | **panic → worker drop** | ☠️ crash |
| `date -r FILE` (mtime) | 2/6 | clap reject, exit 2 | ❌ unsupported |
| `TZ=zone date` | 3/6 | **ignores TZ, exit 0** | ☠️ silent-wrong |
| `--unix` / `--iso` / `--format` (what 0.8.1 advertised) | 0/6 | works | 🤷 nobody types these |

The forms every model types either **worked by accident** (`date +%s` only
because chrono happens to support `%s`) or **failed loudly**. The flags the old
builtin actually advertised were forms **no model ever typed**. And three
behaviors were worse than a clean failure.

### The three footguns

All three violated kaish's contract — *"fail loud, not silent"* and *"crashing is
preferred over data corruption."*

1. **`date +%s%N` panicked the worker.** chrono has no `%N` → `format()` yields
   `Item::Error` → `DelayedFormat`'s `Display` returns `fmt::Error` →
   `.to_string()` panics. The panic dropped the in-flight MCP reply (caller saw a
   cryptic `-32603 worker dropped the reply`); the worker recovered next call, so
   a per-call DoS under a normal-looking command. **Any** unknown specifier
   tripped it.
2. **`date "@1700000000"` echoed the literal with exit 0.** The arg help
   documented `@TIMESTAMP` but `execute()` never parsed `@` — it fell through to
   the format path and printed the input back as success. (Compounding: kaish's
   lexer rejects a bare `@`, so the documented form was also untypeable unquoted —
   triply dead: untypeable, unparsed, and silently wrong when forced through.)
3. **`TZ=zone date` silently ignored the zone.** `Local::now()` reads chrono's
   cached process offset; a kaish per-command env assignment never reached it.
   `TZ=Asia/Tokyo date +%H:%M` returned Eastern, not Tokyo. Wrong answer,
   confident code.

---

## The design

Scoped to kaish's philosophy: **cover the convergent 90% the fleet actually
types, make the long tail fail loudly.** Not a GNU `date` clone. Ordered by
value-to-effort (the lite-tier data reordered this from the first draft —
epoch-decode up front, `--json` to the back).

### 1. `@N` epoch-decode — the best value-to-effort item

Near-universal demand (4/4 lite, 5/6 flagship), tiny code, and it closes footgun
#2 on its own. A leading-`@` argument parses as epoch seconds and formats like
`now`, honoring `-u` and any `+FORMAT`. A malformed `@` is a loud error, never an
echo. (Bare `@` is lexer-rejected, so the typed form is quoted — `date "@N"` — or
arrives via `-d "@N"`.)

### 2. Alias the muscle-memory spellings

- `-I[FMT]` / `--iso-8601[=FMT]`, `FMT ∈ {date,hours,minutes,seconds,ns}`,
  default `date`.
- `-R` / `--rfc-2822` → `to_rfc2822()`.
- `--rfc-3339=FMT` for completeness.
- Keep `+%s`; **demote `--unix` / `--iso` / `--format` to hidden aliases** —
  0/9 models typed them, so they're confusion surface, not ergonomics.

### 3. Harden the format path — translate what models type, reject the rest

`+FORMAT` is the hot path (filename stamps were 4/4 lite), so "reject everything
unknown" is too blunt for the one unknown specifier models actually reach for:

- **Translate `%N` (nanoseconds)** and its width variants `%3N`/`%6N`/`%9N` to
  chrono's `%9f`/`%3f`/`%6f`, so `date +%s%N` *works* rather than panicking.
- **Reject genuinely-unknown specifiers loudly** — validate the format string
  against `Item::Error` up front, returning `exit 2: date: unknown format
  specifier`. No specifier, known or unknown, can panic the worker. Closes #1.
- **`TZ` honored, never silently wrong.** Effective zone is `-u` → UTC, else
  `--tz ZONE`, else exported `$TZ`, else host local, resolved via `chrono-tz`.
  Unknown zone errors loudly. Closes #3.

### 4. `-d` / `--date STRING` — the hard part, the high-value part

The one feature that's real work, and the one the whole fleet reaches for first.
Hand-rolled parser over the convergent subset (no open-ended NL grammar, no new
dep for the grammar — "fail loud on anything outside the subset" is the
kaish-correct behavior, and the experiment handed us the exact subset):

- `yesterday` / `tomorrow` / `now` / `today`.
- `N {sec,min,hour,day,week,month,year}[s] ago`, `±N units`, `in N units`,
  multi-term (`1 hour 30 minutes`).
- `next`/`last <weekday>`.
- absolute ISO date/datetime (`2026-01-01`, `2026-01-01T09:00`).
- **`<absolute> ± <offset>`** (`2026-06-01 -1 day`) — the nested-substitution
  idiom DeepSeek produced (`date -d "$(date +%Y-%m-01) -1 day"`).

### 5. `-r FILE` — file mtime

Justified by *fit*, not demand (3/9). Unlike most of GNU `date`, `-r` *reads the
filesystem* — squarely kaish's wheelhouse, the one form that exercises the
read-only VFS. Resolve `FILE` through the VFS and format its mtime like `now`.

### 6. `--json` — a designer's bet, shipped anyway

Every kaish builtin can emit structured output, and a `date --json` that hands
back every field at once *looks* ideal for a model consumer (pull `.weekday`
instead of recalling `+%A`):

```
{"iso":"2026-06-14T09:03:15-04:00","epoch":1781442195,"utc":"2026-06-14T13:03:15Z",
 "local":"2026-06-14 09:03:15","weekday":"Sunday","tz":"-04:00","offset_seconds":-14400}
```

But be honest: **0 of 9 models reached for anything JSON-shaped.** The survey
measures what models type unprompted, and it says they think in strftime and
`-d`, not JSON. The data-driven recommendation was to gate this behind a
follow-up probe. **Amy chose to ship it now** anyway — it's nearly free
(`OutputData::with_rich_json`) and consistent with every other builtin. Recorded
here as the one place the build deviated from the survey's recommendation.

### Scope discipline — what we deliberately won't do

- No `date -s` / setting the clock (kaish never mutates host state).
- No full GNU NL grammar ("3rd thursday of next month"). Cover the subset; fail
  the rest loudly.
- No locale-aware month/day tables beyond what chrono gives free.
- No BSD `-v`/`-j -f` surface — data-backed, not a judgment call: 9/9 GNU-shaped.

### Determinism — an injectable clock

`date` reads the wall clock — the one builtin whose output isn't a pure function
of the VFS, so it injects nondeterminism into otherwise-reproducible read
sessions. That's benign (reading the clock mutates nothing), but it argues for an
**injectable clock**: a `Clock` trait (real `Utc::now` in prod, a fixed instant
in tests). The old tests could only assert loose properties ("contains a `-`")
because they couldn't pin "now." The clock lets the new paths assert *exactly* —
`date -d "@1700000000" -u` must equal `2023-11-14T22:13:20Z`, an assertion that
can and will fail if the epoch math regresses.

---

## Status — as built

**Implemented 2026-06-14** (`feat(date): rewrite builtin against the empirical
GNU spec`). `crates/kaish-kernel/src/tools/builtin/date.rs` was a full disposable
rewrite; everything in [The design](#the-design) landed, including `--json`.

The one change the *implementation* forced over the spec text: arithmetic runs in
the concrete effective `TimeZone` (generic over `Tz`) and collapses to a fixed
offset only at render, with `day`/`week`/`month`/`year` using calendar
`Days`/`Months`. The first draft collapsed to `FixedOffset` *before* the math,
which **silently shifted the wall clock by an hour across a DST transition** in a
named/local zone — found by kaibo's `deepseek` cast (`deepseek-v4-pro`) reviewing
the landed code, exactly the silent-wrong class the rewrite existed to kill.

### Test plan — the survey is the golden corpus

Per the project TDD standard — tests that can and will fail:

1. **Footgun regressions (red first):** `date "@1700000000" -u` equals the known
   UTC string (was: echoes); a genuinely-unknown specifier like `date +%Q` is
   `exit 2` (was: *any* unknown specifier panics); `date +%s%N` *produces
   nanoseconds*; a Tokyo-zone assertion diverges from `-u` (was: silently equal).
2. **The empirical table is the corpus.** Every form above either produces the
   right answer or fails loud — that turns "what models actually send" from prose
   into an enforced contract, run against the injectable fixed clock.
3. **`-d` grammar:** one assertion per subset form, *including* the
   absolute-plus-offset case (`date -d "2026-06-01 -1 day"` = `2026-05-31`).
4. **`--json`:** parse the object, pin every field and `epoch`↔`iso` consistency.

As shipped: **48 in-module tests** (fixed clock, exact assertions) + **7
kernel-routed tests** (the production lex → parse → schema-bind → clap path,
including spaces in `-d`, a DST crossing, and a zone-carrying `--json`). The
DeepSeek review drove coverage from 27 → 48 (it found `--rfc-3339` had *zero*
tests and two kernel tests were tautological).

### The review pass — back through kaibo

Closing the loop on the instrument: once the rewrite was green, we sent the
landed code through kaibo's `deepseek` cast (`deepseek-v4-pro`) for an
adversarial review weighted on test coverage. It earned its keep. It flagged the
DST-correctness bug above as the headline, and — bluntly — that `--rfc-3339` was
a ghost path with *zero* tests and two kernel-routed tests were structurally
tautological (a length-and-dash-count check that a wrong date would still pass).
All findings were actioned; coverage went 27 → 48 in-module tests. The same
read-only-kaish agent that ran the survey also caught the bug the survey's design
was meant to prevent — a tidy demonstration that the loop closes.

### Known limitations (documented, not bugs)

- Bare `@` is lexer-rejected (a kaish-wide tokenizer constraint), so the epoch
  form must be quoted: `date -d "@N"`.
- `%Z` on a named zone renders the numeric offset, not the abbreviation — the
  fixed-offset render drops the IANA name.

---

*Methodology footnote: "without grounding" is doing real work — each of the nine
models answered from weights alone, no tool calls, no retrieval, all in one
session through kaibo. Reproducible with any panel of models you can prompt cold.*
