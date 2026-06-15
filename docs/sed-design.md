# Designing kaish `sed` from the muscle memory out

*How a small cross-model panel decided which slice of `sed` kaish should
implement — and which silent divergences had to die.*

This is the `sed` companion to [`date-design.md`](date-design.md) and the method
writeup in [`designing-syntax-with-llms.md`](designing-syntax-with-llms.md). Same
premise: kaish's users are language models, so the models are the usability lab.
`sed` matters because [kaibo](https://github.com/tobert/kaijutsu) drives it heavily
when reading and editing code, so a `sed` that *silently does the wrong thing* is a
data-integrity hazard, not just an ergonomic wart.

## The principle

kaish `sed` is deliberately **not** full GNU/BSD `sed`. It's the slice humans and
agents reach for by reflex — a "muscle-memory `sed`", closest in spirit to
**busybox** (a strong influence). The 80% rule, applied to a stream editor.

But "subset" has a hard constraint from kaish's core posture: **fail loud, never
silently corrupt.** A muscle-memory form we don't support must *error*, never
quietly produce wrong output. That line is what this pass was really about.

## The panel

Two vendors, both at the *lite* tier (where the default-pull shows clearest):
**Gemini-flash** and **Claude-haiku**. Eight bash-tempting text-editing tasks,
"output only the `sed` command." No mention of kaish or its limits — we wanted
each model's untouched `sed` instinct.

The result was stark: **both models converged, and on the form kaish broke.**

| Task | Gemini-flash | Claude-haiku | kaish (before) |
|---|---|---|---|
| two substitutions | `-e …g -e …g` | `-e …g -e …g` | ✅ (after the `-e` fix) |
| word swap (capture groups) | `-E '(…) (…)'` | `'\(…\) \(…\)'` | ✗ `-E` rejected · BRE silently no-match |
| 2nd occurrence | `s/x/Y/2` | `s/x/Y/2` | ✗ silently did the 1st |
| append after match | `/ERROR/a ---` | `/ERROR/a\---` | ✗ loud error |
| insert first line | `1i …` | `1i…` | ✗ loud error |
| delete two patterns | `/DEBUG/d; /TRACE/d` | `/DEBUG/d;/TRACE/d` | ✗ **`;` silently dropped the 2nd** |
| in-place edit | `-i 's/…/g'` | `-i 's/…/g'` | ✗ loud error |
| transliterate | `y/abc/xyz/` | `y/abc/xyz/` | ✗ loud error |

## The findings

**The divergence is the spec gap.** The single clean divergence — task 2, where
Gemini wrote ERE-with-`-E` and Haiku wrote BRE-`\(…\)` — maps *exactly* onto
kaish's regex-dialect hole. Two models pick opposite dialects; kaish broke both,
one loud (`-E` rejected) and one **silent** (BRE `\(…\)` compiles as literal
parens and just doesn't match). That's the methodology's core lesson landing on a
real bug.

**The cardinal-sin shortlist.** Four behaviors weren't ergonomic gaps — they were
*silent corruption*, the same class as the `sed -e -e` P1 that kicked this off:

1. `;` silently dropped every command after the first.
2. BRE `\(…\)` silently didn't match (ERE engine, no diagnostic).
3. `s///N` silently ignored the count (replaced the 1st).
4. `0,/re/` silently matched nothing.

Both lite models, both vendors, reached for #1–#3 *unprompted*. That's the
strongest possible signal: the default-pull lands squarely on the silent paths.

## The decisions

**Closed in this pass** (all forms the panel reached for):

- **`;` command separation** — parse `;` as a top-level command separator, the
  same ordered program `-e A -e B` produces. `;` inside `s///`, `y///`, or a
  `/regex/` stays literal; `a`/`i`/`c` swallow the rest as text (GNU one-liner).
- **Regex-dialect honesty** — accept `-E`/`-r` as no-ops (kaish is *always* ERE),
  and **loud-error on the BRE capture-group idiom** (`\(…\)` + a `\N` backref is
  unambiguous BRE intent) with a hint to use `(…)`. Silent-wrong → loud-with-fix.
- **`s///N` / `s///Ng`** — the Nth-match and Nth-onward forms, properly.
- **`a`/`i`/`c` + `y///`** — append/insert/change lines and transliterate, the
  remaining high-frequency commands both models emitted.

**Deliberately still out of scope** (and now erroring honestly, never silent):
hold space (`h`/`H`/`g`/`G`/`x`), labels/branching (`b`/`t`/`:`), `w`/`r` file
I/O, and GNU address extensions (`1~2`, `0,/re/`, `/re/,+N`).

**`-i` (in-place) — deferred, on purpose.** Both models reached for it, so it's
the strongest remaining ergonomic gap. But in-place editing collides with kaish's
write model: writes are meant to route through the VFS / overlay / latch machinery
(so sandbox, overlay-transaction, and confirmation modes all hold), and `-i` is a
file-mutating side effect that needs to honor those. That's its own design, not a
parser tweak — tracked in `docs/issues.md`. Until then `-i` errors loudly rather
than pretending.

## Why ERE, still

kaish chose ERE (egrep-style) over `sed`'s default BRE on purpose: `(…)`, `+`,
`?`, `{n,m}`, `|` unescaped is the form models also know from `grep -E`, every
regex library, and most languages — and it's what kaish's other regex tools
(`grep`, `awk`) use, so the dialect is consistent across the shell. The panel
shows models *will* type BRE `\(…\)` by reflex, so the fix isn't to switch
dialects — it's to **catch that reflex and correct it out loud**, which is what
the BRE-idiom diagnostic does.

---

*Panel: Gemini-flash, Claude-haiku — June 2026. Implementation in
`crates/kaish-kernel/src/tools/builtin/sed.rs`; the supported surface is
summarized in [`LANGUAGE.md`](LANGUAGE.md).*
