# Designing a Language by Asking the Language Models

*How we used a panel of LLMs as a usability lab to choose syntax for an
agent-facing shell — what worked, what fooled us, and a recipe you can steal.*

---

## The premise

[kaish](https://github.com/tobert/kaish) is a shell whose primary users are AI
agents. It's a `sh`-flavored language an LLM drives as an MCP tool: pre-validated,
no word splitting, structured output. When the main author of the code you're
designing is a language model, a question follows that feels slightly heretical:

> If your users are language models, then language models are your usability lab.

Human taste still matters — readability, consistency, the feel of the thing. But
"can a model reliably *emit* this syntax under realistic conditions" stops being a
guess you argue about and becomes a thing you can **measure**, today, for the price
of a few API calls. So we did. This is the writeup of the method, grounded in a
real decision: adding array/hash (list/record) literals to kaish. The design doc
that fell out of it is [`arrays-and-hashes.md`](arrays-and-hashes.md); this doc is
about *how* we got there.

The short version: a few hours of `oneshot` calls to DeepSeek, Gemini, and Claude
Haiku settled a dozen syntax questions, caught problems no amount of armchair
debate would have, and — the punchline — taught us that the model picking the
syntax matters far less than *how completely you teach it*.

---

## The setup

Nothing fancy. The instruments:

- **Cross-vendor breadth.** DeepSeek V4, Gemini 3.x, Claude Haiku. Different
  training mixes fail differently; agreement across vendors is a much stronger
  signal than one model nodding along.
- **Capability tiers, deliberately.** Not just the flagship models — the *small,
  fast, fast-and-loose* ones (DeepSeek-flash with thinking off, Gemini-lite,
  Haiku). These are the MVPs of the whole exercise. A capable model papers over a
  shaky design; a cheap one face-plants on it and shows you exactly where the
  cracks are.
- **Stateless one-shots.** Each test is a self-contained prompt: a tight syntax
  cheat-sheet plus a handful of tasks, "output code only, no prose." No
  conversation state to contaminate the result. Cheap, parallel, seconds per run.

That's it. The leverage isn't the tooling, it's the loop.

---

## The loop

1. **Draft a candidate syntax** as a terse cheat-sheet — the kind of thing you'd
   put in the real docs.
2. **Write bash-tempting tasks.** Pick tasks where a model's training priors
   (bash, Python, JS) would *leak* if your syntax is weak. "Iterate a hash and
   print each key/value." "Append in a loop." "Check membership and branch." If
   the model reverts to `${arr[@]}` or `declare -A`, you'll see it.
3. **Grade the divergences, not just the pass/fail.** This is the core trick. When
   two models disagree, or one fumbles, that spot is almost always **an
   under-specified corner of your design**, not a dumb model. The errors are a map
   of your spec's holes.
4. **Iterate.** Tighten the ambiguous corner, A/B a real alternative, vary how
   much you spell out, and re-run.

The output you care about isn't "did it score 9/10." It's *where* the 1/10 landed
and *why*.

---

## The heuristics (the reusable part)

These generalize past kaish. If you ever design syntax, config, or a DSL that an
LLM will write, these are the lessons that earned their keep.

### 1. A divergence is a spec gap wearing a model costume

Our first round on membership offered a `has` command: `has $list elem`. The
flagship models handled it. The fast ones did not — and they failed *differently*:

```sh
# deepseek-flash wrapped it as a value:
if $(has $colors green) { ... }
# gemini-lite garbled the arguments entirely:
if has keys $inventory "bananas" { ... }
```

Two models, two different wrong answers, same root cause: a bare command named
`has` is ambiguous about whether it's a value or a control-flow predicate, and the
models papered over the ambiguity in incompatible ways. The divergence *was* the
finding. We re-spelled it as a test operator —

```sh
if [[ green in $colors ]]; then echo "has green"; fi
```

— and both fast models went 7/7 across key-membership, `not in`, nested lists,
membership-in-a-loop, and compound `&&`. `in` slots into an existing mental model
(`==`, `-f`, Python's `in`, `for x in`); a bare command doesn't compose. We didn't
"fix the models." We fixed the spec the divergence pointed at.

### 2. The weak-model tail is your actual constraint

Run the same test across tiers and you'll watch a design's true robustness
separate from a model's raw strength. Our nest-vs-spread rule — a bare `$xs` inside
a list literal *nests* as one element, `...$xs` *flattens* it —

```sh
nested=[$a $b]        # a list of two lists
flat=[...$a ...$b]    # one flat list
```

— went **12/12 on both lite models**, with perfect discrimination in both
directions and unprompted nested indexing. That's a green light you can *trust*,
because it held at the bottom of the capability range. Conversely, a design that
only the flagship gets right is a design that will generate broken code in
production the first time someone routes through a cheaper model. **Design for the
tail.** The flagships are fine either way (more on that below).

### 3. What you don't show, the model fills from its priors

Models are relentlessly associative. Anything your spec leaves unspecified gets
back-filled from training — and for a shell, training means *bash*. Some receipts:

- Omit an append idiom, and every model reaches for `xs=[$xs new]` splat —
  bash/Python muscle memory.
- Omit list indexing from an example, and a model invents `$(colors.1)` —
  dropping the `$`, hallucinating a `$()`.
- Omit how to init an empty list, and Haiku — *Haiku!* — leaks bash array syntax
  `keys_list=()`.

The corollary is uncomfortable but useful: **silence is not neutral.** If you
don't want the bash default, you have to actively show the alternative, including
a "don't do this" counter-example, because the model's hands type the prior.

### 4. Teach an operator inside its full control structure, never bare

This one cost us a near-miss. A terse cheat-sheet listed membership as a standalone
line:

```
[[ key in $r ]]    # membership
```

A model read that as a *complete statement* and emitted malformed garbage when
asked to branch on it (`if then; do … fi`). The *same model*, shown the operator
inside a full `if [[ … ]]; then … fi`, produced flawless code. A novel operator
demonstrated in isolation reads as a finished thought. Always document it embedded
in the construct you actually want.

### 5. Vary the scaffolding to separate *compliance* from *default pull*

A spec sitting directly above the tasks with an explicit "don't use bash" is an
easy exam — it measures whether the model can *follow* your rules, not whether it
*reaches* for them. So vary the conditions:

- **Rules-only** vs **example-only** (no rules, just a worked snippet to imitate).
  These fail differently — example-only nails exactly what the example shows and
  guesses wrong on what it omits.
- **Drop the anti-bash warning** and see what leaks when not suppressed.
- **Bury the spec** behind filler to test recall vs recency.
- **Omit the idiom entirely** and watch what the model invents — that tells you the
  default you're fighting (see #3).

Each framing is a different lens on the same design. The example-driven runs matter
most, because that's how your real docs will teach.

### 6. A/B the *real* alternatives, head to head

Don't just validate your favorite — pit it against the contender on identical
tasks. `has` command vs `[[ in ]]` operator. Bare `$xs[0]` access vs braced
`${xs[0]}`. 1-indexed vs 0-indexed. The relative error rate between two concrete
syntaxes is worth more than an absolute score on one.

### 7. Ground it against the actual implementation

Model ergonomics is one axis; parser cost is the other, and they trade off. Before
committing the access syntax we read kaish's real lexer/parser/validator. It turned
out braced `${path}` access *rode infrastructure that already existed*, while bare
`$user.name` postfix access needed a whole new grammar — and braced is
bash-consistent (`${arr[0]}`) besides. The model evidence and the implementation
cost pointed the same way; that's a decision you can make with both hands. Letting
models vote on a syntax your parser can't cheaply support is how you design
yourself into a corner.

---

## A few war stories

**Membership** was the cleanest win — a `has` *command* the small models couldn't
keep straight became `[[ key in $r ]]` and went 7/7 on the tail. (#1)

**The bare for-head.** `for k in keys $r; do …` — letting a builtin sit unwrapped in
the loop head — is technically a parser special case, but *every* model wrote it
naturally and none reached for `$(keys $r)`. When the entire panel converges on the
ergonomic form unprompted, fighting it for purity's sake is a tax you pay forever.

**The silent-corruption catch.** Under reduced scaffolding, a capable model wrote
`append $colors purple`, **threw the returned list away**, and then reported the old
length — a silently wrong answer. That observation killed the pure-functional
`append` and made in-place `push` the primary idiom. A usability test surfaced a
*data-integrity* hazard, not just an ergonomic one.

**A second model as design reviewer.** We handed the whole draft design doc to
Gemini Pro and asked for a hostile review. It earned its keep: it killed our
overload of `$()` for string interpolation (use the `${…}` that already bounds
expansions), caught that our new `${#xs}` length collided with the existing
`${#NAME}`, flagged that `export`-ing a structured value was undefined behavior, and
spotted that we allowed commas in records but not lists — a guaranteed model
stumble. Different model, different role (critic, not generator), high yield.

---

## The two ways the method fooled us

Honesty is part of the method, so:

**We tested a language we don't ship.** Several early cheat-sheets used curly-brace
blocks — `for x in xs { … }` — which kaish *doesn't have*; its blocks are sh-style
`do/done` / `then/fi`. The models happily used whatever delimiter we showed, so the
mistake rode along invisibly until someone re-read the actual language reference.
The collection-specific findings survived (they don't depend on the block
delimiter), but the lesson stands in neon: **test the real target syntax, or you're
validating a dialect that doesn't exist.** Pin your harness to the shipping grammar.

**Easy-condition bias.** Our first runs had the spec directly above the tasks *and*
an explicit "no bash." That measures compliance, not instinct, and it flatters your
design. We only learned what the syntax's *default pull* was after we stripped the
warning and the worked examples (see #5). If your test conditions are too kind, your
green checkmarks are lying to you.

---

## The meta-finding: calibrate to the tail, not the flagship

The most decision-relevant result came last. We fed Haiku the *discarded* syntaxes —
bare access, the `len` builtin, the nested `len $(keys $r)`, the `has` command in an
`if`, 1-indexing, implicit splat — fully expecting it to struggle.

It went 8/8 on essentially all of them. It followed 1-indexing without reverting. It
used `has`-in-`if` correctly — the exact thing that broke the smaller models. Its
*only* stumble was a spec gap (no empty-list example → it leaked bash `()`).

So the capable model is robust to almost any syntax you hand it. Which means the
syntax choice was never *for* the capable model. It was for:

1. the **weak-model tail** that actually fumbles,
2. **consistency** (one length form, one append idiom),
3. **parser/implementation cost**,
4. **silent-failure traps** (the discarded-`append` data corruption),

and — above all — **completeness of how you teach it**, since even Haiku falls back
to bash exactly where the spec goes quiet. Design and document for the weak tail and
for completeness. The strong models will be fine. They were always going to be fine.

---

## The recipe (steal this)

A checklist for using an LLM panel as a syntax usability lab:

1. **Write the cheat-sheet** you'd ship, in the *real* target grammar. Don't
   improvise delimiters.
2. **Pick bash-/Python-tempting tasks** — ones where training priors leak if the
   design is weak.
3. **Run a cross-vendor panel across capability tiers.** Weight the cheap,
   fast-and-loose models heaviest.
4. **Grade divergences, not scores.** Each disagreement or fumble is a coordinate
   on your spec's holes.
5. **Vary the scaffolding** — rules-only, example-only, no-warning, omit-the-idiom
   — to separate "can follow" from "will reach for."
6. **A/B real alternatives** head-to-head on identical tasks.
7. **Cross-check against your parser** so ergonomics and implementation cost vote
   together.
8. **Use a second model as a hostile reviewer** of the whole design.
9. **Re-test the final, changed forms** before you commit — the things you adopted
   late were never actually validated in their final shape.

Total cost for the kaish collections design: a few dozen one-shot calls, a few
hours. It settled a dozen contentious syntax decisions with evidence instead of
vibes, caught a silent-data-corruption footgun and a half-dozen ambiguities, and
left a paper trail of *why* each call was made.

---

## What this does NOT tell you

Be honest about the method's blind spots:

- It measures **generation**, not **comprehension** or long-horizon use. A model
  emitting `${xs[0]}` once says nothing about debugging it three turns later.
- It's biased toward **today's** training priors. Re-weighting on the bash default
  is a moving target as models change.
- It can't price **human** readability, long-term maintenance, or how the syntax
  composes with the *rest* of the language. Models judge the local form, not the
  global grammar.
- Green across an easy condition is not green. Mind the framing (the bias above).

It's a usability lab, not an oracle. But it's a *fast, cheap, repeatable* usability
lab with thousands of synthetic users who think a lot like your real ones — and for
a language whose users are language models, that's about as close to the real thing
as design feedback gets.

---

*Methodology notes from the kaish project. The collection-syntax design doc this
produced is [`arrays-and-hashes.md`](arrays-and-hashes.md). Panel: DeepSeek V4
(pro/flash), Gemini 3.x (pro/lite), Claude Haiku — June 2026.*
