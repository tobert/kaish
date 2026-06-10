# Arrays & Hashes — Design Doc

Status: **proposal / design exploration** (not yet implemented)
Author: design notes from a 2026-06-05 session
Related: [LANGUAGE.md](LANGUAGE.md), [issues.md](issues.md), `arch_data_iteration.md`,
`arch_for_newline_split.md`, `arch_no_json_sniffing.md` (auto-memory)

## Motivation

kaish already has a value model — `Value::Json`, structured `$()`, and a `for` loop that
iterates a builtin's `.data` element-wise. What it lacks is *surface syntax* for users to
**construct, index, and mutate** collections directly: array (list) and hash (record) literals,
element/field access, length, membership, and append.

The goal is to expose the collections kaish can already represent, with ergonomics that are
**predictable for AI agents** — the primary audience. That last constraint is not hand-wavy: we
ran an LLM stress test (below) and let it drive several decisions.

## Design principle: one value model, not two namespaces

bash has two second-class, non-nestable namespaces (`declare -a` indexed, `declare -A`
associative), neither of which is a real value. kaish already votes against this: `Value::Json`
is one recursive, nestable model. So:

- A **list** is a JSON array: `[apple banana cherry]`
- A **record** (hash) is a JSON object: `{name: amy, role: maintainer}`
- They nest arbitrarily, are passed by value, and round-trip through `$()` and `--json`.

A "hash" is just a record. There is no separate associative-array concept and no `declare -A`.
This also kills bash's worst silent-fallback footgun — forgetting `declare -A` silently degrades
to an indexed array — which directly violates our "silent fallbacks are usually a mistake"
directive.

## Proposed syntax

```sh
# LISTS — space-separated, like shell words
fruits=[apple banana cherry]
nums=[1 2 3]
empty=[]

# RECORDS — string-keyed maps
user={name: amy, role: maintainer, age: 40}
nested={tags: [a b c], meta: {active: true}}

# multi-line literals with trailing commas
services={
  web:    {port: 8080, replicas: 3, healthy: true},
  api:    {port: 9000, replicas: 2, healthy: false},
}

# ACCESS — 0-indexed. Bare $xs is the WHOLE value; brace ANY path/index with ${…}.
${fruits[0]}          # apple
${user.name}          # amy
${nested.tags[1]}     # b
${nested.meta.active} # true
$fruits               # whole list — ONE value (no word splitting)
${fruits[-1]}         # cherry        (negative index)
${fruits[0:2]}        # [apple banana] (slice: end-exclusive)
${user.$k}            # dynamic key
${services.web.port}  # 8080 — chained dot (web is a record)
${nested.tags[0]}     # a    — chained dot + bracket (tags is a list)

# Same form inside strings — no special string rule (this is WHY we brace).
# Bare "$user.name" would expand $user only and leave ".name" literal; always brace.
echo "${user.name} lives here"    # amy lives here

# Scalars stay scalars — no auto-coercion to a collection. $PATH is a string;
# ${PATH[0]} is an error. Want a list? split it explicitly:
parts=$(split $PATH :)

# RECORD field set (insert/update) — by path; no spaces (assignment rule)
user.email=amy@example.com
tally={}
tally.total=${#services}           # length is ${#…}, not a builtin

# OPS — keys/values are builtins; length is the param-expansion ${#…} (lists AND records).
echo ${#fruits}      # 3   (element count)
echo ${#user}        # 3   (key count — record)
keys $user           # [name role age]      (builtin; capture with $())
values $user         # [amy maintainer 40]  (builtin; capture with $())
push fruits date       # in-place append; takes the variable NAME (like read/unset), not $fruits
new=[...$fruits date]  # build a NEW list with an extra element (spread; there is no `append` builtin)

# MEMBERSHIP — an operator inside [[ ]], NOT a command. Always shown inside a full
# test, never as a standalone [[ ]] line (see Teaching notes #1).
if [[ banana in $fruits ]]; then echo "have banana"; fi   # element in list
if [[ name in $user ]]; then echo "has name"; fi          # key in record
if [[ tmp not in $services ]]; then echo "no tmp"; fi     # absent
if [[ apple in $fruits && web in $services ]]; then echo both; fi

# ITERATION — for head accepts a bare builtin (no $()).
# Blocks are kaish's standard sh-style do/done & then/fi — NOT curly braces.
for f in $fruits; do echo $f; done
for k in keys $user; do echo "$k = ${user.$k}"; done
for v in values $user; do echo $v; done
for p in ${nested.tags}; do echo $p; done   # iterate a nested list (path → braced)
```

## Worked example

```sh
#!/usr/bin/env kaish
# Fleet summary: report each service and decide what to restart.

services={
  web:    {port: 8080, replicas: 3, healthy: true},
  api:    {port: 9000, replicas: 2, healthy: false},
  worker: {port: 0,    replicas: 5, healthy: true},
}

to_restart=[]

for name in keys $services; do
  cfg=${services.$name}
  echo "$name: port=${cfg.port} replicas=${cfg.replicas}"

  if [[ ${cfg.healthy} == false ]]; then
    push to_restart $name          # in-place append, no reassignment, no discard trap
  fi
done

echo "restart count: ${#to_restart}"
for svc in $to_restart; do
  echo "restarting $svc on port ${services.$svc.port}"
done
```

## Decisions, and the evidence behind them

We stress-tested candidate syntaxes against four models (DeepSeek V4-Pro/Flash, Gemini
Pro/Lite) across multiple rounds — varying how much spec we provided and including
deliberately "fast & loose" small models. Findings:

> Caveat: the *first* round of cheat-sheets used curly-brace blocks (`for x in xs { … }`), which
> kaish does **not** have — its blocks are sh-style `do/done`/`then/fi`. Later rounds used the
> real block syntax and re-confirmed the bare for-head and `[[ in ]]` results (see Teaching notes
> #7). So the collection-specific conclusions below stand under real kaish syntax; the curly-brace
> mistake changed nothing.

1. **Non-bash syntax does NOT confuse models.** Zero bash leakage across every model and
   round — no `${arr[@]}`, no `declare -A`, no `+=()`, no 1-indexing — *even with the
   anti-bash instruction removed and using the loosest small models*. The value-shell surface
   does not pull models back toward bash. This was the headline question and it's settled.

2. **0-indexing is fine.** No off-by-one reversion to 1-indexed habits, even from fast models.
   (zsh/fish 1-indexing is a known porting-bug source; JSON/our lineage says 0.)

3. **`for k in keys $r` (bare builtin in the for-head) is a GO.** Every model used it correctly
   and naturally — plain *and* nested (`for p in ${services.$s}` inside `for s in keys $services`).
   The head position is special enough that nobody got confused, and it's what shell hands type.
   Requiring `$(keys $r)` there is technically consistent with `for v in $(cmd)`, but models
   dropped the `$()` anyway; relaxing the head position costs nothing and removes the most
   common error.

4. **Membership is `[[ key in $r ]]`, NOT `if has`.** This is the clearest result. A `has`
   *command* was unstable: fast models either wrapped it (`if $(has …)`) or garbled the args
   (`if has keys $inventory "bananas"`). Re-spelled as a `[[ ]]` operator, **both fast/loose
   models got all seven membership tasks correct** — key presence, `not in`, list elements,
   nested-list (`[[ 443 in ${servers.web} ]]`), membership in a loop, and compound `&&`. `in`
   slots into the existing test-operator mental model (`==`, `-f`) reinforced by Python's `in`
   and `for x in`. It composes; a bare command does not.

5. **Length collapses onto `${#…}`; `keys`/`values` are builtins that accept records directly.**
   The single most error-prone construct in testing was *nested* capture: `len $(keys $r)`. Fast
   models collapsed it (`echo len keys $inventory`). Two fixes: (a) length is the existing
   param-expansion `${#xs}` extended to collections (list → element count, record → key count),
   not a `len` builtin — one length form, consistent with `${#NAME}` already in LANGUAGE.md, and
   the lexer change is a single spot (`kernel.rs:2802`); (b) `keys $r`/`values $r` accept records
   directly so you never nest. The error mode goes with it.

6. **`append` returns a new list — and discard is a trap.** Under reduced scaffolding, a capable
   model wrote `append $colors purple` and threw the result away, then reported the *old*
   length. Silent wrong answer = exactly the data-corruption case our directives forbid. This
   drove `push` (in-place) as the append idiom and the decision to drop the pure `append` builtin
   entirely (new-list construction is `...` spread instead).

> Model-strength calibration (2026-06-05, Haiku subagent): Haiku handled the **discarded**
> syntaxes too — bare access, the `len` builtin, nested `len $(keys $r)`, the `has` command in an
> `if`, 1-indexed access, implicit splat — all 8/8, including the `has`/nested-`len` forms that
> broke the smaller deepseek-flash / gemini-lite. So the choices here are NOT driven by Haiku's
> capability; they're driven by (1) the weak-model *tail*, (2) consistency, (3) parser/impl cost,
> (4) silent-failure traps. Haiku's only stumble was a *spec gap* (no empty-list-init example → it
> leaked bash `()`), reinforcing that example completeness matters more than model strength.

## Resolved decisions

Decided during this design session; rationale kept for the record.

- **Assignment spacing — no spaces, everywhere** (`fruits=[...]`, `tally.total=$(...)`,
  `user.email=amy@example.com`). Three reasons converge:
  (1) **consistency** — kaish scalar assignment is already no-space (`NAME="value"`); a split
  grammar (no-space scalar / spaced collection) would surprise and would break the habit on
  scalars;
  (2) **shellcheck ethos** — spaced `=` violates SC1068 ("Don't put spaces around `=`"), against
  the "passes `shellcheck --enable=all`" identity; in bash the spaced forms don't even assign
  (`x = 5` runs command `x`; `x= 5` runs `5` with `x=''`);
  (3) **model priors** — models' trained default *is* no-space shell assignment, so this is the
  more robust choice, not a concession (the experiment's spaced form worked, but cut against
  their grain). Path-set lvalues (`user.email=`, `tally.total=`) follow the same no-space rule.

- **Append — `push` (in-place); spelling frozen, pure `append` dropped.** `push name value`
  mutates the named list in place. Because it must write back to the caller's variable, it takes
  the variable **name** (bareword, like `read`/`unset`), not `$name` — `push xs date`, not
  `push $xs date`. The pure functional `append` builtin (returns a new list) is **dropped**: its
  only niche — build a new list with an extra element — is already covered by `...` spread
  (`new=[...$xs date]`), and keeping it re-imported the silent-discard trap (evidence #6). Net:
  **one** append idiom (`push`, in-place) + `...` spread for new-list construction; no two
  competing spellings.
  **`push` to an undefined target is an error, never a silent create** (a typo `push fruit x` for
  `fruits` must NOT spawn a new `fruit` list). The validator tracks variable names in scope
  (`walker.rs:458`), so undefined-target is catchable as an E-code **ahead of runtime**; a target
  that exists but isn't a list is a **runtime** error (the validator tracks names, not types).
  Both crash — no silent corruption.

- **Length is `${#…}`, not a `len` builtin.** `${#xs}` returns element count for a list and key
  count for a record — the existing `${#NAME}` param-expansion (LANGUAGE.md) extended to
  collections. One length form, no nested `len $(keys $r)` trap, lexer change is a single spot
  (`kernel.rs:2802`). `keys $r` / `values $r` stay builtins (no clean param-expansion form, and
  they tested well). Bare `$xs` whole-value, `${#xs}` for its size.

- **String path interpolation is `${path}`; no auto-interpolation.** Inside a string, brace the
  path to bound it — `"${user.name}"`, `"${user.tags[0]}"`, `"${user.$k}"` — exactly the
  `$VAR` vs `${VAR}` rule already in LANGUAGE.md. Bare `"$user.name"` expands `$user` only and
  leaves `.name` as literal text; this is the *current* lexer behavior (`parser.rs:627` stops
  `$VAR` at `.`), not a new rule. `$()` is **not** overloaded for access — it stays
  command-substitution only (this also kills the "models wrap everything in `$()`" risk Gemini
  flagged). `${...}` already parses dotted segments (`lexer.rs:2033`), so this rides existing
  machinery.

- **Value semantics — copy-on-assign.** `b=$a` copies the value; lists/records are never shared
  references. Reference aliasing would be catastrophic for kaish specifically: `Kernel::fork` +
  scatter/gather + background jobs would leak mutable state across workers. Copy matches JSON
  intuitions and shell's value model.

- **Scalars never auto-coerce to collections.** `$PATH` is a string; `${PATH[0]}` / `$PATH.foo`
  on a scalar is an error, not a magic split. To treat a string as a list, split it explicitly
  (`parts=$(split $PATH :)`). Consistent with kaish's no-word-splitting stance: structure is
  always explicit.

- **Membership `in` is collection-only.** `[[ e in $list ]]` (element) and `[[ k in $record ]]`
  (key). A **string** RHS is an error — for substring tests use existing shell syntax (`=~`,
  glob `[[ $s == *sub* ]]`, or `case`). We're already extending the language; keeping substring
  on the sh muscle-memory path avoids overloading `in` and conflicting with `=~`.

- **`export` of a structured value is an error.** You cannot put a list/record in an OS env var;
  kaish will **not** silently JSON-serialize it (today `value_to_string` does — `kernel.rs:3504`;
  flip it to error). If you want JSON in the environment, serialize explicitly first. Surfacing
  this boundary loudly is the point — the structured data model is otherwise invisible at the
  process edge.

- **Commas optional in BOTH lists and records.** `[1 2 3]` ≡ `[1, 2, 3]`; `{a: 1, b: 2}` ≡
  `{a: 1 b: 2}`. Records were shown comma-separated and lists space-separated, which would make
  models hallucinate commas in lists (Gemini's catch); allowing optional commas in both removes
  the inconsistency, matches JSON/model priors, and is easier for static tools to validate.
  (Brushes the bare-comma lexer oddity in issues.md inside `[ ]` context — verify before
  implementing.)

- **Collections are heterogeneous.** It's JSON underneath (`Value::Json`): `[1 two true]` and
  mixed-type records are legal. `x=[]` is an empty list of no fixed element type; `push x 1`
  then `push x two` is fine.

- **Access form — universal `${…}`.** Bare `$xs` is the WHOLE value; ANY path/index access is
  braced: `${xs[0]}`, `${r.key}`, `${r.$k}`, `${xs[-1]}`, `${xs[0:2]}`, `${a.b[0]}`. Chosen over
  bare postfix access (`$xs[0]`/`$user.name`) for four reasons: (1) one form in *and* out of
  strings — no separate string-interpolation rule; (2) bash-consistent — `${arr[0]}` is how bash
  indexes, and bare `$arr[0]` isn't valid bash anyway; (3) it rides the `${…}` segment infra that
  already exists (`lexer.rs:2033`, MODERATE) instead of needing a new bare-postfix grammar (HARD);
  (4) dissolves the dot-vs-bracket question. Assignment *lvalues* stay bare (`user.email=…`,
  `tally.total=…`) — `${…}` is for reading. NOTE: this reverses the bare access models produced
  unprompted in testing, so re-stress-test that requiring `${roles.bob}` doesn't fight the
  JS/Python bare-`r.key` prior before sign-off (Teaching note #8).

- **List-splat in a literal — nest by default, explicit `...` spread.** A bare variable inside
  `[ ]` is ONE element (it nests — consistent with "a value is one value", keeps lists nestable,
  no implicit splitting). To flatten a list's elements into the new list, use the spread
  operator: `flat=[...$a ...$b]`, `more=[...$xs cyan]`, `ys=[0 ...$xs 4]`. In-place append stays
  `push` (the loop-accumulation tool); spread is for *building new* lists from existing ones
  (concat, copy-with-extra). This was the leading option among four (implicit-splat /
  implicit-nest / nest+spread / builtins-only) — the only one that preserves nesting AND forbids
  implicit splitting AND stays teachable. **Smoke-tested 2026-06-05** on both lite models: 12/12,
  perfect nest-vs-spread discrimination in both directions (`[$a $b]` for nest, `[...$a ...$b]`
  for flatten), unprompted nested indexing `${grid[1][0]}` correct. The `...` token's JS/Python
  priors make the choice essentially free for taught models. **Validator hint can be SOFT**
  (low-priority nudge, not a hard error — nesting is legal data): models only default to
  bare-`$xs`-as-splat when `...` is *not* shown, so the hint mainly backstops untaught/human use.
  Token note: confirm `...` / `..` don't clash with the range-parsing oddity logged in issues.md
  before implementing.

## Open decisions

Not yet resolved; flagging them so we decide deliberately.

- **jq vs native — discover after implementation.** With native traversal, `${user.name}` competes
  with `jq -r '.name'`. Deliberately NOT deciding now: the hypothesis is that a lot of jq for
  *simple* cases disappears once native is easy, but jq has heavy model training and stays the
  tool for raw external JSON. Re-evaluate with the feature in hand; then write the heuristic.

- **Quoting inside literals — lean strict.** Elements with spaces / specials
  (`["green apple" banana]`, `["[weird]" x]`) need explicit quotes. Lean toward strict rules and
  let the verify pass nudge models to correctness incrementally (even a lite model converges).
  Exact rules TBD.

- **shellcheck posture.** Arrays/records are a kaish *extension* beyond the `sh` subset that
  passes `shellcheck --enable=all`. Scripts using them are no longer plain-sh-clean by
  construction. Confirm acceptable (it is — explicit extensions, like floats and booleans). See
  issues.md P4 for the broader framing fix.

- **Record field deletion / list removal.** Not designed yet. Lean `unset user.email` (reuses the
  existing shell concept) over inventing `remove`. Out of scope for the first cut.

## Implementation notes (sketch)

Grounded in a 2026-06-05 lexer/parser/validator read (file:line refs are from that pass; verify
before relying on them).

- **Lexer/parser collisions to watch:**
  - `[ ... ]` list literal vs `[[ ... ]]` test vs POSIX `[` test command. The `[[` token is
    already distinct; a single `[` at value position (RHS of assignment / argument) must lex as a
    list literal, not the test command.
  - `{ ... }` record literal vs command block / brace group. Disambiguate by context: a `{` at
    *value* position (after `=`, in an argument, inside a literal) is a record; a `{` at
    *statement* position is a block.
  - **Path access already half-exists**: `${VAR}` interiors are split on `.`/`[` into
    `VarPath`/`VarSegment` segments (`lexer.rs:2033`), but index segments are filtered out
    (`parser.rs:108`) and `VarSegment` has only `Field` (`ast/types.rs:399`); `resolve_path`
    handles single-segment only (`interpreter/scope.rs:354`). To get `${user.tags[0]}`: add
    `VarSegment::Index`, stop filtering, implement nested traversal. (Whether bare `$VAR.foo`
    *outside* strings also gets postfix access is the open access-form decision.)
- **String interpolation** already stops `$VAR` at `.` (`parser.rs:627`), so no-auto-interpolation
  is current behavior; `${path}` is the interpolation form (rides the same `${}` segment infra).
- **`${#…}` length** is one spot (`kernel.rs:2802`, currently `value_to_string(value).len()`):
  type-dispatch on `Value::Json` (array len / object key count). Drop the `len` builtin.
- **`[[ in ]]` evaluation** routes through the existing async test path (`eval_test_async` in
  `kernel.rs`, see auto-memory `[[ ]] File Test Evaluation`) so it is VFS/backend-aware. Add
  `in` / `not in` as comparison operators that inspect the RHS `Value::Json` shape (array →
  element membership; object → key membership; **string RHS → error**). The RHS must accept a
  list/record **literal**, not just a `$var` — Haiku produced `[[ $a not in [dog] ]]`.
- **`for` head** already consumes a value/`.data`; extend the head grammar to accept a bare
  builtin invocation (`keys $r`) in addition to `$VAR` and `$(...)`. NOTE the collision Gemini
  flagged: a literal first word matching a builtin name (`for x in keys`) becomes quasi-reserved
  in the head — document the rule (known-builtin → call; else word list; quote to force literal).
- **`keys`/`values`** dispatch on `Value::Json` shape (record → key list / value list).
- **`push` validation**: validator tracks bound names but not types (`walker.rs:458`,
  `scope_tracker.rs`). Register `push`'s first arg as a var-target; undefined target → E-code
  (pre-runtime); defined-but-not-a-list → runtime error. Never silently create.
- **`export` of a structured value → error**: enforce at `kernel.rs:3504` (and test path
  `dispatch.rs:220`) before `value_to_string` serializes `Value::Json`. Today it silently
  JSON-stringifies (`eval.rs:514`); flip to error.

## Teaching notes for the final docs & instructions

Derived from the model experiments (2026-06-05, four models incl. fast/loose). These are the
things that must be done *carefully* when we write the real LANGUAGE.md / help / instruction
copy, plus what to re-test before sign-off. The collection surface is model-friendly **only when
taught a specific way** — get the examples wrong and even capable models fail.

1. **Show every operator inside its full control structure, never standalone.** Membership broke
   when the cheat sheet listed it as a bare `[[ k in $r ]]` line: a model wrote malformed
   `[[ … ]]` / `if then; do … fi`. The same model assembled `if [[ $c != x ]]; then …; fi`
   correctly, and an *example* that showed `if [[ apple in $prices ]]; then` produced perfect
   results. **Rule:** always document `in`/`not in` as `if [[ k in $r ]]; then … fi`, never as a
   standalone test line. A novel operator shown standalone reads as a complete statement.

2. **Demonstrate every access form explicitly — especially list indexing.** Example-only teaching
   that omitted indexing caused a model to invent `$(colors.1)` (dropped the `$`, added spurious
   `$()`). Docs must show, side by side, the braced forms: `${xs[0]}`, `${xs[-1]}`, `${xs[0:2]}`,
   `${r.k}`, `${r.$key}`, `${r.a.b}`, `${r.a[0]}`. Models reproduce exactly what's shown and guess
   (wrongly) at what isn't.

3. **Contrast expansion vs builtin-capture, with both shown.** Models overgeneralize the `$()`
   capture rule. Put them next to each other: access and length are *expansions* (NO `$()`) —
   `${xs[0]}`, `${r.k}`, `${#xs}`; a *builtin* used as a value NEEDS `$()` — `$(keys $r)`,
   `$(values $r)`. And `$()` is command-substitution ONLY — never wrap an access path in it.

4. **String-interpolated paths use `${path}` braces — SHOWN, not merely stated.** A rules sheet
   that *stated* the rule still produced unwrapped paths; an example that *showed* the braced form
   was copied. Every doc string embedding a path must use `${r.$k}` / `${user.name}` (NOT
   `$($r.$k)` — `$()` is execution only). Show bare `"$user.name"` (expands `$user`, leaves
   `.name` literal) as the WRONG form so models learn the contrast.

5. **Show both `push` and the `...` spread — their absence defaults to bare splat.** With neither
   shown, every model appended via `xs=[$xs new]`, which under our resolved rule *nests* — a
   silent surprise. Teach both idioms: `push name value` for in-place append, and `...$xs` for
   building a new flattened list. Show the nest-vs-spread contrast side by side (`[$a $b]` nests;
   `[...$a ...$b]` flattens) — when shown, models discriminated 12/12. The `push` bareword target
   (`push colors cyan`, `push picks $c`) was used correctly every time; lean on the `read`/`unset`
   analogy in prose ("`push` takes the variable name, like `read`").

6. **`push` is the only append idiom — never show a competing one.** In-place append is `push`
   (frozen); new-list-with-an-extra-element is `...` spread (`[...$xs v]`). The pure `append`
   builtin is dropped, so there's no `push`-vs-`append` choice for a model to fumble. Don't
   document two spellings for the same operation.

7. **Re-test with REAL kaish syntax only.** The first experiment round used curly-brace blocks
   kaish does not have. All future model tests must use sh-style `do/done` / `then/fi`, no-space
   assignment, and `push` for append — otherwise we're validating a language we don't
   ship. Confirmed-good under real blocks so far: bare for-head, nested loops, `[[ in ]]`
   (when shown inside `if…then`), `push`, no-space assignment, `!=` vs `in` distinction,
   `...` spread vs bare-nest, and nested indexing `${grid[1][0]}`.

8. **Pre-sign-off re-test — DONE 2026-06-05 (Haiku subagent; rules + example-driven), all passed.**
   Confirmed under the FINAL braced syntax: `${xs[0]}` / `${r.key}` / `${r.$k}` /
   `${servers.web[0]}` / `${users.alice.city}` access (the form we reversed from bare — **no**
   reliability loss, Haiku produced braced access flawlessly), `${#xs}` length, `${nums[0:2]}`
   slicing, `${path}` string interpolation, `r.key=value` field set, optional commas in lists
   (incl. mixed with spread: `[0, ...$nums, 3]`), `push` in a loop, and nest-vs-spread. 12/12
   rules, 7/7 example-driven. Only residual is the `!=` nit (note 11).

9. **Update the "sh subset / shellcheck" framing in the same PR** (see issues.md P4). The docs
   that introduce collections are the natural place to restate "inspired by sh/bash, informed by
   shellcheck" and that the validator — not shellcheck — owns collection correctness.

10. **Spaced-assignment error needs positive reinforcement.** Models context-switch to JSON/Python
    spacing and will write `x = [a, b]`. The SC1068-style validator error must show BOTH the
    problem AND the fix — e.g. *"kaish assignment takes no spaces around `=`; write `x=[a, b]`"* —
    so the verify pass corrects the model in one round instead of just rejecting.

11. **Show `!=` for scalar inequality.** Given no `!=` example, Haiku expressed "not equal to dog"
    as `[[ $a not in [dog] ]]` — a list-literal membership test. Semantically correct but
    roundabout; docs should show `[[ $x != val ]]` so models don't route every inequality through
    `not in`. This also means the `in` RHS must accept a list/record **literal**, not just a
    `$var` (see impl notes).

## Out of scope (first cut)

- Set/map algebra (union, intersection, merge).
- Comprehensions / map / filter as syntax (use `for` + jq).
- Typed schemas on records.
- Deletion ergonomics (`unset`/`remove`) — noted above, designed later.
