# Arrays & Hashes — Design Doc

Status: **proposal / design exploration** (not yet implemented)
Author: design notes from a 2026-06-05 session.
**Revised 2026-07-01** — brackets-only access, record iteration, comparison/unwrap semantics,
full lvalue rules; implementation notes re-grounded against HEAD `f92070e`. Superseded
decisions are kept below with annotations, since the evidence record is part of the method.
Related: [LANGUAGE.md](LANGUAGE.md), [issues.md](issues.md),
[designing-syntax-with-llms.md](designing-syntax-with-llms.md)

## Motivation

kaish already has a value model — `Value::Json`, structured `$()`, and a `for` loop that
iterates a builtin's `.data` element-wise. What it lacks is *surface syntax* for users to
**construct, index, and mutate** collections directly: array (list) and hash (record) literals,
element/field access, length, membership, and append.

The goal is to expose the collections kaish can already represent, with ergonomics that are
**predictable for AI agents** — the primary audience. That last constraint is not hand-wavy: we
ran an LLM stress test (below) and let it drive several decisions.

A longer-range hope: if native collections cover the common JSON tasks, `jq` may be able to
leave the core builtin set entirely (see Open decisions).

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

# RECORDS — string-keyed maps; colons may be spaced or unspaced ({port:8080} ≡ {port: 8080})
user={name: amy, role: maintainer, age: 40}
nested={tags: [a b c], meta: {active: true}}

# multi-line literals with trailing commas
services={
  web:    {port: 8080, replicas: 3, healthy: true},
  api:    {port: 9000, replicas: 2, healthy: false},
}

# ACCESS — brackets only, 0-indexed. Bare $xs is the WHOLE value (no word
# splitting); ANY access is a bracketed subscript inside ${…}.
${fruits[0]}             # apple
${user[name]}            # amy — a bareword subscript is a LITERAL key
${nested[tags][1]}       # b — chained subscripts
${nested[meta][active]}  # true
$fruits                  # whole list — ONE value
${fruits[-1]}            # cherry         (negative index)
${fruits[0:2]}           # [apple banana] (slice: end-exclusive; yields a list)
${user[$k]}              # dynamic key — a variable is always $-marked
${r["content-type"]}     # quoted key for anything that isn't a bareword
${services[web][port]}   # 8080

# THE ONE SUBSCRIPT RULE: in collection contexts a bareword is a literal key;
# variables are always $-marked. Same rule as record literals ({name: amy} —
# name is literal). Integers index lists; barewords/strings key records.
#
# There is NO dot access. ${user.name} is a loud error with the fix in the
# message ("use ${user[name]}"). Native access is bracket-shaped; jq keeps the
# dot-shaped path language for external JSON — the two don't blur.

# Same form inside strings — no special string rule.
echo "${user[name]} lives here"    # amy lives here

# Scalars stay scalars — no auto-coercion to a collection. $PATH is a string;
# ${PATH[0]} is an error. Want a list? split it explicitly:
parts=$(split $PATH :)

# ASSIGNMENT — the same bracket paths work as lvalues; no spaces around `=`.
user[email]=amy@example.com    # record key: insert or update
fruits[0]=kiwi                 # list index: in-place update (must be in bounds)
services[web][port]=9090       # nested path
tally={}
tally[total]=${#services}      # length is ${#…}, not a builtin
# No autovivification: every intermediate must already exist with the right
# shape. The ONLY thing a path-set may create is the final record key.
# Out-of-bounds list index is an error — push is how lists grow.

# OPS — keys/values are builtins; length is the param-expansion ${#…}.
echo ${#fruits}      # 3   (element count)
echo ${#user}        # 3   (key count — record)
keys $user           # [name role age]      (insertion order; capture with $())
values $user         # [amy maintainer 40]  (same order as keys)
push fruits date       # in-place append; takes the variable NAME (like read/unset)
new=[...$fruits date]  # build a NEW list with an extra element (spread)

# MEMBERSHIP — an operator inside [[ ]], NOT a command. Always shown inside a
# full test, never as a standalone [[ ]] line (see Teaching notes #1).
if [[ banana in $fruits ]]; then echo "have banana"; fi   # element in list
if [[ name in $user ]]; then echo "has name"; fi          # key in record
if [[ tmp not in $services ]]; then echo "no tmp"; fi     # absent
if [[ apple in $fruits && web in $services ]]; then echo both; fi

# ITERATION — always through a $() form. ANY bare `$var`/`${…}` in a for-head is
# an error (E012, no implicit word splitting) — including a subscript access,
# which is still a VarRef. keys/values work on ANY collection: a record → its
# keys/values; a list → its indices/elements.
for f in $(values $fruits); do echo $f; done              # list elements
for k in $(keys $user); do echo "$k = ${user[$k]}"; done  # record keys
for i in $(keys $fruits); do echo $i; done                # list indices [0,1,2,…]
for p in $(values ${nested[tags]}); do echo $p; done      # nested list, still via $()
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

for name in $(keys $services); do    # record → its keys ($()-only; bare $var is E012)
  cfg=${services[$name]}
  echo "$name: port=${cfg[port]} replicas=${cfg[replicas]}"

  if [[ ${cfg[healthy]} == false ]]; then   # typed bool compare (see unwrap rule)
    push to_restart $name            # in-place append, no discard trap
  fi
done

echo "restart count: ${#to_restart}"
for svc in $(values $to_restart); do
  echo "restarting $svc on port ${services[$svc][port]}"
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

3. **`for k in keys $r` (bare builtin in the for-head) is a GO.** *(evidence retained; decision
   **SUPERSEDED TWICE** — first 2026-07-01 (bare `for k in $record` iterates keys), then again
   **2026-07-02** (`$()`-only iteration; see "Collection iteration" under Resolved decisions). Net
   rule: NO bare for-head at all — `for k in $(keys $r)` / `for x in $(values $c)`. A bare-builtin
   head would silently change the meaning of currently-valid word-list syntax; a bare `$var` head
   collides with E012.)* Original finding: every model used the bare form correctly and naturally,
   plain and nested; **requiring `$(keys $r)` in the head was the most common error** — the very
   form now mandated, so the note-#8 panel re-test must measure convergence on `$(values $xs)`
   specifically, and the E012 error copy leads with it. (Reversal is additive/safe: relaxing to
   allow bare collection iteration later breaks zero scripts; the string case becomes a loud
   *runtime* error — kept as the designed v2 relaxation, see Open decisions.)

4. **Membership is `[[ key in $r ]]`, NOT `if has`.** This is the clearest result. A `has`
   *command* was unstable: fast models either wrapped it (`if $(has …)`) or garbled the args
   (`if has keys $inventory "bananas"`). Re-spelled as a `[[ ]]` operator, **both fast/loose
   models got all seven membership tasks correct** — key presence, `not in`, list elements,
   nested-list (`[[ 443 in ${servers[web]} ]]`), membership in a loop, and compound `&&`. `in`
   slots into the existing test-operator mental model (`==`, `-f`) reinforced by Python's `in`
   and `for x in`. It composes; a bare command does not.

5. **Length collapses onto `${#…}`; `keys`/`values` are builtins that accept records directly.**
   The single most error-prone construct in testing was *nested* capture: `len $(keys $r)`. Fast
   models collapsed it (`echo len keys $inventory`). Two fixes: (a) length is the existing
   param-expansion `${#xs}` extended to collections (list → element count, record → key count),
   not a `len` builtin — one length form, consistent with `${#NAME}` already in LANGUAGE.md;
   (b) `keys $r`/`values $r` accept records directly so you never nest. The error mode goes
   with it.

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

Decided during the design sessions (2026-06-05 and the 2026-07-01 revision); rationale kept
for the record.

- **Access form — brackets only, inside `${…}`.** *(Revised 2026-07-01; supersedes the earlier
  "universal `${…}` with dotted paths" decision.)* Bare `$xs` is the WHOLE value; every access
  is a bracketed subscript: `${xs[0]}`, `${r[key]}`, `${r["weird-key"]}`, `${r[$k]}`,
  `${xs[-1]}`, `${xs[0:2]}`, `${a[b][0]}`. There is **no dot access**. Subscript rule: a
  bareword is a **literal key** (bash associative-array prior; same as record-literal keys),
  a `$var` is dynamic, quotes cover non-bareword keys, integers index lists. Why brackets only:
  1. **Lists are brackets in every language anyway** — dots would be a records-only second form
     that exists to save two characters on identifier-shaped keys. One form, no dot-vs-bracket
     question, consistency over convenience.
  2. **Non-identifier keys are first-class** (`${r["content-type"]}`,
     `${r["app.kubernetes.io/name"]}`) instead of falling off a cliff into jq.
  3. **It dissolves `${user.$k}`** — the oddest spelling in the earlier draft — into
     `${user[$k]}`, which is the straight Python/JS prior.
  4. **Clean division from jq**: native access is bracket-shaped, jq stays dot-shaped. With dots
     in both, models produce half-remembered hybrids; with brackets the surfaces don't blur.
  5. **It kills a real hazard**: the lexer's `Ident` regex admits `.` (`lexer.rs:596`), so
     `user.email=x` *parses today* as a flat variable literally named `user.email` — which is
     then unreadable (`${user.email}` rejects multi-segment paths). If dots meant paths, two
     adjacent spellings would silently diverge (flat write vs path write). Brackets-only makes
     every dotted LHS a loud error instead (see lvalue rules).
  Costs, accepted honestly: deep paths are noisier than dotted (`${services[web][port]}`), and
  dot-leakage from jq/JS priors will happen — it's a targeted parse error with the fix in the
  message, converging in one verify round, never a silent misread. One asymmetry to document
  side by side: arithmetic `$((x + 1))` evaluates bare names as variables (separate mini-parser,
  `arithmetic.rs:285`, inherited from bash) while bracket subscripts treat barewords as literal
  keys. Assignment *lvalues* use the same bracket paths, bare (no `${…}`) — `${…}` is for
  reading. **Untested surface — panel re-test required before sign-off** (Teaching note #8).

- **Collection iteration is `$()`-only; a bare `$var` in a for-head stays E012.** *(REVISED
  2026-07-02 — supersedes the "`for k in $record` iterates keys" decision of 2026-07-01.)* The
  earlier plan let a bare record/list in the for-head iterate (keys / elements). It collided with
  the validator's E012 (`ForLoopScalarVar`, `walker.rs:221`): E012 is a *static* hard error on any
  bare `$var`/`${…}` in a for-head (it can't know the runtime type), and it exists to catch the
  no-word-splitting mistake `for x in $stringvar`. Relaxing it for collections would either
  reintroduce that footgun for strings or require a static type-guess. **Decision: don't relax
  E012.** All collection iteration goes through a `$()` form, which E012 never touches (a
  CommandSubst returns typed `.data`). This keeps E012's protection exactly where the mistake is,
  makes every iteration unambiguous, and removes the whole for-head tangle from the resolver work
  (#6). Note a subscript access `${nested[tags]}` is *also* a bare `VarRef` → also E012 → also
  wrapped in `$()`.

  **`keys`/`values` are the iteration workhorses, and work on ANY collection** *(list-argument
  question resolved 2026-07-02)*: a **record** → its keys / its values (insertion order,
  pairwise-aligned); a **list** → its **indices** `[0,1,…,n-1]` (`keys`) / its **elements**
  (`values`) — the jq model. So: `for x in $(values $xs)` (list elements), `for i in $(keys $xs)`
  (list indices), `for k in $(keys $r)` (record keys), `for v in $(values $r)` (record values).
  A non-collection argument (scalar / bytes / unset) is a loud error. This also composes with
  membership: `[[ 1 in $(keys $xs) ]]` is an in-bounds check, because `$(keys $xs)` yields a real
  typed list.
  - *(No extra validator rule needed for the bare-builtin head `for k in keys $user`: that head
    contains a bare `$user` (a VarRef), so E012 already fires on it and its copy now leads with
    `for k in $(keys $user)`. The only bareword head that slips through is one with no `$var` at
    all — `for w in keys values` — which is a plain word list iterating the literal words "keys"
    and "values", an obvious nonsense the model won't write; not worth a rule.)*

- **Access unwraps JSON scalars at the boundary.** *(Added 2026-07-01.)* A subscript access that
  lands on a JSON scalar yields the native `Value`: `Json(Bool)`→`Bool`, `Json(Number)`→`Int`
  (integral) or `Float`, `Json(String)`→`String`, `Json(Null)`→`Null`. Only collections stay
  `Value::Json`. Consequences: `[[ ${cfg[healthy]} == false ]]` is a **typed** bool comparison
  (today `Json(Bool)` vs `Bool` falls through to `values_equal`'s stringify-both-sides fallback,
  `eval.rs:747` — correct by accident; make it correct by design), and `$(( ${cfg[port]} + 1 ))`
  just works. One model, no "JSON-flavored scalars" second species.

- **Comparison semantics for collections.** *(Added 2026-07-01; collection-vs-scalar landed
  2026-07-02.)* `==`/`!=` between two collections is **structural JSON equality** (records
  order-insensitive — `serde_json::Map` equality; lists order-sensitive). A collection compared
  to a scalar is a **loud error** with a hint ("use `in` for membership; jq for structural
  queries") — `[[ $list == banana ]]` being silently false is exactly the trap `in` exists to
  close. Ordering operators (`<`, `-lt`, …) on collections are errors. **Implemented**:
  `values_equal` (`eval.rs`) now returns `EvalResult<bool>` and yields `EvalError::Unsupported`
  for the collection-vs-scalar case; it surfaces as a loud `Err` from `kernel.execute()`, the
  same LOUD contract as the `=~` regex-error fix (a JSON *scalar* is unwrapped at the value
  boundary, so only Array/Object reach the guard). Pulled forward ahead of `in`/membership
  because read-access already lets collections reach `[[ ]]`.

- **Assignment lvalues — bracket paths, loud errors, no autovivification.** *(Added
  2026-07-01.)* `name[seg][seg…]=value` writes into a collection in place:
  - The **final segment** may insert a new record key (`user[email]=x` on a record without
    `email`) or update an existing list index. Everything above it must already exist with the
    right shape — **no autovivification** (the Perl trap): `a[b][c]=x` with no `b` is an error;
    init with `a={}` / `a[b]={}` first.
  - **List index set is in-bounds update only**; out-of-bounds is an error — `push` is the grow
    tool. Negative indices work (read symmetry). **No slice lvalues.**
  - **Undefined root, scalar root, or wrong-shaped intermediate → error** (undefined root is a
    validator E-code, same rule as `push`; shape errors are runtime — the validator tracks
    names, not types).
  - **Path lvalues are illegal in env-prefix position** (`user[email]=x cmd`) — structured
    values can't cross the process boundary anyway (see export decision).
  - **Assignment LHS names tighten to POSIX names** (`[A-Za-z_][A-Za-z0-9_]*`). Today the
    shared `Ident` token admits `.`/`@`/`-` (needed for command names like `some-tool`), so
    `user.email=x` and `my-var=5` parse as flat assignments; enforce the restriction in the
    assignment/env-prefix parser or validator — NOT by changing the `Ident` regex — with the
    error suggesting brackets (`user[email]=x`). Breaking in the letter, not in practice: a
    flat dotted variable is unreadable today (only observable as a child env var via the
    prefix form).

- **Assignment spacing — no spaces, everywhere** (`fruits=[...]`, `tally[total]=$(...)`,
  `user[email]=amy@example.com`). Three reasons converge:
  (1) **consistency** — kaish scalar assignment is already no-space (`NAME="value"`); a split
  grammar (no-space scalar / spaced collection) would surprise and would break the habit on
  scalars;
  (2) **shellcheck ethos** — spaced `=` violates SC1068 ("Don't put spaces around `=`"), against
  the "passes `shellcheck --enable=all`" identity; in bash the spaced forms don't even assign
  (`x = 5` runs command `x`; `x= 5` runs `5` with `x=''`);
  (3) **model priors** — models' trained default *is* no-space shell assignment, so this is the
  more robust choice, not a concession (the experiment's spaced form worked, but cut against
  their grain).

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
  (`walker.rs:92-97`, `scope_tracker.rs:81`), so undefined-target is catchable as an E-code
  **ahead of runtime**; a target that exists but isn't a list is a **runtime** error. Both crash —
  no silent corruption. **`push` accepts the same bracket paths as lvalues**
  (`push services[web][tags] item`) — *revised 2026-07-01 after review pushback*: top-level-only
  forced the hostile spread-assignment workaround for nested appends
  (`a[b]=[...${a[b]} new]`) and contradicted one-append-idiom. The path resolves under the
  lvalue rules (root bound — E-code; intermediates must exist; leaf must be a list — runtime
  error; never silent-create); parsing rides the same lexer rework that stops fusing `[`-runs.

- **Length is `${#…}`, not a `len` builtin.** `${#xs}` returns element count for a list and key
  count for a record — the existing `${#NAME}` param-expansion (LANGUAGE.md) extended to
  collections. One length form, no nested `len $(keys $r)` trap. Strings keep today's behavior.
  *(Correction 2026-07-01: this is no longer "one spot" — there are now duplicated call sites in
  `kernel.rs` (`Expr::VarLength` + in-string), `eval.rs` (sync), and `scheduler/pipeline.rs`
  (reduced sync). Consolidate before extending.)* **Partly landed 2026-07-02**: all sites now
  route length through the shared `value_length` helper (the `scheduler/pipeline.rs` sync path
  previously returned the *string* byte-length — `${#xs}` on `[1,2,3]` gave `7`, not `3`), and
  `value_length` counts `Value::Bytes` by byte length rather than the placeholder-string length.

- **Length/default on a *subscripted path* — loud placeholder now, path-aware with the resolver.**
  *(Spec gap the 2026-07-02 review surfaced; resolved as "loud, not silent". Default semantics
  decided 2026-07-02.)* `${#u[tags]}` and `${cfg[port]:-8080}` were never given semantics, and the
  code fell into silent-wrong-output: `${#u[tags]}` printed `0` (bare-name lookup of the literal
  `"u[tags]"`) and `${cfg[port]:-8080}` **always** returned the default (the `:-` scanner + a
  literal `scope.get`). Root cause: the `VarLength(String)` / `VarWithDefault{name:String}` AST
  nodes carry a bare name and never reach the path resolver. **Interim (landed 2026-07-02):** the
  two `:-` scanners are now bracket-aware so a genuine slice like `${xs[0:-1]}` (negative end) is
  no longer misread as a default; a subscripted name in either form is a **loud "bind it first"
  error** (`x=${u[tags]}; ${#x}`), never the silent wrong answer — until the nodes carry a
  `VarPath` and resolve through the shared read resolver (folds into the bind/walk extraction; see
  Implementation notes).

  **Decided — `${path:-default}` is lenient on *absence*, loud on *misuse*.** When path support
  lands, `${cfg[port]:-8080}` returns the value if the key is present and non-empty, else the
  default — for an **unset root, a missing key, an out-of-bounds index, or an empty value**. The
  `:-` operator *is* the absence affordance (matching every model's bash prior — `${x:-d}` has
  always handled absence), so erroring on a missing key would defeat its purpose. Strictness stays
  available *per expression* by not writing `:-`: bare `${cfg[port]}` is loud-on-absence (already
  shipped), so the call site picks assert-vs-default by whether it types `:-`. **But `:-` does not
  suppress a *shape* error** — a string key on a list, an integer index on a record, or
  subscripting a scalar is "you're accessing it wrong," not "it's absent," and stays loud. This
  needs the resolver to classify its failure as **absence vs shape**: today `PathError` lumps
  missing-key and shape errors into one `Invalid` variant, so splitting them is a #6 resolver job
  (`Bind` tags the error class; `:-` catches only absence). Complementary tools, not substitutes:
  `${r[k]}` asserts (loud), `${r[k]:-d}` defaults (lenient on absence), `[[ k in $r ]]` is the
  boolean check for branching (native form coming with membership; `jq 'has("k")'` works today).
  **No global `set -o strict` for now** — the `:-`-free form already is the strict form; a mode
  that makes even `:-` shout on missing-key is a small future bolt-on if a real need appears.

- **String path interpolation is `${path}`; no auto-interpolation.** Inside a string, brace the
  subscripted path to bound it — `"${user[name]}"`, `"${user[tags][0]}"`, `"${user[$k]}"` —
  exactly the `$VAR` vs `${VAR}` rule already in LANGUAGE.md. Bare `"$user.name"` expands `$user`
  only and leaves `.name` as literal text; this is *current* lexer behavior (`parser.rs:653`
  stops `$VAR` at `.`), not a new rule. `$()` is **not** overloaded for access — it stays
  command-substitution only (this also kills the "models wrap everything in `$()`" risk Gemini
  flagged). The `${…}` interior already splits dotted and bracketed segments
  (`lexer.rs:2178`), so this rides existing machinery.

- **Value semantics — copy-on-assign.** `b=$a` copies the value; lists/records are never shared
  references. Reference aliasing would be catastrophic for kaish specifically: `Kernel::fork` +
  scatter/gather + background jobs would leak mutable state across workers. Copy matches JSON
  intuitions and shell's value model. (Path-set is clone-root → mutate → write back; there is no
  in-place mutation of stored `Value`s anywhere today, and that stays true observationally.)

- **Scalars never auto-coerce to collections.** `$PATH` is a string; `${PATH[0]}` on a scalar is
  an error, not a magic split. To treat a string as a list, split it explicitly
  (`parts=$(split $PATH :)` — the `split` builtin already exists). Consistent with kaish's
  no-word-splitting stance: structure is always explicit.

- **Membership `in` is collection-only.** `[[ e in $list ]]` (element) and `[[ k in $record ]]`
  (key). A **string** RHS is an error — for substring tests use existing shell syntax (`=~`,
  glob `[[ $s == *sub* ]]`, or `case`). We're already extending the language; keeping substring
  on the sh muscle-memory path avoids overloading `in` and conflicting with `=~`.

- **`export` of a structured value is an error.** *(Landed 2026-07-02.)* You cannot put a
  list/record in an OS env var; kaish will **not** silently JSON-serialize it. Both external-spawn
  sites (`kernel.rs::try_execute_external` and its synced test-only twin
  `dispatch.rs::try_external`) now call `structured_export_error` before `cmd.env(...)` and refuse
  loudly, naming the variable and pointing at `export CFG=$(tojson $CFG)`. If you want JSON in the
  environment, serialize explicitly first. Surfacing this boundary loudly is the point — the
  structured data model is otherwise invisible at the process edge. *(`export.rs` display
  formatting of a structured value at declaration time is untouched — the boundary that matters is
  the process edge, and that's where the guard sits.)*

- **Semantics settled in the 2026-07-02 post-fable-review pass** *(feed #6 / follow-ups; recorded
  so they aren't decided by accident inside the implementation):*
  - **(A) `${path:-default}` triggers on absence + emptiness, not on falsy values.** The default
    fires for: unset root, missing key, out-of-bounds index, JSON `null`, and an empty **string**
    (bash `${x:-d}` parity). It does **not** fire for `false`, `0`, an empty list `[]`, or an empty
    record `{}` — those are present values, and Python-style truthiness leaking into a shell would
    be a silent-wrong factory. (`Absence`/`UndefinedRoot` PathError classes + a post-resolution
    empty-string/null check; `Shape` never defaults — see the three-variant split.)
  - **(B) Inside `$(( … ))`, a bareword subscript index is a VARIABLE, not a literal key.**
    `$(( xs[i] + 1 ))` reads variable `i` (bash arithmetic prior; inside arithmetic everything is a
    numeric expression). This is the one place the same spelling diverges by context: `${xs[i]}`
    (interpolation) = literal key `"i"`; `$(( xs[i] ))` (arithmetic) = variable `i`. Both index by
    `i` when written `${xs[$i]}` / `$(( xs[i] ))`. `Bind` takes a context flag; docs show the pair
    side by side.
  - **(C) `[[ e in $coll ]]` element equality = the same rule as `==`.** Implemented (not "by
    accident"): `element_matches` (`eval.rs`) reuses `values_equal` for scalars (so `443` matches
    JSON number 443, `1` matches index 1 via the numeric/string coercion), treats a nested
    collection element as "not a match" (never an abort), and two collections structurally. Record
    membership stringifies the needle (keys are strings). Pinned with tests.
  - **(D) Collection at the boundary — display renders, the process edge refuses.** Display &
    string interpolation (`echo $c`, `"x: $c"`) render **compact JSON** (pinned). A bare collection
    **value** reaching an **external command's argv**, a **redirect target**, or an **env export**
    is a **loud error** — require `$(tojson $c)`, so the serialization reads like what it does and
    stays reversible (auto-expand could be added later but never retracted). Seam: `curl -d "$c"`
    interpolates to a JSON *string* before argv, so it passes as a string; only the bare
    `curl -d $c` (a live collection value) errors. Implement at external-arg resolution (a resolved
    argv `Value::Json(Array|Object)` → error), mirroring the export guard.
  - **(E) Scalar test operators on a collection operand are loud `Shape` errors** — `-z`, `-n`,
    `=~`, `!~`, `-eq`/`-gt`/`-lt`/…, and ordering `<`/`>`. Decided as a matrix so no operator falls
    to a stringify path one at a time. (`==`/`!=` already loud; membership `in` is the *only* op
    that takes a collection RHS.)
  - **(F) Ship a shape guard.** *(Resolved — shipped.)* `typeof $x` (→
    `list`/`record`/`string`/`number`/`bool`/`null`/`bytes`) and `[[ -list $x ]]` /
    `[[ -record $x ]]`. The antidote to the keys-on-list footgun and the
    API-shape-variance trap (Teaching note #12). `typeof` is a pure-data builtin
    (`.data` + text out); the two test operators evaluate the operand's value
    (like `-z`/`-n`, not a path stat like `-f`/`-d`). A defined-but-wrong-shaped
    value is false; a bare unset `$var` is an undefined-variable error (like
    `-z`/`-n`) so a typo isn't silently false. Use them bare.

- **Commas optional in BOTH lists and records.** `[1 2 3]` ≡ `[1, 2, 3]`; `{a: 1, b: 2}` ≡
  `{a: 1 b: 2}`. Records were shown comma-separated and lists space-separated, which would make
  models hallucinate commas in lists (Gemini's catch); allowing optional commas in both removes
  the inconsistency, matches JSON/model priors, and is easier for static tools to validate.
  *(The "bare-comma lexer oddity" this section previously cited is not in issues.md — the entry
  no longer exists. `Token::Comma` is a plain token (`lexer.rs:356`); no known oddity remains.)*

- **Strict quoting inside literals.** *(Resolved 2026-07-01; was open.)* A literal element or
  record value is exactly **one word or one quoted string**: `["green apple" banana]`,
  `{msg: "hello world"}`. Multi-word bareword values are a parse error (`{msg: hello world}` —
  error, with the quoted fix in the message), never silently split or joined. Same for keys:
  bareword or quoted string. The verify pass converges models on this in one round.

- **Collections are heterogeneous.** It's JSON underneath (`Value::Json`): `[1 two true]` and
  mixed-type records are legal. `x=[]` is an empty list of no fixed element type; `push x 1`
  then `push x two` is fine.

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
  *(Token note, corrected 2026-07-01: the "range-parsing oddity" previously cited in issues.md
  does not exist there. The lexer has no `..`/`...` handling at all — `...` is a new token,
  meaningful only inside `[ ]` value context.)*

## Open decisions

Not yet resolved; flagging them so we decide deliberately.

- **jq vs native — and whether jq can leave the core.** With native traversal, `${user[name]}`
  competes with `jq -r '.name'` for simple cases. The division is at least clean now: native is
  bracket-shaped and operates on values already in the shell; jq is dot-shaped and owns complex
  queries. The longer-range possibility (2026-07-01): if native collections + `for`/`if`/`push`
  cover the common tasks (the worked example *is* `map`+`select`), **jq may drop out of the core
  builtin set** into an external plugin/embedder tool. Hard prerequisite: without jq, kaish
  needs a JSON **ingress/egress** path — today jq is the only bridge from external JSON *text*
  (curl output, config files) into `Value::Json` and back. That means small parse/serialize
  builtins (working names `fromjson`/`tojson`, jq-consistent priors) designed alongside or soon
  after the first cut. Deliberately not deciding now: re-evaluate with the feature in hand,
  measure how much jq usage remains, then write the heuristic — and only then consider the
  plugin split (jq is heavily model-trained; removal must not strand agents).

- **shellcheck posture.** Arrays/records are a kaish *extension* beyond the `sh` subset that
  passes `shellcheck --enable=all`. Scripts using them are no longer plain-sh-clean by
  construction. Confirm acceptable (it is — explicit extensions, like floats and booleans). See
  issues.md P4 for the broader framing fix.

- **Record field deletion / list removal.** Not designed yet. Lean `unset user[email]` (reuses
  the existing shell concept, same bracket-path lvalue) over inventing `remove`. Out of scope
  for the first cut.

### fromjson / tojson (SHIPPED 2026-06 — this section is now historical design record)

> `fromjson`/`tojson` landed as builtins (present in every capability build); the sketch below
> is retained as design rationale. The export-error hint `export CFG=$(tojson $CFG)` points at a
> real, shipped builtin.


The JSON ingress/egress pair, named after jq's own functions (strong model prior). Both are
pure data transforms — no OS, no VFS — so they belong in every capability build, including
read-only embedders. Amy's call: prototype these EARLY, ahead of the collections grammar —
they exercise the whole value-model plumbing (structured `$()`, `.data`, assignment) without
touching the lexer/parser, de-risk the boundary semantics and error copy before the big PR,
and give the export-error decision its "serialize explicitly first" story from day one.

```sh
cfg=$(curl -s api/config | fromjson)     # stdin, or one positional string arg
v=$(fromjson "$text")
export CFG_JSON=$(tojson $cfg)           # the export-error escape hatch
tojson --pretty $cfg | tee config.json
```

- **`fromjson`**: whole input is exactly ONE JSON document; empty input or trailing garbage
  is a loud error with line:col (never a silent null). Result lands in `.data` through the
  **same unwrap law as access** (scalars → native `Value`, collections stay `Json`) — which
  `json_to_value` (kaish-types `result.rs`) already implements. One conversion law, two doors.
  Predictability pitch vs today's `$(... | jq .)`: jq's `.data` array-wraps multi-value
  output; `fromjson` is one-doc-one-value. NDJSON (`--lines` → list) is a later flag, not
  first cut.
- **HAZARD — bytes-envelope sniffing**: `json_to_value` auto-decodes objects matching the
  base64 bytes-envelope shape into `Value::Bytes`. External JSON that happens to match the
  envelope must NOT silently become bytes — `fromjson` needs the envelope-free conversion
  path. (Access traversal should use the envelope-free path too.)
- **`tojson`**: one `Value` positional (read off `args.positional` per the Value-typed rule,
  not the clap sink); no stdin (stdin is already text). Compact by default, `--pretty` for
  files/humans. Scalars serialize per JSON (`tojson hello` → `"hello"`). `Value::Bytes` is a
  loud error first cut ("bytes are not JSON — use base64").
- **Roundtrip law, test-pinned**: `fromjson "$(tojson $x)"` ≡ `$x` structurally — and with
  `preserve_order` on, exactly (key order survives).

## Implementation notes (sketch)

Re-grounded 2026-07-01 against HEAD `f92070e` (all file:line refs current as of that pass;
paths relative to `crates/kaish-kernel/src`).

- **Glob-merge is the big lexer collision — including one the first draft missed.**
  Space-separated literals are safe: the glob-run merger only fuses **span-adjacent** tokens
  (`merge_glob_adjacent`, `lexer.rs:1895-1958`), so `[a b c]` never merges. But *glued*
  bracket runs with a matched pair DO merge into a single `Token::GlobWord`: **`[]`, `[dog]`,
  `[1]` — i.e. empty and single-element list literals — currently lex as glob words**, and
  `x=[dog]` parses *today* as an assignment of a glob expansion (`primary_expr`'s glob branch).
  Proposed rule: at **value position** (assignment RHS, literal element, `in`-RHS), a
  `[`-leading token sequence is ALWAYS a list literal — never a glob. Assigning a `[`-leading
  glob becomes a loud parse error pointing at `$(glob '[0-9]*.log')`. This is a small breaking
  change to a dark corner (bare `[`-leading glob *assignment*); command-argument globs
  (`ls [dog]*`, `foo[0-9]`) are untouched. Same care applies to lvalues: `fruits[0]=kiwi` lexes
  today as `GlobWord("fruits[0]") Eq Ident` — a hard parse error (`GlobWord` is illegal at
  statement start) — so the lvalue grammar needs the merger suppressed (or the GlobWord
  re-interpreted) when the run is immediately followed by `=`.
  *Mechanism (2026-07-01 review, gemini):* the merge pass is context-free and runs before the
  parser, so "reinterpret at value position" can't be done purely in the parser against fused
  tokens — the workable fix is to **stop fusing `[`-leading runs in the lexer** (remove
  brackets from the mergeable set or intercept them in the pass) and let the *argv* grammar
  assemble glob patterns from primitive bracket tokens. Whitespace-sensitivity is otherwise a
  trap: `[a]` would parse differently from `[a b]`.

- **The value/argv grammar split is the load-bearing wall** *(2026-07-01 review, gemini —
  confirmed against parser.rs)*: `primary_expr_parser` currently serves BOTH assignment RHS
  and command arguments, so naively adding literals there would make `ls [dog]` pass a JSON
  array and `cmd {a,b}` a parse error. The grammar must bifurcate: a **value expression**
  grammar (collections included) for assignment RHS, literal interiors, `in`-RHS, and `push`
  arguments; an **argv** grammar (unchanged: globs, brace-expansion words, `find`'s `{}`
  placeholder) for command arguments. Collections never appear raw in argv — argv splat stays
  out of scope — and `{a,b}`/`{}` at argv position are untouched, which also kills the
  "backtrack a failed record literal to a string" idea (a silent fallback; against directives).

- **Newlines inside literals**: kaish statements are newline-terminated, and the expression
  grammar does not consume `Token::Newline` — so the multi-line record in the worked example
  would today terminate the assignment at the first line break. The list/record literal
  parsers must consume newlines (and the optional commas) between elements while a literal is
  open. Statement termination resumes at the closing bracket/brace.

- **Unspaced record colons collide with `merge_colon_adjacent`** *(confirmed:
  `lexer.rs:1717`, composed unconditionally at `lexer.rs:2093`)*: the pass fuses span-adjacent
  ident/colon runs (for `host:8080`-style words), so `{port:8080}` — which JSON-prior models
  WILL write — lexes as `LBrace Ident("port:8080") RBrace` and the record parser never sees a
  `Colon`. Same pass needs checking for glued slices (`${xs[0:2]}` — `0:2` inside `${…}` rides
  `parse_var_ref`, so likely safe, but verify). **Decided 2026-07-01: ACCEPT unspaced** —
  `{port:8080}` ≡ `{port: 8080}`, via a colon-fusion exemption inside open literal context
  (not ident-splitting at the parser — brittle). Erroring on the single most common JSON
  spelling would burn a verify round on nearly every script for no safety gain; there is no
  ambiguity to protect.

- **Quoted keys inside interpolated strings break the string lexer** *(confirmed:
  `lexer.rs:489` — the double-quote regex stops at the first unescaped `"`)*:
  `echo "${r["weird key"]}"` shatters the token stream today. **Decided 2026-07-01: quoted
  keys are not allowed inside a double-quoted string** — a loud error that suggests
  assign-first (`k="weird key"; echo "${r[$k]}"`). Rewriting the string lexer to balance
  `${…}` interiors stays available later if the panel shows models reaching for the nested
  form. Verify whether bare `${r["weird key"]}` *outside* strings survives `lex_varref` too.
- **`[[ ]]` interplay**: `[[` is deliberately **not** a distinct token — it's two consecutive
  `Token::LBracket`, chosen *specifically* "to avoid conflicts with nested array syntax"
  (`parser.rs:1913-1920`). Forward-compatible with list literals by design. There is no POSIX
  `[` test command in kaish at all (`command_parser`'s name choice, `parser.rs:1417-1425`), so
  that collision from the first draft is moot. Watch the glued single-element case inside tests
  (`[[ $a in [dog] ]]` — `[dog]` merges to a GlobWord; the `in`-RHS grammar must handle it per
  the value-position rule above).
- **`{ … }` record literal vs existing braces**: `{` is currently only consumed by function
  bodies (`parser.rs:1063-1111`) and glob brace-expansion `{a,b,c}` (comma-separated, no colon,
  `parser.rs:1290-1344`, shared with case patterns). A `{` at **value** position is free space
  for record literals; statement-position `{` and glob/case `{a,b}` are untouched.
- **Path access machinery is half-built on the read side**: the `${…}` interior already splits
  dotted AND bracketed segments (`parse_var_ref`, `lexer.rs:2178-2231` — `${VAR[0][k]}` →
  `["VAR","[0]","[k]"]`), but the parser filters index segments out (`parse_varpath`,
  `parser.rs:141`), `VarSegment` has only `Field` (`ast/types.rs:407-411`), and `resolve_path`
  rejects multi-segment for everything but `$?` (`scope.rs:356-375`). Work: add
  `VarSegment::Index`/`Key`/`Dynamic` (int / literal / `$var`), stop filtering, implement nested
  traversal + **scalar unwrap at the boundary**, slices, negative indices. Brackets-only means
  dotted segments in `${…}` become the targeted error path (with the bracket fix in the
  message), not a resolution path.
- **Lvalues (the phase after #6)** touch five layers, but the navigation core is `walk_write` over
  `&mut serde_json::Value` sharing #6's `resolve_step` verbatim (that sharing is the whole point of
  #6 — read/write classification can't drift): (1) lexer — the glob-merge suppression before `=`
  above; (2) AST — `Assignment`
  grows from `name: String` to an lvalue (root + segments); (3) parser — an `lvalue_parser`
  wired into `assignment_parser` (`parser.rs:1028-1058`) and `env_prefix_assign`
  (`parser.rs:952-970`, which must *reject* path lvalues); (4) interpreter — clone root, navigate
  via `as_object_mut`/`as_array_mut`, set leaf, `scope.set_global` writeback
  (`Stmt::Assignment` eval is `kernel.rs:2053-2069`; there is no in-place mutation precedent —
  assignment is whole-value replace, keep it that way); (5) validator — bind/check the **root**
  name, not the composite string (`validate_assignment`, `walker.rs:92-97`). Estimated cost:
  roughly a third on top of the read-side traversal work, which is needed regardless.
- **POSIX-name tightening for assignment LHS** happens in the assignment/env-prefix parser or
  validator — NOT the lexer: the `Ident` regex (`lexer.rs:596`, `[a-zA-Z_][a-zA-Z0-9_.@-]*`) is
  shared with command names (`some-tool`) and must keep accepting those.
- **`${#…}` length**: consolidated 2026-07-02 onto the shared `value_length` helper across all
  four sites (`kernel.rs` `Expr::VarLength` + in-string, `eval.rs` sync, `scheduler/pipeline.rs`
  reduced sync — the last of which was still `value_to_string(value).len()`). The remaining length
  work is *nested* length (`${#u[tags]}`): the `VarLength(String)` node carries a bare name, so it
  can't reach a subscript — it's a loud "bind first" error until the node carries a `VarPath` (do
  it with the resolver extraction below).
- **`[[ in ]]`** — LANDED 2026-07-02. Routes through the async test path (`eval_test_async` in
  `kernel.rs`) so it is VFS/backend-aware; `TestExpr` gained `In`/`NotIn`; RHS shape-dispatches on
  `Value::Json` (array → element membership; object → key membership; string RHS → loud error).
  Remaining: the RHS accepts a list/record **literal**, not just a
  `$var` — Haiku produced `[[ $a not in [dog] ]]` (mind the glued-GlobWord case).
- **Typed comparison**: DONE 2026-07-02 — `values_equal` (`eval.rs`) now returns
  `EvalResult<bool>`, has the structural-equality arm for collection/collection (via
  `serde_json::Value` `PartialEq`, records order-insensitive under `preserve_order`), and the
  loud `EvalError::Unsupported` for collection/scalar. Boundary unwrap keeps scalar cases native.
- **`for` over collections**: NO for-loop change — iteration is `$()`-only (see the revised
  "Collection iteration" decision). The for-loop keeps iterating a CommandSubst's typed `.data`
  array; a bare `$var`/`${…}` in a for-head stays E012. `keys`/`values` produce the iterable.
- **`keys`/`values`** (landing 2026-07-02) dispatch on `Value::Json` shape: **record** → key list /
  value list; **list** → index list `[0,1,…]` / element list — the jq model. Insertion order,
  keys/values pairwise-aligned. Deterministic order: workspace `serde_json` has `preserve_order`
  on (root `Cargo.toml:42`), matching jq. Non-collection arg → loud error.
- **`push` validation**: no builtin registers a var-target in the validator today (`read`/`unset`
  don't either — `bind` is only called for assignments, for-vars, and function params). New
  pattern following `ScopeTracker::bind` (`scope_tracker.rs:81`): register `push`'s first arg as
  a var-target; undefined target → E-code; defined-but-not-a-list → runtime error.
- **`export` structured error**: DONE 2026-07-02 — `structured_export_error` guards before
  `cmd.env(...)` at both spawn sites (`kernel.rs::try_execute_external`, test-only twin
  `dispatch.rs::try_external` — the hermetic two-spawn-site rule). `export.rs` declaration-time
  display was deliberately left as-is (the process edge is the boundary that matters).
### #6 — the shared per-hop resolver (design settled 2026-07-02; read-side SHIPPED 2026-07-02)

*The single most important thing before the literal/lvalue grammar. This is a read-side refactor
that also unblocks the deferred `${#path}` / `${path:-default}` forms and arithmetic subscripts;
lvalue **writes** are the phase after, but the resolver is shaped so they slot in.*

**SHIPPED (read side, branch `feat/collections-resolver`):** `resolve_step` + the `Cow` borrow
walk (kills the O(n²) root clone), the `PathError` → `UndefinedRoot`/`Absence`/`Shape` split,
path-aware `${#path}` / `${path:-default}` with decision-A semantics (`VarLength`/`VarWithDefault`
now carry a `VarPath`; the lexer `VarLength` regex widened for bracket subscripts), arithmetic
decision B (`$(( xs[i] ))` reads variable `i`), and full-path-prefix error messages. The two
`subscripted_*_error` "bind first" placeholders are deleted. **NOT shipped:** lvalue writes
(`a[b]=x` grammar + `walk_write` + validator root-binding) — the next phase, sharing `resolve_step`.

**The problem.** Today `Scope::apply_segment` (`scope.rs:519`) fuses three jobs per hop — classify
a segment against the container (index-vs-key, negative-index normalization, every loud message),
walk one hop, and unwrap the child via `json_to_value_no_envelope(subtree.clone())` (the clone
makes `for k in $(keys $u); do … ${u[$k]}` O(n²)). The lvalue navigator needs `&mut` serde-tree
navigation and can't reuse that walker; built separately it would duplicate the classification
logic under `&mut`, and read/write would drift on exactly the semantics that must never drift
(`${a[-1]}` read vs `a[-1]=x` write).

**The seam — a shared per-hop resolver, NOT a two-pass Bind→Walk.** The earlier "Bind
`(VarPath,&Scope)→Vec<ConcreteStep>` then Walk" framing is wrong: classification is
**container-dependent** — index-vs-key, negative-index normalization, and slice bounds all need the
container *at that hop* (see the `Dynamic` arm at `scope.rs:551`, which inspects Array-vs-Object to
decide index-vs-key). Only `$k`→value is Scope-only. So the shared unit is a **per-hop**
`resolve_step(&container, segment, &scope) -> Result<Step, PathError>` where `Step` is a concrete
`Index(usize)` / `Key(String)` / `Slice(range)` produced *after* seeing the container. Both a
`walk_read(&serde_json::Value)` and (next phase) a `walk_write(&mut serde_json::Value)` call the
same `resolve_step`; they differ **only** in `&` vs `&mut` descent and leaf handling. All the
classification, negative math, and every error message live in `resolve_step`, once — which is what
guarantees read and write can't diverge.

**Decisions (all settled 2026-07-02):**
- **Arithmetic bareword classification → parse-time.** `$(( xs[i] ))` reads variable `i`;
  `${xs[i]}` is the literal key `"i"`. Resolved at PARSE time, per context: the arithmetic parser
  emits a *variable* subscript segment, the interpolation parser a *key* segment, so `resolve_step`
  stays context-free (no context flag threaded through). `arithmetic.rs` stops hand-collecting
  brackets (also killing the quoted-key whitespace bug).
- **`PathError` → three variants.** `UndefinedRoot` (soft in strings, loud in expr) · **`Absence`**
  (missing key, out-of-bounds) · **`Shape`** (string-key-on-list, integer-index-on-record,
  subscript-a-scalar, dotted-access). `${path:-default}` catches `UndefinedRoot` + `Absence` and
  yields the default; `Shape` always stays loud (a wrong-type access is a bug, not absence).
  Empty-string/`null` → default is a *post*-resolution check, not a `PathError`. **An unset dynamic
  key** (`${r[$k]}` with `$k` itself unset) is `UndefinedRoot`-class — so `${r[$k]:-d}` defaults
  when `$k` is unset (the *variable* is missing, not the key).
- **`VarLength` / `VarWithDefault` → carry a `VarPath`.** AST change + the parser builds the path +
  **widen the lexer `VarLength` regex** (`\$\{#[a-zA-Z_]\w*\}` → accept brackets) so `${#u[tags]}`
  lexes in expression position, not just inside strings. This is what turns the current loud "bind
  first" placeholders (`${#u[tags]}`, `${cfg[port]:-8080}`) into working path-aware forms.
- **Scope of #6 = read side + deferred forms.** Lands: the shared `resolve_step` + `walk_read`, the
  `VarPath` conversion, arithmetic subscripts, the `PathError` split, and full-path-prefix error
  messages (`${services[web][prot]}` names the real path, free once the walker owns the path).
  **Does NOT land lvalue writes** (`a[b]=x` grammar + `walk_write` + validator root-binding) — that
  is the next phase, and `resolve_step` is designed to be its shared core.

**Technical risk to prototype early:** `walk_read` returns a `&serde_json::Value` borrowed from the
`Scope` read-guard, unwrapping to owned only at the leaf (this is what removes the O(n²) clone). The
borrow must complete within the `RwLock` guard's lifetime — validate that borrow shape before
building out.

**Grounding pass 2026-07-02 (against HEAD; design-pass with the code open).** Three findings that
sharpen the plan:
- **The borrow risk mostly dissolves.** `resolve_path(&self, &VarPath) -> Result<Value, PathError>`
  takes `&self` and returns **owned** `Value`, and every caller already holds the guard
  (`let scope = self.scope.read().await; scope.resolve_path(&path)` — `scope: RwLock<Scope>`,
  `kernel.rs:665`). So `walk_read`'s `&serde_json::Value` borrow lives **entirely inside the method
  body**, well within the guard; no lifetime threads out to any caller and the public signature is
  unchanged. The actual O(n²) is today's `self.get(root_name).cloned()` (`scope.rs:507`) cloning the
  *whole root* on every `${u[$k]}`; deleting that `.cloned()` (walk the borrowed root, clone only the
  leaf) is the fix.
- **Arithmetic decision B is a correctness bug, not a refinement.** `arithmetic.rs:352-379`
  hand-collects the bracket text, rebuilds `${root[...]}` as a string, and reparses through the
  *shared* `parse_subscript` — so `$(( xs[i] ))` today resolves `i` as a **literal key**, not variable
  `i`. Fix per B: arithmetic builds the `VarPath` directly, emitting `VarSegment::Dynamic("i")` for a
  bareword subscript (interpolation keeps emitting `Key("i")`), so `resolve_step` stays context-free.
- **The read/write seam, made minimal.** `resolve_step` owns everything **identical** across read and
  write — index-vs-key classification, negative-index math, and `Index` bounds (out-of-bounds →
  `Absence`; a write index-set is also in-bounds-only, so bounds are genuinely shared). The **only**
  thing the walk decides is *missing-`Key` policy*: `walk_read` → `Absence`; the future `walk_write`
  leaf → insert. That is the smallest split that keeps `${a[-1]}` read and `a[-1]=x` write from
  drifting. `resolve_step` does **not** check `Key` existence (that's walk policy).

**`PathError` reclassification — every current `Invalid` site.** Today `UndefinedRoot | Invalid`
(`scope.rs:27`); the split assigns each site:
- **UndefinedRoot** — root not in scope; **unset dynamic key** `${r[$k]}` when `$k` itself is unset
  (`scope.rs:554` — the *variable* is missing, so `${r[$k]:-d}` defaults).
- **Absence** — out-of-bounds index (`scope.rs:79`); missing record key `no such key` (`scope.rs:92`).
- **Shape** — dotted `.field` (`scope.rs:531`); subscript-a-scalar (`scope.rs:540`);
  integer-index-on-record (`scope.rs:66`); string-key-on-list (`scope.rs:96`); slice-a-record
  (`scope.rs:117`); dynamic-index-not-integer (`scope.rs:560`); `$?` subscripted (`scope.rs:499`).

`${path:-default}` catches **UndefinedRoot + Absence** → default; **Shape always stays loud**; plus
the post-resolution `null`/empty-string → default check (decision A — *not* on `false`/`0`/`[]`/`{}`).
- **`...` spread**: new lexer token; no existing `..`/`...` handling to collide with. Scope it
  to `[ ]` value context only.

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
   `$()`). Docs must show, side by side, the bracket forms: `${xs[0]}`, `${xs[-1]}`,
   `${xs[0:2]}`, `${r[k]}`, `${r[$key]}`, `${r["weird key"]}`, `${r[a][b]}`, `${r[a][0]}`.
   Models reproduce exactly what's shown and guess (wrongly) at what isn't. Show `${user.name}`
   as the WRONG form (with its error) so the dot-leakage prior gets a taught contrast.

3. **Contrast expansion vs builtin-capture, with both shown.** Models overgeneralize the `$()`
   capture rule. Put them next to each other: access and length are *expansions* (NO `$()`) —
   `${xs[0]}`, `${r[k]}`, `${#xs}`; a *builtin* used as a value NEEDS `$()` — `$(keys $r)`,
   `$(values $r)`. And `$()` is command-substitution ONLY — never wrap an access path in it.

4. **String-interpolated paths use `${path}` braces — SHOWN, not merely stated.** A rules sheet
   that *stated* the rule still produced unwrapped paths; an example that *showed* the braced form
   was copied. Every doc string embedding a path must use `"${r[$k]}"` / `"${user[name]}"` (NOT
   `$($r[$k])` — `$()` is execution only). Show bare `"$user.name"` (expands `$user`, leaves
   `.name` literal) as the WRONG form so models learn the contrast. Quoted keys can't appear
   inside a double-quoted string (loud error) — show the assign-first idiom
   (`k="weird key"; echo "${r[$k]}"`) wherever a weird key meets interpolation.

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
   assignment, and `push` for append — otherwise we're validating a language we don't ship.

8. **Pre-sign-off re-test — REQUIRED AGAIN (the 2026-06-05 pass is superseded).** The earlier
   Haiku re-test validated the *dotted* braced syntax (`${r.key}`, `${r.$k}`, `${users.alice.city}`)
   and the bare-builtin for-head — both since replaced. Re-run the panel (include the weak tail,
   not just Haiku) against the FINAL surface: bracket access incl. `${r[$k]}` dynamic keys and
   bareword-literal keys, deep chains `${services[web][port]}`, `${grid[1][0]}`, slices,
   **`$()`-only iteration `for x in $(values $c)` / `for k in $(keys $c)`** (NOT bare
   `for k in $record` — superseded), bracket lvalues (`user[email]=x`, `fruits[0]=kiwi`), plus a
   **dot-leakage negative task** (does the model reach for `${user.name}`? does the error copy
   converge it in one round?) and a literal-vs-variable subscript task (`${user[name]}` vs
   `${user[$name]}`). **Watch one number above all: convergence on `$(values $xs)` in the
   for-head** — the 2026-06-05 panel's single most common error was *requiring `$(keys $r)` in the
   head*, which the `$()`-only decision now mandates; if models need more than one verify round to
   accept it routinely, execute the v2 relaxation (bare collection iterates; bare string is a loud
   runtime error) early. Results not yet gathered — do not sign off without them.

9. **Update the "sh subset / shellcheck" framing in the same PR** (see issues.md P4). The docs
   that introduce collections are the natural place to restate "inspired by sh/bash, informed by
   shellcheck" and that the validator — not shellcheck — owns collection correctness.

10. **Spaced-assignment error needs positive reinforcement.** Models context-switch to JSON/Python
    spacing and will write `x = [a, b]`. The SC1068-style validator error must show BOTH the
    problem AND the fix — e.g. *"kaish assignment takes no spaces around `=`; write `x=[a, b]`"* —
    so the verify pass corrects the model in one round instead of just rejecting. The same
    pattern applies to every new loud error in this design: dotted access (`use ${user[name]}`),
    dotted LHS (`use user[email]=`), multi-word literal values (`quote it: {msg: "hello world"}`),
    out-of-bounds set (`push grows lists`), collection-vs-scalar compare (`use in`).

11. **Show `!=` for scalar inequality.** Given no `!=` example, Haiku expressed "not equal to dog"
    as `[[ $a not in [dog] ]]` — a list-literal membership test. Semantically correct but
    roundabout; docs should show `[[ $x != val ]]` so models don't route every inequality through
    `not in`. This also means the `in` RHS must accept a list/record **literal**, not just a
    `$var` (see impl notes, incl. the glued-`[dog]` GlobWord case).

12. **Shape-variance trap — relocated, not gone** *(2026-07-01 gemini review; revised 2026-07-02
    after the $()-only iteration decision)*: the original trap was `for x in $data` silently
    iterating a record's KEYS when an API returned an object instead of the expected list. Bare
    `for x in $data` is now an E012 error, so that exact form is dead — **but the trap relocates**
    to the shipped idiom: `for x in $(values $data)` on a record yields the record's *field
    values* instead of iterating the one object. Still a silent type cascade, not an error. So
    the mitigation stands and is now more load-bearing: the docs must show a shape guard where
    data shape isn't trusted. *(Resolved — the shape predicate shipped: `typeof` /
    `[[ -list ]]` / `[[ -record ]]`, decision F above. Docs cover the guard idiom in
    `docs/LANGUAGE.md` "Shape guards" and the composable-help collections fragment.)*

## Help & teaching delivery

Where the teaching copy actually lands (mapped 2026-07-01 against `crates/kaish-help`; see
[composable-help.md](composable-help.md) for the system). The fragment taxonomy already
matches the teaching notes — `Rule`/`Example`/`Contrast` variants and `Summary`/`Reference`
depths are exactly "state it tersely / show it in a full structure / show the wrong form
with its error" — and the quote-to-join pass (`d324bb7`) is the proven playbook:
Foundations Rule+Contrast → extend `syntax_section`s → regen `syntax.md` (drift-tested) →
hand-write LANGUAGE.md → sweep limits.md/README.

Three layers, matched to three budgets:

1. **Always-on** (MCP instructions / tool descriptions — `Recipe::agent_onboarding()` /
   `tool_description()`). The composed instruction block an embedder ships is already
   ~9–10.5K chars, dominated by the builtin index (~5.4K). Collections gets at most 2–3
   terse Foundations Rules here (the literal forms; "access is always `${name[...]}` —
   never dots"; `push` vs spread) plus one Reference-depth Contrast (`${user.name}` →
   error, with fix). The dense teaching from the notes above must NOT land in this layer.
2. **Reference** (`help syntax` / generated `syntax.md`). A new
   `syntax_section("collections", …)` carries the canonical tested example set (literals,
   every access form side by side, lvalues, `push`/spread, a worked record loop).
   Membership (`in`/`not in`) extends the **existing Test Expressions section** — models
   look for test operators there, and it already shows operators inside full
   `if [[ … ]]` structures (Teaching note #1 for free). Record iteration extends
   Control Flow / Command Substitution.
3. **Deep** (LANGUAGE.md). Hand-written section, per the standing don't-generate decision.

Points decided or flagged during the 2026-07-01 review:

- **`help collections` becomes a fragment query, not a file.** Models will type it.
  Rather than a hand-written `content/en/collections.md` (a dual representation that will
  drift), extend `HelpTopic` so a topic can render a *selection* of fragments —
  `help collections` composes the collections-keyed fragments, single-sourced with
  `syntax.md`. This sets the pattern for future subsystem-sized syntax features.
- **limits.md truth-up.** limits.md stated "kaish will never have `[]` array syntax" —
  reworded 2026-07-01 (same branch as this revision): `[ ]` is *reserved by kaish* —
  `[[ ]]` today, native list literals per this design. The `[` command stays banned
  forever (a `test` builtin might someday return; `[` will not, nor external `[` — we own
  the brackets). The implementation PR must do the final truth-up ("planned" → shipped)
  and add the new loud-error rows: dotted access, `[`-leading glob assignment, multi-word
  literal values, dotted assignment LHS.
- **Onboarding becomes importance-ranked tiers (Amy, 2026-07-01).** The always-on block
  should lead with the most important ~200–300 characters, then successive ~200–300-char
  ranks in descending importance — the client model gets the critical rules immediately,
  even skimmed or truncated. Everything below the ranked tiers moves to an easy-to-find,
  loadable resource (help topics / MCP prompts) that the block's tail points at. This
  reshapes the budget question: fragments gain an importance rank, recipes compose by
  rank, and the size guardrail caps the ranked tier stack rather than one undifferentiated
  blob. Nothing caps composed-help size today (no test, no stated ceiling; `Depth` is the
  only lever) — tracked in issues.md under Composable help; collections is exactly the
  feature that will test it.
- **Validate the shipped artifact, and keep validating.** The pre-sign-off panel re-test
  (Teaching note #8) should use the *composed help output* as its cheat sheet — write the
  fragments first, compose `agent_onboarding()` plus the collections syntax section, and
  hand that to the panel. Every prior round tested ad-hoc cheat sheets; testing the real
  delivery artifact validates the syntax and the teaching copy in one pass, and the
  fragment text arrives pre-validated. Beyond the one-shot gate, we want **recurring
  cross-agent studies** — multiple model families and sizes, doc variants measured
  against task success — to hone the help copy for impact: the
  [designing-syntax-with-llms](designing-syntax-with-llms.md) method extended from syntax
  decisions to the documentation itself. That's a sizable side quest; scope it as its own
  effort, not a rider on the implementation PR.

## Out of scope (first cut)

- Set/map algebra (union, intersection, merge).
- Comprehensions / map / filter as syntax (use `for` + `push`; the worked example is the idiom).
- Typed schemas on records.
- Deletion ergonomics (`unset user[email]`) — noted above, designed later.
- **Argv spread** (`cmd ...$xs` splatting a list into command arguments) — models will try it
  once `...` is taught; explicitly excluded for now, revisit with use cases.
- **Slice lvalues** (`xs[0:2]=…`) — loud error, see lvalue rules. (`push` to a bracketed
  path IS in scope — see the push decision.)
- Pair iteration (`for k v in $record`) — iterate keys, access values.
- JSON ingress/egress builtins (`fromjson`/`tojson`) — sketched above and **prototyped
  early, ahead of the collections grammar** (they need no parser work); hard prerequisite
  for the jq-out-of-core possibility (see Open decisions).
