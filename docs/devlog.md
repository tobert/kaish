# kaish devlog

Narrative history of landed work and the decisions behind it — the "how we got
here" color that used to clutter [issues.md](issues.md) and tax every read of it.
This is *not* the authoritative record: `CHANGELOG.md` (per-version, user-facing)
and `git log` (commits, SHAs) are canonical. This is the story.

Newest themes first within each area; dates are when the work landed.

Write entries **late** — just before signoff or opening a PR — so they carry with
the PR and describe the work as it actually landed. Not early, not mid-flight: the
decisions aren't settled yet, and an entry written ahead of its change goes stale
before it ships.

---

## Importance-ranked onboarding tiers (2026-07-01)

Step 3 of the collections effort splits in two: the tier *mechanism* (pure infra,
this entry) and the collections *fragments* (which describe not-yet-built syntax,
so they wait on a ship-vs-panel scoping call). The mechanism landed first because
issues.md wanted it "before the collections fragments land — they're the feature
that will test it."

The always-on instruction block an embedder ships was an undifferentiated blob in
registry order; nothing capped its size and nothing guaranteed the load-bearing
rules came first. Now `Fragment` carries an importance `rank` (0 = most important;
`UNRANKED` default keeps registry order), and `select_for_concept` stable-sorts by
it — a deliberate no-op for any all-`UNRANKED` concept, so `syntax.md`
(`render_syntax_reference`, which doesn't even go through that path) and the REPL
welcome stay byte-identical. The ~10 Foundations Summary fragments got explicit
ranks 0..9, ordered by what an agent needs to write *correct* kaish first
(no-word-splitting, quote-to-join, then substitution/iteration, then capability
and safety guarantees, with the verbose overlay-mode trailing). A char-budget test
now keeps the spine lean.

Two implementation notes worth keeping: `const fn ranked(self, rank) -> Fragment {
Fragment { rank, ..self } }` relies on functional record update in const context
(stable since Rust 1.83; MSRV is 1.85), which let the rank attach in the static
registry without a builder-signature change touching every call site — only the
ranked fragments changed. And the sort key must stay `rank` alone: adding `key`
would alphabetize equal-rank fragments and reorder the Model concept the REPL
welcome depends on. Deepseek review confirmed the no-op-for-unranked claim and
flagged one latent case (Reference-depth Foundations would separate a Contrast
from its Rule — inert today, filed P4) and the public-field break (loud compile
error, marked BREAKING).

## Native collection read access: the bracket path resolver (2026-07-01)

Step 2 of the collections effort — reading into a value with `${xs[0]}`,
`${r[key]}`, `${r[$k]}`, `${r["weird-key"]}`, `${xs[-1]}`, `${xs[0:2]}`, chained
`${a[b][c]}`, and `${#…}` list/record length. Deliberately *before* the
literal-grammar gate: it only consumes `${…}` segments the lexer already splits,
so no value/argv bifurcation and no glob-merge change — the read side was
"half-built" and this finishes it.

A reconnaissance pass (Explore agent) charted the machinery and surfaced the
shape of the work: the lexer already hands out `["VAR","[0]","[k]"]`, but
`parse_varpath` was *filtering every bracket segment out* (so `${x[0]}` silently
returned the whole value — a latent silent bug the red test baseline caught
immediately), and `resolve_path` rejected anything multi-segment. The clean
insight that shaped the design: dynamic keys are just `$var`, resolved through
`scope.get`, and `resolve_path` is a `Scope` method — so the **entire traversal,
dynamic keys included, lives in `resolve_path`**, and all four var-ref eval sites
(sync + async, expression + string) get collection access for free with no
evaluator plumbing.

The load-bearing decision was the error channel. `resolve_path` went from
`Option<Value>` to `Result<Value, PathError>` with a two-variant split that the
whole no-silent-fallback directive rides on: `UndefinedRoot` is **soft** (an
error in expression position, empty inside a string — bash-compatible, preserving
every existing `"${UNSET}"` → empty), while `Invalid` (subscript on a scalar,
out-of-bounds, missing key, dotted access, string-key-on-list, integer-index-on-
record) is **loud everywhere, including inside double-quoted strings** — never
swallowed to empty. That distinction is exactly why the four primary eval sites
each match both arms explicitly; the three reduced *sync* pipeline sites keep
their pre-existing best-effort coalescing (a known narrow gap, filed P3).

Access unwraps at the boundary through `json_to_value_no_envelope` (the same
envelope-free law `fromjson` uses — an envelope-shaped sub-object stays a record,
never becomes bytes), so `[[ ${cfg[healthy]} == false ]]` is a typed bool compare
and `$(( ${cfg[port]} + 1 ))` just works — the latter needed teaching the
arithmetic mini-parser's `${…}` branch to collect bracket runs and reuse
`parse_varpath` + `resolve_path` rather than choking at the first `[`.

Brackets-only is enforced by making a dotted `${u.name}` a loud error suggesting
the bracket form (the lexer still splits `.field`, so a non-root `Field` segment
*is* the dotted case). Semantic calls, all consistent with the loud-errors ethos:
missing key errors (not bash's silent empty — `[[ k in $r ]]` is the presence
test), integer index on a record errors ("integers index lists"), a dynamic
string key works as a list index if it parses as one. Deepseek review confirmed
the core (envelope-free at every level, panic-free bounds math, correct arithmetic
bracket balancing) and turned up only the two low-priority follow-ups now in
issues.md. Twenty-four kernel-routed tests, built on `fromjson` to construct the
values the literal grammar can't yet.

## JSON bridge: fromjson / tojson land ahead of the grammar (2026-07-01)

First implementation step of the collections effort, and deliberately the one
that touches no lexer or parser. `fromjson`/`tojson` are the value model's text
boundary — the pair the arrays-and-hashes doc sketched as "prototype early." They
exercise the whole value-model plumbing (structured `$()`, `.data`, typed
positionals, assignment capture) end to end, so building them first de-risks the
boundary semantics and pins the error copy before the big grammar PR.

Two integration points were verified against HEAD before writing a line, because
they decide whether the pair is useful *before* native access exists: (1)
command-substitution already prefers a result's `.data` (`kernel.rs:3716`), so
`cfg=$(… | fromjson)` captures the structured `Value`; (2) a variable holding a
`Value::Json` expands straight into `args.positional` as that typed value
(`kernel.rs:3285`), so `tojson $cfg` reads a real value off the positional, not a
stringified copy. Both held — the pair works today, with access arriving later.

The load-bearing decision is **envelope-free conversion**. The internal
`json_to_value` auto-decodes any object matching the base64 byte-envelope shape
into `Value::Bytes` — correct for internal round-tripping, a silent-corruption
trap for *external* JSON that happens to match. Added
`json_to_value_no_envelope` (kaish-types) and routed `fromjson` through it; the
hazard is pinned by a test that feeds an envelope-shaped document through
`fromjson | tojson` and asserts it re-serializes as a record, not as refused
binary. `fromjson` is one-doc-one-value (serde's trailing-garbage rejection does
the work; empty input is a separate loud error), and its parse errors carry
serde's line:column. `tojson` is text-out-only — setting `.data` would make
`$(tojson $x)` round-trip straight back to a value and defeat the export escape
hatch — and refuses `Value::Bytes` loudly rather than emit an envelope. The
roundtrip law (`fromjson "$(tojson $x)"` ≡ `$x`) is test-pinned. Both are pure
data: registered unconditionally, verified under `--no-default-features`, and
tested through `KernelConfig::isolated()` (no localfs).

## Arrays & hashes design revision: the brackets are ours (2026-07-01)

The 2026-06-05 collections proposal came back for review a month later and left
substantially changed — not because the method was wrong (the weak-model stress
tests held up), but because grounding it against the current code surfaced two
facts that flipped decisions, and a design conversation with Amy pushed one
principle — consistency over convenience — further than the original draft dared.

Three Sonnet exploration passes re-grounded every file:line claim (several had
drifted; two issues.md citations pointed at entries that no longer exist). Two
discoveries did real work. First: `for x in a b c` word lists already parse, so
the proposal's bare-builtin for-head (`for k in keys $r`) wouldn't have filled a
grammar hole — it would have changed the meaning of valid syntax, with the
quasi-reserved word set growing every time a builtin ships. Replaced by
`for k in $record` iterating keys: Python's exact prior, zero grammar cost.
Second: the lexer's `Ident` regex admits `.`, so `user.email=x` parses *today* as
a flat, unreadable variable — dotted path-writes would have silently diverged
from that. That discovery sealed Amy's instinct to drop dotted access entirely:
**brackets only** (`${user[name]}`, `${r[$k]}`, `${r["weird key"]}`), bareword
subscripts are literal keys, jq keeps the dot-shaped language so the two surfaces
never blur. The doc keeps the superseded evidence annotated rather than erased —
the record of *why* is part of the method.

The review also filled gaps the original never addressed: scalar unwrap at the
access boundary (so `[[ ${cfg[healthy]} == false ]]` is a typed comparison, not a
stringify accident), structural equality collection↔collection with a loud error
for collection↔scalar, and a full lvalue ruleset — bracket paths, no
autovivification, in-bounds index set, POSIX-name LHS tightening. A fresh lexer
collision fell out of writing the implementation notes: glued `[]`/`[dog]`/`[1]`
merge into GlobWords today (`x=[dog]` is currently a *glob assignment*), so a
`[`-leading token at value position is now ALWAYS a list literal — a small
breaking change Amy ratified with "we own the []". limits.md's "kaish will never
have `[]` array syntax" line got the falsehood treatment the same session.

Cross-model review, house style (no diff, whole files): a gemini-pro batch with
the lexer and parser attached found what the doc's impl notes underestimated —
kaish's pre-parser token-fusion passes are hostile to the new literals.
`merge_colon_adjacent` eats unspaced record colons (`{port:8080}`), the
double-quote regex can't survive quoted keys inside interpolated strings, and the
shared expression parser must bifurcate into value/argv grammars or `ls [dog]`
grows a JSON argument. Every lexer claim was verified locally before folding.
Gemini also pushed back on `push` being top-level-only — the nested-append
workaround (`a[b]=[...${a[b]} new]`) was hostile enough that Amy reversed it:
push takes bracket paths under the lvalue rules. Unspaced colons: accepted.
Quoted keys in strings: loud error suggesting assign-first. A deepseek second
pass on the post-fold doc was still in flight when this PR opened.

Two forward commitments landed in the doc. `fromjson`/`tojson` (jq-named, pure
data, envelope-FREE conversion — `json_to_value`'s bytes-envelope sniffing must
never run on external JSON) get prototyped *before* the grammar: they exercise
the whole value-model plumbing without touching the parser. And the help system
grows importance-ranked ~200–300-char onboarding tiers (Amy's design), with
`help collections` as a fragment query rather than a file, and the panel re-test
gated on testing the *composed* help artifact — the thing we ship, not an ad-hoc
cheat sheet. Collections grammar work does not start until that re-test passes.

## `classify_command`: a supported command classifier for embedder preflight (2026-06-30)

Follow-on to the typed latch accessor (#45). kaijutsu wants to gate kaish's
destructive/external operations for consent — walk a script, find the command
nodes that escape to `PATH`, and block until they're approved. It can already do
most of that with the public surface (`parser::parse`, the `ast`, `tool_schemas`,
`has_function`). The one thing it *can't* do without forking kaish's truth is
classify a command name the way the interpreter resolves it — the two rules that
decide that (`is_static_command_name`, the special-form set) were private helpers
in the validator. If kaijutsu hardcodes copies, the day kaish refines name
resolution its consent gate silently disagrees with what kaish actually runs.
That's the silent-divergence failure class, and here it's security-relevant.

So we added `Kernel::classify_command(name) -> CommandKind` (`Builtin` /
`UserTool` / `Special` / `Dynamic` / `External`, plus `escapes_kernel()` for the
two buckets a gate scrutinizes). `CommandKind` lives in `kaish-types` (pure data,
`#[non_exhaustive]` so a future variant doesn't break embedders — and the safe
default for an unknown kind is to gate it). The classification core is one shared
`classify_command_name` in `validator/walker.rs`; the kernel computes the two
booleans and delegates, so there's a single source of truth.

The sharp bit was discovered empirically: the validator's `is_special_command`
set (`true`/`false`/`:`/`readonly`/`local`) is **not** what the interpreter
actually short-circuits. At runtime `readonly` resolves to an *external* command
(exit 127), `:` is a parse error, `local` is parser-level. The validator uses that
broad set only to suppress a "command not found" warning. `classify_command`
deliberately mirrors the **interpreter's** real special-forms
(`true`/`false`/`source`/`.`) instead — a consent gate must see `readonly` as
`External`, not be told it's internal. The validator↔runtime mismatch is now
filed as a P3 (the validator probably shouldn't suppress those warnings).

A deepseek review (kaibo) caught the one real under-reporting hole: execution
expands **aliases** before the registry/PATH lookup, so `alias cat=/bin/evil`
would have classified as `Builtin` while running external — the dangerous
direction for a consent gate, and there was no public alias API for the embedder
to compensate. `classify_command` now expands aliases internally, mirroring
`execute_command_depth`'s bounded recursion (special-forms re-checked each step),
so it reports `External` there too. `/v/bin/cat` and `.kai`/backend tools still
over-report as `External` (the safe direction — over-gate, never leak a `PATH`
escape). Added a `${VAR}` → `Dynamic` guard for the string-API surface.

**Staying in sync with the executor.** The classifier duplicates the
interpreter's resolution rules, which is the same silent-divergence risk the
feature exists to kill — just moved inside the kernel. Three guards keep it
honest: (1) *membership* never drifts because classify reads the live registry
and user-tool tables, not a copy; (2) the *special-form set* is a single
`SpecialForm` enum whose `from_name` is the only place a name becomes "special" —
classify reports it via `is_runtime_special_form`, and `execute_command_depth`
matches the enum **exhaustively**, so adding a form is a *compile error* until
both the name mapping and the behavior are updated (the first cut used a const +
`unreachable!`, but a deepseek round-2 review flagged that as a runtime panic the
drift test didn't actually exercise — the enum makes it compile-enforced and
drops the panic); (3) a `classify_command_matches_executor` drift test pins the
rules that *can't* be compile-enforced (user-vs-builtin precedence, alias
expansion) by classifying *and* observing the real resolution for each kind, and
now executes every special form (incl. `source`/`.`), not just `true`/`false`.
Add a resolution step to the executor without teaching classify, and either it
won't compile or that test fails.

Deferred, same reasoning as the `kaish-edit` crate: the `PreflightReport`, the AST
walk, and the consent loop are embedder policy (kaijutsu owns them), and a
`Kernel::preflight(src)` convenience waits for a second consumer. Docs: a
"Preflighting a script for external commands" recipe in EMBEDDING.md.

## Interpreter stack-depth analysis → first GitHub Issues (2026-06-30)

A question — "what pushes up the interpreter's need for stack space over time?" —
turned into a map of the async statement engine and three filed issues. The shape
of the answer is worth keeping: stack cost here is **call-chain depth × fat async
frames**, not data.

- The real statement engine is the async kernel in `kernel.rs` (not the sync
  `Evaluator` in `interpreter/eval.rs`, which only does arithmetic/test
  sub-eval). Its core is a set of mutually-recursive `Box::pin`-returning
  functions — `execute_stmt_flow`, `eval_expr_async`, `eval_string_part(s)_async`,
  `eval_test_async`. Rust async recursion *requires* boxing, and each boxed level
  keeps every live local across every `.await`, so frames are heavy.
- The deepest cycle is command substitution: `Expr::CommandSubst(Vec<Stmt>)`
  re-enters `execute_block_capturing` → `execute_stmt_flow`, stacking two boxed
  futures per `$( … )` nesting level. Recursive shell functions
  (`execute_user_tool`) and `.kai`→`.kai` sourcing (`try_execute_script`) deepen
  the *same* thread stack the same way. None of these is depth-guarded — only
  alias re-entry (`<10`) and the lexer (`MAX_PAREN_DEPTH = 256`) are.
- Per command, the dispatch chain (`execute_pipeline` → runner → `dispatch_command`
  → `execute_command_depth`) carries a ~30-field `ExecContext` that's copied twice
  in `dispatch_command`, plus `#[tracing::instrument]` future bloat on the hot
  frames.
- Runtimes are plain `Runtime::new()` (no custom stack). The foreground root future
  runs on the `block_on` thread; forked work (pipeline stages, scatter, background)
  hops to tokio worker threads with the default ~2 MB stack — so the same script
  has less headroom inside a pipe than in the foreground.

Filed as [#46](https://github.com/tobert/kaish/issues/46) (depth guard — a loud
error beats a silent `SIGSEGV`), [#47](https://github.com/tobert/kaish/issues/47)
(explicit, uniform worker-thread stack size), and
[#48](https://github.com/tobert/kaish/issues/48) (a profile-first memory/allocation
pass — suspected low cost with occasional peaks, not urgent). These are the first
issues we've put on GitHub instead of [issues.md](issues.md): an experiment ahead of
announcing kaibo, where outside agents and people will want a public tracker.
`devlog.md` stays in-repo and ships with the code — these repos also teach how a
project gets sculpted.

## `execute_argv` — argv-native kernel entry point (landed 2026-06-29)

`execute(&str)` is *string-native*: it lexes and parses its input. A caller that
already holds tokenized argv — a structured embedder (kaijutsu), a future
busybox-style multicall binary — had to re-quote argv into a string just so the
lexer could split it apart again, a round-trip that's *lossy* for typed values
(`to_argv()` stringifies `Bytes`/`Json`). `Kernel::execute_argv(name, &[Value])`
is the peer door that skips it. Full design: [multicall.md](multicall.md).

**The implementation deviates from the design doc's letter, deliberately.** The
doc proposed a new `build_args_from_argv` that builds `ToolArgs` directly,
"mirroring `build_args_async` minus the `Expr` eval." Writing a *second* binder is
exactly the drift hazard the validation-builder unification (2026-06-23) was about.
Instead the only new logic is `argv_to_args(&[Value]) -> Vec<Arg>`: a classifier
that maps each token to the AST `Arg` the lexer would produce for the equivalent
minimally-quoted word (`--` → `DoubleDash`, `-x…` with an alpha lead → `ShortFlag`,
`--k=v` → `Named`, identifier `k=v` → `WordAssign`, else literal `Positional`; a
**non-string** `Value` is always a literal positional — the typed-passthrough win).
`execute_argv` then runs the resulting `Command` through `execute_pipeline`,
*reusing the entire string-door dispatch chain verbatim* — command resolution,
arg binding, `--json`, the latch. Two binders can't drift because there's still
only one. Typed values survive because `Expr::Literal(Value)` carries the value
and evaluates by identity (it never becomes a `GlobPattern`/`VarRef`, so no
glob/`$VAR`/split can occur).

**Semantics:** tokens are literal — the "single-quoted word" rule taken to its end.
`execute_argv("echo", &["*.txt"])` emits `*.txt`. The kernel's pre-execution
*syntax* validator doesn't run (argv has no shell syntax; a tool's own
`validate()`/clap parse still does, at dispatch).

**Tests** (the design's four surfaces): an in-crate classifier suite incl. the
workspace's **first proptest** — `classifier_matches_parser_on_clean_tokens`
asserts `argv_to_args` agrees with the *real parser* on metachar-free tokens. It
earned its keep immediately, shrinking to `:A=0`: the lexer colon-merges `:A` into
one `Ident` and parses it as a `WordAssign`, where the classifier (bash-correctly)
makes a positional. They converge observably anyway — a `WordAssign` for any
non-`export`/`alias` command stringifies straight back to a `"key=value"`
positional — so the property compares *logical arguments*, not AST tags, and the
case is pinned as a regression seed. Plus kernel-routed `execute_argv_tests.rs`:
argv door ≡ string door over a corpus, literal-not-globbed, no `$VAR` interp,
typed `Int`/`Json` passthrough, `--json` transform, exit-127, and a full latch
round-trip (gate → `--confirm=<nonce>`) all through the argv door.

**Deferred:** the `kaish-multicall` binary (the cheap frontend half) and the
`&[String]` convenience door — both in issues.md.

## rg-features port — `--ftype` filtering on grep + glob (landed 2026-06-27, #38)

`rg` was dropped under the 80% rule, but its still-useful filtering re-homed onto
kaish's two *modern* search builtins. Driver: kaibo's hot path (type-scoped greps,
early-stop on match caps). The walker engine was already done — `WalkOptions.types`
had been live but dormant — so this was surface wiring plus a shared registry.

Landed scope (the design lived in the transient `search-features-port.md`, deleted
on ship):
- **grep + glob only; `find` stays POSIX** (no `--ftype` on find — it keeps
  traditional behavior; the `--no-ignore` recovery question for both search builtins
  and for find-under-`Enforced` is the live deferral, now in issues.md P3).
- **`--ftype` is the kaish-wide file-type-filter standard**, deliberately *not* rg's
  `-t`. Both grep and glob get `--ftype` / `--ftype-not` / `--ftype-list`, sharing
  one `kaish-glob::build_file_types` registry so they can't drift. An unknown type
  name is a loud exit-2, never a silent empty match.
- **All new flags are long-only, no shorts** — sidesteps GNU-grep `-T`/`-m` muscle
  memory landing on different semantics.
- grep also got **`--hidden`** (include dotfiles, bash no-dotglob default off) and
  GNU-semantics **`--max-count <N>`** (per-file cap, streaming early-stop so it
  bounds work on large/piped inputs; a truncated-at-cap UTF-8 carry is no longer
  misflagged as binary). glob keeps its fd-style `-t`/`--type` (entry *kind*:
  file/dir), which composes with the new extension-based `--ftype`.

DeepSeek-reviewed (via kaibo) on the branch.

## Correctness one-offs — grep -c exit, `$()` trim, jq /0 (landed 2026-06-24)

Three small independent silent/surprise fixes (`correctness_oneoffs_tests.rs`):

- **`grep -c` exits 1 on zero matches** (GNU). Was always exit 0 — the count is
  printed but the status must still signal "no match." Both the single-buffer and
  multi-file count paths now set `code = 1` when the total is 0 (multi-file: only
  when *no* file matched; a read-error still overrides to 2).
- **`$()` strips only trailing newlines**, not all trailing whitespace. The bare
  `Expr::CommandSubst` arms used `.trim_end()` (ate spaces/tabs); now
  `trim_end_matches('\n')` — the *exact* trim the quoted `"$(…)"` interpolation path
  (`StringPart::CommandSubst`) uses, so bare and quoted command substitution agree
  (they're the same operation; the for-loop split path's extra `\r` strip is its own
  line-splitting concern). Significant trailing spaces survive (`x=$(printf 'a  ')`).
- **jq `. / 0` fails loudly** instead of silently returning `null`. The mechanism:
  jaq evaluates `n/0` to a non-finite `Val::Float` (inf, or NaN for `0/0`), and
  `val_to_json` did `from_f64(inf) → None → unwrap_or(Null)` — a silent-wrong null.
  New `has_nonfinite_float` (recurses arrays/objects) gates the result loop and
  errors. **Decision:** this also errors on jq's `infinite`/`nan` *literals*, where
  real jq clamps `infinite` to max-f64 and renders `nan` as null. We can't tell a
  division-by-zero inf from an `infinite`-literal inf at serialization time (jaq
  doesn't error at the division like real jq does), and a loud error beats a silent
  null (the no-silent-fallbacks / crash-over-corruption directive). `infinite`/`nan`
  literals are rare; accepted divergence. The separate jaq float-formatting quirk (`3.0` for `6/2`,
  `1e10`→`10000000000.0`) is untouched — still in issues.md P3.

## Common-idiom lexer gaps — `@`, hyphenated numbers, `-1k` (landed 2026-06-24)

Three everyday agent inputs used to fragment into adjacent tokens and trip the
no-token-pasting guard — a *loud but wrong* parse error on ubiquitous text. All
fixed at the lexer with raw-slice tokens (so leading zeros survive), wired
through the parser exactly where `NumberIdent` already flows (argv/case/merge):

- **Bare `@`** — `@` is now an ordinary word char. Mid-word (`user@host`,
  `a@b.com`) joined the `Ident` trailing class; leading-`@` (`@scope/pkg`, `@0`,
  bare `@`) got a new `AtWord` token. `user@host:8080` still colon-merges into
  one word for free (Ident is colon-mergeable).
- **Digit-leading hyphenated words** (`2024-01-02`, `10-20`, `1.5-2`,
  `cut -f 1-3`) and **minus-led numeric predicate values** (`find -size -1k`,
  `-30d`) got one `DashNumWord` token (two regexes). A plain `2024`/`1.5`/`-1`
  stays `Int`/`Float` — the hyphen form requires a `-segment`, the minus form an
  alpha after the digits, so the two never overlap with `Int`/`Float`/`NumberIdent`.

**A deliberate decision reversed, intentionally.** `tr -d 0-9` was previously
made a *loud error* (and pinned by `bareword_comma_tests::numeric_range_is_loud_not_silent`)
because `0-9` could only fragment into `Int(0)`+`Int(-9)` and silently delete just
`0`. That was the best available then, not a desired end state. A contiguous `0-9`
the user typed is *one word*, not two pasted tokens — so it now reaches `tr`
verbatim and the range applies, matching bash/GNU. The test was rewritten to assert
the new behavior; the comma run (`echo 1,2,3`) stays a loud no-pasting error
(commas aren't part of the word). Strictly additive otherwise: every newly-valid
input was a parse error before, so no previously-valid program changes meaning.

## diff/patch improvements — `diff --json` + GNU-fuzz `patch` (landed 2026-06-22)

The diff/patch half of the [editing-for-agents](editing-for-agents.md) design
pass (the `edit` builtin was declined for kaish — embedder concern). Two changes,
both on branch `feat/diff-patch-improvements`:

- **`diff --json`** attaches structured hunks via `OutputData::with_rich_json`
  (same mechanism as `grep --json`) while the text node keeps serving pipes/humans.
  Built from `similar`'s `iter_hunks()` ranges + `iter_changes()` tags. The kernel
  only serializes the rich_json when `--json` is requested, so plain `diff` is
  byte-for-byte unchanged.
- **`patch` gained GNU-style offset search + fuzz**, replacing the
  stricter-than-GNU exact-context matcher that hard-failed on any drift (an
  "attractive nuisance" — agents pipe a near-miss diff, it rejects, they loop). The
  new pure `apply_hunks` locates each hunk by matching its context near the
  header position (searching outward → *offset*) and, failing exact, trimming up to
  2 context lines per end (*fuzz*); only the verified span is rewritten, so fuzz
  never overwrites unverified lines. Offset/fuzz are reported loudly (`Hunk #N
  succeeded at L (offset O lines)`); no-match still fails loud (`Hunk #N FAILED`),
  file untouched. Applied via a whole-file `PatchOp::Replace { offset:0, expected:
  Some(original) }` — TOCTOU-safe CAS, uniform across local/overlay backends.

**Decisions worth keeping:** the strict→fuzzy relaxation is *within one algorithm,
reported* — explicitly NOT the rejected "hijack patch → auto content-anchor on
failure" (a silent algorithm switch, the footgun kaish forbids). Reflex survey
showed agents reach for `patch`/`sed -i` by reflex; research showed line-numbered
diffs are the format LLMs generate worst — so `patch` stays a faithful workalike
and the reliable content-anchored path lives in the embedders (kaijutsu's hashline
`edit`), not here. The `apply_hunks` core is pure and unit-tested (clean/offset/
fuzz/no-match/reverse); execute-level tests confirm loud non-destructive failure.

## OverlayFs — copy-on-write overlay (landed 2026-06-10)

A CoW overlay filesystem composing with `LocalFs`/`MemoryFs`, for kaibo coder
workspaces, kaijutsu context forking, and kaish `--overlay` session mode. Landed
across `69c42e3`+`2a62a72` (MemoryFs lift + overlay core), `dddc85d` (accounting:
`resident_bytes`, `ByteBudget`), `6fe2225` (inspection API), and `d0e0deb` (kernel
surface: 64 MiB agent budget "vfs-memory", `MountInfo.resident_bytes` +
`kaish-mounts`, `--overlay` opt-in across REPL/embedder, `kaish-vfs` builtin
`status`/`diff`/`commit`/`reset`). Full design: [kaish-overlayfs.md](kaish-overlayfs.md).
Deliberately punted in core (per-file whiteout only, layer-local symlink
resolution, no mtime propagation in `commit_into`) — the live residuals are in
issues.md.

## Binary data & strict UTF-8 (landed 2026-06-13)

kaish was UTF-8 text end to end (`ExecResult.out: String`, string-shaped
`OutputData`, pipes consumed as text), so raw bytes couldn't transit and
`/dev/urandom` couldn't exist. Two intertwined efforts fixed that.

**Strict decodes.** The in-process text builtins used to `from_utf8_lossy`
stdin/files — silently replacing invalid bytes with `U+FFFD`, corruption that
looks like success. Replaced with a strict `ExecContext::read_stdin_to_text()`
(errors on non-UTF-8) wired through every text builtin; text tools now loud-error
on binary while byte-aware tools consume it. A same-pass bug: `accumulate_result`
folded every top-level statement via `push_out(text_out())`, lossy-decoding *any*
`Bytes` final result (so even a standalone `cat blob.bin` came back mangled) — now
concatenates raw bytes when binary is involved. A round-2 DeepSeek+Gemini review
caught more silent-corruption sites (cat as a pipeline's last stage, `$()`/block
capture, command-subst-in-string, `kaish-last`, background-job output, `for` over
`Bytes`, `dd skip*bs` overflow), all fixed with regression tests in
`sandbox_mode_tests`.

**Typed `Bytes`.** Design (nushell-style, full plan in
[binary-data.md](binary-data.md)): a `Value::Bytes`/`OutputData::Bytes` that flows
through pipes, coerces to text iff valid UTF-8 (else loud error), and renders at
the boundary (REPL hex dump, `--json`/MCP base64 envelope). All phases landed
(`e612d69` → `818a22e`): the value + boundary type (dead `Value::Blob`/`BlobRef`
deleted), byte-clean transit + redirects, `dd` + `/dev/urandom`/`/dev/random`,
byte-aware movers (cat/head -c/tail -c/base64/xxd/checksum/wc -c/tee/cmp), and
external-command capture → `Bytes` with raw stdin forwarding. North-star test
(`dd if=/dev/urandom of=/dev/null bs=1024 count=10` copies exactly 10240 bytes;
two draws have differing checksums) lives in `sandbox_mode_tests.rs`; DevFs in
`crates/kaish-vfs/src/dev.rs`. **Decisions:** no generic `encode`/`decode`
(`base64`+`xxd` already bridge; a generic pair invites a basenc format×flag
matrix); no `random` builtin (`dd if=/dev/urandom` covers it).

## Streaming file reads (landed 2026-06-14)

Scan-oriented builtins stopped reading whole files into memory. Mechanism respects
kaish-vfs's runtime-free trait (no `AsyncRead`/tokio in `Filesystem`, so WASI stays
clean): `LocalFs::read_range` does a true positional `seek + take`, and
`ExecContext::read_file_chunked` pulls a file forward in 256 KiB windows. Landed:
`wc` (`a610044`), `checksum` (`770766c`), `grep` single-file simple path
(`0fce8c4`), `cmp` lockstep two-file (`dcb806c`), `cat` single-file piped
(`e220767`). Each has a parity test against the *production* whole-buffer path —
that lesson came from a grep draft that hardcoded `byte_offset=0`/`path=null` in
`--json` and passed a text-only self-comparison. Deliberately NOT streamed
(`sort`/`uniq`/`diff`/`jq` and anything emitting structured `.data` — the
`.data`/`OutputData` channel is whole-value by design).

## Output disk-spill — read-only safety (core fix 2026-06-06)

`OutputLimitConfig` carries a runtime `SpillMode` (`Disk` | `Memory`); `Memory`
truncates in memory with no disk I/O. `Kernel::assemble` auto-forces `Memory` for
`NoLocal` mounts, and (fixed 2026-06-08) for any `with_backend` kernel — a
custom-backend kernel owns no host mounts, so a host spill is always a VFS bypass.
This closed the kaibo gap (kaibo uses `with_backend` and didn't opt into `Memory`).

## Composable help (Phases 1–3, 2026-06-06)

The `kaish-help` crate (concept fragments + `compose`/recipes + byte-stable
`get_help`) became the single source for help content. `syntax.md` is generated +
drift-tested; the REPL welcome, the `execute` tool description, and the embedder
prompt set all compose from it (no hand-rolled prose left). Phase 4 publish-half
done 2026-06-08 (`kaish-help` 0.8.0 on crates.io). Full design + resolved
decisions: [composable-help.md](composable-help.md).

## Test fortification (through 2026-06-14)

The 2026-06-09 Fable 5 systemic review (61-agent fleet) drove a fortification pass.
Closed: destructive rails, the realworld port (48 tests through `kernel.execute`,
which immediately caught the grep/rg value-flag binding bug), the `--json` sweep
with a drift guard, snapshot-isolation races, tautological asserts, external-argv
no-split, kill/wait e2e. Lexer negative tests tightened to assert exact
`LexerError` variants (`2026-06-14`), which surfaced a real diagnostic gap (an
unterminated `"string` emits the generic `UnexpectedCharacter`, not the curated
`UnterminatedString`). Seven never-emitted `IssueCode` variants were removed as
silent aspiration; E011/E006/E007 later re-added *with* real `validate` emitters,
honoring the no-variant-without-an-emitter rule.

## The Feb-2026 P0 bugs (validated fixed 2026-04-16)

The original P0 from the Feb-2026 integration-test pass — unknown-command socket
error, missing `$0`, alias concatenation, subprocess capture, arithmetic token
leak — all validated as fixed and were retired from the punch list.

---

## Standing decisions (don't re-litigate)

- **No rustfmt (2026-06-14, Amy).** The audience for this code is Claude and other
  agents, not human reviewers, so rustfmt's payoff is mostly cosmetic; the compiler
  + `cargo clippy --all --all-targets` are the real gate. A first `cargo fmt --all`
  rewrites ~202 files / ~2127 hunks and no small config shrinks it meaningfully
  (`use_small_heuristics` Off/Max both make it worse; the knobs that would preserve
  the hand-style are nightly-only). No `rustfmt.toml` is added (a dormant one makes
  ad-hoc `cargo fmt --check` fail confusingly). If ever revisited: its own commit,
  pinning `edition = "2024"`.
- **No unquoted token-pasting (2026-06-08).** A run of adjacent *unquoted* lexemes
  is never concatenated into one word — `/tmp/$(echo x).txt` lexes as three tokens.
  The quoted form (`"/tmp/$(echo x).txt"`) is the supported idiom and aligns with
  the `shellcheck --enable=all` north star (SC2086). kaish guides agents to write
  reliable scripts; "always quote interpolated words" is simpler and lint-aligned
  than bash's implicit pasting. (Live polish residuals — redirect-target hint,
  post-`--` glue — are in issues.md.)
- **No generic `encode`/`decode`; no `random` builtin (2026-06-13).** See the
  binary-data section above.
- **No `test` builtin and no `[` command (2026-06-25, Amy).** Conditionals go
  through `[[ … ]]` only. The old `test`/`[` builtins were removed: `[`
  (`Bracket`) was effectively dead (it can't be a command name — `[` lexes as
  `LBracket`, absent from `command_parser`'s `command_name`), and `test`'s
  binary-operator half (`test a = b`, `-eq`, `<`, `>`) never reached the builtin
  because `=`/`!=`/`<`/`>` lex as operator tokens the argv parser rejects in
  argument position — only unary `test -z`/`test -f` actually routed. Rather than
  do the delicate, broad-blast-radius parser change to accept those operator
  tokens as barewords (without breaking assignment `x=y`, glob char-classes
  `ls [ab]*`, or `[[ ]]`), we keep `[[ … ]]` as the single test form. **Why
  `[[ ]]` wins:** it's real grammar the validator sees, so a malformed test /
  unknown operator / unquoted expansion is caught *before* runtime; `test`/`[`
  hide their operators as runtime string args (the late-failure footgun kaish
  exists to remove). The fully-written `test`/`[` builtin (with passing
  *direct-`.execute()`* unit tests — the gotcha CLAUDE.md warns about: they
  bypassed the real lex/parse path) was the tell that this never worked end-to-end.
  **Guard (gemini-batch review of PR #29):** a bare `test` in a subprocess build
  falls through to an external `/usr/bin/test` that evaluates against the *real*
  host FS, bypassing the VFS/overlay — a silent wrong boolean into `if`/`&&`. We
  added validator advisory **W006** (`IssueCode::PosixTestCommand`) that steers to
  `[[ … ]]` and *still runs* (Amy chose warn-don't-reject over a poison stub). It's
  the first agent-surfaced validation warning: warnings were trace-only because
  every external command fires `UndefinedCommand`, so a code now opts into
  surfacing via `IssueCode::surfaces_to_agent` — the seam for the broader P4
  "did-you-mean" pass. Surfacing is dual-path: the streaming frontend gets it via a
  pre-loop `on_output` emission; `kernel.execute` reads it off the aggregate
  `result.err` (the two consumers are disjoint, so it prints exactly once each).

## Accepted risks & waived items (decided, not open work)

These were on the issues.md punch list but reached a verdict — recorded so the
deferral stays a decision, not drift. Reopen only if a real failure surfaces.

- **Non-Linux `kill` keeps PID-based signalling.** `pidfd` is Linux-only;
  elsewhere we `kill(pid, sig)` and accept the PID-reuse race for the direct
  child. Acceptable — kaish runs predominantly on Linux.
- **Process-group kill PID-reuse window.** The PG-wide kill that catches
  grandchildren goes through `killpg(pgid, sig)` — no PGID equivalent of pidfd.
  If a leader is reaped and its PGID reused before `killpg` fires, an unrelated
  group could be signalled. Mitigations (cgroup v2 `cgroup.kill`,
  `PR_SET_CHILD_SUBREAPER`) are significant complexity; deferred until a real
  failure.
- **`JobManager::spawn` busy-waits** with `std::hint::spin_loop()` for immediate
  visibility — works, wastes CPU under contention. Channel coordination would be
  cleaner; no trigger.
- **`head -n -0` / signed-zero line counts (waived).** `-0` lexes as `Int(0)`
  (sign lost at the lexer) and `line_spec` only treats a *negative* Int as
  all-but-last, so `head -n -0` prints nothing instead of the whole file. Not
  fixable without lexer changes to preserve signed zero. Obscure; waived.
- **`mktemp` random-suffix modulo bias (recorded by choice).** `byte % 36` skews
  the first four alphabet chars (~3.1% vs ~2.7%, since `256 % 36 = 4`). Negligible
  for temp suffixes; the rejection-sampling fix complicates the
  fail-loud-on-no-entropy contract.
- **`uname -v` discloses build provenance unconditionally.** Formats
  `kaish {version} ({git_hash} {build_date})` from compile-time `option_env!`; an
  embedder that sets `KAISH_GIT_HASH`/`KAISH_BUILD_DATE` fingerprints the exact
  commit even in a minimal build. Gate behind a `verbose-identity` feature only if
  a threat model cares.
- **`mktemp` entropy-failure message is unhelpful on wasm.** A
  `getrandom::fill` failure on `wasm32-wasip1` surfaces a near-empty `Display`.
  Add a `cfg!(target_arch = "wasm32")` hint if it ever matters.
- **`to_argv()` flattens a repeatable scalar array to one JSON token (no live
  trigger).** `ParamSchema.repeatable` stores repeated single-value flags as
  `named[key] = Json(Array([scalar, …]))`; `to_argv()` only splits the
  *array-of-arrays* (`consumes > 1`) shape, so a flat scalar array becomes one
  JSON-text token. `sed` is unaffected (it reads the raw `ToolArgs`); a future
  repeatable-flag builtin that trusts its clap struct after a `to_argv()`
  round-trip would see one mangled value. The real fix needs schema context in
  `to_argv` (it has none). Record-then-defer until a builtin hits it.
