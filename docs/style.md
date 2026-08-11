# kaish writing style

kaish keeps a small, predictable subset of `sh`, chosen so muscle memory transfers. This
guide keeps a small, predictable subset of English, chosen for the same reason. A reader
who understands the language already understands the prose.

These are weights, not gates. There is no linter and there is no compliance pass. Apply
them when you write, and groom the text you touch.

Inspired by the structure of ASD-STE100 Simplified Technical English — a small constraint
set plus a project term list — but not STE and not claiming to be. The STE approved-word
dictionary is copyrighted and aerospace-shaped, so we keep our own.

## Where the weights apply

| Weight | Files |
|---|---|
| Full | `crates/kaish-help/content/en/`, fragment bodies in `crates/kaish-help/src/fragments.rs`, every builtin `description`, `about`, example label, and `///` argument doc, and **every error and diagnostic string a builtin or the kernel returns** |
| Partial (terms and boundary; relax the rest) | `docs/LANGUAGE.md`, `docs/EMBEDDING.md`, `docs/NAMING.md`, and `///` rustdoc on `pub` items in `kaish-kernel`, `kaish-types`, and `kaish-tool-api` |
| Terms only, plus one line per bullet | `CHANGELOG.md` |
| Terms only | `README.md` and the design docs under `docs/` |
| Exempt | `signoff.md`, `docs/designing-syntax-with-llms.md` |

Exempt text tells a story from a point of view, and a story needs a voice.

Error strings are full weight because an agent reads them more often than it reads any
help topic. A failure message is the one piece of our prose that arrives exactly when the
reader is stuck.

`CHANGELOG.md` is the one place where "Keep the why" does **not** win. A bullet carries
the rule and one clause of rationale, on one line. The full narrative belongs in the pull
request body, which becomes the merge commit. If a bullet needs three numbers and three
reasons, it is three bullets.

kaibo and kaish-extras adopt this guide by reference as they evolve. kaijutsu is exempt.

## The weights

### Subset, not slang

Keep the vocabulary small. This is a constraint on how many **distinct** words the corpus
uses, not on how long the text is — a smaller vocabulary usually costs words, and that is
the correct trade.

The class to avoid is the metaphor that names a mental act as a physical one: "reach for,"
"the defensive-quoting dance." A reader who learned English second, or a model working
from a partial context, cannot recover the intent from the figure.

Some idiom is load-bearing and stays. `muscle memory` names the design thesis in two
words. `footgun` names a hazard class we ship a fix for. `escape hatch` names a design
commitment: kaish restricts, and every restriction ships a documented way out. Treat these
as terms, not as decoration.

**The list grows only on evidence.** A candidate must already be in consistent use across
the corpus with one meaning — never on the argument that it would read well. `escape
hatch` was on the banned list above until we counted: about thirty uses, one sense, no
drift. The corpus was right and the guide was wrong, so the guide changed. Count the uses
before you argue.

Borrowed jargon is a separate problem from metaphor. When a tool has a private word for
something the reader can count, use the reader's word: `dhat` calls an allocation a
"block", so a changelog says "18% fewer allocations", not "18% fewer blocks".

American spelling, to match the corpus (`modeled`, not `modelled`).

> Before: Reach for `test` for muscle memory or where a plain command is wanted.
>
> After: Use `test` when you want a plain command, or when the `sh` habit is faster to type.

### One term, one meaning

Pick one word for each concept and keep it. Do not vary the word for style — a synonym
reads as a new concept, and the reader spends attention deciding whether it is one.

Terms that carry a guarantee live in the table in `CLAUDE.md`, which is the source.
`README.md` mirrors it for readers who never open `CLAUDE.md`; keep the two in step. This
guide does not copy it — three copies drifted within a day of being written.

`dialect` is reserved for its technical senses: a ShellCheck language mode, or a regex
flavor. Do not use it about prose.

`surface` is doing too much work across the corpus ("the real surface for", "two execution
surfaces", "the surface to use for"). In reader-facing text, name the thing: the tool
schema, the error message, the help topic.

**Example labels are imperative.** "Send STOP by name", not "Named shorthand" and not
"Alternation (ERE or GNU BRE)". The label sits next to a command, so it should read like
one.

**Cross-references take one form:** ``see `help <topic>` `` for a help topic, and
`docs/LANGUAGE.md`, "Section name" for the reference. Link rather than re-explaining.

### State the number

Agents act on our numbers. Give the exact exit code, the exact size, the exact flag, and
the exact default. A vague verb is a defect in this corpus.

> Before: Oversize output fails.
>
> After: Oversize output spills to a file and exits 3.

State the default and the condition too: "reads stdin when no files are given," "off by
default; applies to `-r` only."

### Fail loud

Put the constraint and its consequence at the front of the sentence. Do not bury a hazard
in a subordinate clause, and do not soften it with a hedge. This mirrors what the shell
itself promises: the boundary is loud.

The first sentence must also work alone. The always-on onboarding spine is capped at 3500
characters (`compose.rs`, `onboarding_spine_stays_within_budget`) and readers skim, so
write so that a truncated fragment still carries the rule.

> Before: Note that files removed this way may not be recoverable in some configurations.
>
> After: `rm` deletes the file permanently unless `set -o trash` is active. Turn on `trash`
> first if you want a recoverable copy.

### Keep the why

A rule earns its rationale. The house pattern is `<rule> — <why>`, and the clause after
the dash is load-bearing: a reader who knows why can guess correctly at the edges, and a
reader who has only the rule cannot.

When a sentence gets tangled, split it. Never drop the rationale to fit. There is no word
budget — counting words instead of judging the sentence is how this weight goes wrong.

**When the source records no rationale, leave the rule bare.** Do not invent one. A bare
rule next to an explained one is honest, and it marks where a real answer is missing.

**Contrast is a rationale.** Comparing against bash is one of the most effective moves in
this corpus, and it is endorsed: "Bash splits unquoted `$VAR` on `$IFS`; kaish never does."

**Tables carry the same weights.** A table cell is prose with the subject moved into the
column header. Write cells as complete clauses — a fragment forces the reader to
reconstruct the verb, and a model reading one cell out of context cannot. Put the rule in
the cell and the rationale after a dash. Expect a table rewritten this way to get longer.
That is the correct trade.

### Do not leak the kernel

Reader-facing text describes what the reader must predict. The test is not whether a
sentence names an internal — it is whether the reader needs that internal to predict
behavior. `[[ ]]` lexes as two bracket tokens is a mechanism *and* the whole contract for
why `[ -f x ]` fails, so it stays. `to_argv()` joins the pair is neither, so it goes.

The boundary has a precise location in the builtins. A `///` comment on an **argument** is
published: `params_from_clap` copies it into `ParamSchema.description`, the kernel exposes
it through `Kernel::tool_schemas()`, and the embedder ships it to the model. A `///` on the
**struct** is never published — `schema_from_clap` reads `cmd.get_about()` instead — so
struct docs and `//` comments are both safe places for mechanism.

> Before: `/// Unset a variable (-u VAR). Repeatable: -u A -u B. Clap sees a single`
> `/// occurrence via to_argv() ... This field is a validation sink only.`
>
> After: `/// Unset a variable (-u VAR). Repeatable: -u A -u B.`

A **blank `///` line** is the third safe place, and the least obvious one. clap splits a
doc comment there: everything before the blank line becomes short help, everything after
becomes long help, and `params_from_clap` publishes short help only. `env`'s `-u` and
`uname`'s `--host` both keep four lines of mechanism this way, directly under the field
they explain, and neither ships a word of it. Use it when the mechanism belongs next to
the published line rather than below it.

That split is also why you cannot audit this by reading the source. A reviewer grepping
`env.rs` for `to_argv` finds the mechanism and reports a leak that does not exist; only
the published surface settles it. Read `Kernel::tool_schemas()`, or run the test named
below.

**When you touch a builtin, audit every `///` on its clap struct.** Grooming alone cannot
reach this class: the mechanism leaks sit in files nobody has reason to open, so the audit
has to ride along with any visit to the file.

### Groom at the point of touch

When you edit a file, bring the part you edited into voice. Leave the rest alone.

We are not scheduling a rewrite. A bulk pass would freeze this guide before we know
whether it works, and it would separate the style decision from the person who understands
the text. Grooming keeps both together.

## Known debt

These are real violations, found by cross-model review of this guide. They are recorded so
that whoever next touches these files knows to fix them, not as a rewrite plan. Clear an
entry when you fix it, and add one when you find a violation you are not fixing today.

- Example labels have not caught up with the imperative ruling. About a hundred of the 311
  labels in the builtin corpus are still noun phrases ("Case-insensitive", "Compact
  notation (default)"). Fix the ones in a file you are already editing; there is no sweep
  scheduled, deliberately — the label needs the person who understands the example.
- Error and diagnostic strings are full weight and largely unswept — about 745 failure
  sites. An agent reads a failure message more often than any help topic, so this is the
  highest-value surface left. A first pass found seven that name something the reader
  cannot act on: `exec.rs:81` and `spawn.rs:103` (`allow_external_commands=false`, a
  `KernelConfig` field — `env.rs:180` says "sandbox mode" for the same condition and reads
  better), `timeout.rs:134` (`into_arc()`), `kaish_vfs.rs:39` and `:410`
  (`KernelConfig::with_overlay(true)`, the `localfs` feature), `kill.rs:518`, and
  `uname.rs:208` (cargo feature names).
- **Open question on those seven:** some are arguably not leaks but deliberate
  dual-audience messages. `kaish_vfs.rs:39` names the REPL flag *and* the embedder call,
  each labeled, and a `timeout` dispatcher error can only be fixed by an embedder — so the
  reader who can act IS the embedder. Decide the rule before rewriting them; the guide's
  own test ("does the reader need it to predict behavior") does not settle who the reader
  is when a builtin fails for a reason only the host can change.
- Two messages state a constraint without its grammar: `sleep.rs:69` ("invalid time
  interval" — no value, no accepted forms, unlike its sibling at `:107`) and
  `kaish_trash.rs:196` (value but no suffix rules). `timeout.rs:100` is the model to copy.
- Tools behind a capability feature are only walked by the mechanism-leak test when the
  build enables them. Run it with `--features full` after touching `timeout`, `tokens`,
  or `ps`; the default CI run does not see them.

Mechanism leaks in **published param descriptions** are now a test, not a rule:
`crates/kaish-kernel/tests/published_prose_tests.rs` walks the live registry and fails on
`to_argv`, `consumes=`, `args.positional`, `clap`, and friends. It was written to lock in
the sweep below and immediately found thirteen builtins the hand-audit had missed — which
is the argument for keeping it. A `///` on a **hidden positional** is published:
`params_from_clap` keeps hidden positionals on purpose, because they are the real operand
surface for `cat`, `mkdir`, and the rest.

Cleared: twenty-three builtins' `Sink —` docs, which published clap mechanism as the
entire description of a parameter; `jq_native.rs`'s `consumes=2`, `See _arg above`, and
its cross-reference to Rust module docs; `sed.rs`'s `(clap Append → schema repeatable)`,
its `-i.bak` lexer note, and its cross-reference to the deleted `issues.md`; the
`bg`/`fg`/`wait` "or PID" string, which described a code path that does not exist; and the
four "reach for" and one "defensive-quoting dance" sites in help content, `fragments.rs`,
and `docs/LANGUAGE.md`. `escape hatch` left this list by becoming a term rather than by
being rewritten — see "Subset, not slang".
