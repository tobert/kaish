# kaish writing style

kaish keeps a small, predictable subset of `sh`, chosen so existing `sh` skill transfers.
This guide keeps a small, predictable subset of English, chosen for the same reason. A
reader who understands the language already understands the prose.

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

Colloquial hazard metaphors are retired as terms (2026-08-17). They read
human-friendly, but the usual reader is a model, and a colloquialism lights
pathways trained on other people's prose — the reader gets the affect without
the definition. The migration notes live in the working plan, not here; this
guide states what to write instead, defined the way the term table in
`CLAUDE.md` defines one:

| Write | Meaning |
|---|---|
| hazard | A condition with a predictable failure. Name the hazard and the fix kaish ships for it; lead with neither. |
| override | A documented, supported way past a restriction kaish enforces. Never a workaround: an override is part of the design, and every restriction that has one names it. |
| affordance | What an output signals can be done next — an error that names its fix affords the fix. Design the output so the next action is visible in it. |
| plain words | For the design thesis, write "familiar syntax" or "your `sh` habits apply" — existing skill transfers because the syntax is a subset, not because of a property of memory. |

Vocabulary comes from the fields kaish is built in — cybernetics, cognitive
science, resilience and reliability engineering — and from user-experience
design. Prefer their terms (constraint, guarantee, hazard, override, recovery,
affordance) over colloquialism, and keep each term to one meaning. This table
is writing guidance; the terms that carry a guarantee live in the table in
`CLAUDE.md`.

**The list grows only on evidence.** A candidate must already be in consistent use across
the corpus with one meaning — never on the argument that it would read well. One
candidate once won its count — about thirty uses, one sense, no drift — and the guide
changed to match the corpus. Count the uses before you argue.

Evidence also retires a term. The same word's count re-opened on 2026-08-17, when
the audience sharpened: most readers are models, and a colloquialism that lands
with a human lights the wrong pathways in a model. The uses were consistent; the
reader had changed. The table above is the rule now.

Borrowed jargon is a separate problem from metaphor. When a tool has a private word for
something the reader can count, use the reader's word: `dhat` calls an allocation a
"block", so a changelog says "18% fewer allocations", not "18% fewer blocks".

American spelling, to match the corpus (`modeled`, not `modelled`).

> Before: Reach for `test` where the plain-command form is wanted.
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

### Writing for the model reader

The reader is usually a language model: it skims under truncation, it
pattern-matches from examples, and it meets a rule the second time as an error
message. These weights apply on top of the ones above, full weight wherever the
table says full weight.

**Example before rule.** The example is the rule; the sentence after it names what
the example shows. A model completes patterns it saw, not rules it read — a
section that leads with prose teaches the skim path nothing.

> Before: **Quote to join.** `$VAR`, `$(cmd)`, and globs are each a separate word
> unless quoted — kaish never pastes adjacent unquoted tokens.
>
> After: `"$dir/file.txt"` — one path. kaish keeps `$VAR`, `$(cmd)`, and globs
> each a separate word; quote the whole word to join text with interpolation.

**Show the wrong way, marked.** The reader's prior already contains the wrong
form, so print it and cross it out: the correct form first, the wrong form
explicitly marked (`# error — use …`), the two adjacent. Never show an unmarked
wrong example — a code block reads as an invitation, and an invitation gets
accepted.

> Before: Unquoted text adjacent to an expansion is a PARSE ERROR (quote the word).
>
> After: `echo "$dir/file.txt"` is one path; the unquoted `echo $dir/file.txt`
> is a parse error — shown marked, next to the fix, wherever it appears.

**Quote the error.** A rule that fails loud quotes its failure text, and the
failure text names its help topic — the reader meets the rule twice, once in each
direction, and the second meeting is where recall happens. `help syntax` says a
bare `for x in $xs` is an error (E012); the E012 message says "bare variable in
for loop iterates once" and hands back six runnable fixes. Keep both ends of that
loop in sync when either end changes.

**Three registers.** Each guarantee lives in exactly three working places —
the onboarding spine, the topic doc, and the error string — and nowhere else.
Repetition across those registers is design: a model that missed the spine still
meets the rule in the topic, and again in the error. The README and the help
overview may *name* a guarantee — they are the invitation — but they do not
restate its mechanism. A fourth working copy is drift waiting to happen — when
a guarantee needs a new home, vacate an old one.

**Separate the embedder from the agent.** When only the host can fix a failure,
write "this session is configured to…" — name the setting in prose, not the Rust
struct. `KernelConfig::` spellings belong in `docs/EMBEDDING.md`, or in a message
that labels the embedder as the reader who can act. This is the dual-audience
rule: the test is still "can the reader act on it", and when the answer is
"only the embedder", the message writes for both readers and labels which half
can act.

### Groom at the point of touch

When you edit a file, bring the part you edited into voice. Leave the rest alone.

We are not scheduling a rewrite. A bulk pass would freeze this guide before we know
whether it works, and it would separate the style decision from the person who understands
the text. Grooming keeps both together.
