# kaish writing style

kaish keeps a small, predictable subset of `sh`, so existing `sh` skill transfers. This
guide keeps a small, predictable subset of English for the same reason. Transfer language
skill into prose skill.

These are weights, not gates. There is no linter and no compliance pass. Use the weights
when you write, and groom the text you touch.

The guide is self-referential on purpose. It uses its own terms in its own rules, reasons,
and examples. Repeat each term in a stable context until it becomes part of the working
vocabulary.

## Where the weights apply

| Weight | Files |
|---|---|
| Full | `crates/kaish-help/content/en/`; fragment bodies in `crates/kaish-help/src/fragments.rs`; every builtin `description`, `about`, example label, and `///` argument doc; every error and diagnostic string a builtin or the kernel returns |
| Partial | `docs/LANGUAGE.md`, `docs/EMBEDDING.md`, `docs/NAMING.md`, and `///` rustdoc on `pub` items in `kaish-kernel`, `kaish-types`, and `kaish-tool-api` — use the terms and respect the action boundary; relax the other weights |
| Terms and one line per bullet | `CHANGELOG.md` |
| Terms | `README.md` and the design docs under `docs/` |
| Exempt | `signoff.md` and `docs/designing-syntax-with-llms.md` |

Exempt text tells a story from a point of view, and a story needs a voice.

Give error strings full weight — make the next action visible at the point of failure,
without requiring a help topic first.

`CHANGELOG.md` is the one place where keep the why does not win. Put the rule and one
clause of rationale on one line. Put the full narrative in the pull request body, which
becomes the merge commit. If a bullet needs three numbers and three reasons, write three
bullets.

kaibo and kaish-extras adopt this guide by reference as they evolve. kaijutsu is exempt.

## The weights

### Subset, not slang

Keep the vocabulary small. This limits the number of distinct words, not the length of the
text — familiar words may require a longer sentence.

Use plain words instead of figures of speech. Make the intended meaning available from the
words themselves, including in second-language or partial-context use.

Use an established technical term when kaish gives it one meaning. Do not introduce a
term because it sounds apt — count its existing uses and check that they agree.

| Write | Meaning |
|---|---|
| hazard | A condition with a predictable failure. Name the condition and the fix kaish provides. |
| override | A documented way past a restriction. An override is part of the design, not a workaround; every restriction that has one names it. |
| affordance | A visible cue for the next available action. An error that names its fix affords that fix. |
| familiar syntax | Existing `sh` skill transfers because kaish preserves familiar syntax. |

This table uses the terms it defines: a missing fix is a hazard; a documented way past a
restriction is an override; a visible next action is an affordance. Use the terms this way
until they become ordinary kaish vocabulary.

Terms that carry a behavioral guarantee live in the table in `CLAUDE.md`, which is the
source. `README.md` mirrors that table; keep the two in step. Do not copy the table here —
three copies drift.

Use the public word instead of a tool's private term. `dhat` calls an allocation a
"block"; write "18% fewer allocations," not "18% fewer blocks."

Use American spelling to match the corpus: `modeled`, not `modelled`.

> Before: Reach for `test` where the plain-command form is wanted.
>
> After: Use `test` when you want a plain command, or when the familiar `sh` form is
> faster to type.

### One term, one meaning

Pick one word for each concept and keep it. Do not vary a word for style — every synonym
creates a second concept until the text proves otherwise.

`dialect` is reserved for a ShellCheck language mode or a regex flavor. Do not use it
about prose.

`surface` can hide the thing it names. In published text, name the tool schema, error
message, help topic, or API.

Write `boundary`, not `seam`. Use a boundary to separate available actions from mechanism
that does not affect those actions.

Example labels are imperative. Write "Send STOP by name," not "Named shorthand." The
label sits next to a command, so it should read like one.

Cross-references take one form: ``see `help <topic>` `` for a help topic, and
`docs/LANGUAGE.md`, "Section name" for the language reference. Link instead of
re-explaining.

This section keeps one term for each concept because one term, one meaning applies to the
guide itself.

### State the number

Give the exact exit code, size, flag, default, and condition. Agents act on numbers; a
vague verb hides part of the contract.

> Before: Oversize output fails.
>
> After: Oversize output spills to a file and exits 3.

State the default and condition too: "reads stdin when no files are given" and "off by
default; applies to `-r` only."

### Fail loud

Put the constraint and consequence at the front of the sentence. Do not bury a hazard in
a subordinate clause or soften it with a hedge. At a loud boundary, state what failed and
what happens next.

The first sentence must work alone. The always-on onboarding spine is capped at 3500
characters (`compose.rs`, `onboarding_spine_stays_within_budget`). Put the rule before
anything truncation can remove.

> Before: Note that files removed this way may not be recoverable in some configurations.
>
> After: `rm` deletes the file permanently unless `set -o trash` is active. Turn on
> `trash` first if you want a recoverable copy.

The second example fails loud: it leads with the consequence, names the condition, and
affords the fix.

### Keep the why

A rule earns its rationale. Use `<rule> — <why>` when it makes the relationship clear.
The clause after the dash is load-bearing: keep enough why to apply the rule at its edges.

Split a tangled sentence instead of dropping its rationale. There is no word budget;
counting words instead of judging the sentence is how keep the why goes wrong.

When the source records no rationale, leave the rule bare. Do not invent one. A bare rule
is honest and makes the missing reason visible.

Contrast can supply the why: "Bash splits unquoted `$VAR` on `$IFS`; kaish never does."

Tables carry the same weights. A table cell is prose with its subject in the column
header. Write a complete clause instead of requiring the verb to be reconstructed. Put the
rule in the cell and the rationale after a dash. A clear table may be a longer table.

This section keeps its own why: the rationale is part of the rule because it helps the
rule travel beyond the example.

### Do not leak the kernel

Describe the behavior that must be predicted. Include an internal name only when it makes
that behavior predictable. `[[ ]]` lexes as two bracket tokens explains why `[ -f x ]`
fails, so it stays. `to_argv()` joins the pair does not change an available action, so it
goes.

Separate the agent from the embedder. When only the host can fix a failure, write "this
session is configured to…" and label the action for the embedder. Put `KernelConfig::`
spellings in `docs/EMBEDDING.md`, where the embedder can act on them.

#### Published builtin text

A `///` comment on a builtin argument is published to agents. `params_from_clap` copies
it into `ParamSchema.description`, and the kernel exposes it through
`Kernel::tool_schemas()`. Describe the argument's behavior there. Put implementation
notes in `//` comments.

A `///` comment on the clap struct is not published; `schema_from_clap` reads
`cmd.get_about()` instead. Struct docs and `//` comments are safe places for mechanism.

> Before: `/// Unset a variable (-u VAR). Repeatable: -u A -u B. Clap sees a single`
> `/// occurrence via to_argv() ... This field is a validation sink only.`
>
> After: `/// Unset a variable (-u VAR). Repeatable: -u A -u B.`

A blank `///` line also splits clap short help from long help. Everything before the blank
line is published; everything after it is not. Use the split when an implementation note
belongs next to the field.

Do not infer the published text by grepping the source. Read `Kernel::tool_schemas()` or
run the published-prose test. When you touch a builtin, audit every `///` on its clap
struct — the visit supplies the context needed to judge each line.

## Write for model context

Use the same prose in human and model contexts. Assume the context may be truncated. Teach
syntax with examples. Repeat a rule in its error. These instructions strengthen the
weights above; they do not replace them.

### The example is the rule

Show the correct example before explaining it. Continue the correct pattern when the
surrounding prose is missing. Make the example carry the rule by itself.

> Before: **Quote to join.** `$VAR`, `$(cmd)`, and globs are each a separate word unless
> quoted — kaish never pastes adjacent unquoted tokens.
>
> After: `"$dir/file.txt"` — one path. kaish keeps `$VAR`, `$(cmd)`, and globs as
> separate words; quote the whole word to join text with interpolation.

Show a likely wrong form only when it is explicitly marked. Put the correct form first and
the marked error next to it: `echo "$dir/file.txt"`; `echo $dir/file.txt # error — quote
the whole path`. An unmarked code block is an affordance, even when nearby prose calls it
wrong.

The section demonstrates its rule: its examples carry useful syntax even if the
explanation is truncated.

### Close the error loop

When a rule fails loud, quote the failure text in its help topic and make the error name
that topic. Meet the same vocabulary in both directions: rule to error and error to rule.

For example, `help syntax` says bare `for x in $xs` is error E012. The E012 message says
"bare variable in for loop iterates once" and supplies runnable fixes. Keep the help and
error in step when either changes.

### Three registers

Each guarantee has three working copies: the onboarding spine, its topic, and its error
string. Repetition across these registers is design. Miss the spine, then meet the rule in
the topic; miss the topic, then meet it in the error.

The README and help overview may name a guarantee as an invitation, but they do not repeat
its mechanism. A fourth working copy creates drift. When a guarantee needs a new home,
vacate an old one.

The terms recur across all three registers because one term, one meaning makes repetition
useful instead of noisy.

## Groom at the point of touch

When you edit a file, bring the part you edit into voice. Leave the rest alone.

Do not schedule a bulk rewrite. A bulk pass freezes the guide before use has tested it and
separates the style decision from the person who understands the text. Grooming keeps the
decision and its context together.
