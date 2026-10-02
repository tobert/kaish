# kaish writing style

This guide applies to every published word: help content, docs, error messages, `///`
on builtin arguments, CHANGELOG entries, commit messages, and PR bodies.

kaish keeps a small, predictable subset of `sh`, so existing shell skills transfer. This
guide keeps a small, predictable subset of English for the same reason.

## Vocabulary choices

Keep the vocabulary small. This limits the number of distinct words, not the length of the
text — familiar words may require a longer sentence.

Use plain words instead of figures of speech. Make the intended meaning available from the
words themselves, including in second-language or partial-context use.

Use an established technical term when kaish gives it one meaning. For example:

| Write | Meaning |
|---|---|
| affordance | A visible cue for the next available action. An error that names its fix affords that fix. |
| familiar syntax | Existing `sh` skill transfers because kaish preserves familiar syntax. |

`hazard` and `override` belong to this vocabulary too; they carry guarantees, so their
definitions live in the Terms table below, with every other term that carries a
behavioral guarantee.

Use American spelling to match the corpus: `modeled`, not `modelled`.

## One term, one meaning

Pick one word for each concept and keep it. Do not vary a word for style.

`dialect` is reserved for a ShellCheck language mode or a regex flavor. Do not use it
about prose.

`surface` can hide the thing it names. In published text, name the tool schema, error
message, help topic, or API.

Example labels are imperative. Write "Send STOP by name," not "Named shorthand." The
label sits next to a command, so it should read like one.

Cross-references take one form per target: ``see `help <topic>` `` for a help topic, and
`docs/LANGUAGE.md`, "Section name" for the language reference. Link instead of
re-explaining.

This section keeps one term for each concept because one term, one meaning applies to the
guide itself.

## Provide specific values

Whenever it's practical, provide the public exit code, size, flag, default, and condition.
This saves round trips to get more information and gives agents clear observations for
updating their model of the world.

> Before: Oversize output fails.
>
> After: Oversize output spills to a file and exits 3.

State the default and condition too, for example: "reads stdin when no files are given"
and "off by default; applies to `-r` only."

## Fast and informative failures

Make errors, warnings, and failures informative and, where possible, instructive.
Lead with consequences, name conditions, and suggest next steps when they are known.

Errors that face users, agents, and models must not leak internals. Internal code names
and references will be unresolvable and should only be exposed for assertions and errors
that indicate a real problem in kaish.

An error is feedback, not a problem. A model that reads `` write `10#$m` `` gets it right
next turn; a silent coercion teaches nothing. When the choice is between degrading quietly
and refusing with the fix named, refuse. Keep the text clinical: the value, the rule in
a few words, the fix.

## Published builtin text

A `///` comment on a builtin argument is published to agents. `params_from_clap` copies
it into `ParamSchema.description`, and the kernel exposes it through
`Kernel::tool_schemas()`. Describe the argument's behavior there. Put implementation
notes in `//` comments.

A `///` comment on the clap struct is not published; `schema_from_clap` reads
`cmd.get_about()` instead. Struct docs and `//` comments are safe places for mechanism.

A blank `///` line splits clap short help from long help. Everything before the blank
line is published; everything after it is not. Use the split when an implementation note
belongs next to the field.

Do not infer the published text by grepping the source. Read `Kernel::tool_schemas()` or
run the published-prose test. When modifying builtins, audit every `///` on its clap
struct to ensure code and documentation stay synchronized.

## Write for model context

Use the same prose in human and model contexts. Assume the context may be truncated and
lead with the most important information. Teach syntax with examples. Repeat important
rules.

## The example is the rule

Show the correct example before explaining it. Continue the correct pattern when the
surrounding prose is missing. Make the example carry the rule by itself.

> Before: **Quote to join.** `$VAR`, `$(cmd)`, and globs are each a separate word unless
> quoted — kaish never pastes adjacent unquoted tokens.
>
> After: `"$dir/file.txt"` — one path. kaish keeps `$VAR`, `$(cmd)`, and globs as
> separate words; quote the whole word to join text with interpolation.

Avoid incorrect examples. When one is necessary, put the correct form first and the
clearly marked error next to it:
`echo "$dir/file.txt"`; `echo $dir/file.txt # error — quote the whole path`.

## Terms

These are the terms that carry a stable definition. **This table is the source.**
The list grows when a collision appears in real prose, not in advance.

| Term | Part of speech | Meaning |
|---|---|---|
| hazard | noun | A condition with a predictable failure. Prose names the hazard and the fix kaish ships for it; neither leads. |
| override | noun | A documented, supported way past a restriction kaish enforces — `-E` out of GNU BRE, `--lines` out of JSONL rows. An override is designed and documented intentionally. |
| fail loudly | adjective, verb phrase | An error is explicit and immediate. kaish never continues on a wrong assumption. |
| builtin | noun | An embedded Unix-like tool that runs inside the kernel process. |
| external command | noun | A program the kernel runs on the underlying system via execve(2) family, often via `$PATH`. |
| kernel | noun | The kaish execution core. Not the OS kernel. |
| mount | noun, verb | A path prefix bound to a filesystem, or the act of binding one. |
| typed | adjective | A value keeps its JSON type through substitution. It is not stringified. |
| overlay | noun, adjective | Copy-on-write mode. Writes land in a virtual upper layer until committed. |
| spill | verb, noun | To write oversize output to a file, or the file that results. |

