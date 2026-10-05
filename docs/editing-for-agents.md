# Editing files from an agent shell: `diff`, `patch`, and `edit`

*A design pass over kaish's diff/patch surface, run through the
[LLM-panel method](designing-syntax-with-llms.md) (existing-tool variant) and
grounded in 2025–2026 research on how models actually edit code. Decides what
stays a workalike, what relaxes, and how content-anchored editing came to kaish
as `edit`.*

---

## The finding

The reflex survey (cheap-tier panel: DeepSeek-V4, Claude Haiku) says agents reach
for `sed -i` (single edit) and `patch -p1 < diff` (multi-line) **by reflex** — so
those forms must work. The research says the opposite of what the reflex implies
about *reliability*:

- **Line-numbered unified diffs are the format LLMs generate worst.** They can't
  count lines or get hunk-header arithmetic (`@@ -N,M +N,M @@`) right. ("To Diff
  or Not to Diff?", ACL Findings 2026 — number-indexed diffs "highly fragile due
  to precise numerical offsets".)
- **The field converged on content-anchored editing.** OpenAI V4A `apply_patch`
  (context anchors, *no line numbers*), Anthropic `str_replace` (unique
  `old_string`→`new_string`), Aider search/replace (flexible patching → **9×
  fewer edit errors**).
- **Naive content-anchoring still fails ~35%** — whitespace/tabs, non-unique
  anchors, delimiter collisions with real file content. So the *failure-mode
  design is the design*, not an afterthought.

Reflex and reliability diverge. kaish keeps the reflex tools honest (a
POSIX-80% shell), and the content-anchored tool that addresses the reliability problem is `edit` (see
"`edit` — anchored line edits").

---

## Decisions

### `diff` — stays a workalike, gains structured output

`diff` is a *producer*; agents read diffs fine. Keep the `similar`-backed unified
diff. Two adds:

- **`--json`** — emit structured hunks (`OutputData`) instead of only text, so a
  pipeline can consume hunks without re-parsing `@@` headers. kaish-native, low
  cost.
- **Fix `diff -C 3 -C 4` arity miscount** (known P4; `context_steals_positional`
  subtracts one for a deduped `-C`).

### `patch` — relax from over-strict to *faithful* GNU (loud)

Today kaish's `patch` is **stricter than GNU**: it hard-errors on any hunk
line-count or context mismatch, with zero fuzz and no offset search. That is an
*attractive nuisance* — agents pipe in a diff with trivial drift, it rejects,
they loop. Make it a faithful GNU workalike: **allow fuzz/offset, but report it
loudly and structurally** (`Hunk #1 applied, offset +30, fuzz 1`).

This is *not* a silent fallback. GNU fuzz relaxes tolerance **within the same
line-context algorithm and reports it** — fine, and it matches the directive to
fail loud, not silently. We explicitly **reject "hijack `patch`"** (try strict
unified-diff apply, then auto-switch to content-anchored matching on failure):
that silently mutates `patch`'s *matching algorithm* based on stage-one success,
which is exactly the silent-fallback footgun kaish forbids — and it spends
fuzzy-matching engineering to rescue the format research says LLMs produce worst.

We also **do not hide or amputate `patch`** — plenty of agents and upstream tools
emit *correct* diffs; breaking a working reflex is gratuitous.

---

## `edit` — anchored line edits (October 2026)

The first pass of this document declined an `edit` builtin: it is not a
POSIX command, kaijutsu already had one as an MCP file tool, and a
`str_replace` shape needs two multi-line blocks that argv handles badly.
Amy reversed that on 2026-09-25 ("kaish gets edit and hashline support
throughout"), and the October design answered each objection:

- **One shape, measured.** With no help text, four model families
  (DeepSeek, Gemini, GPT, Claude Sonnet) all wrote the same commands:
  `edit FILE ANCHOR 'TEXT'`, anchor/text pairs for a batch, a delete flag,
  and `--after`. Help text for exactly that shape, plus `A..B` ranges and
  `--before`, scored 24/24 with byte-identical answers across the four. The
  heredoc, ed-script, and `@@`-hunk candidates were not needed.
- **Anchors only, no `str_replace` mode.** `sed` and `patch` already cover
  substitution and diffs. An anchor forces a read first and makes every edit
  a compare-and-set, which is the reliability property the research above
  asks for. Without the old-text block, the two-heredoc problem goes away.
- **The kernel owns the hash.** Builtins mark which file line a row is, and
  the kernel hashes it with the embedder's `LineHasher` (default FNV-1a, 4
  hex, the same as kaijutsu's). `cat`, `head`, `tail`, and `grep` print
  `LINE:HASH:TEXT` under `--hashline`; `edit` checks with the same hasher.

```sh
cat --hashline config.toml                       # 3:a8c7:port = 8080
edit config.toml 3:a8c7 'port = 9090'            # prints 3:<new hash>:port = 9090
edit config.toml --delete 6:8d62..10:2325
edit config.toml --after 2:7f4b 'workers = 4'
edit config.toml 12:5f5e 'enabled = true' 13:fbb6 'size = 512'   # one batch, all or nothing
```

The failure-mode design, per "The finding":

- **An anchor names a line of one file.** `--hashline` refuses stdin,
  several files to `cat`/`head`/`tail`, `-c`, marked-up `cat`, `grep -U`,
  and decoded text. A row numbered by stream position could carry a hash
  that matches a *different* file line with the same text (blank lines,
  `}`), and `edit` would change the wrong line with no error.
- **A batch resolves every anchor against the file as read**, so line
  numbers do not shift between its edits. Overlapping edits exit 2.
- **A stale anchor writes nothing and asks for a re-read.** In the panel, all
  four models took a fresh anchor straight from the error and overwrote
  someone else's change. So the error shows the current text but no usable
  anchor: `line 12 changed since you read it (now: enabled = maybe);
  nothing was written. Read it again before editing: cat --hashline
  config.toml`.
- **The file keeps its line endings** (CRLF stays CRLF, including inside
  multi-line TEXT) and its final newline, or lack of one.
- **The write is an atomic replace** (#486): after a crash the file is old
  or new, never partial.

`patch` stays a faithful GNU `patch` for the diff-producing world; `edit`
carries the exact-position contract #375 asked about.

## What this is NOT

- Not structure-aware (AST/function-level) editing — the research frontier
  (AdaEdit, BlockDiff/FuncDiff, ACL 2026) needs a per-language parser, out of scope
  for a language-agnostic shell.
- Not a deprecation of `patch`/`diff` — those stay for the diff-producing world
  (git, upstream tools, correct-diff agents).

---

## References

- "To Diff or Not to Diff?" — arxiv.org/abs/2604.27296 (ACL Findings 2026)
- OpenAI V4A `apply_patch` — developers.openai.com/cookbook/examples/gpt4-1_prompting_guide
- Anthropic text editor (`str_replace`) — platform.claude.com/docs/en/agents-and-tools/tool-use/text-editor-tool
- Aider unified diffs / flexible patching — aider.chat/docs/unified-diffs.html
- "The Harness Problem" — blog.can.ac (2026); anthropics/claude-code#25775
- kaijutsu hashline editor (reference): `crates/kaijutsu-kernel/src/file_tools/{hashline,edit,read}.rs`

*Method: [designing-syntax-with-llms.md](designing-syntax-with-llms.md). Panel
June 2026 — DeepSeek-V4, Claude Haiku (reflex); Gemini 3.1 Pro, Claude Opus
(hostile design review). These surveys are disposable; the decisions are durable.*
