# Editing files from an agent shell: `diff` and `patch`

*A design pass over kaish's diff/patch surface, run through the
[LLM-panel method](designing-syntax-with-llms.md) (existing-tool variant) and
grounded in 2025–2026 research on how models actually edit code. Decides what
stays a workalike, what relaxes — and why content-anchored editing belongs in the
embedders, not the kaish shell.*

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

Reflex and reliability diverge. The resolution splits along the kaish/embedder
seam: kaish keeps the reflex tools honest (a POSIX-80% shell), and the
content-anchored tool that addresses the reliability problem lives where it
already does — the embedder's MCP surface (see "Why not a kaish `edit` builtin").

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

## Why not a kaish `edit` builtin

The obvious "kaish magic" move is a content-anchored `edit` builtin
(`str_replace`-style, or hashline `N:hash` anchors). It was considered and
**declined for kaish** — it belongs in the embedders:

- **It isn't shell.** kaish is *the 80% of POSIX/Bourne/bash*. A hashline/
  `str_replace` editor is a wholly invented affordance, not a recognizable Unix
  command — the first builtin that isn't one.
- **It already lives in the right place.** kaijutsu's `file_tools/edit.rs` exposes
  it as an **MCP file tool** with a JSON-param surface that fits it natively, and
  CLAUDE.md is explicit that the MCP/tool surface lives in the embedders, not
  kaish.
- **The ergonomics confirm the seam.** Forcing a JSON-param-shaped tool into
  argv+stdin needs awkward two-block heredoc gymnastics (old *and* new content as
  separate multi-line blocks). When the shell surface fights that hard, the tool
  isn't shell-shaped.
- **The in-shell edit reflex is already covered** by `sed -i` (issues.md P2) plus
  `tee`/`patch` — the POSIX-80% answer. An agent wanting hashline-anchored editing
  is reaching past the shell, into the embedder.

### The kaijutsu hashline paradigm (reference)

Worth knowing, because it's the strongest answer to the reliability problem and
the alignment target if a shared core is ever extracted. kaijutsu's `read` prints
each line as `N:hash→ content`; `edit` addresses a line/range by that `N:hash`
**anchor** and re-hashes before writing — a stale anchor fails loud instead of
splicing the wrong place. The line number locates; the 4-hex FNV-1a hash is a
cheap compare-and-swap. (Background: "The Harness Problem", blog.can.ac 2026;
anthropics/claude-code#25775.) The pure planning core (`line_hash`,
`plan_string_edit`/`plan_anchor_edit`, `render`) is I/O- and CRDT-free, so it
*could* be lifted into a shared leaf crate — but that only pays off once a second
embedder (e.g. kaibo) needs hashline editing. **Deferred**; not built
speculatively, and not a reason to put `edit` in kaish.

---

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
