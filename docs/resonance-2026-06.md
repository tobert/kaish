# Documentation Resonance Panel — June 2026

**Date:** 2026-06-09
**Panel:** DeepSeek-V4-Pro (via dpal, reasoning high) · Gemini 3 Pro (gemini-3.1-pro-preview via gpal, thinking high)
**Method:** Each model read five docs cold — `README.md`, `docs/LANGUAGE.md`,
`kaish-help` `overview.md` + `syntax.md`, `docs/EMBEDDING.md` — with no access to
the code, and evaluated *resonance* (does the doc land?) from two personas:

- **Persona 1:** an AI agent freshly connected to the kaish MCP server, these
  docs its only knowledge, must drive the shell correctly on the first try.
- **Persona 2:** a staff engineer evaluating kaish for adoption/embedding.

This is the companion to the accuracy review of the same date (see
[reviews.md](reviews.md) and [issues.md](issues.md)). Resonance findings are
subjective by design and were **not** adversarially verified; where a panel
claim overlaps a verified accuracy finding, that's noted.

---

## Convergent findings (both models, independently)

When two model families rank the same issue #1, treat it as signal.

### 1. The README MCP section contradicts itself about JSON output

Both models led with this. README says agents "will see json either way," then
says output is "clean text by default … use `--json` when needed." DeepSeek:
"An agent reading both cannot determine whether `--json` is required, ignored,
or merely cosmetic." Gemini: "An agent will hallucinate the parsing logic."

This compounds with a **verified accuracy bug** in the same section: README
documents a `help` MCP *tool* that does not exist (the server exposes only
`execute`; help is a builtin + MCP prompts — see issues.md). The README MCP
section is the single hottest spot in the doc corpus: it's the first thing an
MCP integrator reads, and it's wrong twice.

**Fix direction (synthesized):** state the contract once, precisely — the MCP
return envelope is always JSON (`ExecuteResult`); `--json` controls how a
*builtin renders stdout* inside that envelope. One sentence each, adjacent.

### 2. There is no `execute()` return contract anywhere

DeepSeek: "The single most critical piece of information for Persona 1 and
it's absent." The tool description lists what execute *supports* but never
what it *returns*: the JSON shape, exit-code conventions (0 ok / 1 error /
2 latch / 3 truncated), where `.data` lives, what truncation looks like and
how to recover. Gemini's predicted-mistake #3 is the direct consequence: an
agent gets exit 2 from `rm`, doesn't know it's a latch, and loops on failed
deletions instead of parsing the `--confirm=` nonce.

### 3. What persists across `execute()` calls is undefined

"Fresh kernel" appears in one sentence, but nonces persist, `--init` reloads,
trash state persists. DeepSeek asked for an explicit lifecycle table — *what
resets every call / what persists across the session* — and called it "the
contract the agent codes against." This is cheap to write and prevents the
silently-broken multi-call workflows both models predicted.

### 4. The sandbox pitch and the external-command escape hatch are never reconciled

Gemini (Persona 2): "If I sandbox the VFS but the agent can just run
`/bin/bash` or `curl`, the sandbox is useless. Can I disable external
commands? The docs don't say." DeepSeek asked for a threat-model section with
the same shape: what the sandbox prevents, what it does NOT prevent, what the
embedder owns.

**Important:** the *code* answers this well — `allow_external_commands`,
the capability-feature split (default = `localfs` only, no `subprocess`), the
`with_backend` host-side-channel refusal. The gap is purely documentation: the
strongest security work in the project is invisible in the docs the panel
read. This is the highest-leverage rewrite available — the material exists,
it just isn't surfaced. (EMBEDDING.md predates the capability split entirely;
see the P1 in issues.md.)

---

## DeepSeek-V4-Pro — distinct findings

- **"Fast and predictable" with zero benchmarks** (Persona 2, unconvincing).
  Either show numbers (cold-start `execute()` latency, scatter vs. GNU
  parallel, builtin grep vs. system grep) or claim the design properties that
  enable speed (no fork/exec, no PATH resolution) rather than the outcome.
- **Quote-to-join is taught, but the failure is never shown.** The docs
  explain the rule well; they never show the *error message* an agent gets
  when it writes `echo /tmp/$(id -u).sock`. Agents debug by matching error
  text to known failure classes; one concrete error example in the quoting
  section would short-circuit the wasted retry loop. (Note: the unquoted argv
  case currently *silently splats* rather than erroring — issues.md tracks the
  validator diagnostic that would make DeepSeek's suggested UX real.)
- **"Agent-generated PRs are welcome! 🤖" reads as low review bar** to a
  staff engineer. Suggested reframe: same review process regardless of origin,
  enthusiasm moved to CONTRIBUTING.md.
- **Experimental markers compound.** Scatter "(experimental)" + parser-from-git
  + nightly-only fuzz tests individually fine, collectively a "reasons not to
  adopt" column. (The parser-from-git claim is *stale* — chumsky is on
  crates.io since the 0.8.0 prep — which proves the point: the doc is
  generating adoption risk the project already paid down.)
- **No API stability / semver / MSRV statement in EMBEDDING.md.**
- **Spill-file recovery from MCP is undefined.** LANGUAGE.md's spill paths
  assume a host filesystem; an MCP agent needs to know whether the path is
  VFS-readable. (Accuracy review adds: for `with_backend` kernels there is
  *no* spill file at all — in-memory truncation only. The lifecycle/return
  contract should say so.)

**What resonates (keep, do more):** the quote-to-join/no-splitting pairing
tied to ShellCheck codes ("tie every divergence to a ShellCheck code");
the jq `<<<` idiom as a complete antipattern replacement; the for-loop
data rules with contrasting examples; the cancellation cascade ("this is the
quality level the rest of the docs should aim for"); the nonce lifecycle
table ("extend this pattern to all persistent state"); the migration table.

**Predicted first-contact mistakes:** unquoted `$dir/file.txt` composition;
assuming variables persist across `execute()` calls; `echo "$JSON" | jq`
instead of `jq <<<` (the docs show the idiom but bury it).

---

## Gemini 3 Pro — distinct findings

- **Inline env vars (`FOO=bar cmd`) need an explicit "Do Not" callout.**
  The bareword rule implies `FOO=bar` becomes `$1`, but no doc says
  "`RUST_LOG=debug cargo run` does not do what you think; use
  `export FOO=bar; cmd`." Gemini rates this a top-3 first-contact failure —
  agents use this construction constantly.
- **Structured data vs. pipes boundary is ambiguous.** `for i in $(seq 1 5)`
  iterates structured data — but what does `seq 1 5 | jq .` receive? The
  docs never state when `.data` survives a pipe and when bytes win.
- **"kaish makes no panic-safety guarantees today"** (EMBEDDING.md) is a
  glaring red flag presented casually. Either contextualize it (workspace
  denies `unwrap`/`unsafe`; panics are bugs, not policy) or scope it.
- **GitVfs over-indexes EMBEDDING.md** (~60% of the real estate): "the
  embedding guide reads like a Kaijutsu tutorial rather than a library
  guide." Suggested: move GitVfs/worktrees to its own doc; refocus on the
  KernelBackend trait, in-memory VFS, custom tools, lifecycle.

**What resonates:** the ShellCheck alignment table ("brilliant — instantly
translates the philosophy into a framework both audiences already
understand"); quote-to-join contrast examples; latch/trash ("builds trust");
the hermetic `initial_vars` env story ("exactly what a staff engineer wants
to see regarding state leakage").

**Predicted first-contact mistakes:** unquoted interpolated redirect targets
(`> /tmp/$(date +%s).log`); inline env vars; failing to parse the latch nonce
on exit 2.

---

## Synthesized priority list

In leverage order, combining both models with the accuracy review:

1. **Rewrite the README MCP section** — one tool (`execute`), the JSON
   envelope contract, `--json` semantics in one sentence, exit-code table,
   truncation recovery. Kills convergent findings 1 + 2 and the verified
   `help`-tool falsehood in one edit.
2. **Add the per-call lifecycle table** (resets vs. persists) to the MCP
   section and `overview.md`.
3. **Surface the security model that already exists** — a "Strict
   sandboxing" section in EMBEDDING.md covering `allow_external_commands`,
   capability features, `with_backend` hermeticity; a short threat-model
   paragraph (prevents / does not prevent / embedder owns).
4. **Add the "Do Not" callouts agents need:** inline env vars; one concrete
   quote-to-join error example; latch exit-2 → parse nonce → retry recipe.
5. **Rebalance EMBEDDING.md** (GitVfs out, ExecuteOptions/features in) —
   already required by the accuracy P1; fold the stability/MSRV statement in.
6. **De-risk the maturity signals:** fix the stale parser-from-git line,
   decide what "(experimental)" means for scatter, state design-derived
   performance properties (or benchmark).

The panel's strongest meta-lesson matches the cross-model-eval tradition:
the docs' best sections (cancellation, nonce lifecycle, ShellCheck table) are
the ones that state *contracts* — exhaustively, in tables, with codes. The
weak sections narrate features. Write contracts, not features.
