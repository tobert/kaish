# Composable Help & Instructions — Design Proposal

**Status:** Phases 1–3 landed (2026-06-06) — `kaish-help` crate built, content moved,
`kaish_kernel::help` is a shim, `syntax.md` generated + drift-tested, and every runtime
surface (kernel `help`, REPL welcome, and the agent-onboarding / tool-description /
prompt surfaces an embedder exposes) composes from the corpus. All tests/clippy/WASI
green. Phases 4–5 (publish + i18n) pending (§10).

> **Note (post-MCP-drop):** this design was written when kaish shipped an in-tree MCP
> server (`kaish-mcp`). That crate has since been removed — the MCP surface now lives
> in *embedders* (kaibo, kaijutsu). The composition design below is **unchanged**: the
> recipes that once fed the in-tree handler now feed the embedders' MCP handlers, which
> drive kaish through `KernelClient`/`execute()`. Read "the MCP server" throughout as
> "an embedder's MCP surface" — a consumer of `kaish-help`, not part of the kernel.

**Driver:** Embedders (kaijutsu, kaibo) and the REPL should all build the instructions
they hand to humans and agents on top of *one* canonical kaish content library, so that
a kaish release pushes updated guidance everywhere instead of each frontend hand-rolling
its own prose that silently drifts.

---

## 1. Problem

kaish already has one real source of truth for help *content*: the
`kaish_kernel::help` module (`crates/kaish-kernel/src/help.rs`), with
`get_help(topic, &schemas)`, `list_topics()`, and 7 `include_str!`'d markdown
files. That part is healthy.

The **instructions** surface is not. The agent/human-facing "what kaish is, why,
how to start" guidance is hand-written separately in at least three places, none
of which call the help module:

(Locations below are as of the original write-up, when the in-tree `kaish-mcp` crate
held these surfaces; they now live in embedders' MCP handlers — same drift problem,
same fix.)

| Surface | Location (then) | Sourced from help module? |
|---|---|---|
| MCP `instructions:` field | embedder MCP handler (was `kaish-mcp/handler.rs`) | ❌ bespoke prose |
| MCP `execute` tool description | embedder MCP handler (was `kaish-mcp/handler.rs`) | ❌ bespoke prose |
| REPL startup welcome | `kaish-repl/src/lib.rs` | ❌ bespoke prose |
| MCP prompt set | embedder MCP handler (was `kaish-mcp/handler.rs`) | ✅ via `get_help`, but the *list* is hand-maintained |

An embedder building an agent system prompt today has nothing to call — they would
hand-roll yet another copy. That is exactly the drift this proposal kills.

Two content problems compound it:

- **`docs/LANGUAGE.md` (~6.5K tokens, 26.5 KB, ~23 `##` sections) is not embedded
  at all** — on-disk only. `help language`/`lang` aliases to the *short*
  `syntax.md`, so the full reference is unreachable from the binary and embedders
  can't ship it.
- **`syntax.md` (971 words) is a hand-maintained condensation of LANGUAGE.md.**
  Two documents covering the same grammar, drifting independently.

## 2. Goals (from the design conversation)

1. **One canonical body of content**, mostly *shared* between humans and agents.
   Audience is a *lens* over shared content, not a separate content tree.
2. Anything that genuinely can't be shared lives **outside kaish-kernel** — hence a
   dedicated **`kaish-help` crate**.
3. The taxonomy is organized by **concept** (syntax, style, consistency, …), not by
   audience or by ad-hoc topic. Support **variations of the same idea to reinforce**
   it (rule / example / contrast-with-bash / rationale).
4. **i18n-ready**: the data model is keyed so non-English translations can be added
   later without restructuring.
5. The kernel `help` builtin, the REPL, **and external embedders (kaijutsu, kaibo —
   including their MCP surfaces)** all consume this crate. A kaish release pushes
   updates to all.
6. **LANGUAGE.md becomes composable** — broken into fragments; the monolithic file
   becomes *generated* build output rather than a hand-edited source.

## 3. Crate shape

A new leaf-ish crate, `kaish-help`, depending only on `kaish-types` (for
`ToolSchema`, which drives generated content). It must not depend on the kernel, so
embedders can pull it in cheaply.

```
kaish-types        (pure data: ToolSchema, OutputData, …)
   ▲
kaish-help         (content fragments + composition; i18n; render)
   ▲           ▲
kaish-kernel    │   (help builtin → kaish-help, injects live schemas)
   ▲           │
kaish-repl      │   (REPL welcome → kaish-help recipes)
                └── kaijutsu, kaibo (external embedders) → depend on kaish-help
                    directly (instructions, MCP tool desc, prompts → recipes)
```

`kaish_kernel::help` keeps its current public functions as a **thin re-export /
shim** over `kaish-help` so the ~existing call sites and the `help` builtin don't
churn. WASI build stays clean: `kaish-help` is pure data + string composition, no
OS deps.

## 4. Content model

Content is a set of **fragments**, each keyed by three axes. The same idea can
appear as several fragments (different variants) to reinforce it.

```rust
/// The "what" — concept taxonomy, organized for learning, not by audience.
pub enum Concept {
    Model,        // mental model: kernel/核, VFS, structured data, pre-validation
    Syntax,       // grammar: variables, expansion, quoting, pipes, control flow, …
    Foundations,  // the operating contract — guarantees AND the idioms that follow:
                  // no word splitting → use `split`; structured output → use --json;
                  // newline-split → `for line in $(cmd)`; crash-not-corrupt; latch.
                  // The agent-onboarding spine. (was "Consistency" — resolved Q2)
    Builtins,     // generated: tool index + per-tool help (from ToolSchema)
    Limits,       // intentionally-missing + known limitations + ShellCheck alignment
    // Capabilities — DEFERRED (Q2): near-empty until the capability-feature split
    // gives it a body. For now `set -o`/shell-options live under Syntax.
    // See [[project_capability_feature_split]].
}

/// The "how it's said" — variations of one idea, used to reinforce.
/// NOTE (resolved Q2): there is deliberately no Style/Guidance variant. Idiomatic
/// best-practice ("prefer --json") is foundational *content*, not a rendering — it
/// lives in the `Foundations` concept and is rendered through these same variants.
pub enum Variant {
    Rule,      // terse imperative ("use --json for structured output")
    Example,   // worked snippet (`ls --json | jq '.[].name'`)
    Contrast,  // how bash differs ("bash makes you parse `ls` text")
    Rationale, // why kaish chose this ("every builtin emits structured data")
}

pub struct Locale(/* BCP-47, e.g. "en", "ja" */);

/// A unit of content, addressed by (Concept, sub-key, Variant, Locale).
pub struct Fragment {
    concept: Concept,
    key: &'static str,   // sub-topic within a concept, e.g. "quoting", "for-loop"
    variant: Variant,
    depth: Depth,                // Summary = always-on core; Reference = detail (impl refinement)
    locale: &'static str,        // BCP-47; bodies are compile-time, so no Cow needed yet
    audience: Option<Audience>,  // None = shared (default); Some = rare divergence (Q1)
    body: &'static str,          // markdown
}
```

English is the always-complete base set. Translations are *additive* fragment files.

## 5. Composition API

Audience is a **lens / filter** over shared content — not a fork. Most fragments
are shared (`audience: None`); the rare divergence is `Some(Agent)`/`Some(Human)`.
"Same fact, different emphasis" (e.g. the nonce/latch flow: agents need the exit-2
protocol, humans need "it'll ask you to confirm") is a **Variant**, not an audience
split. Hard boundary (resolved Q1): if it's about *kaish* — the language, the
guarantees — it's shared content here; if it's about *the frontend* — keybindings,
an embedder's UI chrome — it does **not** live in kaish-help, it stays in the REPL /
kaijutsu / kaibo.

```rust
pub enum Audience { Agent, Human }   // Agent: terse; Human: + welcome/examples
pub enum Depth    { Summary, Reference }

pub struct Selector {
    concepts: Vec<Concept>,
    variants: VariantSet,   // which renderings to include
    audience: Audience,
    depth: Depth,
    locale: Locale,         // falls back to English (see §8)
}

/// Live, schema-derived content the static fragments can't hold.
/// The kernel implements this; kaish-help stays free of the registry.
pub trait GeneratedContent {
    fn builtin_index(&self) -> Vec<(String, String)>; // name, one-liner
    fn tool_help(&self, name: &str) -> Option<String>; // schema skeleton, from ToolSchema
}

/// Renders straight to markdown — the only render target for now (resolved Q4,
/// YAGNI). No structured `Document` tree, no JSON renderer yet; fix-forward (no
/// back-compat on main) makes adding one cheap if a kaibo/kaijutsu UI needs it.
pub fn compose(sel: &Selector, gen: &dyn GeneratedContent) -> String;
```

**Per-tool help is a merge** (resolved Q5): the schema skeleton from
`GeneratedContent::tool_help` plus, when present, a hand-written `Builtins`-concept
prose fragment keyed by tool name. Most tools render schema-only; `rg`/`timeout`
(today's orphaned `rg.md`/`timeout.md`) become the first two prose fragments.

**Recipes** so consumers never hand-build selectors:

```rust
impl Recipe {
    fn agent_onboarding() -> Selector;  // embedder agent prompt / MCP `instructions:`
    fn repl_welcome() -> Selector;      // REPL startup
    fn tool_description() -> Selector;  // embedder's `execute` tool description
    fn topic(name: &str) -> Selector;   // backs `help <topic>` builtin
    fn full_reference() -> Selector;    // regenerates LANGUAGE.md
}
```

"Variations to reinforce" falls out of the Variant axis: `agent_onboarding` might
pull `Rule + Contrast` for each core concept (terse + the bash gotcha), while
`full_reference` pulls every variant.

## 6. LANGUAGE.md decomposition

Split LANGUAGE.md along its existing `##` sections into fragments under `Syntax`,
`Model`, and `Limits`. Then:

- **`docs/LANGUAGE.md` becomes generated** — `Recipe::full_reference()` rendered at
  `Depth::Reference`. A test asserts the committed file matches the render
  (drift-fails CI, same spirit as `cargo insta`).
- **`syntax.md` becomes the `Depth::Summary` render of the `Syntax` concept** — no
  longer a separately maintained file.
- The "Available Builtins" / per-tool sections become `GeneratedContent` (they
  already are, in `format_tool_list` / `format_tool_help`) — formalized through the
  trait so `help builtins`, the LANGUAGE.md builtins section, and any embedder index
  are the *same* render.

Net: syntax.md and LANGUAGE.md stop being two hand-edited docs that drift; both
fall out of one fragment set.

## 7. Consumer migration (kills the drift)

- An embedder's MCP `instructions:` → `compose(Recipe::agent_onboarding(), gen)`.
- An embedder's `execute` tool description → `Recipe::tool_description()`.
- REPL welcome (`kaish-repl/src/lib.rs`) → `compose(Recipe::repl_welcome(), gen)`.
- An embedder's MCP prompt set → generated by iterating the concept/recipe registry
  instead of a hand-maintained list.
- kaijutsu / kaibo → depend on `kaish-help`, call `Recipe::agent_onboarding()` (or a
  custom `Selector`) for the instructions they pass to their agents and humans, and
  drive kaish itself through `KernelClient`/`execute()`.

## 8. i18n

Fragment address includes `Locale`. Resolution: requested locale → **English
fallback**. English is the canonical complete set; translations are additive.

Loudness (resolved Q3): the runtime fallback to English is **graceful and
unmarked** — most models work fine reading English even when operating in another
language, and an inline "(untranslated)" marker would corrupt the rendered output
for a human reader. Visibility lives at build/introspection time instead:
- a CI test enumerates every `(Concept, key, Variant)` and flags new gaps per
  non-English locale — partial translations still ship (no hard error), so a `ja`
  set can be filled in incrementally;
- `coverage(locale) -> Vec<MissingFragment>` lets a frontend *show* completeness
  if it wants (e.g. kaibo badging "ja 60%").

## 9. Resolved decisions (2026-06-06)

1. **Audience is an optional filter, not a fork.** `audience: Option<Audience>` on
   each fragment, `None` (shared) by default. Same-fact-different-emphasis → use a
   Variant. Frontend-specific content (keybindings, embedder UI chrome) stays out
   of `kaish-help` entirely. (§5)
2. **Five concepts, not seven.** Model / Syntax / **Foundations** / Builtins /
   Limits. `Consistency` is renamed **Foundations** and broadened: it holds the
   guarantees *and* the idioms that follow from them (no word splitting → `split`;
   structured output → `--json`; newline-split → `for line`) — the agent-onboarding
   spine, foundational *content*, not advisory asides. **Style is not a concept and
   not a variant** — those idioms are Foundations content, rendered through the
   normal Rule/Example/Contrast/Rationale variants. **Capabilities is deferred**
   until the capability-feature split gives it a body; `set -o` lives under Syntax
   for now. (§4)
3. **i18n falls back to English, gracefully and unmarked.** Models cope with English
   in another language; an inline marker would corrupt output. Gaps are surfaced at
   build time (CI coverage test, partial translations allowed — no hard error) and
   via a `coverage(locale)` introspection call. (§8)
4. **Markdown only (YAGNI).** `compose()` returns a markdown `String`; no structured
   `Document` tree or JSON renderer now. Fix-forward (no back-compat on main) makes
   adding one cheap when an embedder UI actually needs it. (§5)
5. **Per-tool help is a merge.** Schema skeleton (`GeneratedContent`) + optional
   hand-written `Builtins` prose fragment keyed by tool name. `rg.md`/`timeout.md`
   become the first two prose fragments instead of orphans. (§5, §6)

## 10. Suggested phasing (when we move past planning)

1. ✅ **DONE (2026-06-06).** Created `kaish-help` (deps: `kaish-types` only); moved
   the 9 help docs to `crates/kaish-help/content/en/` (repo `docs/help` symlink
   repointed); `topic.rs` holds the byte-stable `get_help`/`HelpTopic` compat
   surface; `compose.rs` + `fragments.rs` are the new model seeded with the
   **Foundations** spine + Model fragments; `kaish_kernel::help` is a `pub use`
   shim. 12 new tests + all existing help/sandbox tests pass; clippy/WASI/sandbox
   green. (Refinement: `Fragment` carries `depth`; recipes `agent_onboarding` /
   `repl_welcome` / `tool_description` exist but aren't wired into MCP/REPL yet —
   that's Phase 3.)
2. ✅ **DONE (2026-06-06, light variant — resolved scope Q).** Ported the 15
   `syntax.md` sections into `Concept::Syntax` fragments (titled, via
   `syntax_section()`); `render_syntax_reference()` composes them into
   `content/en/syntax.md`, which is now a committed, **drift-tested** mirror
   (`syntax_md_matches_fragments` + the `regen_syntax` example to regenerate). The
   regenerated file is byte-identical to the old one — zero content change, source
   moved to fragments. `LANGUAGE.md` stays hand-authored; a test guards that it
   still covers the syntax surface. (Full decomposition of LANGUAGE.md itself was
   declined — preserve its prose flow.)
3. ✅ **DONE (2026-06-06).** Wired every runtime surface to the corpus. *(The MCP
   wiring described here lived in the since-removed in-tree `kaish-mcp` crate; the same
   `compose(...)` calls now run in embedders' MCP handlers. Kept as the historical
   record of how the surfaces were single-sourced.)*
   - MCP `instructions:` (`handler.rs` `get_info`) → `compose(Recipe::agent_onboarding,
     SchemaContent::new(&[]))` (empty schemas → skips the inline builtin dump; clients
     run `help builtins`) + the MCP-specific tail (tools / `--init` / `kaish://vfs`),
     which stays in the handler per the frontend boundary.
   - REPL welcome (`lib.rs`) → `compose(Recipe::repl_welcome, …)` (headerless, terse).
   - Added `Selector.headers` (markdown clients want `##` headers; the REPL banner
     doesn't) and fixed compose to render in **registry order**, not key-sort.
   - **`execute` tool description** → the rmcp `list_tools` runtime override now sets it
     from `compose(Recipe::tool_description, …)` plus MCP-frontend framing (lead,
     unsupported-syntax note, paths, `help builtins` pointer), via the testable
     `composed_execute_description()` helper. The `#[tool(description="…")]` macro
     literal is reduced to a stable one-line fallback (real surface is `list_tools`).
   - **MCP prompts** → dropped the `#[prompt_router]`/`#[prompt]` macros entirely.
     `list_prompts`/`get_prompt` are manual and single-source from `help::list_topics()`
     (`build_prompts`/`render_prompt` on the handler): one `kaish-<topic>` prompt per
     topic, content via `get_help`. Adding a topic to the corpus now yields a prompt for
     free; `ignore`/`output-limit` are exposed for the first time (6 → 8); unknown prompt
     names fail loudly (`invalid_params`) instead of serving generic help.
4. Publish; kaijutsu/kaibo adopt `kaish-help`.
5. (Later) i18n scaffolding + first `ja` fragments.

---

*Sibling design docs: [arrays-and-hashes.md](arrays-and-hashes.md),
[designing-syntax-with-llms.md](designing-syntax-with-llms.md). Follow-up tracking:
[GH #185](https://github.com/tobert/kaish/issues/185).*
