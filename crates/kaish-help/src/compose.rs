//! Composition surface: assemble canonical kaish guidance for an audience.
//!
//! Content is a set of [`Fragment`]s keyed by [`Concept`] / [`Variant`] / locale.
//! A [`Selector`] (or a ready-made [`Recipe`]) chooses which fragments to render;
//! [`compose`] assembles them into a single markdown string. Live, schema-derived
//! content (the builtin index, per-tool help) is injected through the
//! [`GeneratedContent`] trait so this crate stays free of the tool registry.
//!
//! Design + resolved decisions: `docs/composable-help.md`.

use std::collections::HashMap;

use kaish_types::ToolSchema;

use crate::fragments::FRAGMENTS;
use crate::topic::tool_help;

/// The "what" — concept taxonomy, organized for learning, not by audience.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Concept {
    /// Mental model: kernel/核, VFS, structured data, pre-validation.
    Model,
    /// Grammar: variables, expansion, quoting, pipes, control flow.
    Syntax,
    /// The operating contract — guarantees AND the idioms that follow from them.
    /// The agent-onboarding spine (renamed from "Consistency"; see design doc).
    Foundations,
    /// Generated: tool index + per-tool help (from [`ToolSchema`]).
    Builtins,
    /// Intentionally-missing features, known limitations, ShellCheck alignment.
    Limits,
    // Capabilities — deferred until the capability-feature split gives it a body.
}

impl Concept {
    /// Human-readable section title used when composing.
    pub fn title(&self) -> &'static str {
        match self {
            Self::Model => "About kaish",
            Self::Syntax => "Syntax",
            Self::Foundations => "How kaish works",
            Self::Builtins => "Builtins",
            Self::Limits => "Limitations",
        }
    }
}

/// The "how it's said" — variations of one idea, used to reinforce.
///
/// There is deliberately no Style/Guidance variant: idiomatic best-practice
/// ("prefer `--json`") is foundational *content* (the [`Concept::Foundations`]
/// concept), not a rendering. See `docs/composable-help.md` (resolved Q2).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Variant {
    /// Terse imperative ("use `--json` for structured output").
    Rule,
    /// Worked snippet (`ls --json | jq '.[].name'`).
    Example,
    /// How bash differs ("bash makes you parse `ls` text").
    Contrast,
    /// Why kaish chose this ("every builtin emits structured data").
    Rationale,
}

impl Variant {
    /// Stable order within a concept/key: rule, then example, contrast, rationale.
    fn order(&self) -> u8 {
        match self {
            Self::Rule => 0,
            Self::Example => 1,
            Self::Contrast => 2,
            Self::Rationale => 3,
        }
    }
}

/// Who the rendered content is for. A *lens*, not a fork: most fragments are
/// shared (`audience: None`); the rare divergence is `Some(_)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Audience {
    /// An agent driving kaish (embedded) — terse, behavior-focused.
    Agent,
    /// A human at the REPL — welcome + discoverability.
    Human,
}

/// How much to include. `Summary` is the always-on core; `Reference` adds detail.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Depth {
    /// Just the load-bearing material.
    Summary,
    /// Everything, including examples and rationale.
    Reference,
}

/// Default (and canonical-complete) content locale.
pub const DEFAULT_LOCALE: &str = "en";

/// The importance rank of an unranked fragment: sorts last, so a fragment with
/// no explicit rank keeps its registry position relative to other unranked
/// fragments. Only the always-on onboarding spine assigns explicit ranks.
pub const UNRANKED: u8 = u8::MAX;

/// A unit of content, addressed by (concept, key, variant, locale).
pub struct Fragment {
    /// Concept this fragment belongs to.
    pub concept: Concept,
    /// Sub-topic within the concept, e.g. `"no-word-splitting"`.
    pub key: &'static str,
    /// How this fragment renders its idea.
    pub variant: Variant,
    /// `Summary` fragments always show; `Reference` only at reference depth.
    pub depth: Depth,
    /// BCP-47 locale tag. English (`"en"`) is the canonical-complete base.
    pub locale: &'static str,
    /// `None` = shared (default); `Some(_)` = audience-specific divergence.
    pub audience: Option<Audience>,
    /// Importance rank for the always-on onboarding block: `0` is the most
    /// important, and composition renders fragments **in ascending rank** so the
    /// client model meets the critical rules first even under skimming or
    /// truncation. Fragments at [`UNRANKED`] (the default) keep registry order.
    /// The rank is a property of the `(concept, key, variant)` slot, so it is
    /// locale-agnostic — a translation of a fragment inherits the same rank.
    pub rank: u8,
    /// Optional section heading for reference rendering (e.g. `"Quoting"`).
    /// `None` for inline fragments (the Foundations spine renders under its
    /// concept header instead). Used by [`render_syntax_reference`].
    pub title: Option<&'static str>,
    /// Markdown body.
    pub body: &'static str,
}

impl Fragment {
    /// Attach an importance `rank` (0 = most important) to a fragment, for the
    /// always-on onboarding block. `const` so it composes in the static registry:
    /// `en(...).ranked(2)`.
    pub const fn ranked(self, rank: u8) -> Fragment {
        Fragment { rank, ..self }
    }
}

/// What to compose. Build one directly, or use a [`Recipe`].
pub struct Selector {
    /// Concepts to include, in render order.
    pub concepts: Vec<Concept>,
    /// Variants to include; empty means all.
    pub variants: Vec<Variant>,
    /// Audience lens.
    pub audience: Audience,
    /// How much detail.
    pub depth: Depth,
    /// Requested locale; falls back to [`DEFAULT_LOCALE`] per slot.
    pub locale: String,
    /// Emit `## <concept title>` section headers. Markdown-rendering clients want
    /// them; a plain-terminal REPL banner does not.
    pub headers: bool,
}

/// Live, schema-derived content the static fragments can't hold.
///
/// The kernel implements this (it owns the tool registry); this crate stays free
/// of the registry. [`SchemaContent`] is the standard implementation.
pub trait GeneratedContent {
    /// `(name, one-line description)` for every available builtin, in list order.
    fn builtin_index(&self) -> Vec<(String, String)>;
    /// The schema skeleton for one tool, or `None` if it isn't registered.
    fn tool_help(&self, name: &str) -> Option<String>;
}

/// [`GeneratedContent`] backed by a slice of tool schemas.
pub struct SchemaContent<'a> {
    schemas: &'a [ToolSchema],
}

impl<'a> SchemaContent<'a> {
    /// Wrap a slice of schemas. Pass `&[]` when a recipe needs no generated content.
    pub fn new(schemas: &'a [ToolSchema]) -> Self {
        Self { schemas }
    }
}

impl GeneratedContent for SchemaContent<'_> {
    fn builtin_index(&self) -> Vec<(String, String)> {
        self.schemas
            .iter()
            .map(|s| (s.name.clone(), s.description.clone()))
            .collect()
    }

    fn tool_help(&self, name: &str) -> Option<String> {
        tool_help(name, self.schemas)
    }
}

/// Whether a fragment passes the selector's audience/depth/variant filters.
/// (Locale is resolved per slot afterwards, not here.)
fn applicable(fragment: &Fragment, selector: &Selector) -> bool {
    let variant_ok = selector.variants.is_empty() || selector.variants.contains(&fragment.variant);
    let depth_ok = fragment.depth == Depth::Summary || selector.depth == Depth::Reference;
    let audience_ok = fragment.audience.is_none_or(|a| a == selector.audience);
    variant_ok && depth_ok && audience_ok
}

/// Choose the fragments for one concept: filter, preserve **registry order** (it's
/// author-controlled and pedagogical — not key-sorted), and resolve each
/// (key, variant) slot to the requested locale, falling back to English.
fn select_for_concept<'f>(concept: Concept, selector: &Selector) -> Vec<&'f Fragment> {
    // Slot order = first appearance in the registry; `chosen` holds the best
    // locale match per slot (replacing in place keeps the slot's position).
    let mut order: Vec<(&str, u8)> = Vec::new();
    let mut chosen: HashMap<(&str, u8), &Fragment> = HashMap::new();

    for fragment in FRAGMENTS
        .iter()
        .filter(|f| f.concept == concept && applicable(f, selector))
    {
        let slot = (fragment.key, fragment.variant.order());
        match chosen.get(&slot) {
            None => {
                order.push(slot);
                chosen.insert(slot, fragment);
            }
            // Prefer the requested locale; otherwise keep what we have (English
            // base, by construction of the registry). Position is unchanged.
            Some(existing) => {
                if fragment.locale == selector.locale && existing.locale != selector.locale {
                    chosen.insert(slot, fragment);
                }
            }
        }
    }

    let mut result: Vec<&Fragment> = order
        .iter()
        .filter_map(|slot| chosen.get(slot).copied())
        .collect();
    // Render in importance order: ascending rank, ties preserving registry
    // position (a stable sort). Unranked fragments (`UNRANKED`) sort last and so
    // keep their registry order among themselves — a no-op for any concept whose
    // fragments are all unranked (Syntax, Model), which keeps `syntax.md` stable.
    result.sort_by_key(|f| f.rank);
    result
}

/// Compose canonical kaish guidance into a single markdown document.
///
/// Markdown is the only render target for now (resolved Q4, YAGNI). Concepts are
/// rendered in selector order under `##` headers; the `Builtins` concept pulls its
/// list from `generated`.
pub fn compose(selector: &Selector, generated: &dyn GeneratedContent) -> String {
    let mut sections: Vec<String> = Vec::new();

    for &concept in &selector.concepts {
        let mut body = String::new();

        if concept == Concept::Builtins {
            let index = generated.builtin_index();
            if index.is_empty() {
                continue;
            }
            let width = index.iter().map(|(name, _)| name.len()).max().unwrap_or(0);
            for (name, desc) in index {
                body.push_str(&format!("  {name:width$}  {desc}\n"));
            }
        } else {
            let fragments = select_for_concept(concept, selector);
            if fragments.is_empty() {
                continue;
            }
            for (i, fragment) in fragments.iter().enumerate() {
                if i > 0 {
                    body.push('\n');
                }
                body.push_str(fragment.body.trim_end());
                body.push('\n');
            }
        }

        let body = body.trim_end();
        if selector.headers {
            sections.push(format!("## {}\n\n{}", concept.title(), body));
        } else {
            sections.push(body.to_string());
        }
    }

    sections.join("\n\n")
}

/// Render the `Syntax` concept as a standalone reference document.
///
/// This is the single source for `content/en/syntax.md` (which is a committed,
/// drift-tested mirror) and for `help syntax`. Each Syntax fragment becomes a
/// `## <title>` section, in registry order. `LANGUAGE.md` stays hand-authored as
/// the deeper human reference; a test guards that it still covers this surface.
pub fn render_syntax_reference() -> String {
    let mut out = String::from("# kaish Syntax Reference\n");
    for fragment in FRAGMENTS
        .iter()
        .filter(|f| f.concept == Concept::Syntax && f.locale == DEFAULT_LOCALE)
    {
        let title = fragment.title.unwrap_or(fragment.key);
        out.push_str(&format!("\n## {title}\n\n{}\n", fragment.body.trim()));
    }
    out
}

/// A fragment present in English but missing in another locale.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MissingFragment {
    /// Concept of the untranslated fragment.
    pub concept: Concept,
    /// Key of the untranslated fragment.
    pub key: &'static str,
    /// Variant of the untranslated fragment.
    pub variant: Variant,
}

/// Report the English fragments that have no translation in `locale`.
///
/// English is canonical-complete, so `coverage(DEFAULT_LOCALE)` is always empty.
/// The runtime fall-back to English is graceful and unmarked; this surfaces gaps
/// at build/introspection time instead (resolved Q3).
pub fn coverage(locale: &str) -> Vec<MissingFragment> {
    FRAGMENTS
        .iter()
        .filter(|f| f.locale == DEFAULT_LOCALE)
        .filter(|f| {
            !FRAGMENTS.iter().any(|g| {
                g.locale == locale
                    && g.concept == f.concept
                    && g.key == f.key
                    && g.variant == f.variant
            })
        })
        .map(|f| MissingFragment {
            concept: f.concept,
            key: f.key,
            variant: f.variant,
        })
        .collect()
}

/// Ready-made [`Selector`]s so frontends never hand-build prose.
///
/// Wiring these into an embedder's agent instructions / tool description and the
/// REPL welcome is the next phase (see `docs/composable-help.md`).
pub struct Recipe;

impl Recipe {
    /// What an embedder's agent instructions / system prompt use:
    /// the model, the operating contract, and the builtin index — terse.
    pub fn agent_onboarding() -> Selector {
        Selector {
            concepts: vec![Concept::Model, Concept::Foundations, Concept::Builtins],
            variants: Vec::new(),
            audience: Audience::Agent,
            depth: Depth::Summary,
            locale: DEFAULT_LOCALE.to_string(),
            headers: true,
        }
    }

    /// The REPL startup welcome: model + the welcome line, human-flavored. Terse
    /// (no Foundations dump, no section headers) — it's a one-time banner.
    pub fn repl_welcome() -> Selector {
        Selector {
            concepts: vec![Concept::Model],
            variants: Vec::new(),
            audience: Audience::Human,
            depth: Depth::Summary,
            locale: DEFAULT_LOCALE.to_string(),
            headers: false,
        }
    }

    /// An embedder's `execute` tool description: the operating contract only, terse.
    pub fn tool_description() -> Selector {
        Selector {
            concepts: vec![Concept::Foundations],
            variants: vec![Variant::Rule, Variant::Contrast],
            audience: Audience::Agent,
            depth: Depth::Summary,
            locale: DEFAULT_LOCALE.to_string(),
            headers: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn no_content() -> SchemaContent<'static> {
        SchemaContent::new(&[])
    }

    #[test]
    fn agent_onboarding_has_foundations_content() {
        let out = compose(&Recipe::agent_onboarding(), &no_content());
        assert!(out.contains("How kaish works"));
        // A core guarantee must be present.
        assert!(
            out.to_lowercase().contains("word"),
            "expected the no-word-splitting guarantee, got:\n{out}"
        );
    }

    #[test]
    fn audience_filters_human_only_from_agent() {
        let agent = compose(&Recipe::agent_onboarding(), &no_content());
        let human = compose(&Recipe::repl_welcome(), &no_content());
        // The REPL welcome line is Human-only and must not leak into the agent blob.
        assert!(human.contains("exit"), "human welcome should mention exit");
        assert!(
            !agent.contains("exit to quit") && !agent.contains("`exit`"),
            "agent onboarding must not include the human welcome line"
        );
    }

    #[test]
    fn agent_only_fragment_excluded_from_human() {
        let agent = compose(&Recipe::agent_onboarding(), &no_content());
        let human = compose(&Recipe::repl_welcome(), &no_content());
        // The "prefer --json when orchestrating" line is Agent-only.
        assert!(agent.contains("orchestrat"), "agent blob should carry the agent-only json guidance");
        assert!(!human.contains("orchestrat"), "agent-only guidance must not appear in human welcome");
    }

    #[test]
    fn builtins_concept_pulls_from_generated_content() {
        let schemas = vec![
            ToolSchema::new("echo", "Print arguments"),
            ToolSchema::new("cat", "Read a file"),
        ];
        let out = compose(&Recipe::agent_onboarding(), &SchemaContent::new(&schemas));
        assert!(out.contains("## Builtins"));
        assert!(out.contains("echo"));
        assert!(out.contains("cat"));
    }

    #[test]
    fn depth_summary_excludes_reference_only_fragments() {
        let mut sel = Recipe::agent_onboarding();
        sel.depth = Depth::Summary;
        let summary = compose(&sel, &no_content());
        sel.depth = Depth::Reference;
        let reference = compose(&sel, &no_content());
        // Reference is a superset: at least as long, and contains an example block.
        assert!(reference.len() >= summary.len());
        assert!(reference.contains("```"), "reference depth should include example fragments");
    }

    #[test]
    fn onboarding_renders_in_importance_rank_order() {
        let out = compose(&Recipe::agent_onboarding(), &no_content());
        let nws = out.find("No word splitting").expect("has no-word-splitting");
        let fail = out.find("Fail loud").expect("has crash-not-corrupt");
        assert!(
            nws < fail,
            "the most-important rule (no-word-splitting, rank 0) must lead:\n{out}"
        );
        // Rank overrides registry position: structured-substitution (rank 2)
        // appears *before* structured-output (rank 5) even though the registry
        // lists structured-output first — proof the rank sort is doing work.
        let subst = out.find("carries structured data").expect("has structured-substitution");
        let output = out.find("Structured output").expect("has structured-output");
        assert!(
            subst < output,
            "rank must reorder ahead of registry position:\n{out}"
        );
    }

    /// The always-on onboarding block (the fragment spine, without the
    /// embedder-supplied builtin index) has a budget: it is composed in
    /// importance order and must stay lean so the critical rules survive
    /// skimming/truncation. Bumping this ceiling should be a deliberate choice —
    /// prefer moving verbose, low-rank content into a `help` topic and pointing
    /// at it from the tail.
    #[test]
    fn onboarding_spine_stays_within_budget() {
        const BUDGET: usize = 3500;
        let out = compose(&Recipe::agent_onboarding(), &no_content());
        assert!(
            out.len() <= BUDGET,
            "always-on onboarding spine is {} chars (budget {BUDGET}) — trim or defer \
             low-rank content to a help topic:\n{out}",
            out.len()
        );
    }

    #[test]
    fn repl_welcome_intro_precedes_help_line() {
        let out = compose(&Recipe::repl_welcome(), &no_content());
        let intro = out.find("Bourne-like").expect("has intro");
        let help_line = out.find("Type `help`").expect("has welcome line");
        assert!(intro < help_line, "intro should precede the help/exit line:\n{out}");
    }

    #[test]
    fn repl_welcome_is_headerless_and_terse() {
        let out = compose(&Recipe::repl_welcome(), &no_content());
        assert!(!out.contains("##"), "REPL banner must not carry markdown headers:\n{out}");
        assert!(out.contains("help"), "welcome should point at help");
        assert!(out.contains("exit"), "welcome should mention exit");
    }

    #[test]
    fn agent_onboarding_renders_section_headers() {
        let out = compose(&Recipe::agent_onboarding(), &no_content());
        assert!(out.contains("## "), "markdown clients want section headers:\n{out}");
    }

    #[test]
    fn syntax_md_matches_fragments() {
        assert_eq!(
            crate::content::SYNTAX,
            render_syntax_reference(),
            "content/en/syntax.md is stale — run \
             `cargo run -p kaish-help --example regen_syntax`"
        );
    }

    #[test]
    fn syntax_reference_covers_core_topics() {
        let out = render_syntax_reference();
        for needle in ["## Variables", "## Quoting", "## Command Substitution", "## Functions"] {
            assert!(out.contains(needle), "syntax reference missing {needle}");
        }
    }

    #[test]
    fn language_md_still_covers_the_syntax_surface() {
        // LANGUAGE.md stays hand-authored (deeper human reference); guard that it
        // hasn't lost coverage of the syntax topics the fragments single-source.
        let lang = std::fs::read_to_string(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../docs/LANGUAGE.md"
        ))
        .expect("read docs/LANGUAGE.md");
        for needle in [
            "Quoting",
            "Parameter Expansion",
            "Pipes & Redirects",
            "Command Substitution",
            "Arithmetic",
            "Functions",
            "Control Flow",
            "Test Expressions",
        ] {
            assert!(lang.contains(needle), "LANGUAGE.md no longer covers: {needle}");
        }
    }

    #[test]
    fn coverage_english_is_complete() {
        assert!(
            coverage(DEFAULT_LOCALE).is_empty(),
            "English is canonical-complete by definition"
        );
    }

    #[test]
    fn coverage_reports_untranslated_locale() {
        // No Japanese fragments exist yet, so every English slot is missing.
        let missing = coverage("ja");
        assert!(!missing.is_empty(), "ja has no fragments, so all slots are missing");
        let english_count = FRAGMENTS.iter().filter(|f| f.locale == DEFAULT_LOCALE).count();
        assert_eq!(missing.len(), english_count);
    }
}
