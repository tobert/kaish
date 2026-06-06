//! Composition surface: assemble canonical kaish guidance for an audience.
//!
//! Content is a set of [`Fragment`]s keyed by [`Concept`] / [`Variant`] / locale.
//! A [`Selector`] (or a ready-made [`Recipe`]) chooses which fragments to render;
//! [`compose`] assembles them into a single markdown string. Live, schema-derived
//! content (the builtin index, per-tool help) is injected through the
//! [`GeneratedContent`] trait so this crate stays free of the tool registry.
//!
//! Design + resolved decisions: `docs/composable-help.md`.

use std::collections::BTreeMap;

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
    /// An agent driving kaish (MCP / embedded) — terse, behavior-focused.
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
    /// Markdown body.
    pub body: &'static str,
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

/// Choose the fragments for one concept: filter, then resolve each (key, variant)
/// slot to the requested locale, falling back to English.
fn select_for_concept<'f>(concept: Concept, selector: &Selector) -> Vec<&'f Fragment> {
    let mut slots: BTreeMap<(&str, u8), &Fragment> = BTreeMap::new();

    for fragment in FRAGMENTS
        .iter()
        .filter(|f| f.concept == concept && applicable(f, selector))
    {
        let slot = (fragment.key, fragment.variant.order());
        match slots.get(&slot) {
            // First candidate for this slot wins by default.
            None => {
                slots.insert(slot, fragment);
            }
            // Prefer the requested locale; otherwise keep what we have (English
            // base, by construction of the registry).
            Some(existing) => {
                if fragment.locale == selector.locale && existing.locale != selector.locale {
                    slots.insert(slot, fragment);
                }
            }
        }
    }

    slots.into_values().collect()
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

        sections.push(format!("## {}\n\n{}", concept.title(), body.trim_end()));
    }

    sections.join("\n\n")
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
/// Wiring these into the MCP server instructions / tool description and the REPL
/// welcome is the next phase (see `docs/composable-help.md`).
pub struct Recipe;

impl Recipe {
    /// What the MCP `instructions:` field and an embedder's agent system prompt use:
    /// the model, the operating contract, and the builtin index — terse.
    pub fn agent_onboarding() -> Selector {
        Selector {
            concepts: vec![Concept::Model, Concept::Foundations, Concept::Builtins],
            variants: Vec::new(),
            audience: Audience::Agent,
            depth: Depth::Summary,
            locale: DEFAULT_LOCALE.to_string(),
        }
    }

    /// The REPL startup welcome: model + operating contract, human-flavored.
    pub fn repl_welcome() -> Selector {
        Selector {
            concepts: vec![Concept::Model, Concept::Foundations],
            variants: Vec::new(),
            audience: Audience::Human,
            depth: Depth::Summary,
            locale: DEFAULT_LOCALE.to_string(),
        }
    }

    /// The MCP `execute` tool description: the operating contract only, terse.
    pub fn tool_description() -> Selector {
        Selector {
            concepts: vec![Concept::Foundations],
            variants: vec![Variant::Rule, Variant::Contrast],
            audience: Audience::Agent,
            depth: Depth::Summary,
            locale: DEFAULT_LOCALE.to_string(),
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
