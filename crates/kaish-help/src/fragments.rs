//! The English fragment registry.
//!
//! Phase 1 seeds the composition surface with the [`Concept::Foundations`] spine
//! (the guarantees and the idioms that follow from them) plus a couple of
//! [`Concept::Model`] fragments — enough for the [`crate::Recipe`]s to produce real
//! output. Decomposing the whole `syntax.md` / `LANGUAGE.md` reference into
//! [`Concept::Syntax`] fragments (and making those docs generated) is the next
//! phase. See `docs/composable-help.md`.
//!
//! Bodies are inline `&'static str` for now; when i18n lands they move to
//! per-locale files keyed by (concept, key, variant).

use crate::compose::{Audience, Concept, Depth, Fragment, Variant, DEFAULT_LOCALE};

/// Shorthand for an English fragment.
const fn en(
    concept: Concept,
    key: &'static str,
    variant: Variant,
    depth: Depth,
    audience: Option<Audience>,
    body: &'static str,
) -> Fragment {
    Fragment {
        concept,
        key,
        variant,
        depth,
        locale: DEFAULT_LOCALE,
        audience,
        body,
    }
}

/// The canonical English content set.
pub const FRAGMENTS: &[Fragment] = &[
    // ---- Model ---------------------------------------------------------------
    en(
        Concept::Model,
        "what",
        Variant::Rule,
        Depth::Summary,
        None,
        "kaish (会sh) is a Bourne-like shell for AI agents: familiar syntax, fewer \
         footguns, validated before execution. Builtins run in-process; external \
         commands run via `PATH`.",
    ),
    en(
        Concept::Model,
        "welcome",
        Variant::Rule,
        Depth::Summary,
        Some(Audience::Human),
        "Type `help` for topics, `help <tool>` for a specific builtin, and `exit` to quit.",
    ),
    // ---- Foundations: guarantees + the idioms that follow --------------------
    en(
        Concept::Foundations,
        "no-word-splitting",
        Variant::Rule,
        Depth::Summary,
        None,
        "**No word splitting.** `$VAR` is always a single value — a variable holding \
         spaces stays one argument. Use `split` when you actually want to split on \
         whitespace, a delimiter, or a regex.",
    ),
    en(
        Concept::Foundations,
        "no-word-splitting",
        Variant::Contrast,
        Depth::Reference,
        None,
        "Bash splits unquoted `$VAR` on `$IFS`; kaish never does, so the defensive-\
         quoting dance is unnecessary.",
    ),
    en(
        Concept::Foundations,
        "structured-output",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Structured output.** Every builtin can emit machine-readable data with \
         `--json` (`ls --json`, `ps --json`, `kaish-vars --json`).",
    ),
    en(
        Concept::Foundations,
        "structured-output",
        Variant::Example,
        Depth::Reference,
        None,
        "```\nls --json | jq '.[].name'\n```",
    ),
    en(
        Concept::Foundations,
        "newline-split",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Newline-split substitution.** `for x in $(cmd)` splits on newlines only — \
         one iteration per line; whitespace within a line never splits.",
    ),
    en(
        Concept::Foundations,
        "structured-substitution",
        Variant::Rule,
        Depth::Summary,
        None,
        "`$(cmd)` carries structured data: `for i in $(seq 1 5)` iterates five values, \
         not split text.",
    ),
    en(
        Concept::Foundations,
        "glob-strict",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Strict globs.** `*.txt` expands to matching files; zero matches is an \
         error, not a silent pass-through.",
    ),
    en(
        Concept::Foundations,
        "pre-validation",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Pre-validation.** kaish validates the whole command before running it — \
         syntax errors are caught up front, so a command never half-runs.",
    ),
    en(
        Concept::Foundations,
        "crash-not-corrupt",
        Variant::Rule,
        Depth::Summary,
        None,
        "**Fail loud, not silent.** kaish prefers to error over corrupting data; \
         destructive operations can require a confirmation nonce via `set -o latch`.",
    ),
    en(
        Concept::Foundations,
        "json-orchestration",
        Variant::Rule,
        Depth::Summary,
        Some(Audience::Agent),
        "When orchestrating tools, prefer `--json` piped through `jq` — consuming \
         structured data beats scraping text output.",
    ),
];
