//! Composable help & instructions for kaish.
//!
//! kaish's canonical guidance lives here, once, so the kernel `help` builtin, the
//! REPL, and external embedders (kaijutsu, kaibo) all build their human- and
//! agent-facing instructions on the same source — a kaish release pushes updates
//! everywhere instead of each frontend hand-rolling prose that drifts.
//!
//! Two surfaces:
//!
//! - [`topic`] — the `help <topic>` command / embedder-prompt compatibility surface:
//!   [`get_help`], [`list_topics`], [`HelpTopic`]. Whole embedded markdown docs.
//! - [`compose`] — the composition surface: a concept-organized [`Fragment`] model
//!   ([`Concept`] / [`Variant`] / [`Audience`] / [`Depth`] / locale) assembled by
//!   [`compose`](compose::compose) via [`Selector`]s and ready-made [`Recipe`]s.
//!
//! Design: `docs/composable-help.md`.

pub mod compose;
pub mod content;
pub mod fragments;
pub mod topic;

// Compatibility surface — the `help <topic>` command and embedder prompts.
pub use topic::{get_help, list_topics, HelpTopic};

// Composition surface — recipes for frontends and embedders.
pub use compose::{
    compose, coverage, render_syntax_reference, Audience, Concept, Depth, Fragment,
    GeneratedContent, MissingFragment, Recipe, SchemaContent, Selector, Variant, DEFAULT_LOCALE,
};
