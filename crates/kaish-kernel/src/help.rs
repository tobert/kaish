//! Help system for kaish.
//!
//! The content and composition logic now live in the `kaish-help` crate so the
//! kernel, REPL, MCP server, and embedders share one canonical source. This module
//! re-exports that surface; existing `crate::help::{get_help, HelpTopic, ...}` paths
//! keep working. See `docs/composable-help.md`.

// Compatibility surface — the `help <topic>` builtin and MCP prompts.
pub use kaish_help::topic::{get_help, list_topics, tool_help, HelpTopic};

// Composition surface — recipes for frontends and embedders.
pub use kaish_help::{
    compose, coverage, Audience, Concept, Depth, Fragment, GeneratedContent, MissingFragment,
    Recipe, SchemaContent, Selector, Variant, DEFAULT_LOCALE,
};
