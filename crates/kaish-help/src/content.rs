//! Embedded help content (English), compiled in from `content/en/`.
//!
//! These whole-document strings back the [`crate::topic`] compatibility surface.
//! The fine-grained [`crate::fragments`] registry that the composition surface
//! uses is separate; decomposing these docs into fragments (and making
//! `LANGUAGE.md` / `syntax.md` generated) is the next phase — see
//! `docs/composable-help.md`.

pub const OVERVIEW: &str = include_str!("../content/en/overview.md");
pub const SYNTAX: &str = include_str!("../content/en/syntax.md");
pub const VFS: &str = include_str!("../content/en/vfs.md");
pub const SCATTER: &str = include_str!("../content/en/scatter.md");
pub const IGNORE: &str = include_str!("../content/en/ignore.md");
pub const OUTPUT_LIMIT: &str = include_str!("../content/en/output-limit.md");
pub const LIMITS: &str = include_str!("../content/en/limits.md");
