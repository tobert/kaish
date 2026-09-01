//! Topic compatibility surface for kaish help.
//!
//! Backs the `help <topic>` builtin and embedder prompt surfaces: topic-based
//! whole-document help embedded at compile time, plus dynamic tool help from the
//! tool registry.
//! Behavior here is intentionally byte-stable — frontends and tests depend on it.

use kaish_types::ToolSchema;

use crate::compose::render_syntax_section;
use crate::content::{IGNORE, LIMITS, OUTPUT_LIMIT, OVERLAY, OVERVIEW, SCATTER, SYNTAX, VFS};

/// Help topics available in kaish.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HelpTopic {
    /// Overview of kaish with topic list.
    Overview,
    /// Syntax reference: variables, quoting, pipes, control flow.
    Syntax,
    /// List of all available builtins.
    Builtins,
    /// Virtual filesystem mounts and paths.
    Vfs,
    /// Scatter/gather parallel processing.
    Scatter,
    /// Ignore file configuration.
    Ignore,
    /// Output size limit configuration.
    OutputLimit,
    /// Known limitations.
    Limits,
    /// Overlay VFS mode and kaish-vfs builtin.
    Overlay,
    /// A single subsystem-sized syntax section (e.g. `collections`), composed
    /// straight from its `Syntax` fragment — single-sourced with `help syntax`.
    SyntaxSection(String),
    /// Help for a specific tool.
    Tool(String),
}

impl HelpTopic {
    /// Parse a topic string into a HelpTopic.
    ///
    /// Returns Overview for empty/None, specific topics for known names,
    /// or Tool(name) for anything else (assumes it's a tool name).
    pub fn parse_topic(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "" | "overview" | "help" => Self::Overview,
            "syntax" | "language" | "lang" => Self::Syntax,
            "builtins" | "tools" | "commands" => Self::Builtins,
            "vfs" | "filesystem" | "fs" | "paths" => Self::Vfs,
            "scatter" | "gather" | "parallel" | "散" | "集" => Self::Scatter,
            "ignore" | "gitignore" | "kaish-ignore" => Self::Ignore,
            "output-limit" | "spill" | "truncate" | "kaish-output-limit" => Self::OutputLimit,
            "limits" | "limitations" | "missing" => Self::Limits,
            "overlay" | "kaish-vfs" | "vfs-overlay" => Self::Overlay,
            other if render_syntax_section(other).is_some() => Self::SyntaxSection(other.to_string()),
            other => Self::Tool(other.to_string()),
        }
    }

    /// Get a short description of this topic.
    pub fn description(&self) -> &'static str {
        match self {
            Self::Overview => "What kaish is, list of topics",
            Self::Syntax => "Variables, quoting, pipes, control flow",
            Self::Builtins => "List of available builtins",
            Self::Vfs => "Virtual filesystem mounts and paths",
            Self::Scatter => "Parallel processing (散/集)",
            Self::Ignore => "Ignore file configuration",
            Self::OutputLimit => "Output size limit configuration",
            Self::Limits => "Known limitations",
            Self::Overlay => "Copy-on-write overlay mode and kaish-vfs",
            Self::SyntaxSection(_) => "A single syntax reference section",
            Self::Tool(_) => "Help for a specific tool",
        }
    }
}

/// Get help content for a topic.
///
/// For static topics, returns embedded markdown.
/// For `Builtins`, generates a tool list from the provided schemas.
/// For `Tool(name)`, looks up the tool in the schemas.
pub fn get_help(topic: &HelpTopic, tool_schemas: &[ToolSchema]) -> String {
    match topic {
        HelpTopic::Overview => OVERVIEW.to_string(),
        HelpTopic::Syntax => SYNTAX.to_string(),
        HelpTopic::Builtins => format_tool_list(tool_schemas),
        HelpTopic::Vfs => VFS.to_string(),
        HelpTopic::Scatter => SCATTER.to_string(),
        HelpTopic::Ignore => IGNORE.to_string(),
        HelpTopic::OutputLimit => OUTPUT_LIMIT.to_string(),
        HelpTopic::Limits => LIMITS.to_string(),
        HelpTopic::Overlay => OVERLAY.to_string(),
        HelpTopic::SyntaxSection(key) => render_syntax_section(key).unwrap_or_else(|| {
            format!(
                "Unknown topic or tool: {key}\n\nUse 'help' to see available topics, or 'help builtins' for tool list."
            )
        }),
        HelpTopic::Tool(name) => format_tool_help(name, tool_schemas),
    }
}

/// Format help for a single tool, or `None` if no such tool is registered.
///
/// The composition surface uses this; the `Unknown topic…` fallback lives in
/// `format_tool_help` for the `help <topic>` command path.
pub fn tool_help(name: &str, schemas: &[ToolSchema]) -> Option<String> {
    let schema = schemas.iter().find(|s| s.name == name)?;
    let mut output = String::new();

    output.push_str(&format!("{} — {}\n\n", schema.name, schema.description));

    if schema.params.is_empty() {
        output.push_str("No parameters.\n");
    } else {
        output.push_str("Parameters:\n");
        push_params(&mut output, &schema.params, "  ");
    }

    // A subcommand-aware tool (`kj`, every wrapped command) keeps its real
    // grammar here, at any depth. Without this the whole allowlist a
    // wrapped command publishes — its verbs, their flags, and the
    // constraints in their descriptions — was invisible to `help`.
    if !schema.subcommands.is_empty() {
        output.push_str("\nSubcommands:\n");
        output.push_str(&subcommand_roster(&schema.subcommands));
    }

    if !schema.examples.is_empty() {
        output.push_str("\nExamples:\n");
        for example in &schema.examples {
            output.push_str(&format!("  # {}\n", example.description));
            output.push_str(&format!("  {}\n\n", example.code));
        }
    }

    Some(output)
}

/// One line per parameter, plus its description indented under it.
///
/// Aliases are named here because they are the spelling agents actually
/// write: a declaration that publishes `-n` for `--max-count` was telling
/// `help` something it then dropped.
fn push_params(output: &mut String, params: &[kaish_types::ParamSchema], indent: &str) {
    for param in params {
        let req = if param.required { " (required)" } else { "" };
        let aliases = if param.aliases.is_empty() {
            String::new()
        } else {
            format!(" (also: {})", param.aliases.join(", "))
        };
        output.push_str(&format!(
            "{indent}{} : {}{}{}\n{indent}  {}\n",
            param.name, param.param_type, req, aliases, param.description
        ));
    }
}

/// The roster lines naming every subcommand at any depth, plus each one's
/// parameters. The caller writes its own `Subcommands:` header.
///
/// `ToolSchema::subcommands` is recursive — a node (`worktree`) can hold a
/// leaf (`list`) that holds another node — but the roster stays flat: every
/// line renders the full path (`worktree list`) at the same two-space
/// indent, never a deeper indent per level. kaish-extras parses this roster
/// by column: exactly two spaces, then the ` — ` (space, em-dash, space)
/// separator. A nested indent or a different separator breaks that reader.
///
/// Public so `help <tool>` and `kaish-tools <name>` render one roster from
/// one implementation instead of drifting into two spellings of a tool's
/// grammar.
pub fn subcommand_roster(subs: &[ToolSchema]) -> String {
    let mut output = String::new();
    push_subcommand_roster(&mut output, "", subs);
    output
}

/// The recursion behind [`subcommand_roster`]. `prefix` is the path accumulated
/// so far and is a detail of the walk, which is why callers never supply it.
fn push_subcommand_roster(output: &mut String, prefix: &str, subs: &[ToolSchema]) {
    for sub in subs {
        let path = if prefix.is_empty() {
            sub.name.clone()
        } else {
            format!("{prefix} {}", sub.name)
        };
        if sub.description.is_empty() {
            output.push_str(&format!("  {path}\n"));
        } else {
            output.push_str(&format!("  {path} — {}\n", sub.description));
        }
        push_params(output, &sub.params, "    ");
        if !sub.subcommands.is_empty() {
            push_subcommand_roster(output, &path, &sub.subcommands);
        }
    }
}

/// Format help for a single tool.
fn format_tool_help(name: &str, schemas: &[ToolSchema]) -> String {
    tool_help(name, schemas).unwrap_or_else(|| {
        format!(
            "Unknown topic or tool: {}\n\nUse 'help' to see available topics, or 'help builtins' for tool list.",
            name
        )
    })
}

/// Format a flat alphabetical list of all available tools.
///
/// Schemas arrive sorted from the registry; only registered tools appear,
/// so feature-gated or unloaded builtins are omitted naturally.
fn format_tool_list(schemas: &[ToolSchema]) -> String {
    let mut output = String::from("# Available Builtins\n\n");

    let max_len = schemas.iter().map(|s| s.name.len()).max().unwrap_or(0);

    for schema in schemas {
        output.push_str(&format!(
            "  {:width$}  {}\n",
            schema.name,
            schema.description,
            width = max_len
        ));
    }

    output.push_str("\n---\n");
    output.push_str("Use 'help <tool>' for detailed help on a specific tool.\n");
    output.push_str("Use 'help syntax' for language syntax reference.\n");

    output
}

/// List available help topics (for autocomplete, etc.).
pub fn list_topics() -> Vec<(&'static str, &'static str)> {
    vec![
        ("overview", "What kaish is, list of topics"),
        ("syntax", "Variables, quoting, pipes, control flow"),
        ("builtins", "List of available builtins"),
        ("vfs", "Virtual filesystem mounts and paths"),
        ("scatter", "Parallel processing (散/集)"),
        ("ignore", "Ignore file configuration"),
        ("output-limit", "Output size limit configuration"),
        ("limits", "Known limitations"),
        ("overlay", "Copy-on-write overlay mode and kaish-vfs"),
        ("collections", "Lists & records: literals, access, iteration, lvalues"),
    ]
}

#[cfg(test)]
mod tests {
    use super::*;
    use kaish_types::ParamSchema;

    /// A two-level grammar (`git worktree list --porcelain`) — the node
    /// (`worktree`) has no params of its own, the leaf (`list`) does.
    fn nested_tool_schema() -> ToolSchema {
        let leaf = ToolSchema::new("list", "List the repository's working trees").param(
            ParamSchema::optional(
                "porcelain",
                "bool",
                kaish_types::Value::Bool(false),
                "Machine-readable output",
            ),
        );
        let node = ToolSchema::new("worktree", "Work with the repository's working trees").subcommand(leaf);
        ToolSchema::new("git", "Git plumbing and porcelain").subcommand(node)
    }

    #[test]
    fn test_tool_help_recurses_into_nested_subcommands() {
        let schema = nested_tool_schema();
        let content = tool_help("git", std::slice::from_ref(&schema)).expect("git is registered");

        // The leaf's full path names the actual verb, not just the node.
        assert!(
            content.contains("worktree list — List the repository's working trees"),
            "expected full-path leaf line, got:\n{content}"
        );
        // The leaf's parameter renders too.
        assert!(
            content.contains("porcelain"),
            "expected leaf parameter to render, got:\n{content}"
        );
        assert!(
            content.contains("Machine-readable output"),
            "expected leaf parameter description to render, got:\n{content}"
        );

        // Flat-roster contract: every roster line is exactly two spaces of
        // indent, path and description joined by " — " (space, em-dash,
        // space) — kaish-extras parses this shape.
        let roster_start = content.find("Subcommands:\n").expect("Subcommands section") + "Subcommands:\n".len();
        for line in content[roster_start..].lines() {
            if line.is_empty() || line.starts_with("    ") || line.starts_with("Examples:") {
                continue; // param line, or past the roster
            }
            assert!(
                line.starts_with("  ") && !line.starts_with("   "),
                "roster line must start with exactly two spaces: {line:?}"
            );
            assert!(
                line.contains(" — "),
                "roster line must use the ' — ' separator: {line:?}"
            );
        }
    }

    #[test]
    fn test_tool_help_recurses_three_levels() {
        // A wrapped command can declare grammar deeper than two levels
        // (`kj context session list --active`) — depth must not cap at 2.
        let leaf = ToolSchema::new("list", "List sessions in this context").param(
            ParamSchema::optional(
                "active",
                "bool",
                kaish_types::Value::Bool(false),
                "Only running sessions",
            ),
        );
        let session = ToolSchema::new("session", "Session operations").subcommand(leaf);
        let context = ToolSchema::new("context", "Context operations").subcommand(session);
        let schema = ToolSchema::new("kj", "kaijutsu control").subcommand(context);

        let content = tool_help("kj", std::slice::from_ref(&schema)).expect("kj is registered");
        assert!(
            content.contains("context session list — List sessions in this context"),
            "expected three-level full-path leaf line, got:\n{content}"
        );
        assert!(content.contains("active"), "expected leaf parameter to render, got:\n{content}");

        let roster_start = content.find("Subcommands:\n").expect("Subcommands section") + "Subcommands:\n".len();
        for line in content[roster_start..].lines() {
            if line.contains(" — ") {
                assert!(
                    line.starts_with("  ") && !line.starts_with("   "),
                    "roster line must stay at exactly two spaces regardless of depth: {line:?}"
                );
            }
        }
    }

    #[test]
    fn test_tool_help_flat_tool_unchanged() {
        // Control: a tool with no subcommands renders exactly as before —
        // no "Subcommands:" section at all.
        let schema = ToolSchema::new("cat", "Read and output file contents")
            .param(ParamSchema::required("path", "string", "File path to read"));
        let content = tool_help("cat", std::slice::from_ref(&schema)).expect("cat is registered");
        assert!(!content.contains("Subcommands:"));
    }

    #[test]
    fn test_topic_parsing() {
        assert_eq!(HelpTopic::parse_topic(""), HelpTopic::Overview);
        assert_eq!(HelpTopic::parse_topic("overview"), HelpTopic::Overview);
        assert_eq!(HelpTopic::parse_topic("syntax"), HelpTopic::Syntax);
        assert_eq!(HelpTopic::parse_topic("SYNTAX"), HelpTopic::Syntax);
        assert_eq!(HelpTopic::parse_topic("builtins"), HelpTopic::Builtins);
        assert_eq!(HelpTopic::parse_topic("vfs"), HelpTopic::Vfs);
        assert_eq!(HelpTopic::parse_topic("scatter"), HelpTopic::Scatter);
        assert_eq!(HelpTopic::parse_topic("集"), HelpTopic::Scatter);
        assert_eq!(HelpTopic::parse_topic("output-limit"), HelpTopic::OutputLimit);
        assert_eq!(HelpTopic::parse_topic("spill"), HelpTopic::OutputLimit);
        assert_eq!(HelpTopic::parse_topic("kaish-output-limit"), HelpTopic::OutputLimit);
        assert_eq!(HelpTopic::parse_topic("limits"), HelpTopic::Limits);
        assert_eq!(
            HelpTopic::parse_topic("grep"),
            HelpTopic::Tool("grep".to_string())
        );
        assert_eq!(
            HelpTopic::parse_topic("collections"),
            HelpTopic::SyntaxSection("collections".to_string())
        );
    }

    #[test]
    fn test_get_help_collections_section() {
        let content = get_help(&HelpTopic::SyntaxSection("collections".to_string()), &[]);
        assert!(content.contains("Collections (lists & records)"));
        assert!(content.contains("xs=[apple banana cherry]"));
        // Single-sourced with `help syntax` — not a second, hand-written copy.
        assert!(SYNTAX.contains("xs=[apple banana cherry]"));
    }

    #[test]
    fn test_get_help_unknown_syntax_section_falls_back() {
        // Guards against constructing the variant directly with a bad key
        // (bypassing parse_topic's existence check) and panicking.
        let content = get_help(&HelpTopic::SyntaxSection("not-a-real-section".to_string()), &[]);
        assert!(content.contains("Unknown topic or tool"));
    }

    #[test]
    fn test_static_content_embedded() {
        // Verify the markdown files are embedded
        assert!(OVERVIEW.contains("kaish"));
        assert!(SYNTAX.contains("Variables"));
        assert!(VFS.contains("Mount Points"));
        assert!(SCATTER.contains("scatter"));
        assert!(IGNORE.contains("kaish-ignore"));
        assert!(OUTPUT_LIMIT.contains("kaish-output-limit"));
        assert!(LIMITS.contains("Limitations"));
    }

    #[test]
    fn test_get_help_overview() {
        let content = get_help(&HelpTopic::Overview, &[]);
        assert!(content.contains("kaish"));
        assert!(content.contains("help syntax"));
    }

    #[test]
    fn test_get_help_unknown_tool() {
        let content = get_help(&HelpTopic::Tool("nonexistent".to_string()), &[]);
        assert!(content.contains("Unknown topic or tool"));
    }

    #[test]
    fn test_tool_help_none_for_missing() {
        assert!(tool_help("nonexistent", &[]).is_none());
    }
}
