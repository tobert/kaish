//! Topic compatibility surface for kaish help.
//!
//! Backs the `help <topic>` builtin and the MCP prompts: topic-based whole-document
//! help embedded at compile time, plus dynamic tool help from the tool registry.
//! Behavior here is intentionally byte-stable — frontends and tests depend on it.

use kaish_types::ToolSchema;

use crate::content::{IGNORE, LIMITS, OUTPUT_LIMIT, OVERVIEW, SCATTER, SYNTAX, VFS};

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
        HelpTopic::Tool(name) => format_tool_help(name, tool_schemas),
    }
}

/// Format help for a single tool, or `None` if no such tool is registered.
///
/// The composition surface uses this; the `Unknown topic…` fallback lives in
/// [`format_tool_help`] for the `help <topic>` command path.
pub fn tool_help(name: &str, schemas: &[ToolSchema]) -> Option<String> {
    let schema = schemas.iter().find(|s| s.name == name)?;
    let mut output = String::new();

    output.push_str(&format!("{} — {}\n\n", schema.name, schema.description));

    if schema.params.is_empty() {
        output.push_str("No parameters.\n");
    } else {
        output.push_str("Parameters:\n");
        for param in &schema.params {
            let req = if param.required { " (required)" } else { "" };
            output.push_str(&format!(
                "  {} : {}{}\n    {}\n",
                param.name, param.param_type, req, param.description
            ));
        }
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
    ]
}

#[cfg(test)]
mod tests {
    use super::*;

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
