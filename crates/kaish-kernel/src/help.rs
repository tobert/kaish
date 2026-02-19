//! Help system for kaish.
//!
//! Provides topic-based help content embedded at compile time,
//! plus dynamic tool help from the tool registry.

use crate::tools::ToolSchema;

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
            Self::Limits => "Known limitations",
            Self::Tool(_) => "Help for a specific tool",
        }
    }
}

// Embed markdown files at compile time from the crate-local docs/help/ directory.
// The repo-root docs/help symlinks here so paths work both locally and in published crates.
const OVERVIEW: &str = include_str!("../docs/help/overview.md");
const SYNTAX: &str = include_str!("../docs/help/syntax.md");
const VFS: &str = include_str!("../docs/help/vfs.md");
const SCATTER: &str = include_str!("../docs/help/scatter.md");
const LIMITS: &str = include_str!("../docs/help/limits.md");

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
        HelpTopic::Limits => LIMITS.to_string(),
        HelpTopic::Tool(name) => format_tool_help(name, tool_schemas),
    }
}

/// Format help for a single tool.
fn format_tool_help(name: &str, schemas: &[ToolSchema]) -> String {
    match schemas.iter().find(|s| s.name == name) {
        Some(schema) => {
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

            output
        }
        None => format!(
            "Unknown topic or tool: {}\n\nUse 'help' to see available topics, or 'help builtins' for tool list.",
            name
        ),
    }
}

/// Format a list of all tools grouped by category.
fn format_tool_list(schemas: &[ToolSchema]) -> String {
    let mut output = String::new();

    output.push_str("# Available Builtins\n\n");

    // Find max name length for alignment
    let max_len = schemas.iter().map(|s| s.name.len()).max().unwrap_or(0);

    // Group tools by category based on name.
    // Keep in sync with actual builtins in tools/builtin/.
    let mut text_tools = Vec::new();
    let mut file_tools = Vec::new();
    let mut system_tools = Vec::new();
    let mut json_tools = Vec::new();
    let mut parallel_tools = Vec::new();
    let mut process_tools = Vec::new();
    let mut introspection_tools = Vec::new();
    let mut other_tools = Vec::new();

    for schema in schemas {
        let entry = (schema.name.as_str(), schema.description.as_str());
        match schema.name.as_str() {
            "grep" | "sed" | "awk" | "cut" | "tr" | "sort" | "uniq" | "wc" | "head" | "tail"
            | "split" | "diff" => text_tools.push(entry),
            "cat" | "ls" | "tree" | "cd" | "pwd" | "mkdir" | "rm" | "cp" | "mv" | "touch"
            | "ln" | "readlink" | "write" | "glob" | "find" | "stat" | "dirname" | "basename"
            | "realpath" | "mktemp" | "patch" => file_tools.push(entry),
            "alias" | "unalias" | "echo" | "printf" | "read" | "sleep" | "date" | "env"
            | "export" | "set" | "unset" | "true" | "false" | "test" | "[" | "assert" | "seq"
            | "tee" | "hostname" | "uname" | "which" => system_tools.push(entry),
            "jq" => json_tools.push(entry),
            "scatter" | "gather" => parallel_tools.push(entry),
            "exec" | "spawn" | "jobs" | "wait" | "ps" | "git" | "bg" | "fg" | "kill" => process_tools.push(entry),
            "help" | "validate" | "vars" | "mounts" | "tools" | "tokens"
            | "kaish-ast" | "kaish-clear" | "kaish-status" | "kaish-version" => {
                introspection_tools.push(entry)
            }
            _ => other_tools.push(entry),
        }
    }

    let format_group = |name: &str, tools: &[(&str, &str)], max: usize| -> String {
        if tools.is_empty() {
            return String::new();
        }
        let mut s = format!("## {}\n\n", name);
        for (tool_name, desc) in tools {
            s.push_str(&format!("  {:width$}  {}\n", tool_name, desc, width = max));
        }
        s.push('\n');
        s
    };

    output.push_str(&format_group("Text Processing", &text_tools, max_len));
    output.push_str(&format_group("Files & Directories", &file_tools, max_len));
    output.push_str(&format_group("JSON", &json_tools, max_len));
    output.push_str(&format_group("Processes & Jobs", &process_tools, max_len));
    output.push_str(&format_group("Parallel (散/集)", &parallel_tools, max_len));
    output.push_str(&format_group("Shell & System", &system_tools, max_len));
    output.push_str(&format_group("Introspection", &introspection_tools, max_len));
    output.push_str(&format_group("Other", &other_tools, max_len));

    output.push_str("---\n");
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
}
