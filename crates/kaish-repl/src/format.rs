//! Output formatting for the REPL.
//!
//! This module handles formatting ExecResult output based on OutputData.
//! It provides smart formatting for different audiences:
//!
//! - **Interactive** → Pretty columns, colors, traditional tree
//! - **Piped/Model** → Token-efficient compact formats
//!
//! If `result.output` is present, format using `format_output_data()`.
//! Otherwise, use raw `result.out`.

use std::io::IsTerminal;

use kaish_kernel::interpreter::{EntryType, ExecResult, OutputData, OutputNode};
use kaish_kernel::tools::OutputContext;

/// Format an ExecResult for display based on the output context.
///
/// This is the main entry point for formatting command output. It uses
/// structured OutputData if present, otherwise uses raw output.
pub fn format_output(result: &ExecResult, context: OutputContext) -> String {
    // Use OutputData if present
    if let Some(ref output) = result.output {
        return format_output_data(output, context);
    }

    // No structured output - use raw output
    result.out.clone()
}

/// Format OutputData for display based on context.
///
/// Rendering rules:
/// - Single text node → Print text
/// - Flat nodes with name only → Multi-column (interactive) or one-per-line
/// - Flat nodes with cells → Aligned table
/// - Nested children → Box-drawing tree (interactive) or brace notation
pub fn format_output_data(output: &OutputData, context: OutputContext) -> String {
    // Non-interactive contexts use canonical string
    if !matches!(context, OutputContext::Interactive) {
        return output.to_canonical_string();
    }

    // Simple text output
    if let Some(text) = output.as_text() {
        return text.to_string();
    }

    // Check if we have nested children (tree structure)
    if !output.is_flat() {
        return format_tree_from_output_data(output);
    }

    // Check if we have tabular data (cells present)
    if output.is_tabular() {
        return format_table_from_output_data(output);
    }

    // Flat list of names - format as columns
    format_columns_from_output_data(output)
}

/// Format output data as a tree with box-drawing characters.
fn format_tree_from_output_data(output: &OutputData) -> String {
    let mut result = String::new();

    for (i, node) in output.root.iter().enumerate() {
        if i > 0 {
            result.push('\n');
        }
        format_tree_node(&mut result, node, "", true);
    }

    result.trim_end().to_string()
}

/// Recursively format a tree node with box-drawing characters.
fn format_tree_node(output: &mut String, node: &OutputNode, prefix: &str, is_last: bool) {
    // Print this node
    let connector = if is_last { "└── " } else { "├── " };
    let name = if node.name.is_empty() {
        node.text.as_deref().unwrap_or("")
    } else {
        &node.name
    };

    // Add directory suffix for directories
    let suffix = if node.entry_type == EntryType::Directory && node.children.is_empty() {
        "/"
    } else {
        ""
    };

    output.push_str(prefix);
    output.push_str(connector);
    output.push_str(&colorize_entry(name, Some(node.entry_type)));
    output.push_str(suffix);
    output.push('\n');

    // Print children
    let child_prefix = format!("{}{}   ", prefix, if is_last { " " } else { "│" });
    let children: Vec<_> = node.children.iter().collect();
    for (i, child) in children.iter().enumerate() {
        let is_last_child = i == children.len() - 1;
        format_tree_node(output, child, &child_prefix, is_last_child);
    }
}

/// Format output data as an aligned table.
fn format_table_from_output_data(output: &OutputData) -> String {
    if output.root.is_empty() {
        return String::new();
    }

    // Build rows from nodes
    let rows: Vec<Vec<&str>> = output.root.iter().map(|node| {
        let mut row = Vec::new();
        // Add name as first column
        row.push(node.display_name());
        // Add cells
        for cell in &node.cells {
            row.push(cell.as_str());
        }
        row
    }).collect();

    // Calculate column widths
    let num_cols = rows.iter().map(|r| r.len()).max().unwrap_or(0);
    let mut col_widths = vec![0; num_cols];

    // Include headers in width calculation
    if let Some(ref headers) = output.headers {
        for (i, header) in headers.iter().enumerate() {
            if i < col_widths.len() {
                col_widths[i] = col_widths[i].max(header.len());
            }
        }
    }

    // Calculate widths from data
    for row in &rows {
        for (i, cell) in row.iter().enumerate() {
            if i < col_widths.len() {
                col_widths[i] = col_widths[i].max(cell.len());
            }
        }
    }

    let mut result = String::new();

    // Output headers if present
    if let Some(ref headers) = output.headers {
        for (i, header) in headers.iter().enumerate() {
            if i > 0 {
                result.push_str("  ");
            }
            result.push_str(header);
            if i < headers.len() - 1 {
                let padding = col_widths[i].saturating_sub(header.len());
                for _ in 0..padding {
                    result.push(' ');
                }
            }
        }
        result.push('\n');
    }

    // Output rows
    for (row_idx, row) in rows.iter().enumerate() {
        for (i, cell) in row.iter().enumerate() {
            if i > 0 {
                result.push_str("  ");
            }

            // Colorize based on entry type (name column is usually first)
            let colored_cell = if i == 0 {
                colorize_entry(cell, Some(output.root[row_idx].entry_type))
            } else {
                (*cell).to_string()
            };

            result.push_str(&colored_cell);

            // Only add padding if not the last column
            if i < row.len() - 1 {
                let padding = col_widths[i].saturating_sub(cell.len());
                for _ in 0..padding {
                    result.push(' ');
                }
            }
        }
        result.push('\n');
    }

    result.trim_end().to_string()
}

/// Format output data as multi-column display (like ls).
fn format_columns_from_output_data(output: &OutputData) -> String {
    if output.root.is_empty() {
        return String::new();
    }

    // Get terminal width, default to 80 if unavailable
    let term_width = terminal_size::terminal_size()
        .map(|(w, _)| w.0 as usize)
        .unwrap_or(80);

    let items: Vec<_> = output.root.iter().collect();

    // Find the longest item
    let max_len = items.iter()
        .map(|n| n.display_name().len())
        .max()
        .unwrap_or(0);

    // Add padding between columns (2 spaces)
    let col_width = max_len + 2;
    // Calculate number of columns that fit
    let num_cols = (term_width / col_width).max(1);

    let mut result = String::new();
    let mut col = 0;

    for (i, node) in items.iter().enumerate() {
        let colored_item = colorize_entry(node.display_name(), Some(node.entry_type));

        if col > 0 && col >= num_cols {
            result.push('\n');
            col = 0;
        }

        if col > 0 {
            // Pad previous item to column width
            let prev_len = items.get(i.saturating_sub(1))
                .map(|n| n.display_name().len())
                .unwrap_or(0);
            let padding = col_width.saturating_sub(prev_len);
            for _ in 0..padding {
                result.push(' ');
            }
        }

        result.push_str(&colored_item);
        col += 1;
    }

    result
}

/// Detect the output context based on terminal state.
pub fn detect_context() -> OutputContext {
    if std::io::stdout().is_terminal() {
        OutputContext::Interactive
    } else {
        OutputContext::Piped
    }
}

/// Colorize an entry based on its type.
fn colorize_entry(name: &str, entry_type: Option<EntryType>) -> String {
    use owo_colors::OwoColorize;

    // Check NO_COLOR environment variable
    if std::env::var("NO_COLOR").is_ok() {
        return name.to_string();
    }

    // Check TERM=dumb
    if std::env::var("TERM").map(|t| t == "dumb").unwrap_or(false) {
        return name.to_string();
    }

    match entry_type {
        Some(EntryType::Directory) => name.blue().bold().to_string(),
        Some(EntryType::Executable) => name.green().bold().to_string(),
        Some(EntryType::Symlink) => name.cyan().to_string(),
        Some(EntryType::File) | Some(EntryType::Text) | None => name.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_output_raw() {
        let result = ExecResult::success("hello world");
        let output = format_output(&result, OutputContext::Interactive);
        assert_eq!(output, "hello world");
    }

    #[test]
    fn test_detect_context_not_terminal() {
        // In test environment, stdout is typically not a terminal
        let context = detect_context();
        assert_eq!(context, OutputContext::Piped);
    }

    #[test]
    fn test_colorize_plain_file() {
        // Regular file should not be colored
        let result = colorize_entry("test.txt", Some(EntryType::File));
        assert_eq!(result, "test.txt");
    }

    #[test]
    fn test_output_data_simple_text() {
        let output_data = OutputData::text("hello world");
        let result = ExecResult::with_output(output_data);
        let formatted = format_output(&result, OutputContext::Interactive);
        assert_eq!(formatted, "hello world");
    }

    #[test]
    fn test_output_data_text_piped() {
        let output_data = OutputData::text("hello world");
        let result = ExecResult::with_output(output_data);
        let formatted = format_output(&result, OutputContext::Piped);
        assert_eq!(formatted, "hello world");
    }

    #[test]
    fn test_output_data_flat_nodes_interactive() {
        let nodes = vec![
            OutputNode::new("file1.txt").with_entry_type(EntryType::File),
            OutputNode::new("file2.txt").with_entry_type(EntryType::File),
            OutputNode::new("dir").with_entry_type(EntryType::Directory),
        ];
        let output_data = OutputData::nodes(nodes);
        let result = ExecResult::with_output(output_data);
        let formatted = format_output(&result, OutputContext::Interactive);
        assert!(formatted.contains("file1.txt"));
        assert!(formatted.contains("file2.txt"));
        assert!(formatted.contains("dir"));
    }

    #[test]
    fn test_output_data_flat_nodes_piped() {
        let nodes = vec![
            OutputNode::new("file1.txt").with_entry_type(EntryType::File),
            OutputNode::new("file2.txt").with_entry_type(EntryType::File),
        ];
        let output_data = OutputData::nodes(nodes);
        let result = ExecResult::with_output(output_data);
        let formatted = format_output(&result, OutputContext::Piped);
        // Piped output should be one per line
        assert_eq!(formatted, "file1.txt\nfile2.txt");
    }

    #[test]
    fn test_output_data_table_with_cells() {
        let nodes = vec![
            OutputNode::new("file1.txt")
                .with_cells(vec!["1024".to_string()])
                .with_entry_type(EntryType::File),
            OutputNode::new("file2.txt")
                .with_cells(vec!["2048".to_string()])
                .with_entry_type(EntryType::File),
        ];
        let output_data = OutputData::table(
            vec!["Name".to_string(), "Size".to_string()],
            nodes,
        );
        let result = ExecResult::with_output(output_data);
        let formatted = format_output(&result, OutputContext::Interactive);
        assert!(formatted.contains("Name"));
        assert!(formatted.contains("Size"));
        assert!(formatted.contains("file1.txt"));
        assert!(formatted.contains("1024"));
    }

    #[test]
    fn test_output_data_nested_children_piped() {
        let child = OutputNode::new("main.rs").with_entry_type(EntryType::File);
        let parent = OutputNode::new("src")
            .with_entry_type(EntryType::Directory)
            .with_children(vec![child]);
        let output_data = OutputData::nodes(vec![parent]);
        let result = ExecResult::with_output(output_data);
        let formatted = format_output(&result, OutputContext::Piped);
        // Piped should use brace notation
        assert!(formatted.contains("src"));
        assert!(formatted.contains("main.rs"));
    }

    #[test]
    fn test_format_output_data_direct() {
        let output_data = OutputData::text("direct test");
        let formatted = format_output_data(&output_data, OutputContext::Interactive);
        assert_eq!(formatted, "direct test");
    }
}
