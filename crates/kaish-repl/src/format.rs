//! Output formatting for the REPL.
//!
//! This module handles formatting ExecResult output based on DisplayHint and OutputContext.
//! It provides smart formatting for different audiences:
//!
//! - **Interactive** → Pretty columns, colors, traditional tree
//! - **Piped/Model** → Token-efficient compact formats

use std::io::IsTerminal;

use kaish_kernel::interpreter::{DisplayHint, EntryType, ExecResult};
use kaish_kernel::tools::OutputContext;

/// Format an ExecResult for display based on the output context.
///
/// This is the main entry point for formatting command output. It examines
/// the DisplayHint and chooses the appropriate format based on context.
pub fn format_output(result: &ExecResult, context: OutputContext) -> String {
    match &result.hint {
        DisplayHint::None => {
            // No hint - use raw output as-is
            result.out.clone()
        }
        DisplayHint::Table {
            headers,
            rows,
            entry_types,
        } => {
            match context {
                OutputContext::Interactive => {
                    format_table_interactive(headers, rows, entry_types.as_deref())
                }
                OutputContext::Piped | OutputContext::Model | OutputContext::Script => {
                    // One entry per line, first column only (names)
                    result.out.clone()
                }
            }
        }
        DisplayHint::Tree {
            traditional,
            compact,
            ..
        } => {
            match context {
                OutputContext::Interactive => traditional.clone(),
                OutputContext::Piped | OutputContext::Model | OutputContext::Script => compact.clone(),
            }
        }
    }
}

/// Detect the output context based on terminal state.
pub fn detect_context() -> OutputContext {
    if std::io::stdout().is_terminal() {
        OutputContext::Interactive
    } else {
        OutputContext::Piped
    }
}

/// Format a table for interactive display with columns.
fn format_table_interactive(
    headers: &Option<Vec<String>>,
    rows: &[Vec<String>],
    entry_types: Option<&[EntryType]>,
) -> String {
    if rows.is_empty() {
        return String::new();
    }

    // Get terminal width, default to 80 if unavailable
    let term_width = terminal_size::terminal_size()
        .map(|(w, _)| w.0 as usize)
        .unwrap_or(80);

    // For single-column tables (like ls without -l), format as columns
    let is_single_column = rows.iter().all(|r| r.len() == 1);

    if is_single_column {
        format_columns(rows, entry_types, term_width)
    } else {
        format_rows_aligned(headers, rows, entry_types)
    }
}

/// Format items in multiple columns like `ls` does.
fn format_columns(
    rows: &[Vec<String>],
    entry_types: Option<&[EntryType]>,
    term_width: usize,
) -> String {
    let items: Vec<_> = rows.iter().filter_map(|r| r.first()).collect();
    if items.is_empty() {
        return String::new();
    }

    // Find the longest item
    let max_len = items.iter().map(|s| s.len()).max().unwrap_or(0);
    // Add padding between columns (2 spaces)
    let col_width = max_len + 2;
    // Calculate number of columns that fit
    let num_cols = (term_width / col_width).max(1);

    let mut output = String::new();
    let mut col = 0;

    for (i, item) in items.iter().enumerate() {
        let colored_item = if let Some(types) = entry_types {
            colorize_entry(item, types.get(i).copied())
        } else {
            (*item).clone()
        };

        if col > 0 && col >= num_cols {
            output.push('\n');
            col = 0;
        }

        if col > 0 {
            // Pad previous item to column width
            let prev_len = items.get(i.saturating_sub(1)).map(|s| s.len()).unwrap_or(0);
            let padding = col_width.saturating_sub(prev_len);
            for _ in 0..padding {
                output.push(' ');
            }
        }

        output.push_str(&colored_item);
        col += 1;
    }

    output
}

/// Format rows with aligned columns (for -l long format).
fn format_rows_aligned(
    headers: &Option<Vec<String>>,
    rows: &[Vec<String>],
    entry_types: Option<&[EntryType]>,
) -> String {
    if rows.is_empty() {
        return String::new();
    }

    // Calculate column widths
    let num_cols = rows.iter().map(|r| r.len()).max().unwrap_or(0);
    let mut col_widths = vec![0; num_cols];

    // Include headers in width calculation
    if let Some(h) = headers {
        for (i, header) in h.iter().enumerate() {
            if i < col_widths.len() {
                col_widths[i] = col_widths[i].max(header.len());
            }
        }
    }

    // Calculate widths from data
    for row in rows {
        for (i, cell) in row.iter().enumerate() {
            if i < col_widths.len() {
                col_widths[i] = col_widths[i].max(cell.len());
            }
        }
    }

    let mut output = String::new();

    // Output headers if present
    if let Some(h) = headers {
        for (i, header) in h.iter().enumerate() {
            if i > 0 {
                output.push_str("  ");
            }
            output.push_str(header);
            if i < h.len() - 1 {
                let padding = col_widths[i].saturating_sub(header.len());
                for _ in 0..padding {
                    output.push(' ');
                }
            }
        }
        output.push('\n');
    }

    // Output rows
    for (row_idx, row) in rows.iter().enumerate() {
        for (i, cell) in row.iter().enumerate() {
            if i > 0 {
                output.push_str("  ");
            }

            // Colorize the last column (usually the name) based on entry type
            let colored_cell = if i == row.len() - 1 {
                if let Some(types) = entry_types {
                    colorize_entry(cell, types.get(row_idx).copied())
                } else {
                    cell.clone()
                }
            } else {
                cell.clone()
            };

            output.push_str(&colored_cell);

            // Only add padding if not the last column
            if i < row.len() - 1 {
                let padding = col_widths[i].saturating_sub(cell.len());
                for _ in 0..padding {
                    output.push(' ');
                }
            }
        }
        output.push('\n');
    }

    output.trim_end().to_string()
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
        Some(EntryType::File) | None => name.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_format_output_none_hint() {
        let result = ExecResult::success("hello world");
        let output = format_output(&result, OutputContext::Interactive);
        assert_eq!(output, "hello world");
    }

    #[test]
    fn test_format_output_table_piped() {
        let rows = vec![
            vec!["file1.txt".to_string()],
            vec!["file2.txt".to_string()],
        ];
        let result = ExecResult::success_table(rows, None);
        let output = format_output(&result, OutputContext::Piped);
        assert_eq!(output, "file1.txt\nfile2.txt");
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
    fn test_format_output_table_interactive() {
        let rows = vec![
            vec!["alpha.txt".to_string()],
            vec!["beta.txt".to_string()],
            vec!["gamma.txt".to_string()],
        ];
        let result = ExecResult::success_table(rows, None);
        let output = format_output(&result, OutputContext::Interactive);
        // Interactive should format as columns (or single column if narrow)
        // At minimum, all items should be present
        assert!(output.contains("alpha.txt"));
        assert!(output.contains("beta.txt"));
        assert!(output.contains("gamma.txt"));
    }

    #[test]
    fn test_format_output_table_with_headers_interactive() {
        let headers = vec!["Name".to_string(), "Size".to_string()];
        let rows = vec![
            vec!["foo.rs".to_string(), "1024".to_string()],
            vec!["bar.rs".to_string(), "2048".to_string()],
        ];
        let result = ExecResult::success_table_with_headers(headers, rows, None);
        let output = format_output(&result, OutputContext::Interactive);
        // Should contain data
        assert!(output.contains("foo.rs"));
        assert!(output.contains("bar.rs"));
        // Should format as aligned columns
        assert!(output.contains("1024") || output.contains("2048"));
    }

    #[test]
    fn test_format_output_tree_interactive() {
        let result = ExecResult::success_tree(
            "src".to_string(),
            serde_json::json!({"lib.rs": null}),
            "src/\n└── lib.rs".to_string(),
            "src/{lib.rs}".to_string(),
        );
        let output = format_output(&result, OutputContext::Interactive);
        // Interactive should use traditional format
        assert!(output.contains("└──"));
        assert!(output.contains("lib.rs"));
    }

    #[test]
    fn test_format_output_tree_piped() {
        let result = ExecResult::success_tree(
            "src".to_string(),
            serde_json::json!({"lib.rs": null}),
            "src/\n└── lib.rs".to_string(),
            "src/{lib.rs}".to_string(),
        );
        let output = format_output(&result, OutputContext::Piped);
        // Piped should use compact format
        assert_eq!(output, "src/{lib.rs}");
    }

    #[test]
    fn test_format_columns_basic() {
        let rows = vec![
            vec!["a".to_string()],
            vec!["b".to_string()],
            vec!["c".to_string()],
        ];
        // format_columns is private, but we can test via format_output
        let result = ExecResult::success_table(rows, None);
        let output = format_output(&result, OutputContext::Interactive);
        // All items should be present
        assert!(output.contains('a'));
        assert!(output.contains('b'));
        assert!(output.contains('c'));
    }
}
