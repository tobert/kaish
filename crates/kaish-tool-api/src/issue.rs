//! Validation issues and formatting.

use std::fmt;

/// Severity level for validation issues.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Severity {
    /// Errors prevent execution.
    Error,
    /// Warnings are advisory but allow execution.
    Warning,
}

impl fmt::Display for Severity {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
        }
    }
}

/// Categorizes validation issues for filtering and tooling.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IssueCode {
    /// Command not found in registry or user tools.
    UndefinedCommand,
    /// Required parameter not provided.
    MissingRequiredArg,
    /// Flag not defined in tool schema.
    UnknownFlag,
    /// Argument type doesn't match schema.
    InvalidArgType,
    /// seq increment is zero (infinite loop).
    SeqZeroIncrement,
    /// Regex pattern is invalid.
    InvalidRegex,
    /// break/continue outside of a loop.
    BreakOutsideLoop,
    /// return outside of a function.
    ReturnOutsideFunction,
    /// Variable may be undefined.
    PossiblyUndefinedVariable,
    /// Bare scalar variable in for loop (no word splitting in kaish).
    ForLoopScalarVar,
    /// scatter without gather — parallel results would be lost.
    ScatterWithoutGather,
    /// Field access on `$?` (e.g. `${?.data}`, `${?.ok}`) was removed.
    /// `$?` is the POSIX exit code; use `kaish-last` for structured data.
    LastResultFieldAccess,
    /// diff was given other than two file operands.
    DiffNeedsTwoFiles,
    /// sed expression is syntactically invalid.
    InvalidSedExpr,
    /// jq filter expression is syntactically invalid.
    InvalidJqFilter,
    /// A POSIX `test` / `[ … ]` conditional was used. kaish has no such command
    /// (it would resolve to an external binary that bypasses the VFS); use
    /// `[[ … ]]`, which the validator checks before runtime.
    PosixTestCommand,
}

impl IssueCode {
    /// Returns a short code string for the issue.
    ///
    /// Code numbers are stable identifiers, not contiguous. E010 and
    /// W003/W004/W005 remain retired. E006 (InvalidSedExpr), E007
    /// (InvalidJqFilter), and E011 (DiffNeedsTwoFiles) were wired up with
    /// real emitters in 2026-06-14.
    pub fn code(&self) -> &'static str {
        match self {
            IssueCode::UndefinedCommand => "E001",
            IssueCode::MissingRequiredArg => "E002",
            IssueCode::UnknownFlag => "W001",
            IssueCode::InvalidArgType => "E003",
            IssueCode::SeqZeroIncrement => "E004",
            IssueCode::InvalidRegex => "E005",
            IssueCode::InvalidSedExpr => "E006",
            IssueCode::InvalidJqFilter => "E007",
            IssueCode::BreakOutsideLoop => "E008",
            IssueCode::ReturnOutsideFunction => "E009",
            // E010 retired — never emitted
            IssueCode::PossiblyUndefinedVariable => "W002",
            IssueCode::DiffNeedsTwoFiles => "E011",
            IssueCode::ForLoopScalarVar => "E012",
            IssueCode::ScatterWithoutGather => "E014",
            IssueCode::LastResultFieldAccess => "E015",
            IssueCode::PosixTestCommand => "W006",
        }
    }

    /// Whether a warning carrying this code should be surfaced to the agent
    /// (appended to the result's stderr) rather than only trace-logged.
    ///
    /// Most warnings stay trace-only — `UndefinedCommand` fires on every
    /// external command (`grep`, `cargo`), so surfacing them all would be
    /// noise. Opt a code in here only when its guidance is worth interrupting
    /// for. This is the surfacing seam for the "did-you-mean" guidance pass.
    pub fn surfaces_to_agent(&self) -> bool {
        matches!(self, IssueCode::PosixTestCommand)
    }

    /// Default severity for this issue code.
    pub fn default_severity(&self) -> Severity {
        match self {
            // These are hard errors that will definitely fail at runtime
            IssueCode::SeqZeroIncrement
            | IssueCode::InvalidRegex
            | IssueCode::InvalidSedExpr
            | IssueCode::InvalidJqFilter
            | IssueCode::DiffNeedsTwoFiles
            | IssueCode::BreakOutsideLoop
            | IssueCode::ReturnOutsideFunction
            | IssueCode::ForLoopScalarVar
            | IssueCode::ScatterWithoutGather
            | IssueCode::LastResultFieldAccess => Severity::Error,

            // These are warnings because context matters:
            // - MissingRequiredArg: might be provided by pipeline stdin or environment
            // - InvalidArgType: shell coerces types at runtime
            // - UndefinedCommand: might be script in PATH or external tool
            IssueCode::MissingRequiredArg
            | IssueCode::InvalidArgType
            | IssueCode::UndefinedCommand
            | IssueCode::UnknownFlag
            | IssueCode::PosixTestCommand
            | IssueCode::PossiblyUndefinedVariable => Severity::Warning,
        }
    }
}

impl fmt::Display for IssueCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.code())
    }
}

/// Source location span.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct Span {
    /// Start byte offset in source.
    pub start: usize,
    /// End byte offset in source.
    pub end: usize,
}

impl Span {
    /// Create a new span.
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    /// Convert byte offset to line:column.
    ///
    /// Returns (line, column) where both are 1-indexed.
    pub fn to_line_col(&self, source: &str) -> (usize, usize) {
        let mut line = 1;
        let mut col = 1;

        for (i, ch) in source.char_indices() {
            if i >= self.start {
                break;
            }
            if ch == '\n' {
                line += 1;
                col = 1;
            } else {
                col += 1;
            }
        }

        (line, col)
    }

    /// Format span as "line:col" string.
    pub fn format_location(&self, source: &str) -> String {
        let (line, col) = self.to_line_col(source);
        format!("{}:{}", line, col)
    }
}

/// A validation issue found in the script.
#[derive(Debug, Clone)]
#[non_exhaustive]
pub struct ValidationIssue {
    /// Severity level.
    pub severity: Severity,
    /// Issue category code.
    pub code: IssueCode,
    /// Human-readable message.
    pub message: String,
    /// Optional source location.
    pub span: Option<Span>,
    /// Optional suggestion for fixing the issue.
    pub suggestion: Option<String>,
}

impl ValidationIssue {
    /// Create a new validation error.
    pub fn error(code: IssueCode, message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Error,
            code,
            message: message.into(),
            span: None,
            suggestion: None,
        }
    }

    /// Create a new validation warning.
    pub fn warning(code: IssueCode, message: impl Into<String>) -> Self {
        Self {
            severity: Severity::Warning,
            code,
            message: message.into(),
            span: None,
            suggestion: None,
        }
    }

    /// Add a span to this issue.
    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    /// Add a suggestion to this issue.
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }

    /// Format the issue for display.
    ///
    /// With source provided, includes line:column information and source context.
    pub fn format(&self, source: &str) -> String {
        let mut result = String::new();

        // Location prefix if we have a span
        if let Some(span) = &self.span {
            let loc = span.format_location(source);
            result.push_str(&format!("{}: ", loc));
        }

        // Severity and code
        result.push_str(&format!("{} [{}]: {}", self.severity, self.code, self.message));

        // Suggestion if available
        if let Some(suggestion) = &self.suggestion {
            result.push_str(&format!("\n  → {}", suggestion));
        }

        // Source context if we have a span
        if let Some(span) = &self.span
            && let Some(line_content) = get_line_at_offset(source, span.start) {
                result.push_str(&format!("\n  | {}", line_content));
            }

        result
    }
}

impl fmt::Display for ValidationIssue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} [{}]: {}", self.severity, self.code, self.message)
    }
}

/// Get the line containing a byte offset.
fn get_line_at_offset(source: &str, offset: usize) -> Option<&str> {
    if offset >= source.len() {
        return None;
    }

    let start = source[..offset].rfind('\n').map_or(0, |i| i + 1);
    let end = source[offset..]
        .find('\n')
        .map_or(source.len(), |i| offset + i);

    Some(&source[start..end])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn span_to_line_col_single_line() {
        let source = "echo hello world";
        let span = Span::new(5, 10);
        assert_eq!(span.to_line_col(source), (1, 6));
    }

    #[test]
    fn span_to_line_col_multi_line() {
        let source = "line one\nline two\nline three";
        // "line" on line 3 starts at offset 18
        let span = Span::new(18, 22);
        assert_eq!(span.to_line_col(source), (3, 1));
    }

    #[test]
    fn span_format_location() {
        let source = "first\nsecond\nthird";
        let span = Span::new(6, 12); // "second"
        assert_eq!(span.format_location(source), "2:1");
    }

    #[test]
    fn issue_formatting() {
        let issue = ValidationIssue::error(IssueCode::UndefinedCommand, "command 'foo' not found")
            .with_span(Span::new(0, 3))
            .with_suggestion("did you mean 'for'?");

        let source = "foo bar";
        let formatted = issue.format(source);

        assert!(formatted.contains("1:1"));
        assert!(formatted.contains("error"));
        assert!(formatted.contains("E001"));
        assert!(formatted.contains("command 'foo' not found"));
        assert!(formatted.contains("did you mean 'for'?"));
    }

    #[test]
    fn get_line_at_offset_works() {
        let source = "line one\nline two\nline three";
        assert_eq!(get_line_at_offset(source, 0), Some("line one"));
        assert_eq!(get_line_at_offset(source, 9), Some("line two"));
        assert_eq!(get_line_at_offset(source, 18), Some("line three"));
    }
}
