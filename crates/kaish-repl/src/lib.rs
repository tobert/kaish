//! kaish REPL — Interactive shell for 会sh.
//!
//! This REPL provides an interactive interface to the kaish kernel.
//! It handles:
//! - Multi-line input via keyword/quote balancing (if/for/while → fi/done)
//! - Tab completion for commands, variables, and paths
//! - Command execution via the Kernel
//! - Result formatting with OutputData
//! - Command history via rustyline

pub mod format;

use std::borrow::Cow;
use std::io::IsTerminal;
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::{Context, Result};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::{Hint, Hinter};
use rustyline::history::DefaultHistory;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Editor, Helper};
use tokio::runtime::Runtime;

use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};

// ── Process result ──────────────────────────────────────────────────

/// Result from processing a line of input.
#[derive(Debug)]
pub enum ProcessResult {
    /// Output to display to the user.
    Output(String),
    /// No output (empty line, etc.).
    Empty,
    /// Exit the REPL.
    Exit,
}

// ── KaishHelper ─────────────────────────────────────────────────────

/// Rustyline helper providing validation, completion, highlighting, and hints.
struct KaishHelper {
    kernel: Arc<Kernel>,
    handle: tokio::runtime::Handle,
    path_completer: FilenameCompleter,
}

impl KaishHelper {
    fn new(kernel: Arc<Kernel>, handle: tokio::runtime::Handle) -> Self {
        Self {
            kernel,
            handle,
            path_completer: FilenameCompleter::new(),
        }
    }

    /// Determine if the input is incomplete (needs more lines).
    ///
    /// Uses a heuristic approach: count keyword depth (if/for/while increment,
    /// fi/done/esac decrement), check for unclosed quotes, and trailing backslash.
    fn is_incomplete(&self, input: &str) -> bool {
        // Trailing backslash = line continuation
        if input.trim_end().ends_with('\\') {
            return true;
        }

        let mut depth: i32 = 0;
        let mut in_single_quote = false;
        let mut in_double_quote = false;

        for line in input.lines() {
            let mut chars = line.chars().peekable();

            while let Some(ch) = chars.next() {
                match ch {
                    '\\' if !in_single_quote => {
                        // Skip escaped character
                        chars.next();
                    }
                    '\'' if !in_double_quote => {
                        in_single_quote = !in_single_quote;
                    }
                    '"' if !in_single_quote => {
                        in_double_quote = !in_double_quote;
                    }
                    _ => {}
                }
            }
        }

        // Unclosed quotes
        if in_single_quote || in_double_quote {
            return true;
        }

        // Count keyword depth from words (outside quotes)
        for word in shell_words(input) {
            match word.as_str() {
                "if" | "for" | "while" | "case" => depth += 1,
                "fi" | "done" | "esac" => depth -= 1,
                "then" | "else" | "elif" => {
                    // These don't change depth, they're part of an if block
                }
                _ => {}
            }
        }

        depth > 0
    }
}

/// Extract "words" from shell input, skipping quoted content.
/// Only used for keyword counting — doesn't need to be a full tokenizer.
fn shell_words(input: &str) -> Vec<String> {
    let mut words = Vec::new();
    let mut current = String::new();
    let mut in_single_quote = false;
    let mut in_double_quote = false;
    let mut in_comment = false;
    let mut prev_was_backslash = false;

    for ch in input.chars() {
        // Comments run to end of line
        if in_comment {
            if ch == '\n' {
                in_comment = false;
            }
            continue;
        }

        if prev_was_backslash {
            prev_was_backslash = false;
            if !in_single_quote {
                current.push(ch);
                continue;
            }
        }

        match ch {
            '\\' if !in_single_quote => {
                prev_was_backslash = true;
            }
            '\'' if !in_double_quote => {
                in_single_quote = !in_single_quote;
            }
            '"' if !in_single_quote => {
                in_double_quote = !in_double_quote;
            }
            '#' if !in_single_quote && !in_double_quote => {
                if !current.is_empty() {
                    words.push(std::mem::take(&mut current));
                }
                in_comment = true;
            }
            _ if ch.is_whitespace() && !in_single_quote && !in_double_quote => {
                if !current.is_empty() {
                    words.push(std::mem::take(&mut current));
                }
            }
            ';' if !in_single_quote && !in_double_quote => {
                // Semicolons split words too (e.g. "if true; then")
                if !current.is_empty() {
                    words.push(std::mem::take(&mut current));
                }
            }
            _ => {
                current.push(ch);
            }
        }
    }

    if !current.is_empty() {
        words.push(current);
    }

    words
}

// ── Completion context ──────────────────────────────────────────────

/// What kind of completion to offer based on cursor context.
enum CompletionContext {
    /// Start of line, after |, ;, &&, || → complete command names
    Command,
    /// After $ or within ${ → complete variable names
    Variable,
    /// Everything else → complete file paths
    Path,
}

/// Characters that delimit words for completion purposes.
fn is_word_delimiter(c: char) -> bool {
    c.is_whitespace() || matches!(c, '|' | ';' | '(' | ')')
}

/// Detect the completion context by scanning backwards from cursor position.
fn detect_completion_context(line: &str, pos: usize) -> CompletionContext {
    let before = &line[..pos];

    // Check for variable completion: look for $ before cursor
    // Walk backwards to find if we're in a $VAR or ${VAR context
    // But NOT $( which is command substitution
    let bytes = before.as_bytes();
    let mut i = pos;
    while i > 0 {
        i -= 1;
        let b = bytes[i];
        if b == b'$' {
            // $( is command substitution, not variable
            if i + 1 < pos && bytes[i + 1] == b'(' {
                break;
            }
            return CompletionContext::Variable;
        }
        if b == b'{' && i > 0 && bytes[i - 1] == b'$' {
            return CompletionContext::Variable;
        }
        // Stop scanning if we hit a non-identifier character
        if !b.is_ascii_alphanumeric() && b != b'_' && b != b'{' {
            break;
        }
    }

    // Check for command position: start of line, or after pipe/semicolon/logical operators/$(
    let trimmed = before.trim();
    if trimmed.is_empty()
        || trimmed.ends_with('|')
        || trimmed.ends_with(';')
        || trimmed.ends_with("&&")
        || trimmed.ends_with("||")
        || trimmed.ends_with("$(")
    {
        return CompletionContext::Command;
    }

    // Find start of current "word" (using delimiters that include parentheses)
    let word_start = before.rfind(is_word_delimiter);
    match word_start {
        None => CompletionContext::Command, // First word on the line
        Some(idx) => {
            // Check what's before the word
            let prefix = before[..=idx].trim();
            if prefix.is_empty()
                || prefix.ends_with('|')
                || prefix.ends_with(';')
                || prefix.ends_with("&&")
                || prefix.ends_with("||")
                || prefix.ends_with("$(")
                || prefix.ends_with("then")
                || prefix.ends_with("else")
                || prefix.ends_with("do")
            {
                CompletionContext::Command
            } else {
                CompletionContext::Path
            }
        }
    }
}

// ── Rustyline trait impls ───────────────────────────────────────────

impl Completer for KaishHelper {
    type Candidate = Pair;

    fn complete(
        &self,
        line: &str,
        pos: usize,
        ctx: &rustyline::Context<'_>,
    ) -> rustyline::Result<(usize, Vec<Pair>)> {
        match detect_completion_context(line, pos) {
            CompletionContext::Command => {
                // Find the prefix being typed
                let before = &line[..pos];
                let word_start = before
                    .rfind(is_word_delimiter)
                    .map(|i| i + 1)
                    .unwrap_or(0);
                let prefix = &line[word_start..pos];

                let mut candidates = Vec::new();

                // Tool/builtin names
                for schema in self.kernel.tool_schemas() {
                    if schema.name.starts_with(prefix) {
                        candidates.push(Pair {
                            display: schema.name.clone(),
                            replacement: schema.name.clone(),
                        });
                    }
                }

                candidates.sort_by(|a, b| a.display.cmp(&b.display));

                Ok((word_start, candidates))
            }

            CompletionContext::Variable => {
                // Find where the variable name starts (after $ or ${)
                let before = &line[..pos];
                let (var_start, prefix) = if let Some(brace_pos) = before.rfind("${") {
                    let name_start = brace_pos + 2;
                    (brace_pos, &line[name_start..pos])
                } else if let Some(dollar_pos) = before.rfind('$') {
                    let name_start = dollar_pos + 1;
                    (dollar_pos, &line[name_start..pos])
                } else {
                    return Ok((pos, vec![]));
                };

                // list_vars is async, use block_on
                let vars = self.handle.block_on(self.kernel.list_vars());

                let mut candidates: Vec<Pair> = vars
                    .into_iter()
                    .filter(|(name, _)| name.starts_with(prefix))
                    .map(|(name, _)| {
                        // Reconstruct the full $VAR or ${VAR} replacement
                        let (display, replacement) = if before.contains("${") {
                            (name.clone(), format!("${{{name}}}"))
                        } else {
                            (name.clone(), format!("${name}"))
                        };
                        Pair {
                            display,
                            replacement,
                        }
                    })
                    .collect();

                candidates.sort_by(|a, b| a.display.cmp(&b.display));

                Ok((var_start, candidates))
            }

            CompletionContext::Path => self.path_completer.complete(line, pos, ctx),
        }
    }
}

impl Validator for KaishHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        let input = ctx.input();
        if input.trim().is_empty() {
            return Ok(ValidationResult::Valid(None));
        }
        if self.is_incomplete(input) {
            Ok(ValidationResult::Incomplete)
        } else {
            Ok(ValidationResult::Valid(None))
        }
    }
}

impl Highlighter for KaishHelper {
    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Cow::Borrowed(hint)
    }
}

/// No-op hint type — we don't provide inline hints yet.
struct NoHint;
impl Hint for NoHint {
    fn display(&self) -> &str {
        ""
    }
    fn completion(&self) -> Option<&str> {
        None
    }
}

impl Hinter for KaishHelper {
    type Hint = NoHint;

    fn hint(&self, _line: &str, _pos: usize, _ctx: &rustyline::Context<'_>) -> Option<NoHint> {
        None
    }
}

impl Helper for KaishHelper {}

// ── REPL core ───────────────────────────────────────────────────────

/// REPL configuration and state.
pub struct Repl {
    kernel: Arc<Kernel>,
    runtime: Runtime,
}

impl Repl {
    /// Create a new REPL instance with passthrough filesystem access.
    pub fn new() -> Result<Self> {
        let config = KernelConfig::repl().with_interactive(true);
        let mut kernel = Kernel::new(config).context("Failed to create kernel")?;
        let runtime = Runtime::new().context("Failed to create tokio runtime")?;

        // Initialize terminal job control if stdin is a TTY
        #[cfg(unix)]
        if std::io::stdin().is_terminal() {
            kernel.init_terminal();
        }

        Ok(Self {
            kernel: Arc::new(kernel),
            runtime,
        })
    }

    /// Create a new REPL with a custom kernel configuration.
    pub fn with_config(config: KernelConfig) -> Result<Self> {
        let mut kernel = Kernel::new(config).context("Failed to create kernel")?;
        let runtime = Runtime::new().context("Failed to create tokio runtime")?;

        // Initialize terminal job control if stdin is a TTY
        #[cfg(unix)]
        if std::io::stdin().is_terminal() {
            kernel.init_terminal();
        }

        Ok(Self {
            kernel: Arc::new(kernel),
            runtime,
        })
    }

    /// Create a new REPL rooted at the given path.
    pub fn with_root(root: PathBuf) -> Result<Self> {
        let config = KernelConfig::repl().with_cwd(root);
        Self::with_config(config)
    }

    /// Process a single line of input.
    pub fn process_line(&mut self, line: &str) -> ProcessResult {
        let trimmed = line.trim();

        // Skip empty lines
        if trimmed.is_empty() {
            return ProcessResult::Empty;
        }

        // Intercept exit/quit before kernel dispatch
        if matches!(trimmed, "exit" | "quit") {
            return ProcessResult::Exit;
        }

        // Execute via kernel with SIGINT handling.
        // A per-execute signal listener catches Ctrl-C during execution,
        // cancels the kernel, and returns exit code 130.
        let kernel = self.kernel.clone();
        let input = trimmed.to_string();
        let result = self.runtime.block_on(async {
            let mut sigint = tokio::signal::unix::signal(
                tokio::signal::unix::SignalKind::interrupt(),
            )?;
            tokio::select! {
                result = kernel.execute(&input) => result,
                _ = sigint.recv() => {
                    kernel.cancel();
                    Ok(ExecResult::failure(130, ""))
                }
            }
        });

        match result {
            Ok(exec_result) => ProcessResult::Output(format_result(&exec_result)),
            Err(e) => ProcessResult::Output(format!("Error: {}", e)),
        }
    }
}

impl Default for Repl {
    #[allow(clippy::expect_used)]
    fn default() -> Self {
        Self::new().expect("Failed to create REPL")
    }
}

// ── Formatting ──────────────────────────────────────────────────────

/// Format an ExecResult for display.
///
/// Uses OutputData when available, otherwise falls back to status+output format.
fn format_result(result: &ExecResult) -> String {
    // If there's structured output, use the formatter
    if result.output.is_some() {
        let context = format::detect_context();
        let formatted = format::format_output(result, context);

        // For failures, append error info
        if !result.ok() && !result.err.is_empty() {
            return format!("{}\n✗ code={} err=\"{}\"", formatted, result.code, result.err);
        }
        return formatted;
    }

    // No structured output — just pass through the raw text.
    // Success: show output directly (no status prefix).
    // Failure: show stderr or exit code so the user notices.
    if result.ok() {
        result.out.clone()
    } else {
        let mut output = String::new();
        if !result.out.is_empty() {
            output.push_str(&result.out);
            if !output.ends_with('\n') {
                output.push('\n');
            }
        }
        if !result.err.is_empty() {
            output.push_str(&format!("✗ {}", result.err));
        } else {
            output.push_str(&format!("✗ [exit {}]", result.code));
        }
        output
    }
}

// ── History ─────────────────────────────────────────────────────────

/// Save REPL history to disk.
fn save_history(rl: &mut Editor<KaishHelper, DefaultHistory>, history_path: &Option<PathBuf>) {
    if let Some(path) = history_path {
        if let Some(parent) = path.parent()
            && let Err(e) = std::fs::create_dir_all(parent) {
                tracing::warn!("Failed to create history directory: {}", e);
            }
        if let Err(e) = rl.save_history(path) {
            tracing::warn!("Failed to save history: {}", e);
        }
    }
}

/// Load REPL history from disk.
fn load_history(rl: &mut Editor<KaishHelper, DefaultHistory>) -> Option<PathBuf> {
    let history_path = directories::BaseDirs::new()
        .map(|b| b.data_dir().join("kaish").join("history.txt"));
    if let Some(ref path) = history_path
        && let Err(e) = rl.load_history(path) {
            let is_not_found = matches!(&e, ReadlineError::Io(io_err) if io_err.kind() == std::io::ErrorKind::NotFound);
            if !is_not_found {
                tracing::warn!("Failed to load history: {}", e);
            }
        }
    history_path
}

// ── RC file and prompt ──────────────────────────────────────────────

/// Load the RC file for interactive sessions.
///
/// Search order: `$KAISH_INIT` → `~/.config/kaish/init.kai` → `~/.kaishrc`
fn load_rc_file(repl: &Repl) {
    let candidates: Vec<PathBuf> = if let Ok(path) = std::env::var("KAISH_INIT") {
        vec![PathBuf::from(path)]
    } else {
        vec![
            kaish_kernel::paths::config_dir().join("init.kai"),
            directories::BaseDirs::new()
                .map(|b| b.home_dir().join(".kaishrc"))
                .unwrap_or_else(|| PathBuf::from("/.kaishrc")),
        ]
    };

    for path in &candidates {
        if path.is_file() {
            let cmd = format!(r#"source "{}""#, path.display());
            if let Err(e) = repl.runtime.block_on(repl.kernel.execute(&cmd)) {
                eprintln!("kaish: warning: error sourcing {}: {}", path.display(), e);
            }
            return;
        }
    }
}

/// Resolve the prompt string: call `kaish_prompt()` if defined, else default.
fn resolve_prompt(repl: &Repl) -> String {
    let has_fn = repl.runtime.block_on(repl.kernel.has_function("kaish_prompt"));
    if has_fn {
        if let Ok(result) = repl.runtime.block_on(repl.kernel.execute("kaish_prompt")) {
            if result.ok() {
                let text = result.out.trim_end().to_string();
                if !text.is_empty() {
                    return text;
                }
            }
        }
    }
    "会sh> ".to_string()
}

// ── Entry points ────────────────────────────────────────────────────

/// Run the REPL.
pub fn run() -> Result<()> {
    println!("会sh — kaish v{}", env!("CARGO_PKG_VERSION"));
    println!("Type help for commands, exit to quit.");

    let mut repl = Repl::new()?;

    // Source RC file (interactive only)
    load_rc_file(&repl);

    // Build the helper with a kernel reference and runtime handle
    let helper = KaishHelper::new(repl.kernel.clone(), repl.runtime.handle().clone());

    let mut rl: Editor<KaishHelper, DefaultHistory> =
        Editor::new().context("Failed to create editor")?;
    rl.set_helper(Some(helper));

    let history_path = load_history(&mut rl);

    println!();

    loop {
        // Dynamic prompt: call kaish_prompt() if defined, else default
        let prompt_string = resolve_prompt(&repl);
        let prompt: &str = &prompt_string;

        match rl.readline(prompt) {
            Ok(line) => {
                if let Err(e) = rl.add_history_entry(line.as_str()) {
                    tracing::warn!("Failed to add history entry: {}", e);
                }

                match repl.process_line(&line) {
                    ProcessResult::Output(output) => {
                        if output.ends_with('\n') {
                            print!("{}", output);
                        } else {
                            println!("{}", output);
                        }
                    }
                    ProcessResult::Empty => {}
                    ProcessResult::Exit => {
                        save_history(&mut rl, &history_path);
                        return Ok(());
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("^D");
                break;
            }
            Err(err) => {
                eprintln!("Error: {}", err);
                break;
            }
        }
    }

    save_history(&mut rl, &history_path);

    Ok(())
}

// ── Tests ───────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_shell_words_simple() {
        assert_eq!(shell_words("echo hello world"), vec!["echo", "hello", "world"]);
    }

    #[test]
    fn test_shell_words_semicolons() {
        assert_eq!(shell_words("if true; then"), vec!["if", "true", "then"]);
    }

    #[test]
    fn test_shell_words_quoted() {
        // Quoted content is a single word (spaces preserved inside)
        assert_eq!(shell_words("echo \"hello world\""), vec!["echo", "hello world"]);
    }

    #[test]
    fn test_shell_words_single_quoted() {
        // Keywords inside quotes are not counted
        assert_eq!(shell_words("echo 'if then fi'"), vec!["echo", "if then fi"]);
    }

    #[test]
    fn test_is_incomplete_if_block() {
        let helper = make_test_helper();
        assert!(helper.is_incomplete("if true; then"));
        assert!(helper.is_incomplete("if true; then\n  echo hello"));
        assert!(!helper.is_incomplete("if true; then\n  echo hello\nfi"));
    }

    #[test]
    fn test_is_incomplete_for_loop() {
        let helper = make_test_helper();
        assert!(helper.is_incomplete("for x in 1 2 3; do"));
        assert!(!helper.is_incomplete("for x in 1 2 3; do\n  echo $x\ndone"));
    }

    #[test]
    fn test_is_incomplete_unclosed_single_quote() {
        let helper = make_test_helper();
        assert!(helper.is_incomplete("echo 'hello"));
        assert!(!helper.is_incomplete("echo 'hello'"));
    }

    #[test]
    fn test_is_incomplete_unclosed_double_quote() {
        let helper = make_test_helper();
        assert!(helper.is_incomplete("echo \"hello"));
        assert!(!helper.is_incomplete("echo \"hello\""));
    }

    #[test]
    fn test_is_incomplete_backslash_continuation() {
        let helper = make_test_helper();
        assert!(helper.is_incomplete("echo hello \\"));
        assert!(!helper.is_incomplete("echo hello"));
    }

    #[test]
    fn test_is_incomplete_while_loop() {
        let helper = make_test_helper();
        assert!(helper.is_incomplete("while true; do"));
        assert!(!helper.is_incomplete("while true; do\n  echo loop\ndone"));
    }

    #[test]
    fn test_is_incomplete_nested() {
        let helper = make_test_helper();
        assert!(helper.is_incomplete("if true; then\n  for x in 1 2; do"));
        assert!(helper.is_incomplete("if true; then\n  for x in 1 2; do\n    echo $x\n  done"));
        assert!(!helper.is_incomplete("if true; then\n  for x in 1 2; do\n    echo $x\n  done\nfi"));
    }

    #[test]
    fn test_is_incomplete_empty() {
        let helper = make_test_helper();
        assert!(!helper.is_incomplete(""));
        assert!(!helper.is_incomplete("echo hello"));
    }

    #[test]
    fn test_detect_context_command_start() {
        assert!(matches!(
            detect_completion_context("", 0),
            CompletionContext::Command
        ));
        assert!(matches!(
            detect_completion_context("ec", 2),
            CompletionContext::Command
        ));
    }

    #[test]
    fn test_detect_context_after_pipe() {
        assert!(matches!(
            detect_completion_context("echo hello | gr", 15),
            CompletionContext::Command
        ));
    }

    #[test]
    fn test_detect_context_variable() {
        assert!(matches!(
            detect_completion_context("echo $HO", 8),
            CompletionContext::Variable
        ));
        assert!(matches!(
            detect_completion_context("echo ${HO", 9),
            CompletionContext::Variable
        ));
    }

    #[test]
    fn test_detect_context_path() {
        assert!(matches!(
            detect_completion_context("cat /etc/hos", 12),
            CompletionContext::Path
        ));
    }

    #[test]
    fn test_detect_context_command_substitution() {
        // $(cmd should complete commands, not variables
        assert!(matches!(
            detect_completion_context("echo $(ca", 9),
            CompletionContext::Command
        ));
        assert!(matches!(
            detect_completion_context("X=$(ec", 6),
            CompletionContext::Command
        ));
    }

    #[test]
    fn test_shell_words_comments() {
        // Keywords in comments should be ignored
        assert_eq!(shell_words("# if this happens"), Vec::<String>::new());
        assert_eq!(shell_words("echo hello # if comment"), vec!["echo", "hello"]);
    }

    #[test]
    fn test_is_incomplete_comment_with_keyword() {
        let helper = make_test_helper();
        // Comments containing keywords should NOT make input incomplete
        assert!(!helper.is_incomplete("# if this happens"));
        assert!(!helper.is_incomplete("echo hello # if we do this"));
    }

    /// Create a test helper (kernel is not used for is_incomplete).
    fn make_test_helper() -> KaishHelper {
        let config = KernelConfig::transient();
        let kernel = Kernel::new(config).expect("test kernel");
        let rt = Runtime::new().expect("test runtime");
        KaishHelper::new(Arc::new(kernel), rt.handle().clone())
    }
}
