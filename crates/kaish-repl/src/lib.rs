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

use anyhow::{Context, Result};
use rustyline::completion::{Completer, FilenameCompleter, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::Highlighter;
use rustyline::hint::{Hint, Hinter};
use rustyline::history::DefaultHistory;
use rustyline::validate::{ValidationContext, ValidationResult, Validator};
use rustyline::{Editor, Helper};
use tokio::runtime::Runtime;

use kaish_client::{EmbeddedClient, KernelClient};
use kaish_kernel::ast::Value;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{ExecuteOptions, Kernel, KernelConfig};

/// Snapshot the OS environment as a map of `String` → `Value::String`.
///
/// The kernel itself is hermetic — it never reads `std::env::vars()`. The REPL
/// (and other shell-like frontends) call this and pass the result via
/// `KernelConfig::with_initial_vars` so users get their normal `PATH`, `HOME`,
/// `EDITOR`, etc. propagated to subprocesses.
pub fn os_env_vars() -> std::collections::HashMap<String, Value> {
    std::env::vars()
        .map(|(k, v)| (k, Value::String(v)))
        .collect()
}

/// Build per-call trace context from the W3C environment variables an upstream
/// tracer sets before invoking kaish: `TRACEPARENT`, `TRACESTATE`, and
/// `BAGGAGE` (the W3C `baggage` header format, `key1=value1,key2=value2`).
///
/// This is the REPL frontend's analogue of the MCP server lifting trace context
/// off the request `_meta`. The kernel is hermetic and never reads env, so a
/// frontend must hand it the context explicitly via [`ExecuteOptions`]. Used by
/// the non-interactive entry points (`kaish script.kai`, `kaish -c '…'`) so a
/// wrapper like `otel-cli exec -- kaish …` traces across the kaish boundary.
pub fn trace_options_from_env() -> ExecuteOptions {
    parse_trace_env(|key| std::env::var(key).ok())
}

/// Pure core of [`trace_options_from_env`], parameterised on the env lookup so
/// it is testable without mutating process-global environment.
fn parse_trace_env(get: impl Fn(&str) -> Option<String>) -> ExecuteOptions {
    let mut opts = ExecuteOptions::new();

    // `tracestate` is meaningless without a `traceparent` (W3C), and the kernel
    // drops it in that case anyway, so only read it alongside a traceparent.
    if let Some(traceparent) = get("TRACEPARENT").filter(|s| !s.is_empty()) {
        opts = opts.with_traceparent(traceparent);
        if let Some(tracestate) = get("TRACESTATE").filter(|s| !s.is_empty()) {
            opts = opts.with_tracestate(tracestate);
        }
    }

    if let Some(raw) = get("BAGGAGE").filter(|s| !s.is_empty()) {
        let baggage = parse_w3c_baggage(&raw);
        if !baggage.is_empty() {
            opts = opts.with_baggage(baggage);
        }
    }

    opts
}

/// Parse a W3C `baggage` header value (`k1=v1,k2=v2`) into a map.
///
/// Entries without a `=` are skipped (malformed — not silently turned into a
/// key with an empty value). Surrounding whitespace is trimmed. W3C allows
/// `;`-delimited properties after a value (`k=v;meta`); those are dropped,
/// keeping just the value. Percent-encoded values are taken verbatim —
/// identifiers in practice don't need decoding.
fn parse_w3c_baggage(raw: &str) -> std::collections::BTreeMap<String, String> {
    let mut map = std::collections::BTreeMap::new();
    for entry in raw.split(',') {
        let entry = entry.trim();
        if entry.is_empty() {
            continue;
        }
        let Some((key, value)) = entry.split_once('=') else {
            tracing::debug!(entry, "skipping malformed BAGGAGE entry (no '=')");
            continue;
        };
        let value = value.split(';').next().unwrap_or(value);
        map.insert(key.trim().to_string(), value.trim().to_string());
    }
    map
}

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
///
/// Holds the kernel as a `KernelClient` trait object so completion is driven
/// purely through the client abstraction — the same surface any embedder
/// (including a future remote client) would use.
struct KaishHelper {
    client: Box<dyn KernelClient>,
    handle: tokio::runtime::Handle,
    path_completer: FilenameCompleter,
}

impl KaishHelper {
    fn new(client: Box<dyn KernelClient>, handle: tokio::runtime::Handle) -> Self {
        Self {
            client,
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

        if depth > 0 {
            return true;
        }

        // Heredoc continuation: ask the lexer whether any heredoc started
        // without seeing its closing delimiter line. Single source of truth
        // with the parser — no parallel hand-rolled heredoc scanner.
        // Other lexer errors (invalid token, etc.) aren't continuation
        // signals; let the kernel surface them on submit.
        if let Err(errs) = kaish_kernel::lexer::tokenize(input)
            && errs
                .iter()
                .any(|e| matches!(e.token, kaish_kernel::lexer::LexerError::UnterminatedHeredoc { .. }))
        {
            return true;
        }

        false
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

                // Tool/builtin names, via the client. Completion is a
                // best-effort affordance: if the client errors, warn and
                // offer nothing rather than failing the keystroke.
                let schemas = match self.handle.block_on(self.client.tool_schemas()) {
                    Ok(schemas) => schemas,
                    Err(e) => {
                        tracing::warn!("completion: tool_schemas failed: {e}");
                        Vec::new()
                    }
                };
                for schema in schemas {
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

                // list_vars is async, use block_on. Best-effort: on error,
                // warn and offer no variable candidates.
                let vars = match self.handle.block_on(self.client.list_vars()) {
                    Ok(vars) => vars,
                    Err(e) => {
                        tracing::warn!("completion: list_vars failed: {e}");
                        Vec::new()
                    }
                };

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
    client: EmbeddedClient,
    runtime: Runtime,
}

/// Build the tokio runtime kaish execution runs on, with worker threads sized
/// to [`kaish_kernel::RECOMMENDED_STACK_SIZE`] so deeply nested command
/// substitution / shell functions / `.kai` scripts reach the interpreter's
/// recursion guard (a loud error) instead of overflowing the default ~2 MB
/// worker stack (GH #46/#47). The `block_on` *driver* thread is sized
/// separately in `main.rs` — tokio doesn't own it.
pub fn build_runtime() -> Result<tokio::runtime::Runtime> {
    tokio::runtime::Builder::new_multi_thread()
        .thread_stack_size(kaish_kernel::RECOMMENDED_STACK_SIZE)
        .enable_all()
        .build()
        .context("Failed to create tokio runtime")
}

impl Repl {
    /// Create a new REPL instance with passthrough filesystem access.
    pub fn new() -> Result<Self> {
        let config = KernelConfig::repl()
            .with_interactive(true)
            .with_initial_vars(os_env_vars());
        let mut kernel = Kernel::new(config).context("Failed to create kernel")?;
        let runtime = build_runtime()?;

        // Initialize terminal job control if stdin is a TTY. See `new()`.
        #[cfg(unix)]
        if std::io::stdin().is_terminal() {
            kernel.init_terminal();
        }

        Ok(Self {
            client: EmbeddedClient::new(kernel),
            runtime,
        })
    }

    /// Create a new REPL with a custom kernel configuration.
    pub fn with_config(config: KernelConfig) -> Result<Self> {
        let mut kernel = Kernel::new(config).context("Failed to create kernel")?;
        let runtime = build_runtime()?;

        // Initialize terminal job control if stdin is a TTY. This needs the
        // owned kernel (&mut, local-TTY setup), so it stays as pre-wrap
        // kernel setup rather than a client-trait call.
        #[cfg(unix)]
        if std::io::stdin().is_terminal() {
            kernel.init_terminal();
        }

        Ok(Self {
            client: EmbeddedClient::new(kernel),
            runtime,
        })
    }

    /// Create a new REPL rooted at the given path.
    pub fn with_root(root: PathBuf) -> Result<Self> {
        let config = KernelConfig::repl()
            .with_cwd(root)
            .with_initial_vars(os_env_vars());
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

        // Execute via the client with SIGINT handling.
        // A per-execute signal listener catches Ctrl-C during execution,
        // cancels the kernel, and returns exit code 130.
        let client = self.client.clone();
        let input = trimmed.to_string();
        let result = self.runtime.block_on(async {
            let mut sigint = tokio::signal::unix::signal(
                tokio::signal::unix::SignalKind::interrupt(),
            )?;
            tokio::select! {
                result = client.execute(&input) => result,
                _ = sigint.recv() => {
                    client.cancel().await?;
                    Ok(ExecResult::failure(130, ""))
                }
            }
        });

        match result {
            Ok(exec_result) => {
                if exec_result.ok() && !exec_result.has_output() && exec_result.text_out().is_empty() {
                    ProcessResult::Empty
                } else {
                    ProcessResult::Output(format_result(&exec_result))
                }
            }
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
    if result.has_output() {
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
        result.text_out().into_owned()
    } else {
        let mut output = String::new();
        let text = result.text_out();
        if !text.is_empty() {
            output.push_str(&text);
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

/// Build the warning line for a `source <rc-file>` outcome, if it's worth
/// surfacing to the user — `None` when the rc file sourced cleanly.
///
/// A nonzero exit (a typo'd command, a failed source line) is not an `Err` —
/// `execute()` still returns `Ok` with the failure folded into the
/// `ExecResult`. Silently discarding that used to leave the user with a
/// half-loaded rc environment and zero indication why (GH #129); this now
/// reports the exit code and any diagnostic text, matching the style of the
/// pre-existing hard-`Err` warning below it.
fn rc_file_warning(path: &std::path::Path, outcome: &kaish_client::ClientResult<ExecResult>) -> Option<String> {
    match outcome {
        Ok(result) if !result.ok() => {
            let err = result.err.trim_end();
            Some(if err.is_empty() {
                format!("kaish: warning: {} exited with code {}", path.display(), result.code)
            } else {
                format!(
                    "kaish: warning: {} exited with code {}: {}",
                    path.display(),
                    result.code,
                    err
                )
            })
        }
        Ok(_) => None,
        Err(e) => Some(format!("kaish: warning: error sourcing {}: {}", path.display(), e)),
    }
}

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
            let outcome = repl.runtime.block_on(repl.client.execute(&cmd));
            if let Some(warning) = rc_file_warning(path, &outcome) {
                eprintln!("{warning}");
            }
            return;
        }
    }
}

/// Resolve the prompt string: call `kaish_prompt()` if defined, else default.
fn resolve_prompt(repl: &Repl) -> String {
    let has_fn = repl
        .runtime
        .block_on(repl.client.has_function("kaish_prompt"))
        .unwrap_or(false);
    if has_fn {
        if let Ok(result) = repl.runtime.block_on(repl.client.execute("kaish_prompt")) {
            if result.ok() {
                let text = result.text_out().trim_end().to_string();
                if !text.is_empty() {
                    return text;
                }
            }
        }
    }
    "会sh> ".to_string()
}

/// Check the JobManager for jobs that finished since the last prompt, print a
/// one-line notification for each (matching the `jobs` builtin's own
/// `[id] status command` line), and reap them.
///
/// A `Latched` job is excluded by `reap_finished` itself — it's "done" in the
/// sense that its future resolved, but it's awaiting confirmation of a
/// pending destructive-operation gate (`set -o latch`) and must stay tracked
/// until confirmed or explicitly discarded (GH #96), not be silently
/// destroyed by an automatic background sweep (GH #131).
fn notify_finished_jobs(repl: &Repl) {
    let manager = repl.client.kernel().jobs();
    for info in repl.runtime.block_on(manager.reap_finished()) {
        println!("[{}] {} {}", info.id, info.status, info.command);
    }
}

// ── Entry points ────────────────────────────────────────────────────

/// Run the REPL with optional overlay mode.
///
/// When `overlay` is `true`, writes are virtual (copy-on-write overlay).
/// Use `kaish-vfs commit` to apply changes to real files.
pub fn run_with_overlay(overlay: bool) -> Result<()> {
    println!("会sh — kaish v{}", env!("CARGO_PKG_VERSION"));
    use kaish_kernel::help::{compose, Recipe, SchemaContent};

    if overlay {
        println!("[overlay mode: writes are virtual — use 'kaish-vfs commit' to apply]");
    }
    println!("{}", compose(&Recipe::repl_welcome(), &SchemaContent::new(&[])));

    let config = KernelConfig::repl()
        .with_interactive(true)
        .with_initial_vars(os_env_vars())
        .with_overlay(overlay);
    let mut repl = Repl::with_config(config)?;

    // Source RC file (interactive only)
    load_rc_file(&repl);

    let helper = KaishHelper::new(
        Box::new(repl.client.clone()),
        repl.runtime.handle().clone(),
    );

    let mut rl: Editor<KaishHelper, DefaultHistory> =
        Editor::new().context("Failed to create editor")?;
    rl.set_helper(Some(helper));

    let history_path = load_history(&mut rl);

    loop {
        notify_finished_jobs(&repl);
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

/// Run the REPL.
pub fn run() -> Result<()> {
    println!("会sh — kaish v{}", env!("CARGO_PKG_VERSION"));
    // Welcome text comes from the canonical kaish-help corpus (via the kernel
    // re-export), so it stays in sync with the MCP/embedder instructions.
    use kaish_kernel::help::{compose, Recipe, SchemaContent};
    println!("{}", compose(&Recipe::repl_welcome(), &SchemaContent::new(&[])));

    let mut repl = Repl::new()?;

    // Source RC file (interactive only)
    load_rc_file(&repl);

    // Build the helper with a client handle (sharing the REPL's kernel) and
    // a runtime handle. Boxed as a trait object so completion runs purely
    // through the KernelClient abstraction.
    let helper = KaishHelper::new(
        Box::new(repl.client.clone()),
        repl.runtime.handle().clone(),
    );

    let mut rl: Editor<KaishHelper, DefaultHistory> =
        Editor::new().context("Failed to create editor")?;
    rl.set_helper(Some(helper));

    let history_path = load_history(&mut rl);

    loop {
        notify_finished_jobs(&repl);
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

    /// Build an env getter over a fixed set of pairs — keeps trace-env tests
    /// off the process-global environment (which races under `cargo test`).
    fn env_of<'a>(pairs: &'a [(&'a str, &'a str)]) -> impl Fn(&str) -> Option<String> + 'a {
        move |key| {
            pairs
                .iter()
                .find(|(k, _)| *k == key)
                .map(|(_, v)| v.to_string())
        }
    }

    #[test]
    fn trace_env_empty_yields_default_options() {
        let opts = parse_trace_env(env_of(&[]));
        assert!(opts.traceparent.is_none());
        assert!(opts.tracestate.is_none());
        assert!(opts.baggage.is_empty());
    }

    #[test]
    fn trace_env_reads_traceparent_and_tracestate() {
        let opts = parse_trace_env(env_of(&[
            ("TRACEPARENT", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
            ("TRACESTATE", "vendor=opaque"),
        ]));
        assert_eq!(
            opts.traceparent.as_deref(),
            Some("00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
        );
        assert_eq!(opts.tracestate.as_deref(), Some("vendor=opaque"));
    }

    #[test]
    fn trace_env_drops_tracestate_without_traceparent() {
        let opts = parse_trace_env(env_of(&[("TRACESTATE", "vendor=opaque")]));
        assert!(opts.traceparent.is_none());
        assert!(opts.tracestate.is_none(), "tracestate alone is meaningless");
    }

    #[test]
    fn trace_env_treats_empty_string_as_unset() {
        let opts = parse_trace_env(env_of(&[("TRACEPARENT", ""), ("BAGGAGE", "")]));
        assert!(opts.traceparent.is_none());
        assert!(opts.baggage.is_empty());
    }

    #[test]
    fn trace_env_parses_baggage() {
        let opts = parse_trace_env(env_of(&[("BAGGAGE", "owner=atobey,tenant=acme")]));
        assert_eq!(opts.baggage.get("owner").map(String::as_str), Some("atobey"));
        assert_eq!(opts.baggage.get("tenant").map(String::as_str), Some("acme"));
    }

    #[test]
    fn baggage_drops_properties_and_skips_malformed() {
        // `k=v;prop` keeps only `v`; a member with no `=` is skipped; whitespace
        // around members and around `=` is trimmed.
        let map = parse_w3c_baggage("owner=atobey;ttl=60 , broken , tenant = acme ");
        assert_eq!(map.get("owner").map(String::as_str), Some("atobey"));
        assert_eq!(map.get("tenant").map(String::as_str), Some("acme"));
        assert!(!map.contains_key("broken"), "member without '=' is skipped");
        assert_eq!(map.len(), 2);
    }

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
    fn test_is_incomplete_unterminated_heredoc() {
        let helper = make_test_helper();
        // No closing EOF — REPL should prompt for more input.
        assert!(helper.is_incomplete("cat <<EOF"));
        assert!(helper.is_incomplete("cat <<EOF\nhello"));
        // <<-form (tab-strip) and quoted delimiters too.
        assert!(helper.is_incomplete("cat <<-DONE\n\thi"));
        assert!(helper.is_incomplete("cat <<'EOF'\n$VAR"));
        // Closing delimiter on its own line — complete.
        assert!(!helper.is_incomplete("cat <<EOF\nhello\nEOF"));
        assert!(!helper.is_incomplete("cat <<-DONE\n\thi\n\tDONE"));
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

    /// Create a test helper (the client is not used for is_incomplete).
    fn make_test_helper() -> KaishHelper {
        let config = KernelConfig::transient();
        let kernel = Kernel::new(config).expect("test kernel");
        let client = EmbeddedClient::new(kernel);
        let rt = Runtime::new().expect("test runtime");
        KaishHelper::new(Box::new(client), rt.handle().clone())
    }

    // Completion runs through the `Box<dyn KernelClient>` trait object. These
    // tests prove that path end-to-end: command names come from
    // `tool_schemas()` and variables from `list_vars()`, both via the client.
    #[test]
    fn test_completion_through_client() {
        let config = KernelConfig::transient();
        let kernel = Kernel::new(config).expect("test kernel");
        let client = EmbeddedClient::new(kernel);
        let rt = Runtime::new().expect("test runtime");

        // Seed a variable on a clone; it shares the kernel with the helper's
        // client, so the completer sees it.
        rt.block_on(client.set_var("MYVAR", Value::String("hi".into())))
            .expect("set_var failed");

        let helper = KaishHelper::new(Box::new(client), rt.handle().clone());
        let history = DefaultHistory::new();
        let ctx = rustyline::Context::new(&history);

        // Command completion: "ec" → echo (sourced from tool_schemas()).
        let (start, candidates) = helper.complete("ec", 2, &ctx).expect("command completion");
        assert_eq!(start, 0);
        assert!(
            candidates.iter().any(|p| p.replacement == "echo"),
            "expected `echo` among command candidates, got {:?}",
            candidates.iter().map(|p| &p.replacement).collect::<Vec<_>>()
        );

        // Variable completion: "$MY" → $MYVAR (sourced from list_vars()).
        let (start, candidates) = helper.complete("$MY", 3, &ctx).expect("variable completion");
        assert_eq!(start, 0);
        assert!(
            candidates.iter().any(|p| p.replacement == "$MYVAR"),
            "expected `$MYVAR` among variable candidates, got {:?}",
            candidates.iter().map(|p| &p.replacement).collect::<Vec<_>>()
        );
    }

    // GH #129: an rc-file source that returns `Ok(ExecResult)` with a nonzero
    // exit code used to be silently discarded — only a hard `Err` warned.
    #[test]
    fn rc_file_warning_none_when_source_succeeds() {
        let path = std::path::Path::new("/home/user/.config/kaish/init.kai");
        let outcome: kaish_client::ClientResult<ExecResult> = Ok(ExecResult::success("ok"));
        assert_eq!(rc_file_warning(path, &outcome), None);
    }

    #[test]
    fn rc_file_warning_reports_nonzero_exit_with_stderr() {
        let path = std::path::Path::new("/home/user/.config/kaish/init.kai");
        let outcome: kaish_client::ClientResult<ExecResult> =
            Ok(ExecResult::failure(127, "source: unknown-command: command not found"));
        let warning = rc_file_warning(path, &outcome).expect("nonzero exit must warn");
        assert!(warning.contains("exited with code 127"), "{warning}");
        assert!(warning.contains("unknown-command: command not found"), "{warning}");
    }

    #[test]
    fn rc_file_warning_reports_nonzero_exit_with_no_stderr() {
        // A failing result with no diagnostic text must still report the exit
        // code (no dangling ": " with nothing after it).
        let path = std::path::Path::new("/home/user/.kaishrc");
        let outcome: kaish_client::ClientResult<ExecResult> =
            Ok(ExecResult::failure(1, ""));
        let warning = rc_file_warning(path, &outcome).expect("nonzero exit must warn");
        assert!(warning.contains("exited with code 1"), "{warning}");
        assert!(!warning.trim_end().ends_with(':'), "{warning}");
    }

    #[test]
    fn rc_file_warning_reports_hard_err() {
        let path = std::path::Path::new("/home/user/.config/kaish/init.kai");
        let outcome: kaish_client::ClientResult<ExecResult> =
            Err(kaish_client::ClientError::Execution("recursion limit exceeded".to_string()));
        let warning = rc_file_warning(path, &outcome).expect("hard Err must warn");
        assert!(warning.contains("error sourcing"), "{warning}");
        assert!(warning.contains("recursion limit exceeded"), "{warning}");
    }
}
