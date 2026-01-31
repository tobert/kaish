//! kaish REPL — Interactive shell for 会sh.
//!
//! This REPL provides an interactive interface to the kaish kernel.
//! It handles:
//! - Meta-commands: `/help`, `/quit`, `/ast`, `/scope`, `/cwd`, `/jobs`, `/tools`
//! - Command execution via the Kernel
//! - Result formatting with DisplayHints
//! - Command history via rustyline

pub mod format;

use std::path::PathBuf;

use anyhow::{Context, Result};
use rustyline::error::ReadlineError;
use rustyline::history::DefaultHistory;
use rustyline::Editor;
use tokio::runtime::Runtime;

use kaish_kernel::ast::Value;
use kaish_kernel::interpreter::ExecResult;
use kaish_kernel::{Kernel, KernelConfig};

/// Result from meta-command handling.
#[derive(Debug)]
enum MetaResult {
    /// Continue with optional output
    Continue(Option<String>),
    /// Exit the REPL (caller should save history and exit)
    Exit,
}

/// REPL configuration and state.
pub struct Repl {
    kernel: Kernel,
    runtime: Runtime,
    show_ast: bool,
}

impl Repl {
    /// Create a new REPL instance with passthrough filesystem access.
    pub fn new() -> Result<Self> {
        // Use REPL config for passthrough filesystem access
        // This gives full filesystem access, appropriate for a human-operated REPL
        // Skip validation to allow experimentation with bash-like syntax
        let config = KernelConfig::repl().with_skip_validation(true);
        let kernel = Kernel::new(config).context("Failed to create kernel")?;

        // Create tokio runtime for async kernel execution
        let runtime = Runtime::new().context("Failed to create tokio runtime")?;

        Ok(Self {
            kernel,
            runtime,
            show_ast: false,
        })
    }

    /// Create a new REPL with a custom kernel configuration.
    pub fn with_config(config: KernelConfig) -> Result<Self> {
        let kernel = Kernel::new(config).context("Failed to create kernel")?;
        let runtime = Runtime::new().context("Failed to create tokio runtime")?;

        Ok(Self {
            kernel,
            runtime,
            show_ast: false,
        })
    }

    /// Create a new REPL with MCP tools pre-registered.
    ///
    /// This allows MCP tools to be available in the REPL.
    ///
    /// # Example
    ///
    /// ```ignore
    /// use kaish_kernel::{Kernel, KernelConfig};
    /// use kaish_kernel::tools::ToolRegistry;
    ///
    /// // Create kernel with custom config
    /// let config = KernelConfig::repl();
    /// let repl = Repl::with_config(config)?;
    /// ```
    pub fn with_root(root: PathBuf) -> Result<Self> {
        let config = KernelConfig::repl().with_cwd(root);
        Self::with_config(config)
    }

    /// Process a single line of input.
    /// Returns Ok(None) for empty input, Ok(Some(output)) for output to display,
    /// or Err with ProcessResult::Exit to signal the REPL should exit.
    pub fn process_line(&mut self, line: &str) -> Result<Option<String>> {
        let trimmed = line.trim();

        // Handle meta-commands (both /cmd and cmd forms for common ones)
        if trimmed.starts_with('/') {
            return match self.handle_meta_command(trimmed) {
                MetaResult::Continue(output) => Ok(output),
                MetaResult::Exit => Err(anyhow::anyhow!("__REPL_EXIT__")),
            };
        }

        // Also support shell-style meta-commands without slash
        if let Some(meta_result) = self.try_shell_style_command(trimmed) {
            return match meta_result {
                MetaResult::Continue(output) => Ok(output),
                MetaResult::Exit => Err(anyhow::anyhow!("__REPL_EXIT__")),
            };
        }

        // Skip empty lines
        if trimmed.is_empty() {
            return Ok(None);
        }

        // Show AST if enabled
        if self.show_ast {
            match kaish_kernel::parser::parse(trimmed) {
                Ok(program) => return Ok(Some(format!("{:#?}", program))),
                Err(errors) => {
                    let mut msg = String::from("Parse error:\n");
                    for err in errors {
                        msg.push_str(&format!("  {err}\n"));
                    }
                    return Ok(Some(msg));
                }
            }
        }

        // Execute via kernel
        let result = self.runtime.block_on(self.kernel.execute(trimmed));

        match result {
            Ok(exec_result) => Ok(Some(format_result(&exec_result))),
            Err(e) => Ok(Some(format!("Error: {}", e))),
        }
    }

    /// Handle a meta-command (starts with /).
    fn handle_meta_command(&mut self, cmd: &str) -> MetaResult {
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        let command = parts.first().copied().unwrap_or("");

        match command {
            "/quit" | "/q" | "/exit" => MetaResult::Exit,
            "/help" | "/h" | "/?" => MetaResult::Continue(Some(HELP_TEXT.to_string())),
            "/ast" => {
                self.show_ast = !self.show_ast;
                MetaResult::Continue(Some(format!(
                    "AST mode: {}",
                    if self.show_ast { "ON" } else { "OFF" }
                )))
            }
            "/scope" | "/vars" => {
                let vars = self.runtime.block_on(self.kernel.list_vars());
                if vars.is_empty() {
                    MetaResult::Continue(Some("(no variables set)".to_string()))
                } else {
                    let mut output = String::from("Variables:\n");
                    for (name, value) in vars {
                        output.push_str(&format!("  {} = {}\n", name, format_value(&value)));
                    }
                    MetaResult::Continue(Some(output.trim_end().to_string()))
                }
            }
            "/result" | "/$?" => {
                let result = self.runtime.block_on(self.kernel.last_result());
                MetaResult::Continue(Some(format_result(&result)))
            }
            "/cwd" => {
                let cwd = self.runtime.block_on(self.kernel.cwd());
                MetaResult::Continue(Some(cwd.to_string_lossy().to_string()))
            }
            "/tools" => {
                let schemas = self.kernel.tool_schemas();
                let names: Vec<_> = schemas.iter().map(|s| s.name.as_str()).collect();
                MetaResult::Continue(Some(format!("Available tools: {}", names.join(", "))))
            }
            "/jobs" => {
                let jobs = self.runtime.block_on(self.kernel.jobs().list());
                if jobs.is_empty() {
                    MetaResult::Continue(Some("(no background jobs)".to_string()))
                } else {
                    let mut output = String::from("Background jobs:\n");
                    for job in jobs {
                        output.push_str(&format!("  [{}] {} {}\n", job.id, job.status, job.command));
                    }
                    MetaResult::Continue(Some(output.trim_end().to_string()))
                }
            }
            "/state" | "/session" => {
                let vars = self.runtime.block_on(self.kernel.list_vars());
                MetaResult::Continue(Some(format!(
                    "Kernel: {}\nVariables: {}",
                    self.kernel.name(),
                    vars.len()
                )))
            }
            "/clear-state" | "/reset" => {
                // Reset the kernel
                if let Err(e) = self.runtime.block_on(self.kernel.reset()) {
                    MetaResult::Continue(Some(format!("Reset failed: {}", e)))
                } else {
                    MetaResult::Continue(Some("Session reset (variables cleared)".to_string()))
                }
            }
            _ => MetaResult::Continue(Some(format!(
                "Unknown command: {}\nType /help or help for available commands.",
                command
            ))),
        }
    }

    /// Try to handle a shell-style command (without leading /).
    /// Returns Some(result) if it was a recognized command, None otherwise.
    fn try_shell_style_command(&mut self, cmd: &str) -> Option<MetaResult> {
        let parts: Vec<&str> = cmd.split_whitespace().collect();
        let command = parts.first().copied().unwrap_or("");

        match command {
            "quit" | "exit" => Some(self.handle_meta_command("/quit")),
            "help" => Some(self.handle_meta_command("/help")),
            "reset" => Some(self.handle_meta_command("/reset")),
            _ => None,
        }
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new().expect("Failed to create REPL")
    }
}

/// Format a Value for display (with quotes on strings).
fn format_value(value: &Value) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => format!("\"{}\"", s),
        Value::Json(json) => json.to_string(),
        Value::Blob(blob) => format!("[blob: {} {}]", blob.formatted_size(), blob.content_type),
    }
}

/// Format an ExecResult for display.
///
/// Uses display hints when available, otherwise falls back to status+output format.
fn format_result(result: &ExecResult) -> String {
    use kaish_kernel::interpreter::DisplayHint;

    // If there's a display hint, use the formatter
    if !matches!(result.hint, DisplayHint::None) {
        let context = format::detect_context();
        let formatted = format::format_output(result, context);

        // For failures, append error info
        if !result.ok() && !result.err.is_empty() {
            return format!("{}\n✗ code={} err=\"{}\"", formatted, result.code, result.err);
        }
        return formatted;
    }

    // No display hint - use classic status format
    let status = if result.ok() { "✓" } else { "✗" };
    let mut output = format!("{} code={}", status, result.code);

    if !result.out.is_empty() {
        if result.out.contains('\n') {
            output.push_str(&format!("\n{}", result.out));
        } else {
            output.push_str(&format!(" out={}", result.out));
        }
    }

    if !result.err.is_empty() {
        output.push_str(&format!(" err=\"{}\"", result.err));
    }

    output
}

const HELP_TEXT: &str = r#"会sh — kaish REPL

Meta Commands (use with or without /):
  help, /help, /?   Show this help
  quit, /quit, /q   Exit the REPL
  reset, /reset     Clear in-memory state

Slash-only commands:
  /ast              Toggle AST display mode
  /scope, /vars     Show all variables (alt: `vars` builtin)
  /result, /$?      Show last command result
  /cwd              Show current working directory
  /tools            List available tools (alt: `tools` builtin)
  /jobs             List background jobs
  /state, /session  Show session info

Built-in Tools:
  echo [args...]    Print arguments
  cat <path> [-n]   Read file contents (-n for line numbers)
  ls [path] [-la]   List directory (-a hidden, -l long)
  cd [path | -]     Change directory (- for previous)
  pwd               Print working directory
  mkdir <path>      Create directory
  rm <path> [-rf]   Remove file/directory
  cp <src> <dst> [-r]  Copy file/directory
  mv <src> <dst>    Move/rename
  grep <pattern> [path] [-inv]  Search patterns
  write <path> <content>  Write to file
  date [format]     Current date/time
  assert <cond>     Assert condition (for tests)
  help [tool]       Show tool help
  jobs              List background jobs
  wait [job_id]     Wait for background jobs

External Commands:
  Commands not found as builtins are searched in PATH
  and executed as external processes (cargo, git, etc.)

Language:
  X=value           Assign a variable
  ${VAR}            Variable reference
  ${VAR.field}      Nested access
  ${?.ok}           Last result access
  a | b | c         Pipeline (connects stdout → stdin)
  cmd &             Run in background
  if cond; then ... fi
  for X in arr; do ... done

Examples:
  ls                         # List current directory
  cat file.txt | grep hello  # Pipeline: search in file
  echo hello | grep ell      # Pipeline: filter text
  sleep 5 &                  # Background job
  cargo build                # External command
  date +%s                   # Date format string
"#;

/// Save REPL history to disk.
fn save_history(rl: &mut Editor<(), DefaultHistory>, history_path: &Option<PathBuf>) {
    if let Some(path) = history_path {
        if let Some(parent) = path.parent() {
            if let Err(e) = std::fs::create_dir_all(parent) {
                tracing::warn!("Failed to create history directory: {}", e);
            }
        }
        if let Err(e) = rl.save_history(path) {
            tracing::warn!("Failed to save history: {}", e);
        }
    }
}

/// Run the REPL.
pub fn run() -> Result<()> {
    println!("会sh — kaish v{}", env!("CARGO_PKG_VERSION"));
    println!("Type /help for commands, /quit to exit.");

    let mut rl: Editor<(), DefaultHistory> =
        Editor::new().context("Failed to create editor")?;

    // Load history if it exists
    let history_path = directories::BaseDirs::new()
        .map(|b| b.data_dir().join("kaish").join("history.txt"));
    if let Some(ref path) = history_path {
        if let Err(e) = rl.load_history(path) {
            // Only log if it's not a "file not found" error (expected on first run)
            let is_not_found = matches!(&e, ReadlineError::Io(io_err) if io_err.kind() == std::io::ErrorKind::NotFound);
            if !is_not_found {
                tracing::warn!("Failed to load history: {}", e);
            }
        }
    }

    let mut repl = Repl::new()?;
    println!();

    loop {
        let prompt = "会sh> ";

        match rl.readline(prompt) {
            Ok(line) => {
                if let Err(e) = rl.add_history_entry(line.as_str()) {
                    tracing::warn!("Failed to add history entry: {}", e);
                }

                match repl.process_line(&line) {
                    Ok(Some(output)) => println!("{}", output),
                    Ok(None) => {}
                    Err(e) if e.to_string() == "__REPL_EXIT__" => {
                        // User requested exit - save history and break
                        save_history(&mut rl, &history_path);
                        return Ok(());
                    }
                    Err(e) => eprintln!("Error: {}", e),
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

    // Save history
    save_history(&mut rl, &history_path);

    Ok(())
}

/// Run a REPL connected to a remote kernel via IpcClient.
///
/// This REPL forwards commands to the remote kernel and displays results.
/// The `LocalSet` is required because IpcClient uses spawn_local internally.
pub fn run_with_client(
    client: kaish_client::IpcClient,
    rt: &Runtime,
    local: &tokio::task::LocalSet,
) -> Result<()> {
    use kaish_client::KernelClient;

    let mut rl: Editor<(), DefaultHistory> =
        Editor::new().context("Failed to create editor")?;

    // Load history
    let history_path = directories::BaseDirs::new()
        .map(|b| b.data_dir().join("kaish").join("history.txt"));
    if let Some(ref path) = history_path {
        let _ = rl.load_history(path);
    }

    println!("会sh — kaish v{} (connected)", env!("CARGO_PKG_VERSION"));
    println!("Type /help for commands, /quit to exit.");
    println!();

    loop {
        let prompt = "会sh> ";

        match rl.readline(prompt) {
            Ok(line) => {
                let _ = rl.add_history_entry(line.as_str());

                let trimmed = line.trim();

                // Handle local meta-commands
                if trimmed == "/quit" || trimmed == "/q" || trimmed == "quit" || trimmed == "exit"
                {
                    break;
                }
                if trimmed == "/help" || trimmed == "/?" || trimmed == "help" {
                    println!("{}", HELP_TEXT);
                    continue;
                }

                if trimmed.is_empty() {
                    continue;
                }

                // Send to remote kernel
                let result = local.block_on(rt, async { client.execute(trimmed).await });

                match result {
                    Ok(exec_result) => {
                        println!("{}", format_result(&exec_result));
                    }
                    Err(e) => {
                        eprintln!("Error: {}", e);
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

    // Save history
    save_history(&mut rl, &history_path);

    Ok(())
}
