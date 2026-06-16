//! kaish CLI entry point.
//!
//! Usage:
//!   kaish                      # Interactive REPL
//!   kaish -c <command>         # Execute command and exit
//!   kaish script.kai           # Run a script

use std::env;
use std::io::{IsTerminal, Read};
use std::process::ExitCode;

use anyhow::{bail, Context, Result};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Read piped stdin for a non-interactive run so a top-level command (`sort`,
/// `cut`, `wc`) can consume it — `printf '…' | kaish -c sort`.
///
/// Returns `None` when stdin is a TTY (interactive): we must not block reading
/// from a keyboard the user isn't piping into. Binary stdin is rejected loudly
/// rather than lossily decoded — kaish feeds binary through files/VFS, and
/// silently corrupting it would violate the fail-loud contract.
fn read_piped_stdin() -> Result<Option<String>> {
    let mut handle = std::io::stdin();
    if handle.is_terminal() {
        return Ok(None);
    }
    let mut buf = Vec::new();
    handle.read_to_end(&mut buf).context("reading stdin")?;
    if buf.is_empty() {
        return Ok(None);
    }
    match String::from_utf8(buf) {
        Ok(s) => Ok(Some(s)),
        Err(_) => bail!("stdin is not valid UTF-8; pipe binary data through a file or the VFS"),
    }
}

fn main() -> ExitCode {
    // Initialize tracing (respects RUST_LOG env var)
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .init();

    match run() {
        Ok(code) => code,
        Err(e) => {
            eprintln!("Error: {e:?}");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<ExitCode> {
    let args: Vec<String> = env::args().collect();

    // Extract --overlay flag (can appear anywhere before positionals).
    let overlay = args.iter().any(|a| a == "--overlay");
    // Remaining args with --overlay stripped out.
    let rest: Vec<&str> = args.iter().skip(1)
        .filter(|a| *a != "--overlay")
        .map(|a| a.as_str())
        .collect();

    // Parse arguments
    match rest.first().copied() {
        None => {
            // No args: interactive REPL
            kaish_repl::run_with_overlay(overlay)?;
            Ok(ExitCode::SUCCESS)
        }

        Some("--help" | "-h") => {
            print_help();
            Ok(ExitCode::SUCCESS)
        }

        Some("--version" | "-V") => {
            println!("kaish {} ({} {})",
                     env!("CARGO_PKG_VERSION"),
                     env!("KAISH_GIT_HASH"),
                     env!("KAISH_BUILD_DATE"));
            Ok(ExitCode::SUCCESS)
        }

        Some("-c") => {
            let cmd = rest.get(1).copied()
                .context("-c requires a command argument")?;
            run_command(cmd, overlay)
        }

        Some(path) if !path.starts_with('-') => {
            // Treat as script file
            run_script(path, overlay)
        }

        Some(unknown) => {
            eprintln!("Unknown option: {unknown}");
            eprintln!("Run 'kaish --help' for usage.");
            Ok(ExitCode::FAILURE)
        }
    }
}

fn print_help() {
    println!(r#"会sh — kaish v{}

Usage:
  kaish                        Interactive REPL
  kaish -c <command>           Execute command and exit
  kaish <script.kai>           Run a script file

Options:
  --overlay                    Enable copy-on-write overlay mode (writes are
                               virtual; use kaish-vfs commit to apply them)
  -c <command>                 Execute command string and exit
  -h, --help                   Show this help
  -V, --version                Show version

Examples:
  kaish                        # Start interactive REPL
  kaish --overlay              # REPL with virtual writes (overlay mode)
  kaish -c 'echo hello'       # Run a command
  kaish --overlay -c 'echo test > file.txt; kaish-vfs diff'
  kaish deploy.kai             # Run a deployment script
"#, env!("CARGO_PKG_VERSION"));
}

/// Run a script file.
fn run_script(path: &str, overlay: bool) -> Result<ExitCode> {
    use kaish_client::EmbeddedClient;
    use kaish_kernel::{Kernel, KernelConfig};

    // Read the script
    let source = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read script: {path}"))?;

    // Skip shebang if present
    let source = if source.starts_with("#!") {
        source.lines().skip(1).collect::<Vec<_>>().join("\n")
    } else {
        source
    };

    // Non-interactive: pipe stdout so command substitution captures output.
    // The streaming callback below still prints output for the user.
    let config = KernelConfig::repl()
        .with_initial_vars(kaish_repl::os_env_vars())
        .with_overlay(overlay);
    let kernel = Kernel::new(config)
        .context("Failed to create kernel")?;

    let client = EmbeddedClient::new(kernel);

    let rt = tokio::runtime::Runtime::new()?;
    // Set $0 to the script path
    rt.block_on(client.kernel().set_positional(path, vec![]));
    // Forward any upstream W3C trace context (TRACEPARENT/TRACESTATE/BAGGAGE)
    // so e.g. `otel-cli exec -- kaish script.kai` traces across the boundary.
    let mut opts = kaish_repl::trace_options_from_env();
    if let Some(stdin) = read_piped_stdin()? {
        opts = opts.with_stdin(stdin);
    }
    let result = rt.block_on(client.execute_with_options_streaming(&source, opts, &mut |r| {
        let text = r.text_out();
        if !text.is_empty() {
            print!("{}", text);
        }
        if !r.err.is_empty() {
            eprint!("{}", r.err);
        }
    }))?;

    if result.ok() {
        Ok(ExitCode::SUCCESS)
    } else {
        Ok(ExitCode::from(result.code as u8))
    }
}

/// Execute a command string and exit.
fn run_command(cmd: &str, overlay: bool) -> Result<ExitCode> {
    use kaish_client::EmbeddedClient;
    use kaish_kernel::{Kernel, KernelConfig};

    // Non-interactive: pipe stdout so command substitution captures output.
    // The streaming callback below still prints output for the user.
    let config = KernelConfig::repl()
        .with_initial_vars(kaish_repl::os_env_vars())
        .with_overlay(overlay);
    let kernel = Kernel::new(config)
        .context("Failed to create kernel")?;

    let client = EmbeddedClient::new(kernel);

    let rt = tokio::runtime::Runtime::new()?;
    // Forward any upstream W3C trace context (TRACEPARENT/TRACESTATE/BAGGAGE)
    // so e.g. `otel-cli exec -- kaish -c '…'` traces across the boundary.
    let mut opts = kaish_repl::trace_options_from_env();
    if let Some(stdin) = read_piped_stdin()? {
        opts = opts.with_stdin(stdin);
    }
    let result = rt.block_on(client.execute_with_options_streaming(cmd, opts, &mut |r| {
        let text = r.text_out();
        if !text.is_empty() {
            print!("{}", text);
        }
        if !r.err.is_empty() {
            eprint!("{}", r.err);
        }
    }))?;

    if result.ok() {
        Ok(ExitCode::SUCCESS)
    } else {
        Ok(ExitCode::from(result.code as u8))
    }
}
