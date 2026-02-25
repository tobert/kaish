//! kaish CLI entry point.
//!
//! Usage:
//!   kaish                      # Interactive REPL
//!   kaish -c <command>         # Execute command and exit
//!   kaish script.kai           # Run a script

use std::env;
use std::process::ExitCode;

use anyhow::{Context, Result};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

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

    // Parse arguments
    match args.get(1).map(|s| s.as_str()) {
        None => {
            // No args: interactive REPL
            kaish_repl::run()?;
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
            let cmd = args.get(2)
                .context("-c requires a command argument")?;
            run_command(cmd)
        }

        Some(path) if !path.starts_with('-') => {
            // Treat as script file
            run_script(path)
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
  -c <command>                 Execute command string and exit
  -h, --help                   Show this help
  -V, --version                Show version

Examples:
  kaish                        # Start interactive REPL
  kaish -c 'echo hello'       # Run a command
  kaish deploy.kai             # Run a deployment script
"#, env!("CARGO_PKG_VERSION"));
}

/// Run a script file.
fn run_script(path: &str) -> Result<ExitCode> {
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
    let config = KernelConfig::repl();
    let kernel = Kernel::new(config)
        .context("Failed to create kernel")?;

    let client = EmbeddedClient::new(kernel);

    let rt = tokio::runtime::Runtime::new()?;
    // Set $0 to the script path
    rt.block_on(client.kernel().set_positional(path, vec![]));
    let result = rt.block_on(client.execute_streaming(&source, &mut |r| {
        if !r.out.is_empty() {
            print!("{}", r.out);
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
fn run_command(cmd: &str) -> Result<ExitCode> {
    use kaish_client::EmbeddedClient;
    use kaish_kernel::{Kernel, KernelConfig};

    // Non-interactive: pipe stdout so command substitution captures output.
    // The streaming callback below still prints output for the user.
    let config = KernelConfig::repl();
    let kernel = Kernel::new(config)
        .context("Failed to create kernel")?;

    let client = EmbeddedClient::new(kernel);

    let rt = tokio::runtime::Runtime::new()?;
    let result = rt.block_on(client.execute_streaming(cmd, &mut |r| {
        if !r.out.is_empty() {
            print!("{}", r.out);
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
