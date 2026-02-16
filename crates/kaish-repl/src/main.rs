//! kaish CLI entry point.
//!
//! Usage:
//!   kaish                      # Interactive REPL
//!   kaish -c <command>         # Execute command and exit
//!   kaish script.kai           # Run a script
//!   kaish serve [--socket=X]   # Start RPC server
//!   kaish --connect <socket>   # REPL connected to remote kernel

use std::env;
use std::path::PathBuf;
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

        Some("serve") => {
            run_serve(&args[2..])
        }

        Some(arg) if arg.starts_with("--connect=") => {
            let socket = &arg["--connect=".len()..];
            run_repl_connected(socket)
        }

        Some("--connect") => {
            let socket = args.get(2)
                .context("--connect requires a socket path")?;
            run_repl_connected(socket)
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
  kaish serve [OPTIONS]        Start RPC server
  kaish --connect <socket>     REPL connected to remote kernel

Options:
  -c <command>                 Execute command string and exit
  -h, --help                   Show this help
  -V, --version                Show version

Serve Options:
  --socket=<path>              Socket path (default: $XDG_RUNTIME_DIR/kaish/default.sock)
  --name=<name>                Kernel name (default: "default")

Examples:
  kaish                        # Start interactive REPL
  kaish -c 'echo hello'       # Run a command
  kaish deploy.kai             # Run a deployment script
  kaish serve                  # Start kernel server
  kaish --connect /tmp/k.sock  # Connect REPL to running kernel
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

/// Start the RPC server.
fn run_serve(args: &[String]) -> Result<ExitCode> {
    use kaish_kernel::rpc::KernelRpcServer;
    use kaish_kernel::{Kernel, KernelConfig};
    use tokio::task::LocalSet;

    let mut socket_path: Option<PathBuf> = None;
    let mut kernel_name = "default".to_string();

    // Parse serve options
    for arg in args {
        if let Some(path) = arg.strip_prefix("--socket=") {
            socket_path = Some(PathBuf::from(path));
        } else if let Some(name) = arg.strip_prefix("--name=") {
            kernel_name = name.to_string();
        } else if arg == "--help" || arg == "-h" {
            println!("kaish serve - Start RPC server\n");
            println!("Options:");
            println!("  --socket=<path>  Socket path");
            println!("  --name=<name>    Kernel name (default: \"default\")");
            return Ok(ExitCode::SUCCESS);
        } else {
            eprintln!("Unknown serve option: {arg}");
            return Ok(ExitCode::FAILURE);
        }
    }

    // Create kernel
    let config = KernelConfig::named(&kernel_name);
    let kernel = Kernel::new(config)
        .context("Failed to create kernel")?;

    // Create and run server
    let server = KernelRpcServer::new(kernel);

    // Use LocalSet because Cap'n Proto RPC uses spawn_local
    let rt = tokio::runtime::Runtime::new()?;
    let local = LocalSet::new();

    local.block_on(&rt, async {
        if let Some(path) = socket_path {
            server.serve(&path).await
        } else {
            server.serve_default().await
        }
    })?;

    Ok(ExitCode::SUCCESS)
}

/// Run REPL connected to a remote kernel.
fn run_repl_connected(socket_path: &str) -> Result<ExitCode> {
    use kaish_client::IpcClient;
    use tokio::task::LocalSet;

    println!("会sh — kaish v{} (connected to {})",
             env!("CARGO_PKG_VERSION"), socket_path);
    println!("Type /help for commands, /quit to exit.\n");

    let rt = tokio::runtime::Runtime::new()?;
    let local = LocalSet::new();

    // Connect to remote kernel (must run inside LocalSet for spawn_local)
    let client = local.block_on(&rt, IpcClient::connect(socket_path))
        .with_context(|| format!("Failed to connect to kernel at {socket_path}"))?;

    // Run connected REPL
    kaish_repl::run_with_client(client, &rt, &local)?;

    Ok(ExitCode::SUCCESS)
}
