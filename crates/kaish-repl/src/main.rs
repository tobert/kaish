//! kaish CLI entry point.
//!
//! Usage:
//!   kaish                      # Interactive REPL
//!   kaish -c <command>         # Execute command and exit
//!   kaish script.kai           # Run a script

use std::env;
use std::io::{IsTerminal, Read};
use std::process::ExitCode;

use anyhow::{Context, Result};
use kaish_kernel::{pipe_stream_default, PipeReader};
use tracing_subscriber::{fmt, prelude::*, EnvFilter};

/// Bridge an open process stdin into the kernel as a **lazy** pipe so a
/// top-level command that reads stdin (`sort`, `cut`, `wc`) consumes it —
/// `printf '…' | kaish -c sort` — without forcing input to be read before
/// execution.
///
/// Returns `None` when stdin is a TTY: we don't seed a pipe whose background
/// read would block on the terminal (and could raise `SIGTTIN`); a TTY isn't a
/// piped-input source in `-c`/script mode. Otherwise spawns a **detached** OS
/// thread that copies process stdin → the pipe writer. The kernel drains the
/// reader only if a command actually reads stdin, so a command that doesn't
/// (`echo`) returns immediately even when stdin is an open pipe that never
/// sends EOF (`sleep 10 | kaish -c 'echo hi'`). The thread is abandoned at
/// process exit, so a read parked on such a pipe never delays shutdown — and
/// because the copy is byte-clean, binary stdin survives losslessly.
fn spawn_stdin_bridge(handle: tokio::runtime::Handle) -> Option<PipeReader> {
    let stdin = std::io::stdin();
    if stdin.is_terminal() {
        return None;
    }
    let (mut writer, reader) = pipe_stream_default();
    let spawned = std::thread::Builder::new()
        .name("kaish-stdin-bridge".to_string())
        .spawn(move || {
            use tokio::io::AsyncWriteExt;
            let mut buf = [0u8; 64 * 1024];
            let mut lock = stdin.lock();
            loop {
                match lock.read(&mut buf) {
                    Ok(0) => break, // EOF: dropping `writer` signals EOF to the reader.
                    Ok(n) => {
                        // `write_all`, NOT `write_bytes`: the latter is a single
                        // `poll_write` that writes at most the pipe's free space
                        // (≤64 KiB) and returns a short count, so ignoring it
                        // silently truncates any input larger than one buffer.
                        // `write_all` loops until every byte lands (or the reader
                        // is dropped → BrokenPipe, meaning the command never read
                        // stdin, so we stop — nothing more to deliver).
                        if handle.block_on(writer.write_all(&buf[..n])).is_err() {
                            return;
                        }
                    }
                    Err(e) if e.kind() == std::io::ErrorKind::Interrupted => continue,
                    Err(_) => return, // read error: stop; reader sees EOF on drop.
                }
            }
        });
    match spawned {
        Ok(_) => Some(reader),
        Err(e) => {
            // Couldn't spawn the bridge: drop the pipe and run without stdin
            // rather than hang. Loud, not silent.
            eprintln!("kaish: could not spawn stdin reader: {e}; running without stdin");
            None
        }
    }
}

/// Execute `source` non-interactively, printing each statement's output as it
/// completes, with a lazily-bridged process stdin (see [`spawn_stdin_bridge`]).
fn execute_noninteractive(
    rt: &tokio::runtime::Runtime,
    client: &kaish_client::EmbeddedClient,
    source: &str,
    opts: kaish_kernel::ExecuteOptions,
) -> Result<kaish_kernel::interpreter::ExecResult> {
    let mut on_output = |r: &kaish_kernel::interpreter::ExecResult| {
        // A binary (`Bytes`) result must reach stdout byte-for-byte — printing
        // `text_out()` would lossy-decode it to U+FFFD and corrupt the stream
        // (e.g. `printf '\xff' | kaish -c cat`). Text results print as-is.
        if let Some(bytes) = r.out_bytes() {
            use std::io::Write;
            let _ = std::io::stdout().write_all(bytes);
        } else {
            let text = r.text_out();
            if !text.is_empty() {
                print!("{}", text);
            }
        }
        if !r.err.is_empty() {
            eprint!("{}", r.err);
        }
    };
    let result = match spawn_stdin_bridge(rt.handle().clone()) {
        Some(reader) => rt.block_on(client.execute_with_pipe_stdin_streaming(
            source,
            opts,
            reader,
            &mut on_output,
        )),
        None => rt.block_on(client.execute_with_options_streaming(source, opts, &mut on_output)),
    };
    result.context("execution failed")
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
    let opts = kaish_repl::trace_options_from_env();
    let result = execute_noninteractive(&rt, &client, &source, opts)?;

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
    let opts = kaish_repl::trace_options_from_env();
    let result = execute_noninteractive(&rt, &client, cmd, opts)?;

    if result.ok() {
        Ok(ExitCode::SUCCESS)
    } else {
        Ok(ExitCode::from(result.code as u8))
    }
}
