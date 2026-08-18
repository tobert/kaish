//! kaish CLI entry point.
//!
//! Usage:
//!   kaish                      # Interactive REPL
//!   kaish -c `<command>`       # Execute command and exit
//!   kaish --plan `<command>`   # Print what it would run, as JSON
//!   kaish --plan-file `<path>` # Same, reading the source from a file
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
    // Initialize tracing (respects RUST_LOG env var) on the process's original
    // thread, before we hand off to the sized driver thread below.
    tracing_subscriber::registry()
        .with(fmt::layer())
        .with(EnvFilter::from_default_env())
        .init();

    // Drive the whole REPL on a thread sized to `RECOMMENDED_STACK_SIZE`. The
    // interpreter recurses on the native stack (command substitution, shell
    // functions, `.kai` scripts) and its foreground work runs on the
    // `block_on` thread — the OS-default main-thread stack (~8 MB) overflows a
    // deep recursion in debug *before* the depth guard's cap trips, defeating
    // it. Worker threads are sized via `build_runtime`; this covers the driver
    // (GH #46/#47).
    let spawned = std::thread::Builder::new()
        .name("kaish".to_string())
        .stack_size(kaish_kernel::RECOMMENDED_STACK_SIZE)
        .spawn(run);
    let joined = match spawned {
        Ok(handle) => handle.join(),
        Err(e) => {
            eprintln!("Error: could not spawn kaish driver thread: {e}");
            return ExitCode::FAILURE;
        }
    };
    match joined {
        Ok(Ok(code)) => code,
        Ok(Err(e)) => {
            eprintln!("Error: {e:?}");
            ExitCode::FAILURE
        }
        Err(_) => {
            eprintln!("Error: kaish driver thread panicked");
            ExitCode::FAILURE
        }
    }
}

fn run() -> Result<ExitCode> {
    let args: Vec<String> = env::args().collect();

    // Skip argv[0]: every arm below matches on the first real argument.
    // Extract --overlay (can appear anywhere before positionals).
    let overlay = args.iter().skip(1).any(|a| a == "--overlay");
    let rest: Vec<&str> = args
        .iter()
        .skip(1)
        .filter(|a| *a != "--overlay")
        .map(|a| a.as_str())
        .collect();

    // Parse arguments
    match rest.first().copied() {
        None => {
            // No args: interactive REPL
            let config = kaish_repl::interactive_config().with_overlay(overlay);
            kaish_repl::run_interactive(config, overlay)?;
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

        Some("--plan") => {
            // A missing source is reported the same way a broken one is. The
            // contract is that stdout is always a JSON object, and "except
            // when you called it wrong" is exactly the case a caller would
            // not have written a branch for.
            Ok(print_plan(rest.get(1).copied().map(str::to_string)))
        }

        Some("--plan-file") => {
            // Reading the source from a file keeps it out of argv, which
            // matters when the thing being analyzed is a whole script rather
            // than a line: argv is capped, and a caller measuring real traffic
            // should not have to shell-quote it to ask a question about it.
            Ok(match rest.get(1).copied() {
                Some(path) => match read_plan_source(path) {
                    Ok(source) => print_plan(Some(source)),
                    Err(e) => print_plan_error(&format!("cannot read {path}: {e}")),
                },
                None => print_plan_error(
                    "--plan-file requires a path: kaish --plan-file <path>, or - for stdin",
                ),
            })
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

/// Print `source`'s statement plans as JSON and exit — command analysis for
/// a consumer that is not written in Rust.
///
/// Nothing executes and no kernel is built: `plan_program` is a pure function
/// of the source text, so this touches no filesystem and needs no capability.
/// `--overlay` is therefore irrelevant here and is ignored.
///
/// The output is always a JSON object, so a caller parses one shape whatever
/// happened: `{"statements": [...]}` and exit 0, or `{"errors": [...]}` and
/// exit 2 — the same usage-error code a builtin returns for bad argv.
///
/// "Always" includes being called with no source at all: a caller that got
/// prose on stderr and an empty stdout for that one case would need a branch
/// it had no reason to write. An error carries `start`/`end` only when it
/// refers to a position in the source.
fn print_plan(source: Option<String>) -> ExitCode {
    let Some(source) = source else {
        return print_plan_error(
            "--plan requires a command argument: kaish --plan '<command>'",
        );
    };
    match kaish_kernel::plan_program(&source) {
        Ok(statements) => {
            let doc = serde_json::json!({ "statements": statements });
            println!("{doc}");
            ExitCode::SUCCESS
        }
        Err(errors) => {
            let errors: Vec<_> = errors
                .iter()
                .map(|e| {
                    serde_json::json!({
                        "message": e.message,
                        "start": e.span.start,
                        "end": e.span.end,
                    })
                })
                .collect();
            let doc = serde_json::json!({ "errors": errors });
            println!("{doc}");
            // 2 is the usage/parse code, matching a builtin's argv rejection.
            ExitCode::from(2)
        }
    }
}

/// Report a plan failure that has no position in a source — a missing
/// argument, or a file that could not be read. Same shape and same exit code
/// as a parse failure, because a caller branches on the shape, not on which
/// of our internal paths produced it.
fn print_plan_error(message: &str) -> ExitCode {
    let doc = serde_json::json!({ "errors": [{ "message": message }] });
    println!("{doc}");
    ExitCode::from(2)
}

/// Read plan source from `path`, or from stdin when `path` is `-`.
fn read_plan_source(path: &str) -> std::io::Result<String> {
    if path == "-" {
        let mut source = String::new();
        std::io::stdin().read_to_string(&mut source)?;
        return Ok(source);
    }
    std::fs::read_to_string(path)
}

fn print_help() {
    println!(r#"会sh — kaish v{}

Usage:
  kaish                        Interactive REPL
  kaish -c <command>           Execute command and exit
  kaish --plan <command>       Print what it would run, as JSON, and exit
  kaish --plan-file <path>     Same, reading the source from a file or -
  kaish <script.kai>           Run a script file

Options:
  --overlay                    Enable copy-on-write overlay mode (writes are
                               virtual; use kaish-vfs commit to apply them)
  -c <command>                 Execute command string and exit
  --plan <command>             Analyze without running: every command, its
                               redirects, the variables it reads and writes,
                               and each heredoc body with its byte offset.
                               Executes nothing and touches no filesystem.
                               Prints {{"statements": [...]}} and exits 0, or
                               {{"errors": [...]}} and exits 2.
  --plan-file <path>           Plan the source in <path>, or stdin for -. Keeps
                               a whole script out of argv, which is capped.
  -h, --help                   Show this help
  -V, --version                Show version

Examples:
  kaish                        # Start interactive REPL
  kaish --overlay              # REPL with virtual writes (overlay mode)
  kaish -c 'echo hello'       # Run a command
  kaish --overlay -c 'echo test > file.txt; kaish-vfs diff'
  kaish deploy.kai             # Run a deployment script
  kaish --plan 'rm -r build'   # See what it would run, without running it
  kaish --plan-file deploy.kai # Plan a script without running it
"#, env!("CARGO_PKG_VERSION"));
}

/// Run a script file.
fn run_script(path: &str, overlay: bool) -> Result<ExitCode> {
    use kaish_client::EmbeddedClient;
    use kaish_kernel::Kernel;

    // Read the script
    let source = std::fs::read_to_string(path)
        .with_context(|| format!("Failed to read script: {path}"))?;

    // Blank out the shebang line (rather than removing it) so every
    // subsequent line keeps its original 1-based line number for error
    // reporting (GH #127).
    let source = if source.starts_with("#!") {
        match source.find('\n') {
            Some(idx) => format!("\n{}", &source[idx + 1..]),
            None => String::new(), // whole file is a single shebang line
        }
    } else {
        source
    };

    // A parse/lexer failure means nothing in the script would have run —
    // print its diagnostic directly and stop, before a kernel even exists,
    // rather than let `execute_noninteractive`'s `Err` reach `main` as an
    // execution-error wrapper.
    if let Some(diagnostic) = kaish_repl::format_parse_error(&source) {
        eprintln!("{diagnostic}");
        return Ok(ExitCode::FAILURE);
    }

    // Non-interactive: pipe stdout so command substitution captures output.
    // The streaming callback below still prints output for the user.
    let config = kaish_repl::noninteractive_config(overlay);
    let kernel = Kernel::new(config)
        .context("Failed to create kernel")?;

    let client = EmbeddedClient::new(kernel);

    let rt = kaish_repl::build_runtime()?;
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
    use kaish_kernel::Kernel;

    // A parse/lexer failure means nothing would have run — print its
    // diagnostic directly and stop, before a kernel even exists, rather
    // than let `execute_noninteractive`'s `Err` reach `main` as an
    // execution-error wrapper.
    if let Some(diagnostic) = kaish_repl::format_parse_error(cmd) {
        eprintln!("{diagnostic}");
        return Ok(ExitCode::FAILURE);
    }

    // Non-interactive: pipe stdout so command substitution captures output.
    // The streaming callback below still prints output for the user.
    let config = kaish_repl::noninteractive_config(overlay);
    let kernel = Kernel::new(config)
        .context("Failed to create kernel")?;

    let client = EmbeddedClient::new(kernel);

    let rt = kaish_repl::build_runtime()?;
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
