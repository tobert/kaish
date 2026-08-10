//! kaish-wasi — a WASI binary for sandboxed shell execution.
//!
//! Reads lines from stdin, executes each as a kaish script, and writes
//! JSON `ExecResult` to stdout. Runs with `NoLocal` VFS and no external
//! commands — a pure-function shell for text transforms.
//!
//! Uses synchronous stdin/stdout because tokio's `io-std` feature is not
//! available on `wasm32-wasip1`. This is fine for a line-oriented REPL
//! where each line is fully processed before reading the next.
//!
//! # Usage
//!
//! ```sh
//! echo 'echo hello' | wasmtime kaish-wasi.wasm
//! echo 'seq 1 10 | sort -rn | head -3' | wasmtime kaish-wasi.wasm
//! ```

// A one-shot WASI binary has nowhere to propagate a startup failure to: the
// runtime and the kernel are built before there is a stdout protocol to report
// on, so a panic is the loud outcome and the only one available.
#![allow(clippy::expect_used)]

use std::io::{self, BufRead, Write};

use kaish_kernel::{Kernel, KernelConfig};

fn main() {
    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()
        .expect("failed to create tokio runtime");

    rt.block_on(async {
        run().await;
    });
}

async fn run() {
    let config = KernelConfig::isolated();
    let kernel = Kernel::new(config).expect("failed to create kernel");
    let kernel = kernel.into_arc();

    let stdin = io::stdin();
    let stdout = io::stdout();
    let mut stdout = stdout.lock();

    for line in stdin.lock().lines() {
        let line = match line {
            Ok(l) => l,
            Err(_) => break,
        };

        let trimmed = line.trim();
        if trimmed.is_empty() {
            continue;
        }

        let result = match kernel.execute(trimmed).await {
            Ok(r) => r,
            Err(e) => {
                let json = serde_json::json!({
                    "code": 1,
                    "out": "",
                    "err": e.to_string(),
                });
                // Errors ignored: stdout is the only channel this binary has,
                // so a failure to write the error has nowhere left to report.
                let _ = serde_json::to_writer(&mut stdout, &json);
                let _ = stdout.write_all(b"\n");
                let _ = stdout.flush();
                continue;
            }
        };

        let json = serde_json::json!({
            "code": result.code,
            "out": result.text_out().as_ref(),
            "err": if result.err.is_empty() { None } else { Some(&result.err) },
        });

        // Errors ignored for the same reason as above.
        let _ = serde_json::to_writer(&mut stdout, &json);
        let _ = stdout.write_all(b"\n");
        let _ = stdout.flush();
    }
}
