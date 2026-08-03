//! Parse throughput across script sizes (GH #255).
//!
//! The allocation harness next door (`alloc_profile`) measures how *much* the
//! parser allocates. It does not measure how long a parse takes, and that gap
//! nearly shipped a regression: the first fix for #255 cached the combinator
//! graph by switching to an owned token input, which cut allocations by 56% on
//! a one-command script while making every script over ~80 tokens 30–50%
//! slower to parse. Allocation count alone said "ship it".
//!
//! So this is the other axis. It is a committed harness rather than a one-off
//! script for the same reason `alloc_profile` is: the next person changing the
//! parser needs to be able to re-run it and compare.
//!
//! # Running it
//!
//! ```text
//! cargo run -p kaish-kernel --release --example parse_bench
//! ```
//!
//! Release only — a debug build measures the optimizer, not the parser. To
//! compare two implementations, run it on each branch and diff the columns.
//!
//! # Reading the output
//!
//! Construction cost is a constant paid once per parse, so it dominates the
//! smallest rows and vanishes in the largest. Per-token cost is flat if the
//! grammar is linear in its input. A change that helps the top rows and hurts
//! the bottom ones has traded throughput for a fixed cost — look at both.

// Harness code: a panic on a known-good fixture IS the harness failing.
// Examples are not `#[test]` context, so the workspace restriction lints need
// an explicit allow (see CLAUDE.md).
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::fmt::Write as _;
use std::time::{Duration, Instant};

/// Build a script of `statements` blocks.
///
/// The block mixes the constructs that make the grammar work hardest — a test
/// expression, command substitution, a pipeline, a loop, and an interpolated
/// string — so the measurement is not dominated by one cheap path.
fn build_script(statements: usize) -> String {
    let mut source = String::new();
    for i in 0..statements {
        let _ = write!(
            source,
            "if [[ -f f{i}.txt ]]; then\n  \
               x=$(grep -n needle f{i}.txt | wc -l)\n  \
               for j in $(seq 1 5); do echo \"$j:${{x}}\"; done\n\
             fi\n"
        );
    }
    source
}

/// Time `parse` over `source`, returning the per-parse duration.
fn time_parse(source: &str, iterations: usize) -> Duration {
    kaish_kernel::parser::parse(source).expect("fixture must parse");

    let start = Instant::now();
    for _ in 0..iterations {
        let program = kaish_kernel::parser::parse(source).expect("fixture must parse");
        // Keep the AST alive to the end of the iteration so the optimizer
        // cannot treat the parse as dead code.
        std::hint::black_box(&program);
    }
    start.elapsed() / iterations as u32
}

fn main() {
    println!(
        "{:>8}  {:>8}  {:>14}  {:>12}",
        "stmts", "tokens", "per parse", "per token"
    );

    for statements in [1usize, 2, 5, 10, 20, 50, 100, 200] {
        let source = build_script(statements);
        let tokens = kaish_kernel::lexer::tokenize(&source)
            .expect("fixture must tokenize")
            .len();

        // Hold total wall time roughly flat as the scripts grow.
        let iterations = (20_000 / statements).max(20);
        let elapsed = time_parse(&source, iterations);
        let per_token = elapsed.as_nanos() as f64 / tokens as f64;

        println!("{statements:>8}  {tokens:>8}  {elapsed:>14?}  {per_token:>9.1} ns");
    }
}
