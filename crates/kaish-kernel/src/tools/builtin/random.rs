//! random — print one integer from `--min` to `--max`, inclusive.
//! kaish has no `$RANDOM`; `$(random --max 100)` is the typed replacement.
//!
//! ```kaish
//! random --max 6   # roll a die: 0..=6
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::ast::Value;
use crate::interpreter::{value_to_string, ExecResult};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// Random tool: print one random integer, uniformly, from a range.
pub struct Random;

/// `$RANDOM`-compatible default: bash's range is 0..=32767.
const DEFAULT_MIN: i64 = 0;
const DEFAULT_MAX: i64 = 32767;

/// clap-derived argv layer for random.
#[derive(Parser, Debug)]
#[command(
    name = "random",
    about = "Print one random integer from --min to --max, inclusive. `$(random --max 100)` replaces bash's `$RANDOM`."
)]
struct RandomArgs {
    /// Lowest value that can be returned, inclusive. Default 0.
    #[arg(long = "min")]
    min: Option<i64>,

    /// Highest value that can be returned, inclusive. Default 32767.
    #[arg(long = "max")]
    max: Option<i64>,

    #[command(flatten)]
    global: GlobalFlags,
}

#[async_trait]
impl Tool for Random {
    fn name(&self) -> &str {
        "random"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &RandomArgs::command(),
            "random",
            "Print one random integer from --min to --max, inclusive. `$(random --max 100)` replaces bash's `$RANDOM`.",
            [
                ("Default range (like $RANDOM)", "random"),
                ("Roll a die", "random --max 6"),
                ("Capture and use", "x=$(random --max 6); echo $((x + 1))"),
            ],
        )
        .with_typed_substitution()
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };

        // No positional args; curate the error before clap ever sees one.
        if let Some(v) = args.positional.first() {
            let value = value_to_string(v);
            return ExecResult::failure(
                2,
                format!("random: takes no positional argument; write `--max {value}`"),
            );
        }

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("random: {e}")),
        };
        let parsed = match RandomArgs::try_parse_from(
            std::iter::once("random".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("random: {e}")),
        };
        parsed.global.apply(ctx);

        let min = parsed.min.unwrap_or(DEFAULT_MIN);
        let max = parsed.max.unwrap_or(DEFAULT_MAX);

        if min > max {
            return ExecResult::failure(
                2,
                format!(
                    "random: --min {min} is greater than --max {max}; swap them or widen the range"
                ),
            );
        }

        let value = match draw_random(min, max) {
            Ok(v) => v,
            Err(e) => {
                return ExecResult::failure(
                    1,
                    format!("random: could not obtain system entropy: {e}"),
                );
            }
        };

        ExecResult::success_with_data(format!("{value}\n"), Value::Int(value))
    }
}

/// Draws one integer in `min..=max` from the OS CSPRNG. No fallback: a
/// `getrandom` failure is a hard error, never a silently guessable value.
fn draw_random(min: i64, max: i64) -> Result<i64, getrandom::Error> {
    loop {
        let mut entropy = [0u8; 8];
        getrandom::fill(&mut entropy)?;
        let draw = u64::from_le_bytes(entropy);
        if let Some(value) = map_draw_to_range(draw, min, max) {
            return Ok(value);
        }
    }
}

/// Maps one 64-bit draw onto `min..=max`, inclusive, via Lemire's method:
/// widen the multiply to 128 bits and reject the low slice that would bias
/// a bucket. `None` means redraw.
fn map_draw_to_range(draw: u64, min: i64, max: i64) -> Option<i64> {
    debug_assert!(min <= max);

    // u128: width can be exactly 2^64, which doesn't fit u64.
    let width: u128 = (max as i128 - min as i128) as u128 + 1;

    if width > u64::MAX as u128 {
        // Full i64 span: every draw already maps 1:1, no bias possible.
        return Some((min as i128 + draw as i128) as i64);
    }
    let width = width as u64;
    if width == 1 {
        return Some(min);
    }

    let product = (draw as u128) * (width as u128);
    let hi = (product >> 64) as u64;
    let lo = product as u64;

    // Below threshold: redraw to avoid skewing the low bucket.
    let threshold = width.wrapping_neg() % width;
    if lo < threshold {
        return None;
    }

    Some((min as i128 + hi as i128) as i64)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[test]
    fn map_draw_is_deterministic() {
        let a = map_draw_to_range(0x1234_5678_9abc_def0, -100, 100);
        let b = map_draw_to_range(0x1234_5678_9abc_def0, -100, 100);
        assert_eq!(a, b);
    }

    #[test]
    fn map_draw_min_equals_max_ignores_the_draw() {
        assert_eq!(map_draw_to_range(0, 7, 7), Some(7));
        assert_eq!(map_draw_to_range(u64::MAX, 7, 7), Some(7));
    }

    #[test]
    fn map_draw_stays_in_bounds() {
        for draw in [0u64, 1, u64::MAX / 2, u64::MAX - 1, u64::MAX] {
            for (min, max) in [(0i64, 6i64), (-5, 5), (i64::MIN, -1), (0, i64::MAX)] {
                if let Some(v) = map_draw_to_range(draw, min, max) {
                    assert!(v >= min && v <= max, "{v} outside {min}..={max}");
                }
            }
        }
    }

    #[test]
    fn map_draw_full_i64_span_never_panics_and_never_rejects() {
        for draw in [0u64, 1, u64::MAX / 2, u64::MAX - 1, u64::MAX] {
            let v = map_draw_to_range(draw, i64::MIN, i64::MAX);
            assert!(v.is_some(), "the full span must never reject a draw");
        }
    }

    #[test]
    fn map_draw_extreme_spans_do_not_panic() {
        let spans = [
            (i64::MIN, i64::MIN),
            (i64::MAX, i64::MAX),
            (i64::MIN, i64::MAX),
            (i64::MIN, i64::MIN + 1),
            (i64::MAX - 1, i64::MAX),
        ];
        for (min, max) in spans {
            for draw in [0u64, u64::MAX] {
                let _ = map_draw_to_range(draw, min, max);
            }
        }
    }

    #[tokio::test]
    async fn execute_defaults_are_random_range() {
        let mut ctx = make_ctx();
        let result = Random.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        let n: i64 = result.text_out().trim().parse().expect("integer output");
        assert!((DEFAULT_MIN..=DEFAULT_MAX).contains(&n));
    }

    #[tokio::test]
    async fn execute_rejects_min_greater_than_max() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named.insert("min".to_string(), Value::Int(10));
        args.named.insert("max".to_string(), Value::Int(5));

        let result = Random.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 2);
        assert!(result.err.contains("--min 10 is greater than --max 5"));
    }

    #[tokio::test]
    async fn execute_rejects_positional_argument() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::Int(100));

        let result = Random.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert_eq!(result.code, 2);
        assert!(result.err.contains("--max 100"));
    }
}
