//! `test` — POSIX condition evaluation, following kaish's `[[` semantics.
//!
//! `test EXPR` exits 0 if EXPR is true, 1 if false, and 2 on a usage or type
//! error. Unlike POSIX `test` it is a *command* over kaish's own value model:
//!
//! - **VFS-aware** file tests (`-e -f -d -r -w -x`) stat through the kernel
//!   backend, not the host filesystem.
//! - **Numeric** comparison (`-eq -ne -gt -lt -ge -le`) is kaish's number
//!   semantics — floats compare, identical to `[[`, not POSIX integer-only.
//!   Non-numeric operands are a loud error, never silently zero.
//! - **String equality** (`=` `==` `!=`) is literal (not glob), reusing `[[`'s
//!   `values_equal`; a collection operand is a loud Shape error.
//! - **No `-a`/`-o`/`( )`** — those XSI footguns are rejected loudly; chain with
//!   shell `&&`/`||` or use `[[ ... ]]`. Negation is a single leading `!`.
//! - **No POSIX arg-count magic**: an operator that is missing its operand
//!   (`test -f`, `test -z`) is a loud error, not a surprise-true.
//!
//! It reads its argv in *source order with types preserved* via the schema's
//! `raw_argv` opt-in, so an operand that looks like a flag (`test $x = -n`,
//! `test 0 -gt -5`) is seen as a literal operand rather than a hoisted flag.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use kaish_types::Value;

use crate::interpreter::{
    is_collection, numeric_compare, scalar_test_operand_error, value_to_string,
    value_to_text_sink_named, values_equal, ExecResult,
};
use kaish_tool_api::{IssueCode, ValidationIssue};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

pub struct Test;

/// clap-derived argv layer for `test`. The POSIX expression grammar is
/// hand-rolled over the source-ordered `args.positional` (see the module docs
/// on `raw_argv`); clap only owns the outer layer + `--json` (a no-op here,
/// `test` has no output). `rest` is a hidden passthrough sink.
#[derive(Parser, Debug)]
#[command(name = "test", about = "Evaluate a conditional expression (exit 0 true / 1 false / 2 error)")]
struct TestArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// The expression to evaluate, as in `test -f file.txt`.
    // Hidden sink: the expression is read from `args.positional`.
    #[arg(trailing_var_arg = true, allow_hyphen_values = true, hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for Test {
    fn name(&self) -> &str {
        "test"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TestArgs::command(),
            "test",
            "Evaluate a conditional expression: exit 0 if true, 1 if false, 2 on error",
            [
                ("File exists and is regular", "test -f config.toml"),
                ("String equality", r#"test "$mode" = release"#),
                ("Numeric comparison", "test $count -gt 0"),
                ("Negation", "test ! -d build"),
                ("Compound via shell", "test -f a && test -f b"),
            ],
        )
        .with_raw_argv()
    }

    /// Reject `-a`/`-o`/`(`/`)` before anything runs.
    ///
    /// `execute` refuses them too, but only a validator error stops the
    /// statement — and a runtime refusal inside an `if` condition is worth
    /// very little, since the branch is chosen from the exit code.
    ///
    /// Reads `positional` in source order, because `test` is `raw_argv` and
    /// the validation binder now mirrors that (see
    /// `build_tool_args_for_validation`). Order is the whole point: an
    /// operator word is only an operator in an operator SLOT. `test "-a" =
    /// "-a"` compares two strings and `test -f "-a"` stats a file named
    /// `-a` — both legal, both refused by the first version of this rule,
    /// which scanned every word because a decomposed `ToolArgs` had no order
    /// left to read.
    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        // EVERY operand, rendered the way `eval_test` renders one when it asks
        // whether a word is an operator. Dropping the non-strings — the first
        // version filtered for `Value::String` — shifted the slots by one for
        // each, so `test "-a" = 1` looked like the two-operand `-a =` and was
        // refused, while `test -a 1` looked like a lone word and was not.
        // Runtime keeps operands typed and compares `value_to_string`, so this
        // is what it sees.
        let words: Vec<String> = args
            .positional
            .iter()
            .map(crate::interpreter::value_to_string)
            .collect();
        let words: Vec<&str> = words.iter().map(String::as_str).collect();

        // A placeholder means an unevaluated expansion; its runtime value is
        // unknown, so judging it would report a program that may be fine.
        if words.contains(&"<dynamic>") {
            return Vec::new();
        }

        // Same slots `eval_test`/`eval_primary` read: skip the leading `!`
        // run, then the operator is the first word of a two-operand primary
        // and the middle word of a three-operand one. Anything longer is
        // already an error, and `-a`/`-o` is why it usually happens.
        let mut rest = words.as_slice();
        while rest.len() >= 2 && rest[0] == "!" {
            rest = &rest[1..];
        }
        let found = match rest.len() {
            // A single operand has no operator slot for a compound to sit in:
            // `test "-a"` is one string. Runtime answers it accurately with
            // "'-a' needs an operand", and since a condition's output reaches
            // the author that report is worth more than E020's, which would
            // name a compound the author never wrote.
            2 => Some(rest[0]).filter(|w| is_compound_op(w)),
            3 => Some(rest[1]).filter(|w| is_compound_op(w)),
            n if n > 3 => rest.iter().copied().find(|w| is_compound_op(w)),
            _ => None,
        };

        match found {
            Some(op) => vec![
                ValidationIssue::error(
                    IssueCode::TestCompoundOperator,
                    format!("test: '{op}' is not supported — {COMPOUND_HINT}"),
                )
                .with_suggestion("test EXPR1 && test EXPR2, or [[ EXPR1 && EXPR2 ]]"),
            ],
            None => Vec::new(),
        }
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("test: {e}")),
        };
        let parsed = match TestArgs::try_parse_from(
            std::iter::once("test".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("test: {e}")),
        };
        parsed.global.apply(ctx);

        // Read the expression from the source-ordered, typed argv.
        match eval_test(ctx, &args.positional).await {
            Ok(true) => ExecResult::success(""),
            Ok(false) => ExecResult::failure(1, ""),
            Err(msg) => ExecResult::failure(2, msg),
        }
    }
}

fn is_unary_op(s: &str) -> bool {
    matches!(s, "-z" | "-n" | "-e" | "-f" | "-d" | "-r" | "-w" | "-x")
}

fn is_binary_op(s: &str) -> bool {
    matches!(s, "=" | "==" | "!=" | "-eq" | "-ne" | "-gt" | "-lt" | "-ge" | "-le")
}

/// The XSI compound / grouping operators.
///
/// The XSI compound / grouping operators kaish deliberately does not implement.
///
/// bash gives `-a`/`-o` two meanings apiece — the binary AND/OR, and a unary
/// `-a FILE` (a synonym for `-e`) / `-o NAME` (a shell option query) — and
/// that overload is what makes the binary form ambiguous to parse. coreutils'
/// own man page warns about it and points at `&&`/`||` instead. On top of the
/// ambiguity, three of bash's operand-count rules outrank `!` in ways that
/// surprise a careful reader: `test ! = x` compares two strings, `test ! -a ""`
/// is an AND, and `test ! x -o x` negates the whole expression rather than
/// `x`. kaish declines all of it and says so, at every arity.
fn is_compound_op(s: &str) -> bool {
    matches!(s, "-a" | "-o" | "(" | ")")
}

const COMPOUND_HINT: &str =
    "kaish `test` has no -a/-o/() compound — chain with shell `&&`/`||` or use `[[ ... ]]`";

fn is_any_op(s: &str) -> bool {
    is_unary_op(s) || is_binary_op(s) || is_compound_op(s) || s == "!"
}

/// Evaluate a `test` expression. A single (or parity-collapsed) leading `!`
/// negates; the rest is a primary. Returns Err (→ exit 2) on any usage/type
/// error, so a malformed expression is loud, never a surprise true/false.
async fn eval_test(ctx: &ExecContext, operands: &[Value]) -> Result<bool, String> {
    // Strip leading `!` operators, but only while an expression remains after
    // them — a bare trailing `!` is a missing-operand error, handled by the
    // primary. `! !` collapses by parity (double negation is identity).
    let mut negate = false;
    let mut ops = operands;
    while ops.len() >= 2 && value_to_string(&ops[0]) == "!" {
        negate = !negate;
        ops = &ops[1..];
    }
    let result = eval_primary(ctx, ops).await?;
    Ok(negate ^ result)
}

async fn eval_primary(ctx: &ExecContext, operands: &[Value]) -> Result<bool, String> {
    match operands.len() {
        // No operands is false, as in bash. Nothing can be hidden by it:
        // there is no expression to have gotten wrong.
        0 => Ok(false),
        1 => {
            let operand = &operands[0];
            // A bare collection has no truth value here — loud Shape error.
            if is_collection(operand) {
                return Err(format!(
                    "test: operand is a {}, not a string; a collection has no truth value",
                    collection_kind(operand)
                ));
            }
            // Loud on binary: `test $BIN` must not silently treat the
            // `[binary: N bytes]` placeholder as a truthy string (found via
            // kaibo review of GH #116 — this raw-argv positional arm is the
            // one sibling of the Named/WordAssign arms in kernel.rs's raw-argv
            // fast path that isn't guarded there, since `test` itself needs
            // the untouched typed value for its other operators; the guard
            // belongs here instead, mirroring `-z`/`-n`/the path operators
            // below).
            let s = value_to_text_sink_named(operand, "a test operand")
                .map_err(|e| format!("test: {e}"))?;
            // A lone operator is a forgotten operand — loud, not surprise-true.
            if is_any_op(&s) {
                return Err(format!("test: '{s}' needs an operand"));
            }
            Ok(!s.is_empty())
        }
        2 => {
            let op = value_to_string(&operands[0]);
            if is_compound_op(&op) {
                return Err(format!("test: '{op}' is not supported — {COMPOUND_HINT}"));
            }
            if is_unary_op(&op) {
                return apply_unary(ctx, &op, &operands[1]).await;
            }
            Err(format!(
                "test: expected a unary operator (-f, -z, …) before the operand, found '{op}'"
            ))
        }
        3 => {
            let op = value_to_string(&operands[1]);
            if is_compound_op(&op) {
                return Err(format!("test: '{op}' is not supported — {COMPOUND_HINT}"));
            }
            if is_binary_op(&op) {
                return apply_binary(&operands[0], &op, &operands[2]);
            }
            Err(format!(
                "test: expected a binary operator (=, !=, -eq, …) between the operands, found '{op}'"
            ))
        }
        _ => Err(format!("test: too many arguments — {COMPOUND_HINT}")),
    }
}

async fn apply_unary(ctx: &ExecContext, op: &str, operand: &Value) -> Result<bool, String> {
    // A collection operand to any unary test is a loud Shape error (Decision E).
    if let Some(msg) = scalar_test_operand_error(op, operand) {
        return Err(msg);
    }
    match op {
        // Loud on binary (found via kaibo review of GH #116): `test -z $BIN`/
        // `test -n $BIN` must not silently treat the `[binary: N bytes]`
        // placeholder as a non-empty string — same class as the path operators
        // below, just for the empty/non-empty-string test instead of a stat.
        "-z" => Ok(value_to_text_sink_named(operand, "a test operand")
            .map_err(|e| format!("test: {e}"))?
            .is_empty()),
        "-n" => Ok(!value_to_text_sink_named(operand, "a test operand")
            .map_err(|e| format!("test: {e}"))?
            .is_empty()),
        "-e" | "-f" | "-d" | "-r" | "-w" | "-x" => {
            // A binary operand goes loud rather than silently stat'ing a file
            // literally named `[binary: N bytes]` — mirrors `[[`'s `FileTest`
            // arm (kernel.rs::eval_test_async) so the two evaluators agree.
            let path = value_to_text_sink_named(operand, "a path").map_err(|e| format!("test: {e}"))?;
            Ok(file_test(ctx, op, &path).await)
        }
        _ => unreachable!("apply_unary called with non-unary op {op:?}"),
    }
}

fn apply_binary(left: &Value, op: &str, right: &Value) -> Result<bool, String> {
    match op {
        // Literal string equality — reuses `[[`'s `values_equal`, which is loud
        // on a collection-vs-scalar operand.
        "=" | "==" => values_equal(left, right).map_err(|e| format!("test: {e}")),
        "!=" => values_equal(left, right)
            .map(|eq| !eq)
            .map_err(|e| format!("test: {e}")),
        "-eq" | "-ne" | "-gt" | "-lt" | "-ge" | "-le" => {
            if let Some(msg) = scalar_test_operand_error(op, left) {
                return Err(msg);
            }
            if let Some(msg) = scalar_test_operand_error(op, right) {
                return Err(msg);
            }
            let ord = numeric_compare(left, right).map_err(|e| format!("test: {e}"))?;
            Ok(match op {
                "-eq" => ord.is_eq(),
                "-ne" => !ord.is_eq(),
                "-gt" => ord.is_gt(),
                "-lt" => ord.is_lt(),
                "-ge" => ord.is_ge(),
                "-le" => ord.is_le(),
                _ => unreachable!(),
            })
        }
        _ => unreachable!("apply_binary called with non-binary op {op:?}"),
    }
}

/// Stat `path` through the VFS backend and answer the file predicate — mirrors
/// `[[`'s `FileTest` arm so the two stay consistent.
async fn file_test(ctx: &ExecContext, op: &str, path: &str) -> bool {
    // The empty path names no file. Resolving it lands on the working
    // directory, so `test -e ""` answered true — bash says false, and so does
    // every reading of "does this file exist". `eval_test_async`'s `FileTest`
    // arm carries the same guard; a fix that lands in only one of them makes
    // the mirror above a lie, which is what happened the first time.
    if path.is_empty() {
        return false;
    }
    let resolved = ctx.resolve_path(path);
    // `-r`/`-w`/`-x` go through `path_access`, never through the raw mode
    // bits: the mount's read-only state is half the answer and `stat` does
    // not carry it. `eval_test_async` reads the same query, and
    // `file_test_writable_tests` runs every case through both.
    match op {
        "-e" => ctx.backend.stat(&resolved).await.is_ok(),
        "-f" => ctx.backend.stat(&resolved).await.is_ok_and(|e| e.is_file()),
        "-d" => ctx.backend.stat(&resolved).await.is_ok_and(|e| e.is_dir()),
        "-r" => ctx
            .backend
            .path_access(&resolved)
            .await
            .is_ok_and(|access| access.readable),
        "-w" => ctx
            .backend
            .path_access(&resolved)
            .await
            .is_ok_and(|access| access.writable),
        "-x" => ctx
            .backend
            .path_access(&resolved)
            .await
            .is_ok_and(|access| access.executable),
        _ => unreachable!("file_test called with non-file op {op:?}"),
    }
}

fn collection_kind(value: &Value) -> &'static str {
    match value {
        Value::Json(serde_json::Value::Array(_)) => "list",
        Value::Json(serde_json::Value::Object(_)) => "record",
        _ => "collection",
    }
}
