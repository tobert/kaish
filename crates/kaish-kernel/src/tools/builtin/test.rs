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
                ("Compound", "test -f a -a -f b"),
            ],
        )
        .with_raw_argv()
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
/// `-a`/`-o` mean AND and OR here and nothing else. bash also gives them
/// unary meanings in the two-operand form — `-a FILE` is a deprecated synonym
/// for `-e`, `-o NAME` asks whether a shell option is on — and that second
/// meaning is precisely what makes them ambiguous to parse, which coreutils'
/// own man page warns about. kaish keeps one meaning per spelling: `-e`
/// already tests existence, so `test -a x` is a loud error rather than a
/// quiet file test.
fn is_compound_op(s: &str) -> bool {
    matches!(s, "-a" | "-o" | "(" | ")")
}

fn is_any_op(s: &str) -> bool {
    is_unary_op(s) || is_binary_op(s) || is_compound_op(s) || s == "!"
}

/// Evaluate a `test` expression. Returns Err (→ exit 2) on any usage/type
/// error, so a malformed expression is loud, never a surprise true/false.
///
/// Structure follows bash, and the order matters: **the operand-count rules
/// come first**, and only a longer expression reaches the precedence parser.
/// That is what makes `test -o` one non-empty string (true) rather than a
/// dangling OR, and `test -a FILE` a file test rather than a broken AND —
/// the same spelling means different things at different lengths, and no
/// single grammar expresses that.
async fn eval_test(ctx: &ExecContext, operands: &[Value]) -> Result<bool, String> {
    // One or two operands can only be a primary: at those lengths `-a`/`-o`
    // are unary operators or plain strings, never the binary connectives.
    if operands.len() <= 2 {
        return eval_primary(ctx, operands).await;
    }

    // Three operands with a binary operator in the middle is a primary too,
    // and the rule outranks `!`: `test ! = x` compares the strings `!` and
    // `x`, it does not negate anything. Letting the parser see this first
    // made it read `!` as negation and then choke on the trailing operand.
    if operands.len() == 3 && is_binary_op(&value_to_string(&operands[1])) {
        return eval_primary(ctx, operands).await;
    }

    // Three operands joined by `-a`/`-o` connect two ONE-operand primaries,
    // and that rule outranks `!` as well: `test ! -a ""` is the string `!`
    // AND the empty string, false — not `!(-a "")`, true. Same family as the
    // binary-operator rule above; both were found by differential sweep.
    if operands.len() == 3 {
        let mid = value_to_string(&operands[1]);
        if mid == "-a" || mid == "-o" {
            let left = eval_primary(ctx, &operands[0..1]).await?;
            let right = eval_primary(ctx, &operands[2..3]).await?;
            return Ok(if mid == "-a" { left && right } else { left || right });
        }
    }

    // Four operands beginning with `!`: bash negates the whole THREE-operand
    // expression that follows, connective included — so `test ! x -o x` is
    // `!(x -o x)`, false, not `(!x) -o x`. That contradicts the precedence
    // the parser below uses, where `!` binds tightest, and bash's arity rule
    // wins. Found by a 420-case differential sweep against bash; it was the
    // only shape that disagreed.
    if operands.len() == 4 && value_to_string(&operands[0]) == "!" {
        let mut inner = ExprParser { ctx, ops: &operands[1..], pos: 0 };
        let value = inner.parse_or().await?;
        if inner.pos != operands.len() - 1 {
            let extra = value_to_string(&operands[1 + inner.pos]);
            return Err(format!(
                "test: unexpected '{extra}' after a complete expression"
            ));
        }
        return Ok(!value);
    }

    // Longer: parse the whole expression with bash's precedence —
    // `!` tightest, then `-a`, then `-o`, with `( )` grouping.
    let mut parser = ExprParser { ctx, ops: operands, pos: 0 };
    let value = parser.parse_or().await?;
    if parser.pos != operands.len() {
        let extra = value_to_string(&operands[parser.pos]);
        return Err(format!(
            "test: unexpected '{extra}' after a complete expression"
        ));
    }
    Ok(value)
}

/// Recursive descent over the operand list, in bash's precedence order.
///
/// The parser is only reached for three or more operands (see [`eval_test`]),
/// and every leaf defers to [`eval_primary`], so the operator tables and the
/// loud type/shape errors stay in one place.
struct ExprParser<'a> {
    ctx: &'a ExecContext,
    ops: &'a [Value],
    pos: usize,
}

impl ExprParser<'_> {
    fn peek(&self) -> Option<String> {
        self.ops.get(self.pos).map(value_to_string)
    }

    /// `or := and ( '-o' and )*`
    async fn parse_or(&mut self) -> Result<bool, String> {
        let mut left = Box::pin(self.parse_and()).await?;
        while self.peek().as_deref() == Some("-o") {
            self.pos += 1;
            // Both sides are evaluated: a `test` operand can name a file, and
            // short-circuiting would make `-o`'s right side conditionally
            // type-checked. A malformed right side is an error either way.
            let right = Box::pin(self.parse_and()).await?;
            left = left || right;
        }
        Ok(left)
    }

    /// `and := not ( '-a' not )*`
    async fn parse_and(&mut self) -> Result<bool, String> {
        let mut left = Box::pin(self.parse_not()).await?;
        while self.peek().as_deref() == Some("-a") {
            self.pos += 1;
            let right = Box::pin(self.parse_not()).await?;
            left = left && right;
        }
        Ok(left)
    }

    /// `not := '!' not | primary`
    async fn parse_not(&mut self) -> Result<bool, String> {
        if self.peek().as_deref() == Some("!") {
            self.pos += 1;
            let inner = Box::pin(self.parse_not()).await?;
            return Ok(!inner);
        }
        Box::pin(self.parse_group_or_primary()).await
    }

    /// `primary := '(' or ')' | <the operand-count forms>`
    async fn parse_group_or_primary(&mut self) -> Result<bool, String> {
        if self.peek().as_deref() == Some("(") {
            self.pos += 1;
            let inner = Box::pin(self.parse_or()).await?;
            if self.peek().as_deref() != Some(")") {
                return Err("test: missing ')'".to_string());
            }
            self.pos += 1;
            return Ok(inner);
        }

        // How many operands this primary takes, by bash's rule — the same
        // one the short forms use, applied at this position. Three when the
        // middle operand is a binary operator (`-n = -n` is string equality,
        // not `-n` applied to `=`), two for a unary operator, otherwise one.
        let start = self.pos;
        let remaining = self.ops.len() - start;
        let take = if remaining >= 3 && is_binary_op(&value_to_string(&self.ops[start + 1])) {
            3
        } else if remaining >= 2 && is_unary_op(&value_to_string(&self.ops[start])) {
            2
        } else if remaining >= 1 {
            1
        } else {
            return Err("test: missing expression".to_string());
        };
        let end = start + take;
        // A connective cannot be swallowed as an operand: reaching one here
        // means the primary before it was incomplete.
        if take == 1 {
            let word = value_to_string(&self.ops[start]);
            if matches!(word.as_str(), "-a" | "-o" | ")") {
                return Err(format!("test: '{word}' has no left-hand expression"));
            }
        }
        self.pos = end;
        eval_primary(self.ctx, &self.ops[start..end]).await
    }
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
            // `!` first: `test ! x` negates the one-operand form.
            if op == "!" {
                let inner = Box::pin(eval_primary(ctx, &operands[1..])).await?;
                return Ok(!inner);
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
            if is_binary_op(&op) {
                return apply_binary(&operands[0], &op, &operands[2]);
            }
            // `! <two-operand form>` — `test ! -f x`.
            if value_to_string(&operands[0]) == "!" {
                let inner = Box::pin(eval_primary(ctx, &operands[1..])).await?;
                return Ok(!inner);
            }
            Err(format!(
                "test: expected a binary operator (=, !=, -eq, …) between the operands, found '{op}'"
            ))
        }
        // Four or more never reaches here: `eval_test` routes anything longer
        // than two operands through the precedence parser, which hands this
        // function one primary at a time.
        n => Err(format!(
            "test: {n} operands is not an expression — a primary is at most \
             three (`a = b`)"
        )),
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
    // every reading of "does this file exist".
    if path.is_empty() {
        return false;
    }
    let resolved = ctx.resolve_path(path);
    let entry = ctx.backend.stat(&resolved).await.ok();
    match op {
        "-e" | "-r" => entry.is_some(),
        "-f" => entry.as_ref().is_some_and(|e| e.is_file()),
        "-d" => entry.as_ref().is_some_and(|e| e.is_dir()),
        "-w" => entry
            .as_ref()
            .is_some_and(|e| e.permissions.is_none_or(|p| p & 0o222 != 0)),
        "-x" => entry
            .as_ref()
            .is_some_and(|e| e.permissions.is_some_and(|p| p & 0o111 != 0)),
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
