//! test — POSIX conditional expressions.
//!
//! Evaluates conditional expressions and returns exit code 0 (true) or 1 (false).
//!
//! # Examples
//!
//! ```kaish
//! test -e file.txt              # File exists
//! test -f file.txt              # Is regular file
//! test -d mydir                 # Is directory
//! test -z ""                    # String is empty
//! test -n "hello"               # String is not empty
//! test "a" = "a"                # String equality
//! test "a" != "b"               # String inequality
//! test 5 -eq 5                  # Numeric equality
//! test 3 -lt 5                  # Numeric less than
//! test 5 -gt 3                  # Numeric greater than
//! test 5 -ge 5                  # Numeric greater than or equal
//! test 5 -le 5                  # Numeric less than or equal
//! test ! -e nofile              # Negation
//! [ -f file.txt ]               # Alternate syntax (requires closing ])
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputFormat};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Test tool: evaluates conditional expressions.
pub struct Test;

/// Bracket alias for test: `[`
pub struct Bracket;

/// clap-derived argv layer for test. See docs/clap-migration.md.
///
/// POSIX `test`/`[` predicate syntax (`-e FILE`, `-z STR`, `EXPR1 -a EXPR2`,
/// …) does not fit clap's option model — `-e` etc. are operators, not flags.
/// We only declare `GlobalFlags` here so `--json` is reflected into the
/// schema; the body skips clap parsing entirely and lets the existing
/// evaluator consume all positionals/flags as POSIX predicate tokens.
#[derive(Parser, Debug)]
#[command(name = "test", about = "Evaluate conditional expressions")]
struct TestArgs {
    #[command(flatten)]
    global: GlobalFlags,
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
            "Evaluate conditional expressions",
            [
                ("File exists", "test -e file.txt"),
                ("String equality", "test \"$VAR\" = \"expected\""),
                ("Numeric comparison", "test 5 -gt 3"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        apply_json_if_present(&args, ctx);
        evaluate_test(args, ctx, false).await
    }
}

#[async_trait]
impl Tool for Bracket {
    fn name(&self) -> &str {
        "["
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TestArgs::command(),
            "[",
            "Evaluate conditional expressions (alternate syntax)",
            [
                ("Check file type", "[ -f file.txt ]"),
                ("String test", "[ -n \"$VAR\" ]"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        apply_json_if_present(&args, ctx);
        evaluate_test(args, ctx, true).await
    }
}

/// Honor the global `--json` flag without invoking the full clap parser —
/// `test`'s positional grammar is not clap-shaped, so we just peek at the
/// already-parsed flags / named map.
fn apply_json_if_present(args: &ToolArgs, ctx: &mut ExecContext) {
    if args.has_flag("json") {
        ctx.output_format = Some(OutputFormat::Json);
    }
}

/// Evaluate a test expression.
async fn evaluate_test(args: ToolArgs, ctx: &mut ExecContext, bracket_mode: bool) -> ExecResult {
    // Reconstruct the original token sequence from parsed ToolArgs.
    // The schema-aware parser splits "-r" into args.flags, but test needs
    // it as a positional operator. Prepend flags as "-{flag}" tokens.
    let mut tokens: Vec<String> = Vec::new();
    for flag in &args.flags {
        tokens.push(format!("-{flag}"));
    }
    for v in &args.positional {
        match v {
            Value::String(s) => tokens.push(s.clone()),
            Value::Int(i) => tokens.push(i.to_string()),
            Value::Float(f) => tokens.push(f.to_string()),
            Value::Bool(b) => tokens.push(if *b { "true" } else { "false" }.to_string()),
            Value::Null => {}
            Value::Json(json) => tokens.push(json.to_string()),
            Value::Blob(blob) => tokens.push(format!("[blob: {} {}]", blob.formatted_size(), blob.content_type)),
        }
    }

    // In bracket mode, verify and remove trailing ]
    if bracket_mode {
        if tokens.last().map(|s| s.as_str()) != Some("]") {
            return ExecResult::failure(2, "[: missing closing ]");
        }
        tokens.pop();
    }

    // Empty expression is false
    if tokens.is_empty() {
        return ExecResult::from_output(1, "", "");
    }

    // Parse and evaluate the expression
    let result = evaluate_expression(&tokens, ctx).await;

    match result {
        Ok(true) => ExecResult::success(""),
        Ok(false) => ExecResult::from_output(1, "", ""),
        Err(e) => ExecResult::failure(2, format!("test: {}", e)),
    }
}

/// Evaluate a test expression from tokens.
///
/// Handles most common POSIX test expressions without complex nesting.
async fn evaluate_expression(tokens: &[String], ctx: &ExecContext) -> Result<bool, String> {
    if tokens.is_empty() {
        return Ok(false);
    }

    // Handle leading negation
    let (negate, tokens) = if tokens[0] == "!" {
        (true, &tokens[1..])
    } else {
        (false, tokens)
    };

    if tokens.is_empty() {
        // "test !" with nothing after - error
        return Err("argument expected".to_string());
    }

    // Single token: non-empty string is true
    let result = if tokens.len() == 1 {
        !tokens[0].is_empty()
    } else if tokens.len() == 2 {
        // Two tokens: unary operator
        evaluate_unary(&tokens[0], &tokens[1], ctx).await?
    } else if tokens.len() >= 3 {
        // Three or more tokens: handle binary + optional compound
        evaluate_with_compounds(tokens, ctx).await?
    } else {
        false
    };

    Ok(if negate { !result } else { result })
}

/// Evaluate expression with potential compound operators (-a, -o).
async fn evaluate_with_compounds(tokens: &[String], ctx: &ExecContext) -> Result<bool, String> {
    // Find all compound operators
    let mut parts: Vec<&[String]> = Vec::new();
    let mut operators: Vec<&str> = Vec::new();
    let mut start = 0;

    for (i, token) in tokens.iter().enumerate() {
        if token == "-a" || token == "-o" {
            if i > start {
                parts.push(&tokens[start..i]);
            }
            operators.push(token);
            start = i + 1;
        }
    }
    if start < tokens.len() {
        parts.push(&tokens[start..]);
    }

    // If no compound operators, just evaluate the simple expression
    if operators.is_empty() {
        return evaluate_simple(tokens, ctx).await;
    }

    // Evaluate left-to-right with compound operators
    // Note: -a has higher precedence than -o in POSIX, but we'll use simple left-to-right
    let mut result = evaluate_simple(parts[0], ctx).await?;

    for (i, op) in operators.iter().enumerate() {
        let right = evaluate_simple(parts[i + 1], ctx).await?;

        result = match *op {
            "-a" => result && right,
            "-o" => result || right,
            _ => return Err(format!("unexpected operator: {}", op)),
        };
    }

    Ok(result)
}

/// Evaluate a simple expression (unary or binary, no compounds).
async fn evaluate_simple(tokens: &[String], ctx: &ExecContext) -> Result<bool, String> {
    if tokens.is_empty() {
        return Ok(false);
    }

    // Handle negation - at most one level
    let (negate, tokens) = if tokens.first().map(|s| s.as_str()) == Some("!") {
        (true, &tokens[1..])
    } else {
        (false, tokens)
    };

    let result = match tokens.len() {
        0 => return Err("argument expected after !".to_string()),
        1 => !tokens[0].is_empty(),
        2 => evaluate_unary(&tokens[0], &tokens[1], ctx).await?,
        3 => evaluate_binary(&tokens[0], &tokens[1], &tokens[2], ctx).await?,
        _ => return Err(format!("too many arguments: {:?}", tokens)),
    };

    Ok(if negate { !result } else { result })
}

/// Evaluate unary operators.
async fn evaluate_unary(op: &str, arg: &str, ctx: &ExecContext) -> Result<bool, String> {
    match op {
        // String tests
        "-z" => Ok(arg.is_empty()),
        "-n" => Ok(!arg.is_empty()),

        // File tests
        "-e" => {
            let path = ctx.resolve_path(arg);
            Ok(ctx.backend.exists(Path::new(&path)).await)
        }
        "-f" => {
            let path = ctx.resolve_path(arg);
            match ctx.backend.stat(Path::new(&path)).await {
                Ok(info) => Ok(info.is_file()),
                Err(_) => Ok(false),
            }
        }
        "-d" => {
            let path = ctx.resolve_path(arg);
            match ctx.backend.stat(Path::new(&path)).await {
                Ok(info) => Ok(info.is_dir()),
                Err(_) => Ok(false),
            }
        }
        "-r" => {
            let path = ctx.resolve_path(arg);
            match ctx.backend.stat(Path::new(&path)).await {
                // MemoryFs has no permissions — assume readable if file exists
                Ok(info) => Ok(info.permissions.is_none_or(|p| p & 0o444 != 0)),
                Err(_) => Ok(false),
            }
        }
        "-w" => {
            let path = ctx.resolve_path(arg);
            match ctx.backend.stat(Path::new(&path)).await {
                Ok(info) => Ok(info.permissions.is_none_or(|p| p & 0o222 != 0)),
                Err(_) => Ok(false),
            }
        }
        "-x" => {
            let path = ctx.resolve_path(arg);
            match ctx.backend.stat(Path::new(&path)).await {
                // Not executable by default when permissions unknown
                Ok(info) => Ok(info.permissions.is_some_and(|p| p & 0o111 != 0)),
                Err(_) => Ok(false),
            }
        }
        "-s" => {
            // File exists and has size > 0
            let path = ctx.resolve_path(arg);
            match ctx.backend.stat(Path::new(&path)).await {
                Ok(info) => Ok(info.size > 0),
                Err(_) => Ok(false),
            }
        }
        "-L" | "-h" => {
            let path = ctx.resolve_path(arg);
            match ctx.backend.lstat(Path::new(&path)).await {
                Ok(info) => Ok(info.is_symlink()),
                Err(_) => Ok(false),
            }
        }

        _ => Err(format!("unknown unary operator: {}", op)),
    }
}

/// Evaluate binary operators.
async fn evaluate_binary(
    left: &str,
    op: &str,
    right: &str,
    _ctx: &ExecContext,
) -> Result<bool, String> {
    match op {
        // String comparison
        "=" | "==" => Ok(left == right),
        "!=" => Ok(left != right),

        // Numeric comparison
        "-eq" => {
            let l = parse_int(left)?;
            let r = parse_int(right)?;
            Ok(l == r)
        }
        "-ne" => {
            let l = parse_int(left)?;
            let r = parse_int(right)?;
            Ok(l != r)
        }
        "-lt" => {
            let l = parse_int(left)?;
            let r = parse_int(right)?;
            Ok(l < r)
        }
        "-le" => {
            let l = parse_int(left)?;
            let r = parse_int(right)?;
            Ok(l <= r)
        }
        "-gt" => {
            let l = parse_int(left)?;
            let r = parse_int(right)?;
            Ok(l > r)
        }
        "-ge" => {
            let l = parse_int(left)?;
            let r = parse_int(right)?;
            Ok(l >= r)
        }

        // String ordering (lexicographic)
        "<" => Ok(left < right),
        ">" => Ok(left > right),

        _ => Err(format!("unknown binary operator: {}", op)),
    }
}

/// Parse an integer from a string.
fn parse_int(s: &str) -> Result<i64, String> {
    s.parse::<i64>()
        .map_err(|_| format!("invalid integer: {}", s))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_test_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        // Create test files and directories
        mem.write(Path::new("file.txt"), b"content").await.unwrap();
        mem.write(Path::new("empty.txt"), b"").await.unwrap();
        mem.mkdir(Path::new("mydir")).await.unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_file_exists() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-e".into()));
        args.positional.push(Value::String("file.txt".into()));

        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "file.txt should exist");
    }

    #[tokio::test]
    async fn test_file_not_exists() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-e".into()));
        args.positional.push(Value::String("nonexistent.txt".into()));

        let result = Test.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1, "nonexistent.txt should not exist");
    }

    #[tokio::test]
    async fn test_is_file() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-f".into()));
        args.positional.push(Value::String("file.txt".into()));

        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "file.txt should be a file");
    }

    #[tokio::test]
    async fn test_is_dir() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-d".into()));
        args.positional.push(Value::String("mydir".into()));

        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "mydir should be a directory");
    }

    #[tokio::test]
    async fn test_file_has_size() {
        let mut ctx = make_test_ctx().await;

        // file.txt has content
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-s".into()));
        args.positional.push(Value::String("file.txt".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "file.txt should have size > 0");

        // empty.txt has no content
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-s".into()));
        args.positional.push(Value::String("empty.txt".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1, "empty.txt should have size 0");
    }

    #[tokio::test]
    async fn test_string_empty() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-z".into()));
        args.positional.push(Value::String("".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "empty string should be zero-length");

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-z".into()));
        args.positional.push(Value::String("hello".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1, "non-empty string should not be zero-length");
    }

    #[tokio::test]
    async fn test_string_not_empty() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-n".into()));
        args.positional.push(Value::String("hello".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "non-empty string should be non-zero-length");
    }

    #[tokio::test]
    async fn test_string_equality() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("=".into()));
        args.positional.push(Value::String("hello".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "strings should be equal");

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        args.positional.push(Value::String("=".into()));
        args.positional.push(Value::String("world".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1, "strings should not be equal");
    }

    #[tokio::test]
    async fn test_string_inequality() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("a".into()));
        args.positional.push(Value::String("!=".into()));
        args.positional.push(Value::String("b".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "a != b should be true");
    }

    #[tokio::test]
    async fn test_numeric_equality() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("5".into()));
        args.positional.push(Value::String("-eq".into()));
        args.positional.push(Value::String("5".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "5 -eq 5 should be true");
    }

    #[tokio::test]
    async fn test_numeric_less_than() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("3".into()));
        args.positional.push(Value::String("-lt".into()));
        args.positional.push(Value::String("5".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "3 -lt 5 should be true");

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("5".into()));
        args.positional.push(Value::String("-lt".into()));
        args.positional.push(Value::String("3".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1, "5 -lt 3 should be false");
    }

    #[tokio::test]
    async fn test_numeric_greater_than() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("5".into()));
        args.positional.push(Value::String("-gt".into()));
        args.positional.push(Value::String("3".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "5 -gt 3 should be true");
    }

    #[tokio::test]
    async fn test_negation() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("!".into()));
        args.positional.push(Value::String("-e".into()));
        args.positional.push(Value::String("nonexistent.txt".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "! -e nonexistent.txt should be true");
    }

    #[tokio::test]
    async fn test_bracket_syntax() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-e".into()));
        args.positional.push(Value::String("file.txt".into()));
        args.positional.push(Value::String("]".into()));
        let result = Bracket.execute(args, &mut ctx).await;
        assert!(result.ok(), "[ -e file.txt ] should be true");
    }

    #[tokio::test]
    async fn test_bracket_missing_close() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("-e".into()));
        args.positional.push(Value::String("file.txt".into()));
        // Missing ]
        let result = Bracket.execute(args, &mut ctx).await;
        assert_eq!(result.code, 2, "should fail without closing ]");
    }

    #[tokio::test]
    async fn test_empty_expression() {
        let mut ctx = make_test_ctx().await;

        let args = ToolArgs::new();
        let result = Test.execute(args, &mut ctx).await;
        assert_eq!(result.code, 1, "empty expression should be false");
    }

    #[tokio::test]
    async fn test_single_string_true() {
        let mut ctx = make_test_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("hello".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "non-empty single string should be true");
    }

    #[tokio::test]
    async fn test_numeric_comparisons() {
        let mut ctx = make_test_ctx().await;

        // -le (less than or equal)
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("5".into()));
        args.positional.push(Value::String("-le".into()));
        args.positional.push(Value::String("5".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "5 -le 5 should be true");

        // -ge (greater than or equal)
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("5".into()));
        args.positional.push(Value::String("-ge".into()));
        args.positional.push(Value::String("5".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "5 -ge 5 should be true");

        // -ne (not equal)
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("3".into()));
        args.positional.push(Value::String("-ne".into()));
        args.positional.push(Value::String("5".into()));
        let result = Test.execute(args, &mut ctx).await;
        assert!(result.ok(), "3 -ne 5 should be true");
    }
}
