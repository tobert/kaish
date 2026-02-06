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
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Test tool: evaluates conditional expressions.
pub struct Test;

/// Bracket alias for test: `[`
pub struct Bracket;

#[async_trait]
impl Tool for Test {
    fn name(&self) -> &str {
        "test"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("test", "Evaluate conditional expressions")
            .param(ParamSchema::optional(
                "expression",
                "string",
                Value::Null,
                "Conditional expression to evaluate",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        evaluate_test(args, ctx, false).await
    }
}

#[async_trait]
impl Tool for Bracket {
    fn name(&self) -> &str {
        "["
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("[", "Evaluate conditional expressions (alternate syntax)")
            .param(ParamSchema::optional(
                "expression",
                "string",
                Value::Null,
                "Conditional expression to evaluate (must end with ])",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        evaluate_test(args, ctx, true).await
    }
}

/// Evaluate a test expression.
async fn evaluate_test(args: ToolArgs, ctx: &mut ExecContext, bracket_mode: bool) -> ExecResult {
    // Collect all positional arguments as strings
    let mut tokens: Vec<String> = args
        .positional
        .iter()
        .filter_map(|v| match v {
            Value::String(s) => Some(s.clone()),
            Value::Int(i) => Some(i.to_string()),
            Value::Float(f) => Some(f.to_string()),
            Value::Bool(b) => Some(if *b { "true" } else { "false" }.to_string()),
            Value::Null => None,
            Value::Json(json) => Some(json.to_string()),
            Value::Blob(blob) => Some(format!("[blob: {} {}]", blob.formatted_size(), blob.content_type)),
        })
        .collect();

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
        Ok(true) => ExecResult::with_output(OutputData::text("")),
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
                Ok(info) => Ok(info.is_file),
                Err(_) => Ok(false),
            }
        }
        "-d" => {
            let path = ctx.resolve_path(arg);
            match ctx.backend.stat(Path::new(&path)).await {
                Ok(info) => Ok(info.is_dir),
                Err(_) => Ok(false),
            }
        }
        "-r" => {
            // Check if file is readable
            let path = ctx.resolve_path(arg);
            check_readable(Path::new(&path))
        }
        "-w" => {
            // Check if file is writable
            let path = ctx.resolve_path(arg);
            check_writable(Path::new(&path))
        }
        "-x" => {
            // Check if file is executable
            let path = ctx.resolve_path(arg);
            check_executable(Path::new(&path))
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
            // Is a symbolic link (for now, always false in VFS)
            Ok(false)
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

/// Check if a path is readable by the current process.
fn check_readable(path: &Path) -> Result<bool, String> {
    // First check if file exists
    if !path.exists() {
        return Ok(false);
    }
    // Try to open for reading
    Ok(std::fs::File::open(path).is_ok())
}

/// Check if a path is writable by the current process.
fn check_writable(path: &Path) -> Result<bool, String> {
    if !path.exists() {
        return Ok(false);
    }
    // Try to open for writing (append mode to avoid truncating)
    Ok(std::fs::OpenOptions::new()
        .append(true)
        .open(path)
        .is_ok())
}

/// Check if a path is executable.
fn check_executable(path: &Path) -> Result<bool, String> {
    if !path.exists() {
        return Ok(false);
    }
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;
        match path.metadata() {
            Ok(metadata) => {
                let mode = metadata.permissions().mode();
                // Check if any execute bit is set
                Ok(mode & 0o111 != 0)
            }
            Err(_) => Ok(false),
        }
    }
    #[cfg(not(unix))]
    {
        // On non-Unix platforms, check file extension
        match path.extension().and_then(|e| e.to_str()) {
            Some("exe") | Some("bat") | Some("cmd") | Some("com") => Ok(true),
            _ => Ok(false),
        }
    }
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
