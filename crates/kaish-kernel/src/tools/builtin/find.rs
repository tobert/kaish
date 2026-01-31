//! find — Search for files in directory hierarchy.
//!
//! # Examples
//!
//! ```kaish
//! find /workspace                    # Find all files
//! find . -name "*.rs"                # Find by name pattern
//! find src -type f                   # Find files only
//! find . -type d                     # Find directories only
//! find . -maxdepth 2                 # Limit recursion depth
//! find . -name "*.rs" -type f        # Combine predicates
//! ```

use async_trait::async_trait;
use std::path::Path;
use std::time::SystemTime;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};
use crate::walker::{EntryTypes, FileWalker, GlobPath, WalkOptions};

/// Find tool: searches for files in directory hierarchy.
pub struct Find;

#[async_trait]
impl Tool for Find {
    fn name(&self) -> &str {
        "find"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("find", "Search for files in directory hierarchy")
            .param(ParamSchema::optional(
                "path",
                "string",
                Value::String(".".into()),
                "Starting directory (default: current directory)",
            ))
            .param(ParamSchema::optional(
                "name",
                "string",
                Value::Null,
                "Pattern to match filename (glob syntax)",
            ))
            .param(ParamSchema::optional(
                "type",
                "string",
                Value::Null,
                "Type filter: 'f' for files, 'd' for directories",
            ))
            .param(ParamSchema::optional(
                "maxdepth",
                "int",
                Value::Null,
                "Maximum depth to descend",
            ))
            .param(ParamSchema::optional(
                "mtime",
                "int",
                Value::Null,
                "Modified time filter: +N older than N days, -N newer than N days",
            ))
            .param(ParamSchema::optional(
                "size",
                "string",
                Value::Null,
                "Size filter: +N larger than N bytes, -N smaller than N bytes",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        // Get starting path (positional or named)
        let start_path = args
            .get_string("path", 0)
            .unwrap_or_else(|| ".".to_string());
        let resolved_path = ctx.resolve_path(&start_path);

        // Check if path exists
        if !ctx.backend.exists(Path::new(&resolved_path)).await {
            return ExecResult::failure(1, format!("find: '{}': No such file or directory", start_path));
        }

        // Parse -name pattern
        let name_pattern = args.get_string("name", usize::MAX);

        // Parse -type
        let type_filter = args.get_string("type", usize::MAX);
        let entry_types = match type_filter.as_deref() {
            Some("f") => EntryTypes::files_only(),
            Some("d") => EntryTypes::dirs_only(),
            Some(t) => {
                return ExecResult::failure(1, format!("find: invalid type '{}': use 'f' or 'd'", t));
            }
            None => EntryTypes::all(),
        };

        // Parse -maxdepth
        let max_depth = args
            .get_named("maxdepth")
            .and_then(|v| match v {
                Value::Int(i) => Some(*i as usize),
                Value::String(s) => s.parse().ok(),
                _ => None,
            });

        // Parse -mtime (days)
        let mtime_filter = args
            .get_named("mtime")
            .or_else(|| args.get_positional(usize::MAX))
            .and_then(parse_relative_int);

        // Parse -size
        let size_filter = args
            .get_string("size", usize::MAX)
            .and_then(|s| parse_size_filter(&s));

        // Build walker options
        let mut options = WalkOptions::default();
        options.max_depth = max_depth;
        options.entry_types = entry_types;
        options.include_hidden = true; // find includes hidden by default
        options.respect_gitignore = false; // find doesn't respect gitignore

        // Build walker
        let mut walker = FileWalker::new(ctx.backend.as_ref(), &resolved_path)
            .with_options(options);

        // Add glob pattern if specified
        if let Some(ref pattern) = name_pattern {
            // Convert simple pattern to full glob
            let glob_pattern = if pattern.contains('/') {
                pattern.clone()
            } else {
                format!("**/{}", pattern)
            };

            match GlobPath::new(&glob_pattern) {
                Ok(glob) => walker = walker.with_pattern(glob),
                Err(e) => {
                    return ExecResult::failure(1, format!("find: invalid pattern '{}': {}", pattern, e));
                }
            }
        }

        // Collect results
        let paths = match walker.collect().await {
            Ok(paths) => paths,
            Err(e) => {
                return ExecResult::failure(1, format!("find: {}", e));
            }
        };

        // Apply post-filters (mtime, size) and collect results
        let mut path_strings: Vec<String> = Vec::new();
        let now_secs = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .map(|d| d.as_secs())
            .unwrap_or(0);

        for path in paths {
            // Check mtime filter
            if let Some((sign, days)) = mtime_filter {
                if let Ok(info) = ctx.backend.stat(&path).await {
                    if let Some(modified) = info.modified {
                        let age_secs = now_secs.saturating_sub(modified);
                        let age_days = age_secs / 86400;
                        let matches = match sign {
                            '+' => age_days > days,  // older than N days
                            '-' => age_days < days,  // newer than N days
                            _ => age_days == days,   // exactly N days
                        };
                        if !matches {
                            continue;
                        }
                    }
                }
            }

            // Check size filter
            if let Some((sign, size)) = size_filter {
                if let Ok(info) = ctx.backend.stat(&path).await {
                    let matches = match sign {
                        '+' => info.size > size,   // larger than N
                        '-' => info.size < size,   // smaller than N
                        _ => info.size == size,    // exactly N
                    };
                    if !matches {
                        continue;
                    }
                }
            }

            path_strings.push(path.to_string_lossy().to_string());
        }

        // Build text output (newline-separated)
        let output = path_strings.join("\n");

        // Build JSON array for structured iteration
        let json_array: Vec<serde_json::Value> = path_strings
            .iter()
            .map(|s| serde_json::Value::String(s.clone()))
            .collect();

        // Return both text output and structured data
        ExecResult::success_with_data(
            output,
            Value::Json(serde_json::Value::Array(json_array)),
        )
    }
}

/// Parse a relative integer like "+7", "-3", or "5".
fn parse_relative_int(value: &Value) -> Option<(char, u64)> {
    let s = match value {
        Value::Int(i) => return Some(('=', *i as u64)),
        Value::String(s) => s.as_str(),
        _ => return None,
    };

    if s.is_empty() {
        return None;
    }

    let first = s.chars().next()?;
    if first == '+' || first == '-' {
        let num: u64 = s[1..].parse().ok()?;
        Some((first, num))
    } else {
        let num: u64 = s.parse().ok()?;
        Some(('=', num))
    }
}

/// Parse a size filter like "+1000", "-500", "1M", "+10K".
fn parse_size_filter(s: &str) -> Option<(char, u64)> {
    if s.is_empty() {
        return None;
    }

    let first = s.chars().next()?;
    let (sign, rest) = if first == '+' || first == '-' {
        (first, &s[1..])
    } else {
        ('=', s)
    };

    // Check for size suffixes
    let last = rest.chars().last()?;
    let (num_str, multiplier) = match last.to_ascii_uppercase() {
        'K' => (&rest[..rest.len()-1], 1024u64),
        'M' => (&rest[..rest.len()-1], 1024 * 1024),
        'G' => (&rest[..rest.len()-1], 1024 * 1024 * 1024),
        _ => (rest, 1),
    };

    let num: u64 = num_str.parse().ok()?;
    Some((sign, num * multiplier))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_test_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        // Create test structure
        mem.mkdir(Path::new("src")).await.unwrap();
        mem.mkdir(Path::new("src/lib")).await.unwrap();
        mem.mkdir(Path::new("test")).await.unwrap();
        mem.mkdir(Path::new(".hidden")).await.unwrap();

        mem.write(Path::new("src/main.rs"), b"fn main() {}")
            .await
            .unwrap();
        mem.write(Path::new("src/lib.rs"), b"pub mod lib;")
            .await
            .unwrap();
        mem.write(Path::new("src/lib/utils.rs"), b"pub fn util() {}")
            .await
            .unwrap();
        mem.write(Path::new("test/test_main.rs"), b"#[test]")
            .await
            .unwrap();
        mem.write(Path::new("README.md"), b"# Test").await.unwrap();
        mem.write(Path::new(".hidden/secret.txt"), b"secret")
            .await
            .unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_find_all() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("main.rs"));
        assert!(result.out.contains("lib.rs"));
        assert!(result.out.contains("README.md"));
    }

    #[tokio::test]
    async fn test_find_by_name() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("name".to_string(), Value::String("*.rs".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("main.rs"));
        assert!(result.out.contains("lib.rs"));
        assert!(result.out.contains("utils.rs"));
        assert!(!result.out.contains("README.md"));
    }

    #[tokio::test]
    async fn test_find_type_file() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("type".to_string(), Value::String("f".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("main.rs"));
        assert!(!result.out.contains("/src\n")); // src is a directory
    }

    #[tokio::test]
    async fn test_find_type_dir() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("type".to_string(), Value::String("d".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.out.contains("src"));
        assert!(result.out.contains("lib"));
        assert!(!result.out.contains("main.rs"));
    }

    #[tokio::test]
    async fn test_find_maxdepth() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));
        args.named.insert("maxdepth".to_string(), Value::Int(1));
        args.named.insert("type".to_string(), Value::String("f".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        // Files at depth 1 (directly under /src or /test)
        assert!(result.out.contains("main.rs"));
        // Files at depth 2 (under /src/lib) should NOT be present
        assert!(!result.out.contains("utils.rs"));
    }

    #[tokio::test]
    async fn test_find_nonexistent_path() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("No such file or directory"));
    }

    #[tokio::test]
    async fn test_find_hidden() {
        let mut ctx = make_test_ctx().await;
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/".into()));

        let result = Find.execute(args, &mut ctx).await;
        assert!(result.ok());
        // find includes hidden files by default
        assert!(result.out.contains(".hidden"));
        assert!(result.out.contains("secret.txt"));
    }

    #[test]
    fn test_parse_size_filter() {
        assert_eq!(parse_size_filter("+1000"), Some(('+', 1000)));
        assert_eq!(parse_size_filter("-500"), Some(('-', 500)));
        assert_eq!(parse_size_filter("100"), Some(('=', 100)));
        assert_eq!(parse_size_filter("+1K"), Some(('+', 1024)));
        assert_eq!(parse_size_filter("+1M"), Some(('+', 1024 * 1024)));
        assert_eq!(parse_size_filter("2G"), Some(('=', 2 * 1024 * 1024 * 1024)));
    }

    #[test]
    fn test_parse_relative_int() {
        assert_eq!(parse_relative_int(&Value::Int(5)), Some(('=', 5)));
        assert_eq!(parse_relative_int(&Value::String("+7".into())), Some(('+', 7)));
        assert_eq!(parse_relative_int(&Value::String("-3".into())), Some(('-', 3)));
        assert_eq!(parse_relative_int(&Value::String("10".into())), Some(('=', 10)));
    }
}
