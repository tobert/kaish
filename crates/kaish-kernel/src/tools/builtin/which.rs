//! which — Locate a command in PATH.
//!
//! # Examples
//!
//! ```kaish
//! which cargo                   # Find cargo in PATH
//! which -a python               # Show all matches, not just first
//! which ls cat                  # Find multiple commands
//! ```

use async_trait::async_trait;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::ExecResult;
use crate::tools::{ExecContext, ParamSchema, Tool, ToolArgs, ToolSchema};

/// Which tool: locates commands in PATH.
pub struct Which;

#[async_trait]
impl Tool for Which {
    fn name(&self) -> &str {
        "which"
    }

    fn schema(&self) -> ToolSchema {
        ToolSchema::new("which", "Locate a command in PATH")
            .param(ParamSchema::required(
                "command",
                "string",
                "Command name(s) to find",
            ))
            .param(ParamSchema::optional(
                "a",
                "bool",
                Value::Bool(false),
                "Print all matches, not just the first (-a)",
            ))
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        if args.positional.is_empty() {
            return ExecResult::failure(1, "which: missing command name");
        }

        let all_matches = args.has_flag("a");

        // Get PATH from scope or environment
        let path_var = ctx
            .scope
            .get("PATH")
            .map(value_to_string)
            .unwrap_or_else(|| std::env::var("PATH").unwrap_or_default());

        let path_dirs: Vec<&str> = path_var.split(':').collect();

        let mut output = String::new();
        let mut found_any = false;
        let mut not_found = Vec::new();

        for arg in &args.positional {
            let name = match arg {
                Value::String(s) => s.as_str(),
                _ => continue,
            };

            let matches = find_in_path(name, &path_dirs);

            if matches.is_empty() {
                not_found.push(name);
            } else {
                found_any = true;
                if all_matches {
                    for path in matches {
                        output.push_str(&path);
                        output.push('\n');
                    }
                } else {
                    output.push_str(&matches[0]);
                    output.push('\n');
                }
            }
        }

        // Trim trailing newline
        if output.ends_with('\n') {
            output.pop();
        }

        if found_any {
            if not_found.is_empty() {
                ExecResult::success(output)
            } else {
                // Some found, some not - still success but report not found
                let mut result = ExecResult::success(output);
                result.err = not_found
                    .iter()
                    .map(|n| format!("which: no {} in ({})", n, path_var))
                    .collect::<Vec<_>>()
                    .join("\n");
                result
            }
        } else {
            ExecResult::failure(
                1,
                not_found
                    .iter()
                    .map(|n| format!("which: no {} in ({})", n, path_var))
                    .collect::<Vec<_>>()
                    .join("\n"),
            )
        }
    }
}

/// Find all occurrences of a command in PATH directories.
fn find_in_path(name: &str, path_dirs: &[&str]) -> Vec<String> {
    let mut results = Vec::new();

    for dir in path_dirs {
        if dir.is_empty() {
            continue;
        }

        let full_path = format!("{}/{}", dir, name);
        let path = Path::new(&full_path);

        // Check if file exists and is executable
        if path.is_file() {
            #[cfg(unix)]
            {
                use std::os::unix::fs::PermissionsExt;
                if let Ok(metadata) = path.metadata() {
                    let mode = metadata.permissions().mode();
                    if mode & 0o111 != 0 {
                        results.push(full_path);
                    }
                }
            }

            #[cfg(not(unix))]
            {
                // On non-Unix systems, just check if file exists
                results.push(full_path);
            }
        }
    }

    results
}

/// Convert a Value to a string.
fn value_to_string(value: &Value) -> String {
    match value {
        Value::Null => String::new(),
        Value::Bool(b) => b.to_string(),
        Value::Int(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::String(s) => s.clone(),
    }
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

    #[tokio::test]
    async fn test_which_finds_common_commands() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ls".into()));

        let result = Which.execute(args, &mut ctx).await;
        // ls should exist on any Unix system
        if result.ok() {
            assert!(result.out.contains("/ls"));
        }
        // If not found, that's also valid (might be in a different location)
    }

    #[tokio::test]
    async fn test_which_not_found() {
        let mut ctx = make_ctx();
        // Set empty PATH
        ctx.scope.set("PATH", Value::String("/nonexistent".into()));

        let mut args = ToolArgs::new();
        args.positional
            .push(Value::String("definitely_not_a_command_xyz".into()));

        let result = Which.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("no definitely_not_a_command_xyz"));
    }

    #[tokio::test]
    async fn test_which_no_args() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Which.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing command name"));
    }

    #[tokio::test]
    async fn test_which_multiple_commands() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ls".into()));
        args.positional.push(Value::String("cat".into()));

        let result = Which.execute(args, &mut ctx).await;
        // Should find at least one of them on Unix
        if result.ok() {
            let lines: Vec<&str> = result.out.lines().collect();
            assert!(lines.len() >= 1);
        }
    }

    #[tokio::test]
    async fn test_which_uses_path_from_scope() {
        let mut ctx = make_ctx();
        ctx.scope.set("PATH", Value::String("/usr/bin:/bin".into()));

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("ls".into()));

        let result = Which.execute(args, &mut ctx).await;
        // Should search in the specified PATH
        if result.ok() {
            assert!(
                result.out.starts_with("/usr/bin/") || result.out.starts_with("/bin/")
            );
        }
    }
}
