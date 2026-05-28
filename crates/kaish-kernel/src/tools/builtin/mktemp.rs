//! mktemp — Create temporary files or directories with unique names.
//!
//! # Examples
//!
//! ```kaish
//! mktemp                        # Create temp file in /tmp
//! mktemp -d                     # Create temp directory
//! mktemp -p /workspace          # Create in specified directory
//! mktemp -t myapp.XXXXXX        # Use template (X's replaced with random chars)
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{SystemTime, UNIX_EPOCH};

use crate::backend::WriteMode;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Global counter for unique temp file generation.
static TEMP_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Mktemp tool: creates temporary files or directories with unique names.
pub struct Mktemp;

/// clap-derived argv layer for mktemp. See docs/clap-migration.md.
#[derive(Parser, Debug)]
#[command(name = "mktemp", about = "Create temporary file or directory with unique name")]
struct MktempArgs {
    /// Create a directory instead of a file (-d)
    #[arg(short = 'd', long = "d")]
    directory: bool,

    /// Use specified directory instead of /tmp (-p DIR)
    #[arg(short = 'p', long = "p")]
    p: Option<String>,

    /// Template for filename (X's replaced with random chars)
    #[arg(short = 't', long = "t")]
    t: Option<String>,

    /// Template (positional form / --template)
    #[arg(long)]
    template: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — the positional template is the same value as `--template`;
    /// read off args.positional so users can write `mktemp TPL.XXXX` without
    /// the flag. The `--template` flag in the schema covers both forms.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for Mktemp {
    fn name(&self) -> &str {
        "mktemp"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &MktempArgs::command(),
            "mktemp",
            "Create temporary file or directory with unique name",
            [
                ("Create temp file", "mktemp"),
                ("Create temp directory", "mktemp -d"),
                ("Custom template", "mktemp -t myapp.XXXXXX"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let parsed = match MktempArgs::try_parse_from(
            std::iter::once("mktemp".to_string()).chain(args.to_argv()),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("mktemp: {e}")),
        };
        parsed.global.apply(ctx);

        let is_dir = parsed.directory;

        // Get parent directory
        let parent_dir = args
            .get_string("p", usize::MAX)
            .unwrap_or_else(|| "/tmp".to_string());

        // Get template
        let template = args
            .get_string("t", 0)
            .or_else(|| args.get_string("template", 0))
            .unwrap_or_else(|| "tmp.XXXXXXXXXX".to_string());

        // Generate unique name from template
        let name = expand_template(&template);
        let full_path = format!("{}/{}", parent_dir, name);
        let resolved = ctx.resolve_path(&full_path);

        // Create the temp file or directory
        if is_dir {
            match ctx.backend.mkdir(Path::new(&resolved)).await {
                Ok(()) => ExecResult::with_output(OutputData::text(resolved.to_string_lossy().to_string())),
                Err(e) => ExecResult::failure(1, format!("mktemp: failed to create directory: {}", e)),
            }
        } else {
            // Create empty file (CreateNew ensures uniqueness)
            match ctx.backend.write(Path::new(&resolved), &[], WriteMode::CreateNew).await {
                Ok(()) => ExecResult::with_output(OutputData::text(resolved.to_string_lossy().to_string())),
                Err(e) => ExecResult::failure(1, format!("mktemp: failed to create file: {}", e)),
            }
        }
    }
}

/// Expand a template by replacing X's with random characters.
///
/// Examples:
/// - `tmp.XXXXXX` → `tmp.a1b2c3`
/// - `myapp.XXX.tmp` → `myapp.f9d.tmp`
fn expand_template(template: &str) -> String {
    // Find the longest sequence of X's
    let mut result = String::with_capacity(template.len());
    let mut x_count = 0;

    for ch in template.chars() {
        if ch == 'X' {
            x_count += 1;
        } else {
            if x_count > 0 {
                // Replace the X sequence with random chars
                result.push_str(&random_suffix(x_count));
                x_count = 0;
            }
            result.push(ch);
        }
    }

    // Handle trailing X's
    if x_count > 0 {
        result.push_str(&random_suffix(x_count));
    }

    result
}

/// Generate a random alphanumeric suffix of the given length.
fn random_suffix(len: usize) -> String {
    const CHARS: &[u8] = b"abcdefghijklmnopqrstuvwxyz0123456789";

    // Try to get system randomness, fall back to time-based entropy
    let mut entropy = get_system_entropy(len);
    if entropy.len() < len {
        // Fallback: use time + counter as entropy source
        let now = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .map(|d| d.as_nanos())
            .unwrap_or(0);
        let counter = TEMP_COUNTER.fetch_add(1, Ordering::SeqCst);
        let mut state = now as u64 ^ (counter << 32) ^ (counter >> 32);

        entropy.clear();
        for _ in 0..len {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            entropy.push(state as u8);
        }
    }

    entropy
        .iter()
        .map(|b| CHARS[(*b as usize) % CHARS.len()] as char)
        .collect()
}

/// Get system-provided random bytes.
#[cfg(unix)]
fn get_system_entropy(len: usize) -> Vec<u8> {
    use std::io::Read;

    let mut buf = vec![0u8; len];
    if let Ok(mut file) = std::fs::File::open("/dev/urandom")
        && file.read_exact(&mut buf).is_ok() {
            return buf;
        }
    Vec::new() // Return empty on failure, triggering fallback
}

#[cfg(not(unix))]
fn get_system_entropy(_len: usize) -> Vec<u8> {
    Vec::new() // Use fallback on non-Unix
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Value;
    use crate::vfs::{MemoryFs, VfsRouter};
    use std::sync::Arc;

    fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        vfs.mount("/", MemoryFs::new());
        vfs.mount("/tmp", MemoryFs::new());
        ExecContext::new(Arc::new(vfs))
    }

    #[test]
    fn test_expand_template_suffix() {
        let result = expand_template("tmp.XXXXXX");
        assert!(result.starts_with("tmp."));
        assert_eq!(result.len(), 10);
        assert!(result[4..].chars().all(|c| c.is_ascii_alphanumeric()));
    }

    #[test]
    fn test_expand_template_middle() {
        let result = expand_template("app.XXX.tmp");
        assert!(result.starts_with("app."));
        assert!(result.ends_with(".tmp"));
        assert!(result[4..7].chars().all(|c| c.is_ascii_alphanumeric()));
    }

    #[test]
    fn test_expand_template_no_x() {
        let result = expand_template("fixed.txt");
        assert_eq!(result, "fixed.txt");
    }

    #[tokio::test]
    async fn test_mktemp_creates_file() {
        let mut ctx = make_ctx();
        let args = ToolArgs::new();

        let result = Mktemp.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().starts_with("/tmp/tmp."));

        // Verify file was created
        let stat = ctx.backend.stat(Path::new(&*result.text_out())).await;
        assert!(stat.is_ok());
        assert_eq!(stat.unwrap().is_file(), true);
    }

    #[tokio::test]
    async fn test_mktemp_creates_directory() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("d".to_string());

        let result = Mktemp.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().starts_with("/tmp/tmp."));

        // Verify directory was created
        let stat = ctx.backend.stat(Path::new(&*result.text_out())).await;
        assert!(stat.is_ok());
        assert_eq!(stat.unwrap().is_dir(), true);
    }

    #[tokio::test]
    async fn test_mktemp_custom_parent() {
        let mut ctx = make_ctx();
        // Create parent directory first
        ctx.backend.mkdir(Path::new("/workspace")).await.unwrap();

        let mut args = ToolArgs::new();
        args.named
            .insert("p".to_string(), Value::String("/workspace".into()));

        let result = Mktemp.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().starts_with("/workspace/tmp."));
    }

    #[tokio::test]
    async fn test_mktemp_custom_template() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.named
            .insert("t".to_string(), Value::String("myapp.XXXXX".into()));

        let result = Mktemp.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().starts_with("/tmp/myapp."));
        assert_eq!(result.text_out().len(), "/tmp/myapp.".len() + 5);
    }

    #[tokio::test]
    async fn test_mktemp_unique_names() {
        let mut ctx = make_ctx();

        let result1 = Mktemp.execute(ToolArgs::new(), &mut ctx).await;
        let result2 = Mktemp.execute(ToolArgs::new(), &mut ctx).await;

        assert!(result1.ok());
        assert!(result2.ok());
        assert_ne!(&*result1.text_out(), &*result2.text_out());
    }
}
