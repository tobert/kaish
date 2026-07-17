//! mktemp — Create temporary files or directories with unique names.
//!
//! # Examples
//!
//! ```kaish
//! mktemp                        # Create temp file in $TMPDIR (default /tmp)
//! mktemp -d                     # Create temp directory
//! mktemp -p /workspace          # Create in specified directory (overrides $TMPDIR)
//! mktemp -t myapp.XXXXXX        # Use template (X's replaced with random chars)
//! ```
//!
//! Parent-directory precedence: explicit `-p` → the `$TMPDIR` kaish var → `/tmp`.
//! All paths route through the VFS, so where temp files land follows the active
//! mount mode (real `/tmp`, an in-memory mount, …) — kaish never reads the
//! host environment directly.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::backend::WriteMode;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Mktemp tool: creates temporary files or directories with unique names.
pub struct Mktemp;

/// clap-derived argv layer for mktemp.
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

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("mktemp: {e}")),
        };
        let parsed = match MktempArgs::try_parse_from(
            std::iter::once("mktemp".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("mktemp: {e}")),
        };
        parsed.global.apply(ctx);

        let is_dir = parsed.directory;

        // Parent directory precedence (POSIX): explicit `-p` wins, then the
        // `$TMPDIR` kaish var (embedder/REPL-controlled via initial_vars — the
        // hermetic-env pattern), then `/tmp`. We never read the host's
        // environment directly.
        let parent_dir = parsed
            .p
            .clone()
            .or_else(|| {
                ctx.var("TMPDIR")
                    .map(|v| crate::interpreter::value_to_string(&v))
            })
            .filter(|s| !s.is_empty())
            .unwrap_or_else(|| "/tmp".to_string());

        // Get template
        let template = args
            .get_string("t", 0)
            .or_else(|| args.get_string("template", 0))
            .unwrap_or_else(|| "tmp.XXXXXXXXXX".to_string());

        // Generate unique name from template. Entropy failure is fatal — we
        // never emit a guessable name (see `random_suffix`).
        let name = match expand_template(&template) {
            Ok(name) => name,
            Err(e) => {
                return ExecResult::failure(1, format!("mktemp: could not obtain system entropy: {e}"));
            }
        };
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
fn expand_template(template: &str) -> Result<String, getrandom::Error> {
    // Find the longest sequence of X's
    let mut result = String::with_capacity(template.len());
    let mut x_count = 0;

    for ch in template.chars() {
        if ch == 'X' {
            x_count += 1;
        } else {
            if x_count > 0 {
                // Replace the X sequence with random chars
                result.push_str(&random_suffix(x_count)?);
                x_count = 0;
            }
            result.push(ch);
        }
    }

    // Handle trailing X's
    if x_count > 0 {
        result.push_str(&random_suffix(x_count)?);
    }

    Ok(result)
}

/// Generate a random alphanumeric suffix of the given length.
///
/// Pulls bytes from the OS CSPRNG via `getrandom` (works on Unix, macOS, and
/// `wasm32-wasip1`). There is deliberately **no fallback**: a predictable temp
/// name invites collision and symlink races, so if the system cannot supply
/// entropy we fail loudly rather than emit a guessable name.
fn random_suffix(len: usize) -> Result<String, getrandom::Error> {
    const CHARS: &[u8] = b"abcdefghijklmnopqrstuvwxyz0123456789";

    let mut entropy = vec![0u8; len];
    getrandom::fill(&mut entropy)?;

    Ok(entropy
        .iter()
        .map(|b| CHARS[(*b as usize) % CHARS.len()] as char)
        .collect())
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
        let result = expand_template("tmp.XXXXXX").expect("entropy");
        assert!(result.starts_with("tmp."));
        assert_eq!(result.len(), 10);
        assert!(result[4..].chars().all(|c| c.is_ascii_alphanumeric()));
    }

    #[test]
    fn test_expand_template_middle() {
        let result = expand_template("app.XXX.tmp").expect("entropy");
        assert!(result.starts_with("app."));
        assert!(result.ends_with(".tmp"));
        assert!(result[4..7].chars().all(|c| c.is_ascii_alphanumeric()));
    }

    #[test]
    fn test_expand_template_no_x() {
        let result = expand_template("fixed.txt").expect("entropy");
        assert_eq!(result, "fixed.txt");
    }

    #[test]
    fn test_expand_template_multiple_groups() {
        // Non-contiguous X runs each get filled; literals between them survive.
        let result = expand_template("X_XXX_X").expect("entropy");
        assert_eq!(result.len(), "X_XXX_X".len());
        let chars: Vec<char> = result.chars().collect();
        assert_eq!(chars[1], '_');
        assert_eq!(chars[5], '_');
        assert!(chars[0].is_ascii_alphanumeric());
        assert!(chars[2..5].iter().all(|c| c.is_ascii_alphanumeric()));
        assert!(chars[6].is_ascii_alphanumeric());
    }

    /// Suffixes must be unpredictable: a regression that swapped the CSPRNG
    /// for a constant or a too-coarse clock source would collide here.
    #[test]
    fn test_random_suffix_is_unique() {
        let mut seen = std::collections::HashSet::new();
        for _ in 0..1000 {
            let suffix = random_suffix(12).expect("entropy");
            assert_eq!(suffix.len(), 12);
            assert!(suffix.chars().all(|c| c.is_ascii_alphanumeric()));
            assert!(seen.insert(suffix), "random_suffix produced a duplicate");
        }
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
        assert!(stat.unwrap().is_file());
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
        assert!(stat.unwrap().is_dir());
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

    /// mktemp honors the `$TMPDIR` kaish var when no explicit `-p` is given,
    /// falling back to /tmp only when unset. An explicit `-p` still wins.
    #[tokio::test]
    async fn test_mktemp_honors_tmpdir() {
        let mut ctx = make_ctx();
        ctx.backend.mkdir(Path::new("/customtmp")).await.unwrap();
        ctx.scope.set("TMPDIR", Value::String("/customtmp".into()));

        let result = Mktemp.execute(ToolArgs::new(), &mut ctx).await;
        assert!(result.ok());
        assert!(
            result.text_out().starts_with("/customtmp/tmp."),
            "expected $TMPDIR to steer the temp file, got {}",
            result.text_out()
        );
    }

    /// An explicit -p overrides $TMPDIR (POSIX precedence).
    #[tokio::test]
    async fn test_mktemp_p_overrides_tmpdir() {
        let mut ctx = make_ctx();
        ctx.backend.mkdir(Path::new("/customtmp")).await.unwrap();
        ctx.backend.mkdir(Path::new("/explicit")).await.unwrap();
        ctx.scope.set("TMPDIR", Value::String("/customtmp".into()));

        let mut args = ToolArgs::new();
        args.named
            .insert("p".to_string(), Value::String("/explicit".into()));
        let result = Mktemp.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(
            result.text_out().starts_with("/explicit/tmp."),
            "expected -p to win over $TMPDIR, got {}",
            result.text_out()
        );
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
