//! patch — Apply unified diffs to files.
//!
//! # Examples
//!
//! ```kaish
//! patch < changes.patch           # Apply patch from stdin
//! patch -p1 < changes.patch       # Strip 1 path component
//! patch -R < changes.patch        # Reverse the patch
//! patch --dry-run < changes.patch # Show what would change
//! patch file.txt < changes.patch  # Explicit target file
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::Path;

use crate::ast::Value;
use crate::backend::PatchOp;
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::builtin::get_path_string;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// Patch tool: applies unified diffs to files.
pub struct Patch;

/// clap-derived argv layer for patch.
#[derive(Parser, Debug)]
#[command(name = "patch", about = "Apply unified diff to files")]
struct PatchArgs {
    /// Strip N leading path components (-p).
    #[arg(short = 'p')]
    p: Option<i64>,

    /// Reverse the patch (swap + and -).
    #[arg(short = 'R', long = "reverse")]
    reverse: bool,

    /// Show what would change without applying.
    #[arg(long = "dry-run", visible_alias = "dry_run")]
    dry_run: bool,

    /// Target file (overrides patch header).
    #[arg(long = "file")]
    file: Option<String>,

    /// Confirmation nonce for a latch-gated overwrite.
    #[arg(long = "confirm")]
    confirm: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Sink — positional file path is the same value as `--file`; the kernel's
    /// `args.get_string("file", 0)` falls back to positional[0], so users can
    /// write either `patch --file foo.txt < diff` or `patch foo.txt < diff`.
    #[arg(hide = true)]
    rest: Vec<String>,
}

#[async_trait]
impl Tool for Patch {
    fn name(&self) -> &str {
        "patch"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &PatchArgs::command(),
            "patch",
            "Apply unified diff to files",
            [
                ("Apply a patch", "patch < changes.patch"),
                ("Dry run", "patch --dry-run < changes.patch"),
                ("Strip path prefix", "patch -p1 < changes.patch"),
            ],
        )
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // Tests poke args.flags.insert("dry-run") and args.named.insert("p", Int(1)).
        // `-R` flag and `--dry-run` flag work directly. The `p=1` form lands as
        // a single-char named entry which to_argv renders as `-p=1`; clap's
        // `Option<i64>` with short='p' handles that natively.
        args.flagify_bool_named(&self.schema());

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("patch: {e}")),
        };
        let parsed = match PatchArgs::try_parse_from(
            std::iter::once("patch".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("patch: {e}")),
        };
        parsed.global.apply(ctx);

        // Read patch content from stdin (pipe or buffered — `read_stdin_to_text`
        // prefers the streaming pipe, so a piped `cmd | patch` / `patch < file`
        // is seen, not just a frontend-buffered String).
        let patch_content = match ctx.read_stdin_to_text().await {
            Ok(s) => s.unwrap_or_default(),
            Err(e) => return ExecResult::failure(2, format!("patch: {e}")),
        };
        if patch_content.is_empty() {
            return ExecResult::failure(1, "patch: no input provided (use stdin)");
        }

        // Parse options
        let strip_level = parsed
            .p
            .map(|i| i as usize)
            .or_else(|| {
                args.get_named("p").and_then(|v| match v {
                    Value::Int(i) => Some(*i as usize),
                    Value::String(s) => s.parse().ok(),
                    _ => None,
                })
            })
            .unwrap_or(0);

        let reverse = parsed.reverse || args.has_flag("R");
        let dry_run = parsed.dry_run || args.has_flag("dry-run");
        // Read the untouched typed value FIRST (not `parsed.file`) — clap's
        // own field comes from `to_argv()`'s re-serialization, the same lossy
        // stringify boundary this PR closes elsewhere, so checking it first
        // would silently defeat the guard below. A binary `--file`/positional
        // override goes loud rather than silently falling back to deriving
        // target paths from the diff hunks.
        let explicit_file = match get_path_string(&args, "file", 0) {
            Ok(Some(f)) => Some(f),
            Ok(None) => parsed.file.clone(),
            Err(e) => return ExecResult::failure(1, format!("patch: {e}")),
        };

        // Parse the unified diff
        let hunks = match parse_unified_diff(&patch_content) {
            Ok(h) => h,
            Err(e) => return ExecResult::failure(1, format!("patch: {}", e)),
        };

        if hunks.is_empty() {
            return ExecResult::failure(1, "patch: no valid hunks found in input");
        }

        let groups = group_by_file(&hunks);

        // Gate truncating overwrites through latch + trash (no-op when both are
        // off; skipped for --dry-run, which never writes). patch always rewrites
        // an existing file, so every target is a non-append overwrite; one nonce
        // scopes the whole set of files the diff touches. The snapshot copies
        // the prior content (it doesn't move the file), so the read + CAS write
        // below still see the file in place.
        if !dry_run {
            let targets: Vec<(String, bool)> = groups
                .iter()
                .map(|fh| {
                    let p = match &explicit_file {
                        Some(explicit) => explicit.clone(),
                        None => strip_path(&fh.target_file, strip_level),
                    };
                    (p, false)
                })
                .collect();
            if let Err(blocked) = ctx
                .gate_overwrites("patch", &targets, parsed.confirm.as_deref(), |nonce, joined| {
                    format!("patch --confirm=\"{nonce}\" {joined}")
                })
                .await
            {
                return blocked;
            }
        }

        let mut output = String::new();

        // Group hunks by target file
        for file_hunks in &groups {
            let target_path = if let Some(ref explicit) = explicit_file {
                explicit.clone()
            } else {
                strip_path(&file_hunks.target_file, strip_level)
            };

            let resolved_path = ctx.resolve_path(&target_path);
            let path = Path::new(&resolved_path);

            // Read current file content — patch is a text operation; a binary
            // target is a loud error, not a lossy decode.
            let current_content = match ctx.backend.read(path, None).await {
                Ok(data) => match String::from_utf8(data) {
                    Ok(s) => s,
                    Err(_) => {
                        return ExecResult::failure(
                            1,
                            format!("patch: '{}': binary data, cannot patch", target_path),
                        )
                    }
                },
                Err(e) => {
                    return ExecResult::failure(
                        1,
                        format!("patch: cannot read '{}': {}", target_path, e),
                    );
                }
            };

            // Locate + apply each hunk with GNU-style offset search and fuzz.
            // Computing the resulting content is a pure, heavily-tested step; a
            // hunk that matches nowhere (even with fuzz) is a loud failure, never
            // a blind splice.
            let (new_content, report) =
                match apply_hunks(&current_content, &file_hunks.hunks, reverse, DEFAULT_FUZZ) {
                    Ok(v) => v,
                    Err(e) => {
                        return ExecResult::failure(
                            1,
                            format!("patch: failed to apply to '{}': {}", target_path, e),
                        );
                    }
                };

            if dry_run {
                output.push_str(&format!("checking file {}\n", target_path));
                for h in &report {
                    output.push_str(&format!("  {}\n", describe_outcome(h)));
                }
            } else {
                // Whole-file compare-and-swap replace: TOCTOU-safe (the CAS
                // `expected` makes a concurrent change a loud Conflict, never a
                // silent overwrite) and uniform across local/overlay backends,
                // which both route `Replace` through `apply_patch_op`.
                let ops = vec![PatchOp::Replace {
                    offset: 0,
                    len: current_content.len(),
                    content: new_content,
                    expected: Some(current_content.clone()),
                }];
                if let Err(e) = ctx.backend.patch(path, &ops).await {
                    return ExecResult::failure(
                        1,
                        format!("patch: failed to apply to '{}': {}", target_path, e),
                    );
                }
                output.push_str(&format!("patching file {}\n", target_path));
                // Follow patch(1): stay quiet on a clean apply; report offset/fuzz
                // loudly when a hunk landed off its header position or needed
                // context trimmed — drift the agent should see, not silently eat.
                for h in &report {
                    if h.offset != 0 || h.fuzz != 0 {
                        output.push_str(&format!("{}\n", describe_outcome(h)));
                    }
                }
            }
        }

        ExecResult::with_output(OutputData::text(output.trim_end()))
    }
}

/// A single hunk from a unified diff.
#[derive(Debug, Clone)]
struct DiffHunk {
    /// Original file start line (1-indexed)
    old_start: usize,
    /// Original file line count
    old_count: usize,
    /// New file start line (1-indexed)
    new_start: usize,
    /// New file line count
    new_count: usize,
    /// Lines in this hunk
    lines: Vec<DiffLine>,
}

/// A single line in a diff hunk.
#[derive(Debug, Clone)]
enum DiffLine {
    Context(String),
    Delete(String),
    Insert(String),
}

/// Hunks grouped by file.
struct FileHunks {
    target_file: String,
    hunks: Vec<DiffHunk>,
}

/// Parse unified diff format into hunks.
fn parse_unified_diff(content: &str) -> Result<Vec<FileHunks>, String> {
    let mut result: Vec<FileHunks> = Vec::new();
    let mut current_file: Option<String> = None;
    let mut current_hunks: Vec<DiffHunk> = Vec::new();
    let mut current_hunk: Option<DiffHunk> = None;

    for line in content.lines() {
        // Detect file header (--- and +++)
        if line.starts_with("--- ") {
            // Save previous file if any
            if let Some(file) = current_file.take() {
                if let Some(hunk) = current_hunk.take() {
                    current_hunks.push(hunk);
                }
                if !current_hunks.is_empty() {
                    result.push(FileHunks {
                        target_file: file,
                        hunks: std::mem::take(&mut current_hunks),
                    });
                }
            }
            // Parse will continue with +++ line
        } else if line.starts_with("+++ ") {
            // Extract target filename (after +++ )
            let path = line
                .strip_prefix("+++ ")
                .unwrap_or("")
                .split('\t')
                .next()
                .unwrap_or("")
                .to_string();
            current_file = Some(path);
        } else if line.starts_with("@@ ") {
            // Save previous hunk if any
            if let Some(hunk) = current_hunk.take() {
                current_hunks.push(hunk);
            }
            // Parse hunk header: @@ -old_start,old_count +new_start,new_count @@
            current_hunk = Some(parse_hunk_header(line)?);
        } else if let Some(ref mut hunk) = current_hunk {
            // Parse hunk content
            if let Some(rest) = line.strip_prefix('-') {
                hunk.lines.push(DiffLine::Delete(rest.to_string()));
            } else if let Some(rest) = line.strip_prefix('+') {
                hunk.lines.push(DiffLine::Insert(rest.to_string()));
            } else if let Some(rest) = line.strip_prefix(' ') {
                hunk.lines.push(DiffLine::Context(rest.to_string()));
            } else if line.is_empty() || line == "\\ No newline at end of file" {
                // Handle empty context line or no-newline marker
                if line.is_empty() {
                    hunk.lines.push(DiffLine::Context(String::new()));
                }
            }
        }
    }

    // Save final hunk and file
    if let Some(hunk) = current_hunk {
        current_hunks.push(hunk);
    }
    if let Some(file) = current_file
        && !current_hunks.is_empty() {
            result.push(FileHunks {
                target_file: file,
                hunks: current_hunks,
            });
        }

    Ok(result)
}

/// Parse a hunk header like "@@ -1,3 +1,4 @@" or "@@ -1 +1,2 @@".
fn parse_hunk_header(line: &str) -> Result<DiffHunk, String> {
    // Remove @@ prefix and suffix
    let content = line
        .strip_prefix("@@ ")
        .and_then(|s| s.split(" @@").next())
        .ok_or_else(|| format!("invalid hunk header: {}", line))?;

    // Split into old and new parts
    let parts: Vec<&str> = content.split_whitespace().collect();
    if parts.len() < 2 {
        return Err(format!("invalid hunk header: {}", line));
    }

    let (old_start, old_count) = parse_range(parts[0].strip_prefix('-').unwrap_or(parts[0]))?;
    let (new_start, new_count) = parse_range(parts[1].strip_prefix('+').unwrap_or(parts[1]))?;

    Ok(DiffHunk {
        old_start,
        old_count,
        new_start,
        new_count,
        lines: Vec::new(),
    })
}

/// Parse a range like "1,3" or just "1" (which means count of 1).
fn parse_range(s: &str) -> Result<(usize, usize), String> {
    if let Some((start, count)) = s.split_once(',') {
        let start: usize = start.parse().map_err(|_| format!("invalid number: {}", s))?;
        let count: usize = count.parse().map_err(|_| format!("invalid number: {}", s))?;
        Ok((start, count))
    } else {
        let start: usize = s.parse().map_err(|_| format!("invalid number: {}", s))?;
        Ok((start, 1))
    }
}

/// Strip leading path components from a path.
fn strip_path(path: &str, level: usize) -> String {
    if level == 0 {
        return path.to_string();
    }

    let components: Vec<&str> = path.split('/').collect();
    if level >= components.len() {
        components.last().unwrap_or(&path).to_string()
    } else {
        components[level..].join("/")
    }
}

/// Group parsed hunks by file.
fn group_by_file(file_hunks: &[FileHunks]) -> Vec<&FileHunks> {
    // Already grouped by parse_unified_diff, just return references
    file_hunks.iter().collect()
}

/// Default fuzz: lines of leading/trailing context a hunk may ignore at each end
/// to still apply (matches GNU patch's default `--fuzz=2`).
const DEFAULT_FUZZ: usize = 2;

/// How far (in lines) a *fuzzed* hunk may be located from its expected position
/// before patch gives up. With context trimmed, the match is weaker evidence, so
/// we trust the header position more: a fuzzy block that only matches far away is
/// more likely a coincidental collision than the intended site — fail loud rather
/// than splice it there. An exact (fuzz-0) match has full context as strong
/// evidence and is searched across the whole file (GNU-parity, nearest wins).
const FUZZ_SEARCH_WINDOW: usize = 64;

/// Where one hunk landed, for the GNU-style success report.
#[derive(Debug, Clone, PartialEq, Eq)]
struct HunkOutcome {
    /// 1-indexed hunk number.
    index: usize,
    /// 1-indexed line in the resulting buffer where the hunk's change block began.
    applied_at: usize,
    /// Lines away from where this hunk was expected (after prior hunks' drift).
    /// 0 = landed exactly where the header (so adjusted) said.
    offset: isize,
    /// Context lines ignored at each end to make it match. 0 = exact context.
    fuzz: usize,
}

/// Format a hunk outcome GNU-patch style:
/// `Hunk #2 succeeded at 20 with fuzz 1 (offset 5 lines)`.
fn describe_outcome(h: &HunkOutcome) -> String {
    let mut s = format!("Hunk #{} succeeded at {}", h.index, h.applied_at);
    if h.fuzz != 0 {
        s.push_str(&format!(" with fuzz {}", h.fuzz));
    }
    if h.offset != 0 {
        let n = h.offset.abs();
        s.push_str(&format!(
            " (offset {} line{})",
            h.offset,
            if n == 1 { "" } else { "s" }
        ));
    }
    s
}

/// The lines a hunk expects to find (`source`) and the lines it leaves behind
/// (`result`), plus the count of pure-context lines at each end (the only lines
/// fuzz may trim). Context lines appear identically in both blocks; deletes live
/// only in `source`, inserts only in `result`. In reverse mode the roles swap —
/// the file currently holds the patched ("new") side.
fn build_blocks(hunk: &DiffHunk, reverse: bool) -> (usize, Vec<String>, Vec<String>, usize, usize) {
    let mut source = Vec::new();
    let mut result = Vec::new();
    for line in &hunk.lines {
        match line {
            DiffLine::Context(s) => {
                source.push(s.clone());
                result.push(s.clone());
            }
            DiffLine::Delete(s) => {
                if reverse {
                    result.push(s.clone());
                } else {
                    source.push(s.clone());
                }
            }
            DiffLine::Insert(s) => {
                if reverse {
                    source.push(s.clone());
                } else {
                    result.push(s.clone());
                }
            }
        }
    }
    let anchor = if reverse { hunk.new_start } else { hunk.old_start };
    let lead_ctx = hunk
        .lines
        .iter()
        .take_while(|l| matches!(l, DiffLine::Context(_)))
        .count();
    let trail_ctx = hunk
        .lines
        .iter()
        .rev()
        .take_while(|l| matches!(l, DiffLine::Context(_)))
        .count();
    (anchor, source, result, lead_ctx, trail_ctx)
}

/// True if `src` matches `buf` exactly starting at `pos`.
fn matches_at(buf: &[String], src: &[&str], pos: usize) -> bool {
    pos + src.len() <= buf.len() && buf[pos..pos + src.len()].iter().zip(src).all(|(a, b)| a == b)
}

/// Find where `src` matches `buf`, searching outward from `center` (the position
/// the header expects) up to `limit` lines away. Returns the nearest match,
/// preferring `center`. An empty `src` (pure insertion) "matches" at the clamped
/// center. `limit` is `usize::MAX` for exact (full-context) matches and a tight
/// window for fuzzed ones — see [`FUZZ_SEARCH_WINDOW`].
fn search_block(buf: &[String], src: &[&str], center: usize, limit: usize) -> Option<usize> {
    if src.is_empty() {
        return Some(center.min(buf.len()));
    }
    if src.len() > buf.len() {
        return None;
    }
    let max_start = buf.len() - src.len();
    // Distance from the *unclamped* center to the farthest valid start in
    // `0..=max_start`. Clamping center to the buffer first (the old bug) would
    // measure `limit` from the clamped position, silently defeating the fuzz
    // window when the header points past EOF — a fuzzed hunk could then splice
    // into a coincidental match ~the-whole-file away. Keeping the true center
    // means `dist > limit` correctly rejects far matches; an exact (full-context)
    // match still searches file-wide because its `limit` is `usize::MAX`.
    let max_possible = center.max(max_start.saturating_sub(center));
    let max_dist = max_possible.min(limit);
    for dist in 0..=max_dist {
        if dist == 0 {
            if center <= max_start && matches_at(buf, src, center) {
                return Some(center);
            }
            continue;
        }
        if center >= dist {
            let backward = center - dist;
            if backward <= max_start && matches_at(buf, src, backward) {
                return Some(backward);
            }
        }
        let forward = center + dist;
        if forward <= max_start && matches_at(buf, src, forward) {
            return Some(forward);
        }
    }
    None
}

/// Apply unified-diff hunks to `content`, GNU-style. Each hunk is located by
/// matching its context+change lines near the header position (adjusted by the
/// drift of prior hunks), searching outward for an *offset* and, failing an exact
/// match, trimming up to `max_fuzz` context lines at each end (*fuzz*). Only the
/// matched span is rewritten — trimmed-away context is left untouched, so fuzz
/// never overwrites lines it didn't verify. Returns the rebuilt content plus a
/// per-hunk report; a hunk that matches nowhere is a loud `Err`, never a splice.
fn apply_hunks(
    content: &str,
    hunks: &[DiffHunk],
    reverse: bool,
    max_fuzz: usize,
) -> Result<(String, Vec<HunkOutcome>), String> {
    // Internal consistency of each hunk body vs its header counts — independent
    // of the target file, so a malformed patch is rejected loudly up front.
    for (i, hunk) in hunks.iter().enumerate() {
        let actual_old = hunk
            .lines
            .iter()
            .filter(|l| matches!(l, DiffLine::Context(_) | DiffLine::Delete(_)))
            .count();
        let actual_new = hunk
            .lines
            .iter()
            .filter(|l| matches!(l, DiffLine::Context(_) | DiffLine::Insert(_)))
            .count();
        if actual_old != hunk.old_count || actual_new != hunk.new_count {
            return Err(format!(
                "hunk {}: line count mismatch (header says -{}/+{}, actual -{}/+{})",
                i + 1,
                hunk.old_count,
                hunk.new_count,
                actual_old,
                actual_new
            ));
        }
    }

    // Preserve the file's line-ending convention. `str::lines()` strips both
    // `\n` and a preceding `\r`, so a naive `join("\n")` would silently rewrite
    // every line of a CRLF file to LF (whole-file corruption from a one-line
    // patch). Detect the dominant ending and rejoin with it; patch bodies are
    // parsed `\r`-stripped too, so inserted lines pick up the same ending.
    let line_ending = if content.contains("\r\n") { "\r\n" } else { "\n" };
    let had_trailing_newline = content.ends_with('\n');
    let mut buf: Vec<String> = content.lines().map(|s| s.to_string()).collect();
    let mut outcomes = Vec::new();
    // Running drift of the buffer vs the original line numbering (lines added
    // minus removed by hunks already applied).
    let mut applied_delta: isize = 0;

    for (i, hunk) in hunks.iter().enumerate() {
        let (anchor, source, result, lead_ctx, trail_ctx) = build_blocks(hunk, reverse);

        // 0-indexed position the full source block is expected at. A pure
        // insertion (empty source) anchors *after* `anchor` original lines.
        let expected = if source.is_empty() {
            (anchor as isize + applied_delta).max(0) as usize
        } else {
            (anchor as isize - 1 + applied_delta).max(0) as usize
        };

        let mut found: Option<(usize, usize, usize, Vec<String>, usize)> = None;
        for f in 0..=max_fuzz {
            let lead_drop = f.min(lead_ctx);
            let trail_drop = f.min(trail_ctx);
            // f==0 always tries the full block; skip a higher fuzz level that
            // would trim nothing new, and one that would over-trim either block.
            if f > 0 && lead_drop == 0 && trail_drop == 0 {
                continue;
            }
            if lead_drop + trail_drop > source.len().min(result.len()) {
                continue;
            }
            let src: Vec<&str> = source[lead_drop..source.len() - trail_drop]
                .iter()
                .map(String::as_str)
                .collect();
            // The inner block starts `lead_drop` lines after the full block.
            let inner_expected = expected + lead_drop;
            // Full-context (f==0) matches are trusted file-wide; fuzzed matches
            // must land near the header position (see FUZZ_SEARCH_WINDOW).
            let limit = if f == 0 { usize::MAX } else { FUZZ_SEARCH_WINDOW };
            if let Some(pos) = search_block(&buf, &src, inner_expected, limit) {
                let res = result[lead_drop..result.len() - trail_drop].to_vec();
                found = Some((pos, f, src.len(), res, inner_expected));
                break;
            }
        }

        let (pos, fuzz, removed, res, inner_expected) = match found {
            Some(v) => v,
            None => {
                return Err(format!(
                    "Hunk #{} FAILED to apply (no matching context near line {})",
                    i + 1,
                    expected + 1
                ));
            }
        };

        let inserted = res.len();
        buf.splice(pos..pos + removed, res);
        applied_delta += inserted as isize - removed as isize;
        outcomes.push(HunkOutcome {
            index: i + 1,
            applied_at: pos + 1,
            offset: pos as isize - inner_expected as isize,
            fuzz,
        });
    }

    let mut new_content = buf.join(line_ending);
    if had_trailing_newline && !new_content.is_empty() {
        new_content.push_str(line_ending);
    }
    Ok((new_content, outcomes))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    async fn make_test_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();

        // Create test file
        mem.write(Path::new("test.txt"), b"line1\nline2\nline3\n")
            .await
            .unwrap();

        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    fn simple_patch() -> String {
        // Note: context lines must have a leading space!
        concat!(
            "--- a/test.txt\n",
            "+++ b/test.txt\n",
            "@@ -1,3 +1,3 @@\n",
            " line1\n",
            "-line2\n",
            "+modified\n",
            " line3\n",
        )
        .to_string()
    }

    #[tokio::test]
    async fn test_patch_apply() {
        let mut ctx = make_test_ctx().await;
        ctx.stdin = Some(simple_patch());

        let mut args = ToolArgs::new();
        // Strip 'b/' prefix from target path
        args.named.insert("p".to_string(), Value::Int(1));
        let result = Patch.execute(args, &mut ctx).await;

        assert!(result.ok(), "patch failed: {}", result.err);
        assert!(result.text_out().contains("patching file"));
        // Follow patch(1): no kaish-specific "N changes applied" summary line.
        assert!(
            !result.text_out().contains("changes applied"),
            "unexpected summary line: {}",
            result.text_out()
        );

        // Verify the file was modified
        let content = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        let text = String::from_utf8_lossy(&content);
        assert!(text.contains("modified"), "file not modified: {}", text);
        assert!(!text.contains("line2"), "old line still present");
    }

    #[tokio::test]
    async fn test_patch_dry_run() {
        let mut ctx = make_test_ctx().await;
        ctx.stdin = Some(simple_patch());

        let mut args = ToolArgs::new();
        args.named.insert("p".to_string(), Value::Int(1));
        args.flags.insert("dry-run".to_string());

        let result = Patch.execute(args, &mut ctx).await;

        assert!(result.ok(), "dry-run failed: {}", result.err);
        assert!(
            result.text_out().contains("checking file") && result.text_out().contains("Hunk #1"),
            "dry-run should report what it checked: {}",
            result.text_out()
        );

        // Verify the file was NOT modified
        let content = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        let text = String::from_utf8_lossy(&content);
        assert!(text.contains("line2"), "file was modified in dry-run mode");
    }

    #[tokio::test]
    async fn test_patch_reverse() {
        let mut ctx = make_test_ctx().await;

        // First apply the patch
        ctx.stdin = Some(simple_patch());
        let mut args = ToolArgs::new();
        args.named.insert("p".to_string(), Value::Int(1));
        Patch.execute(args, &mut ctx).await;

        // Then reverse it
        ctx.stdin = Some(simple_patch());
        let mut args = ToolArgs::new();
        args.named.insert("p".to_string(), Value::Int(1));
        args.flags.insert("R".to_string());

        let result = Patch.execute(args, &mut ctx).await;

        assert!(result.ok(), "reverse patch failed: {}", result.err);

        // Verify original content restored
        let content = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        let text = String::from_utf8_lossy(&content);
        assert!(text.contains("line2"), "original not restored: {}", text);
    }

    #[tokio::test]
    async fn test_patch_strip_path() {
        assert_eq!(strip_path("a/b/c/file.txt", 0), "a/b/c/file.txt");
        assert_eq!(strip_path("a/b/c/file.txt", 1), "b/c/file.txt");
        assert_eq!(strip_path("a/b/c/file.txt", 2), "c/file.txt");
        assert_eq!(strip_path("a/b/c/file.txt", 3), "file.txt");
        assert_eq!(strip_path("a/b/c/file.txt", 10), "file.txt");
    }

    #[tokio::test]
    async fn test_patch_no_input() {
        let mut ctx = make_test_ctx().await;
        // No stdin

        let args = ToolArgs::new();
        let result = Patch.execute(args, &mut ctx).await;

        assert!(!result.ok());
        assert!(result.err.contains("no input"));
    }

    #[test]
    fn test_parse_hunk_header() {
        let hunk = parse_hunk_header("@@ -1,3 +1,4 @@").unwrap();
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.old_count, 3);
        assert_eq!(hunk.new_start, 1);
        assert_eq!(hunk.new_count, 4);

        let hunk = parse_hunk_header("@@ -1 +1,2 @@").unwrap();
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.old_count, 1);
        assert_eq!(hunk.new_start, 1);
        assert_eq!(hunk.new_count, 2);
    }

    #[test]
    fn test_parse_unified_diff() {
        let patch = simple_patch();
        let files = parse_unified_diff(&patch).unwrap();

        assert_eq!(files.len(), 1);
        assert_eq!(files[0].target_file, "b/test.txt");
        assert_eq!(files[0].hunks.len(), 1);

        let hunk = &files[0].hunks[0];
        assert_eq!(hunk.old_start, 1);
        assert_eq!(hunk.lines.len(), 4); // context + delete + insert + context
    }

    #[test]
    fn test_hunk_count_mismatch_detected() {
        // Header claims 2 old lines but only 1 context + 0 deletes = 1 old line
        let bad_patch = concat!(
            "--- a/test.txt\n",
            "+++ b/test.txt\n",
            "@@ -1,2 +1,1 @@\n",
            " line1\n",
        );
        let files = parse_unified_diff(bad_patch).unwrap();
        let content = "line1\nline2\nline3\n";
        let result = apply_hunks(content, &files[0].hunks, false, DEFAULT_FUZZ);
        assert!(result.is_err(), "should reject mismatched hunk counts");
        let err = result.unwrap_err();
        assert!(
            err.contains("line count mismatch"),
            "error should mention mismatch: {}",
            err
        );
    }

    // ── apply_hunks: offset search + fuzz (pure core) ────────────────────────

    #[test]
    fn apply_hunks_clean_apply() {
        let files = parse_unified_diff(&simple_patch()).unwrap();
        let (new, report) =
            apply_hunks("line1\nline2\nline3\n", &files[0].hunks, false, DEFAULT_FUZZ).unwrap();
        assert_eq!(new, "line1\nmodified\nline3\n");
        assert_eq!(report.len(), 1);
        assert_eq!(report[0].offset, 0);
        assert_eq!(report[0].fuzz, 0);
        assert_eq!(report[0].applied_at, 1);
    }

    #[test]
    fn apply_hunks_offset_search() {
        // Two extra leading lines: the hunk's header says line 1, but its context
        // actually sits at line 3 — GNU patch finds it and reports the offset.
        let files = parse_unified_diff(&simple_patch()).unwrap();
        let (new, report) = apply_hunks(
            "x\ny\nline1\nline2\nline3\n",
            &files[0].hunks,
            false,
            DEFAULT_FUZZ,
        )
        .unwrap();
        assert_eq!(new, "x\ny\nline1\nmodified\nline3\n");
        assert_eq!(report[0].offset, 2, "found two lines down");
        assert_eq!(report[0].fuzz, 0);
        assert_eq!(report[0].applied_at, 3);
    }

    #[test]
    fn apply_hunks_fuzz_ignores_changed_context() {
        // The trailing context line drifted ("line3" → "DIFFERENT"): an exact
        // match fails, but trimming one context line (fuzz 1) still applies the
        // real change — and leaves the drifted context untouched.
        let files = parse_unified_diff(&simple_patch()).unwrap();
        let (new, report) = apply_hunks(
            "line1\nline2\nDIFFERENT\n",
            &files[0].hunks,
            false,
            DEFAULT_FUZZ,
        )
        .unwrap();
        assert_eq!(new, "line1\nmodified\nDIFFERENT\n");
        assert_eq!(report[0].fuzz, 1);
        assert_eq!(report[0].offset, 0);
    }

    #[test]
    fn apply_hunks_no_match_fails_loud() {
        // Nothing resembling the hunk's context/changes — patch must refuse, not
        // splice blind.
        let files = parse_unified_diff(&simple_patch()).unwrap();
        let err = apply_hunks("totally\ndifferent\nstuff\n", &files[0].hunks, false, DEFAULT_FUZZ)
            .unwrap_err();
        assert!(err.contains("FAILED"), "got: {err}");
    }

    #[test]
    fn apply_hunks_reverse_restores_original() {
        let files = parse_unified_diff(&simple_patch()).unwrap();
        let (new, _) =
            apply_hunks("line1\nmodified\nline3\n", &files[0].hunks, true, DEFAULT_FUZZ).unwrap();
        assert_eq!(new, "line1\nline2\nline3\n");
    }

    #[test]
    fn apply_hunks_preserves_crlf_line_endings() {
        // Patching one line of a CRLF file must not silently rewrite every line
        // to LF (whole-file corruption). The result keeps \r\n throughout.
        let files = parse_unified_diff(&simple_patch()).unwrap();
        let (new, _) = apply_hunks(
            "line1\r\nline2\r\nline3\r\n",
            &files[0].hunks,
            false,
            DEFAULT_FUZZ,
        )
        .unwrap();
        assert_eq!(new, "line1\r\nmodified\r\nline3\r\n");
    }

    #[test]
    fn apply_hunks_fuzz_search_is_bounded() {
        // Both of the hunk's contexts have drifted, and its lone change line
        // ("line2") exists only far from the header position. A *fuzzed* match
        // must stay near the header — splicing into that distant coincidental
        // match would be silent corruption — so this fails loud instead.
        let files = parse_unified_diff(&simple_patch()).unwrap();
        let mut lines: Vec<String> = (0..100).map(|_| "x".to_string()).collect();
        lines[99] = "line2".to_string(); // > FUZZ_SEARCH_WINDOW from line 1
        let content = format!("{}\n", lines.join("\n"));
        let err = apply_hunks(&content, &files[0].hunks, false, DEFAULT_FUZZ).unwrap_err();
        assert!(err.contains("FAILED"), "got: {err}");
    }

    #[test]
    fn apply_hunks_fuzz_window_bounded_past_eof() {
        // A hunk header pointing far past EOF must not let a *fuzzed* match
        // splice into a coincidental hit elsewhere in a short file. The bug:
        // `search_block` clamped the expected center to the buffer end and then
        // measured the fuzz window from the *clamped* position, silently
        // defeating FUZZ_SEARCH_WINDOW — the change applied ~999 lines from
        // where the header claimed (offset -999), instead of failing loud.
        let patch = concat!(
            "--- a/t.txt\n",
            "+++ b/t.txt\n",
            "@@ -1000,3 +1000,3 @@\n",
            " ctxA\n",
            "-old\n",
            "+new\n",
            " ctxB\n",
        );
        let files = parse_unified_diff(patch).unwrap();
        let err = apply_hunks("x\nold\ny\n", &files[0].hunks, false, DEFAULT_FUZZ).unwrap_err();
        assert!(
            err.contains("FAILED"),
            "a fuzzed match far outside the window must fail loud, got: {err}"
        );
    }

    // ── execute-level: offset reported, failure loud + non-destructive ───────

    #[tokio::test]
    async fn test_patch_apply_reports_offset() {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(Path::new("test.txt"), b"x\ny\nline1\nline2\nline3\n")
            .await
            .unwrap();
        vfs.mount("/", mem);
        let mut ctx = ExecContext::new(Arc::new(vfs));
        ctx.stdin = Some(simple_patch());

        let mut args = ToolArgs::new();
        args.named.insert("p".to_string(), Value::Int(1));
        let result = Patch.execute(args, &mut ctx).await;

        assert!(result.ok(), "patch failed: {}", result.err);
        assert!(
            result.text_out().contains("offset"),
            "should report the offset loudly: {}",
            result.text_out()
        );
        let content = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(
            String::from_utf8_lossy(&content),
            "x\ny\nline1\nmodified\nline3\n"
        );
    }

    #[tokio::test]
    async fn test_patch_failed_is_loud_and_nondestructive() {
        let mut ctx = make_test_ctx().await; // test.txt = line1\nline2\nline3
        let bad = concat!(
            "--- a/test.txt\n",
            "+++ b/test.txt\n",
            "@@ -1,3 +1,3 @@\n",
            " nope1\n",
            "-nope2\n",
            "+changed\n",
            " nope3\n",
        );
        ctx.stdin = Some(bad.to_string());
        let mut args = ToolArgs::new();
        args.named.insert("p".to_string(), Value::Int(1));
        let result = Patch.execute(args, &mut ctx).await;

        assert!(!result.ok(), "mismatched patch must fail loud");
        assert!(
            result.err.to_lowercase().contains("failed"),
            "error should say it failed: {}",
            result.err
        );
        // The file must be untouched.
        let content = ctx.backend.read(Path::new("/test.txt"), None).await.unwrap();
        assert_eq!(String::from_utf8_lossy(&content), "line1\nline2\nline3\n");
    }
}
