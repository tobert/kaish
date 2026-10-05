//! edit — change lines of a file by the anchors `--hashline` prints.
//!
//! `cat --hashline f` prints `3:a8c7:port = 8080`; `edit f 3:a8c7 'port = 9090'`
//! replaces that line. Before writing, `edit` hashes each anchored line of the
//! file as it is now and refuses the whole call if any hash differs, so an
//! edit aimed at text that has changed fails instead of landing on the wrong
//! line. Every anchor in one call refers to the file as it was read, so line
//! numbers do not shift between the edits of a batch.
//!
//! The argv is bound verbatim and parsed here in source order: after an
//! anchor the next word is always TEXT, even when it starts with `-`.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::collections::BTreeMap;
use std::path::Path;

use crate::ast::Value;
use crate::interpreter::{ExecResult, OutputData, OutputNode};
use crate::operation::KernelOperation;
use crate::tools::{exec_context, schema_from_clap, ExecContext, Tool, ToolArgs, ToolCtx, ToolSchema};
use kaish_types::hashline;
use kaish_types::LineHasher;

/// Edit tool: change lines of a file by anchor.
pub struct Edit;

/// The most changed lines `edit` prints before it prints a summary instead.
const SHOW_LIMIT: usize = 40;

const USAGE: &str = "edit FILE ANCHOR TEXT [ANCHOR TEXT]... [--delete ANCHOR[..ANCHOR]] \
[--after ANCHOR TEXT] [--before ANCHOR TEXT]";

/// clap-derived schema for `edit`. Only `schema()` reads it: the argv is bound
/// verbatim and parsed by `parse_words`, because TEXT may look like a flag.
#[derive(Parser, Debug)]
#[command(name = "edit", about = "Change lines of a file by the LINE:HASH anchors --hashline prints")]
struct EditArgs {
    /// Delete the anchored line, or the range ANCHOR..ANCHOR, both ends included.
    #[arg(id = "delete", long = "delete", value_name = "ANCHOR")]
    _delete: Vec<String>,

    /// Insert TEXT after the anchored line: --after ANCHOR TEXT.
    #[arg(id = "after", long = "after", num_args = 2, value_names = ["ANCHOR", "TEXT"])]
    _after: Vec<String>,

    /// Insert TEXT before the anchored line: --before ANCHOR TEXT.
    #[arg(id = "before", long = "before", num_args = 2, value_names = ["ANCHOR", "TEXT"])]
    _before: Vec<String>,

    /// Print nothing on success. By default edit prints the changed lines
    /// with their new anchors, or a summary when more than 40 lines changed.
    #[arg(id = "quiet", short = 'q', long = "quiet")]
    _quiet: bool,

    /// The file, then ANCHOR TEXT pairs. Each pair replaces the anchored line,
    /// or the range ANCHOR..ANCHOR, with TEXT; a newline in TEXT makes more
    /// lines. Anchors are LINE:HASH from cat --hashline. All edits in one call
    /// refer to the file as read, and nothing is written unless every anchor
    /// still matches.
    #[arg(id = "operands", value_names = ["FILE", "ANCHOR TEXT"])]
    _operands: Vec<String>,
}

#[async_trait]
impl Tool for Edit {
    fn name(&self) -> &str {
        "edit"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &EditArgs::command(),
            "edit",
            "Change lines of a file by the LINE:HASH anchors --hashline prints",
            [
                ("Read the anchors first", "cat --hashline config.toml"),
                ("Replace line 3", "edit config.toml 3:a8c7 'port = 9090'"),
                ("Delete lines 6 to 10", "edit config.toml --delete 6:8d62..10:2325"),
                ("Insert after line 2", "edit config.toml --after 2:7f4b 'workers = 4'"),
                (
                    "Change three lines at once, all or nothing",
                    "edit config.toml 12:5f5e 'enabled = true' 13:fbb6 'size = 512' 7:6ed2 'level = \"debug\"'",
                ),
            ],
        )
        .with_operations([KernelOperation::FsOverwrite.as_str()])
        .with_verbatim_argv()
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let Some(raw_words) = args.words.as_ref() else {
            return ExecResult::failure(2, "edit: argv was not bound verbatim");
        };
        if raw_words.iter().any(|word| matches!(word, Value::Bytes(_))) {
            return ExecResult::failure(2, "edit: TEXT and anchors must be text, not binary data");
        }
        let request = match parse_words(&args.words_argv()) {
            Ok(request) => request,
            Err(message) => return ExecResult::failure(2, format!("edit: {message}")),
        };
        run(ctx, request).await
    }
}

/// `LINE:HASH`, as `--hashline` prints it.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Anchor {
    line: usize,
    hash: String,
}

impl std::fmt::Display for Anchor {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(formatter, "{}:{}", self.line, self.hash)
    }
}

/// One anchor, or an inclusive range of two.
#[derive(Debug, Clone, PartialEq, Eq)]
struct Span {
    start: Anchor,
    end: Anchor,
}

impl std::fmt::Display for Span {
    fn fmt(&self, formatter: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.start == self.end {
            write!(formatter, "{}", self.start)
        } else {
            write!(formatter, "{}..{}", self.start, self.end)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Change {
    Replace(Span, String),
    Delete(Span),
    After(Anchor, String),
    Before(Anchor, String),
}

impl Change {
    /// The anchors this change checks against the file.
    fn anchors(&self) -> Vec<&Anchor> {
        match self {
            Change::Replace(span, _) | Change::Delete(span) => vec![&span.start, &span.end],
            Change::After(anchor, _) | Change::Before(anchor, _) => vec![anchor],
        }
    }

    /// The lines this change removes, if any.
    fn removes(&self) -> Option<(usize, usize)> {
        match self {
            Change::Replace(span, _) | Change::Delete(span) => Some((span.start.line, span.end.line)),
            Change::After(..) | Change::Before(..) => None,
        }
    }

    /// How the user wrote it, for error messages.
    fn spelled(&self) -> String {
        match self {
            Change::Replace(span, _) => span.to_string(),
            Change::Delete(span) => format!("--delete {span}"),
            Change::After(anchor, _) => format!("--after {anchor}"),
            Change::Before(anchor, _) => format!("--before {anchor}"),
        }
    }
}

#[derive(Debug)]
struct Request {
    file: String,
    changes: Vec<Change>,
    quiet: bool,
}

fn parse_anchor(word: &str) -> Result<Anchor, String> {
    let not_an_anchor = || {
        format!(
            "'{word}' is not an anchor; write LINE:HASH as cat --hashline prints it, e.g. 3:a8c7"
        )
    };
    let (line, hash) = word.split_once(':').ok_or_else(not_an_anchor)?;
    if line.is_empty() || !line.bytes().all(|b| b.is_ascii_digit()) || hash.is_empty() {
        return Err(not_an_anchor());
    }
    if line.len() > 1 && line.starts_with('0') {
        return Err(not_an_anchor());
    }
    if !hash.bytes().all(|b| b.is_ascii_alphanumeric()) {
        return Err(not_an_anchor());
    }
    let line: usize = line.parse().map_err(|_| not_an_anchor())?;
    if line == 0 {
        return Err(format!("'{word}': lines are numbered from 1"));
    }
    // Compared exactly: a configured hasher may print uppercase.
    Ok(Anchor { line, hash: hash.to_string() })
}

fn parse_span(word: &str) -> Result<Span, String> {
    match word.split_once("..") {
        Some((start, end)) => {
            let (start, end) = (parse_anchor(start)?, parse_anchor(end)?);
            if start.line > end.line {
                return Err(format!(
                    "range {word}: start line {} is after end line {}",
                    start.line, end.line
                ));
            }
            Ok(Span { start, end })
        }
        None => {
            let anchor = parse_anchor(word)?;
            Ok(Span { start: anchor.clone(), end: anchor })
        }
    }
}

/// Parse the verbatim words in source order.
fn parse_words(words: &[String]) -> Result<Request, String> {
    let mut file: Option<String> = None;
    let mut changes = Vec::new();
    let mut quiet = false;
    let mut options_done = false;
    let mut iter = words.iter();
    while let Some(word) = iter.next() {
        let is_option = !options_done && word.starts_with('-') && word.len() > 1;
        if is_option {
            match word.as_str() {
                "--" => options_done = true,
                "-q" | "--quiet" => quiet = true,
                "--delete" => {
                    let span = iter.next().ok_or("--delete needs an anchor: --delete 6:8d62")?;
                    changes.push(Change::Delete(parse_span(span)?));
                }
                "--after" | "--before" => {
                    let flag = word.as_str();
                    let anchor_word = iter
                        .next()
                        .ok_or_else(|| format!("{flag} needs an anchor and TEXT: {flag} 2:7f4b 'workers = 4'"))?;
                    if anchor_word.contains("..") {
                        return Err(format!("{flag} takes one anchor, not a range: {anchor_word}"));
                    }
                    let anchor = parse_anchor(anchor_word)?;
                    let text = iter
                        .next()
                        .ok_or_else(|| format!("{flag} {anchor} needs TEXT after it"))?;
                    changes.push(if flag == "--after" {
                        Change::After(anchor, text.clone())
                    } else {
                        Change::Before(anchor, text.clone())
                    });
                }
                other => {
                    return Err(format!("{other} is not an edit option. Usage: {USAGE}"));
                }
            }
            continue;
        }
        if file.is_none() {
            file = Some(word.clone());
            continue;
        }
        let span = parse_span(word)?;
        let text = iter.next().ok_or_else(|| {
            format!("anchor {span} needs TEXT after it; to delete the line, use --delete {span}")
        })?;
        changes.push(Change::Replace(span, text.clone()));
    }
    let Some(file) = file else {
        return Err(format!("missing FILE. Usage: {USAGE}"));
    };
    if changes.is_empty() {
        return Err(format!("no edits given for {file}. Usage: {USAGE}"));
    }
    Ok(Request { file, changes, quiet })
}

/// Lines a TEXT argument contributes: `""` is one empty line, and one
/// trailing newline does not make an extra line.
fn text_lines(text: &str) -> Vec<&str> {
    if text.is_empty() {
        vec![""]
    } else {
        hashline::lines(text).collect()
    }
}

/// Why the batch cannot apply.
#[derive(Debug, PartialEq, Eq)]
enum PlanError {
    /// Anchors that no longer match: exit 1, nothing written.
    Stale(Vec<String>),
    /// Edits that contradict each other: exit 2.
    Conflict(String),
}

/// The edited file and the 1-based lines of it that are new.
#[derive(Debug, PartialEq, Eq)]
struct Plan {
    content: String,
    /// The new file's lines, as `hashline::lines` splits `content`.
    lines: Vec<String>,
    /// Inclusive ranges of new lines, in file order.
    changed: Vec<(usize, usize)>,
}

/// Check every anchor and build the new content. Pure: no I/O.
fn plan(content: &str, file: &str, changes: &[Change], hasher: &LineHasher) -> Result<Plan, PlanError> {
    let lines: Vec<&str> = hashline::lines(content).collect();
    let endings = line_endings(content);
    debug_assert_eq!(lines.len(), endings.len());

    let mut stale = Vec::new();
    for change in changes {
        for anchor in change.anchors() {
            match lines.get(anchor.line - 1) {
                None => stale.push(format!(
                    "line {} is past the end ({file} has {} line{})",
                    anchor.line,
                    lines.len(),
                    if lines.len() == 1 { "" } else { "s" }
                )),
                Some(text) if hasher.hash(text.as_bytes()) != anchor.hash => {
                    stale.push(format!("line {} changed since you read it (now: {text})", anchor.line));
                }
                Some(_) => {}
            }
        }
    }
    if !stale.is_empty() {
        stale.dedup();
        return Err(PlanError::Stale(stale));
    }

    // Each original line is removed by at most one change, and an insert
    // anchors on a line no change removes.
    let mut removed_by: BTreeMap<usize, &Change> = BTreeMap::new();
    for change in changes {
        if let Some((start, end)) = change.removes() {
            for line in start..=end {
                if let Some(other) = removed_by.insert(line, change) {
                    return Err(PlanError::Conflict(format!(
                        "{} and {} overlap at line {line}; give each line one edit",
                        other.spelled(),
                        change.spelled()
                    )));
                }
            }
        }
    }
    for change in changes {
        if let Change::After(anchor, _) | Change::Before(anchor, _) = change
            && let Some(remover) = removed_by.get(&anchor.line)
        {
            return Err(PlanError::Conflict(format!(
                "{} is inside the lines {} removes; anchor it on a line outside them",
                change.spelled(),
                remover.spelled()
            )));
        }
    }

    let mut before: BTreeMap<usize, Vec<&str>> = BTreeMap::new();
    let mut after: BTreeMap<usize, Vec<&str>> = BTreeMap::new();
    let mut starts: BTreeMap<usize, &Change> = BTreeMap::new();
    for change in changes {
        match change {
            Change::Before(anchor, text) => before.entry(anchor.line).or_default().extend(text_lines(text)),
            Change::After(anchor, text) => after.entry(anchor.line).or_default().extend(text_lines(text)),
            Change::Replace(span, _) | Change::Delete(span) => {
                starts.insert(span.start.line, change);
            }
        }
    }

    // Each output line is text plus the terminator it came with; a new line
    // has none of its own and takes the file's first terminator.
    let mut out: Vec<(&str, Option<&str>)> = Vec::with_capacity(lines.len());
    let mut changed: Vec<(usize, usize)> = Vec::new();
    let mut line = 1;
    while line <= lines.len() {
        if let Some(new) = before.get(&line) {
            push_new(&mut out, &mut changed, new);
        }
        let last = match starts.get(&line) {
            Some(Change::Replace(span, text)) => {
                push_new(&mut out, &mut changed, &text_lines(text));
                span.end.line
            }
            Some(Change::Delete(span)) => span.end.line,
            _ => {
                out.push((lines[line - 1], Some(endings[line - 1])));
                line
            }
        };
        if let Some(new) = after.get(&last) {
            push_new(&mut out, &mut changed, new);
        }
        line = last + 1;
    }

    // Untouched lines keep their own endings; new lines take the file's
    // first. The file keeps its final newline, or lack of one, except that
    // an empty last line exists only with a terminator after it.
    let default = endings.iter().copied().find(|ending| !ending.is_empty()).unwrap_or("\n");
    let had_final_newline = content.ends_with('\n') || content.is_empty();
    let mut new_content = String::with_capacity(content.len());
    for (index, (text, own)) in out.iter().enumerate() {
        new_content.push_str(text);
        let own = own.filter(|ending| !ending.is_empty()).unwrap_or(default);
        let is_last = index + 1 == out.len();
        if !is_last || had_final_newline || text.is_empty() {
            new_content.push_str(own);
        }
    }
    let lines = out.iter().map(|(text, _)| (*text).to_string()).collect();
    Ok(Plan { content: new_content, lines, changed })
}

/// The terminator after each line `hashline::lines` yields: `\r\n`, `\n`,
/// or `""` for a last line with none.
fn line_endings(content: &str) -> Vec<&str> {
    content
        .split_inclusive('\n')
        .map(|piece| {
            if piece.ends_with("\r\n") {
                "\r\n"
            } else if piece.ends_with('\n') {
                "\n"
            } else {
                ""
            }
        })
        .collect()
}

/// `file` as a shell word that runs as written: quoted when needed, and
/// quoted after `--` when it starts with `-`, which kaish reads as a flag.
fn path_operand(file: &str) -> String {
    if file.starts_with('-') {
        format!("-- '{}'", file.replace('\'', "'\\''"))
    } else {
        crate::ast::plan::quote_word(file)
    }
}

fn reread_hint(file: &str) -> String {
    format!("Read it again before editing: cat --hashline {}", path_operand(file))
}

/// Append new lines to `out`, recording them in `changed`; adjacent new
/// lines join one range.
fn push_new<'a>(
    out: &mut Vec<(&'a str, Option<&'a str>)>,
    changed: &mut Vec<(usize, usize)>,
    new: &[&'a str],
) {
    if new.is_empty() {
        return;
    }
    let start = out.len() + 1;
    out.extend(new.iter().map(|text| (*text, None)));
    match changed.last_mut() {
        Some(last) if last.1 + 1 == start => last.1 = out.len(),
        _ => changed.push((start, out.len())),
    }
}

async fn run(ctx: &mut ExecContext, request: Request) -> ExecResult {
    let Request { file, changes, quiet } = request;
    let resolved = ctx.resolve_path(&file);
    let target = Path::new(&resolved);

    let bytes = match ctx.backend.read(target, None).await {
        Ok(bytes) => bytes,
        Err(error) => return ExecResult::failure(1, format!("edit: {file}: {error}")),
    };
    let Ok(content) = String::from_utf8(bytes) else {
        return ExecResult::failure(1, format!("edit: {file}: not UTF-8 text; edit changes text files"));
    };

    let planned = match plan(&content, &file, &changes, &ctx.line_hasher) {
        Ok(planned) => planned,
        Err(PlanError::Conflict(message)) => return ExecResult::failure(2, format!("edit: {message}")),
        Err(PlanError::Stale(reasons)) => {
            let reread = reread_hint(&file);
            let message = match reasons.as_slice() {
                [one] => format!("edit: {file}: {one}; nothing was written. {reread}"),
                many => format!(
                    "edit: {file}: {} anchors no longer match; nothing was written.\n  {}\n{reread}",
                    many.len(),
                    many.join("\n  ")
                ),
            };
            return ExecResult::failure(1, message);
        }
    };

    // Under trash, snapshot the prior content first (no-op with trash off).
    let snapshots = match ctx.snapshot_overwrites("edit", &[(file.clone(), false)]).await {
        Ok(snapshots) => snapshots,
        Err(blocked) => return blocked,
    };
    // Compare-and-set against the bytes the plan was built on, never the
    // trash snapshot: a write between the read and the snapshot would match
    // the snapshot and be overwritten. A write landing between the final
    // re-read and the rename is not detected; LocalFs has no primitive that
    // closes that window.
    let planned_on = crate::tools::OverwriteExpectation::Bytes(content.into_bytes());
    if let Some(snapshot) = snapshots.get(&resolved)
        && *snapshot != planned_on
    {
        return ExecResult::failure(
            1,
            format!("edit: {file}: changed while edit was running; nothing was written. {}", reread_hint(&file)),
        );
    }
    if let Err(error) =
        crate::tools::cas_replace(&*ctx.backend, target, planned.content.as_bytes(), Some(&planned_on)).await
    {
        return ExecResult::failure(1, format!("edit: {file}: {error}"));
    }

    if quiet {
        return ExecResult::success("");
    }
    report(ctx, &file, &planned)
}

/// The changed lines with their new anchors, or a summary when there are
/// more than `SHOW_LIMIT` of them.
fn report(ctx: &ExecContext, file: &str, planned: &Plan) -> ExecResult {
    let count: usize = planned.changed.iter().map(|(start, end)| end - start + 1).sum();
    if count == 0 {
        return ExecResult::success("");
    }
    if count > SHOW_LIMIT {
        let (first, last) = (planned.changed[0].0, planned.changed[planned.changed.len() - 1].1);
        let places = planned.changed.len();
        return ExecResult::success(format!(
            "edit: {file}: changed {count} lines{} (lines {first}-{last}); view them: \
             tail -n +{first} --hashline {} | head -n {}\n",
            if places == 1 { String::new() } else { format!(" in {places} places") },
            path_operand(file),
            last - first + 1
        ));
    }
    let mut rows = Vec::with_capacity(count);
    let mut text = String::new();
    for (start, end) in &planned.changed {
        for line in *start..=*end {
            let body = planned.lines[line - 1].as_str();
            let hash = ctx.line_hasher.hash(body.as_bytes());
            text.push_str(&format!("{line}:{hash}:{body}\n"));
            rows.push(OutputNode::new(body).at_line(line as u64).with_hash(hash));
        }
    }
    ExecResult::with_output_and_text(OutputData::table(vec!["TEXT".to_string()], rows), text)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn words(script: &[&str]) -> Vec<String> {
        script.iter().map(|word| word.to_string()).collect()
    }

    fn anchor(line: usize, text: &str) -> String {
        format!("{line}:{}", hashline::fnv1a_line_hash(text.as_bytes()))
    }

    fn apply(content: &str, script: &[&str]) -> Result<Plan, PlanError> {
        let mut all = vec!["f"];
        all.extend_from_slice(script);
        let request = parse_words(&words(&all)).unwrap();
        plan(content, "f", &request.changes, &LineHasher::default())
    }

    #[test]
    fn parse_keeps_a_dash_word_after_an_anchor_as_text() {
        let request = parse_words(&words(&["f", "1:202b", "-q"])).unwrap();
        assert!(!request.quiet);
        assert_eq!(request.changes.len(), 1);
    }

    #[test]
    fn parse_rejects_leading_zeros_and_line_zero() {
        assert!(parse_anchor("03:a8c7").is_err());
        assert!(parse_anchor("0:a8c7").unwrap_err().contains("numbered from 1"));
        assert!(parse_anchor("3:a8:c7").is_err());
    }

    #[test]
    fn gap_order_is_after_then_before() {
        let a = anchor(1, "a");
        let b = anchor(2, "b");
        let plan = apply("a\nb\n", &["--before", &b, "B0", "--after", &a, "A1"]).unwrap();
        assert_eq!(plan.content, "a\nA1\nB0\nb\n");
        assert_eq!(plan.changed, vec![(2, 3)]);
    }

    #[test]
    fn deleting_every_line_leaves_an_empty_file() {
        let range = format!("{}..{}", anchor(1, "a"), anchor(2, "b"));
        let plan = apply("a\nb\n", &["--delete", &range]).unwrap();
        assert_eq!(plan.content, "");
    }

    #[test]
    fn empty_text_is_one_empty_line() {
        let plan = apply("a\nb\n", &[&anchor(1, "a"), ""]).unwrap();
        assert_eq!(plan.content, "\nb\n");
    }

    #[test]
    fn a_lone_carriage_return_at_the_end_is_part_of_the_line() {
        let plan = apply("a\nb\r", &[&anchor(2, "b\r"), "B"]).unwrap();
        assert_eq!(plan.content, "a\nB");
    }

    #[test]
    fn stale_reports_every_anchor() {
        let error = apply("a\nb\n", &["1:0000", "x", "9:0000", "y"]).unwrap_err();
        assert_eq!(
            error,
            PlanError::Stale(vec![
                "line 1 changed since you read it (now: a)".to_string(),
                "line 9 is past the end (f has 2 lines)".to_string(),
            ])
        );
    }
}
