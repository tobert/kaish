//! sed — Stream editor for filtering and transforming text.
//!
//! A Bourne-lite sed implementation focused on the 80% use case.
//! Uses ERE (extended regex) syntax like egrep. By default it also accepts the
//! GNU BRE backslash-metas (`\|`, `\+`, `\(…\)`, `\{N,M\}`) as a forgiving
//! superset (issue #60); pass `-E`/`-r` for strict ERE where those escapes are
//! literals.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use regex::{Regex, RegexBuilder};
use std::path::Path;

use crate::ast::Value;
use crate::backend::PatchOp;
use crate::tools::builtin::get_path_string;
use crate::tools::builtin::regex_dialect::{append_dialect_hint, bre_metas_to_ere};
use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{schema_from_clap, validate_against_schema, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};
use crate::validator::{IssueCode, ValidationIssue};

/// Sed tool: stream editor for text transformations.
pub struct Sed;

/// clap-derived argv layer for sed.
///
/// Sed expression syntax stays hand-rolled — only argv-level flags like
/// `-n` (quiet) and `-e EXPR` go through clap.
#[derive(Parser, Debug)]
#[command(name = "sed", about = "Stream editor for filtering and transforming text")]
struct SedArgs {
    /// Suppress automatic printing (-n).
    #[arg(short = 'n', long = "quiet")]
    quiet: bool,

    /// Sed expression to execute (-e). Repeatable: each `-e` adds an
    /// expression, applied in order (clap `Append` → schema `repeatable`).
    #[arg(short = 'e', long = "expression")]
    expression: Vec<String>,

    /// Strict ERE (-E/-r): backslash-escaped metas match the literal
    /// character (`\|` is a `|`, `\(` a paren). Default mode also accepts
    /// the GNU BRE spellings (`s/a\|b/X/`, `s/\(a\)\(b\)/\2\1/`) as
    /// operators; see the regex notes in `help sed`.
    #[arg(short = 'E', short_alias = 'r', long = "regexp-extended")]
    extended: bool,

    /// Edit files in place (-i) instead of streaming to stdout. Requires file
    /// operands. (The GNU glued backup suffix `-i.bak` is not yet supported —
    /// kaish's lexer splits `-i.bak` at the dot; the trash snapshot under
    /// `set -o trash` already keeps a recoverable prior copy. See issues.md.)
    #[arg(short = 'i', long = "in-place")]
    in_place: bool,

    /// Confirmation nonce for a latch-gated in-place overwrite.
    #[arg(long = "confirm")]
    confirm: Option<String>,

    #[command(flatten)]
    global: GlobalFlags,

    /// Expression to execute (when `-e` is not used), followed by file paths.
    args: Vec<String>,
}

#[async_trait]
impl Tool for Sed {
    fn name(&self) -> &str {
        "sed"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &SedArgs::command(),
            "sed",
            "Stream editor for filtering and transforming text",
            [
                ("Basic substitution", "sed 's/old/new/' file.txt"),
                ("Global substitution", "sed 's/old/new/g' file.txt"),
                ("Replace the 2nd match only", "sed 's/x/Y/2' file.txt"),
                ("Case-insensitive", "sed 's/hello/hi/gi' file.txt"),
                ("Delete lines matching pattern", "sed '/error/d' log.txt"),
                ("Print only matching lines", "sed -n '/pattern/p' file.txt"),
                ("Multiple commands (;)", "sed 's/a/b/; s/c/d/' file.txt"),
                ("Multiple expressions (-e)", "sed -e 's/a/b/' -e 's/c/d/' file.txt"),
                ("Append a line after matches", "sed '/ERROR/a ---' log.txt"),
                ("Insert a line at the top", "sed '1i #!/bin/sh' script.sh"),
                ("Change matching lines", "sed '/old/c replaced' file.txt"),
                ("Transliterate characters", "sed 'y/abc/xyz/' file.txt"),
                ("Line range", "sed '2,5d' file.txt"),
                ("Alternative delimiter", "sed 's|/usr|/opt|g' file.txt"),
                ("Capture groups (ERE)", "sed 's/(\\w+) (\\w+)/\\2 \\1/' file.txt"),
                ("Alternation (ERE or GNU BRE)", r"sed 's/cat\|dog/pet/g' file.txt"),
            ],
        )
    }

    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        let mut issues = validate_against_schema(args, &self.schema());

        // Collect expressions the same way execute() does: from -e flags or the
        // first positional. Any `<dynamic>` marker means a variable/substitution
        // whose value is unknown at parse time — skip the whole expression so we
        // don't false-error on `F='s/a/b/'; sed $F`.
        // A non-string `-e` is a runtime error; execute() surfaces it loudly.
        // Validation stays lenient here (don't double-author the message).
        let Ok(exprs) = collect_expressions(args) else {
            return issues;
        };
        for expr in &exprs {
            if expr.contains("<dynamic>") {
                return issues;
            }
        }

        // Nothing to validate if expressions are absent (execute() will also
        // reject this case at runtime with "missing expression").
        // `-E`/`-r` selects strict ERE; default is the BRE-superset dialect.
        let extended =
            args.has_flag("E") || args.has_flag("r") || args.has_flag("regexp-extended");
        for expr in &exprs {
            if let Err(msg) = parse_program_dialect(expr, extended) {
                issues.push(
                    ValidationIssue::error(
                        IssueCode::InvalidSedExpr,
                        format!("sed: {msg}"),
                    )
                    .with_suggestion(
                        "commands: s/pat/rep/[gipN], y/abc/xyz/, d, p, q, a/i/c TEXT; \
                         chain with ; or -e; addresses: N, $, /re/, N,M; regex is ERE \
                         (egrep-style; GNU BRE \\| \\(…\\) \\{N,M\\} also accepted)",
                    ),
                );
            }
        }

        issues
    }

    async fn execute(&self, mut args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        // A structured call (`{"in-place": true}`) binds a bool into args.named,
        // which to_argv renders as `--in-place=true` — and clap's SetTrue rejects
        // a value. Move bool-schema named entries into flags so they render bare.
        // (`-i.bak` arrives as named["i"]=".bak", a string, so it's left to be
        // rejected by clap as the unsupported glued-suffix form, as intended.)
        args.flagify_bool_named(&self.schema());

        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("sed: {e}")),
        };
        let parsed = match SedArgs::try_parse_from(
            std::iter::once("sed".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("sed: {e}")),
        };
        parsed.global.apply(ctx);

        let quiet = parsed.quiet || args.has_flag("quiet") || args.has_flag("n");

        // In-place editing (-i [SUFFIX]) and its confirm nonce — captured before
        // `parsed` is shadowed by the built program below. Read raw too: the
        // kernel may surface `-i.bak` as named["i"]=".bak" or bare `-i` as a flag.
        let in_place = parsed.in_place || args.has_flag("i");
        let confirm = parsed.confirm.clone();
        // Regex dialect: `-E`/`-r` is strict ERE; default rewrites GNU BRE metas.
        // Captured before `parsed` is shadowed by the built program below.
        let extended = parsed.extended;

        // Collect expressions from the *raw* args, not `parsed.expression`: the
        // kernel accumulates repeated `-e` into a `Value::Json(Array)`, which
        // `ToolArgs::to_argv()` renders as one JSON token (it can't tell a
        // repeatable scalar array from a single array value). `parsed` above is
        // only consulted for `-n`/global flags; expressions live here.
        let expressions = match collect_expressions(&args) {
            Ok(e) => e,
            Err(msg) => return ExecResult::failure(2, format!("sed: {msg}")),
        };
        if expressions.is_empty() {
            return ExecResult::failure(1, "sed: missing expression");
        }

        // Parse all expressions upfront (fail early). Each expression string may
        // itself hold several `;`-separated commands, so flatten into one
        // ordered program (matching `-e A -e B` and `'A; B'` semantics).
        let mut parsed: Vec<SedExpression> = Vec::new();
        for expr in &expressions {
            match parse_program_dialect(expr, extended) {
                Ok(cmds) => parsed.extend(cmds),
                Err(e) => return ExecResult::failure(1, format!("sed: {}", e)),
            }
        }

        // When `-e` supplied the expression(s), every positional is a file (file
        // at position 0); otherwise the first positional is the expression and
        // the file is at position 1.
        let file_pos = if expression_from_flag(&args) { 0 } else { 1 };

        // In-place: edit each file operand on disk instead of streaming to
        // stdout. It is *always* a truncating overwrite of an existing file, so
        // it routes through the same latch+trash gate as tee/patch. Editing a
        // stream in place is meaningless, so no operands is a loud error.
        if in_place {
            let operands = args.positional.get(file_pos..).unwrap_or(&[]);
            let files: Vec<String> = match crate::interpreter::values_to_text_sink_named(operands, "a path") {
                Ok(f) => f,
                Err(e) => return ExecResult::failure(1, format!("sed: {e}")),
            };
            if files.is_empty() {
                return ExecResult::failure(
                    1,
                    "sed: -i requires file operands (cannot edit a stream in place)",
                );
            }

            // Gate every target with one nonce before touching any file. The
            // latch hint must reinject the flags the operation can't run without:
            // a bare `sed --confirm=… file` would read `file` as the expression
            // and then hang on stdin. Rebuild `-i [-n] -e '<expr>'…` so the
            // advertised re-run actually does the in-place edit.
            let mut hint_prefix = String::from("sed -i");
            if quiet {
                hint_prefix.push_str(" -n");
            }
            for expr in &expressions {
                let escaped = expr.replace('\'', r"'\''");
                hint_prefix.push_str(&format!(" -e '{escaped}'"));
            }
            let targets: Vec<(String, bool)> = files.iter().map(|f| (f.clone(), false)).collect();
            if let Err(blocked) = ctx
                .gate_overwrites("sed", &targets, confirm.as_deref(), |nonce, joined| {
                    format!("{hint_prefix} --confirm=\"{nonce}\" {joined}")
                })
                .await
            {
                return blocked;
            }

            // Apply per file; continue past per-file errors but report every one
            // so a multi-file failure isn't masked down to just the last.
            let mut errors: Vec<String> = Vec::new();
            for path in &files {
                let resolved = ctx.resolve_path(path);
                let target = Path::new(&resolved);
                let content = match ctx.backend.read(target, None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            errors.push(format!("sed: {}: invalid UTF-8", path));
                            continue;
                        }
                    },
                    Err(e) => {
                        errors.push(format!("sed: {}: {}", path, e));
                        continue;
                    }
                };
                let output = execute_sed(&content, &parsed, quiet);
                // Whole-file compare-and-swap, matching patch: the `expected`
                // makes a concurrent change between read and write a loud
                // Conflict, never a silent clobber.
                let ops = vec![PatchOp::Replace {
                    offset: 0,
                    len: content.len(),
                    content: output,
                    expected: Some(content.clone()),
                }];
                if let Err(e) = ctx.backend.patch(target, &ops).await {
                    errors.push(format!("sed: {}: {}", path, e));
                }
            }
            return if errors.is_empty() {
                ExecResult::success("")
            } else {
                ExecResult::failure(1, errors.join("\n"))
            };
        }

        // Streaming mode: read one file (or stdin) and write to stdout. A
        // binary `path` operand goes loud rather than silently falling
        // through to the stdin branch below.
        let input = match get_path_string(&args, "path", file_pos) {
            Ok(Some(path)) => {
                let resolved = ctx.resolve_path(&path);
                match ctx.backend.read(Path::new(&resolved), None).await {
                    Ok(data) => match String::from_utf8(data) {
                        Ok(s) => s,
                        Err(_) => {
                            return ExecResult::failure(1, format!("sed: {}: invalid UTF-8", path))
                        }
                    },
                    Err(e) => return ExecResult::failure(1, format!("sed: {}: {}", path, e)),
                }
            }
            Ok(None) => match ctx.read_stdin_to_text().await {
                Ok(s) => s.unwrap_or_default(),
                Err(e) => return ExecResult::failure(2, format!("sed: {e}")),
            },
            Err(e) => return ExecResult::failure(1, format!("sed: {e}")),
        };

        // Execute
        let output = execute_sed(&input, &parsed, quiet);
        ExecResult::with_output(OutputData::text(output))
    }
}

/// True when the expression(s) came from `-e`/`--expression` flags rather than
/// the first positional. The kernel binds repeated `-e` under the canonical
/// `expression` key (single `-e` may also land under the `e` alias on the sync
/// path), so either key's presence means "flag form".
fn expression_from_flag(args: &ToolArgs) -> bool {
    args.named.contains_key("expression") || args.named.contains_key("e")
}

/// Collect all expressions from args (supports multiple `-e` flags).
///
/// Repeated `-e` flags are accumulated by the kernel into a
/// `Value::Json(Array)` under the canonical `expression` key (see
/// `consume_flag_positionals`); a single value may arrive as a bare
/// `Value::String`. When no `-e` is used, the first positional is the
/// expression. Order is preserved so `-e A -e B` applies A then B.
fn collect_expressions(args: &ToolArgs) -> Result<Vec<String>, String> {
    let mut exprs = Vec::new();

    // Both paths canonicalize `-e`/`--expression` to the long name `expression`,
    // so `e` is never actually populated today; it's defensive insurance against
    // a future change that binds under the short alias. Because only one key is
    // ever present, iterating both can't double-count or reorder.
    //
    // A non-string `-e` value (e.g. `sed -e 5`, which binds `5` as an integer)
    // is rejected loudly rather than silently dropped — `-e <number>` isn't a
    // valid sed program, and silently ignoring it once led to the *filename*
    // being parsed as the program.
    for key in ["expression", "e"] {
        match args.named.get(key) {
            Some(Value::Json(serde_json::Value::Array(items))) => {
                for item in items {
                    match item {
                        serde_json::Value::String(s) => exprs.push(s.clone()),
                        other => return Err(format!(
                            "-e expression must be a string, got `{other}`"
                        )),
                    }
                }
            }
            Some(Value::String(e)) => exprs.push(e.clone()),
            Some(other) => {
                return Err(format!("-e expression must be a string, got `{other:?}`"));
            }
            None => {}
        }
    }

    // First positional is the expression when no -e flag was used.
    if exprs.is_empty() && let Some(Value::String(e)) = args.positional.first() {
        exprs.push(e.clone());
    }

    Ok(exprs)
}

// ============================================================================
// Data Structures
// ============================================================================

/// Address specifies which lines a command applies to.
#[derive(Debug, Clone)]
enum Address {
    /// Apply to all lines (no address specified).
    All,
    /// Single line number (1-indexed).
    Line(usize),
    /// Last line of input.
    LastLine,
    /// Range from start to end (inclusive).
    Range(Box<Address>, Box<Address>),
    /// Lines matching a regex pattern.
    Pattern(Regex),
}

/// A sed command to execute.
#[derive(Debug, Clone)]
enum Command {
    /// Substitute pattern with replacement.
    Substitute {
        pattern: Regex,
        replacement: String,
        /// Replace every match at/after `occurrence` (the `g` flag).
        global: bool,
        /// 1-indexed first match to act on (the `s///N` flag). `0` means
        /// "unspecified" and behaves as `1` (replace the first match).
        occurrence: usize,
        /// Print the pattern space after a successful substitution (`p` flag).
        print: bool,
    },
    /// Delete the pattern space.
    Delete,
    /// Print the pattern space.
    Print,
    /// Quit processing.
    Quit,
    /// Append text after the current line (`a`).
    Append(String),
    /// Insert text before the current line (`i`).
    Insert(String),
    /// Change: replace the matched line(s) with text (`c`).
    Change(String),
    /// Transliterate `from` chars to `to` chars, 1:1 (`y/from/to/`).
    Transliterate { from: Vec<char>, to: Vec<char> },
}

/// A complete sed expression: address + command.
#[derive(Debug, Clone)]
struct SedExpression {
    address: Address,
    command: Command,
}

// ============================================================================
// Parser
// ============================================================================

/// Parse a sed program: one or more `;`/newline-separated commands, each an
/// optional address + command. `-e EXPR` strings and `'A; B'` both arrive here,
/// so multi-command programs collapse to one ordered `Vec` either way.
///
/// `;` is only a separator at the top level. Inside `s///`, `y///`, or a
/// `/regex/` address it's an ordinary character (those parsers consume their own
/// delimiters), and `a`/`i`/`c` swallow the rest of the program as literal text
/// — matching GNU sed's one-line `a TEXT` behavior.
/// Default (BRE-superset) program parse, used by the unit tests. Production
/// routes through [`parse_program_dialect`] with the caller's `-E`/`-r` state.
#[cfg(test)]
fn parse_program(expr: &str) -> Result<Vec<SedExpression>, String> {
    parse_program_dialect(expr, false)
}

/// Parse a full sed program. `extended` selects the regex dialect: `false`
/// (default) rewrites the GNU BRE backslash-metas to ERE so `\|`/`\(…\)`/`\{N\}`
/// behave as operators; `true` (`-E`/`-r`) is strict ERE, where those escapes
/// match the literal character.
fn parse_program_dialect(expr: &str, extended: bool) -> Result<Vec<SedExpression>, String> {
    let mut out = Vec::new();
    let mut rest = expr.to_string();

    loop {
        let next = {
            let trimmed = rest.trim_start_matches([';', '\n', ' ', '\t']);
            if trimmed.is_empty() {
                break;
            }
            let (expr, remaining) = parse_one(trimmed, extended)?;
            out.push(expr);
            remaining
        };
        rest = next;
    }

    if out.is_empty() {
        return Err("empty expression".to_string());
    }
    Ok(out)
}

/// Parse a single address+command, returning it and the unconsumed remainder.
fn parse_one(expr: &str, extended: bool) -> Result<(SedExpression, String), String> {
    let (address, rest) = parse_address(expr, extended)?;
    let (command, rest) = parse_command(rest.trim_start(), extended)?;
    Ok((SedExpression { address, command }, rest))
}

/// Parse an optional address prefix, returning (Address, remaining).
fn parse_address(expr: &str, extended: bool) -> Result<(Address, &str), String> {
    let expr = expr.trim();

    if expr.is_empty() {
        return Ok((Address::All, ""));
    }

    // Check for pattern address: /regex/
    if expr.starts_with('/') {
        let (pattern, rest) = parse_pattern_address(expr, extended)?;
        // Check for range
        if let Some(after_comma) = rest.strip_prefix(',') {
            let (end_addr, final_rest) = parse_address(after_comma, extended)?;
            return Ok((
                Address::Range(Box::new(Address::Pattern(pattern)), Box::new(end_addr)),
                final_rest,
            ));
        }
        return Ok((Address::Pattern(pattern), rest));
    }

    // Check for $ (last line)
    if let Some(rest) = expr.strip_prefix('$') {
        if let Some(after_comma) = rest.strip_prefix(',') {
            let (end_addr, final_rest) = parse_address(after_comma, extended)?;
            return Ok((
                Address::Range(Box::new(Address::LastLine), Box::new(end_addr)),
                final_rest,
            ));
        }
        return Ok((Address::LastLine, rest));
    }

    // Check for line number
    if expr.chars().next().is_some_and(|c| c.is_ascii_digit()) {
        let num_end = expr
            .find(|c: char| !c.is_ascii_digit())
            .unwrap_or(expr.len());
        let num: usize = expr[..num_end]
            .parse()
            .map_err(|_| "invalid line number")?;
        let rest = &expr[num_end..];

        // Check for range
        if let Some(after_comma) = rest.strip_prefix(',') {
            let (end_addr, final_rest) = parse_address(after_comma, extended)?;
            return Ok((
                Address::Range(Box::new(Address::Line(num)), Box::new(end_addr)),
                final_rest,
            ));
        }
        return Ok((Address::Line(num), rest));
    }

    // No address, return all
    Ok((Address::All, expr))
}

/// Parse a /pattern/ address, returning the compiled regex and remaining input.
fn parse_pattern_address(expr: &str, extended: bool) -> Result<(Regex, &str), String> {
    debug_assert!(expr.starts_with('/'));

    let chars: Vec<char> = expr.chars().collect();
    let mut i = 1; // Skip opening /
    let mut pattern = String::new();

    while i < chars.len() {
        let c = chars[i];
        if c == '\\' && i + 1 < chars.len() {
            // Escaped character
            let next = chars[i + 1];
            if next == '/' {
                pattern.push('/');
            } else {
                pattern.push('\\');
                pattern.push(next);
            }
            i += 2;
        } else if c == '/' {
            // End of pattern
            i += 1;
            break;
        } else {
            pattern.push(c);
            i += 1;
        }
    }

    // Default mode rewrites GNU BRE metas to ERE; `-E`/`-r` leaves them literal.
    let rewritten = if extended { pattern.clone() } else { bre_metas_to_ere(&pattern) };
    let rewrote = rewritten != pattern;
    let regex = compile_pattern(&rewritten, false, false)
        .map_err(|e| append_dialect_hint(e, rewrote, Some("-E/-r")))?;

    // Calculate byte offset from char offset
    let consumed: usize = chars[..i].iter().map(|c| c.len_utf8()).sum();
    Ok((regex, &expr[consumed..]))
}

/// Parse a sed command and return it plus the unconsumed remainder of the
/// program (everything after this command's text/flags, including a leading
/// `;`/newline separator if present).
fn parse_command(cmd: &str, extended: bool) -> Result<(Command, String), String> {
    // Safe: `parse_one` trims then checks non-empty before calling us, but guard
    // anyway so a bare `;;` can't panic.
    let Some(first) = cmd.chars().next() else {
        return Err("missing command".to_string());
    };
    let after_first = &cmd[first.len_utf8()..];

    match first {
        's' => parse_substitute(after_first, extended),
        'y' => parse_transliterate(after_first),
        'd' => Ok((Command::Delete, after_first.to_string())),
        'p' => Ok((Command::Print, after_first.to_string())),
        'q' => Ok((Command::Quit, after_first.to_string())),
        // a/i/c take the rest of the program as literal text (GNU one-line form).
        'a' => Ok((Command::Append(parse_text_arg(after_first)), String::new())),
        'i' => Ok((Command::Insert(parse_text_arg(after_first)), String::new())),
        'c' => Ok((Command::Change(parse_text_arg(after_first)), String::new())),
        _ => Err(format!("unknown command: {}", first)),
    }
}

/// Strip the optional leading `\` and one optional space from the text of an
/// `a`/`i`/`c` command. Accepts every form models reach for: `a\text`,
/// `a text`, and `atext` (GNU one-liner).
fn parse_text_arg(s: &str) -> String {
    let s = s.strip_prefix('\\').unwrap_or(s);
    s.strip_prefix(' ').unwrap_or(s).to_string()
}

/// Parse a substitution command `s/pattern/replacement/flags`, returning the
/// command and the unconsumed remainder. Supported flags: `g` (global),
/// `i`/`I` (case-insensitive), `p` (print), `m`/`M` (multiline anchors), and a
/// numeric `N` (act on the Nth match; combine with `g` for "Nth onward").
fn parse_substitute(expr: &str, extended: bool) -> Result<(Command, String), String> {
    let chars: Vec<char> = expr.chars().collect();
    if chars.is_empty() {
        return Err("s command requires delimiter".to_string());
    }
    let delimiter = chars[0];

    let (pattern_str, after_pattern) = parse_delimited(&chars[1..], delimiter)?;
    let (replacement, after_replacement) = parse_delimited(after_pattern, delimiter)?;

    let mut global = false;
    let mut case_insensitive = false;
    let mut multiline = false;
    let mut print = false;
    let mut digits = String::new();
    let mut idx = 0;
    while idx < after_replacement.len() {
        match after_replacement[idx] {
            'g' => global = true,
            'i' | 'I' => case_insensitive = true,
            'm' | 'M' => multiline = true,
            'p' => print = true,
            c if c.is_ascii_digit() => digits.push(c),
            // A separator (or whitespace) ends this command's flags.
            ';' | '\n' | ' ' | '\t' => break,
            other => return Err(format!("unknown s flag: {}", other)),
        }
        idx += 1;
    }
    let occurrence = if digits.is_empty() {
        0
    } else {
        let n = digits
            .parse()
            .map_err(|_| "invalid s/// occurrence number")?;
        // `s///0` is meaningless ("replace the 0th match") — GNU sed rejects it
        // rather than silently treating it as the first match.
        if n == 0 {
            return Err("number option to `s' command may not be zero".to_string());
        }
        n
    };

    // Default mode rewrites GNU BRE metas to ERE (so `s/\(a\)\(b\)/\2\1/` works
    // as capture groups); `-E`/`-r` leaves the escapes literal.
    let (pattern_str, rewrote) = if extended {
        (pattern_str, false)
    } else {
        let rewritten = bre_metas_to_ere(&pattern_str);
        let rewrote = rewritten != pattern_str;
        (rewritten, rewrote)
    };
    let regex = compile_pattern(&pattern_str, case_insensitive, multiline)
        .map_err(|e| append_dialect_hint(e, rewrote, Some("-E/-r")))?;

    let rest: String = after_replacement[idx..].iter().collect();
    Ok((
        Command::Substitute {
            pattern: regex,
            replacement,
            global,
            occurrence,
            print,
        },
        rest,
    ))
}

/// Parse a transliterate command `y/from/to/`, returning the command and the
/// unconsumed remainder. `from` and `to` must be equal length (real sed errors
/// otherwise).
fn parse_transliterate(expr: &str) -> Result<(Command, String), String> {
    let chars: Vec<char> = expr.chars().collect();
    if chars.is_empty() {
        return Err("y command requires delimiter".to_string());
    }
    let delimiter = chars[0];

    let (from, after_from) = parse_delimited(&chars[1..], delimiter)?;
    let (to, after_to) = parse_delimited(after_from, delimiter)?;

    let from: Vec<char> = from.chars().collect();
    let to: Vec<char> = to.chars().collect();
    if from.len() != to.len() {
        return Err("y command: 'from' and 'to' must have the same length".to_string());
    }

    let rest: String = after_to.iter().collect();
    Ok((Command::Transliterate { from, to }, rest))
}

/// Compile a sed pattern as ERE (the regex crate's native dialect), turning the
/// one regex-crate limitation we care about — pattern-side backreferences — into
/// a sed-specific message instead of the engine's raw "regex parse error". kaish
/// sed is *always* ERE, and the linear-time engine has no backreferences in any
/// dialect, so `s/(a)\1/…/` can't work here regardless of `-E`.
fn compile_pattern(pattern: &str, case_insensitive: bool, multiline: bool) -> Result<Regex, String> {
    RegexBuilder::new(pattern)
        .case_insensitive(case_insensitive)
        .multi_line(multiline)
        .build()
        .map_err(|e| {
            if e.to_string().contains("backreferences are not supported") {
                "pattern uses a backreference (\\1-\\9); kaish sed regex is ERE on a \
                 linear-time engine that can't backreference in the pattern — match \
                 the text directly, or split the work across commands"
                    .to_string()
            } else {
                format!("invalid pattern: {e}")
            }
        })
}


/// Parse a delimited section, handling escapes.
fn parse_delimited(chars: &[char], delimiter: char) -> Result<(String, &[char]), String> {
    let mut result = String::new();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];
        if c == '\\' && i + 1 < chars.len() {
            let next = chars[i + 1];
            if next == delimiter {
                result.push(delimiter);
                i += 2;
            } else {
                // Keep the backslash for regex or replacement escapes
                result.push('\\');
                result.push(next);
                i += 2;
            }
        } else if c == delimiter {
            return Ok((result, &chars[i + 1..]));
        } else {
            result.push(c);
            i += 1;
        }
    }

    Err("unterminated expression".to_string())
}

// ============================================================================
// Execution
// ============================================================================

/// Execute sed expressions on input text.
fn execute_sed(input: &str, expressions: &[SedExpression], quiet: bool) -> String {
    let lines: Vec<&str> = input.lines().collect();
    let total_lines = lines.len();
    let mut output = String::new();
    let mut range_active: Vec<bool> = vec![false; expressions.len()];

    for (line_num, line) in lines.iter().enumerate() {
        let one_indexed = line_num + 1;
        let is_last = line_num + 1 == total_lines;

        let mut pattern_space = line.to_string();
        let mut deleted = false;
        let mut printed_by_p = false;
        let mut quit = false;
        // `a` text is emitted *after* the line's auto-print; collect it here so
        // multiple `a` commands queue in order.
        let mut appends: Vec<&str> = Vec::new();

        for (expr_idx, expr) in expressions.iter().enumerate() {
            let addr = address_matches(
                &expr.address,
                one_indexed,
                is_last,
                &pattern_space,
                &mut range_active[expr_idx],
            );

            if !addr.matched {
                continue;
            }

            match &expr.command {
                Command::Substitute {
                    pattern,
                    replacement,
                    global,
                    occurrence,
                    print,
                } => {
                    let new_text =
                        substitute(pattern, &pattern_space, replacement, *global, *occurrence);

                    let changed = new_text != pattern_space;
                    pattern_space = new_text;

                    if *print && changed {
                        output.push_str(&pattern_space);
                        output.push('\n');
                        printed_by_p = true;
                    }
                }
                Command::Delete => {
                    deleted = true;
                    break;
                }
                Command::Print => {
                    output.push_str(&pattern_space);
                    output.push('\n');
                    printed_by_p = true;
                }
                Command::Quit => {
                    quit = true;
                    break;
                }
                // `a`/`i`/`c` text is emitted regardless of `-n` (real sed prints
                // it unconditionally).
                Command::Append(text) => {
                    appends.push(text);
                }
                Command::Insert(text) => {
                    output.push_str(text);
                    output.push('\n');
                }
                Command::Change(text) => {
                    // The matched line(s) are suppressed; the text is emitted once
                    // at the *end* of the selection. For a range that's the closing
                    // line (or EOF if it never closes); for a single line/pattern
                    // every match is its own one-line selection. `range_end`
                    // (computed in `address_matches`) collapses all those cases.
                    deleted = true;
                    if addr.range_end {
                        output.push_str(text);
                        output.push('\n');
                    }
                }
                Command::Transliterate { from, to } => {
                    pattern_space = transliterate(&pattern_space, from, to);
                }
            }
        }

        // Auto-print unless quiet or deleted, then flush any queued `a` text.
        if !deleted && !quiet && !printed_by_p {
            output.push_str(&pattern_space);
            output.push('\n');
        }
        for text in appends {
            output.push_str(text);
            output.push('\n');
        }

        if quit {
            break;
        }
    }

    output
}

/// Transliterate each char of `text` that appears in `from` to the char at the
/// same index in `to` (`y/from/to/`). Callers guarantee `from.len() == to.len()`.
fn transliterate(text: &str, from: &[char], to: &[char]) -> String {
    text.chars()
        .map(|c| {
            from.iter()
                .position(|&f| f == c)
                .and_then(|i| to.get(i).copied())
                .unwrap_or(c)
        })
        .collect()
}

/// Outcome of testing one address against the current line.
struct AddressMatch {
    /// The address selects this line — the command should run.
    matched: bool,
    /// This is the *final* line the address selects: a single line/pattern match,
    /// or the line a range closes on — including a range that opens and closes on
    /// the same line, and one still open when it reaches EOF. The `c` (Change)
    /// command emits its text once keyed off this flag, so a whole range collapses
    /// to a single replacement.
    range_end: bool,
}

/// Check whether an address matches the current line, and whether this is the
/// last line the address selects (see [`AddressMatch::range_end`]).
fn address_matches(
    addr: &Address,
    line_num: usize,
    is_last: bool,
    pattern_space: &str,
    range_active: &mut bool,
) -> AddressMatch {
    // Single-line forms: every match is its own one-line selection.
    let single = |m: bool| AddressMatch {
        matched: m,
        range_end: m,
    };
    match addr {
        Address::All => single(true),
        Address::Line(n) => single(line_num == *n),
        Address::LastLine => single(is_last),
        Address::Pattern(regex) => single(regex.is_match(pattern_space)),
        Address::Range(start, end) => {
            if *range_active {
                let end_matches = match end.as_ref() {
                    Address::Line(n) => line_num >= *n,
                    Address::LastLine => is_last,
                    Address::Pattern(regex) => regex.is_match(pattern_space),
                    _ => false,
                };
                if end_matches {
                    *range_active = false;
                    AddressMatch {
                        matched: true,
                        range_end: true,
                    }
                } else {
                    // Still inside the range — but if this is the last line the
                    // range ends here (an unclosed range terminates at EOF).
                    AddressMatch {
                        matched: true,
                        range_end: is_last,
                    }
                }
            } else {
                let start_matches = match start.as_ref() {
                    Address::Line(n) => line_num == *n,
                    Address::LastLine => is_last,
                    Address::Pattern(regex) => regex.is_match(pattern_space),
                    _ => false,
                };
                if !start_matches {
                    return AddressMatch {
                        matched: false,
                        range_end: false,
                    };
                }
                // GNU sed: a numeric end address at or before the start line
                // collapses the range to a single line (it closes immediately).
                // A regex/last-line end is only tested on *subsequent* lines, so
                // the range stays open past the start line.
                let close_same_line = matches!(end.as_ref(), Address::Line(n) if *n <= line_num);
                if close_same_line {
                    AddressMatch {
                        matched: true,
                        range_end: true,
                    }
                } else {
                    *range_active = true;
                    // A range that opens on the final line also ends there.
                    AddressMatch {
                        matched: true,
                        range_end: is_last,
                    }
                }
            }
        }
    }
}

/// Substitute matches, handling capture groups, the `g` (global) flag, and the
/// `s///N` occurrence count.
///
/// - `occurrence` is the 1-indexed first match to act on (`0` behaves as `1`).
/// - `global` then extends the action to every match at/after that point.
///
/// So `(occurrence=0|1, global=false)` replaces the first match; `g` replaces
/// all; `N` replaces only the Nth; `Ng` replaces the Nth and everything after.
fn substitute(
    pattern: &Regex,
    text: &str,
    replacement: &str,
    global: bool,
    occurrence: usize,
) -> String {
    let skip = occurrence.saturating_sub(1);
    let mut result = String::new();
    let mut last_end = 0;
    let mut seen = 0;
    let mut replaced = false;

    for captures in pattern.captures_iter(text) {
        // Capture group 0 (the full match) is always present when captures succeeds.
        let Some(mat) = captures.get(0) else {
            continue;
        };
        // Skip matches before the requested occurrence; once at/after it, replace
        // either just the first (non-global) or all remaining (global).
        let do_replace = seen >= skip && (global || !replaced);
        seen += 1;
        if do_replace {
            result.push_str(&text[last_end..mat.start()]);
            result.push_str(&expand_replacement(replacement, &captures));
            last_end = mat.end();
            replaced = true;
        }
    }

    result.push_str(&text[last_end..]);
    result
}

/// Expand replacement string with capture groups (\1-\9, &).
fn expand_replacement(replacement: &str, captures: &regex::Captures) -> String {
    let mut result = String::new();
    let chars: Vec<char> = replacement.chars().collect();
    let mut i = 0;

    while i < chars.len() {
        let c = chars[i];
        if c == '\\' && i + 1 < chars.len() {
            let next = chars[i + 1];
            if next.is_ascii_digit() {
                let group_num = (next as u8 - b'0') as usize;
                if let Some(m) = captures.get(group_num) {
                    result.push_str(m.as_str());
                }
                i += 2;
            } else {
                // Other escapes: \n, \t, \\
                match next {
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    '\\' => result.push('\\'),
                    _ => {
                        result.push('\\');
                        result.push(next);
                    }
                }
                i += 2;
            }
        } else if c == '&' {
            // & expands to entire match
            if let Some(m) = captures.get(0) {
                result.push_str(m.as_str());
            }
            i += 1;
        } else {
            result.push(c);
            i += 1;
        }
    }

    result
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vfs::{Filesystem, MemoryFs, VfsRouter};
    use std::sync::Arc;

    /// Test helper: parse a single-command expression. Most parser tests predate
    /// `;`-separated programs and assert against one `SedExpression`; this keeps
    /// them terse by unwrapping the one-element program `parse_program` returns.
    fn parse_expression(expr: &str) -> Result<SedExpression, String> {
        let mut cmds = parse_program(expr)?;
        if cmds.len() != 1 {
            return Err(format!("expected 1 command, got {}", cmds.len()));
        }
        Ok(cmds.remove(0))
    }

    async fn make_ctx() -> ExecContext {
        let mut vfs = VfsRouter::new();
        let mem = MemoryFs::new();
        mem.write(
            Path::new("test.txt"),
            b"hello world\nHELLO WORLD\nfoo bar\nbaz",
        )
        .await
        .unwrap();
        mem.write(
            Path::new("lines.txt"),
            b"line one\nline two\nline three\nline four\nline five",
        )
        .await
        .unwrap();
        vfs.mount("/", mem);
        ExecContext::new(Arc::new(vfs))
    }

    // === Parser Tests ===

    #[test]
    fn test_parse_basic_substitute() {
        let expr = parse_expression("s/foo/bar/").unwrap();
        assert!(matches!(expr.address, Address::All));
        assert!(matches!(expr.command, Command::Substitute { .. }));
    }

    #[test]
    fn test_parse_substitute_flags() {
        let expr = parse_expression("s/foo/bar/gip").unwrap();
        if let Command::Substitute {
            global,
            print,
            pattern,
            ..
        } = &expr.command
        {
            assert!(*global);
            assert!(*print);
            // Case insensitive is baked into the regex
            assert!(pattern.is_match("FOO"));
        } else {
            panic!("expected Substitute");
        }
    }

    #[test]
    fn test_parse_alternative_delimiter() {
        let expr = parse_expression("s|/usr|/opt|").unwrap();
        if let Command::Substitute {
            pattern,
            replacement,
            ..
        } = &expr.command
        {
            assert!(pattern.is_match("/usr"));
            assert_eq!(replacement, "/opt");
        } else {
            panic!("expected Substitute");
        }
    }

    #[test]
    fn test_parse_escaped_delimiter() {
        let expr = parse_expression(r"s/foo\/bar/baz/").unwrap();
        if let Command::Substitute { pattern, .. } = &expr.command {
            assert!(pattern.is_match("foo/bar"));
        } else {
            panic!("expected Substitute");
        }
    }

    #[test]
    fn test_parse_line_address() {
        let expr = parse_expression("5d").unwrap();
        assert!(matches!(expr.address, Address::Line(5)));
        assert!(matches!(expr.command, Command::Delete));
    }

    #[test]
    fn test_parse_last_line_address() {
        let expr = parse_expression("$d").unwrap();
        assert!(matches!(expr.address, Address::LastLine));
    }

    #[test]
    fn test_parse_range_address() {
        let expr = parse_expression("1,5d").unwrap();
        if let Address::Range(start, end) = &expr.address {
            assert!(matches!(start.as_ref(), Address::Line(1)));
            assert!(matches!(end.as_ref(), Address::Line(5)));
        } else {
            panic!("expected Range");
        }
    }

    #[test]
    fn test_parse_range_to_last() {
        let expr = parse_expression("3,$d").unwrap();
        if let Address::Range(start, end) = &expr.address {
            assert!(matches!(start.as_ref(), Address::Line(3)));
            assert!(matches!(end.as_ref(), Address::LastLine));
        } else {
            panic!("expected Range");
        }
    }

    #[test]
    fn test_parse_pattern_address() {
        let expr = parse_expression("/error/d").unwrap();
        assert!(matches!(expr.address, Address::Pattern(_)));
        assert!(matches!(expr.command, Command::Delete));
    }

    #[test]
    fn test_parse_pattern_range() {
        let expr = parse_expression("/start/,/end/d").unwrap();
        if let Address::Range(start, end) = &expr.address {
            assert!(matches!(start.as_ref(), Address::Pattern(_)));
            assert!(matches!(end.as_ref(), Address::Pattern(_)));
        } else {
            panic!("expected Range");
        }
    }

    #[test]
    fn test_parse_delete_command() {
        let expr = parse_expression("d").unwrap();
        assert!(matches!(expr.command, Command::Delete));
    }

    #[test]
    fn test_parse_print_command() {
        let expr = parse_expression("p").unwrap();
        assert!(matches!(expr.command, Command::Print));
    }

    #[test]
    fn test_parse_quit_command() {
        let expr = parse_expression("q").unwrap();
        assert!(matches!(expr.command, Command::Quit));
    }

    #[test]
    fn test_parse_invalid_regex() {
        let result = parse_expression("s/[invalid/bar/");
        assert!(result.is_err());
    }

    // === Execution Tests ===

    #[test]
    fn test_basic_substitution() {
        let input = "hello world\nhello there";
        let expr = parse_expression("s/hello/hi/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "hi world\nhi there\n");
    }

    #[test]
    fn test_global_substitution() {
        let input = "foo bar foo baz foo";
        let expr = parse_expression("s/foo/XXX/g").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "XXX bar XXX baz XXX\n");
    }

    #[test]
    fn test_case_insensitive() {
        let input = "Hello HELLO hello";
        let expr = parse_expression("s/hello/hi/gi").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "hi hi hi\n");
    }

    #[test]
    fn test_print_on_change() {
        let input = "hello world\nfoo bar";
        let expr = parse_expression("s/hello/hi/p").unwrap();
        let output = execute_sed(input, &[expr], true); // quiet mode
        assert_eq!(output, "hi world\n");
    }

    #[test]
    fn test_capture_groups() {
        let input = "John Smith";
        let expr = parse_expression(r"s/(\w+) (\w+)/\2, \1/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "Smith, John\n");
    }

    #[test]
    fn test_ampersand_expansion() {
        let input = "hello world";
        let expr = parse_expression("s/hello/[&]/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "[hello] world\n");
    }

    #[test]
    fn test_delete_command() {
        let input = "keep\ndelete this\nkeep";
        let expr = parse_expression("/delete/d").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "keep\nkeep\n");
    }

    #[test]
    fn test_quiet_mode() {
        let input = "line 1\npattern here\nline 3";
        let expr = parse_expression("/pattern/p").unwrap();
        let output = execute_sed(input, &[expr], true);
        assert_eq!(output, "pattern here\n");
    }

    #[test]
    fn test_line_number_address() {
        let input = "line 1\nline 2\nline 3";
        let expr = parse_expression("2s/line/LINE/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "line 1\nLINE 2\nline 3\n");
    }

    #[test]
    fn test_range_address() {
        let input = "line 1\nline 2\nline 3\nline 4";
        let expr = parse_expression("2,3d").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "line 1\nline 4\n");
    }

    #[test]
    fn test_quit_command() {
        let input = "line 1\nline 2\nline 3\nline 4";
        let expr = parse_expression("2q").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "line 1\nline 2\n");
    }

    #[test]
    fn test_multiple_expressions() {
        let input = "abc 123";
        let e1 = parse_expression("s/a/X/").unwrap();
        let e2 = parse_expression("s/1/Y/").unwrap();
        let output = execute_sed(input, &[e1, e2], false);
        assert_eq!(output, "Xbc Y23\n");
    }

    #[test]
    fn collect_expressions_reads_json_array_from_repeated_e() {
        // The kernel accumulates repeated `-e` into a Json array under the
        // canonical `expression` key. collect_expressions must read every
        // element, in order — the heart of the repeated-`-e` fix.
        let mut args = ToolArgs::new();
        args.named.insert(
            "expression".to_string(),
            Value::Json(serde_json::json!(["s/a/b/", "s/c/d/"])),
        );
        assert_eq!(
            collect_expressions(&args).unwrap(),
            vec!["s/a/b/".to_string(), "s/c/d/".to_string()]
        );
        assert!(expression_from_flag(&args));
    }

    #[test]
    fn collect_expressions_reads_single_string_e() {
        // A single `-e` may arrive as a bare String (e.g. the sync arg path).
        let mut args = ToolArgs::new();
        args.named
            .insert("expression".to_string(), Value::String("s/a/b/".into()));
        assert_eq!(collect_expressions(&args).unwrap(), vec!["s/a/b/".to_string()]);
        assert!(expression_from_flag(&args));
    }

    #[test]
    fn collect_expressions_falls_back_to_positional() {
        // No `-e`: the first positional is the expression.
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/a/b/".into()));
        args.positional.push(Value::String("file.txt".into()));
        assert_eq!(collect_expressions(&args).unwrap(), vec!["s/a/b/".to_string()]);
        assert!(!expression_from_flag(&args));
    }

    #[test]
    fn test_empty_replacement() {
        let input = "hello world";
        let expr = parse_expression("s/hello //").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "world\n");
    }

    #[test]
    fn test_empty_input() {
        let input = "";
        let expr = parse_expression("s/foo/bar/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert!(output.is_empty());
    }

    #[test]
    fn test_no_matches_passthrough() {
        let input = "hello world";
        let expr = parse_expression("s/xyz/abc/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "hello world\n");
    }

    #[test]
    fn test_last_line_address() {
        let input = "line 1\nline 2\nline 3";
        let expr = parse_expression("$s/line/LAST/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "line 1\nline 2\nLAST 3\n");
    }

    #[test]
    fn test_pattern_range() {
        let input = "before\nSTART\nmiddle\nEND\nafter";
        let expr = parse_expression("/START/,/END/d").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "before\nafter\n");
    }

    #[test]
    fn test_escaped_backslash_replacement() {
        let input = "hello";
        let expr = parse_expression(r"s/hello/a\\b/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "a\\b\n");
    }

    #[test]
    fn test_newline_in_replacement() {
        let input = "hello world";
        let expr = parse_expression(r"s/ /\n/").unwrap();
        let output = execute_sed(input, &[expr], false);
        assert_eq!(output, "hello\nworld\n");
    }

    // === `;` command separation ===

    #[test]
    fn semicolon_splits_into_multiple_commands() {
        let prog = parse_program("s/a/X/;s/b/Y/").unwrap();
        assert_eq!(prog.len(), 2);
        let output = execute_sed("abc", &prog, false);
        assert_eq!(output, "XYc\n");
    }

    #[test]
    fn semicolon_with_addresses_and_whitespace() {
        // `/x/d ; /y/d` (spaces around `;`) — two addressed deletes.
        let prog = parse_program("/b/d ; /d/d").unwrap();
        assert_eq!(prog.len(), 2);
        let output = execute_sed("a\nb\nc\nd\n", &prog, false);
        assert_eq!(output, "a\nc\n");
    }

    #[test]
    fn semicolon_inside_regex_is_literal_not_a_separator() {
        // A `;` inside the pattern must not split the command.
        let prog = parse_program("s/a;b/X/").unwrap();
        assert_eq!(prog.len(), 1);
        assert_eq!(execute_sed("a;b", &prog, false), "X\n");
    }

    #[test]
    fn empty_segments_from_doubled_semicolons_are_skipped() {
        let prog = parse_program(";;s/a/X/;;").unwrap();
        assert_eq!(prog.len(), 1);
    }

    // === s///N occurrence count ===

    #[test]
    fn substitute_nth_occurrence_only() {
        let expr = parse_expression("s/a/X/2").unwrap();
        assert_eq!(execute_sed("aaa", &[expr], false), "aXa\n");
    }

    #[test]
    fn substitute_nth_onward_with_g() {
        let expr = parse_expression("s/a/X/2g").unwrap();
        assert_eq!(execute_sed("aaaa", &[expr], false), "aXXX\n");
    }

    #[test]
    fn substitute_default_is_first_match() {
        let expr = parse_expression("s/a/X/").unwrap();
        assert_eq!(execute_sed("aaa", &[expr], false), "Xaa\n");
    }

    // === a / i / c ===

    #[test]
    fn append_emits_text_after_the_line() {
        let expr = parse_expression("/B/a ---").unwrap();
        assert_eq!(execute_sed("A\nB\nC", &[expr], false), "A\nB\n---\nC\n");
    }

    #[test]
    fn insert_emits_text_before_the_line() {
        let expr = parse_expression("1i top").unwrap();
        assert_eq!(execute_sed("A\nB", &[expr], false), "top\nA\nB\n");
    }

    #[test]
    fn change_replaces_single_line() {
        let expr = parse_expression("/B/c NEW").unwrap();
        assert_eq!(execute_sed("A\nB\nC", &[expr], false), "A\nNEW\nC\n");
    }

    #[test]
    fn change_replaces_whole_range_once() {
        let expr = parse_expression("2,3c NEW").unwrap();
        assert_eq!(execute_sed("A\nB\nC\nD", &[expr], false), "A\nNEW\nD\n");
    }

    #[test]
    fn change_range_unclosed_at_eof_still_emits_once() {
        // #1: a range whose end never matches must emit the change text once at
        // EOF, not silently delete to end-of-input with nothing in its place.
        let prog = parse_program("2,/NOPE/c NEW").unwrap();
        assert_eq!(execute_sed("a\nb\nc\nd", &prog, false), "a\nNEW\n");
    }

    #[test]
    fn change_numeric_range_past_eof_emits_once() {
        // #1: numeric end beyond the input length also closes at EOF.
        let prog = parse_program("2,99c NEW").unwrap();
        assert_eq!(execute_sed("a\nb\nc", &prog, false), "a\nNEW\n");
    }

    #[test]
    fn single_line_numeric_range_matches_one_line() {
        // #2: `N,N` must span exactly one line, not two (the old `>=`-on-next-line
        // close included the following line as well).
        let prog = parse_program("2,2d").unwrap();
        assert_eq!(execute_sed("a\nb\nc", &prog, false), "a\nc\n");
    }

    #[test]
    fn change_single_line_numeric_range_emits_once() {
        // #2 + #3: a single-line range opens and closes on the same line, so the
        // change text is emitted once for that one line.
        let prog = parse_program("2,2c NEW").unwrap();
        assert_eq!(execute_sed("a\nb\nc", &prog, false), "a\nNEW\nc\n");
    }

    #[test]
    fn descending_numeric_range_matches_only_start_line() {
        // GNU sed: a numeric end <= the start line collapses to the one start line.
        let prog = parse_program("3,1d").unwrap();
        assert_eq!(execute_sed("a\nb\nc\nd", &prog, false), "a\nb\nd\n");
    }

    #[test]
    fn append_text_emits_even_under_quiet() {
        // a/i/c print unconditionally, like real sed.
        let expr = parse_expression("/B/a ---").unwrap();
        assert_eq!(execute_sed("A\nB\nC", &[expr], true), "---\n");
    }

    #[test]
    fn parse_text_arg_accepts_backslash_space_and_glued_forms() {
        assert_eq!(parse_text_arg(r"\hello"), "hello");
        assert_eq!(parse_text_arg(" hello"), "hello");
        assert_eq!(parse_text_arg("hello"), "hello");
        assert_eq!(parse_text_arg(r"\ hello"), "hello");
    }

    // === y/// transliterate ===

    #[test]
    fn transliterate_maps_chars() {
        let expr = parse_expression("y/abc/xyz/").unwrap();
        assert_eq!(execute_sed("cabbage", &[expr], false), "zxyyxge\n");
    }

    #[test]
    fn transliterate_length_mismatch_errors() {
        let err = parse_program("y/abc/xy/").unwrap_err();
        assert!(err.contains("same length"), "got: {err}");
    }

    // === Regex dialect: default BRE-superset vs `-E`/`-r` strict ERE (#60) ===

    /// Build one expression in strict-ERE (`-E`/`-r`) mode for the tests below.
    fn parse_expression_ere(expr: &str) -> Result<SedExpression, String> {
        let mut cmds = parse_program_dialect(expr, true)?;
        if cmds.len() != 1 {
            return Err(format!("expected 1 command, got {}", cmds.len()));
        }
        Ok(cmds.remove(0))
    }

    #[test]
    fn bre_capture_groups_translate_to_ere_groups() {
        // Default mode: `\(...\)` become real capture groups, `\1`/`\2` backref
        // them — the agent-idiomatic spelling that used to be rejected.
        let expr = parse_expression(r"s/\(a\)\(b\)/\2\1/").unwrap();
        assert_eq!(execute_sed("ab", &[expr], false), "ba\n");
    }

    #[test]
    fn bre_alternation_translates() {
        // `cat\|dog` alternates (was a silent no-match / loud reject before #60).
        let expr = parse_expression(r"s/cat\|dog/X/g").unwrap();
        assert_eq!(execute_sed("cat dog", &[expr], false), "X X\n");
    }

    #[test]
    fn bre_interval_translates() {
        let expr = parse_expression(r"s/a\{2\}/X/").unwrap();
        assert_eq!(execute_sed("aa", &[expr], false), "X\n");
        let expr = parse_expression(r"s/a\{2,\}/X/").unwrap();
        assert_eq!(execute_sed("aaaa", &[expr], false), "X\n");
    }

    #[test]
    fn bre_plus_quantifier_translates() {
        // `\+` is now the one-or-more quantifier (GNU BRE), not a literal `+`.
        // The literal `+` is available as a bracket class `[+]`.
        let expr = parse_expression(r"s/a\+/X/").unwrap();
        assert_eq!(execute_sed("aaab", &[expr], false), "Xb\n");
    }

    #[test]
    fn bre_idioms_work_in_addresses_too() {
        // Addresses compile a pattern as well; the same rewrite applies.
        let expr = parse_expression(r"/cat\|dog/d").unwrap();
        assert_eq!(execute_sed("cat\nfish\ndog", &[expr], false), "fish\n");
    }

    #[test]
    fn ere_interval_and_alternation_are_fine() {
        // The bare ERE forms keep working — the rewrite is a superset.
        let expr = parse_expression("s/a{2}/X/").unwrap();
        assert_eq!(execute_sed("aa", &[expr], false), "X\n");
        let expr = parse_expression("s/cat|dog/X/g").unwrap();
        assert_eq!(execute_sed("cat dog", &[expr], false), "X X\n");
    }

    #[test]
    fn ere_groups_with_backref_work_normally() {
        let expr = parse_expression(r"s/(a)(b)/\2\1/").unwrap();
        assert_eq!(execute_sed("ab", &[expr], false), "ba\n");
    }

    #[test]
    fn ere_mode_treats_backslash_metas_as_literals() {
        // `-E`/`-r` is strict ERE: `\|` matches a literal pipe, `\(`/`\)` literal
        // parens — the escape hatch for matching those characters.
        let expr = parse_expression_ere(r"s/cat\|dog/X/").unwrap();
        assert_eq!(execute_sed("cat|dog here", &[expr], false), "X here\n");
        let expr = parse_expression_ere(r"s/\(x\)/Y/").unwrap();
        assert_eq!(execute_sed("(x)", &[expr], false), "Y\n");
    }

    #[test]
    fn pattern_backreference_gives_sed_specific_error() {
        // The regex crate refuses backreferences; we translate its raw parse
        // error into a sed-flavored message that names the limitation.
        let err = parse_program(r"s/(a)\1/X/").unwrap_err();
        assert!(err.contains("backreference"), "should name backreference: {err}");
        assert!(
            !err.contains("regex parse error"),
            "should not leak the raw engine error: {err}"
        );
    }

    #[test]
    fn escaped_backslash_before_pipe_is_literal_backslash_then_alternation() {
        // `a\\|b` is a literal backslash then `|` alternation in BOTH dialects:
        // the rewrite preserves `\\` as a unit, so the trailing `|` stays bare
        // ERE alternation and is not consumed as a BRE `\|`.
        let expr = parse_expression(r"s/a\\|b/X/").unwrap();
        assert_eq!(execute_sed(r"a\ b c", &[expr], false), "X b c\n");
    }

    // === Integration Tests ===

    #[tokio::test]
    async fn test_sed_from_stdin() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello world".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/world/kaish/".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "hi kaish\n".replace("hi", "hello"));
    }

    #[tokio::test]
    async fn test_sed_from_file() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/hello/hi/".into()));
        args.positional.push(Value::String("/test.txt".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("hi world"));
    }

    #[tokio::test]
    async fn test_sed_quiet_mode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("line 1\npattern\nline 3".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("/pattern/p".into()));
        args.flags.insert("n".to_string());

        let result = Sed.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "pattern\n");
    }

    #[tokio::test]
    async fn test_sed_missing_expression() {
        let mut ctx = make_ctx().await;
        let args = ToolArgs::new();

        let result = Sed.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("missing expression"));
    }

    #[tokio::test]
    async fn test_sed_invalid_expression() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("hello".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/[invalid/bar/".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(!result.ok());
        assert!(result.err.contains("sed:"));
    }

    #[tokio::test]
    async fn test_sed_file_not_found() {
        let mut ctx = make_ctx().await;

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/foo/bar/".into()));
        args.positional.push(Value::String("/nonexistent".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }

    #[tokio::test]
    async fn test_sed_unicode() {
        let mut ctx = make_ctx().await;
        ctx.set_stdin("こんにちは 世界".to_string());

        let mut args = ToolArgs::new();
        args.positional.push(Value::String("s/世界/kaish/".into()));

        let result = Sed.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert_eq!(&*result.text_out(), "こんにちは kaish\n");
    }
}
