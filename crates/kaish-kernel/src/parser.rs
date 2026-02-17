//! Parser for kaish source code.
//!
//! Transforms a token stream from the lexer into an Abstract Syntax Tree.
//! Uses chumsky for parser combinators with good error recovery.

use crate::ast::{
    Arg, Assignment, BinaryOp, CaseBranch, CaseStmt, Command, Expr, FileTestOp, ForLoop, IfStmt,
    Pipeline, Program, Redirect, RedirectKind, Stmt, StringPart, StringTestOp, TestCmpOp, TestExpr,
    ToolDef, Value, VarPath, VarSegment, WhileLoop,
};
use crate::lexer::{self, HereDocData, Token};
use chumsky::{input::ValueInput, prelude::*};

/// Span type used throughout the parser.
pub type Span = SimpleSpan;

/// Parse a raw `${...}` string into an Expr.
///
/// Handles:
/// - Special variables: `${?}` → LastExitCode, `${$}` → CurrentPid
/// - Simple paths: `${VAR}`, `${VAR.field}`, `${VAR[0]}`, `${?.ok}` → VarRef
/// - Default values: `${VAR:-default}` → VarWithDefault (with nested expansion support)
fn parse_var_expr(raw: &str) -> Expr {
    // Special case: ${?} is the last exit code (same as $?)
    if raw == "${?}" {
        return Expr::LastExitCode;
    }

    // Special case: ${$} is the current PID (same as $$)
    if raw == "${$}" {
        return Expr::CurrentPid;
    }

    // Check for default value syntax: ${VAR:-default}
    // Need to find :- that's not inside a nested ${...}
    if let Some(colon_idx) = find_default_separator(raw) {
        // Extract variable name (between ${ and :-)
        let name = raw[2..colon_idx].to_string();
        // Extract default value (between :- and }) and recursively parse it
        let default_str = &raw[colon_idx + 2..raw.len() - 1];
        let default = parse_interpolated_string(default_str);
        return Expr::VarWithDefault { name, default };
    }

    // Regular variable path
    Expr::VarRef(parse_varpath(raw))
}

/// Find the position of :- in a ${VAR:-default} expression, accounting for nested ${...}.
fn find_default_separator(raw: &str) -> Option<usize> {
    let bytes = raw.as_bytes();
    let mut depth = 0;
    let mut i = 0;

    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'$' && bytes[i + 1] == b'{' {
            depth += 1;
            i += 2;
            continue;
        }
        if bytes[i] == b'}' && depth > 0 {
            depth -= 1;
            i += 1;
            continue;
        }
        // Only find :- at the top level (depth == 1 means we're inside the outer ${...})
        if depth == 1 && i + 1 < bytes.len() && bytes[i] == b':' && bytes[i + 1] == b'-' {
            return Some(i);
        }
        i += 1;
    }
    None
}

/// Find the position of :- in variable content (without outer braces), accounting for nested ${...}.
fn find_default_separator_in_content(content: &str) -> Option<usize> {
    let bytes = content.as_bytes();
    let mut depth = 0;
    let mut i = 0;

    while i < bytes.len() {
        if i + 1 < bytes.len() && bytes[i] == b'$' && bytes[i + 1] == b'{' {
            depth += 1;
            i += 2;
            continue;
        }
        if bytes[i] == b'}' && depth > 0 {
            depth -= 1;
            i += 1;
            continue;
        }
        // Find :- at the top level (depth == 0)
        if depth == 0 && i + 1 < bytes.len() && bytes[i] == b':' && bytes[i + 1] == b'-' {
            return Some(i);
        }
        i += 1;
    }
    None
}

/// Parse a raw `${...}` string into a VarPath.
///
/// Handles paths like `${VAR}`, `${?.ok}`. Array indexing is not supported.
fn parse_varpath(raw: &str) -> VarPath {
    let segments_strs = lexer::parse_var_ref(raw).unwrap_or_default();
    let segments = segments_strs
        .into_iter()
        .filter(|s| !s.starts_with('['))  // Skip index segments
        .map(VarSegment::Field)
        .collect();
    VarPath { segments }
}

/// Parse an interpolated string like "Hello ${NAME}!" or "Hello $NAME!" into parts.
/// Extract a pipeline from a statement if possible.
fn stmt_to_pipeline(stmt: Stmt) -> Option<Pipeline> {
    match stmt {
        Stmt::Pipeline(p) => Some(p),
        Stmt::Command(cmd) => Some(Pipeline {
            commands: vec![cmd],
            background: false,
        }),
        _ => None,
    }
}

fn parse_interpolated_string(s: &str) -> Vec<StringPart> {
    // First, replace escaped dollar markers with a temporary placeholder
    // The lexer uses __KAISH_ESCAPED_DOLLAR__ for \$ to prevent re-interpretation
    let s = s.replace("__KAISH_ESCAPED_DOLLAR__", "\x00DOLLAR\x00");

    let mut parts = Vec::new();
    let mut current_text = String::new();
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch == '\x00' {
            // This is our escaped dollar marker - skip "DOLLAR" and the closing \x00
            let mut marker = String::new();
            while let Some(&c) = chars.peek() {
                if c == '\x00' {
                    chars.next(); // consume closing marker
                    break;
                }
                if let Some(c) = chars.next() {
                    marker.push(c);
                }
            }
            if marker == "DOLLAR" {
                current_text.push('$');
            }
        } else if ch == '$' {
            // Check for command substitution $(...)
            if chars.peek() == Some(&'(') {
                // Command substitution $(...)
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }

                // Consume the '('
                chars.next();

                // Collect until matching ')' accounting for nested parens
                let mut cmd_content = String::new();
                let mut paren_depth = 1;
                for c in chars.by_ref() {
                    if c == '(' {
                        paren_depth += 1;
                        cmd_content.push(c);
                    } else if c == ')' {
                        paren_depth -= 1;
                        if paren_depth == 0 {
                            break;
                        }
                        cmd_content.push(c);
                    } else {
                        cmd_content.push(c);
                    }
                }

                // Parse the command content as a pipeline
                // We need to use the main parser for this
                if let Ok(program) = parse(&cmd_content) {
                    // Extract the pipeline from the parsed result
                    if let Some(stmt) = program.statements.first() {
                        if let Some(pipeline) = stmt_to_pipeline(stmt.clone()) {
                            parts.push(StringPart::CommandSubst(pipeline));
                        } else {
                            // If we can't extract a pipeline, treat as literal
                            current_text.push_str("$(");
                            current_text.push_str(&cmd_content);
                            current_text.push(')');
                        }
                    }
                } else {
                    // Parse failed - treat as literal
                    current_text.push_str("$(");
                    current_text.push_str(&cmd_content);
                    current_text.push(')');
                }
            } else if chars.peek() == Some(&'{') {
                // Braced variable reference ${...}
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }

                // Consume the '{'
                chars.next();

                // Collect until matching '}', tracking nesting depth
                let mut var_content = String::new();
                let mut depth = 1;
                for c in chars.by_ref() {
                    if c == '{' && var_content.ends_with('$') {
                        depth += 1;
                        var_content.push(c);
                    } else if c == '}' {
                        depth -= 1;
                        if depth == 0 {
                            break;
                        }
                        var_content.push(c);
                    } else {
                        var_content.push(c);
                    }
                }

                // Parse the content for special syntax
                let part = if let Some(name) = var_content.strip_prefix('#') {
                    // Variable length: ${#VAR}
                    StringPart::VarLength(name.to_string())
                } else if var_content.starts_with("__ARITH:") && var_content.ends_with("__") {
                    // Arithmetic expression: ${__ARITH:expr__}
                    let expr = var_content
                        .strip_prefix("__ARITH:")
                        .and_then(|s| s.strip_suffix("__"))
                        .unwrap_or("");
                    StringPart::Arithmetic(expr.to_string())
                } else if let Some(colon_idx) = find_default_separator_in_content(&var_content) {
                    // Variable with default: ${VAR:-default} - recursively parse the default
                    let name = var_content[..colon_idx].to_string();
                    let default_str = &var_content[colon_idx + 2..];
                    let default = parse_interpolated_string(default_str);
                    StringPart::VarWithDefault { name, default }
                } else {
                    // Regular variable: ${VAR} or ${VAR.field}
                    StringPart::Var(parse_varpath(&format!("${{{}}}", var_content)))
                };
                parts.push(part);
            } else if chars.peek().map(|c| c.is_ascii_digit()).unwrap_or(false) {
                // Positional parameter $0-$9
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                if let Some(digit) = chars.next() {
                    let n = digit.to_digit(10).unwrap_or(0) as usize;
                    parts.push(StringPart::Positional(n));
                }
            } else if chars.peek() == Some(&'@') {
                // All arguments $@
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                chars.next(); // consume '@'
                parts.push(StringPart::AllArgs);
            } else if chars.peek() == Some(&'#') {
                // Argument count $#
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                chars.next(); // consume '#'
                parts.push(StringPart::ArgCount);
            } else if chars.peek() == Some(&'?') {
                // Last exit code $?
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                chars.next(); // consume '?'
                parts.push(StringPart::LastExitCode);
            } else if chars.peek() == Some(&'$') {
                // Current PID $$
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }
                chars.next(); // consume second '$'
                parts.push(StringPart::CurrentPid);
            } else if chars.peek().map(|c| c.is_ascii_alphabetic() || *c == '_').unwrap_or(false) {
                // Simple variable reference $NAME
                if !current_text.is_empty() {
                    parts.push(StringPart::Literal(std::mem::take(&mut current_text)));
                }

                // Collect identifier characters
                let mut var_name = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        if let Some(c) = chars.next() {
                            var_name.push(c);
                        }
                    } else {
                        break;
                    }
                }

                parts.push(StringPart::Var(VarPath::simple(var_name)));
            } else {
                // Literal $ (not followed by { or identifier start)
                current_text.push(ch);
            }
        } else {
            current_text.push(ch);
        }
    }

    if !current_text.is_empty() {
        parts.push(StringPart::Literal(current_text));
    }

    parts
}

/// Parse error with location and context.
#[derive(Debug, Clone)]
pub struct ParseError {
    pub span: Span,
    pub message: String,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} at {:?}", self.message, self.span)
    }
}

impl std::error::Error for ParseError {}

/// Parse kaish source code into a Program AST.
pub fn parse(source: &str) -> Result<Program, Vec<ParseError>> {
    // Tokenize with logos
    let tokens = lexer::tokenize(source).map_err(|errs| {
        errs.into_iter()
            .map(|e| ParseError {
                span: (e.span.start..e.span.end).into(),
                message: format!("lexer error: {}", e.token),
            })
            .collect::<Vec<_>>()
    })?;

    // Convert tokens to (Token, SimpleSpan) pairs
    let tokens: Vec<(Token, Span)> = tokens
        .into_iter()
        .map(|spanned| (spanned.token, (spanned.span.start..spanned.span.end).into()))
        .collect();

    // End-of-input span
    let end_span: Span = (source.len()..source.len()).into();

    // Parse using slice-based input (like nano_rust example)
    let parser = program_parser();
    let result = parser.parse(tokens.as_slice().map(end_span, |(t, s)| (t, s)));

    result.into_result().map_err(|errs| {
        errs.into_iter()
            .map(|e| ParseError {
                span: *e.span(),
                message: e.to_string(),
            })
            .collect()
    })
}

/// Parse a single statement (useful for REPL).
pub fn parse_statement(source: &str) -> Result<Stmt, Vec<ParseError>> {
    let program = parse(source)?;
    program
        .statements
        .into_iter()
        .find(|s| !matches!(s, Stmt::Empty))
        .ok_or_else(|| {
            vec![ParseError {
                span: (0..source.len()).into(),
                message: "empty input".to_string(),
            }]
        })
}

// ═══════════════════════════════════════════════════════════════════════════
// Parser Combinators - generic over input type
// ═══════════════════════════════════════════════════════════════════════════

/// Top-level program parser.
fn program_parser<'tokens, 'src: 'tokens, I>(
) -> impl Parser<'tokens, I, Program, extra::Err<Rich<'tokens, Token, Span>>>
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    statement_parser()
        .repeated()
        .collect::<Vec<_>>()
        .map(|statements| Program { statements })
}

/// Statement parser - dispatches based on leading token.
/// Supports statement-level chaining with && and ||.
fn statement_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    recursive(|stmt| {
        let terminator = choice((just(Token::Newline), just(Token::Semi))).repeated();

        // break [N] - break out of N levels of loops (default 1)
        let break_stmt = just(Token::Break)
            .ignore_then(
                select! { Token::Int(n) => n as usize }.or_not()
            )
            .map(Stmt::Break);

        // continue [N] - continue to next iteration, skipping N levels (default 1)
        let continue_stmt = just(Token::Continue)
            .ignore_then(
                select! { Token::Int(n) => n as usize }.or_not()
            )
            .map(Stmt::Continue);

        // return [expr] - return from a tool
        let return_stmt = just(Token::Return)
            .ignore_then(primary_expr_parser().or_not())
            .map(|e| Stmt::Return(e.map(Box::new)));

        // exit [code] - exit the script
        let exit_stmt = just(Token::Exit)
            .ignore_then(primary_expr_parser().or_not())
            .map(|e| Stmt::Exit(e.map(Box::new)));

        // set command: `set -e`, `set +e`, `set` (no args), `set -o pipefail`
        // This must come BEFORE assignment_parser to handle `set -e` vs `X=value`
        //
        // Strategy: Use lookahead to check what follows `set`:
        // - If followed by a flag (-e, --long, +e): parse as set command
        // - If followed by identifier NOT followed by =: parse as set command (e.g., `set pipefail`)
        // - If followed by nothing (end/newline/semi): parse as set command
        // - If followed by identifier then =: let assignment_parser handle it
        let set_flag_arg = choice((
            select! { Token::ShortFlag(f) => Arg::ShortFlag(f) },
            select! { Token::LongFlag(f) => Arg::LongFlag(f) },
            // PlusFlag for +e, +x etc. - convert to positional arg with + prefix
            select! { Token::PlusFlag(f) => Arg::Positional(Expr::Literal(Value::String(format!("+{}", f)))) },
        ));

        // set with flags: `set -e`, `set -e -u -o pipefail`
        let set_with_flags = just(Token::Set)
            .then(set_flag_arg)
            .then(
                choice((
                    set_flag_arg,
                    // Identifiers like 'pipefail' after -o
                    ident_parser().map(|name| Arg::Positional(Expr::Literal(Value::String(name)))),
                ))
                .repeated()
                .collect::<Vec<_>>(),
            )
            .map(|((_, first_arg), mut rest_args)| {
                let mut args = vec![first_arg];
                args.append(&mut rest_args);
                Stmt::Command(Command {
                    name: "set".to_string(),
                    args,
                    redirects: vec![],
                })
            });

        // set with no args: `set` alone (shows settings)
        // Must be followed by newline, semicolon, end of input, or a chaining operator (&&, ||)
        let set_no_args = just(Token::Set)
            .then(
                choice((
                    just(Token::Newline).to(()),
                    just(Token::Semi).to(()),
                    just(Token::And).to(()),
                    just(Token::Or).to(()),
                    end(),
                ))
                .rewind(),
            )
            .map(|_| Stmt::Command(Command {
                name: "set".to_string(),
                args: vec![],
                redirects: vec![],
            }));

        // Try set_with_flags first (requires at least one flag)
        // Then try set_no_args (no args, followed by terminator)
        // If neither matches, fall through to assignment_parser
        let set_command = set_with_flags.or(set_no_args);

        // Base statement (without chaining)
        let base_statement = choice((
            just(Token::Newline).to(Stmt::Empty),
            set_command,
            assignment_parser().map(Stmt::Assignment),
            // Shell-style functions (use $1, $2 positional params)
            posix_function_parser(stmt.clone()).map(Stmt::ToolDef),  // name() { }
            bash_function_parser(stmt.clone()).map(Stmt::ToolDef),   // function name { }
            if_parser(stmt.clone()).map(Stmt::If),
            for_parser(stmt.clone()).map(Stmt::For),
            while_parser(stmt.clone()).map(Stmt::While),
            case_parser(stmt.clone()).map(Stmt::Case),
            break_stmt,
            continue_stmt,
            return_stmt,
            exit_stmt,
            test_expr_stmt_parser().map(Stmt::Test),
            // Note: 'true' and 'false' are handled by command_parser/pipeline_parser
            pipeline_parser().map(|p| {
                // Unwrap single-command pipelines without background and without redirects
                if p.commands.len() == 1 && !p.background {
                    // Only unwrap if no redirects - redirects require pipeline processing
                    if p.commands[0].redirects.is_empty() {
                        // Safe: we just checked len == 1
                        match p.commands.into_iter().next() {
                            Some(cmd) => Stmt::Command(cmd),
                            None => Stmt::Empty, // unreachable but safe
                        }
                    } else {
                        Stmt::Pipeline(p)
                    }
                } else {
                    Stmt::Pipeline(p)
                }
            }),
        ))
        .boxed();

        // Statement chaining with precedence: && binds tighter than ||
        // and_chain = base_stmt { "&&" base_stmt }
        // or_chain  = and_chain { "||" and_chain }
        let and_chain = base_statement
            .clone()
            .foldl(
                just(Token::And).ignore_then(base_statement).repeated(),
                |left, right| Stmt::AndChain {
                    left: Box::new(left),
                    right: Box::new(right),
                },
            );

        and_chain
            .clone()
            .foldl(
                just(Token::Or).ignore_then(and_chain).repeated(),
                |left, right| Stmt::OrChain {
                    left: Box::new(left),
                    right: Box::new(right),
                },
            )
            .then_ignore(terminator)
    })
}

/// Assignment: `NAME=value` (bash-style) or `local NAME = value` (scoped)
fn assignment_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Assignment, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // local NAME = value (with spaces around =)
    let local_assignment = just(Token::Local)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|(name, value)| Assignment {
            name,
            value,
            local: true,
        });

    // Bash-style: NAME=value (no spaces around =)
    // The lexer produces IDENT EQ EXPR, so we parse it here
    let bash_assignment = ident_parser()
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|(name, value)| Assignment {
            name,
            value,
            local: false,
        });

    choice((local_assignment, bash_assignment))
        .labelled("assignment")
        .boxed()
}

/// POSIX-style function: `name() { body }`
///
/// Produces a ToolDef with empty params - uses positional params ($1, $2, etc.)
fn posix_function_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, ToolDef, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    ident_parser()
        .then_ignore(just(Token::LParen))
        .then_ignore(just(Token::RParen))
        .then_ignore(just(Token::LBrace))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::RBrace))
        .map(|(name, body)| ToolDef { name, params: vec![], body })
        .labelled("POSIX function")
        .boxed()
}

/// Bash-style function: `function name { body }` (without parens)
///
/// Produces a ToolDef with empty params - uses positional params ($1, $2, etc.)
fn bash_function_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, ToolDef, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    just(Token::Function)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::LBrace))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::RBrace))
        .map(|(name, body)| ToolDef { name, params: vec![], body })
        .labelled("bash function")
        .boxed()
}

/// If statement: `if COND; then STMTS [elif COND; then STMTS]* [else STMTS] fi`
///
/// elif clauses are desugared to nested if/else:
///   `if A; then X elif B; then Y else Z fi`
/// becomes:
///   `if A; then X else { if B; then Y else Z fi } fi`
fn if_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, IfStmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    // Parse a single branch: condition + then statements
    let branch = condition_parser()
        .then_ignore(just(Token::Semi).or_not())
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::Then))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.clone()
                .repeated()
                .collect::<Vec<_>>()
                .map(|stmts: Vec<Stmt>| {
                    stmts
                        .into_iter()
                        .filter(|s| !matches!(s, Stmt::Empty))
                        .collect::<Vec<_>>()
                }),
        );

    // Parse elif branches: `elif COND; then STMTS`
    let elif_branch = just(Token::Elif)
        .ignore_then(condition_parser())
        .then_ignore(just(Token::Semi).or_not())
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::Then))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.clone()
                .repeated()
                .collect::<Vec<_>>()
                .map(|stmts: Vec<Stmt>| {
                    stmts
                        .into_iter()
                        .filter(|s| !matches!(s, Stmt::Empty))
                        .collect::<Vec<_>>()
                }),
        );

    // Parse else branch: `else STMTS`
    let else_branch = just(Token::Else)
        .ignore_then(just(Token::Newline).repeated())
        .ignore_then(stmt.repeated().collect::<Vec<_>>())
        .map(|stmts: Vec<Stmt>| {
            stmts
                .into_iter()
                .filter(|s| !matches!(s, Stmt::Empty))
                .collect::<Vec<_>>()
        });

    just(Token::If)
        .ignore_then(branch)
        .then(elif_branch.repeated().collect::<Vec<_>>())
        .then(else_branch.or_not())
        .then_ignore(just(Token::Fi))
        .map(|(((condition, then_branch), elif_branches), else_branch)| {
            // Build nested if/else structure from elif branches
            build_if_chain(condition, then_branch, elif_branches, else_branch)
        })
        .labelled("if statement")
        .boxed()
}

/// Build a nested IfStmt chain from elif branches.
///
/// Transforms:
///   if A then X elif B then Y elif C then Z else W fi
/// Into:
///   IfStmt { cond: A, then: X, else: Some([IfStmt { cond: B, then: Y, else: Some([IfStmt { cond: C, then: Z, else: Some(W) }]) }]) }
fn build_if_chain(
    condition: Expr,
    then_branch: Vec<Stmt>,
    mut elif_branches: Vec<(Expr, Vec<Stmt>)>,
    else_branch: Option<Vec<Stmt>>,
) -> IfStmt {
    if elif_branches.is_empty() {
        // No elif, just if/else
        IfStmt {
            condition: Box::new(condition),
            then_branch,
            else_branch,
        }
    } else {
        // Pop the first elif and recursively build the rest
        let (elif_cond, elif_then) = elif_branches.remove(0);
        let nested_if = build_if_chain(elif_cond, elif_then, elif_branches, else_branch);
        IfStmt {
            condition: Box::new(condition),
            then_branch,
            else_branch: Some(vec![Stmt::If(nested_if)]),
        }
    }
}

/// For loop: `for VAR in ITEMS; do STMTS done`
fn for_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, ForLoop, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    just(Token::For)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::In))
        .then(expr_parser().repeated().at_least(1).collect::<Vec<_>>())
        .then_ignore(just(Token::Semi).or_not())
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::Do))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::Done))
        .map(|((variable, items), body)| ForLoop {
            variable,
            items,
            body,
        })
        .labelled("for loop")
        .boxed()
}

/// While loop: `while condition; do ...; done`
fn while_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, WhileLoop, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    just(Token::While)
        .ignore_then(condition_parser())
        .then_ignore(just(Token::Semi).or_not())
        .then_ignore(just(Token::Newline).repeated())
        .then_ignore(just(Token::Do))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::Done))
        .map(|(condition, body)| WhileLoop {
            condition: Box::new(condition),
            body,
        })
        .labelled("while loop")
        .boxed()
}

/// Case statement: `case expr in pattern) commands ;; esac`
///
/// Supports:
/// - Single patterns: `pattern) commands ;;`
/// - Multiple patterns: `pattern1|pattern2) commands ;;`
/// - Optional leading `(` before patterns: `(pattern) commands ;;`
fn case_parser<'tokens, I, S>(
    stmt: S,
) -> impl Parser<'tokens, I, CaseStmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    S: Parser<'tokens, I, Stmt, extra::Err<Rich<'tokens, Token, Span>>> + Clone + 'tokens,
{
    // Pattern part: individual tokens that make up a glob pattern
    // e.g., "*.rs" is Star + Dot + Ident("rs")
    let pattern_part = choice((
        select! { Token::Ident(s) => s },
        select! { Token::String(s) => s },
        select! { Token::SingleString(s) => s },
        select! { Token::Int(n) => n.to_string() },
        select! { Token::Star => "*".to_string() },
        select! { Token::Question => "?".to_string() },
        select! { Token::Dot => ".".to_string() },
        select! { Token::DotDot => "..".to_string() },
        select! { Token::Tilde => "~".to_string() },
        select! { Token::TildePath(s) => s },
        select! { Token::RelativePath(s) => s },
        select! { Token::Path(p) => p },
        select! { Token::VarRef(v) => v },
        select! { Token::SimpleVarRef(v) => format!("${}", v) },
        // Character class: [a-z], [!abc], [^abc], etc.
        just(Token::LBracket)
            .ignore_then(
                choice((
                    select! { Token::Ident(s) => s },
                    select! { Token::Int(n) => n.to_string() },
                    just(Token::Colon).to(":".to_string()),
                    // Negation: ! or ^ at start of char class
                    just(Token::Bang).to("!".to_string()),
                    // Range like a-z
                    select! { Token::ShortFlag(s) => format!("-{}", s) },
                ))
                .repeated()
                .at_least(1)
                .collect::<Vec<String>>()
            )
            .then_ignore(just(Token::RBracket))
            .map(|parts| format!("[{}]", parts.join(""))),
        // Brace expansion: {a,b,c} or {js,ts}
        just(Token::LBrace)
            .ignore_then(
                choice((
                    select! { Token::Ident(s) => s },
                    select! { Token::Int(n) => n.to_string() },
                ))
                .separated_by(just(Token::Comma))
                .at_least(1)
                .collect::<Vec<String>>()
            )
            .then_ignore(just(Token::RBrace))
            .map(|parts| format!("{{{}}}", parts.join(","))),
    ));

    // A complete pattern is one or more pattern parts joined together
    // e.g., "*.rs" = Star + Dot + Ident
    let pattern = pattern_part
        .repeated()
        .at_least(1)
        .collect::<Vec<String>>()
        .map(|parts| parts.join(""))
        .labelled("case pattern");

    // Multiple patterns separated by pipe: `pattern1 | pattern2`
    let patterns = pattern
        .separated_by(just(Token::Pipe))
        .at_least(1)
        .collect::<Vec<String>>()
        .labelled("case patterns");

    // Branch: `[( ] patterns ) commands ;;`
    let branch = just(Token::LParen)
        .or_not()
        .ignore_then(just(Token::Newline).repeated())
        .ignore_then(patterns)
        .then_ignore(just(Token::RParen))
        .then_ignore(just(Token::Newline).repeated())
        .then(
            stmt.clone()
                .repeated()
                .collect::<Vec<_>>()
                .map(|stmts| stmts.into_iter().filter(|s| !matches!(s, Stmt::Empty)).collect()),
        )
        .then_ignore(just(Token::DoubleSemi))
        .then_ignore(just(Token::Newline).repeated())
        .map(|(patterns, body)| CaseBranch { patterns, body })
        .labelled("case branch");

    just(Token::Case)
        .ignore_then(expr_parser())
        .then_ignore(just(Token::In))
        .then_ignore(just(Token::Newline).repeated())
        .then(branch.repeated().collect::<Vec<_>>())
        .then_ignore(just(Token::Esac))
        .map(|(expr, branches)| CaseStmt { expr, branches })
        .labelled("case statement")
        .boxed()
}

/// Pipeline: `cmd | cmd | cmd [&]`
fn pipeline_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Pipeline, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    command_parser()
        .separated_by(just(Token::Pipe))
        .at_least(1)
        .collect::<Vec<_>>()
        .then(just(Token::Amp).or_not())
        .map(|(commands, bg)| Pipeline {
            commands,
            background: bg.is_some(),
        })
        .labelled("pipeline")
        .boxed()
}

/// Command: `name args... [redirects...]`
/// Command names can be identifiers, 'true', 'false', or '.' (source alias).
fn command_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Command, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Command name can be an identifier, path, 'true', 'false', or '.' (source alias)
    let command_name = choice((
        ident_parser(),
        path_parser(),
        just(Token::True).to("true".to_string()),
        just(Token::False).to("false".to_string()),
        just(Token::Dot).to(".".to_string()),
    ));

    command_name
        .then(args_list_parser())
        .then(redirect_parser().repeated().collect::<Vec<_>>())
        .map(|((name, args), redirects)| Command {
            name,
            args,
            redirects,
        })
        .labelled("command")
        .boxed()
}

/// Arguments list parser that handles `--` flag terminator.
///
/// After `--`, all subsequent flags are converted to positional string arguments.
fn args_list_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Vec<Arg>, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Arguments before `--` (normal parsing)
    let pre_dash = arg_before_double_dash_parser()
        .repeated()
        .collect::<Vec<_>>();

    // The `--` marker itself
    let double_dash = select! {
        Token::DoubleDash => Arg::DoubleDash,
    };

    // Arguments after `--` (flags become positional strings)
    let post_dash_arg = choice((
        // Flags become positional strings
        select! {
            Token::ShortFlag(name) => Arg::Positional(Expr::Literal(Value::String(format!("-{}", name)))),
            Token::LongFlag(name) => Arg::Positional(Expr::Literal(Value::String(format!("--{}", name)))),
        },
        // Everything else stays the same
        primary_expr_parser().map(Arg::Positional),
    ));

    let post_dash = post_dash_arg.repeated().collect::<Vec<_>>();

    // Combine: args_before ++ [--] ++ args_after
    pre_dash
        .then(double_dash.then(post_dash).or_not())
        .map(|(mut args, maybe_dd)| {
            if let Some((dd, post)) = maybe_dd {
                args.push(dd);
                args.extend(post);
            }
            args
        })
}

/// Argument parser for arguments before `--` (normal flag handling).
fn arg_before_double_dash_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Arg, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Long flag with value: --name=value
    let long_flag_with_value = select! {
        Token::LongFlag(name) => name,
    }
    .then_ignore(just(Token::Eq))
    .then(primary_expr_parser())
    .map(|(key, value)| Arg::Named { key, value });

    // Boolean long flag: --name
    let long_flag = select! {
        Token::LongFlag(name) => Arg::LongFlag(name),
    };

    // Boolean short flag: -x
    let short_flag = select! {
        Token::ShortFlag(name) => Arg::ShortFlag(name),
    };

    // Named argument: name=value (must not have spaces around =)
    // We use map_with to capture spans and validate adjacency
    let named = select! {
        Token::Ident(s) => s,
    }
    .map_with(|s, e| -> (String, Span) { (s, e.span()) })
    .then(just(Token::Eq).map_with(|_, e| -> Span { e.span() }))
    .then(primary_expr_parser().map_with(|expr, e| -> (Expr, Span) { (expr, e.span()) }))
    .try_map(|(((key, key_span), eq_span), (value, value_span)): (((String, Span), Span), (Expr, Span)), span| {
        // Check that key ends where = starts and = ends where value starts
        if key_span.end != eq_span.start || eq_span.end != value_span.start {
            Err(Rich::custom(
                span,
                "named argument must not have spaces around '=' (use 'key=value' not 'key = value')",
            ))
        } else {
            Ok(Arg::Named { key, value })
        }
    });

    // Positional argument
    let positional = primary_expr_parser().map(Arg::Positional);

    // Order matters: try more specific patterns first
    // Note: DoubleDash is NOT included here - it's handled by args_list_parser
    choice((
        long_flag_with_value,
        long_flag,
        short_flag,
        named,
        positional,
    ))
    .boxed()
}

/// Redirect: `> file`, `>> file`, `< file`, `<< heredoc`, `2> file`, `&> file`, `2>&1`
fn redirect_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Redirect, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Regular redirects: >, >>, <, 2>, &>
    let regular_redirect = select! {
        Token::GtGt => RedirectKind::StdoutAppend,
        Token::Gt => RedirectKind::StdoutOverwrite,
        Token::Lt => RedirectKind::Stdin,
        Token::Stderr => RedirectKind::Stderr,
        Token::Both => RedirectKind::Both,
    }
    .then(primary_expr_parser())
    .map(|(kind, target)| Redirect { kind, target });

    // Here-doc redirect: << content
    // Quoted delimiters (<<'EOF' or <<"EOF") produce literal heredocs (no expansion).
    // Unquoted delimiters produce interpolated heredocs (variables are expanded).
    let heredoc_redirect = just(Token::HereDocStart)
        .ignore_then(select! { Token::HereDoc(data) => data })
        .map(|data: HereDocData| {
            let target = if data.literal {
                Expr::Literal(Value::String(data.content))
            } else {
                let parts = parse_interpolated_string(&data.content);
                // If there's only one literal part, simplify to Expr::Literal
                if parts.len() == 1 {
                    if let StringPart::Literal(text) = &parts[0] {
                        return Redirect {
                            kind: RedirectKind::HereDoc,
                            target: Expr::Literal(Value::String(text.clone())),
                        };
                    }
                }
                Expr::Interpolated(parts)
            };
            Redirect {
                kind: RedirectKind::HereDoc,
                target,
            }
        });

    // Merge stderr to stdout: 2>&1 (no target needed - implicit)
    let merge_stderr_redirect = just(Token::StderrToStdout)
        .map(|_| Redirect {
            kind: RedirectKind::MergeStderr,
            // Target is unused for MergeStderr, but we need something
            target: Expr::Literal(Value::Null),
        });

    // Merge stdout to stderr: 1>&2 or >&2 (no target needed - implicit)
    let merge_stdout_redirect = choice((
        just(Token::StdoutToStderr),
        just(Token::StdoutToStderr2),
    ))
    .map(|_| Redirect {
        kind: RedirectKind::MergeStdout,
        // Target is unused for MergeStdout, but we need something
        target: Expr::Literal(Value::Null),
    });

    choice((heredoc_redirect, merge_stderr_redirect, merge_stdout_redirect, regular_redirect))
        .labelled("redirect")
        .boxed()
}

/// Test expression parser for `[[ ... ]]` syntax.
///
/// Supports:
/// - File tests: `[[ -f path ]]`, `[[ -d path ]]`, etc.
/// - String tests: `[[ -z str ]]`, `[[ -n str ]]`
/// - Comparisons: `[[ $X == "value" ]]`, `[[ $NUM -gt 5 ]]`
/// - Compound: `[[ -f a && -d b ]]`, `[[ -z x || -n y ]]`, `[[ ! -f file ]]`
///
/// Precedence (highest to lowest): `!` > `&&` > `||`
fn test_expr_stmt_parser<'tokens, I>(
) -> impl Parser<'tokens, I, TestExpr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // File test operators: -e, -f, -d, -r, -w, -x
    let file_test_op = select! {
        Token::ShortFlag(s) if s == "e" => FileTestOp::Exists,
        Token::ShortFlag(s) if s == "f" => FileTestOp::IsFile,
        Token::ShortFlag(s) if s == "d" => FileTestOp::IsDir,
        Token::ShortFlag(s) if s == "r" => FileTestOp::Readable,
        Token::ShortFlag(s) if s == "w" => FileTestOp::Writable,
        Token::ShortFlag(s) if s == "x" => FileTestOp::Executable,
    };

    // String test operators: -z, -n
    let string_test_op = select! {
        Token::ShortFlag(s) if s == "z" => StringTestOp::IsEmpty,
        Token::ShortFlag(s) if s == "n" => StringTestOp::IsNonEmpty,
    };

    // Comparison operators: =, ==, !=, =~, !~, >, <, >=, <=, -gt, -lt, -ge, -le, -eq, -ne
    // Note: = and == are equivalent inside [[ ]] (matching bash behavior)
    let cmp_op = choice((
        just(Token::EqEq).to(TestCmpOp::Eq),
        just(Token::Eq).to(TestCmpOp::Eq),
        just(Token::NotEq).to(TestCmpOp::NotEq),
        just(Token::Match).to(TestCmpOp::Match),
        just(Token::NotMatch).to(TestCmpOp::NotMatch),
        just(Token::Gt).to(TestCmpOp::Gt),
        just(Token::Lt).to(TestCmpOp::Lt),
        just(Token::GtEq).to(TestCmpOp::GtEq),
        just(Token::LtEq).to(TestCmpOp::LtEq),
        select! { Token::ShortFlag(s) if s == "eq" => TestCmpOp::Eq },
        select! { Token::ShortFlag(s) if s == "ne" => TestCmpOp::NotEq },
        select! { Token::ShortFlag(s) if s == "gt" => TestCmpOp::Gt },
        select! { Token::ShortFlag(s) if s == "lt" => TestCmpOp::Lt },
        select! { Token::ShortFlag(s) if s == "ge" => TestCmpOp::GtEq },
        select! { Token::ShortFlag(s) if s == "le" => TestCmpOp::LtEq },
    ));

    // File test: -f path
    let file_test = file_test_op
        .then(primary_expr_parser())
        .map(|(op, path)| TestExpr::FileTest {
            op,
            path: Box::new(path),
        });

    // String test: -z str
    let string_test = string_test_op
        .then(primary_expr_parser())
        .map(|(op, value)| TestExpr::StringTest {
            op,
            value: Box::new(value),
        });

    // Comparison: $X == "value" or $NUM -gt 5
    let comparison = primary_expr_parser()
        .then(cmp_op)
        .then(primary_expr_parser())
        .map(|((left, op), right)| TestExpr::Comparison {
            left: Box::new(left),
            op,
            right: Box::new(right),
        });

    // Primary test expression (atomic - no compound operators)
    let primary_test = choice((file_test, string_test, comparison));

    // Build compound expressions with proper precedence:
    // Grammar:
    //   test_expr = or_expr
    //   or_expr   = and_expr { "||" and_expr }
    //   and_expr  = unary_expr { "&&" unary_expr }
    //   unary_expr = "!" unary_expr | primary_test
    //
    // Precedence: ! (highest) > && > ||

    // Use recursive for the unary NOT operator
    let compound_test = recursive(|compound| {
        // Unary NOT: ! expr (can be chained: ! ! expr)
        let not_expr = just(Token::Bang)
            .ignore_then(compound.clone())
            .map(|expr| TestExpr::Not { expr: Box::new(expr) });

        // Unary level: ! or primary
        let unary = choice((not_expr, primary_test.clone()));

        // AND level: unary && unary && ...
        let and_expr = unary.clone().foldl(
            just(Token::And).ignore_then(unary).repeated(),
            |left, right| TestExpr::And {
                left: Box::new(left),
                right: Box::new(right),
            },
        );

        // OR level: and_expr || and_expr || ...
        and_expr.clone().foldl(
            just(Token::Or).ignore_then(and_expr).repeated(),
            |left, right| TestExpr::Or {
                left: Box::new(left),
                right: Box::new(right),
            },
        )
    });

    // [[ ]] is two consecutive bracket tokens (not a single TestStart token)
    // to avoid conflicts with nested array syntax like [[1, 2], [3, 4]]
    just(Token::LBracket)
        .then(just(Token::LBracket))
        .ignore_then(compound_test)
        .then_ignore(just(Token::RBracket).then(just(Token::RBracket)))
        .labelled("test expression")
        .boxed()
}

/// Condition parser: supports [[ ]] test expressions and commands with && / || chaining.
///
/// Shell semantics: conditions are commands whose exit codes determine truthiness.
/// - `if true; then` → runs `true` builtin, exit code 0 = truthy
/// - `if grep -q pattern file; then` → runs command, checks exit code
/// - `if a && b; then` → runs `a`, if exit 0, runs `b`
///
/// Use `[[ ]]` for comparisons: `if [[ $X -gt 5 ]]; then`
///
/// Grammar (with precedence - && binds tighter than ||):
///   condition = or_expr
///   or_expr   = and_expr { "||" and_expr }
///   and_expr  = base { "&&" base }
///   base      = test_expr | command
fn condition_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // [[ ]] test expression - wrap as Expr::Test
    let test_expr_condition = test_expr_stmt_parser().map(|test| Expr::Test(Box::new(test)));

    // Command as condition (includes true/false as command names)
    // The command's exit code determines truthiness (0 = true, non-zero = false)
    let command_condition = command_parser().map(Expr::Command);

    // Base: test expr OR command
    let base = choice((test_expr_condition, command_condition));

    // && has higher precedence than ||
    // First chain with && (higher precedence)
    let and_expr = base.clone().foldl(
        just(Token::And).ignore_then(base).repeated(),
        |left, right| Expr::BinaryOp {
            left: Box::new(left),
            op: BinaryOp::And,
            right: Box::new(right),
        },
    );

    // Then chain with || (lower precedence)
    and_expr
        .clone()
        .foldl(
            just(Token::Or).ignore_then(and_expr).repeated(),
            |left, right| Expr::BinaryOp {
                left: Box::new(left),
                op: BinaryOp::Or,
                right: Box::new(right),
            },
        )
        .labelled("condition")
        .boxed()
}

/// Expression parser - supports && and || binary operators.
fn expr_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // For now, just primary expressions. Can extend for && / || later if needed.
    primary_expr_parser()
}

/// Primary expression: literal, variable reference, command substitution, or bare identifier.
///
/// Uses `recursive` to support nested command substitution like `$(echo $(date))`.
fn primary_expr_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Positional parameters: $0-$9, $@, $#, ${#VAR}, $?, $$
    let positional = select! {
        Token::Positional(n) => Expr::Positional(n),
        Token::AllArgs => Expr::AllArgs,
        Token::ArgCount => Expr::ArgCount,
        Token::VarLength(name) => Expr::VarLength(name),
        Token::LastExitCode => Expr::LastExitCode,
        Token::CurrentPid => Expr::CurrentPid,
    };

    // Arithmetic expression: $((expr)) - preprocessed into Arithmetic token
    let arithmetic = select! {
        Token::Arithmetic(expr_str) => Expr::Arithmetic(expr_str),
    };

    // Keywords that can also be used as barewords in argument position
    // (e.g., `echo done` should work even though `done` is a keyword)
    let keyword_as_bareword = select! {
        Token::Done => "done",
        Token::Fi => "fi",
        Token::Then => "then",
        Token::Else => "else",
        Token::Elif => "elif",
        Token::In => "in",
        Token::Do => "do",
        Token::Esac => "esac",
    }
    .map(|s| Expr::Literal(Value::String(s.to_string())));

    // Bare words starting with + or - (e.g., date +%s, cat -)
    let plus_minus_bare = select! {
        Token::PlusBare(s) => Expr::Literal(Value::String(s)),
        Token::MinusBare(s) => Expr::Literal(Value::String(s)),
        Token::MinusAlone => Expr::Literal(Value::String("-".to_string())),
    };

    recursive(|expr| {
        choice((
            positional,
            arithmetic,
            cmd_subst_parser(expr.clone()),
            var_expr_parser(),
            interpolated_string_parser(),
            literal_parser().map(Expr::Literal),
            // Bare identifiers become string literals (shell barewords)
            ident_parser().map(|s| Expr::Literal(Value::String(s))),
            // Absolute paths become string literals
            path_parser().map(|s| Expr::Literal(Value::String(s))),
            // Bare words starting with + or - (date +%s, cat -)
            // Shell navigation tokens
            select! {
                Token::DotDot => Expr::Literal(Value::String("..".into())),
                Token::Tilde => Expr::Literal(Value::String("~".into())),
                Token::TildePath(s) => Expr::Literal(Value::String(s)),
                Token::RelativePath(s) => Expr::Literal(Value::String(s)),
            },
            plus_minus_bare,
            // Keywords can be used as barewords in argument position
            keyword_as_bareword,
        ))
        .labelled("expression")
    })
    .boxed()
}

/// Variable reference: `${VAR}`, `${VAR.field}`, `${VAR:-default}`, or `$VAR` (simple form).
/// Returns Expr directly to support both VarRef and VarWithDefault.
fn var_expr_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::VarRef(raw) => parse_var_expr(&raw),
        Token::SimpleVarRef(name) => Expr::VarRef(VarPath::simple(name)),
    }
    .labelled("variable reference")
}

/// Command substitution: `$(pipeline)` - runs a pipeline and returns its result.
///
/// Accepts a recursive expression parser to support nested command substitution.
fn cmd_subst_parser<'tokens, I, E>(
    expr: E,
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
    E: Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone,
{
    // Argument parser using the recursive expression parser
    // Long flag with value: --name=value
    let long_flag_with_value = select! {
        Token::LongFlag(name) => name,
    }
    .then_ignore(just(Token::Eq))
    .then(expr.clone())
    .map(|(key, value)| Arg::Named { key, value });

    // Boolean long flag: --name
    let long_flag = select! {
        Token::LongFlag(name) => Arg::LongFlag(name),
    };

    // Boolean short flag: -x
    let short_flag = select! {
        Token::ShortFlag(name) => Arg::ShortFlag(name),
    };

    // Named argument: name=value
    let named = ident_parser()
        .then_ignore(just(Token::Eq))
        .then(expr.clone())
        .map(|(key, value)| Arg::Named { key, value });

    // Positional argument
    let positional = expr.map(Arg::Positional);

    let arg = choice((
        long_flag_with_value,
        long_flag,
        short_flag,
        named,
        positional,
    ));

    // Command name parser - accepts identifiers and boolean keywords (true/false are builtins)
    let command_name = choice((
        ident_parser(),
        just(Token::True).to("true".to_string()),
        just(Token::False).to("false".to_string()),
    ));

    // Command parser
    let command = command_name
        .then(arg.repeated().collect::<Vec<_>>())
        .map(|(name, args)| Command {
            name,
            args,
            redirects: vec![],
        });

    // Pipeline parser
    let pipeline = command
        .separated_by(just(Token::Pipe))
        .at_least(1)
        .collect::<Vec<_>>()
        .map(|commands| Pipeline {
            commands,
            background: false,
        });

    just(Token::CmdSubstStart)
        .ignore_then(pipeline)
        .then_ignore(just(Token::RParen))
        .map(|pipeline| Expr::CommandSubst(Box::new(pipeline)))
        .labelled("command substitution")
}

/// String parser - handles double-quoted strings (with interpolation) and single-quoted (literal).
fn interpolated_string_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Expr, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    // Double-quoted string: may contain $VAR or ${VAR} interpolation
    let double_quoted = select! {
        Token::String(s) => s,
    }
    .map(|s| {
        // Check if string contains interpolation markers (${} or $NAME) or escaped dollars
        if s.contains('$') || s.contains("__KAISH_ESCAPED_DOLLAR__") {
            // Parse interpolated parts
            let parts = parse_interpolated_string(&s);
            if parts.len() == 1
                && let StringPart::Literal(text) = &parts[0] {
                    return Expr::Literal(Value::String(text.clone()));
                }
            Expr::Interpolated(parts)
        } else {
            Expr::Literal(Value::String(s))
        }
    });

    // Single-quoted string: literal, no interpolation
    let single_quoted = select! {
        Token::SingleString(s) => Expr::Literal(Value::String(s)),
    };

    choice((single_quoted, double_quoted)).labelled("string")
}

/// Literal value parser (excluding strings, which are handled by interpolated_string_parser).
fn literal_parser<'tokens, I>(
) -> impl Parser<'tokens, I, Value, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    choice((
        select! {
            Token::True => Value::Bool(true),
            Token::False => Value::Bool(false),
        },
        select! {
            Token::Int(n) => Value::Int(n),
            Token::Float(f) => Value::Float(f),
        },
    ))
    .labelled("literal")
    .boxed()
}

/// Identifier parser.
fn ident_parser<'tokens, I>(
) -> impl Parser<'tokens, I, String, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::Ident(s) => s,
    }
    .labelled("identifier")
}

/// Path parser: matches absolute paths like `/tmp/out`, `/etc/hosts`.
fn path_parser<'tokens, I>(
) -> impl Parser<'tokens, I, String, extra::Err<Rich<'tokens, Token, Span>>> + Clone
where
    I: ValueInput<'tokens, Token = Token, Span = Span>,
{
    select! {
        Token::Path(s) => s,
    }
    .labelled("path")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_empty() {
        let result = parse("");
        assert!(result.is_ok());
        assert_eq!(result.expect("ok").statements.len(), 0);
    }

    #[test]
    fn parse_newlines_only() {
        let result = parse("\n\n\n");
        assert!(result.is_ok());
    }

    #[test]
    fn parse_simple_command() {
        let result = parse("echo");
        assert!(result.is_ok());
        let program = result.expect("ok");
        assert_eq!(program.statements.len(), 1);
        assert!(matches!(&program.statements[0], Stmt::Command(_)));
    }

    #[test]
    fn parse_command_with_string_arg() {
        let result = parse(r#"echo "hello""#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.args.len(), 1),
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_assignment() {
        let result = parse("X=5");
        assert!(result.is_ok());
        let program = result.expect("ok");
        assert!(matches!(&program.statements[0], Stmt::Assignment(_)));
    }

    #[test]
    fn parse_pipeline() {
        let result = parse("a | b | c");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Pipeline(p) => assert_eq!(p.commands.len(), 3),
            _ => panic!("expected Pipeline"),
        }
    }

    #[test]
    fn parse_background_job() {
        let result = parse("cmd &");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Pipeline(p) => assert!(p.background),
            _ => panic!("expected Pipeline with background"),
        }
    }

    #[test]
    fn parse_if_simple() {
        let result = parse("if true; then echo; fi");
        assert!(result.is_ok());
        let program = result.expect("ok");
        assert!(matches!(&program.statements[0], Stmt::If(_)));
    }

    #[test]
    fn parse_if_else() {
        let result = parse("if true; then echo; else echo; fi");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::If(if_stmt) => assert!(if_stmt.else_branch.is_some()),
            _ => panic!("expected If"),
        }
    }

    #[test]
    fn parse_elif_simple() {
        let result = parse("if true; then echo a; elif false; then echo b; fi");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::If(if_stmt) => {
                // elif is desugared to nested if in else
                assert!(if_stmt.else_branch.is_some());
                let else_branch = if_stmt.else_branch.as_ref().unwrap();
                assert_eq!(else_branch.len(), 1);
                assert!(matches!(&else_branch[0], Stmt::If(_)));
            }
            _ => panic!("expected If"),
        }
    }

    #[test]
    fn parse_elif_with_else() {
        let result = parse("if true; then echo a; elif false; then echo b; else echo c; fi");
        assert!(result.is_ok(), "parse failed: {:?}", result);
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::If(outer_if) => {
                // Check nested structure: if -> elif -> else
                let else_branch = outer_if.else_branch.as_ref().expect("outer else");
                assert_eq!(else_branch.len(), 1);
                match &else_branch[0] {
                    Stmt::If(inner_if) => {
                        // The inner if (from elif) should have the final else
                        assert!(inner_if.else_branch.is_some());
                    }
                    _ => panic!("expected nested If from elif"),
                }
            }
            _ => panic!("expected If"),
        }
    }

    #[test]
    fn parse_multiple_elif() {
        // Shell-compatible: use [[ ]] for comparisons
        let result = parse(
            "if [[ ${X} == 1 ]]; then echo one; elif [[ ${X} == 2 ]]; then echo two; elif [[ ${X} == 3 ]]; then echo three; else echo other; fi",
        );
        assert!(result.is_ok(), "parse failed: {:?}", result);
    }

    #[test]
    fn parse_for_loop() {
        let result = parse("for X in items; do echo; done");
        assert!(result.is_ok());
        let program = result.expect("ok");
        assert!(matches!(&program.statements[0], Stmt::For(_)));
    }

    #[test]
    fn parse_brackets_not_array_literal() {
        // Array literals are no longer supported, [ is just a regular char
        let result = parse("cmd [1");
        // This should fail or parse unexpectedly - arrays are removed
        // Just verify we don't crash
        let _ = result;
    }

    #[test]
    fn parse_named_arg() {
        let result = parse("cmd foo=5");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.args.len(), 1);
                assert!(matches!(&cmd.args[0], Arg::Named { .. }));
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_short_flag() {
        let result = parse("ls -l");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "ls");
                assert_eq!(cmd.args.len(), 1);
                match &cmd.args[0] {
                    Arg::ShortFlag(name) => assert_eq!(name, "l"),
                    _ => panic!("expected ShortFlag"),
                }
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_long_flag() {
        let result = parse("git push --force");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "git");
                assert_eq!(cmd.args.len(), 2);
                match &cmd.args[0] {
                    Arg::Positional(Expr::Literal(Value::String(s))) => assert_eq!(s, "push"),
                    _ => panic!("expected Positional push"),
                }
                match &cmd.args[1] {
                    Arg::LongFlag(name) => assert_eq!(name, "force"),
                    _ => panic!("expected LongFlag"),
                }
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_long_flag_with_value() {
        let result = parse(r#"git commit --message="hello""#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "git");
                assert_eq!(cmd.args.len(), 2);
                match &cmd.args[1] {
                    Arg::Named { key, value } => {
                        assert_eq!(key, "message");
                        match value {
                            Expr::Literal(Value::String(s)) => assert_eq!(s, "hello"),
                            _ => panic!("expected String value"),
                        }
                    }
                    _ => panic!("expected Named from --flag=value"),
                }
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_mixed_flags_and_args() {
        let result = parse(r#"git commit -m "message" --amend"#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "git");
                assert_eq!(cmd.args.len(), 4);
                // commit (positional)
                assert!(matches!(&cmd.args[0], Arg::Positional(_)));
                // -m (short flag)
                match &cmd.args[1] {
                    Arg::ShortFlag(name) => assert_eq!(name, "m"),
                    _ => panic!("expected ShortFlag -m"),
                }
                // "message" (positional)
                assert!(matches!(&cmd.args[2], Arg::Positional(_)));
                // --amend (long flag)
                match &cmd.args[3] {
                    Arg::LongFlag(name) => assert_eq!(name, "amend"),
                    _ => panic!("expected LongFlag --amend"),
                }
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_redirect_stdout() {
        let result = parse("cmd > file");
        assert!(result.is_ok());
        let program = result.expect("ok");
        // Commands with redirects stay as Pipeline, not Command
        match &program.statements[0] {
            Stmt::Pipeline(p) => {
                assert_eq!(p.commands.len(), 1);
                let cmd = &p.commands[0];
                assert_eq!(cmd.redirects.len(), 1);
                assert!(matches!(cmd.redirects[0].kind, RedirectKind::StdoutOverwrite));
            }
            _ => panic!("expected Pipeline"),
        }
    }

    #[test]
    fn parse_var_ref() {
        let result = parse("echo ${VAR}");
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.args.len(), 1);
                assert!(matches!(&cmd.args[0], Arg::Positional(Expr::VarRef(_))));
            }
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn parse_multiple_statements() {
        let result = parse("a\nb\nc");
        assert!(result.is_ok());
        let program = result.expect("ok");
        let non_empty: Vec<_> = program.statements.iter().filter(|s| !matches!(s, Stmt::Empty)).collect();
        assert_eq!(non_empty.len(), 3);
    }

    #[test]
    fn parse_semicolon_separated() {
        let result = parse("a; b; c");
        assert!(result.is_ok());
        let program = result.expect("ok");
        let non_empty: Vec<_> = program.statements.iter().filter(|s| !matches!(s, Stmt::Empty)).collect();
        assert_eq!(non_empty.len(), 3);
    }

    #[test]
    fn parse_complex_pipeline() {
        let result = parse(r#"cat file | grep pattern="foo" | head count=10"#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Pipeline(p) => assert_eq!(p.commands.len(), 3),
            _ => panic!("expected Pipeline"),
        }
    }

    #[test]
    fn parse_json_as_string_arg() {
        // JSON arrays/objects should be passed as string arguments
        let result = parse(r#"cmd '[[1, 2], [3, 4]]'"#);
        assert!(result.is_ok());
    }

    #[test]
    fn parse_mixed_args() {
        let result = parse(r#"cmd pos1 key="val" pos2 num=42"#);
        assert!(result.is_ok());
        let program = result.expect("ok");
        match &program.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.args.len(), 4),
            _ => panic!("expected Command"),
        }
    }

    #[test]
    fn error_unterminated_string() {
        let result = parse(r#"echo "hello"#);
        assert!(result.is_err());
    }

    #[test]
    fn error_unterminated_var_ref() {
        let result = parse("echo ${VAR");
        assert!(result.is_err());
    }

    #[test]
    fn error_missing_fi() {
        let result = parse("if true; then echo");
        assert!(result.is_err());
    }

    #[test]
    fn error_missing_done() {
        let result = parse("for X in items; do echo");
        assert!(result.is_err());
    }

    #[test]
    fn parse_nested_cmd_subst() {
        // Nested command substitution is supported
        let result = parse("X=$(echo $(date))").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name, "X");
                match &a.value {
                    Expr::CommandSubst(outer) => {
                        assert_eq!(outer.commands[0].name, "echo");
                        // The argument should be another command substitution
                        match &outer.commands[0].args[0] {
                            Arg::Positional(Expr::CommandSubst(inner)) => {
                                assert_eq!(inner.commands[0].name, "date");
                            }
                            other => panic!("expected nested cmd subst, got {:?}", other),
                        }
                    }
                    other => panic!("expected cmd subst, got {:?}", other),
                }
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_deeply_nested_cmd_subst() {
        // Three levels deep
        let result = parse("X=$(a $(b $(c)))").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(level1) => {
                    assert_eq!(level1.commands[0].name, "a");
                    match &level1.commands[0].args[0] {
                        Arg::Positional(Expr::CommandSubst(level2)) => {
                            assert_eq!(level2.commands[0].name, "b");
                            match &level2.commands[0].args[0] {
                                Arg::Positional(Expr::CommandSubst(level3)) => {
                                    assert_eq!(level3.commands[0].name, "c");
                                }
                                other => panic!("expected level3 cmd subst, got {:?}", other),
                            }
                        }
                        other => panic!("expected level2 cmd subst, got {:?}", other),
                    }
                }
                other => panic!("expected cmd subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Value Preservation Tests - These test that actual values are captured
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn value_int_preserved() {
        let result = parse("X=42").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name, "X");
                match &a.value {
                    Expr::Literal(Value::Int(n)) => assert_eq!(*n, 42),
                    other => panic!("expected int literal, got {:?}", other),
                }
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn value_negative_int_preserved() {
        let result = parse("X=-99").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::Literal(Value::Int(n)) => assert_eq!(*n, -99),
                other => panic!("expected int, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn value_float_preserved() {
        let result = parse("PI=3.14").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::Literal(Value::Float(f)) => assert!((*f - 3.14).abs() < 0.001),
                other => panic!("expected float, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn value_string_preserved() {
        let result = parse(r#"echo "hello world""#).unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "echo");
                match &cmd.args[0] {
                    Arg::Positional(Expr::Literal(Value::String(s))) => {
                        assert_eq!(s, "hello world");
                    }
                    other => panic!("expected string arg, got {:?}", other),
                }
            }
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_string_with_escapes_preserved() {
        let result = parse(r#"echo "line1\nline2""#).unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::Literal(Value::String(s))) => {
                    assert_eq!(s, "line1\nline2");
                }
                other => panic!("expected string, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_command_name_preserved() {
        let result = parse("my-command").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.name, "my-command"),
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_assignment_name_preserved() {
        let result = parse("MY_VAR=1").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => assert_eq!(a.name, "MY_VAR"),
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn value_for_variable_preserved() {
        let result = parse("for ITEM in items; do echo; done").unwrap();
        match &result.statements[0] {
            Stmt::For(f) => assert_eq!(f.variable, "ITEM"),
            other => panic!("expected for, got {:?}", other),
        }
    }

    #[test]
    fn value_varref_name_preserved() {
        let result = parse("echo ${MESSAGE}").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::VarRef(path)) => {
                    assert_eq!(path.segments.len(), 1);
                    let VarSegment::Field(name) = &path.segments[0];
                    assert_eq!(name, "MESSAGE");
                }
                other => panic!("expected varref, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_varref_field_access_preserved() {
        let result = parse("echo ${RESULT.data}").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::VarRef(path)) => {
                    assert_eq!(path.segments.len(), 2);
                    let VarSegment::Field(a) = &path.segments[0];
                    let VarSegment::Field(b) = &path.segments[1];
                    assert_eq!(a, "RESULT");
                    assert_eq!(b, "data");
                }
                other => panic!("expected varref, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_varref_index_ignored() {
        // Index segments are no longer supported - they're filtered out by parse_varpath
        let result = parse("echo ${ITEMS[0]}").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::VarRef(path)) => {
                    // Index segment [0] is skipped, only ITEMS remains
                    assert_eq!(path.segments.len(), 1);
                    let VarSegment::Field(name) = &path.segments[0];
                    assert_eq!(name, "ITEMS");
                }
                other => panic!("expected varref, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_last_result_ref_preserved() {
        let result = parse("echo ${?.ok}").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::VarRef(path)) => {
                    assert_eq!(path.segments.len(), 2);
                    let VarSegment::Field(name) = &path.segments[0];
                    assert_eq!(name, "?");
                }
                other => panic!("expected varref, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_named_arg_preserved() {
        let result = parse("cmd count=42").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "cmd");
                match &cmd.args[0] {
                    Arg::Named { key, value } => {
                        assert_eq!(key, "count");
                        match value {
                            Expr::Literal(Value::Int(n)) => assert_eq!(*n, 42),
                            other => panic!("expected int, got {:?}", other),
                        }
                    }
                    other => panic!("expected named arg, got {:?}", other),
                }
            }
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn value_function_def_name_preserved() {
        let result = parse("greet() { echo }").unwrap();
        match &result.statements[0] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "greet");
                assert!(t.params.is_empty());
            }
            other => panic!("expected function def, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // New Feature Tests - Comparisons, Interpolation, Nested Structures
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_comparison_equals() {
        // Shell-compatible: use [[ ]] for comparisons
        let result = parse("if [[ ${X} == 5 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { left, op, right } => {
                        assert!(matches!(left.as_ref(), Expr::VarRef(_)));
                        assert_eq!(*op, TestCmpOp::Eq);
                        match right.as_ref() {
                            Expr::Literal(Value::Int(n)) => assert_eq!(*n, 5),
                            other => panic!("expected int, got {:?}", other),
                        }
                    }
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_not_equals() {
        let result = parse("if [[ ${X} != 0 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::NotEq),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_less_than() {
        let result = parse("if [[ ${COUNT} -lt 10 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::Lt),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_greater_than() {
        let result = parse("if [[ ${COUNT} -gt 0 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::Gt),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_less_equal() {
        let result = parse("if [[ ${X} -le 100 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::LtEq),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_greater_equal() {
        let result = parse("if [[ ${X} -ge 1 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::GtEq),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_regex_match() {
        let result = parse(r#"if [[ ${NAME} =~ "^test" ]]; then echo; fi"#).unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::Match),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_regex_not_match() {
        let result = parse(r#"if [[ ${NAME} !~ "^test" ]]; then echo; fi"#).unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { op, .. } => assert_eq!(*op, TestCmpOp::NotMatch),
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_string_interpolation() {
        let result = parse(r#"echo "Hello ${NAME}!""#).unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::Interpolated(parts)) => {
                    assert_eq!(parts.len(), 3);
                    match &parts[0] {
                        StringPart::Literal(s) => assert_eq!(s, "Hello "),
                        other => panic!("expected literal, got {:?}", other),
                    }
                    match &parts[1] {
                        StringPart::Var(path) => {
                            assert_eq!(path.segments.len(), 1);
                            let VarSegment::Field(name) = &path.segments[0];
                            assert_eq!(name, "NAME");
                        }
                        other => panic!("expected var, got {:?}", other),
                    }
                    match &parts[2] {
                        StringPart::Literal(s) => assert_eq!(s, "!"),
                        other => panic!("expected literal, got {:?}", other),
                    }
                }
                other => panic!("expected interpolated, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn parse_string_interpolation_multiple_vars() {
        let result = parse(r#"echo "${FIRST} and ${SECOND}""#).unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => match &cmd.args[0] {
                Arg::Positional(Expr::Interpolated(parts)) => {
                    // ${FIRST} + " and " + ${SECOND} = 3 parts
                    assert_eq!(parts.len(), 3);
                    assert!(matches!(&parts[0], StringPart::Var(_)));
                    assert!(matches!(&parts[1], StringPart::Literal(_)));
                    assert!(matches!(&parts[2], StringPart::Var(_)));
                }
                other => panic!("expected interpolated, got {:?}", other),
            },
            other => panic!("expected command, got {:?}", other),
        }
    }

    #[test]
    fn parse_empty_function_body() {
        let result = parse("empty() { }").unwrap();
        match &result.statements[0] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "empty");
                assert!(t.params.is_empty());
                assert!(t.body.is_empty());
            }
            other => panic!("expected function def, got {:?}", other),
        }
    }

    #[test]
    fn parse_bash_style_function() {
        let result = parse("function greet { echo hello }").unwrap();
        match &result.statements[0] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "greet");
                assert!(t.params.is_empty());
                assert_eq!(t.body.len(), 1);
            }
            other => panic!("expected function def, got {:?}", other),
        }
    }

    #[test]
    fn parse_comparison_string_values() {
        let result = parse(r#"if [[ ${STATUS} == "ok" ]]; then echo; fi"#).unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Test(test) => match test.as_ref() {
                    TestExpr::Comparison { left, op, right } => {
                        assert!(matches!(left.as_ref(), Expr::VarRef(_)));
                        assert_eq!(*op, TestCmpOp::Eq);
                        match right.as_ref() {
                            Expr::Literal(Value::String(s)) => assert_eq!(s, "ok"),
                            other => panic!("expected string, got {:?}", other),
                        }
                    }
                    other => panic!("expected comparison, got {:?}", other),
                },
                other => panic!("expected test expr, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Command Substitution Tests
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_cmd_subst_simple() {
        let result = parse("X=$(echo)").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name, "X");
                match &a.value {
                    Expr::CommandSubst(pipeline) => {
                        assert_eq!(pipeline.commands.len(), 1);
                        assert_eq!(pipeline.commands[0].name, "echo");
                    }
                    other => panic!("expected command subst, got {:?}", other),
                }
            }
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_with_args() {
        let result = parse(r#"X=$(fetch url="http://example.com")"#).unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(pipeline) => {
                    assert_eq!(pipeline.commands[0].name, "fetch");
                    assert_eq!(pipeline.commands[0].args.len(), 1);
                    match &pipeline.commands[0].args[0] {
                        Arg::Named { key, .. } => assert_eq!(key, "url"),
                        other => panic!("expected named arg, got {:?}", other),
                    }
                }
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_pipeline() {
        let result = parse("X=$(cat file | grep pattern)").unwrap();
        match &result.statements[0] {
            Stmt::Assignment(a) => match &a.value {
                Expr::CommandSubst(pipeline) => {
                    assert_eq!(pipeline.commands.len(), 2);
                    assert_eq!(pipeline.commands[0].name, "cat");
                    assert_eq!(pipeline.commands[1].name, "grep");
                }
                other => panic!("expected command subst, got {:?}", other),
            },
            other => panic!("expected assignment, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_in_condition() {
        // Shell-compatible: conditions are commands, not command substitutions
        let result = parse("if validate; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::Command(cmd) => {
                    assert_eq!(cmd.name, "validate");
                }
                other => panic!("expected command, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_cmd_subst_in_command_arg() {
        let result = parse("echo $(whoami)").unwrap();
        match &result.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "echo");
                match &cmd.args[0] {
                    Arg::Positional(Expr::CommandSubst(pipeline)) => {
                        assert_eq!(pipeline.commands[0].name, "whoami");
                    }
                    other => panic!("expected command subst, got {:?}", other),
                }
            }
            other => panic!("expected command, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Logical Operator Tests (&&, ||)
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_condition_and() {
        // Shell-compatible: commands chained with &&
        let result = parse("if check-a && check-b; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, right } => {
                    assert_eq!(*op, BinaryOp::And);
                    assert!(matches!(left.as_ref(), Expr::Command(_)));
                    assert!(matches!(right.as_ref(), Expr::Command(_)));
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_condition_or() {
        let result = parse("if try-a || try-b; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, right } => {
                    assert_eq!(*op, BinaryOp::Or);
                    assert!(matches!(left.as_ref(), Expr::Command(_)));
                    assert!(matches!(right.as_ref(), Expr::Command(_)));
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_condition_and_or_precedence() {
        // a && b || c should parse as (a && b) || c
        let result = parse("if cmd-a && cmd-b || cmd-c; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, right } => {
                    // Top level should be ||
                    assert_eq!(*op, BinaryOp::Or);
                    // Left side should be && expression
                    match left.as_ref() {
                        Expr::BinaryOp { op: inner_op, .. } => {
                            assert_eq!(*inner_op, BinaryOp::And);
                        }
                        other => panic!("expected binary op (&&), got {:?}", other),
                    }
                    // Right side should be command
                    assert!(matches!(right.as_ref(), Expr::Command(_)));
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_condition_multiple_and() {
        let result = parse("if cmd-a && cmd-b && cmd-c; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, .. } => {
                    assert_eq!(*op, BinaryOp::And);
                    // Left side should also be &&
                    match left.as_ref() {
                        Expr::BinaryOp { op: inner_op, .. } => {
                            assert_eq!(*inner_op, BinaryOp::And);
                        }
                        other => panic!("expected binary op, got {:?}", other),
                    }
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    #[test]
    fn parse_condition_mixed_comparison_and_logical() {
        // Shell-compatible: use [[ ]] for comparisons, && to chain them
        let result = parse("if [[ ${X} == 5 ]] && [[ ${Y} -gt 0 ]]; then echo; fi").unwrap();
        match &result.statements[0] {
            Stmt::If(if_stmt) => match if_stmt.condition.as_ref() {
                Expr::BinaryOp { left, op, right } => {
                    assert_eq!(*op, BinaryOp::And);
                    // Left: [[ ${X} == 5 ]]
                    match left.as_ref() {
                        Expr::Test(test) => match test.as_ref() {
                            TestExpr::Comparison { op: left_op, .. } => {
                                assert_eq!(*left_op, TestCmpOp::Eq);
                            }
                            other => panic!("expected comparison, got {:?}", other),
                        },
                        other => panic!("expected test, got {:?}", other),
                    }
                    // Right: [[ ${Y} -gt 0 ]]
                    match right.as_ref() {
                        Expr::Test(test) => match test.as_ref() {
                            TestExpr::Comparison { op: right_op, .. } => {
                                assert_eq!(*right_op, TestCmpOp::Gt);
                            }
                            other => panic!("expected comparison, got {:?}", other),
                        },
                        other => panic!("expected test, got {:?}", other),
                    }
                }
                other => panic!("expected binary op, got {:?}", other),
            },
            other => panic!("expected if, got {:?}", other),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Integration Tests - Complete Scripts
    // ═══════════════════════════════════════════════════════════════════════════

    /// Level 1: Linear script using core features
    #[test]
    fn script_level1_linear() {
        let script = r#"
NAME="kaish"
VERSION=1
TIMEOUT=30
ITEMS="alpha beta gamma"

echo "Starting ${NAME} v${VERSION}"
cat "README.md" | grep pattern="install" | head count=5
fetch url="https://api.example.com/status" timeout=${TIMEOUT} > "/tmp/status.json"
echo "Items: ${ITEMS}"
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        assert_eq!(stmts.len(), 8);
        assert!(matches!(stmts[0], Stmt::Assignment(_)));  // set NAME
        assert!(matches!(stmts[1], Stmt::Assignment(_)));  // set VERSION
        assert!(matches!(stmts[2], Stmt::Assignment(_)));  // set TIMEOUT
        assert!(matches!(stmts[3], Stmt::Assignment(_)));  // set ITEMS
        assert!(matches!(stmts[4], Stmt::Command(_)));     // echo "Starting..."
        assert!(matches!(stmts[5], Stmt::Pipeline(_)));    // cat | grep | head
        assert!(matches!(stmts[6], Stmt::Pipeline(_)));    // fetch (with redirect - Pipeline since it has redirects)
        assert!(matches!(stmts[7], Stmt::Command(_)));     // echo "Items: ${ITEMS}"
    }

    /// Level 2: Script with conditionals (shell-compatible syntax)
    #[test]
    fn script_level2_branching() {
        let script = r#"
RESULT=$(validate "input.json")

if [[ ${RESULT.ok} == true ]]; then
    echo "Validation passed"
    process "input.json" > "output.json"
else
    echo "Validation failed: ${RESULT.err}"
fi

if [[ ${COUNT} -gt 0 ]] && [[ ${COUNT} -le 100 ]]; then
    echo "Count in valid range"
fi

if check-network || check-cache; then
    fetch url=${URL}
fi
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        assert_eq!(stmts.len(), 4);

        // First: assignment with command substitution
        match stmts[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name, "RESULT");
                assert!(matches!(&a.value, Expr::CommandSubst(_)));
            }
            other => panic!("expected assignment, got {:?}", other),
        }

        // Second: if/else
        match stmts[1] {
            Stmt::If(if_stmt) => {
                assert_eq!(if_stmt.then_branch.len(), 2);
                assert!(if_stmt.else_branch.is_some());
                assert_eq!(if_stmt.else_branch.as_ref().unwrap().len(), 1);
            }
            other => panic!("expected if, got {:?}", other),
        }

        // Third: if with && condition
        match stmts[2] {
            Stmt::If(if_stmt) => {
                match if_stmt.condition.as_ref() {
                    Expr::BinaryOp { op, .. } => assert_eq!(*op, BinaryOp::And),
                    other => panic!("expected && condition, got {:?}", other),
                }
            }
            other => panic!("expected if, got {:?}", other),
        }

        // Fourth: if with || of commands
        match stmts[3] {
            Stmt::If(if_stmt) => {
                match if_stmt.condition.as_ref() {
                    Expr::BinaryOp { op, left, right } => {
                        assert_eq!(*op, BinaryOp::Or);
                        assert!(matches!(left.as_ref(), Expr::Command(_)));
                        assert!(matches!(right.as_ref(), Expr::Command(_)));
                    }
                    other => panic!("expected || condition, got {:?}", other),
                }
            }
            other => panic!("expected if, got {:?}", other),
        }
    }

    /// Level 3: Script with loops and function definitions
    #[test]
    fn script_level3_loops_and_functions() {
        let script = r#"
greet() {
    echo "Hello, $1!"
}

fetch_all() {
    for URL in $@; do
        fetch url=${URL}
    done
}

USERS="alice bob charlie"

for USER in ${USERS}; do
    greet ${USER}
    if [[ ${USER} == "bob" ]]; then
        echo "Found Bob!"
    fi
done

long-running-task &
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        assert_eq!(stmts.len(), 5);

        // First function def
        match stmts[0] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "greet");
                assert!(t.params.is_empty());
            }
            other => panic!("expected function def, got {:?}", other),
        }

        // Second function def with nested for loop
        match stmts[1] {
            Stmt::ToolDef(t) => {
                assert_eq!(t.name, "fetch_all");
                assert_eq!(t.body.len(), 1);
                assert!(matches!(&t.body[0], Stmt::For(_)));
            }
            other => panic!("expected function def, got {:?}", other),
        }

        // Assignment
        assert!(matches!(stmts[2], Stmt::Assignment(_)));

        // For loop with nested if
        match stmts[3] {
            Stmt::For(f) => {
                assert_eq!(f.variable, "USER");
                assert_eq!(f.body.len(), 2);
                assert!(matches!(&f.body[0], Stmt::Command(_)));
                assert!(matches!(&f.body[1], Stmt::If(_)));
            }
            other => panic!("expected for loop, got {:?}", other),
        }

        // Background job
        match stmts[4] {
            Stmt::Pipeline(p) => {
                assert!(p.background);
                assert_eq!(p.commands[0].name, "long-running-task");
            }
            other => panic!("expected pipeline (background), got {:?}", other),
        }
    }

    /// Level 4: Complex nested control flow (shell-compatible syntax)
    #[test]
    fn script_level4_complex_nesting() {
        let script = r#"
RESULT=$(cat "config.json" | jq query=".servers" | validate schema="server-schema.json")

if ping host=${HOST} && [[ ${RESULT} == true ]]; then
    for SERVER in "prod-1 prod-2"; do
        deploy target=${SERVER} port=8080
        if [[ ${?.code} != 0 ]]; then
            notify channel="ops" message="Deploy failed"
        fi
    done
fi
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        assert_eq!(stmts.len(), 2);

        // Command substitution with pipeline
        match stmts[0] {
            Stmt::Assignment(a) => {
                assert_eq!(a.name, "RESULT");
                match &a.value {
                    Expr::CommandSubst(pipeline) => {
                        assert_eq!(pipeline.commands.len(), 3);
                    }
                    other => panic!("expected command subst, got {:?}", other),
                }
            }
            other => panic!("expected assignment, got {:?}", other),
        }

        // If with && condition, containing for loop with nested if
        match stmts[1] {
            Stmt::If(if_stmt) => {
                match if_stmt.condition.as_ref() {
                    Expr::BinaryOp { op, .. } => assert_eq!(*op, BinaryOp::And),
                    other => panic!("expected && condition, got {:?}", other),
                }
                assert_eq!(if_stmt.then_branch.len(), 1);
                match &if_stmt.then_branch[0] {
                    Stmt::For(f) => {
                        assert_eq!(f.body.len(), 2);
                        assert!(matches!(&f.body[1], Stmt::If(_)));
                    }
                    other => panic!("expected for in if body, got {:?}", other),
                }
            }
            other => panic!("expected if, got {:?}", other),
        }
    }

    /// Level 5: Edge cases and parser stress test
    #[test]
    fn script_level5_edge_cases() {
        let script = r#"
echo ""
echo "quotes: \"nested\" here"
echo "escapes: \n\t\r\\"
echo "unicode: \u2764"

X=-99999
Y=3.14159265358979
Z=-0.001

cmd a=1 b="two" c=true d=false e=null

if true; then
    if false; then
        echo "inner"
    else
        echo "else"
    fi
fi

for I in "a b c"; do
    echo ${I}
done

no_params() {
    echo "no params"
}

function all_args {
    echo "args: $@"
}

a | b | c | d | e &
cmd 2> "errors.log"
cmd &> "all.log"
cmd >> "append.log"
cmd < "input.txt"
"#;
        let result = parse(script).unwrap();
        let stmts: Vec<_> = result.statements.iter()
            .filter(|s| !matches!(s, Stmt::Empty))
            .collect();

        // Verify it parses without error
        assert!(stmts.len() >= 10, "expected many statements, got {}", stmts.len());

        // Background pipeline
        let bg_stmt = stmts.iter().find(|s| matches!(s, Stmt::Pipeline(p) if p.background));
        assert!(bg_stmt.is_some(), "expected background pipeline");

        match bg_stmt.unwrap() {
            Stmt::Pipeline(p) => {
                assert_eq!(p.commands.len(), 5);
                assert!(p.background);
            }
            _ => unreachable!(),
        }
    }

    // ═══════════════════════════════════════════════════════════════════════════
    // Edge Case Tests: Ambiguity Resolution
    // ═══════════════════════════════════════════════════════════════════════════

    #[test]
    fn parse_keyword_as_variable_rejected() {
        // Keywords CANNOT be used as variable names - this is intentional
        // to avoid ambiguity. Use different names instead.
        let result = parse(r#"if="value""#);
        assert!(result.is_err(), "if= should fail - 'if' is a keyword");

        let result = parse("while=true");
        assert!(result.is_err(), "while= should fail - 'while' is a keyword");

        let result = parse(r#"then="next""#);
        assert!(result.is_err(), "then= should fail - 'then' is a keyword");
    }

    #[test]
    fn parse_set_command_with_flag() {
        let result = parse("set -e");
        assert!(result.is_ok(), "failed to parse set -e: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "set");
                assert_eq!(cmd.args.len(), 1);
                match &cmd.args[0] {
                    Arg::ShortFlag(f) => assert_eq!(f, "e"),
                    other => panic!("expected ShortFlag, got {:?}", other),
                }
            }
            other => panic!("expected Command, got {:?}", other),
        }
    }

    #[test]
    fn parse_set_command_no_args() {
        let result = parse("set");
        assert!(result.is_ok(), "failed to parse set: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "set");
                assert_eq!(cmd.args.len(), 0);
            }
            other => panic!("expected Command, got {:?}", other),
        }
    }

    #[test]
    fn parse_set_assignment_vs_command() {
        // X=5 should be assignment
        let result = parse("X=5");
        assert!(result.is_ok());
        let program = result.unwrap();
        assert!(matches!(&program.statements[0], Stmt::Assignment(_)));

        // set -e should be command
        let result = parse("set -e");
        assert!(result.is_ok());
        let program = result.unwrap();
        assert!(matches!(&program.statements[0], Stmt::Command(_)));
    }

    #[test]
    fn parse_true_as_command() {
        let result = parse("true");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.name, "true"),
            other => panic!("expected Command(true), got {:?}", other),
        }
    }

    #[test]
    fn parse_false_as_command() {
        let result = parse("false");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => assert_eq!(cmd.name, "false"),
            other => panic!("expected Command(false), got {:?}", other),
        }
    }

    #[test]
    fn parse_dot_as_source_alias() {
        let result = parse(". script.kai");
        assert!(result.is_ok(), "failed to parse . script.kai: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, ".");
                assert_eq!(cmd.args.len(), 1);
            }
            other => panic!("expected Command(.), got {:?}", other),
        }
    }

    #[test]
    fn parse_source_command() {
        let result = parse("source utils.kai");
        assert!(result.is_ok(), "failed to parse source: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Command(cmd) => {
                assert_eq!(cmd.name, "source");
                assert_eq!(cmd.args.len(), 1);
            }
            other => panic!("expected Command(source), got {:?}", other),
        }
    }

    #[test]
    fn parse_test_expr_file_test() {
        // Paths must be quoted strings in test expressions
        let result = parse(r#"[[ -f "/path/file" ]]"#);
        assert!(result.is_ok(), "failed to parse file test: {:?}", result);
    }

    #[test]
    fn parse_test_expr_comparison() {
        let result = parse(r#"[[ $X == "value" ]]"#);
        assert!(result.is_ok(), "failed to parse comparison test: {:?}", result);
    }

    #[test]
    fn parse_test_expr_single_eq() {
        // = and == are equivalent inside [[ ]] (matching bash behavior)
        let result = parse(r#"[[ $X = "value" ]]"#);
        assert!(result.is_ok(), "failed to parse single-= comparison: {:?}", result);
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Test(TestExpr::Comparison { op, .. }) => {
                assert_eq!(op, &TestCmpOp::Eq);
            }
            other => panic!("expected Test(Comparison), got {:?}", other),
        }
    }

    #[test]
    fn parse_while_loop() {
        let result = parse("while true; do echo; done");
        assert!(result.is_ok(), "failed to parse while loop: {:?}", result);
        let program = result.unwrap();
        assert!(matches!(&program.statements[0], Stmt::While(_)));
    }

    #[test]
    fn parse_break_with_level() {
        let result = parse("break 2");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Break(Some(n)) => assert_eq!(*n, 2),
            other => panic!("expected Break(2), got {:?}", other),
        }
    }

    #[test]
    fn parse_continue_with_level() {
        let result = parse("continue 3");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Continue(Some(n)) => assert_eq!(*n, 3),
            other => panic!("expected Continue(3), got {:?}", other),
        }
    }

    #[test]
    fn parse_exit_with_code() {
        let result = parse("exit 1");
        assert!(result.is_ok());
        let program = result.unwrap();
        match &program.statements[0] {
            Stmt::Exit(Some(expr)) => {
                match expr.as_ref() {
                    Expr::Literal(Value::Int(n)) => assert_eq!(*n, 1),
                    other => panic!("expected Int(1), got {:?}", other),
                }
            }
            other => panic!("expected Exit(1), got {:?}", other),
        }
    }
}
