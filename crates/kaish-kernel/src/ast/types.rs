//! AST type definitions.

use std::fmt;

/// A complete kaish program is a sequence of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

/// A single statement in kaish.
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Variable assignment: `NAME=value` or `local NAME = value`
    Assignment(Assignment),
    /// Simple command: `tool arg1 arg2`
    Command(Command),
    /// Pipeline: `a | b | c`
    Pipeline(Pipeline),
    /// Conditional: `if cond; then ...; fi`
    If(IfStmt),
    /// Loop: `for X in items; do ...; done`
    For(ForLoop),
    /// While loop: `while cond; do ...; done`
    While(WhileLoop),
    /// Case statement: `case expr in pattern) ... ;; esac`
    Case(CaseStmt),
    /// Break out of loop: `break` or `break N`
    Break(Option<usize>),
    /// Continue to next iteration: `continue` or `continue N`
    Continue(Option<usize>),
    /// Return from tool: `return` or `return expr`
    Return(Option<Box<Expr>>),
    /// Exit the script: `exit` or `exit code`
    Exit(Option<Box<Expr>>),
    /// Tool definition: `tool name(params) { body }`
    ToolDef(ToolDef),
    /// Test expression: `[[ -f path ]]` or `[[ $X == "value" ]]`
    Test(TestExpr),
    /// Statement chain with `&&`: run right only if left succeeds
    AndChain { left: Box<Stmt>, right: Box<Stmt> },
    /// Statement chain with `||`: run right only if left fails
    OrChain { left: Box<Stmt>, right: Box<Stmt> },
    /// Inline env prefix: `NAME=value... command`. The assignments are exported
    /// for the duration of `body` only (bash-style command-scoped environment)
    /// and do not persist after it — distinct from a plain `Assignment`, which
    /// is persistent. `body` is always a command or pipeline.
    EnvScoped { assignments: Vec<Assignment>, body: Box<Stmt> },
    /// Empty statement (newline or semicolon only)
    Empty,
}

impl Stmt {
    /// Human-readable variant name for tracing spans.
    pub fn kind_name(&self) -> &'static str {
        match self {
            Stmt::Assignment(_) => "assignment",
            Stmt::Command(_) => "command",
            Stmt::Pipeline(_) => "pipeline",
            Stmt::If(_) => "if",
            Stmt::For(_) => "for",
            Stmt::While(_) => "while",
            Stmt::Case(_) => "case",
            Stmt::Break(_) => "break",
            Stmt::Continue(_) => "continue",
            Stmt::Return(_) => "return",
            Stmt::Exit(_) => "exit",
            Stmt::ToolDef(_) => "tooldef",
            Stmt::Test(_) => "test",
            Stmt::AndChain { .. } => "and_chain",
            Stmt::OrChain { .. } => "or_chain",
            Stmt::EnvScoped { .. } => "env_scoped",
            Stmt::Empty => "empty",
        }
    }
}

/// Variable assignment: `NAME=value` (bash-style) or `local NAME = value` (scoped)
#[derive(Debug, Clone, PartialEq)]
pub struct Assignment {
    pub name: String,
    pub value: Expr,
    /// True if declared with `local` keyword (explicit local scope)
    pub local: bool,
}

/// A command invocation with arguments and redirections.
#[derive(Debug, Clone, PartialEq)]
pub struct Command {
    pub name: String,
    pub args: Vec<Arg>,
    pub redirects: Vec<Redirect>,
}

/// A pipeline of commands connected by pipes.
#[derive(Debug, Clone, PartialEq)]
pub struct Pipeline {
    pub commands: Vec<Command>,
    pub background: bool,
}

/// Conditional statement.
#[derive(Debug, Clone, PartialEq)]
pub struct IfStmt {
    pub condition: Box<Expr>,
    pub then_branch: Vec<Stmt>,
    pub else_branch: Option<Vec<Stmt>>,
}

/// For loop over items.
#[derive(Debug, Clone, PartialEq)]
pub struct ForLoop {
    pub variable: String,
    /// Items to iterate over. Each is evaluated, then word-split for iteration.
    pub items: Vec<Expr>,
    pub body: Vec<Stmt>,
}

/// While loop with condition.
#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoop {
    pub condition: Box<Expr>,
    pub body: Vec<Stmt>,
}

/// Case statement for pattern matching.
///
/// ```kaish
/// case $VAR in
///     pattern1) commands ;;
///     pattern2|pattern3) commands ;;
///     *) default ;;
/// esac
/// ```
#[derive(Debug, Clone, PartialEq)]
pub struct CaseStmt {
    /// The expression to match against
    pub expr: Expr,
    /// The pattern branches
    pub branches: Vec<CaseBranch>,
}

/// A single branch in a case statement.
#[derive(Debug, Clone, PartialEq)]
pub struct CaseBranch {
    /// Glob patterns to match (separated by `|`)
    pub patterns: Vec<String>,
    /// Commands to execute if matched
    pub body: Vec<Stmt>,
}

/// User-defined tool.
#[derive(Debug, Clone, PartialEq)]
pub struct ToolDef {
    pub name: String,
    pub params: Vec<ParamDef>,
    pub body: Vec<Stmt>,
}

/// Parameter definition for a tool.
#[derive(Debug, Clone, PartialEq)]
pub struct ParamDef {
    pub name: String,
    pub param_type: Option<ParamType>,
    pub default: Option<Expr>,
}

/// Parameter type annotation.
#[derive(Debug, Clone, PartialEq)]
pub enum ParamType {
    String,
    Int,
    Float,
    Bool,
}

/// A command argument (positional or named).
#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    /// Positional argument: `value`
    Positional(Expr),
    /// Long flag with attached value: `--key=value`. Always routes through
    /// `tool_args.named` regardless of the receiving command.
    Named { key: String, value: Expr },
    /// Bareword shell-assignment in argv position: `key=value`.
    ///
    /// Only commands on the kernel's shell-assignment allowlist (`export`,
    /// `alias`) consume this as a named arg; for every other command it's
    /// stringified to a positional `"key=value"`. This matches bash:
    /// `cat foo=bar` opens a file named `foo=bar`, not a magical key=value.
    WordAssign { key: String, value: Expr },
    /// Short flag: `-l`, `-v` (boolean flag)
    ShortFlag(String),
    /// Long flag: `--force`, `--verbose` (boolean flag)
    LongFlag(String),
    /// Double-dash marker: `--` - signals end of flags
    DoubleDash,
}

/// I/O redirection.
#[derive(Debug, Clone, PartialEq)]
pub struct Redirect {
    pub kind: RedirectKind,
    pub target: Expr,
}

/// Type of redirection.
#[derive(Debug, Clone, PartialEq)]
pub enum RedirectKind {
    /// `>` stdout to file (overwrite)
    StdoutOverwrite,
    /// `>>` stdout to file (append)
    StdoutAppend,
    /// `<` stdin from file
    Stdin,
    /// `<<EOF ... EOF` stdin from here-doc
    HereDoc,
    /// `<<< word` stdin from here-string (bash-style)
    HereString,
    /// `2>` stderr to file
    Stderr,
    /// `&>` both stdout and stderr to file
    Both,
    /// `2>&1` merge stderr into stdout
    MergeStderr,
    /// `1>&2` or `>&2` merge stdout into stderr
    MergeStdout,
}

/// A `StringPart` together with its byte offset in the original source.
///
/// Used by [`Expr::HereDocBody`] so the validator and interpreter can attribute
/// diagnostics to a precise location inside an interpolated heredoc body.
/// Double-quoted strings continue to use the spanless [`Expr::Interpolated`];
/// universal spanning is a separate, larger refactor (see plan
/// `make-heredocs-precious-puzzle`).
#[derive(Debug, Clone, PartialEq)]
pub struct SpannedPart {
    /// The part itself.
    pub part: StringPart,
    /// Byte offset of this part in the original source string.
    pub offset: usize,
    /// Byte length of the part's source representation.
    pub len: usize,
}

/// An expression that evaluates to a value.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literal value
    Literal(Value),
    /// Variable reference: `${VAR}` or `${VAR.field}` or `$VAR`
    VarRef(VarPath),
    /// String with interpolation: `"hello ${NAME}"` or `"hello $NAME"`
    Interpolated(Vec<StringPart>),
    /// Interpolated heredoc body with per-part spans for diagnostic precision.
    ///
    /// Heredoc bodies use this variant; double-quoted strings still use
    /// `Interpolated` to keep the existing path untouched. `strip_tabs` is
    /// `true` for the `<<-EOF` form — leading tabs on each body line are
    /// stripped from `StringPart::Literal` content at materialization time
    /// (POSIX semantics); offsets in `parts` reference the verbatim source
    /// so spans remain meaningful.
    HereDocBody {
        parts: Vec<SpannedPart>,
        strip_tabs: bool,
    },
    /// Binary operation: `a && b`, `a || b`
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    /// Command substitution: `$(...)` — runs a statement block (the full grammar:
    /// pipelines, `&&`/`||` chains, `;`/newline sequences, `#` comments) and
    /// returns its accumulated stdout. A single `$(cmd)` is a one-statement block.
    CommandSubst(Vec<Stmt>),
    /// Test expression: `[[ -f path ]]` or `[[ $X == "value" ]]`
    Test(Box<TestExpr>),
    /// Positional parameter: `$0` through `$9`
    Positional(usize),
    /// All positional arguments: `$@`
    AllArgs,
    /// Argument count: `$#`
    ArgCount,
    /// Variable string length: `${#VAR}`
    VarLength(String),
    /// Variable with default: `${VAR:-default}` - use default if VAR is unset or empty
    /// The default can contain nested variable expansions and command substitutions
    VarWithDefault { name: String, default: Vec<StringPart> },
    /// Arithmetic expansion: `$((expr))` - evaluates to integer
    Arithmetic(String),
    /// Command as condition: `if grep -q pattern file; then` - exit code determines truthiness
    Command(Command),
    /// Last exit code: `$?`
    LastExitCode,
    /// Current shell PID: `$$`
    CurrentPid,
    /// Bare glob pattern: `*.txt`, `src/**/*.rs` — expanded during arg building
    GlobPattern(String),
}

/// Test expression for `[[ ... ]]` conditionals.
#[derive(Debug, Clone, PartialEq)]
pub enum TestExpr {
    /// File test: `[[ -f path ]]`, `[[ -d path ]]`, etc.
    FileTest { op: FileTestOp, path: Box<Expr> },
    /// String test: `[[ -z str ]]`, `[[ -n str ]]`
    StringTest { op: StringTestOp, value: Box<Expr> },
    /// Comparison: `[[ $X == "value" ]]`, `[[ $NUM -gt 5 ]]`
    Comparison { left: Box<Expr>, op: TestCmpOp, right: Box<Expr> },
    /// Logical AND: `[[ -f a && -d b ]]` (short-circuit evaluation)
    And { left: Box<TestExpr>, right: Box<TestExpr> },
    /// Logical OR: `[[ -f a || -d b ]]` (short-circuit evaluation)
    Or { left: Box<TestExpr>, right: Box<TestExpr> },
    /// Logical NOT: `[[ ! -f file ]]`
    Not { expr: Box<TestExpr> },
    /// Collection membership: `[[ e in $list ]]` (element) / `[[ k in $record ]]`
    /// (key). A scalar/string RHS is a loud error — see `docs/arrays-and-hashes.md`.
    In { left: Box<Expr>, right: Box<Expr> },
    /// Negated membership: `[[ e not in $coll ]]`.
    NotIn { left: Box<Expr>, right: Box<Expr> },
}

/// File test operators for `[[ ]]`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FileTestOp {
    /// `-e` - exists
    Exists,
    /// `-f` - is regular file
    IsFile,
    /// `-d` - is directory
    IsDir,
    /// `-r` - is readable
    Readable,
    /// `-w` - is writable
    Writable,
    /// `-x` - is executable
    Executable,
}

/// String test operators for `[[ ]]`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum StringTestOp {
    /// `-z` - string is empty
    IsEmpty,
    /// `-n` - string is non-empty
    IsNonEmpty,
}

/// Comparison operators for `[[ ]]` tests.
///
/// Mirrors POSIX `[[ ]]` semantics: `==`/`!=`/`>`/`<`/`>=`/`<=` are string
/// (lexicographic) comparisons, while `-eq`/`-ne`/`-gt`/`-lt`/`-ge`/`-le`
/// are arithmetic comparisons that coerce string operands to numbers.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestCmpOp {
    /// `==` / `=` — string equality
    Eq,
    /// `!=` — string inequality
    NotEq,
    /// `=~` — regex match
    Match,
    /// `!~` — regex not match
    NotMatch,
    /// `>` — string greater than (lexicographic)
    Gt,
    /// `<` — string less than (lexicographic)
    Lt,
    /// `>=` — string greater than or equal (lexicographic)
    GtEq,
    /// `<=` — string less than or equal (lexicographic)
    LtEq,
    /// `-eq` — numeric equality
    NumEq,
    /// `-ne` — numeric inequality
    NumNotEq,
    /// `-gt` — numeric greater than
    NumGt,
    /// `-lt` — numeric less than
    NumLt,
    /// `-ge` — numeric greater than or equal
    NumGtEq,
    /// `-le` — numeric less than or equal
    NumLtEq,
}

// Value lives in kaish-types.
pub use kaish_types::Value;

/// Variable reference path: `${VAR}`, `${VAR[0]}`, `${r[key]}`, `${a[b][c]}`.
///
/// The first segment is always the root variable name (`Field`); the rest are
/// bracket subscripts. `$?` resolves to the previous command's exit code as an
/// int (bare only — `${?.field}` is rejected). Access is brackets-only: a
/// dotted `${VAR.field}` resolves to a loud error suggesting `${VAR[field]}`.
#[derive(Debug, Clone, PartialEq)]
pub struct VarPath {
    pub segments: Vec<VarSegment>,
}

impl VarPath {
    /// Create a simple variable reference with just a name.
    pub fn simple(name: impl Into<String>) -> Self {
        Self {
            segments: vec![VarSegment::Field(name.into())],
        }
    }
}

/// A segment in a variable path.
///
/// The first segment of a path is the root name, carried as `Field`. Every
/// later segment is a bracket subscript. A `Field` in a non-root position
/// represents a dotted `.field` access, which — kaish being brackets-only —
/// resolves to a loud error with the bracket form in the message.
#[derive(Debug, Clone, PartialEq)]
pub enum VarSegment {
    /// The root variable name, or (illegally, past the root) a dotted `.field`.
    Field(String),
    /// Integer subscript `[0]` / `[-1]` — indexes a list (negative from the end).
    Index(i64),
    /// Literal key `[bareword]` or `["quoted key"]` — keys a record.
    Key(String),
    /// Dynamic subscript `[$var]` — the named variable's value is the key
    /// (record) or index (list) at resolution time. Holds the variable name.
    Dynamic(String),
    /// Slice `[a:b]` — end-exclusive, yields a list. Either bound may be omitted.
    Slice(Option<i64>, Option<i64>),
}

/// Part of an interpolated string.
#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    /// Literal text
    Literal(String),
    /// Variable interpolation: `${VAR}` or `$VAR`
    Var(VarPath),
    /// Variable with default: `${VAR:-default}` where default can contain nested expansions
    VarWithDefault { name: String, default: Vec<StringPart> },
    /// Variable string length: `${#VAR}`
    VarLength(String),
    /// Positional parameter: `$0`, `$1`, ..., `$9`
    Positional(usize),
    /// All arguments: `$@`
    AllArgs,
    /// Argument count: `$#`
    ArgCount,
    /// Arithmetic expansion: `$((expr))`
    Arithmetic(String),
    /// Command substitution: `$(...)` embedded in a string — runs a statement
    /// block (full grammar; see `Expr::CommandSubst`) and inlines its stdout.
    CommandSubst(Vec<Stmt>),
    /// Last exit code: `$?`
    LastExitCode,
    /// Current shell PID: `$$`
    CurrentPid,
}

/// Binary operators used to chain command/test conditions with `&&` / `||`.
///
/// Value-level comparisons (`==`, `-eq`, `-gt`, …) live on
/// [`TestCmpOp`] inside `[[ ]]` and are not part of this enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// `&&` - logical and (short-circuit)
    And,
    /// `||` - logical or (short-circuit)
    Or,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
        }
    }
}

impl fmt::Display for RedirectKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RedirectKind::StdoutOverwrite => write!(f, ">"),
            RedirectKind::StdoutAppend => write!(f, ">>"),
            RedirectKind::Stdin => write!(f, "<"),
            RedirectKind::HereDoc => write!(f, "<<"),
            RedirectKind::HereString => write!(f, "<<<"),
            RedirectKind::Stderr => write!(f, "2>"),
            RedirectKind::Both => write!(f, "&>"),
            RedirectKind::MergeStderr => write!(f, "2>&1"),
            RedirectKind::MergeStdout => write!(f, "1>&2"),
        }
    }
}

impl fmt::Display for FileTestOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FileTestOp::Exists => write!(f, "-e"),
            FileTestOp::IsFile => write!(f, "-f"),
            FileTestOp::IsDir => write!(f, "-d"),
            FileTestOp::Readable => write!(f, "-r"),
            FileTestOp::Writable => write!(f, "-w"),
            FileTestOp::Executable => write!(f, "-x"),
        }
    }
}

impl fmt::Display for StringTestOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StringTestOp::IsEmpty => write!(f, "-z"),
            StringTestOp::IsNonEmpty => write!(f, "-n"),
        }
    }
}

impl fmt::Display for TestCmpOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TestCmpOp::Eq => write!(f, "=="),
            TestCmpOp::NotEq => write!(f, "!="),
            TestCmpOp::Match => write!(f, "=~"),
            TestCmpOp::NotMatch => write!(f, "!~"),
            TestCmpOp::Gt => write!(f, ">"),
            TestCmpOp::Lt => write!(f, "<"),
            TestCmpOp::GtEq => write!(f, ">="),
            TestCmpOp::LtEq => write!(f, "<="),
            TestCmpOp::NumEq => write!(f, "-eq"),
            TestCmpOp::NumNotEq => write!(f, "-ne"),
            TestCmpOp::NumGt => write!(f, "-gt"),
            TestCmpOp::NumLt => write!(f, "-lt"),
            TestCmpOp::NumGtEq => write!(f, "-ge"),
            TestCmpOp::NumLtEq => write!(f, "-le"),
        }
    }
}
