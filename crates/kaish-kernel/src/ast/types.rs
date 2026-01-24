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
    /// Empty statement (newline or semicolon only)
    Empty,
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
    /// Named argument: `key=value`
    Named { key: String, value: Expr },
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
    /// `2>` stderr to file
    Stderr,
    /// `&>` both stdout and stderr to file
    Both,
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
    /// Binary operation: `a && b`, `a || b`
    BinaryOp {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
    },
    /// Command substitution: `$(pipeline)` - runs a pipeline and returns its result
    CommandSubst(Box<Pipeline>),
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
    VarWithDefault { name: String, default: String },
    /// Arithmetic expansion: `$((expr))` - evaluates to integer
    Arithmetic(String),
    /// Command as condition: `if grep -q pattern file; then` - exit code determines truthiness
    Command(Command),
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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TestCmpOp {
    /// `==` - string equality
    Eq,
    /// `!=` - string inequality
    NotEq,
    /// `=~` - regex match
    Match,
    /// `!~` - regex not match
    NotMatch,
    /// `-gt` - greater than (numeric)
    Gt,
    /// `-lt` - less than (numeric)
    Lt,
    /// `-ge` - greater than or equal (numeric)
    GtEq,
    /// `-le` - less than or equal (numeric)
    LtEq,
}

/// A literal value.
///
/// Note: Arrays and objects are intentionally not supported. JSON data from
/// MCP tools is stored as strings and processed with `jq`. For complex data
/// manipulation, use Rhai integration.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Int(i64),
    Float(f64),
    String(String),
}

/// Variable reference path: `${VAR}` or `${?.field}` for special variables.
///
/// Simple variable references support only field access for special variables
/// like `$?`. Array indexing is not supported - use `jq` for JSON processing.
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
#[derive(Debug, Clone, PartialEq)]
pub enum VarSegment {
    /// Field access: `.field` or initial name
    /// Only supported for special variables like `$?`
    Field(String),
}

/// Part of an interpolated string.
#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    /// Literal text
    Literal(String),
    /// Variable interpolation: `${VAR}` or `$VAR`
    Var(VarPath),
    /// Variable with default: `${VAR:-default}`
    VarWithDefault { name: String, default: String },
    /// Variable string length: `${#VAR}`
    VarLength(String),
}

/// Binary operators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// `&&` - logical and (short-circuit)
    And,
    /// `||` - logical or (short-circuit)
    Or,
    /// `==` - equality
    Eq,
    /// `!=` - inequality
    NotEq,
    /// `=~` - regex match
    Match,
    /// `!~` - regex not match
    NotMatch,
    /// `<` - less than
    Lt,
    /// `>` - greater than
    Gt,
    /// `<=` - less than or equal
    LtEq,
    /// `>=` - greater than or equal
    GtEq,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::And => write!(f, "&&"),
            BinaryOp::Or => write!(f, "||"),
            BinaryOp::Eq => write!(f, "=="),
            BinaryOp::NotEq => write!(f, "!="),
            BinaryOp::Match => write!(f, "=~"),
            BinaryOp::NotMatch => write!(f, "!~"),
            BinaryOp::Lt => write!(f, "<"),
            BinaryOp::Gt => write!(f, ">"),
            BinaryOp::LtEq => write!(f, "<="),
            BinaryOp::GtEq => write!(f, ">="),
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
            RedirectKind::Stderr => write!(f, "2>"),
            RedirectKind::Both => write!(f, "&>"),
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
            TestCmpOp::Gt => write!(f, "-gt"),
            TestCmpOp::Lt => write!(f, "-lt"),
            TestCmpOp::GtEq => write!(f, "-ge"),
            TestCmpOp::LtEq => write!(f, "-le"),
        }
    }
}
