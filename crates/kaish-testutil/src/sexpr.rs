//! S-expression formatter for kaish AST.
//!
//! Converts AST nodes to the S-expression format used in parser test files.

use kaish_kernel::ast::*;

/// Format a Program as an S-expression.
/// For single-statement programs, formats just the statement.
/// For multi-statement programs, formats as a sequence.
pub fn format_program(program: &Program) -> String {
    let stmts: Vec<_> = program
        .statements
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .collect();

    match stmts.len() {
        0 => "(program)".to_string(),
        1 => format_stmt(stmts[0]),
        _ => {
            let parts: Vec<String> = stmts.iter().map(|s| format_stmt(s)).collect();
            format!("(program {})", parts.join(" "))
        }
    }
}

/// Format a statement as an S-expression.
pub fn format_stmt(stmt: &Stmt) -> String {
    match stmt {
        Stmt::Assignment(a) => format_assignment(a),
        Stmt::Command(cmd) => format_command(cmd),
        Stmt::Pipeline(p) => format_pipeline(p),
        Stmt::If(if_stmt) => format_if(if_stmt),
        Stmt::For(for_loop) => format_for(for_loop),
        Stmt::While(while_loop) => format_while(while_loop),
        Stmt::Case(case_stmt) => format_case(case_stmt),
        Stmt::Break(n) => match n {
            Some(level) => format!("(break {})", level),
            None => "(break)".to_string(),
        },
        Stmt::Continue(n) => match n {
            Some(level) => format!("(continue {})", level),
            None => "(continue)".to_string(),
        },
        Stmt::Return(expr) => match expr {
            Some(e) => format!("(return {})", format_expr(e)),
            None => "(return)".to_string(),
        },
        Stmt::Exit(expr) => match expr {
            Some(e) => format!("(exit {})", format_expr(e)),
            None => "(exit)".to_string(),
        },
        Stmt::ToolDef(tool) => format_tooldef(tool),
        Stmt::Test(test_expr) => format!("(test {})", format_test_expr(test_expr)),
        Stmt::AndChain { left, right } => {
            format!("(and-chain {} {})", format_stmt(left), format_stmt(right))
        }
        Stmt::OrChain { left, right } => {
            format!("(or-chain {} {})", format_stmt(left), format_stmt(right))
        }
        Stmt::Empty => "(empty)".to_string(),
    }
}

/// Format an assignment as an S-expression.
fn format_assignment(a: &Assignment) -> String {
    let value = format_expr(&a.value);
    // Always output local attribute for clarity
    format!("(assign {} {} local={})", a.name, value, a.local)
}

/// Format a command as an S-expression.
fn format_command(cmd: &Command) -> String {
    let mut parts = vec![format!("(cmd {}", cmd.name)];

    for arg in &cmd.args {
        parts.push(format_arg(arg));
    }

    for redir in &cmd.redirects {
        parts.push(format_redirect(redir));
    }

    format!("{})", parts.join(" "))
}

/// Format an argument as an S-expression.
fn format_arg(arg: &Arg) -> String {
    match arg {
        Arg::Positional(expr) => format!("(pos {})", format_expr(expr)),
        Arg::Named { key, value } => format!("(named {} {})", key, format_expr(value)),
        Arg::ShortFlag(f) => format!("(shortflag {})", f),
        Arg::LongFlag(f) => format!("(longflag {})", f),
    }
}

/// Format a redirect as an S-expression.
fn format_redirect(redir: &Redirect) -> String {
    let kind = match redir.kind {
        RedirectKind::StdoutOverwrite => ">",
        RedirectKind::StdoutAppend => ">>",
        RedirectKind::Stdin => "<",
        RedirectKind::HereDoc => "<<",
        RedirectKind::Stderr => "2>",
        RedirectKind::Both => "&>",
    };
    format!("(redir {} {})", kind, format_expr(&redir.target))
}

/// Format a pipeline as an S-expression.
fn format_pipeline(p: &Pipeline) -> String {
    let cmds: Vec<String> = p.commands.iter().map(format_command).collect();

    // Single command background: (background (cmd ...))
    // Multi-command background: (background (pipeline ...))
    if p.background {
        if cmds.len() == 1 {
            format!("(background {})", cmds[0])
        } else {
            format!("(background (pipeline {}))", cmds.join(" "))
        }
    } else {
        format!("(pipeline {})", cmds.join(" "))
    }
}

/// Format an if statement as an S-expression.
fn format_if(if_stmt: &IfStmt) -> String {
    let cond = format_expr(&if_stmt.condition);
    let then_stmts: Vec<String> = if_stmt
        .then_branch
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .map(format_stmt)
        .collect();
    let then_part = format!("(then {})", then_stmts.join(" "));

    match &if_stmt.else_branch {
        Some(else_stmts) => {
            let else_inner: Vec<String> = else_stmts
                .iter()
                .filter(|s| !matches!(s, Stmt::Empty))
                .map(format_stmt)
                .collect();
            if else_inner.is_empty() {
                format!("(if {} {} (else))", cond, then_part)
            } else {
                format!("(if {} {} (else {}))", cond, then_part, else_inner.join(" "))
            }
        }
        None => format!("(if {} {} (else))", cond, then_part),
    }
}

/// Format a for loop as an S-expression.
fn format_for(for_loop: &ForLoop) -> String {
    let items: Vec<String> = for_loop.items.iter().map(format_expr).collect();
    let body_stmts: Vec<String> = for_loop
        .body
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .map(format_stmt)
        .collect();
    format!(
        "(for {} (in {}) (do {}))",
        for_loop.variable,
        items.join(" "),
        body_stmts.join(" ")
    )
}

/// Format a while loop as an S-expression.
fn format_while(while_loop: &WhileLoop) -> String {
    let cond = format_expr(&while_loop.condition);
    let body_stmts: Vec<String> = while_loop
        .body
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .map(format_stmt)
        .collect();
    format!("(while {} (do {}))", cond, body_stmts.join(" "))
}

/// Format a case statement as an S-expression.
fn format_case(case_stmt: &CaseStmt) -> String {
    let expr = format_expr(&case_stmt.expr);
    let branches: Vec<String> = case_stmt
        .branches
        .iter()
        .map(format_case_branch)
        .collect();
    format!("(case {} ({}))", expr, branches.join(" "))
}

/// Format a case branch as an S-expression.
fn format_case_branch(branch: &CaseBranch) -> String {
    let patterns = branch.patterns.join("|");
    let body_stmts: Vec<String> = branch
        .body
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .map(format_stmt)
        .collect();
    format!("(branch \"{}\" ({}))", patterns, body_stmts.join(" "))
}

/// Format a tool definition as an S-expression.
fn format_tooldef(tool: &ToolDef) -> String {
    let params: Vec<String> = tool.params.iter().map(format_param).collect();
    let body_stmts: Vec<String> = tool
        .body
        .iter()
        .filter(|s| !matches!(s, Stmt::Empty))
        .map(format_stmt)
        .collect();
    format!(
        "(tooldef {} ({}) ({}))",
        tool.name,
        params.join(" "),
        body_stmts.join(" ")
    )
}

/// Format a parameter definition as an S-expression.
fn format_param(param: &ParamDef) -> String {
    let type_str = param
        .param_type
        .as_ref()
        .map(|t| match t {
            ParamType::String => "string",
            ParamType::Int => "int",
            ParamType::Float => "float",
            ParamType::Bool => "bool",
        })
        .unwrap_or("any");

    match &param.default {
        Some(default) => format!("(param {} {} {})", param.name, type_str, format_expr(default)),
        None => format!("(param {} {})", param.name, type_str),
    }
}

/// Format an expression as an S-expression.
pub fn format_expr(expr: &Expr) -> String {
    match expr {
        Expr::Literal(value) => format_value(value),
        Expr::VarRef(path) => format!("(varref {})", format_varpath(path)),
        Expr::Interpolated(parts) => {
            let parts_str: Vec<String> = parts
                .iter()
                .map(|p| match p {
                    StringPart::Literal(s) => format!("\"{}\"", s),
                    StringPart::Var(path) => format!("(varref {})", format_varpath(path)),
                })
                .collect();
            format!("(interpolated {})", parts_str.join(" "))
        }
        Expr::BinaryOp { left, op, right } => {
            let op_str = match op {
                BinaryOp::And => "and",
                BinaryOp::Or => "or",
                BinaryOp::Eq => "eq",
                BinaryOp::NotEq => "neq",
                BinaryOp::Match => "match",
                BinaryOp::NotMatch => "not-match",
                BinaryOp::Lt => "<",
                BinaryOp::Gt => ">",
                BinaryOp::LtEq => "<=",
                BinaryOp::GtEq => ">=",
            };
            format!("({} {} {})", op_str, format_expr(left), format_expr(right))
        }
        Expr::CommandSubst(pipeline) => {
            format!("(cmdsubst {})", format_pipeline(pipeline))
        }
        Expr::Test(test_expr) => format!("(test {})", format_test_expr(test_expr)),
        Expr::Positional(n) => format!("(positional {})", n),
        Expr::AllArgs => "(all-args)".to_string(),
        Expr::ArgCount => "(arg-count)".to_string(),
        Expr::VarLength(name) => format!("(var-length {})", name),
        Expr::VarWithDefault { name, default } => {
            format!("(var-default {} \"{}\")", name, default)
        }
        Expr::Arithmetic(expr_str) => format!("(arithmetic \"{}\")", expr_str),
    }
}

/// Format a test expression as an S-expression.
fn format_test_expr(test: &TestExpr) -> String {
    match test {
        TestExpr::FileTest { op, path } => {
            let op_str = match op {
                FileTestOp::Exists => "-e",
                FileTestOp::IsFile => "-f",
                FileTestOp::IsDir => "-d",
                FileTestOp::Readable => "-r",
                FileTestOp::Writable => "-w",
                FileTestOp::Executable => "-x",
            };
            format!("(file {} {})", op_str, format_expr(path))
        }
        TestExpr::StringTest { op, value } => {
            let op_str = match op {
                StringTestOp::IsEmpty => "-z",
                StringTestOp::IsNonEmpty => "-n",
            };
            format!("(string {} {})", op_str, format_expr(value))
        }
        TestExpr::Comparison { left, op, right } => {
            let op_str = match op {
                TestCmpOp::Eq => "==",
                TestCmpOp::NotEq => "!=",
                TestCmpOp::Gt => "-gt",
                TestCmpOp::Lt => "-lt",
                TestCmpOp::GtEq => "-ge",
                TestCmpOp::LtEq => "-le",
            };
            format!(
                "(cmp {} {} {})",
                op_str,
                format_expr(left),
                format_expr(right)
            )
        }
    }
}

/// Escape control characters for display in test output.
fn escape_for_display(s: &str) -> String {
    s.replace('\n', "\\n")
        .replace('\t', "\\t")
        .replace('\r', "\\r")
}

/// Format a value as an S-expression.
fn format_value(value: &Value) -> String {
    match value {
        Value::Null => "(null)".to_string(),
        Value::Bool(b) => format!("(bool {})", b),
        Value::Int(n) => format!("(int {})", n),
        Value::Float(f) => format!("(float {})", f),
        Value::String(s) => format!("(string \"{}\")", escape_for_display(s)),
    }
}

/// Format a variable path as an S-expression.
fn format_varpath(path: &VarPath) -> String {
    path.segments
        .iter()
        .map(|seg| match seg {
            VarSegment::Field(name) => name.clone(),
        })
        .collect::<Vec<_>>()
        .join(".")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_simple_int() {
        assert_eq!(format_value(&Value::Int(42)), "(int 42)");
    }

    #[test]
    fn format_simple_string() {
        assert_eq!(format_value(&Value::String("hello".to_string())), "(string \"hello\")");
    }

    #[test]
    fn format_varpath_simple() {
        let path = VarPath::simple("X");
        assert_eq!(format_varpath(&path), "X");
    }

    #[test]
    fn format_varpath_nested() {
        let path = VarPath {
            segments: vec![
                VarSegment::Field("VAR".to_string()),
                VarSegment::Field("field".to_string()),
            ],
        };
        assert_eq!(format_varpath(&path), "VAR.field");
    }
}
