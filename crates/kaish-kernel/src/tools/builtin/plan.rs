//! plan — Report what a statement would run, without running it.
//!
//! # Examples
//!
//! ```kaish
//! plan 'rm -r "$d"' --json           # The statement projection, as JSON
//! echo "$stmt" | plan --json         # Same, reading the statement from stdin
//! plan 'for f in $(glob "*"); do echo $f; done'
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::ExecResult;
use crate::tools::{exec_context, 
    schema_from_clap, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema,
};

/// plan: the statement projection an embedder judges a command by.
pub struct PlanTool;

/// clap-derived argv layer for plan.
#[derive(Parser, Debug)]
#[command(
    name = "plan",
    about = "Report what a statement would run, without running it"
)]
struct PlanArgs {
    #[command(flatten)]
    global: GlobalFlags,

    /// The kaish source to plan. Reads stdin when no source is given.
    source: Vec<String>,
}

#[async_trait]
impl Tool for PlanTool {
    fn name(&self) -> &str {
        "plan"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &PlanArgs::command(),
            "plan",
            "Report what a statement would run, without running it",
            [
                ("Plan a statement", "plan 'rm -r \"$d\"' --json"),
                ("Plan what is on stdin", "echo \"$stmt\" | plan --json"),
            ],
        )
        .with_typed_substitution()
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("plan: {e}")),
        };
        let parsed =
            match PlanArgs::try_parse_from(std::iter::once("plan".to_string()).chain(argv)) {
                Ok(p) => p,
                Err(e) => return ExecResult::failure(2, format!("plan: {e}")),
            };
        parsed.global.apply(ctx);

        // A statement is one block on stdin, or one argument.
        //
        // Refusing more than one is the whole point. Joining them with a space
        // would re-word an unquoted statement into something the caller did not
        // write, and taking the first would plan a *different* statement than
        // the one asked about — `plan rm build` would report a bare `rm` with no
        // arguments. A tool whose answer decides whether a command is dangerous
        // must never quietly answer about a shorter command.
        if args.positional.len() > 1 {
            let count = args.positional.len();
            return ExecResult::failure(
                2,
                format!(
                    "plan: expected one statement, got {count} words — quote the \
                     whole statement: plan '<statement>'"
                ),
            );
        }
        let source = match args.get_string("source", 0) {
            Some(s) => s,
            None => match ctx.read_stdin_to_text().await {
                Ok(Some(s)) => s,
                Ok(None) => {
                    return ExecResult::failure(
                        2,
                        "plan: no source — pass a statement (plan '<statement>') \
                         or pipe one in (echo \"$stmt\" | plan)",
                    )
                }
                Err(e) => return ExecResult::failure(2, format!("plan: {e}")),
            },
        };

        // `plan_program` is a pure function of the source text: nothing is
        // executed, no substitution runs, and no filesystem is touched. That is
        // the whole point — the caller judges what the statement *asked for*,
        // before anything it names can happen.
        match crate::plan_program(&source) {
            Ok(statements) => {
                let mut doc = serde_json::json!({
                    "statements": statements,
                    "kaish_version": crate::KAISH_VERSION,
                    "kaish_git_hash": crate::KAISH_GIT_HASH,
                    "kaish_build_date": crate::KAISH_BUILD_DATE,
                });
                // The same `warnings` array the CLI emits. The two are
                // documented as one projection, so a key on one and not the
                // other is a divergence, not a difference in scope.
                let warnings = plan_warnings(&source);
                if !warnings.is_empty()
                    && let Some(object) = doc.as_object_mut()
                {
                    object.insert("warnings".into(), serde_json::Value::Array(warnings));
                }
                let text = render_plan_text(&statements);
                ExecResult::success_with_data(
                    text,
                    crate::interpreter::json_to_value_no_envelope(doc),
                )
            }
            Err(errors) => {
                let doc = serde_json::json!({
                    "errors": errors
                        .iter()
                        .map(|e| serde_json::json!({
                            "message": e.message,
                            "start": e.span.start,
                            "end": e.span.end,
                        }))
                        .collect::<Vec<_>>(),
                    "kaish_version": crate::KAISH_VERSION,
                    "kaish_git_hash": crate::KAISH_GIT_HASH,
                    "kaish_build_date": crate::KAISH_BUILD_DATE,
                });
                let mut msg = String::from("plan: parse error:\n");
                for err in &errors {
                    msg.push_str(&format!("  {err}\n"));
                }
                // 2 is the usage/parse code, matching the CLI's `--plan` and a
                // builtin's argv rejection.
                let mut result = ExecResult::failure(2, msg);
                result.data = Some(crate::interpreter::json_to_value_no_envelope(doc));
                result
            }
        }
    }
}

/// Text rendering: one line per statement, then the commands it would run.
///
/// The commands are the point — a statement's text can bury a command deep
/// enough that a reader skims past it, and `commands` lists each one on its own
/// whether it sits in a loop body, an `if` condition, or a `$(...)`.
fn render_plan_text(statements: &[crate::ast::plan::PlannedStatement]) -> String {
    let mut out = String::new();
    for statement in statements {
        out.push_str(&format!(
            "{}  {}  {}\n",
            statement.index, statement.plan.statement_kind, statement.plan.rendered
        ));
        for command in &statement.plan.commands {
            out.push_str(&format!("     -> {}\n", command.name));
        }
    }
    out
}

/// Statements that will run and fail, as plan JSON objects.
///
/// Filtered by `IssueCode::surfaces_in_plan`, not by severity — most
/// warnings are advisory and would be noise in a plan. Mirrors
/// `plan_validation_issues` in the REPL binary; the two emit one projection.
fn plan_warnings(source: &str) -> Vec<serde_json::Value> {
    let Ok(issues) = crate::validator::validate_program(source) else {
        return Vec::new();
    };
    issues
        .iter()
        .filter(|issue| issue.severity != crate::validator::Severity::Error)
        .filter(|issue| issue.code.surfaces_in_plan())
        .map(|issue| {
            let mut object = serde_json::Map::new();
            object.insert("code".into(), issue.code.code().into());
            object.insert("message".into(), issue.message.clone().into());
            if let Some(span) = &issue.span {
                object.insert("start".into(), span.start.into());
                object.insert("end".into(), span.end.into());
            }
            if let Some(suggestion) = &issue.suggestion {
                object.insert("suggestion".into(), suggestion.clone().into());
            }
            serde_json::Value::Object(object)
        })
        .collect()
}
