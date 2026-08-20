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
use crate::tools::{
    schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema,
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
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
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
                let doc = serde_json::json!({ "statements": statements });
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
