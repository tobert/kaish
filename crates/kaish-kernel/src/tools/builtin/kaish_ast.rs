//! kaish-ast — Parse and display AST without executing.
//!
//! # Examples
//!
//! ```kaish
//! kaish-ast 'echo hello | grep h'   # One-shot: print AST
//! kaish-ast -on                      # Toggle AST mode on
//! kaish-ast -off                     # Toggle AST mode off
//! ```

use async_trait::async_trait;
use clap::{CommandFactory, Parser};

use crate::interpreter::{ExecResult, OutputData};
use crate::parser::parse;
use crate::tools::{schema_from_clap, ExecContext, ToolCtx, GlobalFlags, Tool, ToolArgs, ToolSchema};

/// kaish-ast: parse expressions and display their AST.
pub struct KaishAst;

/// clap-derived argv layer for kaish-ast.
#[derive(Parser, Debug)]
#[command(name = "kaish-ast", about = "Parse and display AST without executing")]
struct KaishAstArgs {
    /// Enable AST mode (show AST for every command).
    #[arg(long = "on")]
    on: bool,

    /// Disable AST mode.
    #[arg(long = "off")]
    off: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Source expression or file path to parse.
    source: Vec<String>,
}

#[async_trait]
impl Tool for KaishAst {
    fn name(&self) -> &str {
        "kaish-ast"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &KaishAstArgs::command(),
            "kaish-ast",
            "Parse and display AST without executing",
            [
                ("Parse an expression", "kaish-ast 'echo hello | grep h'"),
                ("Enable AST mode", "kaish-ast -on"),
                ("Disable AST mode", "kaish-ast -off"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("kaish-ast: {e}")),
        };
        let parsed = match KaishAstArgs::try_parse_from(
            std::iter::once("kaish-ast".to_string()).chain(argv),
        ) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("kaish-ast: {e}")),
        };
        parsed.global.apply(ctx);

        // Toggle mode
        if parsed.on {
            ctx.scope.set_show_ast(true);
            return ExecResult::with_output(OutputData::text("AST mode: ON\n"));
        }
        if parsed.off {
            ctx.scope.set_show_ast(false);
            return ExecResult::with_output(OutputData::text("AST mode: OFF\n"));
        }

        // One-shot: parse expression and display AST
        let expr = match args.get_string("expr", 0) {
            Some(e) => e,
            None => {
                // Toggle mode if no args
                let current = ctx.scope.show_ast();
                ctx.scope.set_show_ast(!current);
                let state = if !current { "ON" } else { "OFF" };
                return ExecResult::with_output(OutputData::text(format!("AST mode: {state}\n")));
            }
        };

        match parse(&expr) {
            Ok(program) => ExecResult::with_output(OutputData::text(format!("{:#?}\n", program))),
            Err(errors) => {
                let mut msg = String::from("Parse error:\n");
                for err in errors {
                    msg.push_str(&format!("  {err}\n"));
                }
                ExecResult::failure(1, msg)
            }
        }
    }
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
        ExecContext::new(Arc::new(vfs))
    }

    #[tokio::test]
    async fn test_ast_one_shot() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("echo hello".into()));

        let result = KaishAst.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("Echo") || result.text_out().contains("Command") || result.text_out().contains("echo"));
    }

    #[tokio::test]
    async fn test_ast_toggle_on() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.flags.insert("on".to_string());

        let result = KaishAst.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("ON"));
        assert!(ctx.scope.show_ast());
    }

    #[tokio::test]
    async fn test_ast_toggle_off() {
        let mut ctx = make_ctx();
        ctx.scope.set_show_ast(true);
        let mut args = ToolArgs::new();
        args.flags.insert("off".to_string());

        let result = KaishAst.execute(args, &mut ctx).await;
        assert!(result.ok());
        assert!(result.text_out().contains("OFF"));
        assert!(!ctx.scope.show_ast());
    }

    #[tokio::test]
    async fn test_ast_parse_error() {
        let mut ctx = make_ctx();
        let mut args = ToolArgs::new();
        args.positional.push(Value::String("if".into()));

        let result = KaishAst.execute(args, &mut ctx).await;
        assert!(!result.ok());
    }
}
