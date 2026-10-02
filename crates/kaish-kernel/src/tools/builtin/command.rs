//! command -v / command -V / type — name what a command word would run.
//!
//! # Examples
//!
//! ```kaish
//! command -v gcc >/dev/null || echo MISSING   # is gcc available?
//! command -v ll                               # alias ll='ls -l'
//! type echo                                   # echo is a shell builtin
//! type -t greet                               # function
//! ```
//!
//! Resolution follows `Kernel::execute_command_depth`: `true`, `false`,
//! `source`, and `.`; an alias; a `/v/bin/` path; a function; a builtin; a
//! `.kai` script on `PATH`; a program on `PATH`; an embedder tool. Lookup
//! is separate from execution: a program on `PATH` is named even when this
//! shell refuses to run it, as `which` does.

use async_trait::async_trait;
use clap::{CommandFactory, Parser};
use std::path::PathBuf;

use crate::interpreter::{ExecResult, OutputData};
use crate::tools::{exec_context, schema_from_clap, ExecContext, GlobalFlags, Tool, ToolArgs, ToolCtx, ToolSchema};

/// What a command word resolves to.
#[derive(Debug, Clone, PartialEq)]
enum Resolution {
    Alias(String),
    Function,
    Builtin,
    /// A `/v/bin/NAME` path to a builtin.
    VirtualBin,
    /// A `.kai` script on `PATH`.
    Script(PathBuf),
    /// A program on `PATH`.
    File(String),
    /// A tool the embedder registered with the kernel backend.
    Tool,
}

impl Resolution {
    /// The `command -v` line.
    fn short(&self, name: &str) -> String {
        match self {
            Resolution::Alias(value) => format!("alias {name}='{value}'"),
            Resolution::Script(path) => path.display().to_string(),
            Resolution::File(path) => path.clone(),
            Resolution::Function | Resolution::Builtin | Resolution::VirtualBin | Resolution::Tool => {
                name.to_string()
            }
        }
    }

    /// The `type` and `command -V` line.
    fn long(&self, name: &str) -> String {
        match self {
            Resolution::Alias(value) => format!("{name} is aliased to `{value}'"),
            Resolution::Function => format!("{name} is a function"),
            Resolution::Builtin => format!("{name} is a shell builtin"),
            Resolution::VirtualBin => format!("{name} is {name}"),
            Resolution::Script(path) => format!("{name} is {}", path.display()),
            Resolution::File(path) => format!("{name} is {path}"),
            Resolution::Tool => format!("{name} is an embedder tool"),
        }
    }

    /// The `type -t` word.
    fn word(&self) -> &'static str {
        match self {
            Resolution::Alias(_) => "alias",
            Resolution::Function => "function",
            Resolution::Builtin | Resolution::VirtualBin => "builtin",
            Resolution::Script(_) | Resolution::File(_) => "file",
            Resolution::Tool => "tool",
        }
    }
}

/// Resolve `name` in the kernel's order. `Ok(None)` when nothing claims it.
async fn resolve(ctx: &ExecContext, tool: &str, name: &str) -> Result<Option<Resolution>, String> {
    if crate::validator::is_runtime_special_form(name) {
        return Ok(Some(Resolution::Builtin));
    }
    if let Some(value) = ctx.aliases.get(name) {
        return Ok(Some(Resolution::Alias(value.clone())));
    }
    let is_builtin = |n: &str| ctx.tool_schemas.iter().any(|schema| schema.name == n);
    if let Some(builtin) = name.strip_prefix("/v/bin/") {
        return Ok(is_builtin(builtin).then_some(Resolution::VirtualBin));
    }
    let Some(dispatcher) = ctx.dispatcher.as_ref() else {
        return Err(format!("{tool}: no dispatcher available (Kernel must be created via into_arc())"));
    };
    if dispatcher.has_function(name).await {
        return Ok(Some(Resolution::Function));
    }
    if is_builtin(name) {
        return Ok(Some(Resolution::Builtin));
    }

    let path_var = ctx.scope.get("PATH").map(crate::interpreter::value_to_string).unwrap_or_default();
    // The kernel looks for scripts under `/bin` when PATH is unset.
    let script_dirs = if ctx.scope.get("PATH").is_some() { path_var.as_str() } else { "/bin" };
    for dir in script_dirs.split(':').filter(|dir| !dir.is_empty()) {
        let script = PathBuf::from(dir).join(format!("{name}.kai"));
        if ctx.backend.exists(&script).await {
            return Ok(Some(Resolution::Script(script)));
        }
    }

    #[cfg(feature = "subprocess")]
    if let Some(path) = super::resolve_in_path(name, &path_var) {
        return Ok(Some(Resolution::File(path)));
    }

    match ctx.backend.get_tool(name).await {
        Ok(Some(_)) => Ok(Some(Resolution::Tool)),
        Ok(None) => Ok(None),
        Err(e) => Err(format!("{tool}: {name}: {e}")),
    }
}

/// command: name what a command word would run.
pub struct CommandBuiltin;

/// clap-derived argv layer for command.
#[derive(Parser, Debug)]
#[command(name = "command", about = "Name what a command word would run")]
struct CommandArgs {
    /// Print the path, builtin name, or alias each name runs; print nothing
    /// and exit 1 for a name that is not found.
    #[arg(short = 'v')]
    short: bool,

    /// Describe each name, as `type` does.
    #[arg(short = 'V')]
    long: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Command names to look up.
    names: Vec<String>,
}

#[async_trait]
impl Tool for CommandBuiltin {
    fn name(&self) -> &str {
        "command"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &CommandArgs::command(),
            "command",
            "Name what a command word would run (-v, -V)",
            [
                ("Check for a program", "command -v gcc >/dev/null || echo MISSING"),
                ("Describe a name", "command -V ls"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("command: {e}")),
        };
        let parsed = match CommandArgs::try_parse_from(std::iter::once("command".to_string()).chain(argv)) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("command: {e}")),
        };
        parsed.global.apply(ctx);

        if !parsed.short && !parsed.long {
            let name = parsed.names.first().map(String::as_str).unwrap_or("NAME");
            return ExecResult::failure(
                2,
                format!("command: only -v and -V are supported; run {name} directly, or check it with `command -v {name}`"),
            );
        }
        if parsed.names.is_empty() {
            return ExecResult::failure(2, "command: missing command name");
        }
        describe_all(ctx, "command", &parsed.names, |resolution, name| {
            if parsed.long { resolution.long(name) } else { resolution.short(name) }
        }, parsed.long)
        .await
    }
}

/// type: describe what a command word would run.
pub struct Type;

/// clap-derived argv layer for type.
#[derive(Parser, Debug)]
#[command(name = "type", about = "Describe what a command word would run")]
struct TypeArgs {
    /// Print one word: alias, function, builtin, file, or tool.
    #[arg(short = 't')]
    word: bool,

    #[command(flatten)]
    global: GlobalFlags,

    /// Command names to describe.
    names: Vec<String>,
}

#[async_trait]
impl Tool for Type {
    fn name(&self) -> &str {
        "type"
    }

    fn schema(&self) -> ToolSchema {
        schema_from_clap(
            &TypeArgs::command(),
            "type",
            "Describe what a command word would run",
            [
                ("Describe a name", "type ls"),
                ("Print the kind only", "type -t ls"),
            ],
        )
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let ctx = exec_context(ctx);
        let argv = match args.to_argv() {
            Ok(v) => v,
            Err(e) => return ExecResult::failure(2, format!("type: {e}")),
        };
        let parsed = match TypeArgs::try_parse_from(std::iter::once("type".to_string()).chain(argv)) {
            Ok(p) => p,
            Err(e) => return ExecResult::failure(2, format!("type: {e}")),
        };
        parsed.global.apply(ctx);

        if parsed.names.is_empty() {
            return ExecResult::failure(2, "type: missing command name");
        }
        // `type -t` prints nothing for a missing name, as bash does.
        describe_all(ctx, "type", &parsed.names, |resolution, name| {
            if parsed.word { resolution.word().to_string() } else { resolution.long(name) }
        }, !parsed.word)
        .await
    }
}

/// One line per name found. Exit 1 when any name is not found; with
/// `report_missing`, each missing name also gets a stderr line.
async fn describe_all(
    ctx: &ExecContext,
    tool: &str,
    names: &[String],
    line: impl Fn(&Resolution, &str) -> String,
    report_missing: bool,
) -> ExecResult {
    let mut lines = Vec::new();
    let mut missing = Vec::new();
    for name in names {
        match resolve(ctx, tool, name).await {
            Ok(Some(resolution)) => lines.push(line(&resolution, name)),
            Ok(None) => missing.push(name.as_str()),
            Err(message) => return ExecResult::failure(2, message),
        }
    }
    let mut result = ExecResult::with_output(OutputData::text(lines.join("\n")));
    if !missing.is_empty() {
        result = result.with_code(1);
        if report_missing {
            let text: Vec<String> = missing.iter().map(|name| format!("{tool}: {name}: not found")).collect();
            result.err = ExecResult::terminate_diagnostic(text.join("\n"));
        }
    }
    result
}
