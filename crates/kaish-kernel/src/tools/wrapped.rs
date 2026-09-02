//! Wrapped commands: external programs registered as kaish tools.
//!
//! The embedder declares the program, the verbs it allows, and the flags each
//! verb accepts. The kernel validates a call against that declaration and
//! renders the argv itself, so a value is never parsed as a flag by the child
//! unless the declaration put it in flag position.
//!
//! This narrows what a kernel can run; it does not widen it.
//! `allow_external_commands` chooses between "no word spawns anything" and
//! "every program on `$PATH` spawns, with any arguments." A wrapped command is
//! the setting between: the executable is pinned at registration, so a changed
//! `$PATH` changes nothing, and an undeclared verb or flag is refused before
//! any spawn. Registering wrappers with the switch left `false` is the intended
//! shape, not a way around it.
//!
//! ```no_run
//! use kaish_kernel::tools::wrapped::{Flag, Positional, Verb, WrappedCommand};
//!
//! # fn main() -> anyhow::Result<()> {
//! let git = WrappedCommand::new("git")
//!     .executable("/usr/bin/git")
//!     .about("Version control, read-mostly.")
//!     .lead(["--no-pager"])
//!     .env("GIT_PAGER", "cat")
//!     .verb(Verb::new("log")
//!         .flag(Flag::value("max-count").alias("-n").int())
//!         .flag(Flag::switch("oneline"))
//!         .positional(Positional::many("revision")))
//!     .build()?;
//! # let _ = git;
//! # Ok(())
//! # }
//! ```
//!
//! See `docs/wrapped_command.md` for the contract.

mod constraint;
mod declaration;
mod error;
mod parse;
mod render;

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use async_trait::async_trait;

use kaish_types::{ExecResult, ParamSchema, ToolArgs, ToolSchema, Value};

use kaish_tool_api::{IssueCode, ValidationIssue};

use crate::spawn::{
    hermetic_env, spawn_process, OutputPolicy, SpawnContext, SpawnRequest, StdinPolicy,
};
use crate::tools::{virtual_cwd_error, ExecContext, Tool, ToolCtx};

pub use declaration::{find_executable, Flag, Positional, Stdin, Style, Tail, Verb, WrappedCommand};
pub use error::WrappedError;

use constraint::resolve_under;

use parse::Word;

/// A `path_under` positional the call still has to satisfy.
///
/// An absolute value is decided when the call is planned; a relative one
/// needs the kernel's real cwd, which only the execute path holds.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct PathCheck {
    /// The declared positional's name.
    pub positional: String,
    /// The value as the agent wrote it.
    pub value: String,
    /// The declared root the resolved path must be inside.
    pub root: PathBuf,
    /// Where the value sits in [`RenderedCall::argv`], so the resolved path
    /// can replace it.
    pub argv_index: usize,
    /// True when planning already resolved the value and rewrote `argv`.
    pub resolved: bool,
}

/// A call the declaration accepts, rendered into the child's argv.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub struct RenderedCall {
    /// The selected verb's own declared name; `None` for the root verb.
    /// Never the full path — `git worktree list` still reports `list` here.
    pub verb: Option<String>,
    /// The full path to the selected verb: `git worktree list`, or the
    /// command name alone for the root verb.
    pub scope: String,
    /// The child's argv, without the executable.
    pub argv: Vec<String>,
    /// What the child's standard input is connected to.
    pub stdin: Stdin,
    /// The verb declared its stdout to be JSON.
    pub json_output: bool,
    /// `path_under` positionals still to satisfy. Every one whose `resolved`
    /// is false needs the real cwd before the child runs.
    pub path_checks: Vec<PathCheck>,
}

/// A checked declaration with its executable pinned.
#[derive(Debug, Clone)]
pub struct WrappedTool {
    declaration: WrappedCommand,
    executable: PathBuf,
}

impl WrappedTool {
    pub(crate) fn from_parts(declaration: WrappedCommand, executable: PathBuf) -> Self {
        Self {
            declaration,
            executable,
        }
    }

    /// The tool name agents write.
    pub fn name(&self) -> &str {
        &self.declaration.name
    }

    /// The pinned executable, verified at `build()`.
    pub fn executable(&self) -> &Path {
        &self.executable
    }

    /// The environment pins for the child. They win over exported scope
    /// variables of the same name.
    pub fn env(&self) -> &BTreeMap<String, String> {
        &self.declaration.env
    }

    /// The checked declaration.
    pub fn declaration(&self) -> &WrappedCommand {
        &self.declaration
    }

    /// The schema the kernel publishes to agents.
    ///
    /// `raw_argv`, so the binder hands the wrapper every word in source order
    /// with `--` preserved — the wrapper's own grammar decides what each word
    /// is. Named verbs become subcommands; the root verb's flags and
    /// positionals sit on the root schema.
    pub fn schema(&self) -> ToolSchema {
        let mut schema = ToolSchema::new(&self.declaration.name, self.root_description());
        if let Some(root) = &self.declaration.root {
            for param in verb_params(root) {
                schema = schema.param(param);
            }
            for (label, command) in &root.examples {
                schema = schema.example(label, command);
            }
        }
        for (label, command) in &self.declaration.examples {
            schema = schema.example(label, command);
        }
        for verb in &self.declaration.verbs {
            schema = schema.subcommand(verb_schema(verb));
        }
        schema = schema.with_raw_argv();
        // The kernel reads `typed_substitution` from the root schema, so
        // `$(cmd …)` binds typed only when every verb the tool can run says
        // its stdout is JSON — a mixed tool must not make `$(cargo build)`
        // bind a value that does not exist. A JSON verb on a mixed tool still
        // binds typed: `bind_json_output` stamps `data_is_value` on that
        // verb's own result, and the kernel ORs this flag in rather than
        // assigning it. The flag stays on the verb's schema for `help`.
        if self.every_verb_is_json() {
            schema = schema.with_typed_substitution();
        }
        schema
    }

    fn root_description(&self) -> String {
        match &self.declaration.root {
            Some(root) if root.tail == Tail::Forward => {
                append_clause(&self.declaration.about, "forwards undeclared flags")
            }
            _ => self.declaration.about.clone(),
        }
    }

    /// True when every leaf the tool can run declares `json_output()`. A
    /// node is never callable, so its own `json_output` (if set) does not
    /// count — only the leaves under it decide this.
    fn every_verb_is_json(&self) -> bool {
        let mut any = false;
        let mut all = true;
        for verb in self.declaration.root.iter().chain(self.declaration.verbs.iter()) {
            walk_leaf_json(verb, &mut any, &mut all);
        }
        any && all
    }

    /// Plan a call: parse it, check every constraint, and render the argv.
    ///
    /// Fails with the first refusal. Nothing spawns; every refusal exits 2.
    pub fn plan_call(&self, args: &ToolArgs) -> Result<RenderedCall, WrappedError> {
        let words = self.execution_words(args)?;
        let call = parse::parse(&self.declaration, &words).map_err(|failure| failure.error)?;
        let Some(verb) = call.verb(&self.declaration) else {
            // `select_verb` only declines when a word is opaque, and execution
            // has no opaque words.
            return Err(WrappedError::MissingVerb {
                command: self.declaration.name.clone(),
                allowed: parse::allowed_verbs(&self.declaration),
            });
        };
        if let Some(error) = constraint::check(&self.declaration, verb, &call).into_iter().next() {
            return Err(error);
        }

        let rendered = render::render(&self.declaration, verb, &call);
        let mut argv = rendered.argv;
        let mut path_checks = constraint::path_checks(verb, &call, &rendered.item_argv_index);

        // An absolute value is decided now: validation rejects a path outside
        // its root before anything spawns. A relative one waits for the real
        // cwd, which only the execute path holds.
        for check in &mut path_checks {
            if !Path::new(&check.value).is_absolute() {
                continue;
            }
            let resolved = resolve_under(&check.value, Path::new("/"), &check.root)
                .map_err(|e| e.attributed_to(&self.declaration.name, &check.positional))?;
            if let Some(word) = argv.get_mut(check.argv_index) {
                *word = resolved.to_string_lossy().into_owned();
            }
            check.resolved = true;
        }

        Ok(RenderedCall {
            verb: verb.name.clone(),
            scope: self.declaration.scope_of_path(&call.verb_path),
            argv,
            stdin: verb.stdin,
            json_output: verb.json_output,
            path_checks,
        })
    }

    /// Judge a call before it runs, without evaluating anything.
    ///
    /// A word the validation binder could not evaluate is opaque: it cannot
    /// be an unknown flag, cannot fail a `choices` set, and cannot be
    /// path-checked. A literal word is judged in full.
    pub fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        let words: Vec<Word> = args
            .positional
            .iter()
            .map(|value| Word::from_validation_text(crate::interpreter::value_to_string(value)))
            .collect();

        let call = match parse::parse(&self.declaration, &words) {
            Ok(call) => call,
            Err(failure) => return vec![issue(&failure.error, failure.uncertain)],
        };
        let Some(verb) = call.verb(&self.declaration) else {
            return Vec::new();
        };

        let mut issues: Vec<ValidationIssue> = constraint::check(&self.declaration, verb, &call)
            .iter()
            .map(|error| issue(error, call.uncertain))
            .collect();

        let rendered = render::render(&self.declaration, verb, &call);
        for check in constraint::path_checks(verb, &call, &rendered.item_argv_index) {
            if !Path::new(&check.value).is_absolute() {
                continue;
            }
            if let Err(error) = resolve_under(&check.value, Path::new("/"), &check.root) {
                issues.push(issue(
                    &error.attributed_to(&self.declaration.name, &check.positional),
                    call.uncertain,
                ));
            }
        }
        issues
    }

    /// Finish a deferred [`PathCheck`] against the kernel's real cwd.
    ///
    /// A relative value cannot be resolved when the call is planned — only
    /// the execute path knows where the kernel is. The returned path is
    /// canonical and inside the declared root; it replaces
    /// `argv[check.argv_index]` before the child runs.
    pub fn resolve_path_check(
        &self,
        check: &PathCheck,
        real_cwd: &Path,
    ) -> Result<PathBuf, WrappedError> {
        resolve_under(&check.value, real_cwd, &check.root)
            .map_err(|e| e.attributed_to(&self.declaration.name, &check.positional))
    }

    /// The words execution hands the parser: every one literal, with the two
    /// values argv cannot carry refused by name.
    fn execution_words(&self, args: &ToolArgs) -> Result<Vec<Word>, WrappedError> {
        let mut words = Vec::with_capacity(args.positional.len());
        for (offset, value) in args.positional.iter().enumerate() {
            let position = offset + 1;
            if let Value::Bytes(bytes) = value {
                return Err(WrappedError::BinaryArgument {
                    command: self.declaration.name.clone(),
                    position,
                    byte_len: bytes.len(),
                });
            }
            let text = crate::interpreter::value_to_string(value);
            if text.contains('\0') {
                return Err(WrappedError::NulByte {
                    command: self.declaration.name.clone(),
                    position,
                });
            }
            words.push(Word::literal(text));
        }
        Ok(words)
    }
}

#[async_trait]
impl Tool for WrappedTool {
    fn name(&self) -> &str {
        // The inherent methods carry these three; method-call syntax resolves
        // to them anyway, and naming the type says so.
        WrappedTool::name(self)
    }

    fn schema(&self) -> ToolSchema {
        WrappedTool::schema(self)
    }

    fn validate(&self, args: &ToolArgs) -> Vec<ValidationIssue> {
        WrappedTool::validate(self, args)
    }

    async fn execute(&self, args: ToolArgs, ctx: &mut dyn ToolCtx) -> ExecResult {
        let Some(ctx) = ctx.as_any_mut().downcast_mut::<ExecContext>() else {
            return ExecResult::failure(1, "internal error: kernel builtin requires ExecContext");
        };
        self.run(args, ctx).await
    }
}

impl WrappedTool {
    /// Run a call against the pinned executable.
    ///
    /// Every refusal the declaration raises exits 2 and spawns nothing. Past
    /// that point the child's own exit code is the answer, unchanged — the
    /// wrapper adds nothing on top.
    async fn run(&self, args: ToolArgs, ctx: &mut ExecContext) -> ExecResult {
        let call = match self.plan_call(&args) {
            Ok(call) => call,
            Err(error) => return ExecResult::failure(error.exit_code(), error.to_string()),
        };
        let label = call.scope.clone();

        // A virtual cwd has no location to spawn in. The same refusal an
        // external command gets, named for this command.
        let Some(real_cwd) = ctx.backend.resolve_real_path(&ctx.cwd) else {
            return virtual_cwd_error(&self.declaration.name, &ctx.cwd);
        };

        // A relative `path_under` value could not be decided when the call was
        // planned; the real cwd is only known here. The canonical path
        // replaces the word the agent wrote, so the child opens what kaish
        // checked.
        let mut argv = call.argv;
        for check in &call.path_checks {
            if check.resolved {
                continue;
            }
            match self.resolve_path_check(check, &real_cwd) {
                Ok(resolved) => {
                    if let Some(word) = argv.get_mut(check.argv_index) {
                        *word = resolved.to_string_lossy().into_owned();
                    }
                }
                Err(error) => return ExecResult::failure(error.exit_code(), error.to_string()),
            }
        }

        // The kernel's hermetic environment, then the declaration's pins,
        // which win on conflict.
        let mut env = match hermetic_env(&ctx.scope) {
            Ok(env) => env,
            Err(e) => return ExecResult::failure(1, format!("{label}: {e}")),
        };
        env.retain(|(name, _)| !self.declaration.env.contains_key(name));
        env.extend(
            self.declaration
                .env
                .iter()
                .map(|(name, value)| (name.clone(), value.clone())),
        );

        let stdin = match call.stdin {
            Stdin::Closed => {
                // Silently dropping the input is not an option: the call said
                // one thing and the declaration another. Refuse without
                // taking the stdin, so whatever produced it is still intact.
                if ctx.pipe_stdin.is_some() || ctx.stdin.is_some() {
                    return ExecResult::failure(2, format!("{label}: does not read stdin"));
                }
                StdinPolicy::Null
            }
            Stdin::Pipe => {
                // Take both, and do not drain: a pipe read can block on a
                // still-running upstream stage, so `sleep 1 | wrapped` would
                // deadlock. `spawn_process` streams them after the fork.
                let pipe = ctx.pipe_stdin.take();
                let prefix = ctx.take_stdin();
                match (prefix, pipe) {
                    (None, None) => StdinPolicy::Null,
                    (prefix, pipe) => StdinPolicy::Piped { prefix, pipe },
                }
            }
        };

        let spawn_ctx = SpawnContext::from_exec_context(ctx);
        let request = SpawnRequest {
            executable: self.executable.clone(),
            argv,
            cwd: real_cwd,
            // Never `Inherit`: a wrapped command's output belongs to the
            // kernel, so the output limits and the spill contract see it.
            output: OutputPolicy::Captured,
            env,
            stdin,
            label: label.clone(),
        };
        let result = spawn_process(request, &spawn_ctx).await;

        if call.json_output {
            return bind_json_output(result, &label);
        }
        result
    }
}

/// Parse a `json_output` verb's stdout and hand it back as the result's value.
///
/// The text stays as the child printed it, so the REPL still shows the JSON;
/// `data_is_value` is what makes `$(…)` bind it typed. The kernel ORs that
/// marker in rather than assigning it, so a JSON verb on a tool with text
/// verbs binds typed without the whole tool claiming
/// [`ToolSchema::typed_substitution`].
fn bind_json_output(mut result: ExecResult, label: &str) -> ExecResult {
    // A child that failed, or whose stdout was evicted from the capture ring,
    // has not delivered the JSON its verb promised. Its own code is the
    // answer; a parse failure here would replace it with the wrapper's.
    if !result.ok() || result.did_spill {
        return result;
    }
    let text = match result.try_text_out() {
        Ok(text) => text.into_owned(),
        Err(e) => return ExecResult::failure(1, format!("{label}: declared JSON output, but {e}")),
    };
    match serde_json::from_str::<serde_json::Value>(&text) {
        // No envelope sniffing: a child's JSON object that happens to match
        // the base64 bytes envelope is a plain record, not binary.
        Ok(json) => {
            result.data = Some(kaish_types::json_to_value_no_envelope(json));
            result.data_is_value = true;
            result
        }
        Err(e) => ExecResult::failure(
            1,
            format!("{label}: declared JSON output, but stdout does not parse: {e}"),
        ),
    }
}

/// Map a refusal onto the validator's vocabulary.
///
/// `uncertain` softens the verdict to a warning: a word the parser could not
/// judge sat in flag-or-positional position, so this reading of the call may
/// not be the one that runs.
fn issue(error: &WrappedError, uncertain: bool) -> ValidationIssue {
    let code = match error {
        WrappedError::UnknownFlag { .. }
        | WrappedError::ClusteredShort { .. }
        | WrappedError::GluedShortValue { .. }
        | WrappedError::UnexpectedFlagValue { .. }
        | WrappedError::RepeatedFlag { .. } => IssueCode::UnknownFlag,
        WrappedError::MissingFlagValue { .. }
        | WrappedError::MissingRequiredFlag { .. }
        | WrappedError::MissingRequiredPositional { .. } => IssueCode::MissingRequiredArg,
        WrappedError::NotAnInteger { .. } | WrappedError::NotInChoices { .. } => {
            IssueCode::InvalidArgType
        }
        _ => IssueCode::WrappedCallRejected,
    };
    let issue = if uncertain {
        ValidationIssue::warning(code, error.to_string())
    } else {
        ValidationIssue::error(code, error.to_string())
    };
    // Every variant names its command, so absent here would mean "not about a
    // command" for a message that opens with one. Empty only before
    // `attributed_to` has run.
    if error.command().is_empty() {
        issue
    } else {
        issue.with_command(error.command().to_string())
    }
}

/// Visit every leaf under `verb` (a node's own `json_output`, if set, does
/// not count — only a leaf runs), tracking whether at least one leaf exists
/// and whether every leaf visited so far declares JSON output.
fn walk_leaf_json(verb: &Verb, any: &mut bool, all: &mut bool) {
    if verb.verbs.is_empty() {
        *any = true;
        *all = *all && verb.json_output;
    } else {
        for child in &verb.verbs {
            walk_leaf_json(child, any, all);
        }
    }
}

/// The schema for one verb, recursing into its children so a node's schema
/// carries its own leaves as nested subcommands.
fn verb_schema(verb: &Verb) -> ToolSchema {
    let description = match verb.tail {
        Tail::Forward => append_clause(&verb.about, "forwards undeclared flags"),
        _ => verb.about.clone(),
    };
    let mut schema = ToolSchema::new(verb.name_or_root(), description);
    for param in verb_params(verb) {
        schema = schema.param(param);
    }
    for (label, command) in &verb.examples {
        schema = schema.example(label, command);
    }
    if verb.json_output {
        schema = schema.with_typed_substitution();
    }
    for child in &verb.verbs {
        schema = schema.subcommand(verb_schema(child));
    }
    schema
}

/// Flags then positionals, in declaration order — the order
/// `validate_against_schema` and `help` both read.
fn verb_params(verb: &Verb) -> Vec<ParamSchema> {
    let mut params = Vec::with_capacity(verb.flags.len() + verb.positionals.len());
    for flag in &verb.flags {
        let param_type = if !flag.takes_value {
            "bool"
        } else if flag.int {
            "int"
        } else {
            "string"
        };
        let mut description = flag.about.clone();
        if !flag.choices.is_empty() {
            description = append_clause(&description, &format!("one of: {}", flag.choices.join(", ")));
        }
        let mut param = ParamSchema::new(&flag.name, param_type)
            .with_required(flag.required)
            .with_description(description)
            .with_aliases(flag.aliases.clone())
            .with_repeatable(flag.repeatable);
        if !flag.takes_value {
            param = param.with_default(Some(Value::Bool(false)));
        }
        params.push(param);
    }
    for positional in &verb.positionals {
        let mut description = positional.about.clone();
        if let Some(root) = &positional.path_under {
            description = append_clause(&description, &format!("must be under {}", root.display()));
        }
        params.push(
            ParamSchema::new(&positional.name, "string")
                .with_required(positional.required)
                .with_description(description)
                .positional(),
        );
    }
    params
}

/// Join a clause onto a description without leaving a stray separator when
/// the description is empty.
fn append_clause(description: &str, clause: &str) -> String {
    if description.is_empty() {
        clause.to_string()
    } else {
        format!("{description}; {clause}")
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::expect_used)]
mod tests;
